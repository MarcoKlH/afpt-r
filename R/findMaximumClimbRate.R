findMaximumClimbRate <- function(bird,maximumPower,speed='opt',...) {
    if(any(class(bird)=='function')) return(.findMaximumClimbRate.function(bird,maximumPower,speed,...))
    if(any(class(bird)=='bird')) {
        if(missing(maximumPower)){
            message('findMaximumClimbRate(): argument "maximumPower" is missing; a default value will be computed from "bird"')
            maximumPower<- computeAvailablePower(bird)
        }
        return(.findMaximumClimbRate.multiBird(bird,maximumPower,speed,...))
    }
    # otherwise:
    warning('Wrong class for bird input! trying to cast to bird object...')
    return(try(findMaximumClimbRate(Bird(bird),maximumPower,speed,...),silent=TRUE))
}

.findMaximumClimbRate.bird <- function(bird,maximumPower,speed,...){
    fun <- function(climbAngle,speed) computeFlappingPower(bird,speed,...,climbAngle=climbAngle)
    return(.findMaximumClimbRate.function(fun,maximumPower,speed,...))
}

.findMaximumClimbRate.multiBird <- function(bird,maximumPower,speed,...){
    ## handle multiple birds (split rows)
    nBirds <- nrow(bird)
    nSpeeds <- length(speed)
    expSpeeds <- nSpeeds>1 & nSpeeds!=nBirds
    if ( expSpeeds ) { # epxand speeds
        bird <- bird[rep(seq_len(nrow(bird)),each=nSpeeds),]
        maximumPower <- rep(maximumPower,each=nSpeeds)
        speed <- rep(speed,nBirds)
    }
    nBirds <- nrow(bird)
    iFun <- function(iBird).findMaximumClimbRate.bird(
        bird[iBird,],
        maximumPower[iBird],
        speed[(nSpeeds==1) + (nSpeeds>1)*iBird],
        ...
    )
    for (iBird in seq_len(nBirds)) {
        if (iBird==1) {
            powerOut <- iFun(iBird)
        } else {
            tmp <- iFun(iBird)
            powerOut <- rbind(powerOut,tmp)
        }
    }
    return(powerOut)
}

.findMaximumClimbRate.function <- function(bird,maximumPower,speed,...){
    verbose = 0
    fun <- match.fun(bird)
    if (grepl('opt',speed)) { # optimize speed (is this a good idea?)
        speed <- findMinimumPowerSpeed(function(speed)fun(0,speed),strokeplane=0,lower=4,upper=20)$speed

        fun.speed <- function(speed).findMaximumClimbRate.function(fun,maximumPower,speed,...)

        optResult <- stats::optimize(
            function(speed)fun.speed(speed)$climbRate*-1,
            c(0.9*speed,1.5*speed),
            tol=0.01
        )
        optResult$xmin <- optResult$minimum

        output <- fun.speed(optResult$xmin)
        return(output)
    }

    ## estimate climb rate
    P.0 <- fun(0,speed)
    excessPower <- maximumPower-P.0$power
    climbRate.est <- excessPower/P.0$L
    if (abs(climbRate.est)>speed) climbAngle.est <- sign(climbRate.est)*89
    else  climbAngle.est <- asin(climbRate.est/speed)*180/pi

    lower <- max(climbAngle.est-5,-89)  # lower bound
    upper <- min(climbAngle.est+5,89)  # upper bound

    ## check if specified arguments make sense.
    P.lo <- fun(lower,speed)
    P.hi <- fun(upper,speed)

    if (P.lo$power > maximumPower) {
        if (verbose > 0) {
            warning("Power required to descend at this speed is higher than the power available.")
        }
        output <- P.lo
        output$climbAngle <- lower

    }
    else if (P.hi$power < maximumPower ) {
        if (verbose>0) {
            warning("Power required for vertical ascend less than power available (Outside model validity range!)")
        }
        output <- P.hi
        output$climbAngle <- upper
    } else {
        optResult <- stats::uniroot(
            function(climbAngle)fun(climbAngle,speed)$power-maximumPower,
            c(lower,upper),
            f.lower=P.lo$power,f.upper=P.hi$power,
            extendInt="yes" # have uniroot extend the search range if needed
            )

        output <- fun(optResult$root,speed)
        output$climbAngle = optResult$root # add climb angle in degrees
    }
    output$climbRate <- output$speed*sin(output$climbAngle*pi/180) # add climb rate in m/s

    flag.ctr <- sum(output$flags.redFreqLo,output$flags.redFreqHi,output$flags.speedLo,output$flags.thrustHi)
    if (flag.ctr>0){
        flag.str <- '\n'
        if (output$flags.redFreqLo) {flag.str <- paste(flag.str,'Low Reduced Frequency\n')}
        if (output$flags.redFreqHi) {flag.str <- paste(flag.str,'High Reduced Frequency\n')}
        if (output$flags.speedLo) {flag.str <- paste(flag.str,'Low speed\n')}
        if (output$flags.thrustHi) {flag.str <- paste(flag.str,'High Thrust Ratio\n')}
        if (verbose>0) {
            warning(sprintf("%i model validity flags raised:%s",flag.ctr,flag.str))
        }
    }
    return(output)
}
