computeFlappingPower <- function(bird,speed,...,frequency = bird$wingbeatFrequency,strokeplane = 'opt') {
    opts <- list(...)
    climbAngle <- .setDefault(opts,'climbAngle',0)

    checkedArguments <- .computeFlappingPower.interpretArguments(bird,speed,opts,frequency,strokeplane,climbAngle)

    output<- with(checkedArguments,
         if (try(grepl('opt',strokeplane))) {
             .computeFlappingPower.optimizeStrokeplane(bird,speed,opts,frequency,climbAngle)
         } else {
             .computeFlappingPower.base(bird,speed,opts,frequency,strokeplane,climbAngle)
         }
    )

    return(output)
}

.computeFlappingPower.base <- function(bird,speed,opts,frequency,strokeplane,climbAngle) {
    ## flight condition
    fc <- .setDefault(opts,'flightcondition',ISA0)
    rho <- .setDefault(opts,'density',fc$density)
    g <- .setDefault(opts,'gravity',fc$gravity)
    nu <- .setDefault(opts,'viscosity',fc$viscosity)

    ## short hand bird parameters
    m <- bird$massTotal
    b <- bird$wingSpan
    S <- bird$wingArea
    Sb <- bird$bodyFrontalArea
    phi <- strokeplane*pi/180 # assume phi comes in degrees

    ## reduced frequency
    kf <- reducedFrequency(b,frequency,speed)
    ReynoldsNo <- computeReynoldsNumber(speed,S/b,nu)

    ## decompose weight into lift and drag components wrt climb angle
    climbAngle <- climbAngle*pi/180
    L.climb <- m*g*cos(climbAngle)
    D.climb <- m*g*sin(climbAngle)

    ## Forces
    L <- .setDefault(opts,'lift',L.climb)
    Dnf <- dragForces(bird,speed,L,fc,opts)
    Dnf$par <- Dnf$par+D.climb

    # Thrust ratio
    ToverL <- (Dnf$ind + Dnf$pro0 + Dnf$pro2 + Dnf$par)/(L - fD.ind(kf,phi)*Dnf$ind - fD.pro0(kf,phi)*Dnf$pro0 - fD.pro2(kf,phi)*Dnf$pro2)

    kD <- data.frame(
        ind = (1 + fD.ind(kf,phi)*ToverL),
        pro0 = (1 + fD.pro0(kf,phi)*ToverL),
        pro2 = (1 + fD.pro2(kf,phi)*ToverL)
    )
    kP <- data.frame(
        ind = (1 + fP.ind(kf,phi)*ToverL),
        pro0 = (1 + fP.pro0(kf,phi)*ToverL),
        pro2 = (1 + fP.pro2(kf,phi)*ToverL)
    )

    power.ind <- kP$ind*Dnf$ind*speed
    power.pro0 <- kP$pro0*Dnf$pro0*speed
    power.pro2 <- kP$pro2*Dnf$pro2*speed
    power.par <- Dnf$par*speed
    power.total <- power.ind + power.pro0 + power.pro2 + power.par

    # induced velocity in hover (for forward flight check)
    vih <- sqrt(L/(1/2*rho*pi*b^2))

    # model validity check
    flags <- data.frame(
        redFreqLo = kf<1,
        redFreqHi = kf>6,
        thrustHi = ToverL>0.3,
        speedLo = speed<2*vih
    )

    output <- data.frame(
        bird.name = bird$name,
        speed = speed,
        power = power.total,
        power.chem = mech2chem(power.total,bird),
        strokeplane = strokeplane,
        amplitude = amplitude(kf,phi,ToverL),
        frequency = frequency,
        flags = flags,
        kD = kD,
        kP = kP,
        CDpro0 = Dnf$pro0/(1/2*fc$density*speed^2*S),
        ReynoldsNumber = ReynoldsNo,
        Dnf = Dnf,
        L = L
    )

    return(output)
}


.computeFlappingPower.optimizeStrokeplane <- function(bird,speed,opts,frequency,climbAngle){
    # strokeplane <- mapply(
    #     function(speed,frequency){
    #         result <- stats::optimize(function(x) .computeFlappingPower.base(bird,speed,opts,frequency=frequency,strokeplane=x)$power,c(0,50),tol=0.1)
    #         return(result$minimum)
    #     },speed=speed,frequency=frequency)
    strokeplane <- 0*speed
    optimizeStrokeplane <- function(bird,speed,opts,frequency,climbAngle) {
        result <- stats::optimize(
            function(x) .computeFlappingPower.base(bird,speed,opts,frequency,x,climbAngle)$power,
            c(0,50),
            tol=0.1)
        return(result$minimum)
    }
    for (i in seq_len(length(speed))) {
        strokeplane[i] <- optimizeStrokeplane(bird[i,],speed[i],opts,frequency[i],climbAngle[i])
    }
    return(.computeFlappingPower.base(bird,speed,opts,frequency,strokeplane,climbAngle))
}


.computeFlappingPower.interpretArguments <- function(bird,speed,opts,frequency,strokeplane,climbAngle) {
    ## handle multiple birds (only split rows if necessary)
    nBirds <- nrow(bird)
    nSpeeds <- length(speed)
    if (nSpeeds == nBirds) { # simple row splitting
        useBird <- bird
        useSpeed <- speed
    } else { # first
        useBird <- bird[rep(seq_len(nBirds),each=nSpeeds),]
        useSpeed <- rep(speed,times=nBirds)
    }

    ## deal with wingbeat frequency
    frequency <- switch(typeof(frequency),
                        'double'=frequency, # if a numerical value provided, just take that.
                        'closure'=try(frequency(speed),silent=TRUE), # if function provided, try if it works (otherwise deal with error later)
                        bird$wingbeatFrequency # otherwise just fall back on bird reference frequency
    )
    if (class(frequency)[1]=='try-error') { # deal with try-error in case provided function doesn't work -> fall back on bird reference frequency
        warning('Could not interpret specified function for wingbeat frequency. Resorting to default bird wingbeat frequency.')
        frequency <- bird$wingbeatFrequency
    }
    frequency <- (frequency<0)*bird$wingbeatFrequency + (frequency>0)*frequency # ensure positive frequency
    if (length(frequency)==1) frequency <- rep(frequency,length(useSpeed))
    if (length(frequency)==nSpeeds & nSpeeds!=nBirds) frequency <- rep(frequency,nBirds)


    # Deal with strokeplane
    strokeplane <- switch(typeof(strokeplane),
                          'closure'=try(strokeplane(speed),silent=TRUE), # if function provided, try if it works (otherwise deal with error later)
                          strokeplane # otherwise just use strokeplane
    )
    if (class(strokeplane)[1]=='try-error') { # deal with try-error in case provided function doesn't work -> fall back on strokeplane optimisation
        warning('Could not interpret specified function for strokeplane angle. Defaulting to optimize strokeplane angle instead.')
        strokeplane <- 'opt'
    }

    # Deal with climbAngle
    if (length(climbAngle)==1) climbAngle <- rep(climbAngle,length(useSpeed))
    if (length(climbAngle)==nSpeeds & nSpeeds!=nBirds) climbAngle <- rep(climbAngle,nBirds)



    checkedArguments <- list(
        'bird' = useBird,
        'speed' = useSpeed,
        'frequency' = frequency,
        'strokeplane' = strokeplane,
        'climbAngle' = climbAngle
    )
}
