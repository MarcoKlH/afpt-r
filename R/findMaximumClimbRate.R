findMaximumClimbRate <- function(bird,maximumPower,speed='opt',...) {
  if(any(class(bird)=='function')) return(.findMaximumClimbRate.function(bird,maximumPower,speed,...))
  if(any(class(bird)=='bird')) return(.findMaximumClimbRate.multiBird(bird,maximumPower,speed,...))
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
  fun <- match.fun(bird)
  if (grepl('opt',speed)) { # optimize speed (is this a good idea?)
    speed <- findMinimumPowerSpeed(function(speed)fun(0,speed),strokeplane=0,lower=4,upper=20)$speed

    fun.speed <- function(speed).findMaximumClimbRate.function(fun,maximumPower,speed,...)
    #optResult <- pracma::fminbnd(
    #  function(speed)fun.speed(speed)$climbRate*-1,
    #  0.9*speed,
    #  1.5*speed
    #)
    optResult <- stats::optimize(
      function(speed)fun.speed(speed)$climbRate*-1,
      c(0.9*speed,1.5*speed),
      tol=0.01
    )
    optResult$xmin <- optResult$minimum

    output <- fun.speed(optResult$xmin)
    return(output)
  }

  lower <- -89  # lower bound vertically down
  upper <- 89  # upper bound vertically up

  ## check if specified arguments make sense.
  P.lo <- fun(lower,speed)
  P.hi <- fun(upper,speed)
  if (P.hi$power<maximumPower) {
    warning('Excess power available in vertical climb')
  } else if (P.lo$power>maximumPower) {
    warning('Too little power for vertical descent??? Check input arguments')
  }

  # optResult <- pracma::brent(
  #   function(climbAngle)fun(climbAngle,speed)$power-maximumPower,
  #   lower,
  #   upper
  # )

  optResult <- stats::uniroot(
    function(climbAngle)fun(climbAngle,speed)$power-maximumPower,
    c(lower,upper),
    tol = 0.01
  )

  output <- fun(optResult$root,speed)
  output$climbAngle = optResult$root # add climb angle in degrees
  output$climbRate = output$speed*sin(optResult$root*pi/180) # add climb rate in m/s
  return(output)
}
