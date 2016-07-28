findMaximumPowerSpeed <- function(bird,maximumPower,lower,upper,...) {
  if (any(class(bird)=='function')) return(.findMaximumPowerSpeed.function(bird,maximumPower,lower,upper,...))
  if(any(class(bird)=='bird')) return(.findMaximumPowerSpeed.multiBird(bird,maximumPower,lower,upper,...))
  # otherwise
  warning('Wrong class for bird input! trying to cast to bird object...')
  return(try(.findMaximumPowerSpeed.multiBird(Bird(bird),maximumPower,lower,upper,...),silent=TRUE))
}


.findMaximumPowerSpeed.bird <- function(bird,maximumPower,lower,upper,...) {
  fun <- function(speed)computeFlappingPower(bird,speed,...)
  return(.findMaximumPowerSpeed.function(fun,maximumPower,lower,upper,...))
}

.findMaximumPowerSpeed.function <- function(bird,maximumPower,lower,upper,...) {
  fun <- match.fun(bird)

  P.lo <- fun(lower)
  P.hi <- fun(upper)
  if (P.lo$power<maximumPower && P.hi$power<maximumPower) {
    warning('Specified maximum power not in range (too high); expanding search range')
    return(findMaximumPowerSpeed(bird,maximumPower,0.1*lower,2*upper,...))
  } else if (P.lo$power>maximumPower && P.hi$power>maximumPower) {
    warning('Specified maximum power not in range (too low)')
    if (P.lo$power<P.hi$power) return(P.lo) else return(P.hi)
  }
  #optResult <- pracma::brent(function(speed)fun(speed)$power-maximumPower,lower,upper)
  optResult <- stats::uniroot(function(speed)fun(speed)$power-maximumPower,c(lower,upper),tol=0.01)
  return(fun(optResult$root))
}


.findMaximumPowerSpeed.multiBird <- function(bird,maximumPower,lower,upper,...) {
  ## handle multiple birds (split rows)
  nBirds <- nrow(bird)
  iFun <- function(iBird).findMaximumPowerSpeed.bird(
    bird[iBird,],
    maximumPower[iBird],
    lower[iBird*(length(lower)==nBirds)+1*(length(lower)!=nBirds)],
    upper[iBird*(length(upper)==nBirds)+1*(length(upper)!=nBirds)],
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
