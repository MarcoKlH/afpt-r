findMaximumRangeSpeed <- function(bird,lower=NULL,upper=NULL,windSpeed=0,windDir=0,...){
  if(any(class(bird)=='function')) return(.findMaximumRangeSpeed.function(bird,lower,upper,windSpeed,windDir,...))
  if(any(class(bird)=='bird')) return(.findMaximumRangeSpeed.multiBird(bird,lower,upper,windSpeed,windDir,...))
  # otherwise
  warning('Wrong class for bird input! trying to cast to bird object...')
  return(try(.findMaximumRangeSpeed.multiBird(Bird(bird),lower,upper,windSpeed,windDir,...),silent=TRUE))
}

.findMaximumRangeSpeed.bird <- function(bird,lower,upper,windSpeed=0,windDir=0,...){
  fun <- function(speed){
    computeFlappingPower(bird,speed,...)
  }
  return(.findMaximumRangeSpeed.function(fun,lower,upper,windSpeed,windDir,...))
}

.findMaximumRangeSpeed.function <- function(bird,lower,upper,windSpeed=0,windDir=0,...){
  if (is.null(lower)|is.null(upper)) stop("Can't proceed without lower and upper bounds")

  fun <- match.fun(bird)
  opts <- list(...)
  wind <- windSpeed
  beta <- windDir*pi/180

  lower <- max(wind*sin(beta),lower) #  ensure that lower bound gives valid groundspeed
  upper <- upper + lower*(lower>upper) #  ensure that upper bound is larger than lower
  upper <- upper + 2*wind*(2*wind>upper) #  ensure that upper bound is larger than twice the wind

  gamma <- .setDefault(opts,'climbAngle',0)*pi/180

  groundSpeed <- function(speed) {
    groundSpeed <- air2ground(
      speed, windSpeed = wind, windDir = windDir, climbAngle = gamma
    )$groundSpeed
  }

  costOfTransport <- function(speed) {
    COT <- fun(speed)$power.chem/groundSpeed(speed)
  }

  optResult <- stats::optimize(costOfTransport,c(lower,upper),tol=0.01)
  optResult$xmin <- optResult$minimum

  dist2lower <- (optResult$xmin - lower)/optResult$xmin
  dist2upper <- (upper - optResult$xmin)/optResult$xmin

  if (dist2upper<1E-4) {# converged on upper bound;
    return(findMaximumRangeSpeed(bird,0.999*upper,2*upper,windSpeed,windDir,...)) #  expand bounds
  }

  output <- fun(optResult$xmin)
  return(output)
}


.findMaximumRangeSpeed.multiBird <- function(bird,lower,upper,windSpeed=0,windDir=0,...){
  # allow for missing lower and upper definition
  if (is.null(lower)) {
    Vmr <- with(bird,
                ((massTotal*9.81)^2/(0.5*1.225*pi*wingSpan^2))/
                  (0.5*1.225*coef.bodyDragCoefficient*bodyFrontalArea)
    )^(1/4)

    lower <- 0.7*Vmr
    upper <- 1.5*Vmr
  }

  ## handle multiple birds (split rows)
  nBirds <- nrow(bird)
  iFun <- function(iBird).findMaximumRangeSpeed.bird(
    bird[iBird,],
    lower[iBird*(length(lower)==nBirds)+1*(length(lower)!=nBirds)],
    upper[iBird*(length(upper)==nBirds)+1*(length(upper)!=nBirds)],
    windSpeed = windSpeed,
    windDir = windDir,
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
