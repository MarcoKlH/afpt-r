findMinimumPowerSpeed <- function(bird,lower=NULL,upper=NULL,...) {
  if(any(class(bird)=='function')) return(.findMinimumPowerSpeed.function(bird,lower,upper,...))
  if(any(class(bird)=='bird')) return(.findMinimumPowerSpeed.multiBird(bird,lower,upper,...))
  # otherwise
  warning('Wrong class for bird input! trying to cast to bird object...')
  try(.findMinimumPowerSpeed.multiBird(Bird(bird),lower,upper,...),silent=TRUE)
}

.findMinimumPowerSpeed.bird <-  function(bird,lower,upper,...) {
  fun <- function(speed) computeFlappingPower(bird,speed,...)
  output <- .findMinimumPowerSpeed.function(fun,lower,upper,...)
}

.findMinimumPowerSpeed.function <- function(bird,lower,upper,...) {
  fun <- match.fun(bird)

  #optResult <- pracma::fminbnd(function(speed)fun(speed)$power,lower,upper)
  optResult <- stats::optimize(function(speed)fun(speed)$power,c(lower,upper),tol=0.01)
  optResult$xmin <- optResult$minimum

  output <- fun(optResult$xmin)
}

.findMinimumPowerSpeed.multiBird <- function(bird,lower,upper,...) {
  # allow for missing lower and upper definition
  if (is.null(lower)) {
    Vmp <- with(bird,
                ((massTotal*9.81)^2/(0.5*1.225*pi*wingSpan^2))/
                  (3*0.5*1.225*coef.bodyDragCoefficient*bodyFrontalArea)
    )^(1/4)
    lower <- 0.8*Vmp
    upper <- 1.3*Vmp
  }

  ## handle multiple birds (split rows)
  nBirds <- nrow(bird)
  iFun <- function(iBird).findMinimumPowerSpeed.bird(
    bird[iBird,],
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
