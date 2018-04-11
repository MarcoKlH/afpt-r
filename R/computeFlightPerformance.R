computeFlightPerformance <- function (bird,...,length.out=10) {
  opts <- list(...)

  fc <- .setDefault(opts,'flightcondition',ISA0)

  ## check if bird object contains required fields
  requiredNames <- c('wingSpan','wingArea','massTotal','bodyFrontalArea')
  .checkNames(bird,requiredNames)

  ## bird short hand
  m <- bird$massTotal
  b <- bird$wingSpan
  S <- bird$wingArea
  Sb <- bird$bodyFrontalArea

  ## bird aerodynamic coefficients
  ki <- .setDefault(bird,'coef.inducedDragFactor',1)
  kp <- .setDefault(bird,'coef.profileDragLiftFactor',1)
  CDp <- .setDefault(bird,'coef.profileDragCoefficient',0.02)
  CDb <- .setDefault(bird,'coef.bodyDragCoefficient',1)

  ## compute simplified powercurve coefficients
  lift <- m*fc$gravity
  rho <- fc$density
  c1.ind <- lift^2/(1/2*rho*pi*b^2)*ki
  c1.pro <- lift^2/(1/2*rho*S)*kp
  c2.pro <- 1/2*rho*S*CDp
  c2.body <- 1/2*rho*Sb*CDb
  c1 <- c1.ind+c1.pro
  c2 <- c2.pro+c2.body
  Vmp <- (c1/3/c2)^(1/4)
  Vmr <- (c1/c2)^(1/4)

  # estimate available power
  powerAvailable <- computeAvailablePower(bird)

  # find characteristic speeds
  minimumPowerSpeed <- findMinimumPowerSpeed(bird, 0.5*Vmp,Vmr )
  maximumRangeSpeed <- findMaximumRangeSpeed(bird, Vmp,2*Vmr,...)
  Vmp <- minimumPowerSpeed$speed #  (improved estimate for other searches)
  minimumMaxPowerSpeed <- findMaximumPowerSpeed(bird,powerAvailable, 0.1*Vmp,Vmp )
  maximumMaxPowerSpeed <- findMaximumPowerSpeed(bird,powerAvailable, Vmp,10*Vmr )

  maximumClimbRate <- findMaximumClimbRate(bird,powerAvailable)



  # collect data in a table
  powerList <- list(
    minimumSpeed = minimumMaxPowerSpeed,
    minimumPower = minimumPowerSpeed,
    maximumRange = maximumRangeSpeed,
    maximumSpeed = maximumMaxPowerSpeed
  )
  powerTable <- powercurve2table(powerList)
  names(powerTable)[3] <- 'power.aero'

  climbTable <- powercurve2table(list(maximumClimbRate = maximumClimbRate))
  names(climbTable)[3] <- 'power.aero'

  # construct speed-power curves (consider replacing with 3rd,4th, or 5th order function of speed...)
  if (length.out>2) {
    rngCurve <- .setDefault(opts,'rangePowercurve',c(minimumMaxPowerSpeed$speed,maximumMaxPowerSpeed$speed))
    powercurve <- computeFlappingPower(bird,seq(rngCurve[1],rngCurve[2],length.out=length.out))
    names(powercurve)[3] <- 'power.aero'
  } else powercurve = NULL

  # collect list for output
  output = list(
    birdWSName = deparse(substitute(bird)),
    bird = bird,
    table = powerTable,
    maxClimb = climbTable,
    powercurve = powercurve
  )
  class(output) <- append(class(output),'powercurve.set')

  return(output)
}

powercurve2table <- function(listobj,showfields) {
  if(missing(showfields)) showfields <- names(listobj)
  df <- data.frame()
  for (fieldname in showfields) {
    df <- rbind(df,listobj[[fieldname]])
  }
  rownames(df) <- showfields
  return(df)
}
