computeFlappingPower <- function(bird,speed,...,frequency,strokeplane = 'opt') {
  opts <- list(...)


  ## handle multiple birds (only split rows if necessary)
  nBirds <- nrow(bird)
  nSpeeds <- length(speed)
  optStrokeplane <- grepl('opt',strokeplane)
  expSpeeds <- nSpeeds>1 & nSpeeds!=nBirds
  splitRows <- nBirds>1 & ( optStrokeplane | expSpeeds )
  if ( expSpeeds ) speed <- rep(speed,nBirds) # epxand speeds
  if ( splitRows ) {
    if (missing(frequency))iFun <- function(iBird){
      computeFlappingPower(
        bird[iBird,],
        speed[(!expSpeeds)*iBird + expSpeeds*((iBird-1)*nSpeeds + seq_len(nSpeeds))],
        ...,
        strokeplane=strokeplane
      )
    }
    else {
      if ( length(frequency)!=nBirds ) {
        iFun <- function(iBird)computeFlappingPower(
          bird[iBird,],
          speed[(!expSpeeds)*iBird + expSpeeds*((iBird-1)*nSpeeds + seq_len(nSpeeds))],
          ...,
          frequency=frequency[1], # take only first frequency
          strokeplane = strokeplane
        )
      } else {
        iFun <- function(iBird)computeFlappingPower(
          bird[iBird,],
          speed[(!expSpeeds)*iBird + expSpeeds*((iBird-1)*nSpeeds + seq_len(nSpeeds))],
          ...,
          frequency=frequency[iBird], # take matching entry
          strokeplane = strokeplane
        )
      }
    }
  }
  if ( splitRows ) {
    for (iBird in seq_len(nBirds)) {
      if (iBird==1) powerOut <- iFun(iBird)
      else powerOut <- rbind(powerOut,iFun(iBird))
    }
    return(powerOut)
  }


  ## deal with wingbeat frequency
  if (missing(frequency)) frequency = bird$wingbeatFrequency
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

## deal with strokeplane angle
strokeplane <- switch(typeof(strokeplane),
                    'closure'=try(strokeplane(speed),silent=TRUE), # if function provided, try if it works (otherwise deal with error later)
                    strokeplane # otherwise just use strokeplane
)
if (class(strokeplane)[1]=='try-error') { # deal with try-error in case provided function doesn't work -> fall back on strokeplane optimisation
  warning('Could not interpret specified function for strokeplane angle. Defaulting to optimize strokeplane angle instead.')
  strokeplane <- 'opt'
}

  ## Strokeplane optimisation (could try to make this more integrated if it takes too much resources... but seems to work for now)
  if (try(grepl('opt',strokeplane))) {
    strokeplane <- mapply(
      function(speed,frequency){
        #result <- pracma::fminbnd(function(x) computeFlappingPower(bird,speed,...,frequency=frequency,strokeplane=x)$power,0,50)
        # return(result$xmin)
        result <- stats::optimize(function(x) computeFlappingPower(bird,speed,...,frequency=frequency,strokeplane=x)$power,c(0,50),tol=0.1)
        return(result$minimum)
      },speed=speed,frequency=frequency)
  }

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
  phi <- if(!missing(strokeplane)) strokeplane*pi/180 else 0 # assume phi comes in degrees

  ## reduced frequency
  kf <- reducedFrequency(b,frequency,speed)
  ReynoldsNo <- computeReynoldsNumber(speed,S/b,nu)

  ## decompose weight into lift and drag components wrt climb angle
  climbAngle <- .setDefault(opts,'climbAngle',0)*pi/180
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
    power = power.ind + power.pro0 + power.pro2 + power.par,
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
  class(output) <- append(class(output),'power.mechanical')

  return(output)
}

