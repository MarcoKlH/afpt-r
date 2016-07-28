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
        result <- optimize(function(x) computeFlappingPower(bird,speed,...,frequency=frequency,strokeplane=x)$power,c(0,50),tol=0.1)
        return(result$minimum)
      },speed=speed,frequency=frequency)
  }

  ## deal with optional argument for body drag coefficient
  CDb <- bird$coef.bodyDragCoefficient # default
  if (.hasField(opts,'bodyDragCoefficient')) {
    CDb <- opts$bodyDragCoefficient
    CDb <- switch(typeof(CDb),
                        'double' = CDb, # if a numerical value provided, just take that.
                        'closure' = try(CDb(speed),silent=TRUE), # if function provided, try if it works (otherwise deal with error later)
                        bird$coef.bodyDragCoefficient # otherwise just fall back on bird body drag coefficient
    )
    if (class(CDb)[1]=='try-error') { # deal with try-error in case provided function doesn't work -> fall back on bird body drag coefficient
      warning('Could not interpret specified function for body drag coefficient. Resorting to default for bird.')
      CDb <- bird$coef.bodyDragCoefficient
    }
    CDb <- (CDb<0)*bird$coef.bodyDragCoefficient + (CDb>0)*CDb # ensure positive body drag coefficient
  }

  ## flight condition
  fc <- .setDefault(opts,'flightcondition',FLIGHTCONDITION)
  rho <- .setDefault(opts,'density',fc$density)
  g <- .setDefault(opts,'gravity',fc$gravity)
  nu <- .setDefault(opts,'viscosity',fc$viscosity)

  ## short hand bird parameters
  m <- bird$massTotal
  b <- bird$wingSpan
  S <- bird$wingArea
  Sb <- bird$bodyFrontalArea
  phi <- if(!missing(strokeplane)) strokeplane*pi/180 else 0 # assume phi comes in degrees

  # reduced frequency
  kf <- reducedFrequency(b,frequency,speed)

  ## bird aerodynamic coefficients
  kp <- bird$coef.profileDragLiftFactor
  #CDb <- bird$coef.bodyDragCoefficient # dealt with above...
  Re <- speed/nu*S/b

  CDf.lam <- function(Re) 2.66/sqrt(Re)
  CDf.tur <- function(Re) 2*0.074/(Re)^(1/5)
  if (.hasField(opts,'CDpro0')) {
    if (length(opts$CDpro0)>0) { # fixed coefficient
      CDpro0 <- opts$CDpro0[1]
    }
    if (length(opts$CDpro0)>1) { # turbulent transition flat plate model
      Re.tr <- opts$CDpro0[2]
      CDpro0 <- CDpro0 +
        (Re<Re.tr)*CDf.lam(Re) +
        (Re>Re.tr)*(CDf.tur(Re) - (CDf.tur(Re.tr)-CDf.lam(Re.tr))*Re.tr/Re) # turbulent transition model
    }
    if (length(opts$CDpro0)>2) { # multiplier
      CDpro0 <- CDpro0*opts$CDpro0[3]
    }
  } else {
    CDpro0 <- CDf.lam(Re)
  }

  # decompose weight into lift and drag components wrt climb angle
  climbAngle <- .setDefault(opts,'climbAngle',0)*pi/180
  L.climb <- m*g*cos(climbAngle)
  D.climb <- m*g*sin(climbAngle)

    # Forces
  L <- .setDefault(opts,'lift',L.climb)
  Dnf.ind  <- L^2/(1/2*rho*pi*b^2)/speed^2
  Dnf.pro0 <- 1/2*rho*S*CDpro0*speed^2
  Dnf.pro2 <- L^2/(1/2*rho*S)*kp/speed^2
  D.body <- 1/2*rho*Sb*CDb*speed^2
  D.add <- .setDefault(opts,'addedDrag',D.climb)

  # Thrust ratio
  ToverL <- (Dnf.ind + Dnf.pro0 + Dnf.pro2 + D.body + D.add)/(L - fD.ind(kf,phi)*Dnf.ind - fD.pro0(kf,phi)*Dnf.pro0 - fD.pro2(kf,phi)*Dnf.pro2)

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

  power.ind <- kP$ind*Dnf.ind*speed
  power.pro0 <- kP$pro0*Dnf.pro0*speed
  power.pro2 <- kP$pro2*Dnf.pro2*speed
  power.body <- D.body*speed
  power.add <- D.add*speed

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
    power = power.ind + power.pro0 + power.pro2 + power.body + power.add,
    strokeplane = strokeplane,
    amplitude = amplitude(kf,phi,ToverL),
    frequency = frequency,
    flags = flags,
    kD = kD,
    kP = kP,
    CDpro0 = CDpro0,
    ReynoldsNumber = Re,
    Dnf = data.frame(
      ind = Dnf.ind,
      pro0 = Dnf.pro0,
      pro2 = Dnf.pro2,
      par = D.body+D.add
    ),
    L = L
  )
  class(output) <- append(class(output),'power.mechanical')

  return(output)
}

