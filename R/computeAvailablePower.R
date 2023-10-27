computeAvailablePower = function(bird,maxPowerAero,...) {
  # split into components, then call itself with components as arguments
  maxPower <- .computeAvailablePower(
    muscleMass = bird$muscleFraction * bird$massEmpty,
    wingbeatFrequency = bird$wingbeatFrequency,
    maxPowerAero = maxPowerAero,
    activeStrain = bird$coef.activeStrain,
    isometricStress = bird$coef.isometricStress
  )
}

.computeAvailablePower = function(muscleMass, wingbeatFrequency, maxPowerAero, activeStrain = 0.26, isometricStress = 400E3, muscleDensity = 1060, powerDensityMitochondria = 1.2E-6, optStressMaxPower = 0.30) {
  sigma <- optStressMaxPower*isometricStress
  lambda <- activeStrain
  Vmusc <- muscleMass/muscleDensity
  f <- wingbeatFrequency
  kmito <- powerDensityMitochondria

  # first compute the maximum possible aerobic power:
  maxPower <- sigma*lambda*Vmusc*f/(1+sigma*lambda*kmito*f)

  # if provided specified maximum aerobic power:
  if (!missing(maxPowerAero)) {
    if (maxPowerAero>maxPower) {
      print('Requested maximum aerobic power exceeds obtainable maximum for muscle.')
      print('Returning computed maximum aerobic power.')
    } else {
      maxPower <- sigma*lambda*Vmusc*f*(1-kmito*maxPowerAero/Vmusc)
    }
  }

  return(maxPower)
}
