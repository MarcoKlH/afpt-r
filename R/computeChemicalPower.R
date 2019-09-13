computeChemicalPower <- function (power.mech,bird,...) UseMethod('computeChemicalPower')

computeChemicalPower.power.mechanical <- function (power.mech,bird,...) {
  warning('computeChemicalPower has been made redundant... power.mechanical class has also been made redundant...')
  power.chem <- power.mech

  power.chem$power <- mech2chem(power.mech$power,bird,...)

  class(power.chem) <- c('data.frame','power.chemical')

  return(power.chem)
}

computeChemicalPower.numeric <- function (power.mech,bird,...) {
  warning('computeChemicalPower has been made redundant... please adjust calling function to use mech2chem() instead.')
  mech2chem(power.mech,bird,...)
}
