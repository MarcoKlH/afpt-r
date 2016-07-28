computeChemicalPower <- function (power.mech,bird,...) UseMethod('computeChemicalPower')

computeChemicalPower.power.mechanical <- function (power.mech,bird,...) {
  power.chem <- power.mech

  power.chem$power <- mech2chem(power.mech$power,bird,...)

  class(power.chem) <- c('data.frame','power.chemical')

  return(power.chem)
}

computeChemicalPower.numeric <- function (power.mech,bird,...) {
  mech2chem(power.mech,bird,...)
}
