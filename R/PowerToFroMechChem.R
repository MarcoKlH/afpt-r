
mech2chem <- function(power.mech,bird,...) {
  R <- bird$coef.respirationFactor
  eta <- bird$coef.conversionEfficiency
  BMR <- bird$basalMetabolicRate

  power.chem <- (power.mech/eta+BMR)*R

}

chem2mech <- function(power.chem,bird,...) {
  R <- bird$coef.respirationFactor
  eta <- bird$coef.conversionEfficiency
  BMR <- bird$basalMetabolicRate

  power.mech <- (power.chem/R-BMR)*eta

}
