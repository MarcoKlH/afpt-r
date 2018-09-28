print.power.mechanical <- function(x,...) {
  print(as.double(x),...)
}
print.power.chemical <- function(x,...) {
  print(as.double(x),...)
}
print.powercurve.set <- function(x,...) {
  listobj <- x
  cat(paste('Name:', listobj$bird$name,'\n'))
  cat(paste('Sc. name:', listobj$bird$name.scientific,'\n'))
  cat(paste('Bird definitions:',listobj$bird$source,'\n'))

  # used somewhat dirty method for getting nice formatting of the data... probably
  # better to format the whole string with fprintf() and then print it with cat()
  listobj$table$strokeplane <- round(10*listobj$table$strokeplane)/10
  listobj$table$amplitude <- round(10*listobj$table$amplitude)/10
  print(listobj$table[c('speed','power.aero','power.chem','strokeplane','amplitude')],digits=4)

  listobj$maxClimb$strokeplane <- round(10*listobj$maxClimb$strokeplane)/10
  listobj$maxClimb$amplitude <- round(10*listobj$maxClimb$amplitude)/10
  cat(paste('Maximum climb performance:','\n'))
  print(listobj$maxClimb[c('speed','power.aero','power.chem','strokeplane','amplitude','climbRate')],digits=4)
  cat(paste('Minimized migration time:','\n'))
  print(listobj$minTime[c('speed','speed.migration','power.aero','power.chem','power.dep','strokeplane','amplitude')],digits=4)
}

print.bird <- function(x,...) {
  listobj <- x
  cat(paste('name:', listobj$name,'\n'))
  cat(paste('name.scientific:', listobj$name.scientific,'\n'))
  cat(paste('source:',listobj$source,'\n'))
  cat(paste('type:',listobj$type,'\n'))
  cat(paste('massTotal:',listobj$massTotal,'\n'))
  cat(paste(' massEmpty:',listobj$massEmpty,'\n'))
  cat(paste(' massFat:',listobj$massFat,'\n'))
  cat(paste(' massLoad:',listobj$massLoad,'\n'))
  cat(paste('muscleFraction:',listobj$muscleFraction,'\n'))
  cat(paste('bodyFrontalArea:',listobj$bodyFrontalArea,'\n'))
  cat(paste('wingbeatFrequency:',listobj$wingbeatFrequency,'\n'))
  cat(paste('basalMetabolicRate:',listobj$basalMetabolicRate,'\n'))
  cat(paste('coef.profileDragLiftFactor:',listobj$coef.profileDragLiftFactor,'\n'))
  cat(paste('coef.bodyDragCoefficient:',listobj$coef.bodyDragCoefficient,'\n'))
  cat(paste('coef.conversionEfficiency:',listobj$coef.conversionEfficiency,'\n'))
  cat(paste('coef.respirationFactor:',listobj$coef.respirationFactor,'\n'))
  cat(paste('coef.activeStrain:',listobj$coef.activeStrain,'\n'))
  cat(paste('coef.isometricStress:',listobj$coef.isometricStress,'\n'))

}
