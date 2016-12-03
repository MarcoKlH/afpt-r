computeReynoldsNumber <- function(speed,length,viscosity=ISA0$viscosity){
    speed*length/viscosity
}
