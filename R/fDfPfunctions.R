## induced drag thrust requirement factor
fD.ind  <- function(kf,phi) {
  C <- FLAPPINGMODELCOEFFS$D.ind
  phi <- tan(phi)
  (C$p00+C$p02*phi^2)/kf + (C$p10+C$p11*phi+C$p12*phi^2)
}

## profile drag thrust requirement factor
fD.pro0 <- function(kf,phi) {
  C <- FLAPPINGMODELCOEFFS$D.pro0
  phi <- tan(phi)
  (C$p00+C$p02*phi^2) + (C$p10+C$p11*phi+C$p12*phi^2)*kf + (C$p20)/kf^2
}

## profile drag thrust requirement factor
fD.pro2 <- function(kf,phi) {
  C <- FLAPPINGMODELCOEFFS$D.pro2
  phi <- tan(phi)
  (C$p00+C$p02*phi^2)/kf + (C$p10+C$p11*phi+C$p12*phi^2)
}

## induced power thrust requirement factor
fP.ind  <- function(kf,phi) {
  C <- FLAPPINGMODELCOEFFS$P.ind
  phi <- tan(phi)
  (C$p00+C$p02*phi^2)/kf^C$r + (C$p10+C$p11*phi+C$p12*phi^2)
}


## profile power thrust requirement factor
fP.pro0 <- function(kf,phi) {
  C <- FLAPPINGMODELCOEFFS$P.pro0
  phi <- tan(phi)
  (C$p00+C$p02*phi^2) + (C$p10+C$p11*phi+C$p12*phi^2)*kf + C$p20/kf^2
}

## profile power thrust requirement factor
fP.pro2 <- function(kf,phi) {
  C <- FLAPPINGMODELCOEFFS$P.pro2
  phi <- tan(phi)
  (C$p00+C$p02*phi^2)/kf^C$r + (C$p10+C$p11*phi+C$p12*phi^2)
}
