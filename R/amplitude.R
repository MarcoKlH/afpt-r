## function amplitude
##
## returns amplitude optimized for minimum induced power for prescribed reduced frequency (kf), strokeplane angle (phi), and thrust requirement / thrust-to-lift ratio (TL)
amplitude <- function(kf,phi,TL) {
  C <- FLAPPINGMODELCOEFFS$A
  (1 + (C$q00 + C$q01*phi + C$q02*phi^2)*kf)*atan((C$p00 + C$p01*phi + C$p02*phi^2)*(TL/kf)^(1/C$r) + (C$p10 + C$p11*phi + C$p12*phi^2)*(TL/kf) + (C$p40 + C$p41*phi + C$p42*phi^2)*(TL/kf)^4) * 180/pi
}
