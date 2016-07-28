air2ground <- function(airSpeed,windSpeed=0,windDir=0,climbAngle=0){
  windDir <- windDir*pi/180

  U <- airSpeed*cos(climbAngle)
  WtoV <- windSpeed*sin(windDir)
  WtoV[abs(WtoV)>U] <- NA;

  driftAngle <- asin(WtoV/U)
  if (is.nan(driftAngle)) driftAngle <- 0
  groundSpeed = windSpeed*cos(windDir) + U*cos(driftAngle)
  #groundSpeed = windSpeed*cos(windDir) + sqrt((U*cos(climbAngle))^2-WtoV^2)

  return(list(
    groundSpeed = groundSpeed,
    driftAngle = driftAngle
    ))
}
