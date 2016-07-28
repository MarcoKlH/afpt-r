computeBodyFrontalArea <- function (massEmpty,type='other') {
  isPasserine <- type == 'passerine'
  isOther <- !isPasserine

  bodyFrontalArea <- 0.0129*massEmpty^(0.614) * isPasserine +
                     0.00813*massEmpty^(0.666) * isOther
  return(bodyFrontalArea)
  #switch(type,
  #       'passerine' = 0.0129*massEmpty^(0.614),
  #       0.00813*massEmpty^(0.666)
  #)
}
