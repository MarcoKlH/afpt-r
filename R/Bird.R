Bird <- function(massTotal,wingSpan,wingArea,...) UseMethod('Bird')

Bird.data.frame <- function(massTotal,wingSpan,wingArea,...) {
  df <- massTotal
  massTotal <- .setDefault(df,'massTotal',NULL)
  wingSpan <- df$wingSpan
  wingArea <- .setDefault(df,'wingArea',NULL)
  bird <- Bird.default(massTotal,wingSpan,wingArea,df)
}


Bird.default <- function(massTotal,wingSpan,wingArea=NULL,...) {
  opts <- list(...)
  if (length(opts)>0) if (class(opts[[1]])=='data.frame') opts <- opts[[1]];
  bird <- data.frame(
    wingSpan = wingSpan
  )
  if(!is.null(wingArea)) {
    bird$wingArea <- wingArea
  } else if (.hasField(opts,'wingAspect')) {
    bird$wingArea <- .aspect2area(wingSpan,opts$wingAspect)
  } else {
      stop("I can't figure out the wing area... please provide wingArea or wingAspect")
  }

  bird$name <- .setDefault(opts,'name',NA)
  bird$name.scientific <- .setDefault(opts,'name.scientific',NA)
  bird$source <- .setDefault(opts,'source',NA)

  if (!missing(massTotal)) {
    bird$massTotal <- massTotal
  }

  # assign sub weights (for now... should expand this to include more sub weights)
  bird <- .massComposition(bird,opts)

  bird$muscleFraction <- .setDefault(opts,'muscleFraction',0.17)

  # assign bird type
  bird$type <- .setDefault(opts,'type','other')

  # estimate body frontal area:
  bird$bodyFrontalArea <- .setDefault(
    opts,'bodyFrontalArea',
    computeBodyFrontalArea( bird$massEmpty, bird$type )
  )

  # estimate typical frequency:
  bird$wingbeatFrequency <- .setDefault(opts,'wingbeatFrequency',
    .estimateFrequency(bird)
  )

  # aerodynamic coefficients
  #bird$coef.inducedDragFactor <- .setDefault(opts,'coef.inducedDragFactor',1.2)
  bird$coef.profileDragLiftFactor <- .setDefault(opts,'coef.profileDragLiftFactor',0.03)
  bird$coef.bodyDragCoefficient <- .setDefault(opts,'coef.bodyDragCoefficient',0.2)
  bird$coef.conversionEfficiency <- .setDefault(opts,'coef.conversionEfficiency',0.23)
  bird$coef.respirationFactor <- .setDefault(opts,'coef.respirationFactor',1.1)
  bird$coef.activeStrain <- .setDefault(opts,'coef.activeStrain',0.26)
  bird$coef.isometricStress <- .setDefault(opts,'coef.isometricStress',400E3)# adjusted to maximum given by Weis-Fogh and Alexander 1977 (250 - 400)

  # basal metabolic rate
  bird$basalMetabolicRate <- .estimateBasalMetabolicRate(bird)


  class(bird) <- append(class(bird),'bird')
  #print(bird)

  return(bird)
}

.estimateFrequency <- function (bird,...) {
  opts <- list(...)
  rho <- .setDefault(opts,'density',1.225)
  g <- .setDefault(opts,'gravity',9.81)
  b <- bird$wingSpan
  m <- bird$massEmpty
  S <- bird$wingArea

  m^(3/8)*sqrt(g)*b^(-23/24)*S^(-1/3)*rho^(-3/8)
}

.estimateBasalMetabolicRate <- function(bird,...) {
  opts <- list(...)
  type <- .setDefault(bird,'type','other')
  massEmpty <- bird$massEmpty

  isPasserine <- type == 'passerine'
  isSeaBird <- type == 'seabird'
  isBat <- type == 'bat'
  isOther <- !(isPasserine | isSeaBird | isBat)

  bmr <- 6.25*massEmpty^0.724 * isPasserine +
         5.43*massEmpty^0.72 * isSeaBird + # [Ellis and Gabrielsen, 2002]
         3.14*massEmpty^0.744 * isBat +  # [Speakman J.R., Thomas D.W., (2003) Physiological ecology and energetics of bats
         3.79*massEmpty^0.723 * isOther
  return(bmr)
  # switch(type,
  #    'passerine' = 6.25*massEmpty^0.724,
  #    'seabird' = 5.43*massEmpty^0.72,  # [Ellis and Gabrielsen, 2002]
  #    'bat' = 3.14*massEmpty^0.744,  # [Speakman J.R., Thomas D.W., (2003) Physiological ecology and energetics of bats]
  #    3.79*massEmpty^0.723
  # )
}

.aspect2area <- function(wingSpan,wingAspect) wingSpan^2/wingAspect

.massComposition <- function(bird,opts) {
  has.massTotal <- .hasField(bird,'massTotal')
  has.massEmpty <- .hasField(opts,'massEmpty')
  has.massFat <- .hasField(opts,'massFat')
  has.massLoad <- .hasField(opts,'massLoad')
  has.mass.count <- has.massTotal + has.massEmpty + has.massFat + has.massLoad
  if ( has.mass.count == 4) {# check correct
    bird$massEmpty <- opts$massEmpty
    bird$massFat <- opts$massFat
    bird$massLoad <- opts$massLoad
    massSum = with(bird, massEmpty + massFat + massLoad )
    if (bird$massTotal != massSum) {
      bird$massTotal <- massSum
      warning('Mismatch in mass composition. Recomputed total mass.')
    }
  } else if (has.mass.count == 3) {# compute missing
    if (!has.massTotal) {
      bird$massEmpty <- opts$massEmpty
      bird$massFat <- opts$massFat
      bird$massLoad <- opts$massLoad
      bird$massTotal <- with(bird, massEmpty + massFat + massLoad )
    } else if (!has.massEmpty) {
      bird$massFat <- opts$massFat
      bird$massLoad <- opts$massLoad
      bird$massEmpty <- with(bird, massTotal - massFat - massLoad )
    } else if (!has.massLoad) {
      bird$massFat <- opts$massFat
      bird$massEmpty <- opts$massEmpty
      bird$massLoad <- with(bird, massTotal - massFat - massEmpty )
    } else if (!has.massFat) {
      bird$massLoad <- opts$massLoad
      bird$massEmpty <- opts$massEmpty
      bird$massFat <- with(bird, massTotal - massEmpty - massLoad )
    }
  } else if (has.mass.count <= 2) {
    has.massTotal
    has.massEmpty
    if (has.massTotal & has.massEmpty) {
      bird$massLoad <- 0
      bird$massEmpty <- opts$massEmpty
      bird$massFat <- bird$massTotal - bird$massEmpty
    } else if (has.massTotal | has.massEmpty) {
      bird$massLoad <- .setDefault(opts,'massLoad',0)
      bird$massFat <- .setDefault(opts,'massFat',0)
      bird$massEmpty <- .setDefault(opts,'massEmpty',bird$massTotal)
      bird$massTotal <- with(bird,massEmpty+massFat+massLoad)
    } else {
      stop("Can't resolve mass composition. Define at least massTotal or massEmpty.")
    }
  }
  return(bird)
}
