findMinimumTimeSpeed <- function(bird,EnergyDepositionRate=1.5*bird$basalMetabolicRate,lower=NULL,upper=NULL,windSpeed=0,windDir=0,...){
    if(any(class(bird)=='function')) return(.findMinimumTimeSpeed.function(bird,EnergyDepositionRate,lower,upper,windSpeed,windDir,...))
    if(any(class(bird)=='bird')) return(.findMinimumTimeSpeed.multiBird(bird,EnergyDepositionRate,lower,upper,windSpeed,windDir,...))
    # otherwise
    warning('Wrong class for bird input! trying to cast to bird object...')
    return(try(.findMinimumTimeSpeed.multiBird(Bird(bird),lower,upper,windSpeed,windDir,...),silent=TRUE))
}

.findMinimumTimeSpeed.bird <- function(bird,EnergyDepositionRate,lower=NULL,upper=NULL,windSpeed=0,windDir=0,...){
    # allow for missing lower and upper definition
    if (is.null(lower)) {
        Vmr <- with(bird,
                    ((massTotal*9.81)^2/(0.5*1.225*pi*wingSpan^2))/
                        (0.5*1.225*coef.bodyDragCoefficient*bodyFrontalArea)
        )^(1/4)

        lower <- 1*Vmr
        upper <- 3*Vmr
    }
    fun <- function(speed){
        powerOut <- computeFlappingPower(bird,speed,...)
        powerOut$power.chem <- powerOut$power.chem + EnergyDepositionRate
        return(powerOut)
    }
    tmp_powerOut <- .findMaximumRangeSpeed.function(fun,lower,upper,windSpeed,windDir,...)
    powerOut <- computeFlappingPower(bird,tmp_powerOut$speed,...)
    powerOut$power.dep <- EnergyDepositionRate
    powerOut$speed.migration <- powerOut$speed*EnergyDepositionRate/(EnergyDepositionRate+powerOut$power.chem)
    return(powerOut)
}

.findMinimumTimeSpeed.function <- function(bird,EnergyDepositionRate,lower,upper,windSpeed=0,windDir=0,...){
    fun1 <- match.fun(bird)

    fun <- function(speed){
        powerOut <- fun1(speed)
        powerOut$power.chem <- powerOut$power.chem + EnergyDepositionRate
        return(powerOut)
    }
    tmp_powerOut <- .findMaximumRangeSpeed.function(fun,lower,upper,windSpeed,windDir,...)
    powerOut <- fun1(powerOut$speed,...)
    powerOut$EnergyDepositionRate <- EnergyDepositionRate
    powerOut$speed.migration <- powerOut$speed*EnergyDepositionRate/(EnergyDepositionRate+powerOut$power.chem)
    return(powerOut)
}


.findMinimumTimeSpeed.multiBird <- function(bird,EnergyDepositionRate,lower,upper,windSpeed=0,windDir=0,...){
    ## handle multiple birds (split rows)
    nBirds <- nrow(bird)
    if ((length(EnergyDepositionRate)) == 1){
        EnergyDepositionRate <- rep.int(EnergyDepositionRate,nBirds)
    }
    iFun <- function(iBird).findMinimumTimeSpeed.bird(
        bird[iBird,],
        EnergyDepositionRate[iBird],
        lower[iBird*(length(lower)==nBirds)+1*(length(lower)!=nBirds)],
        upper[iBird*(length(upper)==nBirds)+1*(length(upper)!=nBirds)],
        windSpeed = windSpeed,
        windDir = windDir,
        ...
    )
    for (iBird in seq_len(nBirds)) {
        if (iBird==1) {
            powerOut <- iFun(iBird)
        } else {
            tmp <- iFun(iBird)
            powerOut <- rbind(powerOut,tmp)
        }
    }
    return(powerOut)
}
