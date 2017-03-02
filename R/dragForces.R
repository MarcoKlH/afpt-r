dragForces <- function(bird,speed,lift=NULL,fc=ISA0,opts){
    if (is.null(lift)) lift <- bird$massTotal*fc$gravity

    b <- bird$wingSpan
    S <- bird$wingArea
    Sb <- bird$bodyFrontalArea

    q = 1/2*ISA0$density*speed^2 # dynamic pressure

    ReynoldsNo <- computeReynoldsNumber(speed,S/b,fc$viscosity)

    ## bird aerodynamic coefficients
    kp <- bird$coef.profileDragLiftFactor
    CDpro0 <- dragCoefs.ProfileDrag0(ReynoldsNo,opts)
    CDb <- dragCoefs.BodyDrag(bird,speed,opts)


    dragOut <- data.frame(
        ind = lift^2/(q*pi*b^2),
        pro0 = q*S*CDpro0,
        pro2 = lift^2/(q*S)*kp,
        par = q*Sb*CDb
    )
}

dragCoefs.InducedDrag <- function(liftcoef,aspectratio,k=1){
    k*liftcoef^2/(pi*aspectratio)
}

dragCoefs.ProfileDrag0 <- function(ReynoldsNo,opts){
    if (.hasField(opts,'CDpro0')) {
        if (length(opts$CDpro0)>0) { # fixed coefficient
            CDpro0 <- opts$CDpro0[1]
        }
        if (length(opts$CDpro0)>1) { # turbulent transition flat plate model
            ReynoldsNo.tr <- opts$CDpro0[2]
            CDpro0 <- CDpro0 +
                calcCDf2(ReynoldsNo,ReynoldsNo.tr) # turbulent transition model
        }
        if (length(opts$CDpro0)>2) { # multiplier
            CDpro0 <- CDpro0*opts$CDpro0[3]
        }
    } else {
        CDpro0 <- calcCDf.lam(ReynoldsNo)
    }
}

dragCoefs.ProfileDrag2 <- function(liftcoef,k=0.03){
    k*liftcoef^2
}

dragCoefs.BodyDrag <- function(bird,speed,opts){
    CDb <- bird$coef.bodyDragCoefficient # default
    if (.hasField(opts,'bodyDragCoefficient')) {
        CDb <- opts$bodyDragCoefficient
        CDb <- switch(typeof(CDb),
                      'double' = CDb, # if a numerical value provided, just take that.
                      'closure' = try(CDb(speed),silent=TRUE), # if function provided, try if it works (otherwise deal with error later)
                      bird$coef.bodyDragCoefficient # otherwise just fall back on bird body drag coefficient
        )
        if (class(CDb)[1]=='try-error') { # deal with try-error in case provided function doesn't work -> fall back on bird body drag coefficient
            warning('Could not interpret specified function for body drag coefficient. Resorting to default for bird.')
            CDb <- bird$coef.bodyDragCoefficient
        }
        CDb <- (CDb<0)*bird$coef.bodyDragCoefficient + (CDb>0)*CDb # ensure positive body drag coefficient
    }
    CDb
}

## induced drag
dragForces.InducedDrag <- function(lift,airspeed,wingspan,airdensity=1.225,k=1){
    k*lift^2/(1/2*airdensity*airspeed^2*pi*wingspan^2)
}

## zero lift wing profile drag
dragForces.ProfileDrag0 <- function(airspeed,wingarea,CDpro0,airdensity=1.225){
    1/2*airdensity*airspeed^2*CDpro0*wingarea
}

## lift dependent profile drag
dragForces.ProfileDrag2 <- function(lift,airspeed,wingarea,airdensity=1.225,k=0.03){
    k*lift^2/(1/2*airdensity*airspeed^2*wingarea)
}

## parasite drag
dragForces.ParasiteDrag <- function(airspeed,wingarea,CDpar,airdensity=1.225){
    1/2*airdensity*airspeed^2*CDpar*wingarea
}

calcCDf <- function(ReynoldsNo,ReynoldsNo.transition=5E5){
    CDf.lam <- calcCDf.lam(ReynoldsNo)
    CDf.lam.tr <- calcCDf.lam(ReynoldsNo.transition)
    CDf.tur <- calcCDf.tur(ReynoldsNo)
    CDf.tur.tr <- calcCDf.tur(ReynoldsNo.transition)

    # return:
    CDf.lam*(ReynoldsNo<=ReynoldsNo.transition) +
        (CDf.tur - (CDf.tur.tr - CDf.lam.tr)*ReynoldsNo.transition/ReynoldsNo)*(ReynoldsNo>ReynoldsNo.transition) # turbulent transition model (Anderson Fundamentals of Aerodynamics p360)
}

calcCDf2 <- function(ReynoldsNo,ReynoldsNo.transition=5E5){
    CDf.lam <- calcCDf.lam(ReynoldsNo)
    CDf.lam.tr <- calcCDf.lam(ReynoldsNo.transition)
    CDf.tur <- calcCDf.tur(ReynoldsNo)
    CDf.tur.tr <- calcCDf.tur(ReynoldsNo.transition)
    Rlt <- CDf.lam.tr/CDf.tur.tr
    Rclt <- (1-ReynoldsNo.transition/ReynoldsNo*(1-Rlt))
    Rclt[Rclt<0] <- 0

    # return:
    (ReynoldsNo<=ReynoldsNo.transition) * CDf.lam +
        (ReynoldsNo>ReynoldsNo.transition) * (
            CDf.lam.tr * ReynoldsNo.transition/ReynoldsNo +
                CDf.tur * Rclt^(6/5) -
                CDf.tur.tr * Rlt^(6/5)*ReynoldsNo.transition/ReynoldsNo) # turbulent transition model (matching transition thickness))
}

calcCDf.lam <- function(ReynoldsNo) 2.66/sqrt(ReynoldsNo)

calcCDf.tur <- function(ReynoldsNo) 2*0.074/(ReynoldsNo)^(1/5)
