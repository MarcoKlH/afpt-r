.hasField <- function (obj,name) length(grep(as.name(name),names(as.list(obj)))) > 0
.checkNames <- function (obj,requiredNames) for(name in requiredNames) if(!.hasField(obj,name)) stop('missing column for ', name)

.setDefault <- function (obj,name,default) if (.hasField(obj,name)) obj[[name]] else default


.simplifiedPerformance <- function(bird,fc=ISA0) {

    ## bird short hand
    m <- bird$massTotal
    b <- bird$wingSpan
    S <- bird$wingArea
    Sb <- bird$bodyFrontalArea

    ## bird aerodynamic coefficients
    ki <- .setDefault(bird,'coef.inducedDragFactor',1)
    kp <- .setDefault(bird,'coef.profileDragLiftFactor',1)
    CDp <- .setDefault(bird,'coef.profileDragCoefficient',0.02)
    CDb <- .setDefault(bird,'coef.bodyDragCoefficient',1)

    ## compute simplified powercurve coefficients
    lift <- m*fc$gravity
    rho <- fc$density
    c1.ind <- lift^2/(1/2*rho*pi*b^2)*ki
    c1.pro <- lift^2/(1/2*rho*S)*kp
    c2.pro <- 1/2*rho*S*CDp
    c2.body <- 1/2*rho*Sb*CDb
    c1 <- c1.ind+c1.pro
    c2 <- c2.pro+c2.body
    Vmp <- (c1/3/c2)^(1/4)
    Vmr <- (c1/c2)^(1/4)

    return(data.frame('Vmp'=Vmp,'Vmr'=Vmr))
}
