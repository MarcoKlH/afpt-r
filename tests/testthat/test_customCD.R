library(afpt)
context("Custom drag coefficient in computeFlappingPower()")

myBird <- Bird(massTotal = 0.215,wingSpan = 0.67,wingArea = 0.0652)
flightSpeed <- seq(5,15,1)
q <- 1/2*afpt:::ISA0$density*flightSpeed^2

test_that("default returns original coefficients", {
    power.aero <- computeFlappingPower(bird = myBird, speed =  flightSpeed)
    CD.par <- power.aero$Dnf.par/q/myBird$bodyFrontalArea
    expect_equal(CD.par,rep(myBird$coef.bodyDragCoefficient,length(flightSpeed)))
})

test_that("single custom body drag coefficient returns correctly", {
    customBodyDragCoefficient <- 0.1
    power.aero <- computeFlappingPower(bird = myBird, speed =  flightSpeed, bodyDragCoefficient=customBodyDragCoefficient)
    CD.par <- power.aero$Dnf.par/q/myBird$bodyFrontalArea
    expect_equal(CD.par,rep(customBodyDragCoefficient,length(flightSpeed)))
})

test_that("speed dependent closure object returns drag coefficient correctly for multiple speeds", {
    bodyDragCoef.closure <- function(flightSpeed){0.1 + 1/flightSpeed}
    power.aero <- computeFlappingPower(bird = myBird, speed =  flightSpeed, bodyDragCoefficient=bodyDragCoef.closure)
    CD.par <- power.aero$Dnf.par/q/myBird$bodyFrontalArea
    expect_equal(CD.par,bodyDragCoef.closure(flightSpeed))
})

# test_that("multiple custom body drag coefficients return correctly", {
#     bodyDragCoef.multi <- 0.1 + 1/flightSpeed
#     power.aero <- computeFlappingPower(bird = myBird, speed =  flightSpeed, bodyDragCoefficient=bodyDragCoef.multi)
#     CD.par <- power.aero$Dnf.par/q/myBird$bodyFrontalArea
#     expect_equal(CD.par,bodyDragCoef.multi)
# })


test_that("computeFlightPerformance works with custom drag coefficient of closure type", {
    bodyDragCoef.closure <- function(flightSpeed){0.1 + 1/flightSpeed}
    flightperf <- computeFlightPerformance(bird = myBird, bodyDragCoefficient = bodyDragCoef.closure, length.out = 0)
    CD.par <- with(flightperf$table,Dnf.par/(1/2*1.225*speed^2)/flightperf$bird$bodyFrontalArea)
    expect_equal(CD.par,bodyDragCoef.closure(flightperf$table$speed))
})
