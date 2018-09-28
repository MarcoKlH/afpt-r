library(testthat)
library(afpt)
context("Multiple birds for computeFlappingPower()")

bird <- Bird(massTotal = 0.215,wingSpan = 0.67,wingArea = 0.0652)
speeds <- seq(5,15,1)


test_that("computeFlappingPower() produces mechanical power and chemical power",{
    powerOut <- computeFlappingPower(bird,speeds)
    expect_equal(powerOut$power.chem,mech2chem(powerOut$power,bird))
})

test_that("optimal speeds still work",{
    powerMR <- findMaximumRangeSpeed(bird)
    powerMP <- findMinimumPowerSpeed(bird)
    power.max <- computeAvailablePower(bird)
    powerMaxSpeed <- findMaximumPowerSpeed(bird,power.max,powerMP$speed,2*powerMR$speed)
    powerMinSpeed <- findMaximumPowerSpeed(bird,power.max,0.1*powerMP$speed,powerMP$speed)
    powerClimbRate <- findMaximumClimbRate(bird,power.max)
    performance <- computeFlightPerformance(bird,length.out=20)
    print(performance)
    print(c(powerMinSpeed$power,powerMP$power,powerMR$power,powerMaxSpeed$power,powerClimbRate$power))
    plot(performance)
})
