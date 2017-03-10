library(testthat)
library(afpt)
context("Multiple birds for computeFlappingPower()")

data("climbing_birds")
multiBird <- Bird(climbing_birds)
multiSpeed <- climbing_birds$climbSpeed

test_that("Single bird with multiple speeds", {
    singleBird.power <- computeFlappingPower(multiBird[1,],multiSpeed)
    expect_equal(singleBird.power$speed,multiSpeed)
    expect_equivalent(levels(singleBird.power$bird.name),multiBird$name[1])
})

test_that("Multiple birds with single speed returns same speed for all birds", {
    multiBird.power <- computeFlappingPower(multiBird,multiSpeed[1])
    expect_equivalent(unique(multiBird.power$speed),multiSpeed[1])
    expect_equivalent(levels(multiBird.power$bird),multiBird$name)
})
