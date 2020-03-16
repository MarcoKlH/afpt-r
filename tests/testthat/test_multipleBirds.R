library(testthat)
library(afpt)
context("Multiple birds for computeFlappingPower()")

data("climbing_birds")
multiBird <- Bird(climbing_birds)
multiSpeed <- climbing_birds$climbSpeed

test_that("Single bird with single speed", {
    singleBird.power <- computeFlappingPower(multiBird[1,],multiSpeed[1])
    expect_equal(singleBird.power$speed,multiSpeed[1])
    expect_equivalent(singleBird.power$bird.name,multiBird$name[1])
})

test_that("Single bird with multiple speeds", {
    singleBird.power <- computeFlappingPower(multiBird[1,],multiSpeed)
    expect_equal(singleBird.power$speed,multiSpeed)
    expect_equivalent(unique(singleBird.power$bird.name),multiBird$name[1])
})

test_that("Single bird with multiple speeds with prescribed strokeplane angle", {
    singleBird.power <- computeFlappingPower(multiBird[1,],multiSpeed,strokeplane=0)
    expect_equal(singleBird.power$strokeplane,0*multiSpeed)
    expect_equal(singleBird.power$speed,multiSpeed)
    expect_equivalent(unique(singleBird.power$bird.name),multiBird$name[1])
})

test_that("Multiple birds with single speed returns same speed for all birds", {
    multiBird.power <- computeFlappingPower(multiBird,multiSpeed[1])
    expect_equivalent(unique(multiBird.power$speed),multiSpeed[1])
    expect_equivalent(multiBird.power$bird,multiBird$name)
    expect_equal(multiBird.power$frequency,multiBird$wingbeatFrequency)
})

test_that("Multiple birds with multiple speeds returns correct speed", {
    multiBird.power <- computeFlappingPower(multiBird,multiSpeed)
    expect_equivalent(unique(multiBird.power$speed),multiSpeed)
    expect_equivalent(multiBird.power$bird,multiBird$name)
    expect_equal(multiBird.power$frequency,multiBird$wingbeatFrequency)
})

test_that("Multiple birds with multiple speeds but a single frequency returns correct speed", {
    multiBird.power <- computeFlappingPower(multiBird,multiSpeed,frequency=20)
    expect_equivalent(unique(multiBird.power$speed),multiSpeed)
    expect_equivalent(multiBird.power$bird,multiBird$name)
    expect_equal(multiBird.power$frequency,20+0*multiSpeed)
})
