test_that("TTestBayesianOneSample equals its snapshot", {

  debugOff()

  rawResults <- loadRawResults("TTestBayesianOneSample.rds")
  simplifiedResults <- simplifyResults(rawResults)
  expect_snapshot(simplifiedResults)

  debugOn()
  expect_snapshot(print(simplifiedResults, short = TRUE, indent = 2))
  debugOff()

})

test_that("Descriptives equals its snapshot", {

  temp <- loadRawResults("Descriptives.rds")

  rawResults1 <- temp[[1L]]
  simplifiedResults1 <- simplifyResults(rawResults1)
  expect_snapshot(simplifiedResults1)

  rawResults2 <- temp[[2L]]
  simplifiedResults2 <- simplifyResults(rawResults2)
  expect_snapshot(simplifiedResults2)

  debugOn()
  expect_snapshot(print(simplifiedResults1, short = TRUE, indent = 3))
  debugOff()

  debugOn()
  expect_snapshot(print(simplifiedResults2, short = TRUE, indent = 4))
  debugOff()

})

test_that("Distributions equals its snapshot", {

  temp <- loadRawResults("Distributions.rds")

  rawResults1 <- temp[[1L]]
  simplifiedResults1 <- simplifyResults(rawResults1)
  expect_snapshot(simplifiedResults1)

  debugOn()
  expect_snapshot(print(simplifiedResults1, short = TRUE, indent = 2))
  debugOff()

})
