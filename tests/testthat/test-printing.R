test_that("TTestBayesianOneSample equals its snapshot", {

  resetFormatOptions()

  rawResults <- loadRawResults("TTestBayesianOneSample.rds")
  simplifiedResults <- simplifyResults(rawResults)
  expect_snapshot(simplifiedResults)

  formatOptions(debug = TRUE)
  expect_snapshot(print(simplifiedResults, short = TRUE, indent = 2))

})

test_that("Descriptives equals its snapshot", {

  resetFormatOptions()

  temp <- loadRawResults("Descriptives.rds")

  rawResults1 <- temp[[1L]]
  simplifiedResults1 <- simplifyResults(rawResults1)
  expect_snapshot(simplifiedResults1)

  rawResults2 <- temp[[2L]]
  simplifiedResults2 <- simplifyResults(rawResults2)
  expect_snapshot(simplifiedResults2)

  formatOptions(debug = TRUE)
  expect_snapshot(print(simplifiedResults1, short = TRUE, indent = 3))
  expect_snapshot(print(simplifiedResults2, short = TRUE, indent = 4))

})

test_that("Distributions equals its snapshot", {

  resetFormatOptions()

  temp <- loadRawResults("Distributions.rds")

  rawResults1 <- temp[[1L]]
  simplifiedResults1 <- simplifyResults(rawResults1)
  expect_snapshot(simplifiedResults1)

  formatOptions(debug = TRUE)
  expect_snapshot(print(simplifiedResults1, short = TRUE, indent = 2))

})
