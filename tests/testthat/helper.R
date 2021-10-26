loadRawResults <- function(name) {
  readRDS(testthat::test_path(file.path("rawResults", name)))
}
