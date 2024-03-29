library(tempSyntaxPackage)

resultsFile <- file.path("tests", "testthat", "rawResults", "Descriptives.rds")
if (!file.exists(resultsFile)) {

  path <- normalizePath("../../jaspDescriptives")
  renv::activate(path)

  jaspTools::setPkgOption("module.dirs", path)
  jaspTools::setPkgOption("reinstall.modules", FALSE)

  options <- jaspTools::analysisOptions("Descriptives")
  options$variables <- c("contNormal", "contGamma", "facFive", "contcor1", "contcor2", "debSame", "contExpon", "contWide")
  options$splitby <- "contBinom"
  options$frequencyTables <- TRUE
  options$splitPlots <- TRUE
  options$plotCorrelationMatrix <- TRUE
  results1 <- jaspTools::runAnalysis("Descriptives", "test.csv", options, view = FALSE)

  opts2 <- options
  opts2$plotCorrelationMatrix <- FALSE
  opts2$splitPlots <- FALSE
  opts2$transposeMainTable <- TRUE
  results2 <- jaspTools::runAnalysis("Descriptives", "test.csv", opts2, view = FALSE)

  saveRDS(list(results1 = results1, results2 = results2), file = resultsFile)

} else {
  library(jaspTools)
  tmp <- readRDS(resultsFile)
  results1 <- tmp$results1
  results2 <- tmp$results2
}

jaspTools::view(results1)
simp1 <- simplifyResults(results1)
simp1
simp1$`Descriptive Statistics`


jaspTools::view(results2)
simp2 <- simplifyResults(results2)
simp2

formatOptions(indent = 2, debug = TRUE, short = TRUE)
simp2
