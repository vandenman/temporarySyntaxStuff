library(tempSyntaxPackage)

resultsFile <- file.path("tests", "testthat", "rawResults", "Distributions.rds")
if (!file.exists(resultsFile)) {

  path <- normalizePath("../../jaspDistributions")
  renv::activate(path)

  jaspTools::setPkgOption("module.dirs", path)
  jaspTools::setPkgOption("reinstall.modules", FALSE)

  # test-ldgaussianunivariate.R
  options <- jaspTools::analysisOptions("LDgaussianunivariate")
  options$.meta <- list(newVariableName = list(containsColumn = TRUE), variable = list(containsColumn = TRUE))
  options$andersonDarling <- TRUE
  options$ciInterval <- TRUE
  options$ciIntervalInterval <- 0.95
  options$cramerVonMisses <- TRUE
  options$ecdf <- TRUE
  options$estCDF <- TRUE
  options$estPDF <- TRUE
  options$explanatoryText <- TRUE
  options$highlightDensity <- TRUE
  options$highlightProbability <- TRUE
  options$histogram <- TRUE
  options$kolmogorovSmirnov <- TRUE
  options$methodMLE <- TRUE
  options$moments <- TRUE
  options$momentsUpTo <- 10
  options$newVariableName <- ""
  options$outputEstimates <- TRUE
  options$outputSE <- TRUE
  options$parsSupportMoments <- TRUE
  options$plotCDF <- TRUE
  options$plotQF <- TRUE
  options$ppplot <- TRUE
  options$qqplot <- TRUE
  options$shapiroWilk <- TRUE
  options$summary <- TRUE
  options$variable <- "Normal100(mu=0,sigma=1)"
  set.seed(1)
  results1 <- jaspTools::runAnalysis("LDgaussianunivariate", "Distributions.csv", options)

  saveRDS(list(results1 = results1), file = resultsFile)

} else {
  library(jaspTools)
  tmp <- readRDS(resultsFile)
  results1 <- tmp$results1
}

jaspTools::view(results1)

# debugonce(tempSyntaxPackage:::simplifyResults)
simp <- simplifyResults(results1)
simp
debugOn()
print(simp, short = TRUE)
