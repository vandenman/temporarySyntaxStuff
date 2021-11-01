library(tempSyntaxPackage)

modulesRoot <- "~/github/jaspModules/"
resultsFile <- file.path("tests", "testthat", "rawResults", "Correlation.rds")
if (!file.exists(resultsFile)) {

  path <- file.path(modulesRoot, "jaspRegression")
  renv::activate(path)

  jaspTools::setPkgOption("module.dirs", path)
  jaspTools::setPkgOption("reinstall.modules", FALSE)

  options <- jaspTools::analysisOptions("Correlation")
  options$variables <- c("contNormal", "contcor1", "contcor2")
  options$conditioningVariables <- "contGamma"
  options$spearman <- TRUE
  options$kendallsTauB <- TRUE
  options$confidenceIntervals <- TRUE
  options$reportSignificance <- TRUE
  options$sampleSize <- TRUE
  options$VovkSellkeMPR <- TRUE
  options$multivariateShapiro <- TRUE
  options$pairwiseShapiro <- TRUE
  results1 <- jaspTools::runAnalysis("Correlation", "test.csv", options, view = FALSE)

  opts2 <- options
  opts2$displayPairwise <- TRUE
  options$multivariateShapiro <- FALSE
  options$pairwiseShapiro <- FALSE
  results2 <- jaspTools::runAnalysis("Correlation", "test.csv", opts2, view = FALSE)

  saveRDS(list(results1 = results1, results2 = results2), file = resultsFile)

} else {
  library(jaspTools)
  tmp <- readRDS(resultsFile)
  results1 <- tmp$results1
  results2 <- tmp$results2
}

results2
r2 <- jaspResultsCPP$toRObject()

names(results2$results)
names(r2)


results1

jaspTools::view(results1)

debugonce(tempSyntaxPackage:::getFieldsAsDf)
debugonce(tempSyntaxPackage:::listOfListsToDataFrame)
debugonce(tempSyntaxPackage:::simplifyTable)

# so this does not work because the correlation code mixes data types (numeric and character)
# we should probably detect this and use lists to contain these mixed types
simp1 <- simplifyResults(results1)
simp1

debugonce(tempSyntaxPackage:::tbl_format_setup.jaspTableWrapper)
simp1$`Partial Correlation Table`

as.data.frame(simp1$`Partial Correlation Table`)


jaspTools::view(results2)
simp2 <- simplifyResults(results2)
simp2

# hack finishJaspResults
# store logical stuff in environment

jaspBase::runJaspResults()

jaspResultsCPP$prepareForWriting()
