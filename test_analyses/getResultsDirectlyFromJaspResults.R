finishJaspResults2 <- function(jaspResultsCPP, calledFromAnalysis = TRUE) {

  assign("jaspResultsCPP", jaspResultsCPP, envir = .GlobalEnv)
  jaspResultsCPP$prepareForWriting()
  newState <- list(figures = jaspResultsCPP$getPlotObjectsForState(),
                   other = jaspResultsCPP$getOtherObjectsForState())
  jaspResultsCPP$relativePathKeep <- jaspBase::: .saveState(newState)$relativePath
  returnThis <- NULL
  if (calledFromAnalysis) {
    returnThis <- list(keep = jaspResultsCPP$getKeepList())
    jaspResultsCPP$complete()
  } else {
    jaspResultsCPP$saveResults()
    jaspResultsCPP$finishWriting()
  }
  return(returnThis)
}

jaspBase::assignFunctionInPackage(
  fun     = finishJaspResults2,
  name    = "finishJaspResults",
  package = "jaspBase"
)

record <- list(jaspResults = list(Package = "jaspResults", Version = "1.15", Source = "Local",
                                  RemoteType = "local", RemoteUrl = "/home/don/github/jasp-desktop/R-Interface/jaspResults",
                                  Cacheable = FALSE, Path = "/home/don/github/jasp-desktop/R-Interface/jaspResults"))

options(renv.config.install.verbose = TRUE)
renv::install(record)

library(tempSyntaxPackage)
library(jaspResults)

modulesRoot <- "~/github/jaspModules/"

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

opts2 <- options
opts2$displayPairwise <- TRUE
opts2$plotHeatmap <- TRUE
results2 <- jaspTools::runAnalysis("Correlation", "test.csv", opts2)

results2R <- jaspResultsCPP$toRObject()

simp2 <- tempSyntaxPackage::simplifyResults(results2)

names(simp2)
simp2
names(results2R)
results2R$mainTable$`Partial Correlation Table`


str(results2R, max.level = 3)

results2R$assumptionsContainer$`Partial Correlation Table`
results2R$assumptionsContainer$`Partial Correlation Table`

results2R$results$kendall$`Partial Pearson's r`

results2R$mainTable$multivariateShapiro
