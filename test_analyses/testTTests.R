library(tempSyntaxPackage)

resultsFile <- file.path("results_objects", "TTestBayesianOneSample.rds")
if (!file.exists(resultsFile)) {
  path <- normalizePath("../../jaspTTests")
  renv::activate(path)
  library(jaspTools)
  jaspTools::setPkgOption("module.dirs", path)
  jaspTools::setPkgOption("reinstall.modules", FALSE)

  options <- jaspTools::analysisOptions("TTestBayesianOneSample")
  options$variables <- c("contNormal", "contGamma")
  options$descriptives <- TRUE
  options$descriptivesPlots <- TRUE
  options$descriptivesPlotsCredibleInterval <- 0.65
  options$plotPriorAndPosterior <- TRUE
  results <- runAnalysis("TTestBayesianOneSample", "test.csv", options)
  saveRDS(results, file = resultsFile)
} else {
  library(jaspTools)
  results <- readRDS(resultsFile)
}

simp <- simplifyResults(results)
simp

options("jaspDebug" = TRUE)
simp$`Bayesian One Sample T-Test`
print(simp, short = TRUE, indent = 2)

simp$`Bayesian One Sample T-Test`
simp$`Inferential Plots`$contNormal$`Prior and Posterior`$`Prior and Posterior`
simp$Descriptives
simp$`Descriptives Plots`
simp$`Inferential Plots`$contNormal$`Prior and Posterior`$`Prior and Posterior`
simp$`Inferential Plots`$contGamma$`Prior and Posterior`$`Prior and Posterior`
simp$`Descriptives Plots`$contNormal$contNormal

simp$`Descriptives Plots`$contNormal

options("jaspDebug" = FALSE)
print(simp, short = TRUE, indent = 2)


ttt <- simp$descriptivesContainer$Descriptives
ttt
attributes(ttt)

meta <- attr(ttt, "meta")
meta$fields$overTitle[2:3] <- paste(letters[1:26], collapse = "")
attr(ttt, "meta") <- meta
ttt

subset_cols_jaspTableWrapper <- function(tbl, idx) {
  tblNew <- tbl[, idx]
  attributes(tblNew)$meta <- attributes(tbl)$meta
  attributes(tblNew)$meta$fields <- attributes(tblNew)$meta$fields[idx, ]
  tblNew
}
ttt
dim(ttt)
idx <- c(2:3, 6:7)
eee

subset_cols_jaspTableWrapper(ttt, 2:3)

simp$ttestContainer$`Bayesian One Sample T-Test`

simp$ttestContainer
names(simp)



