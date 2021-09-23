source("R/simplifyResults.R")

resultsFile <- file.path("results_objects", "Descriptives.rds")
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
# debugonce(simplifyResults)
simp <- simplifyResults(results)


foo <- function(x) {
  tibble::tibble(name = names(x), class = unname(sapply(x, \(x) class(x)[1])))
}

results$results$tables$collection$tables_debSame
foo(simp)
foo(simp$`Frequency Tables`)
foo(simp$)

opts2 <- options
opts2$plotCorrelationMatrix <- FALSE
opts2$splitPlots <- FALSE
opts2$transposeMainTable <- TRUE
results2 <- runAnalysis("Descriptives", "test.csv", opts2)

simp2 <- simplifyResults(results2)
print(simp2$`Descriptive Statistics`, has_row_id = FALSE)
undebug(simplifyTable)
simp2$`Frequency Tables`$`Frequencies for facFive`
simp2$`Frequency Tables`$`Frequencies for debSame`

tibble::tibble(a = c("0", ""))

cat(simp2$`Descriptive Statistics`$data[1, ])

simp2$`Frequency Tables`$`Frequencies for facFive`
results$results$.meta
# TODO - this table is not transposed!
simp$`Descriptive Statistics`

# the type should not be character!
simp$`Frequency Tables`$`Frequencies for facFive`$Valid.Percent # bad
simp$`Frequency Tables`$`Frequencies for facFive`$Percent       # good
simp$`Frequency Tables`$`Frequencies for facFive`

names(simp)

