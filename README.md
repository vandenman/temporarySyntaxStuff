# A temporary repo/ package for the syntax project

All relevant functions so far are in `R/simplifyResults.R`.
The working directory is always assumed to be the top level of the repository.

# Example usage

In general usage looks like this:
```r
rawResults <- jaspTools::runAnalysis(...)
simplifiedResults <- tempSyntaxPackage::simplifyResults(rawResults)
```

For a Bayesian one-sample T-test this may look like this:
```r
library(tempSyntaxPackage)
options <- jaspTools::analysisOptions("TTestBayesianOneSample")
options$variables <- c("contNormal", "contGamma")
options$descriptives <- TRUE
options$descriptivesPlots <- TRUE
options$descriptivesPlotsCredibleInterval <- 0.65
options$plotPriorAndPosterior <- TRUE
rawResults <- runAnalysis("TTestBayesianOneSample", "test.csv", options)

simplifiedResults <- simplifyResults(rawResults)
simplifiedResults
```
where the last statement prints:
```
Bayesian One Sample T-Test
─────────────────────────────────
                  BF₁₀    error %
─────────────────────────────────
contNormal   5.082e- 1  2.809e- 5
contGamma    3.872e+20  4.971e-27
─────────────────────────────────
For all tests, the alternative
hypothesis specifies that the
population mean differs from 0.

Inferential Plots

    contNormal
    
        Prior and Posterior
               +--+--+---+--+-*+
             5 +              *+
               |             **|
               |  *****      * |
             0 + **   ***   ** +
               | *      *****  |
               |**             |
            -5 +*              +
               +*-+--+---+--+--+
                 -4 -2   0  2   
    
    contGamma
    
        Prior and Posterior
               +--+--+---+--+-*+
             5 +              *+
               |             **|
               |  *****      * |
             0 + **   ***   ** +
               | *      *****  |
               |**             |
            -5 +*              +
               +*-+--+---+--+--+
                 -4 -2   0  2   

Descriptives
────────────────────────────────────────────────────────────────
                                          65% Credible Interval 
                                         ───────────────────────
               N     Mean     SD      SE       Lower       Upper
────────────────────────────────────────────────────────────────
contNormal   100  -0.1887  1.058  0.1058     -0.2881    -0.08936
contGamma    100   2.033   1.532  0.1532      1.889      2.177  
────────────────────────────────────────────────────────────────

Descriptives Plots

    contNormal
           +--+--+---+--+-*+
         5 +              *+
           |             **|
           |  *****      * |
         0 + **   ***   ** +
           | *      *****  |
           |**             |
        -5 +*              +
           +*-+--+---+--+--+
             -4 -2   0  2   
    
    contGamma
           +--+--+---+--+-*+
         5 +              *+
           |             **|
           |  *****      * |
         0 + **   ***   ** +
           | *      *****  |
           |**             |
        -5 +*              +
           +*-+--+---+--+--+
             -4 -2   0  2   
```
There are also some options for more compact printing and adding debug information:
```r
debugOn() # add debug information
print(simplifiedResults, short = TRUE)
```
prints
```
jaspResultsWrapper
    jaspTableWrapper: Bayesian One Sample T-Test
    jaspContainerWrapper: Inferential Plots
        jaspContainerWrapper: contNormal
            jaspPlotWrapper: Prior and Posterior
        jaspContainerWrapper: contGamma
            jaspPlotWrapper: Prior and Posterior
    jaspTableWrapper: Descriptives
    jaspContainerWrapper: Descriptives Plots
        jaspPlotWrapper: contNormal
        jaspPlotWrapper: contGamma
```
`debugOn()` ensures the type of the jaspWrapper is printed, while `short = TRUE` implies we only print the structure of the jaspWrappers and not the contents.
To turn off debug information use `debugOff()`.
`debugOn()` also adds type information to the tables:
```r
simplifiedResults$`Bayesian One Sample T-Test`
```
prints
```
jaspTableWrapper: Bayesian One Sample T-Test
─────────────────────────────────
                  BF₁₀    error %
─────────────────────────────────
<chr>          <fit:4>    <fit:4>  # <- this row is normally not printed.
contNormal   5.082e- 1  2.809e- 5
contGamma    3.872e+20  4.971e-27
─────────────────────────────────
For all tests, the alternative
hypothesis specifies that the
population mean differs from 0.
```

