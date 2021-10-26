# TTestBayesianOneSample equals its snapshot

    Code
      simplifiedResults
    Output
      Bayesian One Sample T-Test
      ─────────────────────────────────
                  BF₁₀       error %   
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
                  N    Mean     SD     SE      Lower       Upper      
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

---

    Code
      print(simplifiedResults, short = TRUE, indent = 2)
    Output
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

# Descriptives equals its snapshot

    Code
      simplifiedResults1
    Output
      Descriptive Statistics
      ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
                       contNormal      contGamma     facFive      contcor1        contcor2      debSame      contExpon          contWide     
                     ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
                     0       1       0      1      0     1     0      1       0      1         0    1    0        1        0        1        
      ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
               Valid      58      42     58     42    58    42     58      42     58        42   58   42       58       42       58        42
             Missing       0       0      0      0     0     0      0       0      0         0    0    0        0        0        0         0
                Mean -0.1201 -0.2835  2.155  1.864 3.121 2.833 0.1995 -0.1504 0.1256 -0.007464 12.3 12.3 5.155e47 8.190e55 1.092e98 -2.890e98
      Std. Deviation   1.106  0.9946  1.721  1.225 1.488 1.324  1.046  0.9376  1.046    0.9504    0    0 3.926e48 5.308e56 4.826e99  5.588e99
             Minimum  -2.337  -3.024 0.1973 0.3430     1     1 -2.147  -1.586 -2.710    -2.418 12.3 12.3 1.74e-49 1.25e-32 -8.96e99  -8.89e99
             Maximum   3.356   2.179  8.768  5.870     5     5  2.362   1.923  2.135     1.832 12.3 12.3  2.99e49  3.44e57  8.55e99   8.93e99
      ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
      
      Frequency Tables
      
          Frequencies for facFive
          ──────────────────────────────────────────────────────────────────────────
          contBinom  facFive  Frequency  Percent  Valid Percent  Cumulative Percent 
          ──────────────────────────────────────────────────────────────────────────
          0          1                12     20.7           20.7                20.7
                     2                 9     15.5           15.5                36.2
                     3                12     20.7           20.7                56.9
                     4                10     17.2           17.2                74.1
                     5                15     25.9           25.9               100.0
                     Missing           0      0.0                                   
                     Total            58    100.0                                   
          1          1                 8     19.0           19.0                19.0
                     2                11     26.2           26.2                45.2
                     3                 8     19.0           19.0                64.3
                     4                10     23.8           23.8                88.1
                     5                 5     11.9           11.9               100.0
                     Missing           0      0.0                                   
                     Total            42    100.0                                   
          ──────────────────────────────────────────────────────────────────────────
          The following variables have more than 10 distinct values and are
          omitted: contNormal, contGamma, contcor1, contcor2, contExpon, contWide.
          
          Frequencies for debSame
          ──────────────────────────────────────────────────────────────────────────
          contBinom  debSame  Frequency  Percent  Valid Percent  Cumulative Percent 
          ──────────────────────────────────────────────────────────────────────────
          0          12.3             58    100.0          100.0               100.0
                     Missing           0      0.0                                   
                     Total            58    100.0                                   
          1          12.3             42    100.0          100.0               100.0
                     Missing           0      0.0                                   
                     Total            42    100.0                                   
          ──────────────────────────────────────────────────────────────────────────
      
      Correlation plots
      
          0
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
          
          1
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
      
      Boxplots
      
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
          
          facFive
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
          
          contcor1
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
          
          contcor2
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
          
          debSame
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
          
          contExpon
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
          
          contWide
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

---

    Code
      simplifiedResults2
    Output
      Descriptive Statistics
      ────────────────────────────────────────────────────────────────────────────────
                     Valid  Missing  Mean        Std. Deviation  Minimum     Maximum  
      ────────────────────────────────────────────────────────────────────────────────
      contNormal  0      58        0  -1.201e- 1       1.106e+ 0  -2.337e+ 0  3.356e 0
      contNormal  1      42        0  -2.835e- 1       9.946e- 1  -3.024e+ 0  2.179e 0
      contGamma   0      58        0   2.155e+ 0       1.721e+ 0   1.973e- 1  8.768e 0
      contGamma   1      42        0   1.864e+ 0       1.225e+ 0   3.430e- 1  5.870e 0
      facFive     0      58        0   3.121e+ 0       1.488e+ 0   1    e+ 0  5    e 0
      facFive     1      42        0   2.833e+ 0       1.324e+ 0   1    e+ 0  5    e 0
      contcor1    0      58        0   1.995e- 1       1.046e+ 0  -2.147e+ 0  2.362e 0
      contcor1    1      42        0  -1.504e- 1       9.376e- 1  -1.586e+ 0  1.923e 0
      contcor2    0      58        0   1.256e- 1       1.046e+ 0  -2.710e+ 0  2.135e 0
      contcor2    1      42        0  -7.464e- 3       9.504e- 1  -2.418e+ 0  1.832e 0
      debSame     0      58        0   1.23 e+ 1       0           1.23 e+ 1  1.23 e 1
      debSame     1      42        0   1.23 e+ 1       0           1.23 e+ 1  1.23 e 1
      contExpon   0      58        0   5.155e+47       3.926e+48   1.74 e-49  2.99 e49
      contExpon   1      42        0   8.190e+55       5.308e+56   1.25 e-32  3.44 e57
      contWide    0      58        0   1.092e+98       4.826e+99  -8.96 e+99  8.55 e99
      contWide    1      42        0  -2.890e+98       5.588e+99  -8.89 e+99  8.93 e99
      ────────────────────────────────────────────────────────────────────────────────
      
      Frequency Tables
      
          Frequencies for facFive
          ──────────────────────────────────────────────────────────────────────────
          contBinom  facFive  Frequency  Percent  Valid Percent  Cumulative Percent 
          ──────────────────────────────────────────────────────────────────────────
          0          1                12     20.7           20.7                20.7
                     2                 9     15.5           15.5                36.2
                     3                12     20.7           20.7                56.9
                     4                10     17.2           17.2                74.1
                     5                15     25.9           25.9               100.0
                     Missing           0      0.0                                   
                     Total            58    100.0                                   
          1          1                 8     19.0           19.0                19.0
                     2                11     26.2           26.2                45.2
                     3                 8     19.0           19.0                64.3
                     4                10     23.8           23.8                88.1
                     5                 5     11.9           11.9               100.0
                     Missing           0      0.0                                   
                     Total            42    100.0                                   
          ──────────────────────────────────────────────────────────────────────────
          The following variables have more than 10 distinct values and are
          omitted: contNormal, contGamma, contcor1, contcor2, contExpon, contWide.
          
          Frequencies for debSame
          ──────────────────────────────────────────────────────────────────────────
          contBinom  debSame  Frequency  Percent  Valid Percent  Cumulative Percent 
          ──────────────────────────────────────────────────────────────────────────
          0          12.3             58    100.0          100.0               100.0
                     Missing           0      0.0                                   
                     Total            58    100.0                                   
          1          12.3             42    100.0          100.0               100.0
                     Missing           0      0.0                                   
                     Total            42    100.0                                   
          ──────────────────────────────────────────────────────────────────────────

---

    Code
      print(simplifiedResults1, short = TRUE, indent = 3)
    Output
      jaspResultsWrapper
         jaspTableWrapper: Descriptive Statistics
         jaspContainerWrapper: Frequency Tables
            jaspTableWrapper: Frequencies for facFive
            jaspTableWrapper: Frequencies for debSame
         jaspContainerWrapper: Correlation plots
            jaspPlotWrapper: 0
            jaspPlotWrapper: 1
         jaspContainerWrapper: Boxplots
            jaspPlotWrapper: contNormal
            jaspPlotWrapper: contGamma
            jaspPlotWrapper: facFive
            jaspPlotWrapper: contcor1
            jaspPlotWrapper: contcor2
            jaspPlotWrapper: debSame
            jaspPlotWrapper: contExpon
            jaspPlotWrapper: contWide

---

    Code
      print(simplifiedResults2, short = TRUE, indent = 4)
    Output
      jaspResultsWrapper
          jaspTableWrapper: Descriptive Statistics
          jaspContainerWrapper: Frequency Tables
              jaspTableWrapper: Frequencies for facFive
              jaspTableWrapper: Frequencies for debSame

# Distributions equals its snapshot

    Code
      simplifiedResults1
    Output
      
          <h3> Demonstration of the normal distribution </h3>
      This demonstration is divided into four parts.
      The first part displays the normal distribution, its probability density function, cumulative distribution function, and quantile function.
      The second part allows you to generate data from the normal distribution, compute descriptive statistics, and display descriptive plots.
      The third part allows you to estimate the parameters of the normal distribution.
      The fourth part allows you to check the fit of the normal distribution to the data.
      
      <b>References</b>
      
      Blitzstein, J. K., & Hwang, J. (2014). <i>Introduction to probability.</i> Chapman and Hall/CRC.
      
      Leemis, L. M., & Pasupathy, R. (2019). The ties that bind. <i>Significance, 16</i>(4), 8–9.
      
      For relationships with other distributions, visit www.math.wm.edu/~leemis/chart/UDR/UDR.html.
      
      https://en.wikipedia.org/wiki/List_of_probability_distributions
      
      Parameters, Support, and Moments
          <b>Parameters</b>
            mean: &mu; ∈ ℝ 
       variance: &sigma;<sup>2</sup> ∈ ℝ<sup>+</sup>
      
            <b>Support</b>
            x ∈ ℝ
      
            <b>Moments</b>
            E(X) = &mu;
            Var(X) = &sigma;<sup>2</sup>
      
      Probability Density Function
      
          
              The probability density function (PDF), usually denoted as f(x), is a function of a random variable X.
          The value of f(x) provides the relative likelihood that a realization of the random variable X yields a value equal to x.
          More formally, the PDF is defined as a derivative of a cumulative distribution function (CDF).
      
          The density plot displays the probability density function of a random variable.
          The <i>y</i>-axis displays the value of the density function for a particular value of the random variable (displayed on the <i>x</i>-axis).
          
          Density Plot
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
      
      Cumulative Distribution Function
      
          
              The cumulative distribution function (CDF), usually denoted as F(x), is a function of a random variable X.
          The value of F(x) provides the probability that a realization of the random variable X yields a value that is equal to or smaller than x.
      
          The cumulative probability plot displays the cumulative distribution function of a random variable.
          The <i>y</i>-axis displays the value of the cumulative distribution function for a particular value of the random variable (displayed on the <i>x</i>-axis).
          
          Cumulative Probability Plot
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
      
      Quantile Function
      
          
              The quantile function, usually denoted as Q(p), is the inverse of the cumulative distribution function.
          The function gives the quantile such that the probability of the random variable being less than or equal to that value equals the given probability p.
      
          The quantile plot displays the quantile function.
          The <i>y</i>-axis displays the quantile of which the probability that the random variable is less or equal to that value is equal to p (displayed on the <i>x</i>-axis).
          
          Quantile Plot
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
      
      Overview - Normal100(mu=0,sigma=1)
      
          Descriptives
          ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
          Variable                 n    Mean    Variance  Std. deviation  Minimum  25% Quantile  Median  75% Quantile  Maximum 
          ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
          Normal100(mu=0,sigma=1)   100  0.1304    0.8364          0.9146   -2.215       -0.4942  0.1139        0.7043    2.402
          ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
          
          Observed Moments
          ────────────────────────────
          Moment  Raw       Central   
          ────────────────────────────
                1    0.1304    0.1304 
                2    0.8451    0.8281 
                3    0.2785   -0.04770
                4    2.070     2.010  
                5    1.028    -0.2927 
                6    7.420     7.134  
                7    4.881    -1.683  
                8   31.62     29.98   
                9   25.86     -9.626  
               10  147.8     137.4    
          ────────────────────────────
          
          Histogram
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
          
          Empirical Cumulative Distribution
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
      
      Maximum likelihood
      
          Estimated Parameters
          ───────────────────────────────────────────────
                                             95% CI      
                                        ─────────────────
          Parameter  Estimate  SE       Lower     Upper  
          ───────────────────────────────────────────────
          μ             0.1304  0.09100  -0.04793  0.3088
          σ²            0.8281  0.1171    0.5986   1.058 
          ───────────────────────────────────────────────
          Standard errors and confidence intervals were
          calculated using the delta method.
          
          Fit Assessment
          
              Fit Statistics
              ─────────────────────────────────────
              Test                Statistic  p     
              ─────────────────────────────────────
              Kolmogorov-Smirnov     0.04292  0.993
              Cramér-von Mises       0.02180  0.995
              Anderson-Darling       0.1343   0.999
              Shapiro-Wilk           0.9961   0.994
              ─────────────────────────────────────
              
              Histogram vs. Theoretical PDF
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
              
              Q-Q plot
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
              
              Empirical vs. Theoretical CDF
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
              
              P-P plot
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

---

    Code
      print(simplifiedResults1, short = TRUE, indent = 2)
    Output
      jaspResultsWrapper
        jaspHtmlWrapper: 
        jaspHtmlWrapper: Parameters, Support, and Moments
        jaspContainerWrapper: Probability Density Function
          jaspHtmlWrapper: 
          jaspPlotWrapper: Density Plot
        jaspContainerWrapper: Cumulative Distribution Function
          jaspHtmlWrapper: 
          jaspPlotWrapper: Cumulative Probability Plot
        jaspContainerWrapper: Quantile Function
          jaspHtmlWrapper: 
          jaspPlotWrapper: Quantile Plot
        jaspContainerWrapper: Overview - Normal100(mu=0,sigma=1)
          jaspTableWrapper: Descriptives
          jaspTableWrapper: Observed Moments
          jaspPlotWrapper: Histogram
          jaspPlotWrapper: Empirical Cumulative Distribution
        jaspContainerWrapper: Maximum likelihood
          jaspTableWrapper: Estimated Parameters
          jaspContainerWrapper: Fit Assessment
            jaspTableWrapper: Fit Statistics
            jaspPlotWrapper: Histogram vs. Theoretical PDF
            jaspPlotWrapper: Q-Q plot
            jaspPlotWrapper: Empirical vs. Theoretical CDF
            jaspPlotWrapper: P-P plot

