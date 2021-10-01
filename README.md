# Sexual mixing by diagnosed HIV status and pre-exposure prophylaxis use among men who have sex with men: stochastic reclassification to address information bias in egocentric network data
This repository holds the source to code to reproduce the analysis featured in our population-level estimates of sexual mixing. This study estimated mixing using a stochastic reclassification model to correct for information bias. We compared the estimates to results from mixing matrices including unknown values as well as matrices based on complete-case analyses.

Additional details may be found in the pre-print article:

**Pre-print:**

## Abstract

**Background:** Population-level estimates of sexual network mixing are needed to parameterize prediction models of pre-exposure prophylaxis (PrEP) effectiveness to prevent human immunodeficiency virus (HIV) among men who have sex with men (MSM). Estimates obtained by egocentric sampling are vulnerable to information bias due to incomplete respondent knowledge.

**Methods:** We estimated patterns of serosorting and PrEP sorting among MSM in the United States using data from a 2017–2019 egocentric sexual network study. Respondents served as proxies to report the HIV status (test-negative, diagnosed HIV, or unknown) and PrEP use (ever, never, or unknown) of recent sexual partners. We contrasted results from a complete-case analysis (unknown HIV and PrEP excluded) versus a bias analysis with respondent-reported data stochastically reclassified to simulate unobserved self-reported data from sexual partners.

**Results:** We found strong evidence of preferential partnering across analytical approaches. The reclassification bias analysis showed concordance among MSM with diagnosed HIV (39.3%; 95% simulation interval: 30.9, 46.0), MSM who used PrEP (31.9%; 21.0, 37.4), and MSM who did not use PrEP (82.6%; 79.3, 87.1). The fraction of partners with diagnosed HIV was higher among MSM who used PrEP (11.1%; 8.6, 13.5) compared to MSM who did not use PrEP (3.7%; 2.7, 4.6). Comparatively, the complete-case analysis showed higher fractions of partners with diagnosed HIV and those who used PrEP, across all strata, and lower fractions of partners who did not use PrEP.

**Discussion:** Serosorting and PrEP sorting among MSM may influence HIV transmission dynamics and effectiveness of PrEP at the population level. Complete-case analyses may misestimate population-level mixing. Reclassification analyses can reduce bias, but validation data are needed to verify results.

## Model Code

These models are written and executed in the R statistical software language. To run these files, it is necessary to first install a number of packages.

This is accomplished with the `renv` package in R. First install `renv` (if you do not already have it installed) and run:


```r
renv::init()
```


in your project directory. Select the option to restore the package set from the "renv.lock" file. Currently, this requires access to the `ARTnetData` package, which requires a limited access data use agreement due to the sensitive nature of those study data. Please contact the corresponding author for access.

The analysis also requires the `INLA` package, which is currently unavailable on CRAN. To install `INLA`, visit https://www.r-inla.org/download-install and follow the instructions.