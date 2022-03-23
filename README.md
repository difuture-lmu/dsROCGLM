
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Actions
Status](https://github.com/difuture-lmu/dsROCGLM/workflows/R-CMD-check/badge.svg)](https://github.com/difuture-lmu/dsROCGLM/actions)
[![License: LGPL
v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![codecov](https://codecov.io/gh/difuture-lmu/dsROCGLM/branch/main/graph/badge.svg?token=E8AZRM6XJX)](https://codecov.io/gh/difuture-lmu/dsROCGLM)

# ROC-GLM for DataSHIELD

The package provides functionality to conduct and visualize ROC analysis
on decentralized data. The basis is the
DataSHIELD\](<https://www.datashield.org/>) infrastructure for
distributed computing. This package provides the calculation of the
[**ROC-GLM**](https://www.jstor.org/stable/2676973?seq=1) as well as
[**AUC confidence
intervals**](https://www.jstor.org/stable/2531595?seq=1). In order to
calculate the ROC-GLM it is necessry to push models and predict them at
the servers. This is done automatically by the base package
[`dsPredictBase`](https://github.com/difuture-lmu/dsPredictBase). Note
that DataSHIELD uses an option `datashield.privacyLevel` to indicate the
minimal amount of numbers required to be allowed to share an aggregated
value of these numbers. Instead of setting the option, we directly
retrieve the privacy level from the
[`DESCRIPTION`](https://github.com/difuture-lmu/dsROCGLM/blob/master/DESCRIPTION)
file each time a function calls for it. This options is set to 5 by
default.

## Installation

At the moment, there is no CRAN version available. Install the
development version from GitHub:

``` r
remotes::install_github("difuture-lmu/dsROCGLM")
```

#### Register methods

It is necessary to register the assign and aggregate methods in the OPAL
administration. These methods are registered automatically when
publishing the package on OPAL (see
[`DESCRIPTION`](https://github.com/difuture/dsROCGLM/blob/main/DESCRIPTION)).

Note that the package needs to be installed at both locations, the
server and the analysts machine.

## Usage

A more sophisticated example is available
[here](github.com/difuture-lmu/datashield-roc-glm-demo).

``` r
library(DSI)
#> Loading required package: progress
#> Loading required package: R6
library(DSOpal)
#> Loading required package: opalr
#> Loading required package: httr
library(dsBaseClient)

library(dsPredictBase)
library(dsROCGLM)
```

#### Log into DataSHIELD server

``` r
builder = newDSLoginBuilder()

surl     = "https://opal-demo.obiba.org/"
username = "administrator"
password = "password"

builder$append(
  server   = "ds1",
  url      = surl,
  user     = username,
  password = password
)
builder$append(
  server   = "ds2",
  url      = surl,
  user     = username,
  password = password
)

connections = datashield.login(logins = builder$build(), assign = TRUE)
#> 
#> Logging into the collaborating servers
```

#### Assign iris at DataSHIELD

``` r
datashield.assign(connections, "iris", quote(iris))
datashield.assign(connections, "y", quote(c(rep(1, 50), rep(0, 100))))
```

#### Load test model, push to DataSHIELD, and calculate predictions

``` r
# Model predicts if species of iris is setosa or not.
iris$y = ifelse(iris$Species == "setosa", 1, 0)
mod = glm(y ~ Sepal.Length, data = iris, family = binomial())

# Push the model to the DataSHIELD servers using `dsPredictBase`:
pushObject(connections, mod)

# Calculate scores and save at the servers using `dsPredictBase`:
pfun =  "predict(mod, newdata = D, type = 'response')"
predictModel(connections, mod, "pred", "iris", predict_fun = pfun)

datashield.symbols(connections)
#> $ds1
#> [1] "iris" "mod"  "pred" "y"   
#> 
#> $ds2
#> [1] "iris" "mod"  "pred" "y"
```

#### Calculate l2-sensitivity

``` r
# In order to securely calculate the ROC-GLM, we have to assess the
# l2-sensitivity to set the privacy parameters of differential
# privacy adequately:
l2s = dsL2Sens(connections, "iris", "pred")
l2s

# Due to the results presented in https://arxiv.org/abs/2203.10828, we set the privacy parameters to
# - epsilon = 0.2, delta = 0.1 if i      l2s <= 0.01
# - epsilon = 0.3, delta = 0.4 if 0.01 < l2s <= 0.03
# - epsilon = 0.5, delta = 0.3 if 0.03 < l2s <= 0.05
# - epsilon = 0.5, delta = 0.5 if 0.05 < l2s <= 0.07
# - epsilon = 0.5, delta = 0.5 if 0.07 < l2s BUT results may be not good!
```

#### Calculate ROC-GLM

``` r
roc_glm = dsROCGLM(connections, truth_name = "y", pred_name = "pred",
  dat_name = "iris", seed_object = "y")
#> 
#> [2022-03-23 12:54:29] L2 sensitivity is: 0.1281
#> Warning in dsROCGLM(connections, truth_name = "y", pred_name = "pred", dat_name
#> = "iris", : l2-sensitivity may be too high for good results! Epsilon = 0.5 and
#> delta = 0.5 is used which may lead to bad results.
#> 
#> [2022-03-23 12:54:30] Setting: epsilon = 0.5 and delta = 0.5
#> 
#> [2022-03-23 12:54:30] Initializing ROC-GLM
#> 
#> [2022-03-23 12:54:30] Host: Received scores of negative response
#> [2022-03-23 12:54:30] Receiving negative scores
#> [2022-03-23 12:54:31] Host: Pushing pooled scores
#> [2022-03-23 12:54:31] Server: Calculating placement values and parts for ROC-GLM
#> [2022-03-23 12:54:32] Server: Calculating probit regression to obtain ROC-GLM
#> [2022-03-23 12:54:33] Deviance of iter1=137.2431
#> [2022-03-23 12:54:34] Deviance of iter2=121.5994
#> [2022-03-23 12:54:35] Deviance of iter3=147.7237
#> [2022-03-23 12:54:35] Deviance of iter4=140.4008
#> [2022-03-23 12:54:36] Deviance of iter5=129.2244
#> [2022-03-23 12:54:37] Deviance of iter6=123.9979
#> [2022-03-23 12:54:38] Deviance of iter7=123.1971
#> [2022-03-23 12:54:39] Deviance of iter8=124.1615
#> [2022-03-23 12:54:39] Deviance of iter9=124.5356
#> [2022-03-23 12:54:40] Deviance of iter10=124.5503
#> [2022-03-23 12:54:41] Deviance of iter11=124.5504
#> [2022-03-23 12:54:42] Deviance of iter12=124.5504
#> [2022-03-23 12:54:42] Host: Finished calculating ROC-GLM
#> [2022-03-23 12:54:42] Host: Cleaning data on server
#> [2022-03-23 12:54:42] Host: Calculating AUC and CI
#> [2022-03-23 12:54:47] Finished!
plot(roc_glm)
```

![](Readme_files/unnamed-chunk-8-1.png)<!-- -->

``` r
datashield.logout(connections)
```

## Deploy information:

**Build by root (Darwin) on 2022-03-23 12:54:50.**

This readme is built automatically after each push to the repository.
Hence, it also is a test if the functionality of the package works also
on the DataSHIELD servers. We also test these functionality in
`tests/testthat/test_on_active_server.R`. The system information of the
local and remote servers are as followed:

  - Local machine:
      - `R` version: R version 4.1.3 (2022-03-10)
      - Version of DataSHELD client packages:

| Package       | Version |
| :------------ | :------ |
| DSI           | 1.3.0   |
| DSOpal        | 1.3.1   |
| dsBaseClient  | 6.1.1   |
| dsPredictBase | 0.0.1   |
| dsROCGLM      | 0.0.1   |

  - Remote DataSHIELD machines:
      - `R` version of ds1: R version 4.1.1 (2021-08-10)
      - `R` version of ds2: R version 4.1.1 (2021-08-10)
      - Version of server packages:

| Package       | ds1: Version | ds2: Version |
| :------------ | :----------- | :----------- |
| dsBase        | 6.1.1        | 6.1.1        |
| resourcer     | 1.1.1        | 1.1.1        |
| dsPredictBase | 0.0.1        | 0.0.1        |
| dsROCGLM      | 0.0.1        | 0.0.1        |
