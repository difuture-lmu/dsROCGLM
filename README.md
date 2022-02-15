
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Actions
Status](https://github.com/difuture-lmu/dsROCGLM/workflows/R-CMD-check/badge.svg)](https://github.com/difuture-lmu/dsROCGLM/actions)
[![License: LGPL
v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![codecov](https://codecov.io/gh/difuture-lmu/dsROCGLM/branch/master/graph/badge.svg?token=E8AZRM6XJX)](https://codecov.io/gh/difuture-lmu/dsROCGLM)

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

It is necessary to register the aggregate and assign methods in the OPAL
administration. The assign methods are:

**Assign methods:**

  - `rocGLMFrame`

**Aggregate methods:**

  - `getPositiveScores`
  - `getNegativeScores`
  - `calculateDistrGLMParts`

## Usage

``` r
library(DSI)
library(DSOpal)
library(DSLite)
library(dsBaseClient)


## DataSHIELD login:
## ========================================

builder = DSI::newDSLoginBuilder()

builder$append(
  server   = "ibe",
  url      = "*****''",
  user     = "***",
  password = "******",
  table    = "ProVal.KUM"
)

logindata = builder$build()
connections = DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D",
  opts = list(ssl_verifyhost = 0, ssl_verifypeer=0))

### Get available tables:
DSI::datashield.symbols(connections)

## Read test data (same as on server)
## ========================================

# We use this data to calculate a model which we want to evaluate. Here a simple logistic regression:
dat = read.csv("data/test-kum.csv")
mod = glm(gender ~ age + height, family = "binomial", data = dat)



## Preperation for ROC-GLM
## ========================================

### The ROC-GLM requires scores and true values. The function `predictModel` calculates the
### scores based on a model. In our case the logistic regression form above.

### Upload model to DataSHIELD server:
pushModel(connections, mod)

### Predict uploaded model on server data. Scores are stored in an object called `pred`:
predictModel(connections, mod, "pred", "D", predict_fun = "predict(mod, newdata = D, type = 'response')")

### The `pred` object is later used for the ROC-GLM.

### Get object on server:
DSI::datashield.symbols(connections)



## Calculate and visualize ROC-GLM
## ========================================

### Now, calculate ROC-GLM:
roc_glm = dsROCGLM(connections, "D$gender", "pred")
roc_glm

### And plot it:
plot(roc_glm)



## Logout from DataSHIELD server
## ========================================

DSI::datashield.logout(conns = connections, save = FALSE)
```
