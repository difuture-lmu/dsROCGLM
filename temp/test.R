base_dir = here::here()



## Load packages:
## DataSHIELD login:
## ========================================

devtools::load_all(paste0(base_dir, "/../ds.calibration"))
devtools::load_all(paste0(base_dir, "/../ds.predict.base"))
devtools::load_all(paste0(base_dir, "/../ds.roc.glm"))



## DataSHIELD login:
## ========================================

library(DSI)
library(DSOpal)
library(DSLite)
library(dsBaseClient)

builder = DSI::newDSLoginBuilder()

builder$append(
  server   = "ibe",
  url      = "https://dsibe.ibe.med.uni-muenchen.de",
  user     = "ibe",
  password = "123456",
  table    = "ProVal.KUM"
)
builder$append(
  server   = "ibe2",
  url      = "https://dsibe.ibe.med.uni-muenchen.de",
  user     = "ibe",
  password = "123456",
  table    = "ProVal.KUM"
)


logindata = builder$build()
connections = DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D", opts = list(ssl_verifyhost = 0, ssl_verifypeer = 0))

### Get object on server:
DSI::datashield.symbols(connections)



## Read test data (same as on server)
## ========================================

# We use this data to calculate a model which we want to evaluate. Here a simple logistic regression:
dat = read.csv(paste0(base_dir, "/data/test-kum.csv"))
mod = glm(gender ~ age + height, family = "binomial", data = dat)



## Preperation for ROC-GLM
## ========================================

### The ROC-GLM requires scores and true values. The function `predictModel` calculates the
### scores based on a model. In our case the logistic regression form above.

### Upload model to DataSHIELD server:
pushModel(connections, mod)

### Predict uploaded model on server data. Scores are stored in an object called `pred`:
predictModel(connections, mod, "pred", "D")
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


## Calibration
## ========================================

### For calibration, we also have to specify the score and truth object.
### As for the AUC, we have `D$gender` as true vector and `pred` as
### scores.

### Calculate brier score:
dsBrierScore(connections, "D$gender", "pred")

### Calculate and plot calibration curve:
cc = dsCalibrationCurve(connections, "D$gender", "pred", 10, 3)
plotCalibrationCurve(cc)


### Logout from servers:
DSI::datashield.logout(conns = connections, save = FALSE)

