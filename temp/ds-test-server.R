## SETUP
## ============================================

client_pkgs = c("DSI", "DSOpal", "dsBaseClient")
install.packages(client_pkgs, repos = c(getOption("repos"), "https://cran.obiba.org"))

knitr::kable(installed.packages()[client_pkgs, c("Version", "Built")])

base_dir = here::here()
surl     = "https://opal-demo.obiba.org/"
username = "administrator"
password = "password"

## Install packages on test server:
opal = opalr::opal.login(username = username, password = password, url = surl)
#opal = opalr::opal.login(token = "p7wfg92h7T22MabBJ15BdtSQCcJOW1tr", url = surl)

pkgs = c("ds.predict.base", "ds.calibration", "ds.roc.glm")
for (pkg in pkgs) {
  check1 = opalr::dsadmin.install_github_package(opal = opal, pkg = pkg, username = "difuture-lmu")
  if (! check1)
    warning("Was not able to install ", pkg, "!")
  else
    message("Install package ", pkg, "!")

  check2 = opalr::dsadmin.publish_package(opal = opal, pkg = pkg)
  if (! check2)
    stop("Was not able to publish methods of ", pkg, "!")
  else
    message("Publish methods from ", pkg, "!")
}


## TEST DATA AND MODEL
## ============================================

local_data1 = read.csv(paste0(base_dir, "/data/ds-test-cnsim1.csv"))
local_data2 = read.csv(paste0(base_dir, "/data/ds-test-cnsim2.csv"))

local_data = rbind(local_data1, local_data2)
local_data$GENDER = as.factor(local_data$GENDER)

## Train model:
mod = glm(DIS_DIAB ~ LAB_TSC + LAB_TRIG + LAB_HDL + GENDER, data = local_data, family = binomial())



## ROC-GLM
## ============================================

## DataSHIELD login:
library(DSI)
library(DSOpal)
library(dsBaseClient)

devtools::load_all("~/repos/ds.predict.base")
devtools::load_all("~/repos/ds.calibration")
devtools::load_all("~/repos/ds.roc.glm")

## DataSHIELD login:
## ----------------------------------------

builder = newDSLoginBuilder()

builder$append(
  server   = "ds-test-server-dummy1",
  url      = surl,
  user     = username,
  password = password,
  table    = "CNSIM.CNSIM1"
)
builder$append(
  server   = "ds-test-server-dummy2",
  url      = surl,
  user     = username,
  password = password,
  table    = "CNSIM.CNSIM2"
)

logindata = builder$build()


connections = datashield.login(logins = logindata, assign = TRUE)
#  opts = list(ssl_verifyhost = 0, ssl_verifypeer=0))

# test assignment:
datashield.assign(connections, 'some.data', quote(c(1:10)))

# test aggregate:
datashield.aggregate(connections, expr = quote(NROW(D)))

### Get available tables:
datashield.symbols(connections)




## Preperation for ROC-GLM
## ----------------------------------------

pushObject(connections, mod)
datashield.symbols(connections)

### Predict uploaded model on server data. Scores are stored in an object called `pred`:
predictModel(connections, mod, "pred", "D", predict_fun = "predict(mod, newdata = D, type = 'response')")

### The `pred` object is later used for the ROC-GLM.

### Get object on server:
datashield.symbols(connections)



## Calculate and visualize ROC-GLM
## ========================================

### Now, calculate ROC-GLM:
roc_glm = dsROCGLM(connections, "D$DIS_DIAB", "pred")
roc_glm

truth_name = "D$DIS_DIAB[-1]"
pred_name = "pred[-1]"
n_scores = DSI::datashield.aggregate(conns = connections, paste0("getNegativeScores(\"", truth_name, "\", \"", pred_name, "\")"))


### And plot it:
plot(roc_glm)



## Logout from DataSHIELD server
## ========================================

DSI::datashield.logout(conns = connections, save = FALSE)




