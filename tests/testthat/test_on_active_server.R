context("Check if functionality works on the DataSHIELD test sever")

test_that("all methods can be used and produce reasonable output", {
  irisb = iris
  irisb$y = ifelse(iris$Species == "setosa", 1, 0)
  irisb$Species = NULL

  mod <<- glm(y ~ Sepal.Length, data = irisb, family = binomial())
  p <<- predict(mod, type = "response")

  surl     = "https://opal-demo.obiba.org/"
  username = "administrator"
  password = "password"

  opal = opalr::opal.login(username = username, password = password, url = surl)

  pkgs = c("dsPredictBase", "dsROCGLM")
  for (pkg in pkgs) {
    check1 = opalr::dsadmin.install_github_package(opal = opal, pkg = pkg, username = "difuture-lmu", ref = "main")
    if (! check1)
      stop("[", Sys.time(), "] Was not able to install ", pkg, "!")

    check2 = opalr::dsadmin.publish_package(opal = opal, pkg = pkg)
    if (! check2)
      stop("[", Sys.time(), "] Was not able to publish methods of ", pkg, "!")
  }

  library(DSI)
  library(DSOpal)
  library(dsBaseClient)
  library(dsPredictBase)

  builder = newDSLoginBuilder()

  surl     = "https://opal-demo.obiba.org/"
  username = "administrator"
  password = "password"

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
  connections <<- datashield.login(logins = builder$build(), assign = TRUE)

  datashield.assign(connections, "valid", quote(c(rep(1, 50), rep(0, 100))))
  pushObject(connections, mod)
  datashield.assign(connections, "dat", quote(iris))
  predictModel(connections, mod, "pred", "dat", predict_fun = "predict(mod, newdata = D, type = 'response')")

  expect_equal(l2sens("iris", "p", nbreaks = 10)$l2sens, dsL2Sens(connections, "dat", "pred", nbreaks = 10))
  expect_message(expect_warning({
    roc_glm = dsROCGLM(connections, "valid", "pred", nbreaks = 10, dat_name = "iris",
      seed_object = "pred")
  }))
  expect_equal(class(roc_glm), "ROC.GLM")

  expect_message(expect_warning({
    roc_glm2 = dsROCGLM(connections, "valid", "pred", nbreaks = 10, dat_name = "iris",
      seed_object = "pred")
  }))
  expect_equal(roc_glm, roc_glm2)

  expect_silent({gg = plot(roc_glm)})
  expect_true(inherits(gg, "ggplot"))
  expect_output(print(roc_glm))

  datashield.logout(connections)
})