context("Check if functionality works on the DataSHIELD test sever")

test_that("all methods can be used and produce reasonable output", {
  irisb = iris
  irisb$y = ifelse(iris$Species == "setosa", 1, 0)
  irisb$y[sample(seq_len(nrow(irisb)), 30)] = 1
  irisb$Species = NULL

  mod <<- glm(y ~ Sepal.Length + Petal.Length, data = irisb, family = binomial())
  p <<- predict(mod, type = "response")

  surl     = "https://opal-demo.obiba.org/"
  username = "administrator"
  password = "password"

  opal = opalr::opal.login(username = username, password = password, url = surl)
  ref = "pkg-merge"

  # Check if package can be installed:
  expect_true(opalr::dsadmin.install_github_package(opal = opal, pkg = pkg, username = "difuture-lmu", ref = ref))
  expect_true(opalr::dsadmin.publish_package(opal = opal, pkg = pkg))

  library(DSI)
  library(DSOpal)
  library(dsBaseClient)

  builder = newDSLoginBuilder()

  builder$append(
    server   = "ds-test-server-dummy1",
    url      = surl,
    user     = username,
    password = password
  )
  builder$append(
    server   = "ds-test-server-dummy2",
    url      = surl,
    user     = username,
    password = password
  )
  connections <<- datashield.login(logins = builder$build(), assign = TRUE)

  datashield.assign(connections, "dat", quote(iris))
  vcall = paste0("quote(c(", paste(rep(c(1, 0), times = c(35, 115)), collapse = ", "), "))")
  datashield.assign(connections, "valid", eval(parse(text = vcall)))
  pushObject(connections, mod)
  predictModel(connections, mod, "pred", "dat", predict_fun = "predict(mod, newdata = D, type = 'response')")

  ds_summary = ds.summary("pred")
  nuisance = lapply(ds_summary, function(dss) {
    expect_equal(unname(dss$`quantiles & mean`["Mean"]), mean(p))
    expect_equal(dss$`quantiles & mean`["50%"], quantile(p, 0.5))
    expect_equal(dss$`quantiles & mean`["25%"], quantile(p, 0.25))
    expect_equal(dss$`quantiles & mean`["75%"], quantile(p, 0.75))
  })


  expect_equal(l2sens("iris", "p", nbreaks = 30L)$l2sens, dsL2Sens(connections, "dat", "pred", nbreaks = 30L))
  expect_message({
    roc_glm = dsROCGLM(connections, "valid", "pred", dat_name = "iris",
      seed_object = "pred")
  })
  expect_equal(class(roc_glm), "ROC.GLM")

  expect_message({
    roc_glm2 = dsROCGLM(connections, "valid", "pred", dat_name = "iris",
      seed_object = "pred")
  })
  expect_equal(roc_glm, roc_glm2)

  expect_silent({gg = plot(roc_glm)})
  expect_true(inherits(gg, "ggplot"))
  expect_output(print(roc_glm))


  datashield.assign(connections, "dat_no_na", quote(removeMissings("dat")))
  nuisance = lapply(DSI::datashield.symbols(connections), function(s) {
     expect_true("dat_no_na" %in% s)
  })

 ri = datashield.aggregate(connections, quote(getDataSHIELDInfo()))
  expect_equal(class(ri), "list")
  nuisance = lapply(ri, function(r) {
    expect_equal(names(r), c("session_info", "pcks"))
  })



  datashield.logout(connections)
})
