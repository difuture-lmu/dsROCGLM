context("Check if functionality works on the DataSHIELD test sever")

test_that("all methods can be used and produce reasonable output", {
  mod = lm(Sepal.Length ~ ., data = iris)
  p = predict(mod)

  surl     = "https://opal-demo.obiba.org/"
  username = "administrator"
  password = "password"

  opal = opalr::opal.login(username = username, password = password, url = surl)

  pkgs = c("dsROCGLM")
  for (pkg in pkgs) {
    check1 = opalr::dsadmin.install_github_package(opal = opal, pkg = pkg, username = "difuture-lmu", ref = "pkg-merge")
    if (! check1)
      stop("[", Sys.time(), "] Was not able to install ", pkg, "!")

    check2 = opalr::dsadmin.publish_package(opal = opal, pkg = pkg)
    if (! check2)
      stop("[", Sys.time(), "] Was not able to publish methods of ", pkg, "!")
  }
  library(DSI)
  library(DSOpal)
  library(dsBaseClient)

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

  pushObject(connections, mod)
  nuisance = lapply(DSI::datashield.symbols(connections), function(s) {
     expect_true("mod" %in% s)
  })

  datashield.assign(connections, "dat", quote(iris))
  predictModel(connections, mod, "pred", "dat")
  nuisance = lapply(DSI::datashield.symbols(connections), function(s) {
     expect_true("pred" %in% s)
  })
  ds_summary = ds.summary("pred")
  nuisance = lapply(ds_summary, function(dss) {
    expect_equal(unname(dss$`quantiles & mean`["Mean"]), mean(p))
    expect_equal(dss$`quantiles & mean`["50%"], quantile(p, 0.5))
    expect_equal(dss$`quantiles & mean`["25%"], quantile(p, 0.25))
    expect_equal(dss$`quantiles & mean`["75%"], quantile(p, 0.75))
  })

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
