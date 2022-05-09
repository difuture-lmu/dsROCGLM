#'
#' @title Predict model on server site
#' @description This function enables to make predictions on the server for any
#'   model. Requirement is that it can be serialized and deserialized without loosing
#'   model components. Therefore, use `check_serialization = TRUE`.
#' @param connections (`DSI::connection`) Connection to an OPAL server.
#' @param mod (`arbitrary`) R object containing a model which is used for predictions on the server.
#' @param pred_name (`character(1L)`) Name of the object predictions should be assigned to.
#' @param dat_name (`character(1L)`) Name of the data object on the server (default is `"D"`.
#' @param predict_fun (`character(1L)`) The prediction function call as string. As placeholder
#'   use 'mod' for the model and 'D' for the data, e.g. `predict(mod, newdata = D)`. This
#'   gives the possibility to call arbitrary predict function like
#'   `predict(mod, newdata = D, type = \"response\", family = binomial())' for a GLM. The
#'   default is `predict(mod, newdata = D)`.
#' @param repush (`logical(1L)`) Logical value indicating if the model should again pushed to the server
#'   (default is `TRUE`).
#' @param check_serialization (`logical(1L)`) Check if the serialized model can be deserialized locally
#'   (default is `TRUE`).
#' @param package (`character(1L)`) Package required for model predictions (default is `NULL`).
#' @param just_return_call (`logical(1L)`) Just return the call and not execute on server (mainly for testing purposes,
#'   default is `FALSE`).
#' @return The prediction object returned from `DSI::datashield.assign`
#' @author Daniel S.
#' @export
predictModel = function(connections, mod, pred_name, dat_name = "D", predict_fun = "predict(mod, newdata = D)",
  repush = FALSE, check_serialization = TRUE, package = NULL, just_return_call = FALSE) {

  checkmate::assertLogical(repush, len = 1L, null.ok = FALSE, any.missing = FALSE)
  checkmate::assertCharacter(pred_name, len = 1L, null.ok = FALSE, any.missing = FALSE)
  checkmate::assertCharacter(dat_name, len = 1L, null.ok = FALSE, any.missing = FALSE)
  checkmate::assertCharacter(predict_fun, len = 1L, null.ok = FALSE, any.missing = FALSE)
  checkmate::assertCharacter(package, len = 1L, null.ok = TRUE, any.missing = FALSE)

  mod_name = deparse(substitute(mod))
  checkmate::assertChoice(mod_name, ls(.GlobalEnv))
  if (just_return_call) {
    checkmate::assertChoice(dat_name, ls(.GlobalEnv))
  } else {
    sym = DSI::datashield.symbols(connections)

    snames = names(sym)
    for (s in snames) {
      if (! dat_name %in% sym[[s]]) stop("There is no data object '", dat_name, "' on server '", s, "'.")
    }
  }

  if (repush) pushObject(connections, mod, mod_name, check_serialization, package)

  call = stringr::str_replace(predict_fun, "mod", mod_name)
  call = stringr::str_replace(call, "D", dat_name)

  # Hack to send string to server. The servers do not allow to send e.g. braces
  # in a string value ... therefore decode locally and encode at server ...
  bin_pred_call = encodeObject(call)

  call = paste0("assignPredictModel(\"", bin_pred_call, "\")")
  cq = NULL
  eval(parse(text = paste0("cq = quote(", call, ")")))
  if (just_return_call) {
    return(cq)
  } else {
    DSI::datashield.assign(connections, pred_name, cq)
  }
}

#'
#' @title Wrapper to predict model on server site
#' @description This function enables to make predictions on the server for any
#'   model.
#' @param bin_call (`character(1L)`) Binary predict call (encodeObject applied on call).
#' @return Vector of predictions
#' @author Daniel S.
#' @export
assignPredictModel = function(bin_call) eval(parse(text = decodeBinary(bin_call)))
