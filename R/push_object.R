#'
#' @title Push object to DataSHIELD server
#' @description This function pushes a serialized object and assigns a server
#'   variable to it.
#' @param connections (`DSI::connection`) Connection to an OPAL server.
#' @param obj (arbitrary R object) Object containing which should be pushed to the server.
#' @param check_serialization (`logical(1L)`) Check if the serialized model can be deserialized
#'   locally (default is `TRUE`).
#' @param package (`character(1L)`) Package required for model predictions (default is `NULL`).
#' @param just_return_call (`logical(1L)`) Just return the call and not execute on server (mainly for testing purposes,
#'   default is `FALSE`).
#' @author Daniel S.
#' @examples
#' # Just for local testing:
#' cl = pushObject("Dummy", iris, just_return_call = TRUE)
#' all.equal(iris, eval(parse(text = cl)))
#' @export
pushObject = function(connections, obj, check_serialization = TRUE, package = NULL,
  just_return_call = FALSE) {

  obj_name = deparse(substitute(obj))

  # Checks are done in encodeObject:
  bin = encodeObject(obj, obj_name, check_serialization)
  if (is.null(package)) package = "NULL" else packge = paste0("\"", package, "\"")

  call = paste0("decodeBinary(\"", bin, "\", ", package, ")")
  cq = NULL # Dummy for checks
  eval(parse(text = paste0("cq = quote(", call, ")")))

  if (just_return_call) {
    return(call)
  } else {
    DSI::datashield.assign(connections, names(bin), cq)
    return(invisible(NULL))
  }
}
