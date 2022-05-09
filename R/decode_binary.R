#'
#' @title Deserialize object
#' @description Decode a given string of a serialized object.
#' @param bin (`character(1L)`) Binary string value containing the serialized model.
#' @param package (`character(1L)`) Package required for object deserialization (default is `NULL`).
#' @return Deserialized object from `bin`
#' @author Daniel S.
#' @examples
#' mod = lm(Sepal.Width ~ ., data = iris)
#' bin = encodeObject(mod)
#' mod_b = decodeBinary(bin)
#' all.equal(mod, mod_b)
#' @export
decodeBinary = function(bin, package = NULL) {
  checkmate::assertCharacter(bin, len = 1L, null.ok = FALSE, any.missing = FALSE)
  checkmate::assertCharacter(package, len = 1L, null.ok = TRUE, any.missing = FALSE)

  # Check if model is installed and install if not:
  if (! is.null(package) && require(package, quietly = TRUE, character.only = TRUE)) {
    stop("Package '", package, "' is not installed. Please install",
      " it or contact the administrator to do this for you.")
  }

  binary_str_deparse = substring(bin, seq(1, nchar(bin), 2), seq(2, nchar(bin), 2))

  raw = as.raw(as.hexmode(binary_str_deparse))
  obj = unserialize(raw)

  return(obj)
}
