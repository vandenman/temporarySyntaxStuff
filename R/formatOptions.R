
.defaultFormatOptions <- list(
  short = FALSE,
  indent = 4,
  debug = FALSE
)

# TODO: does this need to be exported?
#' @rdname formatOptions
#' @export
.formatOptions <- list2env(.defaultFormatOptions)

#' @rdname formatOptions
#' @export
resetFormatOptions <- function() {
  formatOptions(.defaultFormatOptions)
}

#' @rdname formatOptions
#' @export
getFormatOption <- function(name) {
  return(get(name, envir = .formatOptions))
}

#' @rdname formatOptions
#' @export
setFormatOption <- function(name, value) {
  assign(name, value, envir = .formatOptions)
  return(invisible(value))
}

#' @rdname formatOptions
#' @title Global format options
#' @param ... modify options using name = value.
#' @param name character string of the value to get or set.
#' @param value the value to change x into.
#'
#' @export
formatOptions <- function(...) {

  args <- list(...)
  if (length(args) == 1L && is.list(args[[1L]]))
    args <- args[[1L]]

  if (!(length(args) && is.null(names(args)))) {
    if (length(args)) {
      for (i in seq_along(args)) {
        setFormatOption(names(args)[[i]], args[[i]])
      }

      return(invisible(args))

    } else {

      return(as.list(.formatOptions))

    }
  }

  args <- unlist(args)
  out <- as.list(.formatOptions)[args]

  if (length(out) == 1)
    out <- out[[1]]

  return(out)
}

.onAttach <- function(libname, pkgname) {
  assign(".formatOptions", .formatOptions, envir = as.environment("package:tempSyntaxPackage"))
}

