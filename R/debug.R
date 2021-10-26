debugIsOn <- function() {
  getOption("jaspDebug", default = FALSE)
}

#' @title Turn on/ off debug information
#' @rdname debug
#' @export
debugOn  <- function() { options("jaspDebug" = TRUE) }

#' @rdname debug
#' @export
debugOff <- function() { options("jaspDebug" = FALSE) }
