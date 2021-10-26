
getIndent <- function(indent) {
  strrep(" ", indent)
}

# TODO: are the format.xxx function deprecated in favor of getFormat?
#' @export
format.jaspResultsWrapper <- function(x, indent = 4, short = FALSE, ...) {

  individualFormats <- lapply(x, getFormat, indent = indent, short = short)
  if (length(individualFormats) > 1L && !short) {
    tmp <- vector("list", 2L * length(individualFormats) - 1L)
    tmp[seq(1, length(tmp), 2)] <- individualFormats
    tmp[seq(2, length(tmp), 2)] <- list("")
    individualFormats <- tmp
  }

  res <- unlist(individualFormats, use.names = FALSE)
  if (debugIsOn())
    res <- c("jaspResultsWrapper", paste0(getIndent(indent), res))

  return(res)
}

#' @export
format.jaspContainerWrapper <- function(x, indent = 4, short = FALSE, ...) {
  individualFormats <- lapply(x, getFormat, indent = indent, short = short)
  if (length(individualFormats) > 1L && !short) {
    tmp <- vector("list", 2L * length(individualFormats) - 1L)
    tmp[seq(1, length(tmp), 2)] <- individualFormats
    tmp[seq(2, length(tmp), 2)] <- list("")
    individualFormats <- tmp
  }

  title <- attr(x, "title")
  if (is.null(title)) {
    warning("container had a title of NULL!")
    title <- ""
  }

  if (debugIsOn())
    title <- paste("jaspContainerWrapper:", title)

  if (!short)
    c(title, "", paste0(getIndent(indent), unlist(individualFormats, use.names = FALSE)))
  else c(title, paste0(getIndent(indent), unlist(individualFormats, use.names = FALSE)))
}

#' @export
format.jaspPlotWrapper <- function(x, indent = 4, short = FALSE, ...) {
  title <- if (debugIsOn()) paste("jaspPlotWrapper:", names(x)[1L]) else names(x)[1L]
  if (!short)
    title <- c(title, paste0(getIndent(indent), terminalPlot(x)))
  return(title)
}

# format is extensively used in pillar, so let's not overload the S3 method
#' @export
getFormat <- function(x, ...) {
  UseMethod("getFormat", x)
}

#' @export
getFormat.default <- function(x, ...) {
  stop("getFormat is not implemented for objects of class ", paste(class(x), collapse = ", "))
}

#' @export
getFormat.jaspWrapper <- function(x, indent = 4, short = FALSE, ...) {
  return(format(x, indent, short, ...))
}

#' @export
getFormat.jaspTableWrapper <- function(x, indent = 4, short = FALSE) {
  if (short) {
    return(tbl_sum.jaspTableWrapper(x))
  } else {
    # TODO: this is not a proper way to set the width, but it needs to follow
    # some global setting. See the TODO in tbl_format_body.jaspTableWrapper
    return(format(x, width = 3000))
  }
}

#' @export
getFormat.jaspHtmlWrapper <- function(x, indent = 4, short = FALSE) {

  title <- attr(x, "title")
  if (title == "hide me")
    title <- ""

  if (debugIsOn())
    title <- paste("jaspHtmlWrapper:", title)

  if (!short)
    title <- c(title, paste0(getIndent(indent), format(x[["rawtext"]])))

  return(title)

}

terminalPlot <- function(plt) {

  # TODO: actually do something based on the plot?

  # xx <- seq(-5, 3, length.out = 100)
  # plot(xx, f(xx))
  # capture.output(txtplot::txtplot(xx, f(xx), width = 20, height = 10))

  c(
    "   +--+--+---+--+-*+",
    " 5 +              *+",
    "   |             **|",
    "   |  *****      * |",
    " 0 + **   ***   ** +",
    "   | *      *****  |",
    "   |**             |",
    "-5 +*              +",
    "   +*-+--+---+--+--+",
    "     -4 -2   0  2   "
  )
}

#' @export
print.jaspWrapper <- function(x, indent = 4, short = FALSE,...) {
  writeLines(getFormat(x, indent = indent, short = short, ...))
  invisible(x)
}
