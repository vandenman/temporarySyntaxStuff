
jaspPlotWrapper <- function(plt, title) {
  attr(plt, "title") <- title
  res <- setNames(list(plt), title)
  class(res) <- c("jaspPlotWrapper", "jaspWrapper")
  res
}

jaspContainerWrapper <- function(res, title) {

  class(res) <- c("jaspContainerWrapper", "jaspWrapper")
  attr(res, "title") <- title

  return(res)
}

jaspResultsWrapper <- function(res, title = "Analysis title") {
  class(res) <- c("jaspResultsWrapper", "jaspWrapper")
  attr(res, "title") <- title
  res
}

jaspTableWrapper <- function(data, meta) {

  stopifnot(all(colnames(data) == meta$fields$name))

  x <- vctrs::new_data_frame(data, class = c("jaspTableWrapper", "jaspWrapper", "tbl"))
  attr(x, "meta") <- meta
  # for consistency with the other wrappers
  attr(x, "title") <- meta[["title"]]
  return(x)
}

jaspHtmlWrapper <- function(res, title) {

  class(res) <- c("jaspHtmlWrapper", "jaspWrapper")
  attr(res, "title") <- title

  return(res)

}

is.jaspContainerWrapper <- function(x) inherits(x, "jaspContainerWrapper")
is.jaspPlotWrapper      <- function(x) inherits(x, "jaspPlotWrapper")
is.jaspTableWrapper     <- function(x) inherits(x, "jaspTableWrapper")
is.jaspHtmlWrapper      <- function(x) inherits(x, "jaspHtmlWrapper")
is.jaspResultsWrapper   <- function(x) inherits(x, "jaspResultsWrapper")
is.jaspWrapper          <- function(x) inherits(x, "jaspWrapper")

# functions on jaspWrapper ----
getTitle <- function(x) attr(x, "title")

# table functions ----

getTableMeta <- function(x) {

  if (!is.jaspTableWrapper(x))
    stop("x should be of class 'jaspTableWrapper'!")

  attr(x, "meta")
}

getFields <- function(x) {
  attr(x, "meta")$fields
}

jaspTableMeta <- function(fields, footnotes = list(), title = "", firstColumnIsOverTitle = FALSE, transposed = FALSE) {

  # TODO: assert/ validate the input here!

  lst <- list(
    fields                 = fields,
    footnotes              = footnotes,
    title                  = title,
    # these can only both be true
    firstColumnIsOverTitle = firstColumnIsOverTitle && transposed,
    transposed             = transposed
  )
  class(lst) <- c("tableMeta", "list")
  return(lst)

}

`[.jaspTableWrapper` <- function(x, i, j, drop = FALSE) {

  meta <- getTableMeta(x)
  class(x) <- class(x)[-1L]
  y <- x[i, j, drop = FALSE]

  # if there are multiple empty column names then they are replaced by ".1", ".2"
  if (!missing(j)) {
    colnames(y) <- colnames(x)[j]
    meta$fields <- meta$fields[j, , drop = FALSE]
  }

  return(jaspTableWrapper(y, meta))
}

t.jaspTableWrapper <- function(x) {
  attr(x, "meta")$transposed <- !attr(x, "meta")$transposed
  x
}

guessFields <- function(x) {
  type <- vapply(x, function(x) {
    switch (typeof(x),
            "double"    = "number",
            "integer"   = "integer",
            "character" = "string"
    )
  }, FUN.VALUE = character(1L))
  return(data.frame(
    name      = colnames(x),
    title     = colnames(x),
    type      = type,
    format    = ifelse(type == "number", "sf:4;dp:3", ""),
    overTitle = rep("", length(x))
  ))
}
