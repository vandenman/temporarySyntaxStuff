isCollection <- function(x) "collection" %in% names(x)
isTable      <- function(x) "schema"     %in% names(x)
isPlot       <- function(x) "editable"   %in% names(x)
isHtml       <- function(x) "rawtext"    %in% names(x)


getFieldsAsDf <- function(table) {
  fields <- table$schema$fields
  for (i in seq_along(fields)) {
    if (is.null(fields[[i]]$combine))
      fields[[i]]$combine <- FALSE
    if (is.null(fields[[i]]$format))
      fields[[i]]$format <- ""
    if (is.null(fields[[i]]$overTitle))
      fields[[i]]$overTitle <- ""

    fields[[i]]$.footnotes <- if (is.null(fields[[i]]$.footnotes)) {
      NA_integer_
    } else {
      unlist(fields[[i]]$.footnotes)
    }

    # if it turns out that footnotes must be represented as lists then this may be useful
    # if (is.null(fields[[i]]$.footnotes)) {
    #   fields[[i]]$.footnotes <- list()
    # }
  }

  return(do.call(rbind.data.frame, fields))
  # use this is footnotes must be lists
  # return(listOfListsToDataFrame(fields))
}

listOfListsToDataFrame <- function(lst) {

  # this function only exists because do.call(rbind.data.frame, list(lst, lst))
  # does not work properly, when you want an entry to be a list which is one way
  # to represent footnotes.
  # rbind tries to unpack lists, whereas this function keeps them as is.

  lstToColumn <- function(vals) {
    if (is.list(vals[[1L]]) || length(vals[[1L]]) != 1L) { # keep lists AsIs
      I(vals)
    } else { # convert everything else to vector
      unlist(vals)
    }
  }

  nms <- Reduce(unique, lapply(lst, names))
  res <- NULL#data.frame()
  for (nm in nms) {
    vals <- lapply(lst, `[[`, nm)
    if (is.null(res)) {
      res <- data.frame(lstToColumn(vals))
      names(res) <- nm
    } else {
      res[[nm]] <- lstToColumn(vals)
    }
  }
  return(res)
}


simplifyTable <- function(table) {

  fields <- getFieldsAsDf(table)
  data <- table[["data"]]

  tableList <- lapply(seq_len(nrow(fields)), function(i) {

    name <- fields[i, "name"]
    values <- if (fields[i, "type"] == "string") {
      v <- unlist(lapply(data, `[[`, name))
      # if we don't replace empty strings by NAs then pillar explicitly prints "" (as "\"\"") and also turns "0" into "\"0\""
      v[v == ""] <- NA_character_
      v
    } else {
      unlist(lapply(data, function(x, name) {
        y <- x[[name]]
        if (is.numeric(y)) y else if (is.integer(x)) NA_integer_ else NA_real_
        # if (is.numeric(y)) y else if (is.integer(x)) NA_integer_ else y
      }, name = name))
    }

    if (isTRUE(fields[i, "combine"]))
      values[duplicated(values)] <- NA

    values

  })

  meta <- jaspTableMeta(
    fields                 = fields,
    footnotes              = table[["footnotes"]],
    title                  = table[["title"]],
    firstColumnIsOverTitle = table[["overTitle"]],
    transposed             = table[["casesAcrossColumns"]]
  )

  data <- as.data.frame(setNames(tableList, fields[["name"]]), fix.empty.names = FALSE, check.names = FALSE)

  return(jaspTableWrapper(data, meta))
  # return(setNames(list(jaspTableWrapper(data, meta)), table[["title"]]))

}

simplifyPlot <- function(plt, figures) {

  if (!(plt[["data"]] %in% names(figures)))
    stop("Could not find a plot with identifier ", plt[["data"]], " in the results!")

  return(jaspPlotWrapper(figures[[plt[["data"]]]][["obj"]], plt[["title"]]))
}

simplifyContainer <- function(collection, figures, meta) {

  nms <- names(meta[["meta"]])
  res <- list()

  for (nm in nms) {

    title <- meta[["meta"]][[nm]]$title

    if (is.null(title))
      title <- ""

    obj <- simplifyJaspObject(collection[["collection"]][[nm]], figures, meta[["meta"]][[nm]])
    res[[title]] <- obj

  }

  return(jaspContainerWrapper(res, collection[["title"]]))

}

simplifyHtml <- function(obj) {
  jaspHtmlWrapper(obj, obj[["title"]])
}

simplifyJaspObject <- function(obj, figures, meta) {

  if (isCollection(obj)) return(simplifyContainer(obj, figures, meta))
  if (isTable(obj))      return(simplifyTable(obj))
  if (isPlot(obj))       return(simplifyPlot(obj, figures))
  if (isHtml(obj))       return(simplifyHtml(obj))

  stop("Unknown jasp object!")
}

simplifyMeta <- function(meta) {
  # ensures that we go from unnamed lists, where each element is a list entry with a name and possibly another meta
  names(meta) <- vapply(meta, `[[`, character(1L), "name")
  lapply(meta, function(x) {
    if (!is.null(x[["meta"]])) {
      x[["meta"]] <- simplifyMeta(x[["meta"]])
      return(x)
    } else {
      return(x)
    }
  })
}

#' @export
simplifyResults <- function(results) {

  # needed to order things
  meta <- simplifyMeta(results[["results"]][[".meta"]])

  rawResults <- results[["results"]]
  figures    <- results[["state"]][["figures"]]

  # intersect(names(rawResults), meta$
  topLevelNames <- names(meta)

  # res <- list()
  # for (name in topLevelNames) {
  #   obj <- simplifyJaspObject(rawResults[[name]], figures, meta[[name]])
  #   res <- c(res, obj)
  #   class(res[[length(res)]]) <- class(obj)
  # }
  res <- list() # setNames(vector("list", length(topLevelNames)), topLevelNames)
  for (name in topLevelNames) {
    obj <- simplifyJaspObject(rawResults[[name]], figures, meta[[name]])
    title <- meta[[name]][["title"]]
    if (title == "")
      res <- c(res, obj)
    else
      res[[title]] <- obj
  }

  return(jaspResultsWrapper(res))
}

