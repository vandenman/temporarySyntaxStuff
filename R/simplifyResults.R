# should be done inside the package to ensure we dispatch to the right S3 methods
loadNamespace("pillar")

# simplify results structure ----
isCollection <- function(x) "collection" %in% names(x)
isTable      <- function(x) "schema"     %in% names(x)
isPlot       <- function(x) "editable"   %in% names(x)


getFieldsAsDf <- function(table) {
  fields <- table$schema$fields
  for (i in seq_along(fields)) {
    if (is.null(fields[[i]]$combine))
      fields[[i]]$combine <- FALSE
    if (is.null(fields[[i]]$format))
      fields[[i]]$format <- ""
    if (is.null(fields[[i]]$overTitle))
      fields[[i]]$overTitle <- ""
  }
  return(do.call(rbind.data.frame, fields))
}

simplifyTable <- function(table) {

  fields <- getFieldsAsDf(table)
  data <- table[["data"]]

  tableList <- lapply(seq_len(nrow(fields)), function(i) {

    name  <- fields[i, "name"]
    values <- if (fields[i, "type"] == "string") {
      v <- unlist(lapply(data, `[[`, name))
      # if we don't replace empty strings by NAs then pillar explicitly prints "" (as "\"\"") and also turns "0" into "\"0\""
      v[v == ""] <- NA_character_
      v
    } else {
      unlist(lapply(data, function(x, name) {
        y <- x[[name]]
        if (is.numeric(y)) y else if (is.integer(x)) NA_integer_ else NA_real_
      }, name = name))
    }
    if (isTRUE(fields[i, "combine"]))
      values[duplicated(values)] <- NA


    # if (fields[i, "type"] == "number")
    #   values <- applyPillarNum(values, fields[i, "format"])

    values
  })
  meta  <- list(
    fields                 = fields,
    footnotes              = table[["footnotes"]],
    title                  = table[["title"]],
    firstColumnIsOverTitle = table[["overTitle"]],
    transposed             = table[["casesAcrossColumns"]]
  )

  data <- as.data.frame(setNames(tableList, fields[["title"]]), fix.empty.names = FALSE, check.names = FALSE)

  return(jaspTableWrapper(data, meta))
  # return(setNames(list(jaspTableWrapper(data, meta)), table[["title"]]))

}

simplifyPlot <- function(plt, figures) {

  if (!(plt[["data"]] %in% names(figures)))
    stop("Could not find a plot with identifier ", plt[["data"]], " in the results!")

  return(jaspPlotWrapper(figures[[plt[["data"]]]][["obj"]], plt[["title"]]))
}

simplifyContainer <- function(collection, figures, meta) {
  # res <- list()
  # for (nm in names(meta[["meta"]])) {
  #   obj <- simplifyJaspObject(collection[["collection"]][[nm]], figures, meta[["meta"]][[nm]])
  #   res <- c(res, obj)
  #   class(res[length(res)]) <- class(obj)
  # }

  nms <- names(meta[["meta"]])
  res <- list()#setNames(vector("list", length(nms)), nms)
  for (nm in nms) {
    title <- meta[["meta"]][[nm]]$title
    if (is.null(title)) {
      title <- ""
    }

    obj <- simplifyJaspObject(collection[["collection"]][[nm]], figures, meta[["meta"]][[nm]])
    # browser()
    res[[title]] <- obj
    # if (is.jaspContainerWrapper(obj)) {
    #   # browser()
    #   res[[title]] <- obj[[1L]]
    # } else {
    #   res[[title]] <- obj
    # }
    # if (title == "contNormal")
    #   browser()

  }

  return(jaspContainerWrapper(res, collection[["title"]]))

}

simplifyJaspObject <- function(obj, figures, meta) {

  if (isCollection(obj)) return(simplifyContainer(obj, figures, meta))
  if (isTable(obj))      return(simplifyTable(obj))
  if (isPlot(obj))       return(simplifyPlot(obj, figures))

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

# wrapper classes ----

jaspPlotWrapper <- function(plt, title) {
  attr(plt, "title") <- title
  res <- setNames(list(plt), title)
  class(res) <- c("jaspPlotWrapper", "jaspWrapper")
  res
}

jaspContainerWrapper <- function(res, title) {

  # if (title != "") {

    # TODO: I don't get this myself but it works...
    class(res) <- c("jaspContainerWrapper", "jaspWrapper")
    attr(res, "title") <- title
    # res <- list(res)
    # names(res) <- title
    # class(res) <- c("jaspContainerWrapper", "jaspWrapper")
    # attr(res, "title") <- title

  # } else {
  #   # drop the outer container
  #   browser()
  #   res <- lapply(res, function(x) if (is.jaspContainerWrapper(x)) x else x[[1L]])
  #   class(res) <- c("jaspContainerWrapper", "jaspWrapper")
  # }
  return(res)
}

jaspResultsWrapper <- function(res, title = "Analysis title") {
  class(res) <- c("jaspResultsWrapper", "jaspWrapper")
  attr(res, "title") <- title
  res
}

jaspTableWrapper <- function(data, meta) {
  x <- vctrs::new_data_frame(data, class = c("jaspTableWrapper", "jaspWrapper", "tbl"))
  attr(x, "meta") <- meta
  # for consistency with the other wrappers
  attr(x, "title") <- meta[["title"]]
  return(x)
}

is.jaspContainerWrapper <- function(x) inherits(x, "jaspContainerWrapper")
is.jaspPlotWrapper      <- function(x) inherits(x, "jaspPlotWrapper")
is.jaspTableWrapper     <- function(x) inherits(x, "jaspTableWrapper")
is.jaspResultsWrapper   <- function(x) inherits(x, "jaspResultsWrapper")
is.jaspWrapper          <- function(x) inherits(x, "jaspWrapper")

# functions on jaspWrapper ----
getTitle <- function(x) attr(x, "title")

# table functions ----

getTableMeta <- function(x) {
  attr(x, "meta")
}

applyPillarNum <- function(values, format) {

  matches <- as.numeric(regmatches(format, gregexpr("[[:digit:]]+", format))[[1L]])

  notation <- "fit"
  if (length(matches) == 2L) {
    sigfig   <- matches[1L]
    digits   <- NULL#matches[2L]
  } else if (startsWith(format, "sf")) {
    sigfig <- matches[1L]
    digits     <- NULL#3L # TODO: what to do here?
  } else {
    notation <- "dec"
    digits   <- matches[1L]
    sigfig   <- NULL#matches[1L]
  }
  pillar::num(values, sigfig = sigfig, digits = digits, notation = notation)

}

dropBackTicks <- function(x) {
  if (startsWith(x, "`") && endsWith(x, "`"))
    return(substr(x, 2L, nchar(x) - 1L))
  x
}

tbl_format_setup.jaspTableWrapper <- function(x, width = NULL, ..., n = NULL, max_extra_cols = NULL, max_footer_lines = NULL) {

  # cat("tbl_format_setup.jaspTableWrapper")
  fields <- attr(x, "meta")$fields
  for (i in seq_along(x))  {
    if (fields[i, "type"] == "number")
      x[[i]] <- applyPillarNum(x[[i]], fields[i, "format"])
  }

  # TODO: clean this up, we don't need the chunks anymore!
  if (any(fields[["overTitle"]] != "")) {
    attr(x, "hasOverTitles") <- TRUE
    allOverTitles <- unique(setdiff(fields[["overTitle"]], ""))
    for (i in seq_along(allOverTitles)) {
      idx <- which(fields[["overTitle"]] == allOverTitles[i])

      if (!identical(idx, idx[1L]:(idx[1L] + length(idx[1L]))))
        warning("indices for overtitles were not consecutive!")

      if (length(idx) > 1L) {
        # TODO: benchmark this vs substring?
        titleSplit <- strsplit(fields[[idx[1L], "overTitle"]], "")[[1]]
        # splits an overtitle into equally sized chunks.
        overTitleChunks <- unlist(
          tapply(titleSplit, cut(seq_along(titleSplit), length(idx), labels = FALSE), paste, collapse = "", simplify = FALSE),
        use.names = FALSE)

        if (length(overTitleChunks) %% 2L == 0L)
          overTitleChunks[length(overTitleChunks)] <- paste0(overTitleChunks[length(overTitleChunks)], " ")

      } else { # only one chunk
        overTitleChunks <- overTitleChunks
      }

      for (j in seq_along(idx)) {
        k <- idx[j]
        attr(x[[k]], "overTitleInfo") <- list(
          title     = overTitleChunks[j],
          noColumns = length(idx),
          index     = j,
          index2    = i
        )
      }
    }
  } else {
    attr(x, "hasOverTitles") <- FALSE
  }

  "!!!!DEBUG tbl_format_setup.jaspTableWrapper()"

  # <=> start of copied code
  # Number of rows
  rows <- nrow(x)

  if (is.na(rows)) {
    df <- pillar:::df_head(x, n + 1)
    if (nrow(df) <= n) {
      rows <- nrow(df)
    } else {
      df <- pillar:::vec_head(df, n)
    }
  } else {
    df <- pillar:::df_head(x, n)
  }

  if (is.na(rows)) {
    # Lazy table with too many rows
    needs_dots <- (nrow(df) >= n)
  } else {
    # Lazy table with few rows, or regular data frame
    needs_dots <- (rows > n)
  }

  if (needs_dots) {
    rows_missing <- rows - n
  } else {
    rows_missing <- 0L
  }

  # Header
  tbl_sum <- pillar::tbl_sum(x)

  # Body
  rownames(df) <- NULL
# browser()
  colonnade <- pillar:::ctl_colonnade(
    df,
    # <=> end of copied code
    has_row_id = FALSE,
    # <=> start of copied code
    width = width,
    controller = x
  )

  body <- colonnade$body

  # Extra columns
  extra_cols <- colonnade$extra_cols
  extra_cols_total <- length(extra_cols)

  if (extra_cols_total > max_extra_cols) {
    length(extra_cols) <- max_extra_cols
  }

  # Result
  pillar:::new_tbl_format_setup(
    x = x,
    df = df,
    width = width,
    tbl_sum = tbl_sum,
    body = body,
    rows_missing = rows_missing,
    rows_total = rows,
    extra_cols = extra_cols,
    extra_cols_total = extra_cols_total,
    max_footer_lines = max_footer_lines
  )
  # <=> end of copied code
  # this would also work if we could set has_row_id somehow...
  # NextMethod("tbl_format_setup", x)

}

tbl_sum.jaspTableWrapper <- function(x, ...) {
  if (debug())
    paste("jaspTableWrapper:", getTableMeta(x)$title)
  else getTableMeta(x)$title
}

tbl_format_header.jaspTableWrapper <- function(x, setup, ...) {
  setup$tbl_sum
}


tbl_format_footer.jaspTableWrapper <- function(x, setup, ...) {
  default_footer <- NextMethod()
  footnotes <- character()
  if (length(getTableMeta(x)$footnotes) > 0L) {
    footnotes <- strwrap(
      paste(vapply(getTableMeta(x)$footnotes, `[[`, character(1L), "text"), collapse = "\n"),
      width = nchar(setup$body[1], type = "width")
    )
  }
  c(default_footer, footnotes)
}

getTableLineStyles <- function() {

  noUnicode <- FALSE
  if (noUnicode) {
    topLineChar <- "="#"\uFFE3"
    midLineChar <- "-"#"\uFFE3"
    botLineChar <- "="#"\uFF3F"
  } else {
    # topLineChar <- "\u2501"
    # midLineChar <- "\u2500"
    # botLineChar <- "\u2501"
    topLineChar <- "\u2500"
    midLineChar <- "\u2500"
    botLineChar <- "\u2500"
  }
  return(list(topLineChar = topLineChar, midLineChar = midLineChar, botLineChar = botLineChar))
}

ctl_new_pillar.jaspTableWrapper <- function(controller, x, width, ..., title = NULL) {
  out <- NextMethod()
  out$title[[1]][1][[1]] <- dropBackTicks(out$title[[1]][1][[1]])
  noEquals <- max(attr(out$title, "width"), attr(out$data, "width")) + 1

  # why did I do this again?
  if ("lhs" %in% names(out$data[[1]][[1]])) {
    idxNA <- out$data[[1]][[1]]$lhs == "NA"
    out$data[[1]][[1]]$lhs[idxNA] <- ""
  } else {
    idxNA <- is.na(out$data[[1]][[1]])
    out$data[[1]][[1]][idxNA] <- ""
  }

  tmp <- getTableLineStyles()
  topLineChar <- tmp$topLineChar
  midLineChar <- tmp$midLineChar
  botLineChar <- tmp$botLineChar

  if (attr(controller, "hasOverTitles")) {

    overTitleInfo <- attr(x, "overTitleInfo")
    if (!is.null(overTitleInfo)) {
      # browser()
      overTitle <- overTitleInfo$title
      ncols     <- overTitleInfo$noColumns
      index     <- overTitleInfo$index2

      noEquals  <- max(noEquals, nchar(overTitle))
      overTitleWidth <- noEquals#nchar(overTitle)
      overTitle <- strrep(letters[index], noEquals)
      # if (index == 1) {
      # } else {
      #   overTitle <- ""
      #   overTitleWidth <- 0L
      # }
      overTitleLine <- midLineChar

    } else {
      overTitle      <- ""
      overTitleLine  <- ""
      overTitleWidth <- noEquals

    }

    lst <- list(
      top_top_rule = pillar::new_pillar_component(list(strrep(topLineChar, noEquals)), width = noEquals),
      overTitle    = pillar::new_pillar_component(list(overTitle),                     width = overTitleWidth),
      top_mid_rule = pillar::new_pillar_component(list(strrep(overTitleLine, noEquals)), width = noEquals),
      title        = out$title,
      # type        = out$type, # useful for debugging
      bot_mid_rule = pillar::new_pillar_component(list(strrep(midLineChar, noEquals)), width = noEquals),
      data         = out$data,
      bot_bot_rule = pillar::new_pillar_component(list(strrep(botLineChar, noEquals)), width = noEquals)
    )

    if (debug())
      lst <- append(lst, list(type = out$type), after = 4)

  } else {
    lst <- list(
      top_rule    = pillar::new_pillar_component(list(strrep(topLineChar, noEquals)), width = noEquals),
      title       = out$title,
      mid_rule    = pillar::new_pillar_component(list(strrep(midLineChar, noEquals)), width = noEquals),
      data        = out$data,
      bottom_rule = pillar::new_pillar_component(list(strrep(botLineChar, noEquals)), width = noEquals)
    )

    if (debug())
      lst <- append(lst, list(type = out$type), after = 3)
  }

  return(pillar::new_pillar(lst))
}

tbl_format_body.jaspTableWrapper <- function(x, setup, ...) {
  # pillar leaves an empty space between adjacent columns
  # in addition, overtitles may be separated
  # here we fix these issues
  body <- setup$body
  n <- length(setup$body)
  tmp <- getTableLineStyles()
  topLineChar <- tmp$topLineChar
  midLineChar <- tmp$midLineChar
  botLineChar <- tmp$botLineChar

  fields <- getFields(x)

  # TODO: decide if we want to do things like this to shade every 2nd row or so
  # could make complete themes like that
  # bgGrey <- crayon::make_style("grey40", bg = TRUE)
  # cat(bgGrey("This will have some fancy colors"), "\n")
  # use rstudioapi::getThemeInfo() to get the background color


  if (any(fields[, "overTitle"] != "")) {
    body[1L] <- gsub("\\s", topLineChar, body[1L])

    allOverTitles <- unique(setdiff(fields[["overTitle"]], ""))
    splitBody <- strsplit(body[2L], "")[[1L]]
    idxSeen <- integer()
    for (i in seq_along(allOverTitles)) {

      target <- letters[i]
      indices <- setdiff(which(splitBody == target), idxSeen)
      idx <- indices[1L]:indices[length(indices)]
      splitBody[idx] <- strsplit(format(allOverTitles[i], width = length(idx), justify = "centre"), "")[[1L]]
      idxSeen <- union(idxSeen, idx)

    }
    body[2L] <- paste(splitBody, collapse = "")

    # matches <- regmatches(body[2L], regexpr("(\\S+ )(?=\\S+)", body[2L], perl = TRUE))
    # matches <- paste0(" ", substr(matches, 1L, nchar(matches) -1L))
    # body[2L] <- gsub("(\\S+ )(?=\\S+)", matches, body[2L], perl = TRUE)

    if (debug()) {
      body[4L] <- gsub(paste0(midLineChar, " ", midLineChar), strrep(midLineChar, 3L), body[4L], fixed = TRUE)
      body[6L] <- gsub("\\s", midLineChar, body[6L])
    } else {
      body[3L] <- gsub(paste0(midLineChar, " ", midLineChar), strrep(midLineChar, 3L), body[3L], fixed = TRUE)
      body[5L] <- gsub("\\s", midLineChar, body[5L])
    }
    body[n]  <- gsub("\\s", botLineChar, body[n])
  } else {

    body[1L] <- gsub("\\s", topLineChar, body[1L])
    body[3L] <- gsub("\\s", midLineChar, body[3L])
    body[n]  <- gsub("\\s", botLineChar, body[n])
  }
  body
}

getFields <- function(x) {
  attr(x, "meta")$fields
}

# print functions ----
debug <- function() {
  getOption("jaspDebug", default = FALSE)
}
debugOn  <- function() { options("jaspDebug" = TRUE) }
debugOff <- function() { options("jaspDebug" = FALSE) }

getIndent <- function(indent) {
  strrep(" ", indent)
}

format.jaspResultsWrapper <- function(x, indent = 4, short = FALSE, ...) {

  individualFormats <- lapply(x, getFormat, indent = indent, short = short)
  if (length(individualFormats) > 1L && !short) {
    tmp <- vector("list", 2L * length(individualFormats) - 1L)
    tmp[seq(1, length(tmp), 2)] <- individualFormats
    tmp[seq(2, length(tmp), 2)] <- list("")
    individualFormats <- tmp
  }

  res <- unlist(individualFormats, use.names = FALSE)
  if (debug())
    res <- c("jaspResultsWrapper", paste0(getIndent(indent), res))

  return(res)
}

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

  if (debug())
    title <- paste("jaspContainerWrapper:", title)

  if (!short)
    c(title, "", paste0(getIndent(indent), unlist(individualFormats, use.names = FALSE)))
  else c(title, paste0(getIndent(indent), unlist(individualFormats, use.names = FALSE)))
}

format.jaspPlotWrapper <- function(x, indent = 4, short = FALSE, ...) {
  title <- if (debug()) paste("jaspPlotWrapper:", names(x)[1L]) else names(x)[1L]
  if (!short)
    title <- c(title, paste0(getIndent(indent), terminalPlot(x)))
  return(title)
}

# format is extensively used in
getFormat <- function(x, ...) {
  UseMethod("getFormat", x)
}

getFormat.default <- function(x, indent = 4, short = FALSE, ...) {
  return(format(x, indent, short, ...))
}

getFormat.jaspTableWrapper <- function(x, indent = 4, short = FALSE) {
  if (short) {
    return(tbl_sum.jaspTableWrapper(x))
  } else {
    return(format(x))
  }
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

print.jaspWrapper <- function(x, indent = 4, short = FALSE,...) {
  writeLines(format(x, indent = indent, short = short, ...))
  invisible(x)
}

# other stuff ----
# simplifyCollection2 <- function(collection, figures) {
#   res <- vector("list", length(collection$collection))
#   for (i in seq_along(collection$collection)) {
#     v <- simplifyJaspObject(collection[["collection"]][[i]], figures)
#     res[i] <- v
#     names(res)[i] <- names(v)
#   }
#   return(setNames(list(res), collection[["title"]]))
#   # this function is actually slower...
#   # bch <- bench::mark(
#   #   expr1 = simplifyCollection(collection, figures),
#   #   expr2 = simplifyCollection2(collection, figures),
#   #   check = FALSE
#   # )
#   # summary(bch, relative = TRUE)
# }
