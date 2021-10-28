# these are required to make the S3 dispatch work correctly
#' @importFrom pillar tbl_format_header tbl_format_setup tbl_format_body tbl_sum tbl_format_footer ctl_new_pillar

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

#' @export
tbl_format_setup.jaspTableWrapper <- function(x, width = NULL, ..., n = NULL, max_extra_cols = NULL, max_footer_lines = NULL) {

  # cat("tbl_format_setup.jaspTableWrapper\n")
  meta   <- attr(x, "meta")
  fields <- meta$fields

  if (meta$firstColumnIsOverTitle && meta$transposed) { # transposeWith

    # mat is a data.frame where each column is a list.
    # those lists have named elements with values, to support mixed types
    mat <- do.call(data.frame, lapply(split(x, seq_len(nrow(x))), function(x) I(as.list(x))))

    matNames <- paste0("V", seq_len(nrow(x)))
    colnames(mat) <- matNames

    transposedFields <- data.frame(
      name      = matNames,
      title     = x[[2]],
      type      = "mixed",
      combine   = FALSE,
      format    = "mixed",
      overTitle = x[[1]]
    )

    # add a column since the original column names now become the first row
    transposedFields <- rbind.data.frame(
      # TODO: this should be a different name
      data.frame(name = "V0", title = "", type = "string", combine = FALSE, format = "string", overTitle = ""),
      transposedFields
    )

    meta$transposedFields <- transposedFields

    # use the internal names
    for (i in seq_along(mat))
      names(mat[[i]]) <- fields[["name"]]
    mat <- mat[-(1:2), , drop = FALSE]

    # bind the original column names as the first row
    mat <- cbind.data.frame(data.frame(name = I(as.list(fields$name[-(1:2)]))), mat)
    names(mat[[1]]) <- rep("V0", nrow(mat)) # there must be some name

    colnames(mat) <- transposedFields[["name"]]

    fields <- rbind(transposedFields[1, ], fields)

    meta$fields <- transposedFields
    meta$originalFields <- fields

    x <- jaspTableWrapper(mat, meta)

    # this is ugly
    fields <- transposedFields

  } else if (!is.null(meta$transposed) && meta$transposed) {

    # TODO!
  } else {

    for (i in seq_along(x))  {
      if (fields[i, "type"] == "number")
        x[[i]] <- applyPillarNum(x[[i]], fields[i, "format"])
    }

  }

  attr(x, "indexEnv") <- list2env(list(index = 1L))

  # TODO: clean this up, we don't need the chunks anymore!
  # browser()
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

#' @export
tbl_sum.jaspTableWrapper <- function(x, ...) {
  if (getFormatOption("debug"))
    paste("jaspTableWrapper:", getTableMeta(x)$title)
  else getTableMeta(x)$title
}

#' @export
tbl_format_header.jaspTableWrapper <- function(x, setup, ...) {
  setup$tbl_sum
}

#' @export
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

#' @export
ctl_new_pillar.jaspTableWrapper <- function(controller, x, width, ..., title = NULL) {

  index <- attr(controller, "indexEnv")$index
  attr(controller, "indexEnv")$index <- index + 1L

  hasOverTitles <- attr(controller, "hasOverTitles", exact = TRUE)
  meta   <- getTableMeta(controller)
  fields <- meta$fields

  # TODO: avoid this lookup, maybe cache the column index or so
  # idx <- which(fields$name == dropBackTicks(title))[1L]
  title <- fields$title[index]
  if (title == "")
    title <- " " # pillar::new_pillar_title does bad things with ""

  title <- pillar::new_pillar_title(title)

  type <- pillar::new_pillar_type(x)
  data <- if (!is.list(x)) {
    pillar::pillar(x)$data
  } else {
      pillar::new_ornament(x)
  }

  columnWidth <- max(attr(title, "width"), attr(data, "width")) + 1

  # TODO: why did I do this again? I think to get rid of something within the t-test descriptives table?
  if ("lhs" %in% names(data[[1]][[1]])) {
    idxNA <- data[[1]][[1]]$lhs == "NA"
    data[[1]][[1]]$lhs[idxNA] <- ""
  } else {
    idxNA <- is.na(data[[1]][[1]])
    data[[1]][[1]][idxNA] <- ""
  }

  tmp <- getTableLineStyles()
  topLineChar <- tmp$topLineChar
  midLineChar <- tmp$midLineChar
  botLineChar <- tmp$botLineChar

  # TODO: maybe meta$firstColumnIsOverTitle should just be meta$tranposed?
  if (meta$transposed) {

    originalFields <- meta$originalFields
    rawdata <- character(length(x))
    # TODO: this needs to be wrapped in a function and could be reused in other places, e.g., tbl_format_setup.jaspTableWrapper
    for (i in seq_along(x))  {
      nm <- names(x)[i]
      j <- which(originalFields$name == nm)
      rawdata[i] <- switch(originalFields[j, "type"],
        "number"  = format(applyPillarNum(x[[i]], originalFields[j, "format"])),
        "integer" = pillar::num(x[[i]]),
        "string"  = pillar::char(x[[i]])
      )
    }

    # TODO: consider columnWidth <- max(title, columnWidth)?
    columnWidth <- max(attr(title, "width"), pillar::get_max_extent(rawdata))
    newData <- pillar::new_ornament(rawdata, columnWidth, "right")
    data <- pillar::new_pillar_component(list(newData), width = columnWidth)

  }



  if (hasOverTitles) {

    # not attr(x, ...) because the attributes are lost for tranposed tables
    overTitleInfo <- attr(controller[[index]], "overTitleInfo")
    if (!is.null(overTitleInfo)) {

      overTitle <- overTitleInfo$title
      ncols     <- overTitleInfo$noColumns
      index     <- overTitleInfo$index2

      columnWidth  <- max(columnWidth, nchar(overTitle))
      overTitleWidth <- columnWidth
      overTitle <- strrep(letters[index], columnWidth)
      # print("adding overtitle")
      # print(overTitle)
      overTitleLine <- midLineChar

    } else {

      overTitle      <- ""
      overTitleLine  <- ""
      overTitleWidth <- columnWidth

    }

    lst <- list(
      top_top_rule = pillar::new_pillar_component(list(strrep(topLineChar, columnWidth)),   width = columnWidth),
      overTitle    = pillar::new_pillar_component(list(overTitle),                          width = overTitleWidth),
      top_mid_rule = pillar::new_pillar_component(list(strrep(overTitleLine, columnWidth)), width = columnWidth),
      title        = title,
      bot_mid_rule = pillar::new_pillar_component(list(strrep(midLineChar, columnWidth)),   width = columnWidth),
      data         = data,
      bot_bot_rule = pillar::new_pillar_component(list(strrep(botLineChar, columnWidth)),   width = columnWidth)
    )

    if (getOption("debug"))
      lst <- append(lst, list(type = type), after = 4)

  } else {
    lst <- list(
      top_rule    = pillar::new_pillar_component(list(strrep(topLineChar, columnWidth)), width = columnWidth),
      title       = title,
      mid_rule    = pillar::new_pillar_component(list(strrep(midLineChar, columnWidth)), width = columnWidth),
      data        = data,
      bottom_rule = pillar::new_pillar_component(list(strrep(botLineChar, columnWidth)), width = columnWidth)
    )

    if (getOption("debug")) {
      lst <- append(lst, list(type = type), after = 3)
    }
  }

  return(pillar::new_pillar(lst))

  #   lst <- list(
  #     top_rule    = pillar::new_pillar_component(list(strrep(topLineChar, columnWidth)), width = columnWidth),
  #     title       = pillar::pillar_component(out$title),
  #     mid_rule    = pillar::new_pillar_component(list(strrep(midLineChar, columnWidth)), width = columnWidth),
  #     data        = pillar::new_pillar_component(list(newData),                       width = columnWidth),
  #     bottom_rule = pillar::new_pillar_component(list(strrep(botLineChar, columnWidth)), width = columnWidth)
  #   )
  #
  # } else if (meta$transposed) {
  #
  #   overTitleInfo <- attr(x, "overTitleInfo")
  #   if (!is.null(overTitleInfo)) {
  #     # browser()
  #     overTitle <- overTitleInfo$title
  #     ncols     <- overTitleInfo$noColumns
  #     index     <- overTitleInfo$index2
  #
  #     columnWidth  <- max(columnWidth, nchar(overTitle))
  #     overTitleWidth <- columnWidth#nchar(overTitle)
  #     overTitle <- strrep(letters[index], columnWidth)
  #     # if (index == 1) {
  #     # } else {
  #     #   overTitle <- ""
  #     #   overTitleWidth <- 0L
  #     # }
  #     overTitleLine <- midLineChar
  #
  #   } else {
  #     overTitle      <- ""
  #     overTitleLine  <- ""
  #     overTitleWidth <- columnWidth
  #
  #   }
  #
  #   lst <- list(
  #     top_top_rule = pillar::new_pillar_component(list(strrep(topLineChar, columnWidth)), width = columnWidth),
  #     overTitle    = pillar::new_pillar_component(list(overTitle),                     width = overTitleWidth),
  #     top_mid_rule = pillar::new_pillar_component(list(strrep(overTitleLine, columnWidth)), width = columnWidth),
  #     title        = out$title,
  #     bot_mid_rule = pillar::new_pillar_component(list(strrep(midLineChar, columnWidth)), width = columnWidth),
  #     data         = out$data,
  #     bot_bot_rule = pillar::new_pillar_component(list(strrep(botLineChar, columnWidth)), width = columnWidth)
  #   )
  #
  #   if (debugIsOn())
  #     lst <- append(lst, list(type = out$type), after = 4)
  #
  # } else {
  #   lst <- list(
  #     top_rule    = pillar::new_pillar_component(list(strrep(topLineChar, columnWidth)), width = columnWidth),
  #     title       = out$title,
  #     mid_rule    = pillar::new_pillar_component(list(strrep(midLineChar, columnWidth)), width = columnWidth),
  #     data        = out$data,
  #     bottom_rule = pillar::new_pillar_component(list(strrep(botLineChar, columnWidth)), width = columnWidth)
  #   )
  #
  #   if (debugIsOn())
  #     lst <- append(lst, list(type = out$type), after = 3)
  # }
  #
  # if (!rlang::is_bare_list(lst))
  #   browser()
  #
  # return(pillar::new_pillar(lst))
}

#' @export
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

  # fields <- getFields(x)
  fields <- getFields(setup$x)

  # TODO: decide if we want to do things like this to shade every 2nd row or so
  # could make complete themes like that
  # bgGrey <- crayon::make_style("grey40", bg = TRUE)
  # cat(bgGrey("This will have some fancy colors"), "\n")
  # use rstudioapi::getThemeInfo() to get the background color

  hasOvertitle <- any(fields[, "overTitle"] != "")

  # if the console is too narrow for the table, the extra columns are folded/ printed underneath
  # count how many columns there are printed underneath
  tableRows <- nrow(setup$df) + if (hasOvertitle) 6L else 4L
  tableReps <- length(body) / tableRows

  # TODO: specify some setting to just stack side-by-side, stack underneath, or omit columns with a footnote
  repIfNeeded <- function(i) {
    seq(i, i + tableRows * (tableReps - 1), length.out = tableReps)
  }

  if (tableReps > 1L) {

    newbody <- character(tableRows)
    for (i in seq_len(tableRows)) {
      idx <- repIfNeeded(i)
      newbody[i] <- paste(body[idx], collapse = " ")
    }

    body <- newbody
    repIfNeeded <- identity


  } else { # other behavior

  }


  # top line
  topLineIdx <- repIfNeeded(1L)
  body[topLineIdx] <- gsub("\\s", topLineChar, body[topLineIdx])

  # middle line: the line that separate column titles or over titles from the data
  startMidLine <- if (hasOvertitle) 5L else 3L
  midLineIdx <- repIfNeeded(startMidLine)
  body[midLineIdx] <- gsub("\\s", midLineChar, body[midLineIdx])

  # bottom line
  botLineIdx <- repIfNeeded(tableRows)
  body[botLineIdx]  <- gsub("\\s", botLineChar, body[botLineIdx])

  # optional: line between title and overtitle
  # TODO: the over
  if (hasOvertitle) {

    overTitleIdx <- repIfNeeded(2L)
    allOverTitles <- unique(setdiff(fields[["overTitle"]], ""))
    splitBody <- strsplit(body[overTitleIdx], "")
    titlesPerLine <- lapply(splitBody, function(x) setdiff(unique(x), " "))

    currentOverTitleIdx <- 1L
    for (i in seq_along(titlesPerLine)) {

      idxSeen <- integer()
      for (j in seq_along(titlesPerLine[[i]])) {

        target <- letters[currentOverTitleIdx]
        indices <- setdiff(which(splitBody[[i]] == target), idxSeen)
        # if (length(indices) == 0L)
        #   browser()

        idx <- indices[1L]:indices[length(indices)]

        splitBody[[i]][idx] <- strsplit(format(allOverTitles[currentOverTitleIdx], width = length(idx), justify = "centre"), "")[[1L]]
        idxSeen <- union(idxSeen, idx)
        currentOverTitleIdx <- currentOverTitleIdx + 1L

      }

      body[overTitleIdx[i]] <- paste(splitBody[[i]], collapse = "")

    }

    overTitleLineIdx <- repIfNeeded(3L)
    body[overTitleLineIdx] <- gsub(paste0(midLineChar, " ", midLineChar), strrep(midLineChar, 3L), body[overTitleLineIdx], fixed = TRUE)

  }

  # TODO: use append to add debug info

    # body[1L] <- gsub("\\s", topLineChar, body[1L])


  #   allOverTitles <- unique(setdiff(fields[["overTitle"]], ""))
  #   splitBody <- strsplit(body[2L], "")[[1L]]
  #   idxSeen <- integer()
  #   for (i in seq_along(allOverTitles)) {
  #
  #     target <- letters[i]
  #     indices <- setdiff(which(splitBody == target), idxSeen)
  #     idx <- try(indices[1L]:indices[length(indices)])
  #     if (inherits(idx, "try-errir"))
  #       browser()
  #
  #     splitBody[idx] <- strsplit(format(allOverTitles[i], width = length(idx), justify = "centre"), "")[[1L]]
  #     idxSeen <- union(idxSeen, idx)
  #
  #   }
  #   body[2L] <- paste(splitBody, collapse = "")
  #
  #   # matches <- regmatches(body[2L], regexpr("(\\S+ )(?=\\S+)", body[2L], perl = TRUE))
  #   # matches <- paste0(" ", substr(matches, 1L, nchar(matches) -1L))
  #   # body[2L] <- gsub("(\\S+ )(?=\\S+)", matches, body[2L], perl = TRUE)
  #
  #   if (debugIsOn()) {
  #     body[4L] <- gsub(paste0(midLineChar, " ", midLineChar), strrep(midLineChar, 3L), body[4L], fixed = TRUE)
  #     body[6L] <- gsub("\\s", midLineChar, body[6L])
  #   } else {
  #     body[3L] <- gsub(paste0(midLineChar, " ", midLineChar), strrep(midLineChar, 3L), body[3L], fixed = TRUE)
  #     body[5L] <- gsub("\\s", midLineChar, body[5L])
  #   }
  #   body[n]  <- gsub("\\s", botLineChar, body[n])
  # } else {
  #
  #   # body[1L] <- gsub("\\s", topLineChar, body[1L])
  #   body[3L] <- gsub("\\s", midLineChar, body[3L])
  #   body[n]  <- gsub("\\s", botLineChar, body[n])
  # }
  body
}
