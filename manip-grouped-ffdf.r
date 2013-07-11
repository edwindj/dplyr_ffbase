#' Data manipulation for grouped data tables.
#'
#' @param .data a data table
#' @param ... variables interpreted in the context of \code{.data}
#' @param inplace if \code{FALSE} (the default) the data frame will be copied
#'   prior to modification to avoid changes propagating via reference.
#' @examples
#' if (require("ffbase")) {
#' data("baseball", package = "plyr")
#' baseball_ffdf <- as.ffdf(baseball)
#' players <- group_by(baseball_ffdf, id)
#
#' filter(players, g == max(g))
#' summarise(players, g = mean(g))
#' mutate(players, cyear = year - min(year) + 1)
#' arrange(players, id, desc(year))
#' select(players, id:team)
#'
#' # All manip functions preserve grouping structure, except for summarise
#' # (for hopefully obvious reasons)
#' by_year <- mutate(players, cyear = year - min(year) + 1)
#' summarise(by_year, years = max(cyear))
#'
#' # You can also manually ungroup:
#' arrange(ungroup(by_year), id, year)
#' }
#' @name manip_grouped_ffdf
NULL

#' @rdname manip_grouped_ffdf
#' @export
#' @method filter grouped_ffdf
filter.grouped_ffdf <- function(.data, ...) {
  expr <- and_expr(dots(...))

  env <- new.env(parent = parent.frame(), size = 1L)
  env$data <- .data$obj
  env$vars <- deparse_all(.data$vars)

  call <- substitute(data[data[, .I[expr], by = vars]$V1])
  out <- eval(call, env)

  grouped_ffdf(
    data = out,
    vars = .data$vars
  )
}

#' @rdname manip_grouped_ffdf
#' @export
#' @method summarise grouped_ffdf
summarise.grouped_ffdf <- function(.data, ...) {
  # Set keys, if needed
  keys <- deparse_all(.data$vars)
  if (!identical(keys, key(.data$obj))) {
    setkeyv(.data$obj, keys)
  }

  cols <- named_dots(...)
  list_call <- as.call(c(quote(list), dplyr:::named_dots(...)))
  call <- substitute(data[, list_call, by = vars])

  env <- new.env(parent = parent.frame(), size = 1L)
  env$data <- .data$obj
  env$vars <- keys
  out <- eval(call, env)

  grouped_ffdf(
    data = out,
    vars = .data$vars
  )
}

#' @rdname manip_grouped_ffdf
#' @export
#' @method mutate grouped_ffdf
mutate.grouped_ffdf <- function(.data, ..., inplace = FALSE) {
  data <- .data$obj
  # Set keys, if needed
  keys <- deparse_all(.data$vars)
  if (!inplace) data <- clone(data)

  env <- new.env(parent = parent.frame(), size = 1L)
  env$data <- data
  env$vars <- keys

  cols <- dplyr:::named_dots(...)
  # For each new variable, generate a call of the form df[, new := expr]
  for(col in names(cols)) {
    call <- substitute(data[, lhs := rhs, by = vars],
      list(lhs = as.name(col), rhs = cols[[col]]))
    eval(call, env)
  }

  grouped_ffdf(
    data = data,
    vars = .data$vars
  )
}

#' @rdname manip_grouped_ffdf
#' @export
#' @method arrange grouped_ffdf
arrange.grouped_ffdf <- function(.data, ...) {
  keys <- deparse_all(.data$vars)

  order_call <- as.call(c(quote(fforder), .data$vars, dots(...)))
  call <- substitute(data[order_call])

  env <- new.env(parent = parent.frame(), size = 1L)
  env$data <- .data$obj

  out <- eval(call, env)

  grouped_ffdf(
    data = out,
    vars = .data$vars
  )
}

#' @rdname manip_grouped_ffdf
#' @export
#' @method select grouped_ffdf
select.grouped_ffdf <- function(.data, ...) {
  input <- var_eval(.data$obj, dplyr:::dots(...), parent.frame())
  out <- .data$obj[input]

  grouped_ffdf(
    data = out,
    vars = .data$vars
  )
}


#' @S3method do grouped_ffdf
do.grouped_ffdf <- function(.data, .f, ...) {
  keys <- dplyr:::deparse_all(.data$vars)
  env <- new.env(parent = parent.frame(), size = 1L)
  env$data <- .data$obj
  env$vars <- keys
  env$f <- .f

  call <- substitute(data[, list(out = list(f(.SD, ...))), by = vars])
  eval(call, env)$out
}
