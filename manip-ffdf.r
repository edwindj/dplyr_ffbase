#' Data manipulation for ffdf.
#'
#' @param .data a ffdf
#' @param ... variables interpreted in the context of \code{.data}
#' @param inplace if \code{FALSE} (the default) the data frame will be copied
#'   prior to modification to avoid changes propagating via reference.
#' @examples
#' if (require("ffbase")) {
#' data("baseball", package = "plyr")
#'
#' # If you start with a ffdf, you end up with a ffdf
#' baseball <- as.ffdf(baseball)
#' filter(baseball, year > 2005, g > 130)
#' head(select(baseball, id:team))
#' summarise(baseball, g = mean(g))
#' head(mutate(baseball, rbi = r / ab, rbi2 = rbi ^ 2))
#' head(arrange(baseball, id, desc(year)))
#'
#' # If you start with a source, you end up with a source
#' baseball_s <- as.source(baseball)
#' }
#' @name manip_ffdf
NULL

and_expr <- function(exprs) {
  assert_that(is.list(exprs))
  if (length(exprs) == 0) return(TRUE)
  if (length(exprs) == 1) return(exprs[[1]])

  left <- exprs[[1]]
  for (i in 2:length(exprs)) {
    left <- substitute(left & right, list(left = left, right = exprs[[i]]))
  }
  left
}

#' @rdname manip_ffdf
#' @export
#' @method filter ffdf
filter.ffdf <- function(.data, ...) {
  expr <- and_expr(dots(...))
  idx <- ffwhich(.data, as.expression(expr), envir=parent.frame())
  .data[idx, ]
}

#' @S3method filter source_ffdf
filter.source_ffdf <- function(.data, ...) {
  source_ffdf(
    filter.ffdf(.data$obj, ...)
  )
}

#' @rdname manip_ffdf
#' @export
#' @method summarise ffdf
summarise.ffdf <- function(.data, ...) {
  cols <- named_dots(...)
  list_call <- as.call(c(quote(list), named_dots(...)))
  call <- substitute(.data[, list_call])

  eval(call, parent.frame())
}

#' @S3method summarise source_ffdf
summarise.source_ffdf <- function(.data, ...) {
  source_ffdf(
    summarise.ffdf(.data$obj, ...)
  )
}

#' @rdname manip_ffdf
#' @export
#' @method mutate fdf
mutate.ffdf <- function(.data, ..., inplace = FALSE) {
  if (!inplace) .data <- clone(.data)

  env <- new.env(parent = parent.frame(), size = 1L)
  env$data <- .data

  cols <- dplyr:::named_dots(...)
  # For each new variable, generate a call of the form df[, new := expr]
  for(col in names(cols)) {
    call <- substitute(data[, lhs := rhs],
      list(lhs = as.name(col), rhs = cols[[col]]))
    eval(call, env)
  }

  .data
}

#' @S3method mutate source_ffdf
mutate.source_ffdf <- function(.data, ...) {
  source_ffdf(
    mutate.ffdf(.data$obj, ...)
  )
}

#' @rdname manip_ffdf
#' @export
#' @method arrange ffdf
arrange.ffdf <- function(.data, ...) {
  call <- substitute(data[fforder(...)])
  env <- new.env(parent = parent.frame(), size = 1L)
  env$data <- .data
  out <- eval(call, env)

  eval(call, env)
}

#' @S3method arrange source_ffdf
arrange.source_ffdf <- function(.data, ...) {
  source_ffdf(
    arrange.ffdf(.data$obj, ...)
  )
}

#' @rdname manip_ffdf
#' @export
#' @method select ffdf
select.ffdf <- function(.data, ...) {
  input <- var_eval(.data, dplyr:::dots(...), parent.frame())
  .data[input]
}

#' @S3method select source_ffdf
select.source_ffdf <- function(.data, ...) {
  source_ffdf(
    select.ffdf(.data$obj, ...)
  )
}

#' @S3method do ffdf
do.ffdf <- function(.data, .f, ...) {
  list(.f(as.data.frame(.data), ...))
}

#' @S3method do source_ffdf
do.source_ffdf <- function(.data, .f, ...) {
  list(.f(as.data.frame(.data$obj), ...))
}
