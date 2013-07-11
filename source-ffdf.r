#' Create a ffdf source.
#'
#' A ffdf source wraps a local ffdf.
#'
#' @export
#' @param data a ffdf data.frame
#' @examples
#' if (require("ffbase")) {
#' ds <- source_ffdf(mtcars)
#' ds
#' }
source_ffdf <- function(data) {
  if (!require("ffbase")) {
    stop("ffbase package required to use ffdf", call. = FALSE)
  }

  data <- as.ffdf(data)
  structure(list(obj = data),
    class = c("source_ffdf", "source"))
}

#' @S3method as.source ffdf
as.source.ffdf <- function(x, ...) {
  source_ffdf(x)
}

#' @S3method source_vars source_ffdf
source_vars.source_ffdf <- function(x) names(x$obj)

# Standard data frame methods --------------------------------------------------

#' Coerce data table to source.
#'
#' @export
#' @keywords internal
as.ffdf.source_ffdf <- function(x, keep.rownames = NULL) {
  if (!is.null(keep.rownames)) {
    warning("keep.rownames argument ignored", call. = FALSE)
  }

  x$obj
}

#' @S3method as.data.frame source_ffdf
as.data.frame.source_ffdf <- function(x, row.names = NULL, optional = FALSE, ...) {
  if (!is.null(row.names)) warning("row.names argument ignored", call. = FALSE)
  if (!identical(optional, FALSE)) warning("optional argument ignored", call. = FALSE)
  as.data.frame(x$obj, ...)
}

#' @S3method print source_ffdf
print.source_ffdf <- function(x, ...) {
  cat("Source:     ffdf ", dim_desc(x), "\n", sep = "")
  cat("\n")
  # TODO add head and tail to ffdf
  trunc_mat(x)
}

#' @S3method dimnames source_ffdf
dimnames.source_ffdf <- function(x) dimnames(x$obj)

#' @S3method dim source_ffdf
dim.source_ffdf <- function(x) dim(x$obj)

#' @S3method head source_ffdf
head.source_ffdf <- function(x, n=6L, ...) x$obj[seq_len(n), ] # NOTE no negative n supported!

#' @S3method tail source_ffdf
tail.source_ffdf <- function(x, n=6L, ...) tail(x$obj, n=n, ...)