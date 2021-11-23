#' Shiny UI for feedr package
#'
#' User-friendly Shiny user interfaces for working with the feedr package.
#'
#' @references animalnexus <http://animalnexus.ca>.
#' @docType package
#' @name feedr-package
#' @aliases feedrUI feedrUI-package
#' @importFrom rlang .data
#' @import feedr
NULL


# Dealing with Non-standard evaluation
.onLoad <- function(libname = find.package("feedrUI"), pkgname = "feedrUI"){
  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(
      c(".") # piping requires '.' at times
    )
  invisible()
}
