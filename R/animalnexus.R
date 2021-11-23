#' Launch local animalnexus
#'
#' A local shiny app of the animalnexus site allowing users to import, transform and visualize data locally. Note that visualizations require an internet connection to download map tiles.
#'
#' @examples
#'
#' \dontrun{
#' animalnexus()
#' }
#'
#' @aliases animalnexus animalnexus-site
#' @export
animalnexus <- function() {
  appDir <- system.file("extdata", "shiny-examples", "animalnexus", package = "feedrUI")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `feedr`.", call. = FALSE)
  }

  shiny::runApp(appDir, launch.browser = TRUE)
}
