#' Run the example Shiny app
#'
#' Launch a small pixel art editor that demonstrates how to use
#' an [UndoManager] inside a Shiny app. The whole canvas is stored as a single
#' matrix and every stroke is one item in the manager's history, with buttons
#' and a timeline to move through it. The source is in
#' `system.file("examples-shiny/pixelart", package = "undomanager")`.
#'
#' Requires the \pkg{shiny} package.
#'
#' @param ... Passed on to [shiny::runApp()], such as `port` or `launch.browser`.
#' @return Does not return while the app is running.
#' @examples
#' if (interactive()) {
#'   run_example()
#' }
#' @export
run_example <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("run_example: the 'shiny' package is required to run the example app",
         call. = FALSE)
  }
  app_dir <- system.file("examples-shiny", "pixelart", package = "undomanager")
  if (app_dir == "") {
    stop("run_example: could not find the example app", call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
