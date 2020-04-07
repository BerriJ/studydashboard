#' @title Launches the Studydashboard App as an RStudio Job
#'
#' @description Note that RStudio as well as the 'rstudioapi' package are required in order for this function to work.
#' @description This allows you to run studydashboard in the background without keeping your console busy.
#'
#' @seealso \code{\link{launchApp}},
#' \code{\link{reset_db}},
#' \link[rstudioapi]{jobRunScript}
#'
#' @export launchApp_background


launchApp_background <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("Package \"rstudioapi\" needed for this function to work. Please install it or use launchApp() to start the dashboard in your console.",
         call. = FALSE)
  }
  rstudioapi::jobRunScript(system.file("job.R", package="studydashboard"))
}
