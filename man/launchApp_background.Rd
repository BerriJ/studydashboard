% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/launchApp_background.R
\name{launchApp_background}
\alias{launchApp_background}
\title{Launches the Studydashboard App as an RStudio Job}
\usage{
launchApp_background()
}
\description{
Note that RStudio as well as the 'rstudioapi' package are required in order for this function to work.

This allows you to run studydashboard in the background without keeping your console busy.
}
\seealso{
\code{\link{launchApp}},
\code{\link{reset_db}},
\link[rstudioapi]{jobRunScript}
}
