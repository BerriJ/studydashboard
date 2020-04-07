#' @title Delete Database Content
#'
#' @description This function deletes the database content.
#'
#' @param fs Logical wich defines if the fs data should be deleted.
#' @param studium Logical wich defines if the studium data should be deleted.
#' @param pruefung Logical wich defines if the pruefung data should be deleted.
#'
#' @export reset_db
#'
#' @seealso \code{\link{launchApp}}, \code{\link{launchApp_background}}
#'
#' @examples
#' # Delete fs and pruefung data:
#' reset_db(studium = FALSE)

reset_db <- function(fs = TRUE, studium = TRUE, pruefung = TRUE){

  if(fs) file.remove(system.file("extdata/fs.db", package="studydashboard"))

  if(studium) file.remove(system.file("extdata/studium.db", package="studydashboard"))

  if(pruefung) file.remove(system.file("extdata/pruefung.db", package="studydashboard"))

  print("Success!")
}
