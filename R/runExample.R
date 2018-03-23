#' Function used to run interactive apps included in the HannayIntroStats Package
#'
#' This function is useful to visualize clusters of states. It uses the kmeans function to form clusters of the states and then provides a visualization
#' of the data. State names should be saved as the row.names of the passed in data frame. Depends on the map function from the maps package.
#'
#' @param example the name of the app you want to run in string format
#' @return None
#' @examples
#'  runExample('AssessingNormality')
#' @export

runExample <- function(example) {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shiny-examples", package = "HannayIntroStats"))

  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(example) || !nzchar(example) ||
      !example %in% validExamples) {
    stop(
      'Please run `runExample()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("shiny-examples", example, package = "HannayIntroStats")
  shiny::runApp(appDir, display.mode = "normal")
}
