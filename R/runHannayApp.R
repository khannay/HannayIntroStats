#' Run an App
#'
#' This can be used to run the included apps in the HannayIntroStats package.
#'
#'
#' @param example the name of the app you want to run in string format
#' @return None
#' @examples
#'  runExample('AssessingNormality')
#' @export

runHannayApp <- function(example) {
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
