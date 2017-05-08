#' write report to file
#'
#' This writes a report to file with today's date in the name
#' @param file a character string of the file location to write the report to, Defaults to working directory
#' @param name a character string of the report name
#' @keywords write.csv, report
#' @export
#' @examples
#' writereport()
#'

writereport <- function(report, file = getwd(), name) {

  if(!require(readr)) {
    message("installing the 'readr' package")
    install.packages("readr")
  }

  today <- format(Sys.Date(), "%d%m%Y")

  filename <- paste(file, today, name, ".csv", sep = "")

  write.csv(filename, row.names = FALSE)

}
