#' get Body Corp Tank report
#'
#' This queries Gentrack and gets a report of the active customers with a Body Corp SDC
#' @keywords Body Corporate, Tank report, SRSS
#' @export
#' @examples
#' getBCreport()
#'

getBCreport <- function() {

  today <- format(Sys.Date(), "%d%m%Y")

  filename <- paste("N:/FWBI/SDCanalysis/Reports/PermanentBCreport/", today, "_BodyCorpSDC.csv", sep = "")

  readr::read_csv(filename)

}
