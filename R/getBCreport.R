#' get Body Corp Tank report
#'
#' This queries Gentrack and gets a report of the active customers with a Body Corp SDC
#' @keywords Body Corporate, Tank report, SRSS
#' @export
#' @examples
#' getBCreport()
#'

getBCreport <- function() {

  files <- list.files("N:/FWBI/SDCanalysis/Reports/PermanentBCreport", full.names = TRUE)

  today <- format(Sys.Date(), "%d%m%Y")

  filename <- paste("N:/FWBI/SDCanalysis/Reports/PermanentBCreport/", today, "_BodyCorpSDC.csv", sep = "")

  if(filename %in% files) {

    df <- readr::read_csv(filename)

    }

  else {

    reports <- files[grepl("BodyCorpSDC", files)]

    ord <- sort.list(as.Date(gsub(".*\\/([0-9]*)_.*$","\\1",reports), format = "%d%m%Y"), decreasing = F)

    df <- readr::read_csv(tail(reports[ord], 1))

  }

  return(df)
}
