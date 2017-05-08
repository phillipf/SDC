#' get Body Corp Tank report
#'
#' This queries Gentrack and gets a report of the active customers with a Body Corp SDC
#' @keywords Body Corporate, Tank report, SRSS
#' @export
#' @examples
#' getBCdata()
#'

getBCData <- function() {

  cmd <- readLines("N:/FWBI/SDCanalysis/Reports/PermanentBCreport/GetData.txt")

  today <- format(Sys.Date(), "%d%m%Y")

  cmd <- gsub("(.*)(\\'\\D*)(\\d+)(.*\\')(.*)", paste0("\\1", "\\2", today, "\\4", ")"), cmd)

  shell(cmd, shell="C:/Windows/system32/cmd.exe")

}
