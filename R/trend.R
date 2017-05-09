#' trend the count of Body corporate customers
#'
#' This reads in all reports for BC customers and creates a summary dataframe by date
#' @keywords Body corporate
#' @export
#' @examples
#' trend()
#'

trend <- function() {

  files <- list.files("N:/FWBI/SDCanalysis/Reports/PermanentBCreport", full.names = TRUE)

  reports <- files[grepl("BodyCorpSDC", files)]

  sort.list(as.Date(gsub(".*\\/([0-9]*)_.*$","\\1",reports), format = "%d%m%Y"), decreasing = F)

  reportdate <- plyr::ldply(seq_along(reports), function(i)
    c(reportdate = gsub(".*\\/([0-9]*)_.*$","\\1",reports[i]), loc = reports[i])) %>%
    mutate(reportdate = as.Date(reportdate, format = "%d%m%Y")) %>%
    arrange(reportdate)

  reports <- as.list(reportdate)

  count_bydate <- plyr::ldply(seq_along(reports$loc), function(i)
    c(New = ifelse(i > 1,
                   nrow(data.table::data.table(
                     data.table::fread(reports$loc[i], stringsAsFactors = F)[, MASTERID := as.numeric(substring(ID, 3, 9))],
                     key = "MASTERID")[!data.table::data.table(
                       data.table::fread(reports$loc[i - 1], stringsAsFactors = F)[, MASTERID := as.numeric(substring(ID, 3, 9))],
                       key = "MASTERID")]),
                   0),
      Removed = ifelse(i > 1,
                       nrow(data.table::data.table(
                         data.table::fread(reports$loc[i - 1], stringsAsFactors = F)[, MASTERID := as.numeric(substring(ID, 3, 9))],
                         key = "MASTERID")[!data.table::data.table(
                           data.table::fread(reports$loc[i], stringsAsFactors = F)[, MASTERID := as.numeric(substring(ID, 3, 9))],
                           key = "MASTERID")]),
                       0),
      date = gsub(".*\\/([0-9]*)_.*$","\\1",reports$loc[i]))) %>%
    dplyr::mutate(date = as.Date(date, format = "%d%m%Y"),
           Net = as.numeric(New) - as.numeric(Removed)) %>%
    dplyr::mutate(CumulativeTotal = cumsum(Net))

  return(count_bydate)

}
