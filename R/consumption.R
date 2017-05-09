#' Consumption data
#'
#' This takes a dataframe of consumption, groups by the column parameter and returns a dataframe of mean consumption of the most
#' recent breakout by the ID specified.
#' @keywords breaks, breakoutDetection, dplyr
#' @param PROP dataframe of consumption from CWW_DW database (dbo.GENTRACK_WATER_EFFICIENCY) in WVDBQDEVSQL
#' @export
#' @examples
#' consumption()
#'

consumption <- function(){

  prop_dt <- data.table::fread("N:/FWBI/SDCanalysis/Reports/Consumption/PROP.csv")

  prop_dt2 <- prop_dt[,c(1,2,4,5,8,9)][ !is.na(MASTERID) ][, c("CONSUMP","DAYS"):=list(sum(CONSUMP, na.rm = T),
                                                                                       sum(DAYS, na.rm = T)),
                                                           by=list(MASTERID
                                                                   ,ACNAME
                                                                   ,FINANCIAL_YEAR
                                                                   ,Qtr)][, DAILYAVG := CONSUMP/ DAYS]

  data.table::setkey(prop_dt2)

  prop_dt3 <- unique(prop_dt2)

  setkey(prop_dt3, MASTERID, ACNAME)

  return(prop_dt3)

}
