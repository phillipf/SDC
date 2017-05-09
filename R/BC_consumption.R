#' Consumption data for Body corporate customers
#'
#' This takes a dataframe of current Body corporate customers and joins it with the PROP table produced by
#' the consumption function
#' @keywords Body corporate, consumption
#' @param PROP dataframe of consumption from CWW_DW database (dbo.GENTRACK_WATER_EFFICIENCY) in WVDBQDEVSQL
#' @param BC_report dataframe of current Body corporate customers produced by the BC_report on the tank
#' @export
#' @examples
#' BC_consumption()
#'

BC_consumption <- function() {

  BC_report <- getBCreport()

  PROP <- consumption()

  BC_dt <- data.table::data.table(BC_report)

  BC_dt2 <- BC_dt[, MASTERID:= as.numeric(substring(CMAS_CONSUMER, 3, 9))][,c(30, 20)]

  BC_dt3 <- unique(BC_dt2)

  data.table::setkey(BC_dt3, MASTERID, ACNAME)

  BC_consump <- BC_dt3[PROP, nomatch=0]

  BC_consump <- BC_consump[CONSUMP >= 0]

  return(BC_consump)

}


