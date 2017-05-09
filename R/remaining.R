#' SDC project Phase one
#'
#' This reads in a static summary of the results from phase one of the SDC project
#' @keywords Body Corporate, phase one
#' @export
#' @examples
#' remaining()
#'

remaining <- function(BC_report) {

  BC_tracking <- phase_two()
  phaseone_BC <- phase_one()

  BC <- disposal_class(BC_report)

  Excemptions <- c(BC_tracking[grepl("Exempt|NO SEWER", BC_tracking$TT.Status, ignore.case = TRUE),]$CONSUMERNO,
                   phaseone_BC[phaseone_BC$TT.Status == "N",]$CONSUMERNO)

  Excemptions2 <- BC_report[!is.na(BC_report$ITSDISPCOMMENT),]$CMAS_CONSUMER

  BC_remaining <- BC %>%
    dplyr::filter(FINAL_SDC == 0) %>%
    dplyr::mutate(MASTERID = as.numeric(substr(CMAS_CONSUMER, 3, 9))) %>%
    #dplyr::left_join(PROP) %>%
    dplyr::filter(#FINANCIAL_YEAR == 2016,
           !(CMAS_CONSUMER %in% Excemptions),
           !(CMAS_CONSUMER %in% Excemptions2),
           !(CMAS_CONSUMER %in% BC_tracking$CONSUMERNO)) #%>% #filter out excempt
    # dplyr::select(CMAS_CONSUMER, MASTERID, Qtr, CONSUMP, DISPOSAL_DESCRIPTION) %>%
    # dplyr::group_by(CMAS_CONSUMER, MASTERID, Qtr, DISPOSAL_DESCRIPTION) %>%
    # dplyr::summarise(CONSUMP = sum(CONSUMP)) %>%
    # dplyr::ungroup %>%
    # dplyr::group_by(CMAS_CONSUMER, MASTERID, DISPOSAL_DESCRIPTION) %>%
    # dplyr::summarise(CONSUMP = mean(CONSUMP)) %>%
    # dplyr::mutate(BC = "NOTBILLED",
    #               Change = (CONSUMP * 0.9 * 1.7324) - 0)

  return(BC_remaining)

}
