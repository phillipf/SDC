#' Disposal class table
#'
#' This takes the BC_report dataframe and adds in a column for the disposal factor
#' @param BC_report the BC_report returned by getBCdata.R
#' @export
#' @examples
#' disposal_class()
#'

disposal_class <- function(BC_report) {

  DISP_CLASS <- read.csv("N:/FWBI/SDCanalysis/Reports/PermanentBCreport/091216_DISPOSAL_CLASS_TABLE.csv",
                         stringsAsFactors = FALSE) %>%
    dplyr::mutate(DISFACTOR = DISFACTOR/10000)

  BC <- BC_report %>%
    dplyr::mutate(MASTERID = as.numeric(substring(CMAS_CONSUMER, 3, 9))) %>%
    dplyr::filter(!is.na(CMAS_CONSUMER)) %>%
    # dplyr::mutate(NUMBER_FIRST = LOCA_HOUSE,
    #        NUMBER_LAST = NA,
    #        FINAL_STREET = LOCA_STREET,
    #        FINAL_LOCALITY = LOCA_SUBURB,
    #        ADDRESS_TYPE = 1) %>%
    dplyr::select(CMAS_CONSUMER, MASTERID, ITSDISPCLASS:SDCF)

  BC$DISP_FAC <-  unlist(lapply(seq_along(BC$ITSDISPCLASS),
                                function(i)
                                  if(is.na(BC$DISP_FAC[i])) {
                                    DISP_CLASS[DISP_CLASS$DISPCLASS == BC$ITSDISPCLASS[i],]$DISFACTOR}
                                else {BC$DISP_FAC[i]}
                                )
                         )

  BC$OV_RIDE <-  unlist(lapply(seq_along(BC$ITSOVDISPCLASS),
                               function(i)
                                 if(is.na(BC$OV_RIDE[i]) & !is.na(BC$ITSOVDISPCLASS[i])) {
                                   DISP_CLASS[DISP_CLASS$DISPCLASS == BC$ITSOVDISPCLASS[i],]$DISFACTOR}
                               else {BC$OV_RIDE[i]}
                               )
                        )

  BC$FINAL_SDC <- apply(BC, 1, finalSDC)

  return(BC)

}
