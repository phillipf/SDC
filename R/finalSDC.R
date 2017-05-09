#' Final SDC
#'
#' This takes a customers' disposal class information and summarises it into a "FinalSDC"
#' @param customer a row from the BC_report dataframe returned from getBCreport.R
#' @export
#' @examples
#' FinalSDC()
#'

finalSDC <- function(customer) {

  if(!is.na(customer[["SDCF"]])) {

    return(as.numeric(customer[["SDCF"]]))

  }

  else if(!is.na(customer[["ITSOVDISPCLASS"]])) {

    return(as.numeric(customer[["OV_RIDE"]]))

  }

  else {

    return(as.numeric(customer[["DISP_FAC"]]))

  }

}
