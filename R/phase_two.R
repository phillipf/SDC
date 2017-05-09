#' SDC project Phase two
#'
#' This reads in a static summary of the results from phase two of the SDC project
#' @keywords Body Corporate, phase two
#' @export
#' @examples
#' phase_two()
#'

phase_two <- function() {

  BC_tracking <- read.csv("N:/FWBI/SDCanalysis/Comms/Letters phase two/301116_BC_TrackingSheet.csv", stringsAsFactors = FALSE)

  BC_tracking <- BC_tracking[,1:11, drop=TRUE]

  return(BC_tracking)

}
