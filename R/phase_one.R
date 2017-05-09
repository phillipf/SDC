#' SDC project Phase one
#'
#' This reads in a static summary of the results from phase one of the SDC project
#' @keywords Body Corporate, phase one
#' @export
#' @examples
#' phase_one()
#'

phase_one <- function() {

  phaseone_BC <- read.csv("file:///N:/FWBI/SDCanalysis/KeyDocuments/BCsSDC.csv",
                          stringsAsFactors = FALSE) %>%
    rbind(read.csv("file:///N:/FWBI/SDCanalysis/KeyDocuments/SchoolsSDC.csv",
                   stringsAsFactors = FALSE, col.names = colnames(.)))

  return(phaseone_BC)

}
