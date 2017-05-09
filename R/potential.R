#' potential BC customers to be returned to 0.9
#'
#' This reads in a static summary of the results from phase one of the SDC project
#' @keywords Body Corporate, phase one
#' @export
#' @examples
#' potential()
#'

potential <- function() {

  BC_report <- getBCreport()

  BC_remaining <- remaining(BC_report)

  BC_remaining <- disposal_class(BC_remaining)

  BC_consump <- BC_consumption()

  BC_breaks <- breaks_df(BC_consump, "MASTERID")

  BC_potential <- BC_breaks %>%
                  filter(DAILYAVG >= 0.15) %>%
                  inner_join(BC_remaining)


  return(BC_potential)

}
