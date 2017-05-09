#' Consumption breaks dataframe wrapper
#'
#' This takes a dataframe of consumption, groups by the column parameter and returns a dataframe of mean consumption of the most
#' recent breakout by the ID specified.
#' @keywords breaks, breakoutDetection, dplyr
#' @param data dataframe of consumption and an ID
#' @param column an ID column to pass to the dplyr
#' @export
#' @examples
#' breaks_df()
#'

breaks_df <- function(data, column){
  data %>%
    dplyr::group_by_(.dots = column) %>%
    dplyr::summarise(DAILYAVG = breaks(DAILYAVG))
}
