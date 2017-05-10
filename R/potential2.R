#' potential BC customers to be returned to 0.9
#'
#' This reads in a static summary of the results from phase one of the SDC project
#' @keywords Body Corporate, phase one
#' @export
#' @examples
#' potential2()
#'

potential2 <- function() {

  BC_report <- getBCreport()

  BC_remaining <- remaining(BC_report)

  BC_remaining <- disposal_class(BC_remaining)

  BC_consump <- BC_consumption() %>%
                group_by(MASTERID) %>%
                filter(!all(DAILYAVG == 0)) %>%
                ungroup()

  BC_list <- plyr::dlply(BC_consump %>% select(MASTERID, DAILYAVG), "MASTERID" , function(i) i$DAILYAVG)

  BC_breaks <- plyr::ldply(names(BC_list), .id = names(BC_list), function(i)
    if(all(BC_list[[i]] == 0) | all(is.na(BC_list[[i]])))
    {return(c(Mean = 0,
              MeanBreakout = NA,
              BreakoutIndex = NA,
              anomIndex = NA,
              anomValue = NA,
              Veclength = NA))}

    else{breaks2(BC_list[[i]])})


  BC_breaks$MASTERID <- as.numeric(names(BC_list))

  BC_potential <- BC_breaks %>%
                  #filter(DAILYAVG >= 0.15) %>%
                  inner_join(BC_remaining)


  return(BC_potential)

}
