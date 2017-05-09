#' Consumption breakout function
#'
#' This takes a vector of consumption and returns the mean consumption of the most recent breakout.
#' @keywords breakoutDetection
#' @param DAILYAVG a vector of daily average consumption
#' @export
#' @examples
#' breaks()
#'

breaks2 <- function(DAILYAVG) {

    out <- c(MeanBreakout = NA,
                BreakoutIndex = NA,
                anomIndex = NA,
                anomValue = NA,
                Veclength = NA)


    Veclength <- length(DAILYAVG[!is.na(DAILYAVG)])

    out[["Veclength"]] <- Veclength

    if(Veclength >= 5) {

      anoms = AnomalyDetection::AnomalyDetectionVec(DAILYAVG[!is.na(DAILYAVG)], max_anoms=0.02, direction='both', plot=F, period=2)

    }

      res = BreakoutDetection::breakout(DAILYAVG, min.size=4, method='multi', beta=.001, degree=1, plot=F)


      if(length(res$loc) >= 1) {

        out[["MeanBreakout"]] <- mean(DAILYAVG[tail(res$loc,1):length(DAILYAVG)])

        out[["BreakoutIndex"]] <- tail(res$loc,1)

      }

      if(nrow(anoms$anoms) >= 1) {

        out[["anomIndex"]] <- anoms$anoms$index[anoms$anoms$index == max(anoms$anoms$index)]

        out[["anomValue"]] <- anoms$anoms$anoms[anoms$anoms$index == max(anoms$anoms$index)]

      }

      else {

        out[["MeanBreakout"]] <- mean(DAILYAVG)

      }

   return(out)
}


