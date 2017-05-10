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

      out <- c(Mean = NA,
               MeanBreakout = NA,
               BreakoutIndex = NA,
               Veclength = NA,
               anomIndex = NA,
               anomValue = NA,
               MeanNoAnom = NA
               )


      Veclength <- length(DAILYAVG[!is.na(DAILYAVG)])

      out[["Veclength"]] <- Veclength

      if(Veclength >= 9) {

          anoms = AnomalyDetection::AnomalyDetectionVec(DAILYAVG[!is.na(DAILYAVG)], max_anoms=0.02, direction='pos', plot=F, period=4)

          res = BreakoutDetection::breakout(DAILYAVG, min.size=4, method='multi', beta=.001, degree=1, plot=F)

          if(!is.na(anoms$anoms$anoms) & !is.na(res$loc)) {

            out[["MeanBreakout"]] <- mean(DAILYAVG[tail(res$loc,1):length(DAILYAVG)], na.rm = T)

            out[["BreakoutIndex"]] <- tail(res$loc,1)

            out[["anomIndex"]] <- anoms$anoms$index[anoms$anoms$index == max(anoms$anoms$index)]

            out[["anomValue"]] <- anoms$anoms$anoms[anoms$anoms$index == max(anoms$anoms$index)]

            out[["MeanNoAnom"]] <- mean(DAILYAVG[tail(res$loc,1):length(DAILYAVG) & -anoms$anoms$index], na.rm = T)


          }

          else if(!is.na(anoms$anoms$anoms) & is.na(res$loc)) {

            out[["anomIndex"]] <- anoms$anoms$index[anoms$anoms$index == max(anoms$anoms$index)]

            out[["anomValue"]] <- anoms$anoms$anoms[anoms$anoms$index == max(anoms$anoms$index)]

            out[["MeanNoAnom"]] <- mean(DAILYAVG[-max(anoms$anoms$index)], na.rm = T)


          }

          else if(is.na(anoms$anoms$anoms) & !is.na(res$loc)){

            out[["MeanBreakout"]] <- mean(DAILYAVG[tail(res$loc,1):length(DAILYAVG)], na.rm = T)

            out[["BreakoutIndex"]] <- tail(res$loc,1)

          }

          else {

            out[["Mean"]] <- mean(DAILYAVG, na.rm = T)

          }
          }

      if(Veclength < 9) {

          res = BreakoutDetection::breakout(DAILYAVG, min.size=4, method='multi', beta=.001, degree=1, plot=F)

          if(!is.na(res$loc)) {

            out[["MeanBreakout"]] <- mean(DAILYAVG[tail(res$loc,1):length(DAILYAVG)], na.rm = T)

            out[["BreakoutIndex"]] <- tail(res$loc,1)

          }

          out[["Mean"]] <- mean(DAILYAVG, na.rm = T)

        }

      return(out)
}


