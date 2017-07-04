#' Calculate extended speed matrix from individual tracking events
#'
#' \code{esm_calc} collects the output from the tracking software and creates the extended speed matrix as a summary of a given experiment.
#' Time of encounter with the odorant (i.e. the intersection of a tracked item and a linear model of odor travel) is determined for every item individually.
#' After calculation of walking speed, individual speed trajectories are aligned to a common timeline. Tracking events will only be analyzed, if the timer contains
#' neither intervals longer than 0.1s nor intervals of length 0. To account for edge effects, x-position values < 0.5 and > 19 are treated as \code{NA}
#'
#' @param directory A directory containing tracking raw data.
#' @param tubeDelay a \code{double} specifying the delay (s) between the trigger to deliver the stimulus and the odor entering the glass tube.
#' @param windspeed a \code{double} specifying the wind speed (cm/s) inside the glasse tubes.
#' @param interval a \code{double} specifying the desired time interval (s) between two speed datapoints, typically 0.1s.
#' @param before.after a \code{double} specifying the time (s) before and after odor encounter to analyze.
#' @param priority.interval a vector specifying the time interval (s) during which the item has to be continually tracked to be considered for further analysis.
#' @return extended speed matrix, a \code{data.frame}, will also be written to file \code{extendedSpeedMatrix.csv}
#'
esm_calc <-function(directory, tubeDelay, windspeed, interval, before.after, priority.interval){

  aim.start <- -before.after
  aim.end <- before.after
  aim <- seq(aim.start, aim.end, interval)

  speed.aim.start <- -before.after + 0.5 * interval
  speed.aim.end <- before.after - 0.5 * interval
  speed.aim <- seq(speed.aim.start, speed.aim.end, interval)

  esm <- data.frame()
  log <- data.frame()
  failed.pulses <- data.frame()

  #### need a for-loop for tracking events here!!!!!!

  olddir <- getwd()
  setwd(directory)
  nfile <- dir()

  i.lf <- 1:length(dir())

  for (i in i.lf){

    object <- read.table(nfile[i], sep=";", header = T, fill = T)
    object[object == "NaN"] <- NA

    #### 1st step: find linear function of odor travel

    odor <- object[, 3]
    od <- odor - odor[1]
    time <- object[, 2] / 1000
    odor.on <- which(od != 0)[1]
    odor.start <- time[odor.on]
    odor.in <- odor.start + tubeDelay
    rel.time <- time - odor.start



    odor.intercept <- -windspeed * odor.in
    odor.x <- windspeed * time + odor.intercept

    j.fly.x <- c(9, 15, 21, 27, 33, 39, 45, 51, 57, 63, 69, 75, 81, 87, 93)



    time.change <- diff(time)
    if(any(time.change > 0.1) || any(time.change == 0)){

      if(any(time.change>0.1)){

        print(paste(nfile[i], " jump in timer"))
        pulse.number <- object[odor.on, 6]
        failed.pulse.i <- data.frame(nfile[i], pulse.number, "jump")
        colnames(failed.pulse.i) <- c("file", "pulse", "error")
        failed.pulses <- rbind(failed.pulses, failed.pulse.i)}

      if(any(time.change == 0)){

        print(paste(nfile[i], " nonlinear timer"))
        pulse.number <- object[odor.on, 6]
        failed.pulse.i <- data.frame(nfile[i], pulse.number, "discontinuous")
        colnames(failed.pulse.i) <- c("file", "pulse", "error")
        failed.pulses <- rbind(failed.pulses, failed.pulse.i)}



    } else {for (j in j.fly.x){

      fly.x <- object[,j] / 10

      fly.x[fly.x < 0.5] <- NA
      fly.x[fly.x > 19] <- NA

      fly.x.ID <- which(j.fly.x == j)
      fly.x.odor <- odor[odor.on]
      fly.x.pulse <- object[odor.on, 6]

      diff.fly.odor <- fly.x - odor.x

      tracked.after.encounter <- any(which(diff.fly.odor < 0))

      if(tracked.after.encounter == TRUE){

        first <- which(diff.fly.odor < 0)[1]

        priority.start <- first + priority.interval[1] * 20
        priority.end <- first + priority.interval[2] * 20

        excl1 <- any(is.na(diff.fly.odor[first]))
        excl2 <- any(is.na(diff.fly.odor[first-1]))
        excl3 <- any(is.na(diff.fly.odor[priority.start:priority.end]))

        ID.odor.pulse <- data.frame()
        speed.matrix <- matrix(NA, ncol=length(speed.aim), nrow = 1)


        fly.x.encounter <- rel.time[first]

        if (excl1 == FALSE && excl2 == FALSE && excl3 == FALSE){

          ID.odor.pulse[1,1] <- fly.x.ID
          ID.odor.pulse[1,2] <- fly.x.odor
          ID.odor.pulse[1,3] <- fly.x.pulse


          meetingTime <- time[first]
          new.time <- time - meetingTime
          interpolated.x <- approx(x = new.time, y = fly.x, xout = aim)
          speed <- diff(interpolated.x$y) / interval

          speed.matrix[1, 1:length(speed.aim)] <- speed
          fly.esm <- data.frame(ID.odor.pulse, speed.matrix)
          colnames(fly.esm)<-c("fly.nr", "odour", "pulse", speed.aim)

          esm <- rbind(esm, fly.esm)


          fly.log <- data.frame(fly.x.ID, fly.x.odor, fly.x.pulse, fly.x.encounter, 1)
          colnames(fly.log) <- c("fly.nr", "odour", "pulse", "rel.meetingTime", "analyzed")

          log <- rbind(log, fly.log)

          good.news <- paste("fly", fly.x.ID, "odor", fly.x.odor, "pulse", fly.x.pulse, "done")
          print(good.news)
      } else {

        fly.log <- data.frame(fly.x.ID, fly.x.odor, fly.x.pulse, NA, 0)
        colnames(fly.log)<-c("fly.nr", "odour", "pulse", "rel.meetingTime", "analyzed")

        log <- rbind(log, fly.log)

        bad.news <- paste("fly", fly.x.ID, "odor", fly.x.odor, "pulse", fly.x.pulse, "could not be analyzed, no meeting time")
        print(bad.news)
        }}}
  }

}

percent.analyzed <- sum(log[,5]) / (length(log[,5]) + 15 * length(levels(failed.pulses[,1])))
  if (percent.analyzed >= 0.9){                           rank <- "Outstanding!!!!!"}
  if (percent.analyzed >= 0.7 && percent.analyzed < 0.9){ rank <- "Exceeds Expectations!!!"}
  if (percent.analyzed >= 0.5 && percent.analyzed < 0.7){ rank <- "Acceptable!"}
  if (percent.analyzed >= 0.3 && percent.analyzed < 0.5){ rank <- "Poor"}
  if (percent.analyzed >= 0.2 && percent.analyzed < 0.3){ rank <- "Dreadful"}
  if (percent.analyzed < 0.2){                            rank <- "Troll"}



print(paste(percent.analyzed * 100, "% of possible fly-odor-pulse combinations successfully analyzed -", rank))

colnames(failed.pulses) <- c("file", "pulse", "error")
print("not able to process, failed timer:")
print(failed.pulses)

setwd(olddir)

write.table(esm, file="extendedSpeedMatrix.csv", sep=";", row.names=F)
write.table(log, file="analysis_log.csv", sep=";", row.names=F)
#write.table(failed.pulses,file="failed_pulses_log.csv",sep=";",row.names=F)

  return(esm)


}
