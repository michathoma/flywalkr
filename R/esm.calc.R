esm.calc <-function(directory, tubeDelay, windspeed, interval, before.after, priority.interval){

  ###### Version 1.0; 131030

  ###### variable legend:
  ###### directory        ... directory where data-files are stored
  ###### tubeDelay        ... delay between valve opening and odor entering glass-tubes in seconds
  ###### windspeed        ... air flow within glass tubes in cm/s
  ###### interval         ... frequency of analysis -> if analysis @ 10Hz -> interval = 0.1s
  ###### before.after     ... time before and after meeting time to be analyzed
  ###### aim              ... time vector for interpolated x-positions
  ###### speed.aim        ... time vector for x-velocities (-> also: header of extended speed matrix)
  ###### esm              ... extended speed matrix, the output data.frame
  ###### nfile            ... vector of filenames of files contained in directory
  ###### i.lf             ... running index for the loop analyzing all files within directory
  ###### data             ... single flywalk 2.0 output-file
  ###### odor             ... column in data containing odor information
  ###### od               ... odor vector modified to detect changes (if od != 0 -> odor has changed)
  ###### time             ... running time within data-file in seconds
  ###### odor.on          ... index of first timepoint of open valve
  ###### odor.start       ... first timepoint of open valve
  ###### odor.in          ... odor enters glasstubes
  ###### odor.intercept   ... intercept of the linear function of odor travel
  ###### odor.x           ... x-positions of odor @ time
  ###### j.fly.x          ... column numbers of x-coordinates of flies
  ###### fly.x            ... x-coordinates of fly of interest
  ###### diff.fly.odor    ... differences between x-coordinates of fly and odor, will be negative when odor has passed the fly
  ###### first            ... index of first odor contact
  ###### excl1&2          ... situations to be excluded: fly not tracked during or 1 frame before meeting time
  ###### ID.odor.pulse    ... data.frame for fly ID, odor and pulse number
  ###### speed.matrix     ... matrix for calculated speed
  ###### meetingTime      ... time in experiment of encounter with odor
  ###### new.time         ... Experimental time - meetingTime -> Meeting time will be set to 0
  ###### interpolated.x   ... interpolated x-coordinates of flies in the time and frequency specified in aim
  ###### speed            ... pretty obvious

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

    object <- read.table(nfile[i], sep=";", header=T, fill=T)
    object[object == "NaN"]<-NA

    #### 1st step: find linear function of odor travel

    odor <- object[,3]
    od <- odor - odor[1]
    time <- object[,2]/1000
    odor.on <- which(od!=0)[1]
    odor.start <- time[odor.on]
    odor.in <- odor.start+tubeDelay
    rel.time <- time-odor.start



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
