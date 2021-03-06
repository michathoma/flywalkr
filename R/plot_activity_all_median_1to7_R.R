#' Calculate and plot median activity across flies.
#'
#' \code{plot_activity_all_median_1to7} calculates the mean activity within flies and calculates and plots the median activity time-courses across flies around odor encounter.
#' Rows containing missing values in the interval between 1s before and 7s after odor encounter will be removed.
#' Activity timecourses for individual flies are written to \code{within_fly_activity.csv} and a overview plot is exported (\code{activity_overview.pdf}).
#'
#' @param object a \code{data.frame} as produced by \code{read.esm}
#' @return a \code{data.frame} containing mean activity time-courses per fly and odor
#' @export
plot_activity_all_median_1to7 <- function(object){

  data.short <- data.frame(cbind(object[, 1:3], object[, 94:173]))

  data.clean <- data.short[!apply(is.na(data.short), 1, any),]

  data.num <- data.clean[, 4:83]

  act <- matrix(1, ncol = dim(data.num)[2], nrow = dim(data.num)[1])

  for (i in 1:dim(data.num)[1]){
	  for(j in 1:dim(data.num)[2]){
		  if (data.num[i, j] == "0"){act[i, j] <- 0}
		  if (data.num[i, j] == "NaN"){act[i, j] <- NA}}}

  data.clean <- cbind(data.clean[, 1:3], act)


  animals <- levels(as.factor(data.clean$fly.nr))
  i.animals <- 1:length(levels(as.factor(data.clean$fly.nr)))
	medi.frame <- data.frame()


  for(i in i.animals){

	  animalx <- subset(data.clean, fly.nr == animals[i])
    k.animal <- 1:dim(animalx)[1]


	  odors <- levels(as.factor(animalx$odour))
	  j.odors <- 1:length(odors)
	  mednum <- matrix(NA, ncol=80, nrow=length(odors))
	  me <- data.frame(ncol = 2,nrow = length(odors))

	  for (j in j.odors){

		  odorx <- subset(animalx, odour == odors[j])

		  if(dim(odorx)[1] > 0){

		    medi <- apply(odorx[, 4:83], 2, mean, na.rm = T)


		    me[j, 1] <- odorx[1, 1]
		    me[j, 2] <- as.character(odorx[1, 2])

		    mednum[j, 1:80] <- medi

		  } else {

		    me[j, 1] <- odorx[1, 1]
		    me[j, 2] <- as.character(odorx[1, 2])

		    mednum[j, 1:80] <- rep(NA, 80)}
    }

	  medframe <- cbind(me, mednum)
		medi.frame <- rbind(medi.frame, medframe)

  }
  colnames(medi.frame) <- c("fly", "odor", seq(-0.95, 6.95, 0.1))
  medi.frame <- medi.frame[order(medi.frame$odor, medi.frame$fly),]

  write.table(medi.frame, file = "within_fly_activity", sep=",", row.names = F)


  odornew <- levels(as.factor(medi.frame$odor))

  j.odornew <- 1:length(odornew)

  pdf(file = "activity_overview.pdf", paper = "a4", height=11.69, width=8.27)

  par(mfrow = c(3, 2))
  ylim = c(-1, 1)

  for (k in j.odornew){

	  odorj <- subset(medi.frame, odor == odornew[k])
	  mediodor <- apply(odorj[, 3:82], 2, median, na.rm = T)
	  meanodor <- apply(odorj[, 3:82], 2, mean, na.rm = T)
	  up <- apply(odorj[, 3:82], 2, quantile, probs=0.75, na.rm = T)
	  dn <- apply(odorj[, 3:82], 2, quantile, probs=0.25, na.rm = T)


	  xa <- seq(-0.95, 6.95, 0.1)
	  xarev <- rev(xa)

	  plot(x = seq(-0.95, 6.95, 0.1), y = mediodor, ylim = c(0, 1), main = odornew[k], col = "black", type = "n",
	     ylab="activity (%)",xlab="time (s)")

	  polygon(x = c(xa,xarev), y = c(up, rev(dn)), col = "grey80", border = "grey80")
	  lines(x = seq(-0.95, 6.95, 0.1), y = mediodor)

	  lines(x = c(0, 0), y = c(0, 1))
  }

  dev.off()

return(medi.frame)

}
