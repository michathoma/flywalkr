#' Plot overview over the experiment across flies.
#'
#' Plot overview over the experiment across flies. First calculates median response time-courses per odorant within flies.
#' From these, a boxplot including lines for mean and median time-courses per odorant across flies is created.
#'
#' @param object object a \code{data.frame} as produced by \code{read.esm}
#' @return \code{speed_overview.pdf}
#'
plot_box_all_1to7<-function(object){

  data.short <- data.frame(cbind(object[, 1:3], object[, 94:173]))
  data.clean <- data.short[!apply(is.na(data.short), 1, any),]

  animals <- levels(as.factor(data.clean$fly.nr))
  i.animals <- 1:length(levels(as.factor(data.clean$fly.nr)))
	medi.frame <- data.frame()


  for(i in i.animals){

	  animalx <- subset(data.clean, fly.nr == animals[i])
	  k.animal <- 1:dim(animalx)[1]


	  odors <- levels(as.factor(animalx$odour))
	  j.odors <- 1:length(odors)
	  mednum <- matrix(NA, ncol = 80, nrow = length(odors))
	  me <- data.frame(ncol = 2, nrow = length(odors))

	  for (j in j.odors){

		  odorx <- subset(animalx, odour == odors[j])

		  if(dim(odorx)[1] > 0){

		    medi <- apply(odorx[, 4:83], 2, median, na.rm = T)
		    me[j, 1] <- odorx[1, 1]
		    me[j, 2] <- as.character(odorx[1, 2])
		    mednum[j, 1:80] <- medi

		  } else {
		    me[j, 1] <- odorx[1, 1]
		    me[j, 2] <- as.character(odorx[1, 2])
		    mednum[j, 1:80] <- rep(NA, 80)
		  }
    }

	  medframe <- cbind(me, mednum)
		medi.frame <- rbind(medi.frame, medframe)
	}

	colnames(medi.frame) <- c("fly", "odor", seq(-0.95, 6.95, 0.1))

  write.table(medi.frame, file = "median_traces_within", sep=",", row.names = F)

  odornew <- levels(as.factor(medi.frame$odor))

  j.odornew <- 1:length(odornew)

  odcompl <- data.frame()
  meanmat <- matrix(NA, ncol = 80, nrow = length(odornew))
  medimat <- matrix(NA, ncol = 80, nrow = length(odornew))

  for (k in j.odornew){

	  odorj <- subset(medi.frame, odor == odornew[k])

	  odcompl[k, 1] <- odorj[1, 2]
	  mediodor <- apply(odorj[, 3:82], 2, median, na.rm = T)
	  meanodor <- apply(odorj[, 3:82], 2, mean, na.rm = T)

	  meanmat[k, 1:80] <- meanodor
	  medimat[k, 1:80] <- mediodor
	}


  meanframe <- cbind(odcompl, meanmat)
  mediframe <- cbind(odcompl, medimat)

  colnames(meanframe) <- c("odor", seq(-0.95, 6.95, 0.1))
  colnames(mediframe) <- c("odor", seq(-0.95, 6.95, 0.1))

  write.table(meanframe, file = "mean_traces_across", sep = ",", row.names = F)
  write.table(meanframe, file = "median_traces_across", sep = ",", row.names = F)



  pdf(file = "speed_overview.pdf", paper = "a4", height = 11.69, width = 8.27)
  par(mfrow = c(2, 2))

  for (k in j.odornew){

	  odorj <- subset(medi.frame, odor == odornew[k])

	  mediodor <- apply(odorj[, 3:82], 2, median, na.rm = T)
	  meanodor <- apply(odorj[, 3:82], 2, mean, na.rm = T)
	  boxplot(-odorj[, 3:82], ylim = c(-0.5, 1), main = odornew[k], outline = F, col = "white", border = "black")

	  lines(-mediodor, col = "red")
	  lines(-meanodor, col = "green")
  }

  dev.off()
}
