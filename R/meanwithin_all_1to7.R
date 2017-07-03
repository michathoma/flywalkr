#' Calculate mean response time-course per fly and odor.
#'
#' \code{mean_within_all_1to7} calculates the mean response time-course per fly and odor.
#' Rows containing missing values in the interval between 1s before and 7s after odor encounter will be removed.
#' Results are written to \code{within_fly_mean_traces.csv} and a heatmap respresentation is exported (\code{mean_speed_heatmap.pdf}).
#'
#' @param object a \code{data.frame} as produced by \code{read.esm}
#' @return a \code{data.frame} containing mean response time-courses per fly and odor
#'

mean_within_all_1to7 <- function(object){

  data.short <- data.frame(cbind(object[,1:3], object[,94:173]))

  data.clean <- data.short[!apply(is.na(data.short), 1, any),]

  animals <- levels(as.factor(data.clean$fly.nr))
  i.animals <- 1:length(levels(as.factor(data.clean$fly.nr)))
	medi.frame <- data.frame()


  for(i in i.animals){

	  animalx <- subset(data.clean,fly.nr == animals[i])

	  k.animal <- 1:dim(animalx)[1]


	  odors <- levels(as.factor(animalx$odour))
	  j.odors <- 1:length(odors)
	  mednum <- matrix(NA, ncol=80, nrow=length(odors))
	  me <- data.frame(ncol=2, nrow=length(odors))

	  for (j in j.odors){

		  odorx <- subset(animalx, odour==odors[j])

		  if(dim(odorx)[1] > 0){

		  medi <- apply(odorx[,4:83], 2, mean, na.rm=T)


		  me[j, 1] <- odorx[1, 1]
		  me[j, 2] <- as.character(odorx[1, 2])

		  mednum[j, 1:80] <- medi}

		  else {
		  me[j, 1] <- odorx[1, 1]
		  me[j, 2] <- as.character(odorx[1, 2])

		  mednum[j,1:80]<-rep(NA,80)}


    }
		medframe <- cbind(me, mednum)
		medi.frame <- rbind(medi.frame, medframe)

  }
  colnames(medi.frame)<-c("fly", "odor", seq(-0.95, 6.95, 0.1))
  medi.frame <- medi.frame[order(medi.frame$odor, medi.frame$fly),]

  a <- -medi.frame[, 3:82]

  a[a > 1] <- 1
  a[a < (-1)] <- (-1)

  pdf(file="mean_speed_heatmap.pdf", paper="a4", height=11.69, width=8.27)

  gplots::heatmap.2(as.matrix(a), Rowv = F, Colv = F, dendrogram = "none", trace = "none", labRow = medi.frame$odor,
                    xlab = "time (s)", ylab = "odor", col = colorRampPalette(c("red", "white", "blue")),
                    main = "mean responses", symbreaks = T,key = T)

  dev.off()

  write.table(medi.frame, file="within_fly_mean_traces", sep=",", row.names=F)

  return(medi.frame)
}
