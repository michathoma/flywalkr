#' Calculate distance covered within 4s after odor encounter.
#'
#' \code{distance_4s_calc} calculates the distance covered within 4s after odor encounter.
#' Rows containing missing values in the first 4s after odor encounter will be removed.
#' Results are written to \code{responses.csv}.
#'
#' @param object a \code{data.frame} as produced by \code{read.esm}
#' @return a \code{data.frame} containing net replacement within 4s per fly and odor
#'
distance_4s_calc <- function(object){

  data.short <- data.frame(cbind(object[,1:3], object[,104:144]))

  data.clean <- data.short[!apply(is.na(data.short), 1, any),]

  after <- apply(data.clean[,4:44], 1, sum, na.rm=T)
  after <- (-0.1 * after)


  resp.frame <- data.frame(cbind(data.clean[,1:3], after))

  testframe <- data.frame()


  for (i in 1:dim(resp.frame)[1]){
	  flyodor <- subset(resp.frame, fly.nr == resp.frame$fly.nr[i] & odour == resp.frame$odour[i])


	  n <- dim(flyodor)[1]
	  test.frame <- data.frame(flyodor[1,1], flyodor[1,2], n, mean(flyodor$after))
	  testframe <- rbind(testframe, test.frame)
	  testframe <- unique(testframe)
    }

  colnames(testframe) <- c("fly", "odor", "n", "response")
  write.table(testframe, file="responses", sep=",", row.names=F)

  return(testframe)

}
