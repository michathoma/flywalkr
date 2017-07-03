response_per_pulse<-function(object){

  data.short <- data.frame(cbind(object[, 1:3], object[, 104:144]))

  data.clean <- data.short[!apply(is.na(data.short), 1, any),]

  after <- apply(data.clean[, 4:44], 1, sum, na.rm = T)
  after <- (-0.1 * after)


  resp.frame <- data.frame(cbind(data.clean[, 1:3], after))

  testframe <- data.frame()


  for (i in 1:dim(resp.frame)[1]){
	  flypulse <- subset(resp.frame, pulse == resp.frame$pulse[i] & odour == resp.frame$odour[i])
	  n <- dim(flypulse)[1]
	  test.frame <- data.frame(flypulse[1, 2], flypulse[1, 3], n, mean(flypulse$after))
	  testframe <- rbind(testframe, test.frame)
	  testframe <- unique(testframe)
  }

  colnames(testframe) <- c("odor", "pulse", "n", "response")
  write.table(testframe, file = "responses", sep = ",", row.names = F)

  return(testframe)
}
