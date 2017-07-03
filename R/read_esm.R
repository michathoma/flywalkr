#' Read all esm-csv in a gven directory.
#'
#' Will read all esm files in the given directory and concatenate them in single
#' \code{data.frame}. Filenames will be used to keep track of individual flies
#' in different experiments carrying the same within-experiment number.
#'
#' @param directory directory containing extended speed matrizes
#' @param delimiter delimiter used in extended speed matrizes, defaults to ";"
#' @param parser pattern to parse file names for naming of individual flies
#' @return \code{data.frame} containing combined data

read_esm <- function(directory, delimiter = ";", parser = "_"){

  oldwd <- getwd()
  setwd(directory)

  files <- dir()

  file_index<-1:length(files)
  combdata <- numeric()

  for (i in file_index){
    file_i <- read.csv(files[i], header = TRUE, sep = delimiter)
    experiment <- strsplit(files[i],parser)[[1]][1]
    file_i$fly.nr <- paste(rep(experiment,dim(file_i)[1]),file_i$fly.nr,sep="_")
    combdata <- rbind(combdata, file_i)
    print(files[i])
  }

  combdata<-combdata[order(combdata$odour,combdata$fly.nr),]

  setwd(oldwd)
  return(combdata)
}
