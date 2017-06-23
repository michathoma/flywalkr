read_esm <- function(directory, header=TRUE, delimiter = ";", parser = "_"){

##### will read all .csv files within given directory and combine to a new dataframe
##### IMPORTANT: .csv have to be exported with ; as separator (default in EXCEL, no worries there), decimal separator must be "."
##### Also IMPORTANT: file names have to be in the format: e.g. 071210a_..., as the first part will be used to rename the flies (in case of multiple experiments)

oldwd <- getwd()
setwd(directory)

files <- dir()

file_index<-1:length(files)
combdata <- numeric()

for (i in file_index)
{
file_i <- read.csv(files[i], header==TRUE, sep=delimiter)
experiment <- strsplit(files[i],parser)[[1]][1]
file_i$fly.nr <- paste(rep(experiment,dim(file_i)[1]),file_i$fly.nr,sep="_")
combdata <- rbind(combdata, file_i)
print(files[i])
}

combdata<-combdata[order(combdata$odour,combdata$fly.nr),]

setwd(oldwd)
return(combdata)
}
