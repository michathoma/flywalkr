read.esm<-function(directory,header=TRUE){

##### will read all .csv files within given directory and combine to a new dataframe
##### IMPORTANT: .csv have to be exported with ; as separator (default in EXCEL, no worries there), decimal separator must be "."
##### Also IMPORTANT: file names have to be in the format: e.g. 071210a_..., as the first part will be used to rename the flies (in case of multiple experiments)


setwd(directory)
nfile<-dir()
i.lf<-1:length(dir())
combdata <- numeric()

for (i in i.lf) 
{
if (header==FALSE) {
pcd <- read.csv(nfile[i],sep=";")

#### renaming starts here, split filename, the part before the _ will be used...

exp<-strsplit(nfile[i],"_")[[1]][1]

#### ...and rename by pasting experiment plus fly nr. from the esm

pcd$fly.nr<-paste(rep(exp,dim(pcd)[1]),pcd$fly.nr)

combdata <- rbind(combdata, pcd)
print(nfile[i])
}
else {
pcd <- read.csv(nfile[i],header==TRUE,sep=";")


exp<-strsplit(nfile[i],"_")[[1]][1]
pcd$fly.nr<-paste(rep(exp,dim(pcd)[1]),pcd$fly.nr,sep="_")

combdata <- rbind(combdata, pcd)
print(nfile[i])
}
}

combdata<-combdata[order(combdata$odour,combdata$fly.nr),]


#### setwd path has to be changed individually for different users/computers

setwd("C:/Documents and Settings/tretzke/My Documents/")
return(combdata)
}
