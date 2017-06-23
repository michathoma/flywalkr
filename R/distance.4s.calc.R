distance.4s.calc<-function(object){


##### new dataframe including parameters and the interval between -2.05 and +3.05
data.short<-data.frame(cbind(object[,1:3],object[,104:144]))

##### remove rows including NA values
data.clean<-data.short[!apply(is.na(data.short),1,any),]

####calculate mean speed for every single trace in the intervals [-1.05,-0.05] and [0.95,1.95]

after<-apply(data.clean[,4:44],1,sum,na.rm=T)
after<-(-0.1*after)


resp.frame<-data.frame(cbind(data.clean[,1:3],after))

testframe<-data.frame()


for (i in 1:dim(resp.frame)[1]){
	flyodor<-subset(resp.frame,fly.nr==resp.frame$fly.nr[i]&odour==resp.frame$odour[i])
	
#### exclude mean traces with a sample size < 3

	n<-dim(flyodor)[1]
	test.frame<-data.frame(flyodor[1,1],flyodor[1,2],n,mean(flyodor$after))
	testframe<-rbind(testframe,test.frame)
	testframe<-unique(testframe)



}

#### rename columns
colnames(testframe)<-c("fly","odor","n","response")
write.table(testframe,file="responses",sep=",",row.names=F)
##setwd("./Data/Flywalk/New/")

#### export .csv, can be imported in excel
##write.table(testframe,file="response_test.csv",sep=";",row.names=F)
return(testframe)

}
