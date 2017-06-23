plot.trace.all.1to7<-function(object){

  colnames(object)<-c("fly.nr","odour","pulse",seq(-9.95,9.95,0.1))
##### exports pdf for each animal, plot for all odors include single measurements, median trace and interquartile range
##### this version limits to -2.95-+9.95, bur rapidly adaptable to other needs
##### 
##### new dataframe including parameters and the interval between -2.95 and +9.95
data.short<-data.frame(cbind(object[,1:3],object[,94:173]))

##### remove rows including NA values
data.clean<-data.short[!apply(is.na(data.short),1,any),]

animals<-levels(as.factor(data.clean$fly.nr))
i.animals<-1:length(levels(as.factor(data.clean$fly.nr)))

for(i in i.animals){

	animalx<-subset(data.clean,fly.nr==animals[i])
	animalx$odour<-factor(animalx$odour)
	#### ylim=c(min(animalx[,4:53]),max(animalx[,4:53]))
	ylim=c(-1,1)
	pdf(file=paste(animals[i],"_overview.pdf"),paper="a4",height=11.69,width=8.27)

	k.animal<-1:dim(animalx)[1]
	par(mfrow=c(3,3))
	
	odors<-levels(as.factor(animalx$odour))
	j.odors<-1:length(odors)

	for (j in j.odors){

		odorx<-subset(animalx,odour==odors[j])
		odorx$odour<-factor(odorx$odour)
		
		mediodor<-apply(odorx[,4:83],2,median,na.rm=T)
		meanodor<-apply(odorx[,4:83],2,mean,na.rm=T)
		
		low<-c()
		high<-c()
		
		#for (k in 4:63){
		#	low[k]<-quantile(odorx[,k],probs=.25)
		#	high[k]<-quantile(odorx[,k],probs=.75)
		#	
		#	}

#		low<-c(na.omit(low))
#		high<-c(na.omit(high))
		

		x<-seq(-0.95,6.95,0.1)
		xr<-rev(x)

		plot(seq(-0.95,6.95,0.1),-mediodor,type="n",xlim=c(-1,7),ylim=ylim,main=paste(animalx[1,1],odorx$odour[1]),xlab="time(s)",ylab="mean speed (cm/s)", bty="n")

##### quartile shadings
		
		
		#### polygon(x=c(x,xr),y=c(high,rev(low)),col="gray",border="NA")
##### single traces
		
		for(l in 1:dim(odorx)[1]){
			lines(x,-odorx[l,4:83],col="gray")}

##### median trace

		lines(x,-mediodor,col="red")
		lines(x,-meanodor,col="green")
		
		lines(x=c(0,0),y=ylim)
		legend("topright",legend=paste("N=",dim(odorx)[1]),bty="n")
		}

	dev.off()

}
}
