esm.calc<-function(directory,tubeDelay,windspeed,interval,before.after,priority.interval){
  
  ###### Version 1.0; 131030
  
  ###### variable legend:
  ###### directory        ... directory where data-files are stored
  ###### tubeDelay        ... delay between valve opening and odor entering glass-tubes in seconds
  ###### windspeed        ... air flow within glass tubes in cm/s
  ###### interval         ... frequency of analysis -> if analysis @ 10Hz -> interval = 0.1s
  ###### before.after     ... time before and after meeting time to be analyzed
  ###### aim              ... time vector for interpolated x-positions
  ###### speed.aim        ... time vector for x-velocities (-> also: header of extended speed matrix)
  ###### esm              ... extended speed matrix, the output data.frame
  ###### nfile            ... vector of filenames of files contained in directory
  ###### i.lf             ... running index for the loop analyzing all files within directory
  ###### data             ... single flywalk 2.0 output-file
  ###### odor             ... column in data containing odor information
  ###### od               ... odor vector modified to detect changes (if od != 0 -> odor has changed)
  ###### time             ... running time within data-file in seconds
  ###### odor.on          ... index of first timepoint of open valve
  ###### odor.start       ... first timepoint of open valve
  ###### odor.in          ... odor enters glasstubes
  ###### odor.intercept   ... intercept of the linear function of odor travel
  ###### odor.x           ... x-positions of odor @ time
  ###### j.fly.x          ... column numbers of x-coordinates of flies
  ###### fly.x            ... x-coordinates of fly of interest
  ###### diff.fly.odor    ... differences between x-coordinates of fly and odor, will be negative when odor has passed the fly
  ###### first            ... index of first odor contact
  ###### excl1&2          ... situations to be excluded: fly not tracked during or 1 frame before meeting time
  ###### ID.odor.pulse    ... data.frame for fly ID, odor and pulse number
  ###### speed.matrix     ... matrix for calculated speed
  ###### meetingTime      ... time in experiment of encounter with odor
  ###### new.time         ... Experimental time - meetingTime -> Meeting time will be set to 0
  ###### interpolated.x   ... interpolated x-coordinates of flies in the time and frequency specified in aim
  ###### speed            ... pretty obvious
  
  aim.start<--before.after                                           ###### new time interval for position
  aim.end<-before.after                                              ###### new time interval for position
  aim<-seq(aim.start,aim.end,interval)                               ###### new time interval for position
  
  speed.aim.start<--before.after+0.5*interval                        ###### new time interval for speed
  speed.aim.end<-before.after-0.5*interval                           ###### new time interval for speed
  speed.aim<-seq(speed.aim.start,speed.aim.end,interval)             ###### new time interval for speed
  
  esm<-data.frame()                                                  ###### create empty extended speed matrix                                      
  log<-data.frame()                                                  ###### create empty log-file
  failed.pulses<-data.frame()                                        ###### create empty data.frame for failed pulses
  #### need a for-loop for tracking events here!!!!!!
  setwd("~/")
  setwd(directory)                                                   ###### set working directory to directory containing tracking data
  nfile<-dir()                                                       ###### vector of filenames inside directory
  i.lf<-1:length(dir())                                              ###### running index for individual tracking csvs
  
  for (i in i.lf){
    
    object<-read.table(nfile[i],sep=";",header=T,fill=T)             ###### read ith tracking file
    object[object=="NaN"]<-NA                                        ###### replace NaNs with NAs (-> numeric vectors will not be converted to factors then)
    
    #### 1st step: find linear function of odor travel
    
    odor<-object[,3]                                                 ###### extract column containing odor information
    od<-odor-odor[1]                                                 ###### substract starting value from odor vector-> odor!=0 if valve switches
    time<-object[,2]/1000                                            ###### extract time column and convert ms to s
    odor.on<-which(od!=0)[1]                                         ###### find index of valve opening
    odor.start<-time[odor.on]                                        ###### find 1st timestamp with open valve
    odor.in<-odor.start+tubeDelay                                    ###### find timepoint of odor entering tube
    rel.time<-time-odor.start                                        ###### time of valve switching set to zero (only for log file)
    
    
    
    odor.intercept<--windspeed*odor.in                               ###### linear function f(x)=m*x+t solved for f(x)=0 (odor entering tube)
    odor.x<-windspeed*time+odor.intercept                            ###### linear function for odor in given stimulation cycle
  
    j.fly.x<-c(9,15,21,27,33,39,45,51,57,63,69,75,81,87,93)          ###### ???? which columns are the fly x-coordinates in cm
    
    
    
    time.change<-diff(time)                                          
    if(any(time.change>0.1)||any(time.change==0)){                   
      
      if(any(time.change>0.1)){
      
        print(paste(nfile[i]," jump in timer"))                       
        pulse.number<-object[odor.on,6]                                
        failed.pulse.i<-data.frame(nfile[i],pulse.number,"jump")
        colnames(failed.pulse.i)<-c("file","pulse","error")
        failed.pulses<-rbind(failed.pulses,failed.pulse.i)}
      
      if(any(time.change==0)){
        
        print(paste(nfile[i]," nonlinear timer"))                       
        pulse.number<-object[odor.on,6]                                
        failed.pulse.i<-data.frame(nfile[i],pulse.number,"discontinuous")
        colnames(failed.pulse.i)<-c("file","pulse","error")
        failed.pulses<-rbind(failed.pulses,failed.pulse.i)}
    
  ##### need a for-loop for items here!!!!!!
  
    } else {for (j in j.fly.x){
    
      fly.x<-object[,j]/10                                           ###### x-coordinates of fly of interest
      
      fly.x[fly.x<0.5]<-NA                                           ###### lower bound of artificial ROI
      fly.x[fly.x>19]<-NA                                            ###### upper bound of artificial ROI
      
      fly.x.ID<-which(j.fly.x==j)                                  ###### column number of x-positions -> fly ID
      fly.x.odor<-odor[odor.on]                                    ###### odor ID
      fly.x.pulse<-object[odor.on,6]                                 ###### pulse ID
      
      diff.fly.odor<-fly.x-odor.x                                    ###### when do odor and fly intersect?
      
      tracked.after.encounter<-any(which(diff.fly.odor<0))           ###### has the fly been tracked in artificial ROI after passage of the odor
      
      if(tracked.after.encounter==TRUE){                             ###### if so, analyze
      
        first<-which(diff.fly.odor<0)[1]                             ###### take the first negative value, i.e. the first timepoint at which the odor has passed the fly 
        
        priority.start<-first+priority.interval[1]*20
        priority.end<-first+priority.interval[2]*20
        
        excl1<-any(is.na(diff.fly.odor[first]))                      ###### exclusion1: fly not tracked at first negative value or previous one
        excl2<-any(is.na(diff.fly.odor[first-1]))                    ###### exclusion2: fly not tracked one frame before meeting time
        excl3<-any(is.na(diff.fly.odor[priority.start:priority.end]))
        
        ID.odor.pulse<-data.frame()                                  ###### empty data.frame for fly.nr, odor ID and pulse nr
        speed.matrix<-matrix(NA,ncol=length(speed.aim),nrow=1)       ###### empty matrix for interpolated speed
      
        
        fly.x.encounter<-rel.time[first]                             ###### delay between valve switching and odor encounter (should be below 1+tubeDelay)
      
        if (excl1==FALSE&&excl2==FALSE&&excl3==FALSE){               ###### if fly has been tracked around meeting time
      
          ID.odor.pulse[1,1]<-fly.x.ID                               ###### FLY!!!!!
          ID.odor.pulse[1,2]<-fly.x.odor                             ###### ODOR!!!!
          ID.odor.pulse[1,3]<-fly.x.pulse                            ###### PULSE!!!
        
        
          meetingTime<-time[first]                                   ###### meeting time is between first and previous
          new.time<-time-meetingTime                                 ###### new time with meeting time = 0 for individual flies
          interpolated.x<-approx(x=new.time,y=fly.x,xout=aim)        ###### interpolate x-position at defined interval 
        
          speed<-diff(interpolated.x$y)/interval                     ###### calculate speed in x-direction
        
          speed.matrix[1,1:length(speed.aim)]<-speed                 ###### enter fly speed in first (and only) row ofspeed.matrix
        
          fly.esm<-data.frame(ID.odor.pulse,speed.matrix)            ###### combine speed with other info
          colnames(fly.esm)<-c("fly.nr","odour","pulse",speed.aim)   ###### rename column names of fly.esm
        
          esm<-rbind(esm,fly.esm)                                    ###### attach fly.esm to whole esm
        
        
          fly.log<-data.frame(fly.x.ID,fly.x.odor,fly.x.pulse,fly.x.encounter,1)
          colnames(fly.log)<-c("fly.nr","odour","pulse","rel.meetingTime","analyzed")
          
          log<-rbind(log,fly.log)
        
          good.news<-paste("fly",fly.x.ID,"odor",fly.x.odor,"pulse",fly.x.pulse,"done")
          print(good.news)
      } else {
        
        fly.log<-data.frame(fly.x.ID,fly.x.odor,fly.x.pulse,NA,0)
        colnames(fly.log)<-c("fly.nr","odour","pulse","rel.meetingTime","analyzed")
        
        log<-rbind(log,fly.log)
        
        bad.news<-paste("fly",fly.x.ID,"odor",fly.x.odor,"pulse",fly.x.pulse,"could not be analyzed, no meeting time")
        print(bad.news)
        }}} 
  }

}

percent.analyzed<-sum(log[,5])/(length(log[,5])+15*length(levels(failed.pulses[,1])))
  if (percent.analyzed>=0.9){rank<-"Outstanding!!!!!"}
  if (percent.analyzed>=0.7&&percent.analyzed<0.9){rank<-"Exceeds Expectations!!!"}
  if (percent.analyzed>=0.5&&percent.analyzed<0.7){rank<-"Acceptable!"}
  if (percent.analyzed>=0.3&&percent.analyzed<0.5){rank<-"Poor"}
  if (percent.analyzed>=0.2&&percent.analyzed<0.3){rank<-"Dreadful"}
  if (percent.analyzed<0.2){rank<-"Troll"}
  
  
  
print(paste(percent.analyzed*100,"% of possible fly-odor-pulse combinations successfully analyzed -",rank))  

colnames(failed.pulses)<-c("file","pulse","error")
print("not able to process, failed timer:")
print(failed.pulses)

setwd("~/")         ###### set working directory back to my MyDocuments
  
write.table(esm,file="extendedSpeedMatrix.csv",sep=";",row.names=F)       ###### write extended speed matrix in working directory
write.table(log,file="analysis_log.csv",sep=";",row.names=F)
#write.table(failed.pulses,file="failed_pulses_log.csv",sep=";",row.names=F)

  return(esm)  
  

}