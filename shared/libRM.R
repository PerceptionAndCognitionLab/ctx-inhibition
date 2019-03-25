excludeSub=c(132,164,368,402,429)  #from Rey-Mermet's code

readData=function(fileroot,datCols,
                  colNames=c('sub','ageGroup','block','trialType','cond','stim','acc','rt'))
  {
	cmd=paste("ls -1 ",fileroot," >temp",sep='')
	system(cmd)
	filenames=paste(fileroot,read.table('temp')[,1],sep='')
	I=length(filenames)

	dat=NULL
	for (i in 1:I){
		mysub=read.table(filenames[i])
		dat=rbind(dat,mysub)
	}

	dat=dat[,datCols]
	colnames(dat)=colNames
	return(dat)}
	
cleanData=function(dat,
  top=1.995,                 
	hiTrlRT=1.995,
	loTrlRT=.275,
	loTop=.98,
	loAcc=.90,
	loRT=.99){
	#exclusions
	bad0=dat$sub %in% excludeSub
	bad1=dat$block == 'practice'
	bad2=dat$trialType != 'exp'
	bad3=!(dat$acc %in% c(0,1)) #there are some strange things
	dat=dat[!(bad0 | bad1 | bad2 | bad3),]	
	sub=tapply(dat$sub,dat$sub,mean)
	topBySub=tapply(dat$rt<top,dat$sub,mean)
	accBySub=(tapply(dat$acc,dat$sub,mean))
	loBySub=tapply(dat$rt>loTrlRT,dat$sub,mean)	
	bad4=dat$sub %in% sub[
		accBySub<loAcc |
		topBySub<loTop |
		loBySub<loRT]
	bad5 = dat$rt<loTrlRT | dat$rt>min(hiTrlRT,top) | dat$acc==0 
	dat=dat[!(bad4 | bad5),]	
	return(dat)
}
