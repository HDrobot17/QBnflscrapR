#initialize array arrays
qbDeepL=c(1:6)
qbDeepM=c(1:6)
qbDeepR=c(1:6)
qbShortL=c(1:6)
qbShortM=c(1:6)
qbShortR=c(1:6) #qb pass bucket vectors

apDeepL=c(1:6)
apDeepM=c(1:6)
apDeepR=c(1:6)
apShortL=c(1:6)
apShortM=c(1:6)
apShortR=c(1:6)#all players pass bucket vectors

qbDeepLcp=c(1:6)
qbDeepMcp=c(1:6)
qbDeepRcp=c(1:6)
qbShortLcp=c(1:6)
qbShortMcp=c(1:6)
qbShortRcp=c(1:6) #QB completion vectors

apDeepLcp=c(1:6)
apDeepMcp=c(1:6)
apDeepRcp=c(1:6)
apShortLcp=c(1:6)
apShortMcp=c(1:6)
apShortRcp=c(1:6) # all player completion vectors

year=c(2016:2011)
QBArray=array(0,dim=c(2,3))#initialize array
dataQB=array(0,dim=c(6,6))
dataAP=array(0,dim=c(6,6))
CdataAP=array(0,dim=c(6,6))
CdataQB=array(0,dim=c(6,6))
EPAdataAP=array(0,dim=c(6,6))
EPAdataQB=array(0,dim=c(6,6))
for(i in 1:6){
	PBP=read_excel("PlayByPlay.xlsx", sheet=i) # puts the 2016 data into R
	#build area of field array
	AllPass=subset(PBP, PlayType == "Pass") #eliminate non-pass data
	QBPass=subset(AllPass, Passer == "D.Carr") #QB's passes
	APDeep=subset(AllPass, PassLength == "Deep") #all deep passes
	APShort=subset(AllPass, PassLength == "Short") #all short passes
	QBDeep=subset(QBPass, PassLength == "Deep") #QB deep passes
	QBShort=subset(QBPass, PassLength == "Short") #QB short passes
	# some discrepancy, but only 0.68% of all passes
	APDeepM=subset(APDeep, PassLocation == "middle") #more subsets
	APDeepL=subset(APDeep, PassLocation == "left")
	APDeepR=subset(APDeep, PassLocation == "right")
	QBDeepM=subset(QBDeep, PassLocation == "middle")
	QBDeepL=subset(QBDeep, PassLocation == "left")
	QBDeepR=subset(QBDeep, PassLocation == "right")
	APShortM=subset(APShort, PassLocation == "middle")
	APShortL=subset(APShort, PassLocation == "left")
	APShortR=subset(APShort, PassLocation == "right")
	QBShortM=subset(QBShort, PassLocation == "middle")
	QBShortL=subset(QBShort, PassLocation == "left")
	QBShortR=subset(QBShort, PassLocation == "right")
	# Russ's array
	qbDeepL[i]=nrow(QBDeepL)
	qbDeepM[i]=nrow(QBDeepM)
	qbDeepR[i]=nrow(QBDeepR)
	qbShortL[i]=nrow(QBShortL)
	qbShortM[i]=nrow(QBShortM)
	qbShortR[i]=nrow(QBShortR)
	QBArray=array(0,dim=c(2,3))#initialize array
	QBArray[1,1]=qbDeepL[i]
	QBArray[1,2]=qbDeepM[i]
	QBArray[1,3]=qbDeepR[i]
	QBArray[2,1]=qbShortL[i]
	QBArray[2,2]=qbShortM[i]
	QBArray[2,3]=qbShortR[i]
	QBTot=sum(QBArray)
	dataQB[i,1]=qbDeepL[i]
	dataQB[i,2]=qbDeepM[i]
	dataQB[i,3]=qbDeepR[i]
	dataQB[i,4]=qbShortL[i]
	dataQB[i,5]=qbShortM[i]
	dataQB[i,6]=qbShortR[i]
	EPAdataQB[i,1]=mean(as.numeric(unlist(QBDeepL["EPA"])), na.rm=TRUE)
	EPAdataQB[i,2]=mean(as.numeric(unlist(QBDeepM["EPA"])), na.rm=TRUE)
	EPAdataQB[i,3]=mean(as.numeric(unlist(QBDeepR["EPA"])), na.rm=TRUE)
	EPAdataQB[i,4]=mean(as.numeric(unlist(QBShortL["EPA"])), na.rm=TRUE)
	EPAdataQB[i,5]=mean(as.numeric(unlist(QBShortM["EPA"])), na.rm=TRUE)
	EPAdataQB[i,6]=mean(as.numeric(unlist(QBShortR["EPA"])), na.rm=TRUE)
	RelQBArray=QBArray/QBTot
	print(year[i])
	print(RelQBArray)
	# All player's array
	apDeepL[i]=nrow(APDeepL)
	apDeepM[i]=nrow(APDeepM)
	apDeepR[i]=nrow(APDeepR)
	apShortL[i]=nrow(APShortL)
	apShortM[i]=nrow(APShortM)
	apShortR[i]=nrow(APShortR)
	APArray=array(0,dim=c(2,3))#initialize array
	APArray[1,1]=apDeepL[i]
	APArray[1,2]=apDeepM[i]
	APArray[1,3]=apDeepR[i]
	APArray[2,1]=apShortL[i]
	APArray[2,2]=apShortM[i]
	APArray[2,3]=apShortR[i]
	APTot=sum(APArray)
	dataAP[i,1]=apDeepL[i]
	dataAP[i,2]=apDeepM[i]
	dataAP[i,3]=apDeepR[i]
	dataAP[i,4]=apShortL[i]
	dataAP[i,5]=apShortM[i]
	dataAP[i,6]=apShortR[i]
	RelAPArray=APArray/APTot
	EPAdataAP[i,1]=mean(as.numeric(unlist(APDeepL["EPA"])), na.rm=TRUE)
	EPAdataAP[i,2]=mean(as.numeric(unlist(APDeepM["EPA"])), na.rm=TRUE)
	EPAdataAP[i,3]=mean(as.numeric(unlist(APDeepR["EPA"])), na.rm=TRUE)
	EPAdataAP[i,4]=mean(as.numeric(unlist(APShortL["EPA"])), na.rm=TRUE)
	EPAdataAP[i,5]=mean(as.numeric(unlist(APShortM["EPA"])), na.rm=TRUE)
	EPAdataAP[i,6]=mean(as.numeric(unlist(APShortR["EPA"])), na.rm=TRUE)
	print(year[i])
	print(RelAPArray)
	RelRel=RelQBArray/RelAPArray # attempts
		
	#time to try to figure out completions
	APDeepMComp=subset(APDeepM, PassOutcome == "Complete") #more subsets
	APDeepLComp=subset(APDeepL, PassOutcome == "Complete")
	APDeepRComp=subset(APDeepR, PassOutcome == "Complete")
	QBDeepMComp=subset(QBDeepM, PassOutcome == "Complete")
	QBDeepLComp=subset(QBDeepL, PassOutcome == "Complete")
	QBDeepRComp=subset(QBDeepR, PassOutcome == "Complete")
	APShortMComp=subset(APShortM, PassOutcome == "Complete")
	APShortLComp=subset(APShortL, PassOutcome == "Complete")
	APShortRComp=subset(APShortR, PassOutcome == "Complete")
	QBShortMComp=subset(QBShortM, PassOutcome == "Complete")
	QBShortLComp=subset(QBShortL, PassOutcome == "Complete")
	QBShortRComp=subset(QBShortR, PassOutcome == "Complete")
	#QBcompletions
	qbDeepLcp[i]=nrow(QBDeepLComp)
	qbDeepMcp[i]=nrow(QBDeepMComp)
	qbDeepRcp[i]=nrow(QBDeepRComp)
	qbShortLcp[i]=nrow(QBShortLComp)
	qbShortMcp[i]=nrow(QBShortMComp)
	qbShortRcp[i]=nrow(QBShortRComp)
	QBArrayC=array(0,dim=c(2,3))#initialize array
	QBArrayC[1,1]=qbDeepLcp[i]
	QBArrayC[1,2]=qbDeepMcp[i]
	QBArrayC[1,3]=qbDeepRcp[i]
	QBArrayC[2,1]=qbShortLcp[i]
	QBArrayC[2,2]=qbShortMcp[i]
	QBArrayC[2,3]=qbShortRcp[i]
	QBArrayCP=QBArrayC/QBArray
	CdataQB[i,1]=qbDeepLcp[i]
	CdataQB[i,2]=qbDeepMcp[i]
	CdataQB[i,3]=qbDeepRcp[i]
	CdataQB[i,4]=qbShortLcp[i]
	CdataQB[i,5]=qbShortMcp[i]
	CdataQB[i,6]=qbShortRcp[i]

	#AP completions
	apDeepLcp[i]=nrow(APDeepLComp)
	apDeepMcp[i]=nrow(APDeepMComp)
	apDeepRcp[i]=nrow(APDeepRComp)
	apShortLcp[i]=nrow(APShortLComp)
	apShortMcp[i]=nrow(APShortMComp)
	apShortRcp[i]=nrow(APShortRComp)
	APArrayC=array(0,dim=c(2,3))#initialize array
	APArrayC[1,1]=apDeepLcp[i]
	APArrayC[1,2]=apDeepMcp[i]
	APArrayC[1,3]=apDeepRcp[i]
	APArrayC[2,1]=apShortLcp[i]
	APArrayC[2,2]=apShortMcp[i]
	APArrayC[2,3]=apShortRcp[i]
	APArrayCP=APArrayC/APArray
	CdataAP[i,1]=apDeepLcp[i]
	CdataAP[i,2]=apDeepMcp[i]
	CdataAP[i,3]=apDeepRcp[i]
	CdataAP[i,4]=apShortLcp[i]
	CdataAP[i,5]=apShortMcp[i]
	CdataAP[i,6]=apShortRcp[i]
