# new pass length delinations
# initializes arrays!
#QB att arrays
qbScreenL=c(1:6)
qbScreenM=c(1:6)
qbScreenR=c(1:6)
qbShortL=c(1:6)
qbShortM=c(1:6)
qbShortR=c(1:6)
qbMidL=c(1:6)
qbMidM=c(1:6)
qbMidR=c(1:6)
qbDeepL=c(1:6)
qbDeepM=c(1:6)
qbDeepR=c(1:6)

#All passes att arrays 
apScreenL=c(1:6)
apScreenM=c(1:6)
apScreenR=c(1:6)
apShortL=c(1:6)
apShortM=c(1:6)
apShortR=c(1:6)
apMidL=c(1:6)
apMidM=c(1:6)
apMidR=c(1:6)
apDeepL=c(1:6)
apDeepM=c(1:6)
apDeepR=c(1:6)

#QB CP arrays
qbScreenLcp=c(1:6)
qbScreenMcp=c(1:6)
qbScreenRcp=c(1:6)
qbShortLcp=c(1:6)
qbShortMcp=c(1:6)
qbShortRcp=c(1:6)
qbMidLcp=c(1:6)
qbMidMcp=c(1:6)
qbMidRcp=c(1:6)
qbDeepLcp=c(1:6)
qbDeepMcp=c(1:6)
qbDeepRcp=c(1:6)

#All passes CP arrays 
apScreenLcp=c(1:6)
apScreenMcp=c(1:6)
apScreenRcp=c(1:6)
apShortLcp=c(1:6)
apShortMcp=c(1:6)
apShortRcp=c(1:6)
apMidLcp=c(1:6)
apMidMcp=c(1:6)
apMidRcp=c(1:6)
apDeepLcp=c(1:6)
apDeepMcp=c(1:6)
apDeepRcp=c(1:6)

year=c(2016:2011)
QBArray2=array(0,dim=c(4,3))#initialize array
dataQB=array(1,dim=c(6,12))
ReldataQB=array(1,dim=c(6,12))
APArray2=array(0,dim=c(4,3))
dataAP=array(0,dim=c(6,12))
ReldataAP=array(1,dim=c(6,12))
CdataAP=array(0,dim=c(6,12))

APArrayC2=array(0,dim=c(4,3,6))#initialize array
QBArrayC=array(0,dim=c(6,12))
APArrayC=array(0,dim=c(6,12))
AYdataAP=array(0,dim=c(6,12))
AYdataQB=array(0,dim=c(6,12))
EPAdataAP=array(0,dim=c(6,12))
EPAdataQB=array(0,dim=c(6,12))
QBTot=c(1:6)


for(i in 1:6){
	#be sure to set directory to football
	PBP=read_excel("PlayByPlay.xlsx", sheet=i) # puts the 2016 data into R
	#build area of field array
	AllPass=subset(PBP, PlayType == "Pass") #eliminate non-pass data
	QBPass=subset(AllPass, Passer == "R.Wilson") #QB's passes



	# four bins, <2, 2<7, 7<15, >15
	APScreen=subset(AllPass, AirYards <= 2)
	preAPShort=subset(AllPass, AirYards > 2)
	APShort=subset(preAPShort, AirYards <= 7)
	preAPMid=subset(AllPass, AirYards > 7)
	APMid=subset(preAPMid, AirYards <=15)
	APDeep=subset(AllPass, AirYards > 15)

	QBScreen=subset(QBPass, AirYards <= 2)
	preQBShort=subset(QBPass, AirYards > 2)
	QBShort=subset(preQBShort, AirYards <= 7)
	preQBMid=subset(QBPass, AirYards > 7)
	QBMid=subset(preQBMid, AirYards <=15)
	QBDeep=subset(QBPass, AirYards > 15)
	#direction bins
	APDeepM=subset(APDeep, PassLocation == "middle")
	APDeepR=subset(APDeep, PassLocation == "right")
	APDeepL=subset(APDeep, PassLocation == "left")
	APMidM=subset(APMid, PassLocation == "middle")
	APMidR=subset(APMid, PassLocation == "right")
	APMidL=subset(APMid, PassLocation == "left")
	APShortM=subset(APShort, PassLocation == "middle")
	APShortR=subset(APShort, PassLocation == "right")
	APShortL=subset(APShort, PassLocation == "left")
	APScreenM=subset(APScreen, PassLocation == "middle")
	APScreenR=subset(APScreen, PassLocation == "right")
	APScreenL=subset(APScreen, PassLocation == "left")
	#Russ Bins
	QBDeepM=subset(QBDeep, PassLocation == "middle")
	QBDeepR=subset(QBDeep, PassLocation == "right")
	QBDeepL=subset(QBDeep, PassLocation == "left")
	QBMidM=subset(QBMid, PassLocation == "middle")
	QBMidR=subset(QBMid, PassLocation == "right")
	QBMidL=subset(QBMid, PassLocation == "left")
	QBShortM=subset(QBShort, PassLocation == "middle")
	QBShortR=subset(QBShort, PassLocation == "right")
	QBShortL=subset(QBShort, PassLocation == "left")
	QBScreenM=subset(QBScreen, PassLocation == "middle")
	QBScreenR=subset(QBScreen, PassLocation == "right")
	QBScreenL=subset(QBScreen, PassLocation == "left")
	#Russ's array
	qbDeepL[i]=nrow(QBDeepL)
	qbDeepM[i]=nrow(QBDeepM)
	qbDeepR[i]=nrow(QBDeepR)
	qbMidL[i]=nrow(QBMidL)
	qbMidM[i]=nrow(QBMidM)
	qbMidR[i]=nrow(QBMidR)
	qbShortL[i]=nrow(QBShortL)
	qbShortM[i]=nrow(QBShortM)
	qbShortR[i]=nrow(QBShortR)
	qbScreenL[i]=nrow(QBScreenL)
	qbScreenM[i]=nrow(QBScreenM)
	qbScreenR[i]=nrow(QBScreenR)

	QBArray2[1,1]=qbDeepL[i]
	QBArray2[1,2]=qbDeepM[i]
	QBArray2[1,3]=qbDeepR[i]
	QBArray2[2,1]=qbMidL[i]
	QBArray2[2,2]=qbMidM[i]
	QBArray2[2,3]=qbMidR[i]
	QBArray2[3,1]=qbShortL[i]
	QBArray2[3,2]=qbShortM[i]
	QBArray2[3,3]=qbShortR[i]
	QBArray2[4,1]=qbScreenL[i]
	QBArray2[4,2]=qbScreenM[i]
	QBArray2[4,3]=qbScreenR[i]
	print(QBArray2)
	QBTot=sum(QBArray2)
	relQB2=QBArray2/QBTot
	dataQB[i,1]=qbDeepL[i]
	dataQB[i,2]=qbDeepM[i]
	dataQB[i,3]=qbDeepR[i]
	dataQB[i,4]=qbMidL[i]
	dataQB[i,5]=qbMidM[i]
	dataQB[i,6]=qbMidR[i]
	dataQB[i,7]=qbShortL[i]
	dataQB[i,8]=qbShortM[i]
	dataQB[i,9]=qbShortR[i]
	dataQB[i,10]=qbScreenL[i]
	dataQB[i,11]=qbScreenM[i]
	dataQB[i,12]=qbScreenR[i]
	QBTot[i]=sum(dataQB[i,])
	ReldataQB[i,]=dataQB[i,]/QBTot[i]

	#Air Yard Data
	AYdataQB[i,1]=mean(as.numeric(unlist(QBDeepL["AirYards"])), na.rm=TRUE)
	AYdataQB[i,2]=mean(as.numeric(unlist(QBDeepM["AirYards"])), na.rm=TRUE)
	AYdataQB[i,3]=mean(as.numeric(unlist(QBDeepR["AirYards"])), na.rm=TRUE)
	AYdataQB[i,4]=mean(as.numeric(unlist(QBMidL["AirYards"])), na.rm=TRUE)
	AYdataQB[i,5]=mean(as.numeric(unlist(QBMidM["AirYards"])), na.rm=TRUE)
	AYdataQB[i,6]=mean(as.numeric(unlist(QBMidR["AirYards"])), na.rm=TRUE)
	AYdataQB[i,7]=mean(as.numeric(unlist(QBShortL["AirYards"])), na.rm=TRUE)
	AYdataQB[i,8]=mean(as.numeric(unlist(QBShortM["AirYards"])), na.rm=TRUE)
	AYdataQB[i,9]=mean(as.numeric(unlist(QBShortR["AirYards"])), na.rm=TRUE)
	AYdataQB[i,10]=mean(as.numeric(unlist(QBScreenL["AirYards"])), na.rm=TRUE)
	AYdataQB[i,11]=mean(as.numeric(unlist(QBScreenM["AirYards"])), na.rm=TRUE)
	AYdataQB[i,12]=mean(as.numeric(unlist(QBScreenR["AirYards"])), na.rm=TRUE)	


	#All Player's array
	apDeepL[i]=nrow(APDeepL)
	apDeepM[i]=nrow(APDeepM)
	apDeepR[i]=nrow(APDeepR)
	apMidL[i]=nrow(APMidL)
	apMidM[i]=nrow(APMidM)
	apMidR[i]=nrow(APMidR)
	apShortL[i]=nrow(APShortL)
	apShortM[i]=nrow(APShortM)
	apShortR[i]=nrow(APShortR)
	apScreenL[i]=nrow(APScreenL)
	apScreenM[i]=nrow(APScreenM)
	apScreenR[i]=nrow(APScreenR)

	APArray2[1,1]=apDeepL[i]
	APArray2[1,2]=apDeepM[i]
	APArray2[1,3]=apDeepR[i]
	APArray2[2,1]=apMidL[i]
	APArray2[2,2]=apMidM[i]
	APArray2[2,3]=apMidR[i]
	APArray2[3,1]=apShortL[i]
	APArray2[3,2]=apShortM[i]
	APArray2[3,3]=apShortR[i]
	APArray2[4,1]=apScreenL[i]
	APArray2[4,2]=apScreenM[i]
	APArray2[4,3]=apScreenR[i]
	print(APArray2)
	APTot=sum(APArray2)
	relAP2=APArray2/APTot
	dataAP[i,1]=apDeepL[i]
	dataAP[i,2]=apDeepM[i]
	dataAP[i,3]=apDeepR[i]
	dataAP[i,4]=apMidL[i]
	dataAP[i,5]=apMidM[i]
	dataAP[i,6]=apMidR[i]
	dataAP[i,7]=apShortL[i]
	dataAP[i,8]=apShortM[i]
	dataAP[i,9]=apShortR[i]
	dataAP[i,10]=apScreenL[i]
	dataAP[i,11]=apScreenM[i]
	dataAP[i,12]=apScreenR[i]
	APTot[i]=sum(dataAP[i,])
	ReldataAP[i,]=dataAP[i,]/APTot[i]
	
	RelRel2=ReldataQB/ReldataAP
	
	# AIR YARDS
	AYdataAP[i,1]=mean(as.numeric(unlist(APDeepL["AirYards"])), na.rm=TRUE)
	AYdataAP[i,2]=mean(as.numeric(unlist(APDeepM["AirYards"])), na.rm=TRUE)
	AYdataAP[i,3]=mean(as.numeric(unlist(APDeepR["AirYards"])), na.rm=TRUE)
	AYdataAP[i,4]=mean(as.numeric(unlist(APMidL["AirYards"])), na.rm=TRUE)
	AYdataAP[i,5]=mean(as.numeric(unlist(APMidM["AirYards"])), na.rm=TRUE)
	AYdataAP[i,6]=mean(as.numeric(unlist(APMidR["AirYards"])), na.rm=TRUE)
	AYdataAP[i,7]=mean(as.numeric(unlist(APShortL["AirYards"])), na.rm=TRUE)
	AYdataAP[i,8]=mean(as.numeric(unlist(APShortM["AirYards"])), na.rm=TRUE)
	AYdataAP[i,9]=mean(as.numeric(unlist(APShortR["AirYards"])), na.rm=TRUE)
	AYdataAP[i,10]=mean(as.numeric(unlist(APScreenL["AirYards"])), na.rm=TRUE)
	AYdataAP[i,11]=mean(as.numeric(unlist(APScreenM["AirYards"])), na.rm=TRUE)
	AYdataAP[i,12]=mean(as.numeric(unlist(APScreenR["AirYards"])), na.rm=TRUE)	

	AYdataQB[i,1]=mean(as.numeric(unlist(QBDeepL["AirYards"])), na.rm=TRUE)
	AYdataQB[i,2]=mean(as.numeric(unlist(QBDeepM["AirYards"])), na.rm=TRUE)
	AYdataQB[i,3]=mean(as.numeric(unlist(QBDeepR["AirYards"])), na.rm=TRUE)
	AYdataQB[i,4]=mean(as.numeric(unlist(QBMidL["AirYards"])), na.rm=TRUE)
	AYdataQB[i,5]=mean(as.numeric(unlist(QBMidM["AirYards"])), na.rm=TRUE)
	AYdataQB[i,6]=mean(as.numeric(unlist(QBMidR["AirYards"])), na.rm=TRUE)
	AYdataQB[i,7]=mean(as.numeric(unlist(QBShortL["AirYards"])), na.rm=TRUE)
	AYdataQB[i,8]=mean(as.numeric(unlist(QBShortM["AirYards"])), na.rm=TRUE)
	AYdataQB[i,9]=mean(as.numeric(unlist(QBShortR["AirYards"])), na.rm=TRUE)
	AYdataQB[i,10]=mean(as.numeric(unlist(QBScreenL["AirYards"])), na.rm=TRUE)
	AYdataQB[i,11]=mean(as.numeric(unlist(QBScreenM["AirYards"])), na.rm=TRUE)
	AYdataQB[i,12]=mean(as.numeric(unlist(QBScreenR["AirYards"])), na.rm=TRUE)	

	#time to try to figure out completions
	APDeepMComp=subset(APDeepM, PassOutcome == "Complete") #more subsets
	APDeepLComp=subset(APDeepL, PassOutcome == "Complete")
	APDeepRComp=subset(APDeepR, PassOutcome == "Complete")
	QBDeepMComp=subset(QBDeepM, PassOutcome == "Complete")
	QBDeepLComp=subset(QBDeepL, PassOutcome == "Complete")
	QBDeepRComp=subset(QBDeepR, PassOutcome == "Complete")

	APMidMComp=subset(APMidM, PassOutcome == "Complete") #more subsets
	APMidLComp=subset(APMidL, PassOutcome == "Complete")
	APMidRComp=subset(APMidR, PassOutcome == "Complete")
	QBMidMComp=subset(QBMidM, PassOutcome == "Complete")
	QBMidLComp=subset(QBMidL, PassOutcome == "Complete")
	QBMidRComp=subset(QBMidR, PassOutcome == "Complete")

	APShortMComp=subset(APShortM, PassOutcome == "Complete") #more subsets
	APShortLComp=subset(APShortL, PassOutcome == "Complete")
	APShortRComp=subset(APShortR, PassOutcome == "Complete")
	QBShortMComp=subset(QBShortM, PassOutcome == "Complete")
	QBShortLComp=subset(QBShortL, PassOutcome == "Complete")
	QBShortRComp=subset(QBShortR, PassOutcome == "Complete")

	APScreenMComp=subset(APScreenM, PassOutcome == "Complete") #more subsets
	APScreenLComp=subset(APScreenL, PassOutcome == "Complete")
	APScreenRComp=subset(APScreenR, PassOutcome == "Complete")
	QBScreenMComp=subset(QBScreenM, PassOutcome == "Complete")
	QBScreenLComp=subset(QBScreenL, PassOutcome == "Complete")
	QBScreenRComp=subset(QBScreenR, PassOutcome == "Complete")

	#QBcompletions
	qbDeepLcp[i]=nrow(QBDeepLComp)
	qbDeepMcp[i]=nrow(QBDeepMComp)
	qbDeepRcp[i]=nrow(QBDeepRComp)
	qbMidLcp[i]=nrow(QBMidLComp)
	qbMidMcp[i]=nrow(QBMidMComp)
	qbMidRcp[i]=nrow(QBMidRComp)
	qbShortLcp[i]=nrow(QBShortLComp)
	qbShortMcp[i]=nrow(QBShortMComp)
	qbShortRcp[i]=nrow(QBShortRComp)
	qbScreenLcp[i]=nrow(QBScreenLComp)
	qbScreenMcp[i]=nrow(QBScreenMComp)
	qbScreenRcp[i]=nrow(QBScreenRComp)

	QBArrayC2=array(0,dim=c(4,3))#initialize array
	QBArrayC2[1,1]=qbDeepLcp[i]
	QBArrayC2[1,2]=qbDeepMcp[i]
	QBArrayC2[1,3]=qbDeepRcp[i]
	QBArrayC2[2,1]=qbMidLcp[i]
	QBArrayC2[2,2]=qbMidMcp[i]
	QBArrayC2[2,3]=qbMidRcp[i]
	QBArrayC2[3,1]=qbShortLcp[i]
	QBArrayC2[3,2]=qbShortMcp[i]
	QBArrayC2[3,3]=qbShortRcp[i]
	QBArrayC2[4,1]=qbScreenLcp[i]
	QBArrayC2[4,2]=qbScreenMcp[i]
	QBArrayC2[4,3]=qbScreenRcp[i]

	QBArrayCP2=QBArrayC2/QBArray2
	QBArrayC[i,1]=qbDeepLcp[i]
	QBArrayC[i,2]=qbDeepMcp[i]
	QBArrayC[i,3]=qbDeepRcp[i]
	QBArrayC[i,4]=qbMidLcp[i]
	QBArrayC[i,5]=qbMidMcp[i]
	QBArrayC[i,6]=qbMidRcp[i]
	QBArrayC[i,7]=qbShortLcp[i]
	QBArrayC[i,8]=qbShortMcp[i]
	QBArrayC[i,9]=qbShortRcp[i]
	QBArrayC[i,10]=qbScreenLcp[i]
	QBArrayC[i,11]=qbScreenMcp[i]
	QBArrayC[i,12]=qbScreenRcp[i]

	QBArrayCP=QBArrayC/dataQB


	#All Playercompletions
	apDeepLcp[i]=nrow(APDeepLComp)
	apDeepMcp[i]=nrow(APDeepMComp)
	apDeepRcp[i]=nrow(APDeepRComp)
	apMidLcp[i]=nrow(APMidLComp)
	apMidMcp[i]=nrow(APMidMComp)
	apMidRcp[i]=nrow(APMidRComp)
	apShortLcp[i]=nrow(APShortLComp)
	apShortMcp[i]=nrow(APShortMComp)
	apShortRcp[i]=nrow(APShortRComp)
	apScreenLcp[i]=nrow(APScreenLComp)
	apScreenMcp[i]=nrow(APScreenMComp)
	apScreenRcp[i]=nrow(APScreenRComp)

	
	APArrayC2[1,1,i]=apDeepLcp[i]
	APArrayC2[1,2,i]=apDeepMcp[i]
	APArrayC2[1,3,i]=apDeepRcp[i]
	APArrayC2[2,1,i]=apMidLcp[i]
	APArrayC2[2,2,i]=apMidMcp[i]
	APArrayC2[2,3,i]=apMidRcp[i]
	APArrayC2[3,1,i]=apShortLcp[i]
	APArrayC2[3,2,i]=apShortMcp[i]
	APArrayC2[3,3,i]=apShortRcp[i]
	APArrayC2[4,1,i]=apScreenLcp[i]
	APArrayC2[4,2,i]=apScreenMcp[i]
	APArrayC2[4,3,i]=apScreenRcp[i]

	APArrayC[i,1]=apDeepLcp[i]
	APArrayC[i,2]=apDeepMcp[i]
	APArrayC[i,3]=apDeepRcp[i]
	APArrayC[i,4]=apMidLcp[i]
	APArrayC[i,5]=apMidMcp[i]
	APArrayC[i,6]=apMidRcp[i]
	APArrayC[i,7]=apShortLcp[i]
	APArrayC[i,8]=apShortMcp[i]
	APArrayC[i,9]=apShortRcp[i]
	APArrayC[i,10]=apScreenLcp[i]
	APArrayC[i,11]=apScreenMcp[i]
	APArrayC[i,12]=apScreenRcp[i]
	
	# EPA
	EPAdataQB[i,1]=mean(as.numeric(unlist(QBDeepL["EPA"])), na.rm=TRUE)
	EPAdataQB[i,2]=mean(as.numeric(unlist(QBDeepM["EPA"])), na.rm=TRUE)
	EPAdataQB[i,3]=mean(as.numeric(unlist(QBDeepR["EPA"])), na.rm=TRUE)
	EPAdataQB[i,4]=mean(as.numeric(unlist(QBMidL["EPA"])), na.rm=TRUE)
	EPAdataQB[i,5]=mean(as.numeric(unlist(QBMidM["EPA"])), na.rm=TRUE)
	EPAdataQB[i,6]=mean(as.numeric(unlist(QBMidR["EPA"])), na.rm=TRUE)
	EPAdataQB[i,7]=mean(as.numeric(unlist(QBShortL["EPA"])), na.rm=TRUE)
	EPAdataQB[i,8]=mean(as.numeric(unlist(QBShortM["EPA"])), na.rm=TRUE)
	EPAdataQB[i,9]=mean(as.numeric(unlist(QBShortR["EPA"])), na.rm=TRUE)
	EPAdataQB[i,10]=mean(as.numeric(unlist(QBScreenL["EPA"])), na.rm=TRUE)
	EPAdataQB[i,11]=mean(as.numeric(unlist(QBScreenM["EPA"])), na.rm=TRUE)
	EPAdataQB[i,12]=mean(as.numeric(unlist(QBScreenR["EPA"])), na.rm=TRUE)

	EPAdataAP[i,1]=mean(as.numeric(unlist(APDeepL["EPA"])), na.rm=TRUE)
	EPAdataAP[i,2]=mean(as.numeric(unlist(APDeepM["EPA"])), na.rm=TRUE)
	EPAdataAP[i,3]=mean(as.numeric(unlist(APDeepR["EPA"])), na.rm=TRUE)
	EPAdataAP[i,4]=mean(as.numeric(unlist(APMidL["EPA"])), na.rm=TRUE)
	EPAdataAP[i,5]=mean(as.numeric(unlist(APMidM["EPA"])), na.rm=TRUE)
	EPAdataAP[i,6]=mean(as.numeric(unlist(APMidR["EPA"])), na.rm=TRUE)
	EPAdataAP[i,7]=mean(as.numeric(unlist(APShortL["EPA"])), na.rm=TRUE)
	EPAdataAP[i,8]=mean(as.numeric(unlist(APShortM["EPA"])), na.rm=TRUE)
	EPAdataAP[i,9]=mean(as.numeric(unlist(APShortR["EPA"])), na.rm=TRUE)
	EPAdataAP[i,10]=mean(as.numeric(unlist(APScreenL["EPA"])), na.rm=TRUE)
	EPAdataAP[i,11]=mean(as.numeric(unlist(APScreenM["EPA"])), na.rm=TRUE)
	EPAdataAP[i,12]=mean(as.numeric(unlist(APScreenR["EPA"])), na.rm=TRUE)	
