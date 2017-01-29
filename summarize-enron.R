## put your script in this file
cleanVec<-function(inputVec)
{
  cleanVec <- inputVec[which(inputVec != "" & inputVec !="blank")]
  cleanVec <- cleanVec[!(cleanVec %in% grep("*Undisclosed*",cleanVec))]
  cleanVec <- cleanVec[!(cleanVec %in% grep("*-*",cleanVec))]
  cleanVec
}
outputCountsCSV<-function(inputFileName, outputFileName)
{
  df<-read.csv(inputFileName, header=FALSE, stringsAsFactors=FALSE)
  names(df)<-c("time", "id", "sender", "recipients", "topic", "mode")
  recipients<-unlist(strsplit(df$recipients, '\\|'))
  senders<-df$sender
  recipientsFiltered = sort(cleanVec(recipients))
  sendersFiltered = sort(cleanVec(senders))
  recipientsRLE<-rle(recipientsFiltered)
  sendersRLE<-rle(sendersFiltered)
  commonNames<-intersect(sendersRLE$values, recipientsRLE$values)
  sendersCounts<-sendersRLE$length[sendersRLE$values %in% commonNames]
  recipientsCounts<-recipientsRLE$length[recipientsRLE$values %in% commonNames]
  outputDF<-data.frame(commonNames, sendersCounts, recipientsCounts)
  names(outputDF)<-c("person", "sent", "received")
  
  uniqueSendersNames<-sendersRLE$values[!(sendersRLE$values %in% commonNames)]
  uniqueSendersCounts<-sendersRLE$length[sendersRLE$values %in% uniqueSendersNames]
  zerosVec<-rep(0, length(uniqueSendersCounts))
  uniqueSendersDF<-data.frame(uniqueSendersNames, uniqueSendersCounts, zerosVec)
  names(uniqueSendersDF)<-c("person", "sent", "received")
  
  uniqueRecipientsNames<-recipientsRLE$values[!(recipientsRLE$values %in% commonNames)]
  uniqueRecipientsCounts<-recipientsRLE$length[recipientsRLE$values %in% uniqueRecipientsNames]
  zerosVec<-rep(0, length(uniqueRecipientsCounts))
  uniqueRecipientsDF<-data.frame(uniqueRecipientsNames, zerosVec, uniqueRecipientsCounts)
  names(uniqueRecipientsDF)<-c("person", "sent", "received")
  
  #using rbind here to avoid installing to minimize number of required packages, though in reality
  #would have used rbindlist from data.table, which is more efficient:
  
  outputDF<-rbind(outputDF, uniqueSendersDF)
  outputDF<-rbind(outputDF, uniqueRecipientsDF)
  outputDF<-outputDF[order(-outputDF$sent),]
  write.csv(outputDF, outputFileName, row.names=FALSE)
  return(list(rawDF=df, outputDF=outputDF))
}
kTopSenders<-function(outputDF, k)
{
  if (nrow(outputDF)<k || k<1){
    return (NULL)
  }
  return(as.vector(outputDF[1:k,c("person")]))
}
bucketDateRange<-function(rawDF, n)
{
  minTime<-min(rawDF$time)
  maxTime<-max(rawDF$time)
  
  stepSize<-floor((maxTime-minTime)/n)
  timeSeq<-seq(from=minTime, to=maxTime, by=stepSize)
  timeSeq<-timeSeq
  #As Enron is in Houston, and Houston time is CST
  dateSeq<-as.Date(as.POSIXct(timeSeq/1000, origin="1970-01-01", tz="America/Chicago"))
  
  return (list(timeSeq=timeSeq, dateSeq=dateSeq))
}
plotMessagesSentOverTime<-function(topK,dateBucketSize,outputDF, sendersImgFileName)
{
  print("Generating Visualization for Messages Sent Over Time")
  library(ggplot2)
  kTopNames<-kTopSenders(outputDF, topK)
  kTopSendersDF<-rawDF[rawDF$sender %in% kTopNames,c("sender","time")]
  
  bucketRange<-bucketDateRange(kTopSendersDF, dateBucketSize)
  timeSeq<-bucketRange$timeSeq
  dateSeq<-bucketRange$dateSeq
  splitSendersDF<-split(kTopSendersDF, kTopSendersDF$sender)
  
  sendersCounts<-rep(0, topK*dateBucketSize)
  sendersStartTime<-rep(0, topK*dateBucketSize)
  sendersName<-rep(0, topK*dateBucketSize)
  
  for (i in 1:length(splitSendersDF)){
    name<-unique(as.vector(splitSendersDF[[i]]$sender))
    dfTime<-as.vector(splitSendersDF[[i]]$time)
    for (j in 1:dateBucketSize){
      ind<-j+(i-1)*topK
      sendersCounts[ind]<-length(subset(dfTime,dfTime>=timeSeq[j]& dfTime<=timeSeq[j+1])) 
      sendersStartTime[ind]<-dateSeq[j]
      sendersName[ind]<-name
    }
  }
  sendersCountsByTime<-data.frame(sendersName, sendersStartTime, sendersCounts)
  
  plot<-ggplot(data=sendersCountsByTime, aes(x=as.Date(sendersStartTime, origin='1970-01-01'), y=sendersName)) +
  geom_point(aes(size=sendersCounts), shape=21, fill="white") +
  geom_text(aes(label=sendersCounts), size=4, vjust=2.5, hjust=0.5)+
  scale_size_continuous(range=c(2,15)) + 
  ggtitle('Messages Sent - Count Over 3 Month Periods for Top 10 Senders') +
    scale_x_date()+
    xlab('StartDate')
  ggsave(filename=sendersImgFileName,plot=plot, height = 6, width=8)
}
plotUniqueSenderCountOverTime<-function(topK,dateBucketSize,outputDF, rawDF, uniqueSendersImgFileName)
{
  print("Generating visualizaion")
  kTopNames<-kTopSenders(outputDF, topK)
  kTopSendersDF<-rawDF[rawDF$sender %in% kTopNames,c("sender","time")]
  bucketRange<-bucketDateRange(kTopSendersDF, dateBucketSize)
  timeSeq<-bucketRange$timeSeq
  dateSeq<-bucketRange$dateSeq
  
  uniqueSendersCounts<-rep(0, topK*dateBucketSize)
  uniqueSendersStartTime<-rep(0, topK*dateBucketSize)
  uniqueSendersName<-rep(0, topK*dateBucketSize)
 
  #number of unique senders in each time period:
  for (i in 1:length(kTopNames)){
    filter=grep(kTopNames[i], rawDF$recipients)
    df<-rawDF[filter,]
    for (j in 1:dateBucketSize){
      ind<-j+(i-1)*topK
      timeDF<-subset(df,df$time>=timeSeq[j]& df$time<=timeSeq[j+1])
      uniqueSendersCounts[ind]<-length(unique(timeDF$sender))
      uniqueSendersStartTime[ind]<-dateSeq[j]
      uniqueSendersName[ind]<-kTopNames[i]
    }
  }
  
  uniqueSendersCountsByTime<-data.frame(uniqueSendersName, uniqueSendersStartTime, uniqueSendersCounts)  
  plot<-ggplot(data=uniqueSendersCountsByTime, aes(x=as.Date(uniqueSendersStartTime, origin='1970-01-01'), y=uniqueSendersName)) +
    geom_point(aes(size=uniqueSendersCounts), shape=21, fill="white") +
    geom_text(aes(label=uniqueSendersCounts), size=4, vjust=2.5, hjust=0.5)+
    scale_size_continuous(range=c(2,15)) + 
    ggtitle('Number of Unique Senders to Top 10 Senders- Count Over 3 Month Periods') +
    scale_x_date()+
    xlab('StartDate')
  ggsave(filename=uniqueSendersImgFileName,plot=plot, height = 6, width=8)
}
args <- commandArgs()
len<-length(args)
inputFileName <-args[len]
outputFileName<-"enron-out.csv"
output<-outputCountsCSV(inputFileName, outputFileName)

rawDF<-output$rawDF
outputDF<-output$outputDF

topK <-10
dateBucketSize<-10
sendersImgFileName<-"senders.png"
uniqueSendersImgFileName<-"uniqueSenders.png"
plotMessagesSentOverTime(topK,dateBucketSize,outputDF,sendersImgFileName)
plotUniqueSenderCountOverTime(topK,dateBucketSize,outputDF, rawDF,uniqueSendersImgFileName)
  

