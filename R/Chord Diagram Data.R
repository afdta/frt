source("/home/alec/Dropbox/Projects/Brookings/DataViz/FreightFlows/R/1. Import and Process.R")
library(ggplot2)

flowList <- split(flows,flows$Group_ID)

#big function to be applied to each member of flowList
squareMatrix <- function(df){
  #Commodity code
  commCode <- df[1,"Group_ID"]
  print(paste("Running data for commodity group",commCode))
  
  #Largest Domestic
  sumDomestic <- aggregate(df["Value_2010"],df["Metro_Code"],sum)
  sumDomestic <- merge(lookupD,sumDomestic,by.x="Geo_ID",by.y="Metro_Code")
  sumDomestic <- sumDomestic[order(sumDomestic$Value_2010,decreasing=TRUE),]
  
  #Largest Foreign
  sumForeign <- aggregate(df["Value_2010"],df["Trader_Code"],sum)
  sumForeign <- merge(lookupG,sumForeign,by.x="Geo_ID",by.y="Trader_Code")
  sumForeign <- sumForeign[order(sumForeign$Value_2010,decreasing=TRUE),]
  
  numDom <- nrow(sumDomestic)
  numFor <- nrow(sumForeign)
  if(numDom > 75){numDom <- 75}
  if(numFor > 25){numFor <- 25}
  print(paste("Pulling the top",numDom,"domestic flows and the top",numFor,"foreign flows"))
  
  if(numDom > 0 && numFor > 0){
    sums <- rbind(sumDomestic[1:numDom,],sumForeign[1:numFor,]) #top 75 domestic, top 25 foreign
  } else if(numDom > 0){
    sums <- sumDomestic[1:numDom,]
  } else if(numFor > 0){
    sums <- sumForeign[1:numFor,]
  }
  
  sums <- sums[order(sums$Viz_Tick),]
  
  GC4Matrix <- as.character(sumForeign[1:25,"Geo_ID"])
  GlobalCodeStacker <- data.frame(Trader_Code=GC4Matrix,Value_2010=0) #used as a spacer below to round out square matrix
  
  makeRow <- function(e){
    e <- as.character(e) #convert to character
    #extract data for this code
    if(e %in% as.character(lookupD$Geo_ID)){
      dat <- df[as.character(df$Metro_Code)==e,c("Metro_Code","Trader_Code","Value_2010")]
      dat <- rbind(dat,data.frame(Metro_Code=e,Trader_Code=e,Value_2010=0)) #to complete square matrix
      names(dat) <- c("Metro1","Metro2","Value")
    } else if(e %in% as.character(lookupG$Geo_ID)){
      dat <- df[as.character(df$Trader_Code)==e,c("Trader_Code","Metro_Code","Value_2010")]
      GlobalCodeStacker$Metro_Code <- e #since this actually reassigns to GlobalCodeStacker, and assignments are done in local scope (<- vs <<-), it does not affect the outer value (i.e. a local copy is made)
      dat <- rbind(dat,GlobalCodeStacker)
      names(dat) <- c("Metro1","Metro2","Value")
    } else{
      print(paste("No data found for:",e))
      dat <- data.frame(Metro1=character(0),Metro2=character(0),Value=numeric(0));
    }
    #need to limit the number of connections to the chosen universe of top 75/25 (matrix must be square)
    dat <- dat[as.character(dat$Metro2) %in% as.character(sums$Geo_ID),]
    
    if(nrow(dat) < nrow(sums)){
      #if there are missing connections listed, pad the data with 0s
      nodata <- sums[!(sums$Geo_ID %in% dat$Metro2),"Geo_ID"]
      nodata <- data.frame(Metro2=nodata)
      nodata$Metro1 <- e
      nodata$Value <- 0
      dat <- rbind(dat,nodata)     
    }

    dat[dat$Value < 0.1,"Value"] <- 0 #prevents errors in D3 rendering 
    
    #reorder dat so it matches the order of Viz_Tick
    if(!is.null(dat)){
      dat$order <- factor(dat$Metro2,levels=as.character(sums$Geo_ID)) #row will be sorted in same order as sums
      dat <- dat[order(dat$order),]
      row <- matrix(dat$Value,nrow=1,dimnames=list(Metro1=e,Metro2=as.character(dat$Metro2)))
    }    
    return(row)
  } #end makeRow

  getMax <- function(e){
    e <- as.character(e) #convert to character
    #extract data for this code
    if(e %in% as.character(lookupD$Geo_ID)){
      dat <- df[as.character(df$Metro_Code)==e,"Value_2010"]
    } else if(e %in% as.character(lookupG$Geo_ID)){
      dat <- df[as.character(df$Trader_Code)==e,"Value_2010"]
    } else{
      print(paste("No data found for:",e))
      dat <- NULL
    }
    return(max(dat))
  }

  allRows <- lapply(sums$Geo_ID,makeRow) #relies on sums being sorted
  bigMatrix <- do.call(rbind,allRows)
  labels <- sums[c("fullname","CensusDiv","Geo_ID")]
  maxVals <- sapply(sums$Geo_ID,getMax)
  
  return(list(data=bigMatrix,places=labels,max=maxVals,commodity=commCode))
}

options(scipen=999)
bigBigMatrix <- lapply(flowList[1],squareMatrix)
  
library(jsonlite)
for(i in bigBigMatrix){
  writeLines(toJSON(i),paste("/home/alec/Dropbox/Projects/Brookings/DataViz/FreightFlows/json/chord_data/bigMatrix_",i$commodity,".json",sep=""))
}




##SCRATCHPAD
sum(flowsDDO[flowsDDO$FD=="Domestic","Value_2010"])*2 + sum(flowsDDO[flowsDDO$FD=="Global","Value_2010"])
sum(flows$Value_2010)

###...
sum(sumDomestic$Value_2010) + sum(sumForeign$Value_2010)
sum(flowSizeD$Outflow) + sum(flowSizeD$Inflow)
