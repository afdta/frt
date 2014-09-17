setwd("/home/alec/Dropbox/Projects/Brookings/DataViz/FreightFlows/csv")
flows <- read.csv("DataViz_AllRoutes.txt",stringsAsFactors=TRUE,row.names=NULL)
lookup <- read.csv("DataViz_GeoLookup.txt",stringsAsFactors=TRUE,row.names=NULL)
lookup2 <- read.csv("LookupV2.csv",stringsAsFactors=TRUE,row.names=NULL)
lookup2 <- rbind(lookup2[81:450,],lookup2[1:80,]) 
lookup2$Viz_Tick <- 1:450

lookupD <- lookup2[lookup2$Geo_Type=="Domestic",]
lookupG <- lookup2[lookup2$Geo_Type=="Global" & lookup2$Geo_Description != "Unknown",]
load("geocodes5.RData")

#NOTE: 
#Data contain duplicates when viewing total volume, e.g. Akron -> Atlanta and Atlanta -> Akron are in the data.
#However, the global geographies do not appear in the Metro_Code field so those pairs are not duplicated
#Example:
flows[with(flows,(Metro_Code==10180 & Trader_Code=="10420")|(Metro_Code==10420 & Trader_Code=="10180")),]

pairs <- paste(flows$Metro_Code,flows$Trader_Code,sep="-")
pairs2 <- sapply(pairs,function(el){
  s <- strsplit(el,"-")
  c <- sort(unlist(s))
  return(paste(c,collapse="-"))
},USE.NAMES=FALSE)
flows$pair <- pairs2

##De-Duped flows
flowsDD <- flows[!duplicated(flows$pair),]
##De-Duped and Ordered flows
flowsDDO <- flowsDD[order(flowsDD$Value_2010,decreasing=TRUE),]
row.names(flowsDDO)<-NULL
flowsDDO$rank <- order(flowsDDO$Value_2010,decreasing=TRUE)
flowsDDO$FD <- ifelse(flowsDDO$Trader_Code %in% lookupD$Geo_ID,"Domestic","Global")

TOT <- sum(flowsDDO$Value_2010)
FD <- aggregate(flowsDDO["Value_2010"],by=flowsDDO["FD"],sum)
FD[1,"Value_2010"]/TOT #73.8% domestic
FD[2,"Value_2010"]/TOT #26.2% global

#ORDERING
#Largest Domestic
sumDomestic <- aggregate(flows[c("Value_2010","Outflow","Inflow")],flows["Metro_Code"],sum)
sumDomestic <- merge(lookupD,sumDomestic,by.x="Geo_ID",by.y="Metro_Code")
sumDomestic <- sumDomestic[order(sumDomestic$Value_2010,decreasing=TRUE),]

#Largest Foreign
sumForeign <- aggregate(flows[c("Value_2010","Outflow","Inflow")],flows["Trader_Code"],sum)
sumForeign <- merge(lookupG,sumForeign,by.x="Geo_ID",by.y="Trader_Code")
sumForeign <- sumForeign[order(sumForeign$Value_2010,decreasing=TRUE),]



#Code that, for a place, pulls top 'num' flows and rolls up the others 
getFlow <- function(geoID,num=50){
  e <- as.character(geoID) #convert to character
  #extract data for this (domestic) code
  dat <- flows[as.character(flows$Metro_Code)==e,c("Metro_Code","Trader_Code","Value_2010","pair")]
  dat <- dat[order(dat$Value_2010,decreasing=TRUE),]
  tot <- sum(dat$Value_2010)
  N <- nrow(dat)
  if(N==0){warning("WARNING: No data for selected geography.")}
  cum <- 0
  names(dat) <- c("Metro1","Metro2","Value","PairName")
  whatShare <- function(e){
    cum <<- e + cum
    return(cum/tot)
  }
  shares <- sapply(dat$Value,whatShare)
  
  if(num >= N){
    warning(paste("You've selected",num, "flows.","The data contain",N,"flows."))
    keepers <- dat
    otherTotal <- 0
  } else{
    keepers <- dat[1:num,]
    others <- dat[(num+1):N,]
    otherTotal <- sum(others$Value)
    print(paste("Rolling up",nrow(others),"observations into an 'other' category, accounting for",otherTotal,"dollars, or",round(otherTotal/tot,4),"of all value."))
  }
  
  #print(which(duplicated(keepers$Metro2)))
  #keepers <- aggregate(keepers["Value"],by=keepers["Metro2"],sum) #why would you need this
  #keepers <- keepers[order(keepers$Value,decreasing=TRUE),]
  row_num_check <- nrow(keepers)
  keepers <- merge(keepers,lookup2[c("Geo_ID","Geo_Description","Viz_Tick","CensusDiv")],by.x="Metro2",by.y="Geo_ID") 
  if(row_num_check!=nrow(keepers)){stop("Bad lookup of geo names")}

  keepers <- keepers[order(keepers$Value,decreasing=TRUE),c("Geo_Description","CensusDiv","Value")]
  names(keepers) <- c("nm","div","val")
  
  keepers <- rbind(keepers,data.frame(nm="Aggregate of Other Flows",div="OTHER",val=otherTotal))
  
  return(keepers)
}
 
library(jsonlite)
for(i in 1:nrow(lookupD)){
  writeLines(toJSON(getFlow(lookupD[i,"Geo_ID"])),paste("/home/alec/Dropbox/Projects/Brookings/DataViz/FreightFlows/metflows/",lookupD[i,"Geo_ID"],".json",sep=""))
}

writeLines(toJSON(lookupD[order(as.character(lookupD$Geo_Description)),c("Geo_ID","Geo_Description","CensusDiv")]),"/home/alec/Dropbox/Projects/Brookings/DataViz/FreightFlows/metflows/index.json")



####RUN THE FULL DATASET

sums <- rbind(sumDomestic[1:75,],sumForeign[1:25,]) #top 50 domestic, top 15 foreign
#sum(sumDomestic[1:100,"Value_2010"])/sum(sums$Value_2010)

sums <- sums[order(sums$Viz_Tick),]
GC4Matrix <- as.character(sumForeign[1:25,"Geo_ID"])

#use dataOrder to create a function that creates a 449x449 matrix showing balance (not directed) relationships
GlobalCodeStacker <- data.frame(Trader_Code=GC4Matrix,Value_2010=0,pair="global-global")
makeRow <- function(e){
  e <- as.character(e) #convert to character
  #extract data for this code
  if(e %in% as.character(lookupD$Geo_ID)){
    dat <- flows[as.character(flows$Metro_Code)==e,c("Metro_Code","Trader_Code","Value_2010","pair")]
    dat <- rbind(dat,data.frame(Metro_Code=e,Trader_Code=e,Value_2010=0,pair=paste(e,e,sep="-")))
    names(dat) <- c("Metro1","Metro2","Value","PairName")
  } else if(e %in% as.character(lookupG$Geo_ID)){
    dat <- flows[as.character(flows$Trader_Code)==e,c("Trader_Code","Metro_Code","Value_2010","pair")]
    GlobalCodeStacker$Metro_Code <- e #since this actually reassigns to GlobalCodeStacker, and assignments are done in local scope (<- vs <<-), it does not affect the outer value (i.e. a local copy is made)
    dat <- rbind(dat,GlobalCodeStacker)
    names(dat) <- c("Metro1","Metro2","Value","PairName")
  } else{
    print(paste("No data found for:",e))
    dat <- NULL
  }
  #need to limit the number of rows to match what is in the 150 chosen places
  dat <- dat[as.character(dat$Metro2) %in% as.character(sums$Geo_ID),]
  
  #reorder dat so it matches the order of Viz_Tick
  if(!is.null(dat)){
    dat$order <- factor(dat$Metro2,levels=as.character(lookup2$Geo_ID)) #relies on lookup2 to be sorted
    dat <- dat[order(dat$order),]
    row <- matrix(dat$Value,nrow=1,dimnames=list(Metro1=e,Metro2=as.character(dat$order)))
  }
  #need to reorder and create a matrix with a single row with dimnames of (e,dataOrder$order) obviously data must match
  
  return(row)
}

test2 <- lapply(sums$Geo_ID,makeRow) #relies on sums being sorted
bigMatrix <- do.call(rbind,test2)
labels <- sums[c("Geo_Description","CensusDiv")]

library(jsonlite)
json<-toJSON(list(data=bigMatrix,places=labels))
writeLines(json,"/home/alec/Dropbox/Projects/Brookings/DataViz/FreightFlows/metflows/bigMatrix.json")




##SCRATCHPAD
sum(flowsDDO[flowsDDO$FD=="Domestic","Value_2010"])*2 + sum(flowsDDO[flowsDDO$FD=="Global","Value_2010"])
sum(flows$Value_2010)

###...
sum(sumDomestic$Value_2010) + sum(sumForeign$Value_2010)
sum(flowSizeD$Outflow) + sum(flowSizeD$Inflow)
