#Code that, for a place, pulls top 'num' flows and rolls up the others 
getFlow <- function(geoID,num=50){
  e <- as.character(geoID) #convert to character
  
  #extract data for this (domestic) code
  AllDat <- flows[as.character(flows$Metro_Code)==e,c("Metro_Code","Trader_Code","Value_2010","Group_ID")]
  AllDat$Group_ID <- paste("comm",AllDat$Group_ID,sep="")
  
  datList <- split(AllDat,AllDat$Group_ID)
    
  groupOperator <- function(dat){
    dat <- dat[order(dat$Value_2010,decreasing=TRUE),]
    tot <- sum(dat$Value_2010)
    N <- nrow(dat)
    if(N==0){warning("WARNING: No data for selected geography.")}
    cum <- 0
    names(dat) <- c("Metro1","Metro2","Value","GroupID")
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
    keepers <- merge(keepers,lookup2[c("Geo_ID","fullname","Viz_Tick","CensusDiv")],by.x="Metro2",by.y="Geo_ID") 
    if(row_num_check!=nrow(keepers)){stop("Bad lookup of geo names")}
    
    keepers <- keepers[order(keepers$Value,decreasing=TRUE),c("fullname","Metro2","CensusDiv","Value")]
    names(keepers) <- c("nm","id","div","val")
    
    keepers <- rbind(keepers,data.frame(nm="Aggregate of Other Flows",id="OTHER",div="OTHER",val=otherTotal))
    
    return(keepers)
  }
  
  RET <- lapply(datList,groupOperator)
  
  return(RET)
}

library(jsonlite)
for(i in 1:nrow(lookupD)){
  writeLines(toJSON(getFlow(lookupD[i,"Geo_ID"])),paste("/home/alec/Dropbox/Projects/Brookings/DataViz/FreightFlows/json/detailed_flows/",lookupD[i,"Geo_ID"],".json",sep=""))
}

index <- list(
  places = lookupD[order(as.character(lookupD$fullname)),c("Geo_ID","fullname","CensusDiv")],
  commodities = rbind(data.frame(Group_ID=0,Commodity_Group="All Commodities (Total Trade)"),commlookup)
  )
writeLines(toJSON(index),"/home/alec/Dropbox/Projects/Brookings/DataViz/FreightFlows/json/index.json")

