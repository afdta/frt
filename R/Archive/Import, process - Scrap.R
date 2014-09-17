#OLD, UNUSED CODE
#Code that, for a place, pulls 90% of traffic
getFlow <- function(geoID){
  e <- as.character(geoID) #convert to character
  #extract data for this (domestic) code
  dat <- flows[as.character(flows$Metro_Code)==e,c("Metro_Code","Trader_Code","Value_2010","pair")]
  dat <- dat[order(dat$Value_2010,decreasing=TRUE),]
  tot <- sum(dat$Value_2010)
  cum <- 0
  names(dat) <- c("Metro1","Metro2","Value","PairName")
  whatShare <- function(e){
    cum <<- e + cum
    return(cum/tot)
  }
  shares <- sapply(dat$Value,whatShare)
  keepers <- dat[shares<0.85,]
  others <- dat[shares>=0.85,]
  otherTotal <- sum(others$Value)
  print(paste("Rolling up",nrow(others),"observations into an 'other' category, accounting for",otherTotal,"dollars, or",round(otherTotal/tot,4),"of all value."))
  
  keepers <- aggregate(keepers["Value"],by=keepers["Metro2"],sum)
  #keepers <- keepers[order(keepers$Value,decreasing=TRUE),]
  keepers <- rbind(data.frame(Metro2=e,Value=0),keepers)
  row_num_check <- nrow(keepers)
  keepers <- merge(keepers,lookup2[c("Geo_ID","Geo_Description","Viz_Tick","CensusDiv")],by.x="Metro2",by.y="Geo_ID") 
  if(row_num_check!=nrow(keepers)){stop("Bad lookup of geo names")}
  
  keepers <- keepers[order(keepers$Viz_Tick),]
  final_index <- which(as.character(e)==as.character(keepers$Metro2))
  if(length(final_index)!=1){stop("Error locating index of passed geography.")}
  
  row <- c(keepers$Value,otherTotal)
  nfill <- length(row) #doesn't account for 1 others observation
  filler <- matrix(rep(0,(nfill^2)),nrow=nfill)
  
  filler[final_index,] <- row
  filler[,final_index] <- row
  
  #places <- lookup[match(keepers$Metro2,lookup$Geo_ID),c("Geo_ID","Geo_Description")]
  #print(sum(as.character(places$Geo_ID)==as.character(keepers$Metro2))==nrow(keepers))
  
  places <- keepers[c("Geo_Description","CensusDiv")]
  places <- rbind(places,data.frame(Geo_Description="Aggregate of Other Flows",CensusDiv="OTHER"))
  
  #return(list(dat=filler_2,ordering=places))
  return(list(data=filler,places=places))
}