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