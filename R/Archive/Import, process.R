setwd("/home/alec/Dropbox/Projects/Brookings/DataViz/FreightFlows/csv")
flows <- read.csv("DataViz_AllRoutes.txt",stringsAsFactors=TRUE,row.names=NULL)
lookup <- read.csv("DataViz_GeoLookup.txt",stringsAsFactors=TRUE,row.names=NULL)
lookupD <- lookup[lookup$Geo_Type=="Domestic",]
lookupG <- lookup[lookup$Geo_Type=="Global" & lookup$Geo_Description != "Unknown",]
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

#how many places involved in the X largest flows
Top15k <- flowsDDO[1:10000,]
Top15kPlaces <- unique(c(as.character(Top15k$Metro_Code),as.character(Top15k$Trader_Code)))

#Largest Domestic
sumDomestic <- aggregate(flowsDDO["Value_2010"],flowsDDO["Metro_Code"],sum)
sumDomestic <- merge(lookupD,sumDomestic,by.x="Geo_ID_Val",by.y="Metro_Code")
sumDomestic <- sumDomestic[order(sumDomestic$Value_2010,decreasing=TRUE),]

#Largest Foreign
sumForeign <- aggregate(flowsDDO["Value_2010"],flowsDDO["Trader_Code"],sum)
sumForeign <- merge(lookupG,sumForeign,by.x="Geo_ID",by.y="Trader_Code")
sumForeign <- sumForeign[order(sumForeign$Value_2010,decreasing=TRUE),]

library(ggplot2)
library(scales)
options(scipen=30)
ggplot(sumForeign) + geom_density(aes(x=Value_2010)) + scale_x_continuous(breaks=pretty_breaks(10))
ggplot(sumDomestic) + geom_point(aes(x=1:409,y=Value_2010)) + scale_y_continuous(breaks=pretty_breaks(20)) + scale_x_continuous(breaks=pretty_breaks(20))
ggplot(sumForeign) + geom_point(aes(x=1:40,y=Value_2010)) + scale_y_continuous(breaks=pretty_breaks(20)) + scale_x_continuous(breaks=pretty_breaks(20))

sum(sumForeign$Value_2010)
sum(sumDomestic$Value_2010)

#Who does Buffalo trade with
Buffalo <- flows[flows$Metro_Code==15380,]
Buffalo <- merge(Buffalo,lookup[c("Geo_Description","Geo_ID")],by.x="Trader_Code",by.y="Geo_ID")
Buffalo <- Buffalo[order(Buffalo$Value_2010,decreasing=TRUE),]
ggplot(Buffalo) + geom_point(aes(x=1:nrow(Buffalo),y=Value_2010))


#Ordered Domestic + Foreign (not quite right b/c this uses de-duped flows so A-B gets counted for A, not B)
sums <- rbind(sumDomestic[1:125,],sumForeign[1:25,]) #top 50 domestic, top 15 foreign
GC4Matrix <- as.character(sumForeign[1:25,"Geo_ID"])
#sums <- sums[order(sums$Value_2010,decreasing=TRUE),]
sums$order <- factor(as.character(sums$Geo_ID),as.character(sums$Geo_ID))
dataOrder <- sums[c("Geo_Description","order")]

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
  #need to limit the number of rows to match what is in dataOrder
  dat <- dat[as.character(dat$Metro2) %in% as.character(dataOrder$order),]
  
  #reorder dat so it matches the order of dataOrder$order
  if(!is.null(dat)){
    dat$order <- factor(dat$Metro2,levels=levels(dataOrder$order))
    dat <- dat[order(dat$order),]
    row <- matrix(dat$Value,nrow=1,dimnames=list(Metro1=e,Metro2=levels(dat$order)))
  }
  #need to reorder and create a matrix with a single row with dimnames of (e,dataOrder$order) obviously data must match
  
  return(row)
}

test <- lapply(dataOrder$order,makeRow)
bigMatrix <- do.call(rbind,test)

library(jsonlite)
json<-toJSON(bigMatrix)
writeLines(json,"bigMatrix.json")

labels <- toJSON(dataOrder$Geo_Description)
writeLines(labels,"labels.json")

#Domestic
flowsDomestic <- flowsDDO[flowsDDO$Trader_Code %in% lookupD$Geo_ID,]
row.names(flowsDomestic)<-NULL
flowsDomestic$rank <- order(flowsDomestic$Value_2010,decreasing=TRUE)
#Foreign
flowsForeign <- flowsDDO[flowsDDO$Trader_Code %in% lookupG$Geo_ID,]
row.names(flowsForeign)<-NULL
flowsForeign$rank <- order(flowsForeign$Value_2010,decreasing=TRUE)

#Use sapply to calc cumulative value
cumfn <- function(V){
  cum <- 0
  RV <- sapply(V,function(e){
    cum <<- e+cum; #make sure to assign to the outer function value
    return(cum)
  })
  return(RV)
}

#cum values
flowsDDO$cum <- cumfn(flowsDDO$Value_2010)
flowsDDO$cumshare <- flowsDDO$cum/sum(flowsDDO$Value_2010)
flowsDomestic$cum <- cumfn(flowsDomestic$Value_2010)
flowsDomestic$cumshare <- flowsDomestic$cum/sum(flowsDomestic$Value_2010)
flowsForeign$cum <- cumfn(flowsForeign$Value_2010)
flowsForeign$cumshare <- flowsForeign$cum/sum(flowsForeign$Value_2010)

sum(flowsDomestic$Value_2010)/sum(flowsDDO$Value_2010)
sum(flowsForeign$Value_2010)/sum(flowsDDO$Value_2010)


#flowsDDO[round(flowsDDO$cumshare,3)==0.50,] #num to get to 50%
#CDF
library(ggplot2)
options(scipen=999)
annos <- flowsDDO[c(seq(5000,25000,5000),50000,75000),"cum",drop=FALSE]
annos$y <- annos$cum/sum(flowsDDO$Value_2010)
annos$x <- as.numeric(row.names(annos))
annos$label <- paste(formatC(annos$y*100,digits=1,format="f"),"%",sep="")

base <- ggplot(flowsDDO)
base + geom_step(aes(x=rank,y=cumshare)) + labs(x="Rank of Trading Pair (1 == Largest Metro-to-Metro Pair by Volume)",y="Cumulative Value ($)")
base + geom_step(aes(x=rank/99796,y=cumshare)) + 
  labs(x="Rank of Trading Pair",y="Cumulative Value (%)") +
  annotate("point",x=annos$x/99796,y=annos$y) +
  annotate("text",x=annos$x/99796,y=annos$y,label=annos$label) +
  geom_step(aes(x=rank/83436,y=cumshare), data=flowsDomestic, colour="blue") +
  geom_step(aes(x=rank/16360,y=cumshare), data=flowsForeign, colour="red")

base + geom_bar(aes(x=Value_2010,y=..density..),binwidth=1) + stat_bin(aes(x=Value_2010+1,y=..density..,label=round(..density..,3)*100,size=2,angle=45,hjust=0),binwidth=1,geom="text") + scale_x_continuous(limits=c(0,100))


tst <- data.frame(v=rnorm(100000,25,2))
ggtst <- ggplot(tst,aes(x=v))
ggtst + geom_density()


library(ggmap)
geocodes1 <- geocode(as.character(lookup$Geo_Description[1:100]),output="more")
geocodes2 <- geocode(as.character(lookup$Geo_Description[101:200]),output="more")
geocodes3 <- geocode(as.character(lookup$Geo_Description[201:300]),output="more")
geocodes4 <- geocode(as.character(lookup$Geo_Description[301:361]),output="more")
geocodes5 <- geocode(gsub("Rest of |Remainder of ","",as.character(lookup$Geo_Description[362:450])),output="more")
geocodes <- rbind(geocodes1,geocodes2,geocodes3,geocodes4,geocodes5)

geocodes5 <- geocode("South America",output="more")

#quick eyeball of geocodes
ggplot(geocodes5,aes(x=lon,y=lat)) + geom_point() + geom_text(aes(x=lon+0.1,label=query,angle=45,size=2,hjust=0))

#########OLD##########
#sapply MUCH faster than this
for(i in 1:nrow(flowsDDO)){
  if(i==1){
    flowsDDO[i,"cum"] <- flowsDDO[i,"Value_2010"]
  }else{
    flowsDDO[i,"cum"] <- flowsDDO[i,"Value_2010"] + flowsDDO[(i-1),"cum"]
  }
}

#check that we have all metros
#metpops <- metropops(TRUE,vintage="2013")
#origins <- unique(flows[c("CBSA_Code","CBSA_Name")])
#missing <- merge(metpops,origins,by="CBSA_Code",all=TRUE)

#LOOK AT TOTAL VOLUMES
totforeign <- aggregate(flows[c("Exports","Imports","Volume")],by=flows[c("Trader_Name","Trader_Code")],sum)
totforeign <- totforeign[order(totforeign$Volume,decreasing=TRUE),]
totforeign$name <- factor(totforeign$Trader_Name,totforeign$Trader_Name)

totdomestic <- aggregate(flows[c("Exports","Imports","Volume")],by=flows[c("CBSA_Code","CBSA_Name")],sum)
totdomestic <- totdomestic[order(totdomestic$Volume,decreasing=TRUE),]
totdomestic$name <- factor(totdomestic$CBSA_Name,totdomestic$CBSA_Name)
totdomestic$rank <- 1:nrow(totdomestic)
cum=0
for(i in 1:nrow(totdomestic)){
  totdomestic[i,"cum"] <- cum+totdomestic[i,"Volume"]
  cum <- cum+totdomestic[i,"Volume"]
}

options(scipen=999)
library(ggplot2)
base <- ggplot(data=totforeign)
base + geom_bar(aes(y=Volume,x=name),stat="identity") + theme(axis.text.x = element_text(angle=90,hjust=1))

base2 <- ggplot(data=totdomestic)
base2 + geom_bar(aes(y=Volume,x=name),stat="identity") + theme(axis.text.x = element_text(angle=90,hjust=1))

base2 + geom_density(aes(x=rank,weight=Volume/sum(Volume))) + theme(axis.text.x = element_text(angle=90,hjust=1))

cdf <- ggplot(data=totdomestic,aes(x=rank,y=cum/sum(Volume)))
cdf + geom_step()

tgg <- ggplot(data=totdomestic,aes(x=rank,y=Volume/sum(Volume)))
tgg + geom_step()

tgg + stat_function(fun=function(d){return(d)},geom="step")

#library(grid)
#theme <- theme_bw() + theme(plot.background=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
#                            panel.border=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), 
#                            strip.background=element_rect(fill=NA,colour=NA), strip.text=element_text(family="Sawasdee",size=25), 
#                            legend.text=element_text(family="Sawasdee",size=12),legend.title=element_text(family="Sawasdee",size=12),
#                            legend.key.size=unit(2,"lines"),legend.key.width=unit(1,"lines"),legend.key.height=unit(1.5,"lines"), 
#                            legend.position=c(0.5,0), axis.title=element_blank(), plot.margin=unit(rep(0,4),"lines"))

#base <- ggplot(data=GeoMelt2, aes(x=long,y=lat,group=group)) + geom_polygon(aes(fill=ChangeBin, group=group),colour=NA, size = .3) +
#  discrete_scale("fill",scale_name="r2b",palette=pF,guide=guide_legend(title="Annual population change",title.position="top",ncol=1,direction="vertical")) + 
#  coord_fixed(xlim=c(-2200000,2200000),ylim=c(-1750000,1400000)) + facet_wrap(~ ENDYEAR, ncol=3) + theme + theme(legend.position=c(0.5,0.095),plot.margin=unit(c(1,1,1,1),"cm"))

#CairoFonts(regular="Lato:style=Regular",bold="Lato:style=Bold")