setwd("/home/alec/Dropbox/Projects/Brookings/DataViz/FreightFlows/csv")
library(metromonitor)

##READ IN FLOWS
flowsTot <- read.csv("DataViz_AllRoutes.txt",stringsAsFactors=TRUE,row.names=NULL)[,c("Metro_Code","Trader_Code","Value_2010")]
flowsTot$Group_ID <- 0
flowsByC <- read.csv("DataViz_AllRoutes_Commodity.txt",stringsAsFactors=TRUE,row.names=NULL)

flows <- rbind(flowsTot,flowsByC)

##GEO LOOKUP
lookup2 <- read.csv("LookupV2.csv",stringsAsFactors=TRUE,row.names=NULL)
lookup2$shortname <- nameshort(lookup2$Geo_Description, type="Met")
lookup2$Geo_Description <- ifelse(grepl("Rest of",lookup2$Geo_Description),
                                  paste(sub("Rest of ","",lookup2$Geo_Description)," (Rem.)"),
                                  as.character(lookup2$Geo_Description)
)
lookup2$Geo_Description <- ifelse(grepl("Remainder of",lookup2$Geo_Description),
                                  paste(sub("Remainder of ","",lookup2$Geo_Description)," (Rem.)"),
                                  as.character(lookup2$Geo_Description)
)
lookup2$fullname <- ifelse(lookup2$Geo_Type=="Domestic" & lookup2$Geo_Div != "Rest",
                           paste(lookup2$shortname,lookup2$US_States,sep=", "),
                           as.character(lookup2$Geo_Description))
lookup2[lookup2$fullname=="United Kingdom of Great Britain and Northern Ireland","fullname"] <- "United Kingdom"

lookup2 <- rbind(lookup2[162:450,],lookup2[1:161,]) #reorient the "dial"
lookup2$Viz_Tick <- 1:450

lookupD <- lookup2[lookup2$Geo_Type=="Domestic",]
lookupG <- lookup2[lookup2$Geo_Type=="Global" & lookup2$Geo_Description != "Unknown",]
#load("geocodes5.RData")

##COMMODITY LOOKUP
commlookup <- unique(read.csv("DataViz_CommodityLookup.txt",stringsAsFactors=TRUE,row.names=NULL)[,1:2])

