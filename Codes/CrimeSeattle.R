# http://zevross.com/blog/2015/02/12/using-r-to-download-and-parse-json-an-example-using-data-from-an-open-data-portal/
# Is incident report correct? https://data.seattle.gov/Public-Safety/Seattle-Police-Department-911-Incident-Response/3k2p-39jp
# incident report still has plenty more than 35222 -> why the restriction?
# alternatively: https://data.seattle.gov/browse?category=Public+Safety


library(jsonlite)

options(max.print = 30)
getOption("max.print")

SeattleCrimeRaw<-jsonlite::fromJSON("https://data.seattle.gov/api/views/3k2p-39jp/rows.json?accessType=DOWNLOAD", simplifyVector = FALSE)

# extract the data node
SeattleCrime<-SeattleCrimeRaw[['data']] 
SeattleCrime

# Assemble the data frame: extracting all the variables (except the geography)
library(gdata) # for the trim function

grabInfo<-function(var){
  print(paste("Variable", var, sep=" "))  
  sapply(SeattleCrime, function(x) returnData(x, var)) 
}

returnData<-function(x, var){
  if(!is.null( x[[var]])){
    return( trim(x[[var]]))
  }else{
    return(NA)
  }
}

# to avoid geography only till variable 22
SCDataDF<-data.frame(sapply(1:22, grabInfo), stringsAsFactors=FALSE)

# geo data is itself a list
grabGeoInfo<-function(val){
  
  l<- length(SeattleCrime[[1]][[val]])
  tmp<-lapply(1:l, function(y) 
    
    sapply(SeattleCrime, function(x){
      
      if(!is.null(x[[val]][[y]])){
        return(x[[val]][[y]])
      }else{
        return(NA)
      }
      
    })     
  )
}


SeattleCrime[[1]][[25]]

SCDataGeo<-grabGeoInfo(23)
SCDataGeo<-data.frame(do.call("cbind", SCDataGeo), stringsAsFactors=FALSE)

SCDataDF<-cbind(SCDataDF, SCDataGeo)

# Add column names
columns<-SeattleCrimeRaw[['meta']][['view']][['columns']]

# names function
getNames<-function(x){
  if(is.null(columns[[x]]$subColumnTypes)){
    return(columns[[x]]$name)
  }else{
    return(columns[[x]]$subColumnTypes)
  }
}

SCNames<-unlist(sapply(1:23, getNames))

# add names
names(SCDataDF)<-SCNames
head(SCDataDF)

# Format the lat/long
SCDataDF$latitude<-as.numeric(SCDataDF$latitude)
SCDataDF$longitude<-as.numeric(SCDataDF$longitude)

names(SCDataDF)
SCDataDF <- SCDataDF[c(2, 12:19, 13, 24:25)]

###
# Data Cleaning
###
colnames(Seattle.Crime)[colnames(Seattle.Crime)=="latitude"] <- "lat"
colnames(Seattle.Crime)[colnames(Seattle.Crime)=="longitude"] <- "lon"

# Descriptive
table(Seattle.Crime$Event.Clearance.Description) # Narcotics, marijuana, liquor, dui and drug related
narcotics <- which(grepl("NARCOTICS", Seattle.Crime$Event.Clearance.Description))
marijuana <- which(grepl("MARIJUANA", Seattle.Crime$Event.Clearance.Description))
liquor <- which(grepl("LIQUOR", Seattle.Crime$Event.Clearance.Description))
dui <- which(grepl("DUI", Seattle.Crime$Event.Clearance.Description))
drug.related <- which(grepl("DRUG RELATED", Seattle.Crime$Event.Clearance.Description))

Seattle.Crime.narcotics <- Seattle.Crime[c(narcotics, marijuana, liquor, dui, drug.related),]
table(Seattle.Crime.narcotics$Event.Clearance.Description)

# Write Table
write.table(Seattle.Crime.Narcotics,gzfile("./Data/CrimeSeattleNarcotics.gz"))

# Remove 
rm(Seattle.Crime.narcotics, narcotics, marijuana, liquor, dui, drug.related)
rm(SCDataGeo, columns, SCNames, SeattleCrime, SeattleCrimeRaw, getNames, grabGeoInfo, grabInfo, returnData)
