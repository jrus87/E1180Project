# Setting our working directory
wrkdir <- c('C:/Users/Benji/Desktop/Statistics/Git/Repositories/E1180Project', 
            '~/Hertie School/Fall 2016/CollaborativeSocialScienceDataAnalysis/CSSR')
repmis::set_valid_wd(wrkdir)

# Installing packages
packages <- c('dplyr', 'repmis', 'ggmap', 'ggplot2', 'geosphere', 'fields', 'lubridate')
for (p in packages) {
  if (p %in% installed.packages()) require(p, character.only=TRUE) 
  else {
    install.packages(p)
    require(p, character.only=TRUE)
  }
}

# Creating a BibTex file
repmis::LoadandCite(packages, file = './BibTex/RpackageCitations_Analysis.bib')

# Removing from global environment for better readability
rm(packages, p, wrkdir)

###
# Data (last updated on November 5th)
###

# Clean both Retailer and Producer/Processor Data
source("./Codes/Cleaning_Retailers.R")
source("./Codes/Cleaning_ProdProc.R")
# Locations of Seattle's retailers are not sources, as the geocaching from Google is temperemental
Seattle.Retailer <- read.table("./Data/Location.txt", sep=",", stringsAsFactors = FALSE) 
# Seattle.Crime.Narcotics is not sourced, as the data frame is too large; if big picture needed, then source it
Seattle.Crime.Narcotics <- read.table(gzfile("./Data/CrimeSeattleNarcotics.gz"),row.names=1)
source("./Codes/US Census Data.R") # still need to subset to GEOIDs of Seattle (to have only relevant data)
# US Census Data for Seattle # merged with seattle.crime (and .narcotics)

# Finding the hundred building blocks to merge the data from Seattle Retailers with
x1 <- aggregate(cbind(lon.Ret, lat.Ret) ~ Address.Ret, data=Seattle.Retailer, FUN=function(x) mean(range(x)))
x2 <- aggregate(cbind(lon, lat) ~ Hundred.Block.Location, 
                data=Seattle.Crime.Narcotics, FUN=function(x) mean(range(x)))

# create a loop where each individual value of x1 is distance measured with x2
# and print the resulting 15 observations in a new object called "min.dist"
# This can then be used for merging along this point

# Create a loop to get the minimum distance values for each Retailer to Hundred Blocks
for (loop in (1:nrow(x1))){
  a <- rdist.earth(matrix(c(x2$lon, x2$lat), ncol=2),
                   matrix(c(x1[1:loop,"lon.Ret"], x1[1:loop,"lat.Ret"]), ncol=2),
                   miles=FALSE, R=6371)
}

# Create a vector "b" for all minimal distances
a <- as.data.frame(a)
for (value in (1:ncol(a))){
  a[value, "value"] <- which.min(a[,value])
}

# Clean vector for analysis
b <- na.omit(a)
b <- as.data.frame(b$value)
b

# Which Hundred blocks are concerned?
Seattle.Crime.Narcotics[b$`b$value`,c("Hundred.Block.Location", "lat", "lon")]

# Clean for mergers
b$Address <- x1$Address
b$Address.narc <- Seattle.Crime.Narcotics[b$`b$value`,"Hundred.Block.Location"]
b <- b[,-1]
b
# Merge Addresses for narcotics with normal addresses
colnames(Seattle.Retailer)[colnames(Seattle.Retailer)=="Address.Ret"] <- "Address"
colnames(b)[colnames(b)=="Address.narc"] <- "Address.Narc"
Seattle.Retailer <- inner_join(Seattle.Retailer, b, by="Address")

# Merge Seattle.Retailer with Seattle.Crime.Narcotics
colnames(Seattle.Crime.Narcotics)[colnames(Seattle.Crime.Narcotics)=="lat"] <- "lat.Narc"
colnames(Seattle.Crime.Narcotics)[colnames(Seattle.Crime.Narcotics)=="lon"] <- "lon.Narc"
colnames(Seattle.Crime.Narcotics)[colnames(Seattle.Crime.Narcotics)=="Hundred.Block.Location"] <- "Address.Narc"
colnames(Seattle.Retailer)[colnames(Seattle.Retailer)=="Address"] <- "Address.Ret"

# Merge also by Month
Seattle.Crime.Narcotics$Month.Narc <- gsub('.{12}$', '',Seattle.Crime.Narcotics$Event.Clearance.Date)
Seattle.Retailer$Month.Narc <- Seattle.Retailer$Month.Ret
Seattle.Merged <- merge(Seattle.Crime.Narcotics, Seattle.Retailer, by=c("Address.Narc", "Month.Narc"), all=TRUE)


# Remove 
rm(a, b, x1, x2, loop, value)


###
# Data Cleaning
###

# 1. Drop Month if 2016-11
month_vector <- which(grepl("2016-11", Seattle.Merged$Month.Narc))
Seattle.Merged <- Seattle.Merged[-month_vector,]

# 2. Which columns do we need
names(Seattle.Merged)

# We do not need columns: 4, 6:7, 9:11
Seattle.Merged <- Seattle.Merged[,-c(4, 6:7, 9:11, 19)]

# 3. Set date in R-readable format
class(Seattle.Merged$Event.Clearance.Date) # at the moment it is a factor variable
# Delete the time component, as we are only interested in dates. Also only interested in months 
Seattle.Merged$Event.Clearance.Date <- gsub('.{9}$', '',Seattle.Merged$Event.Clearance.Date)
Seattle.Merged$Event.Clearance.Date <- strptime(Seattle.Merged$Event.Clearance.Date, format= "%Y-%m-%d")
head(Seattle.Merged$Event.Clearance.Date) # now date is in POSIXlt format

rm(month_vector)