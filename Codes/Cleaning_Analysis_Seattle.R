# Setting our working directory
wrkdir <- c('C:/Users/Benji/Desktop/Statistics/Git/Repositories/E1180Project', 
            '~/Hertie School/Fall 2016/CollaborativeSocialScienceDataAnalysis/CSSR')
repmis::set_valid_wd(wrkdir)

# Installing packages
packages <- c('dplyr', 'repmis', 'fields', 'ggmap', 'ggplot2')
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
source("./Codes/Scraping_USCensusData.R") 


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
Seattle.Merged <- Seattle.Merged[,-c(4, 6:7, 9:11)]

# 3. Set date in R-readable format
class(Seattle.Merged$Event.Clearance.Date) # at the moment it is a factor variable
# Delete the time component, as we are only interested in dates. Also only interested in months 
Seattle.Merged$Event.Clearance.Date <- gsub('.{9}$', '',Seattle.Merged$Event.Clearance.Date)
Seattle.Merged$Event.Clearance.Date <- strptime(Seattle.Merged$Event.Clearance.Date, format= "%Y-%m-%d")
head(Seattle.Merged$Event.Clearance.Date) # now date is in POSIXlt format

rm(month_vector)

# Quick cleaning of the established.ret variable to make reading of it easier
Seattle.Merged$Established.Ret <- gsub('^.{15}', '',Seattle.Merged$Established.Ret)
table(Seattle.Merged$Established.Ret)

# For each Retailer founded before 2016, look up sequence of Address.Narc
# These are: "#HASHTAG", "PONDER", "RUCKUS", "SEATTLE CANNABIS COMPANY", "CANNABIS CITY", "AMERICAN MARY", "VELA", "OZ.", "GANJA GODDESS", "UNCLE IKE's"
# Are all consistently represented? No - see VELA
table(Seattle.Merged$Month.Ret[Seattle.Merged$Name.Ret=="#HASHTAG"]) # APR 2015
table(Seattle.Merged$Month.Ret[Seattle.Merged$Name.Ret=="PONDER"]) # AUG 2015
table(Seattle.Merged$Month.Ret[Seattle.Merged$Name.Ret=="RUCKUS"]) # DEC 2015
table(Seattle.Merged$Month.Ret[Seattle.Merged$Name.Ret=="SEATTLE CANNABIS COMPANY"]) # FEB 2015
table(Seattle.Merged$Month.Ret[Seattle.Merged$Name.Ret=="CANNABIS CITY"]) # JUL 2014
table(Seattle.Merged$Month.Ret[Seattle.Merged$Name.Ret=="AMERICAN MARY"]) # JUN 2015
table(Seattle.Merged$Month.Ret[Seattle.Merged$Name.Ret=="VELA"]) #only AUG 2016 and forth -> drop 2015 -> negligible (http://www.502data.com/license/415534)
table(Seattle.Merged$Month.Ret[Seattle.Merged$Name.Ret=="OZ."]) #  MAY 2015
table(Seattle.Merged$Month.Ret[Seattle.Merged$Name.Ret=="GANJA GODDESS"]) # NOV 2014
table(Seattle.Merged$Month.Ret[Seattle.Merged$Name.Ret=="UNCLE IKE'S"]) # SEP 2014

# Create the binary variable
Seattle.Merged$Est <- 0

# Add 1 for all of #HASHTAG
j <- which(grepl("#HASHTAG", Seattle.Merged$Name.Ret)) # 2015-04
unique(Seattle.Merged[j,1])
# 29XX BLOCK OF E MADISON ST
j <- which(grepl("29XX BLOCK OF E MADISON ST", Seattle.Merged$Address.Narc))
Seattle.Merged[j, "Month.Narc"]
which(grepl("29XX BLOCK OF E MADISON ST", Seattle.Merged$Address.Narc) &
        grepl("2015-10", Seattle.Merged$Month.Narc)) # earliest date -> observation 42642
which(j==42642) # 5th j
j <- j[-c(1:4)]
Seattle.Merged$Est[j] <- 1

# Add 1 for all of PONDER
j <- which(grepl("PONDER", Seattle.Merged$Name.Ret)) # 2015-08
unique(Seattle.Merged[j,1])
# 1 AV / PINE ST
j <- which(grepl("1 AV / PINE ST", Seattle.Merged$Address.Narc))
Seattle.Merged[j, "Month.Narc"]
which(grepl("1 AV / PINE ST", Seattle.Merged$Address.Narc) &
         grepl("2015-08", Seattle.Merged$Month.Narc)) # earliest date -> observation 1065
which(j==1065) # 124th j
j <- j[-c(1:123)]
Seattle.Merged$Est[j] <- 1

# Add 1 for all of RUCKUS
j <- which(grepl("RUCKUS", Seattle.Merged$Name.Ret)) # 2015-12
unique(Seattle.Merged[j,1])
# XX BLOCK OF THOMAS ST
j <- which(grepl("XX BLOCK OF THOMAS ST", Seattle.Merged$Address.Narc))
Seattle.Merged[j, "Month.Narc"]
which(grepl("XX BLOCK OF THOMAS ST", Seattle.Merged$Address.Narc) &
        grepl("2015-12", Seattle.Merged$Month.Narc)) # earliest date -> observation 65123
which(j==65123) # 51th j
j <- j[-c(1:50)]
Seattle.Merged$Est[j] <- 1

# Add 1 for all of SEATTLE CANNABIS COMPANY
j <- which(grepl("SEATTLE CANNABIS COMPANY", Seattle.Merged$Name.Ret)) # 2015-02
unique(Seattle.Merged[j,1])
# 16XX BLOCK OF WESTERN AVE
j <- which(grepl("16XX BLOCK OF WESTERN AVE", Seattle.Merged$Address.Narc))
Seattle.Merged[j, "Month.Narc"]
which(grepl("16XX BLOCK OF WESTERN AVE", Seattle.Merged$Address.Narc) &
        grepl("2015-12", Seattle.Merged$Month.Narc)) # earliest date -> observation 19912
which(j==19912) # 102th j
j <- j[-c(1:101)]
Seattle.Merged$Est[j] <- 1

# Add 1 for all of CANNABIS CITY
j <- which(grepl("CANNABIS CITY", Seattle.Merged$Name.Ret)) # 2014-07
unique(Seattle.Merged[j,1])
# 92XX BLOCK OF 16TH AVE SW
j <- which(grepl("92XX BLOCK OF 16TH AVE SW", Seattle.Merged$Address.Narc))
Seattle.Merged[j, "Month.Narc"]
which(grepl("92XX BLOCK OF 16TH AVE SW", Seattle.Merged$Address.Narc) &
        grepl("2014-12", Seattle.Merged$Month.Narc)) # earliest date -> observation 95160
which(j==95160) # 18th j
j <- j[-c(1:17)]
Seattle.Merged$Est[j] <- 1

# Add 1 for all of AMERICAN MARY
j <- which(grepl("AMERICAN MARY", Seattle.Merged$Name.Ret)) # 2015-06
unique(Seattle.Merged[j,1])
# 4XX BLOCK OF PINE ST
j <- which(grepl("4XX BLOCK OF PINE ST", Seattle.Merged$Address.Narc))
Seattle.Merged[j, "Month.Narc"]
which(grepl("4XX BLOCK OF PINE ST", Seattle.Merged$Address.Narc) &
        grepl("2015-06", Seattle.Merged$Month.Narc)) # earliest date -> observation 73677
which(j==73677) # 422th j
j <- j[-c(1:421)]
Seattle.Merged$Est[j] <- 1

# Add 1 for all of OZ.
j <- which(grepl("OZ.", Seattle.Merged$Name.Ret)) # 2015-05
unique(Seattle.Merged[j,1])
# 19XX BLOCK OF 2ND AVE
j <- which(grepl("19XX BLOCK OF 2ND AVE", Seattle.Merged$Address.Narc))
Seattle.Merged[j, "Month.Narc"]
which(grepl("19XX BLOCK OF 2ND AVE", Seattle.Merged$Address.Narc) &
        grepl("2015-12", Seattle.Merged$Month.Narc)) # earliest date -> observation 22397
which(j==22397) # 71th j
j <- j[-c(1:70)]
Seattle.Merged$Est[j] <- 1

# Add 1 for all of GANJA GODDESS
j <- which(grepl("GANJA GODDESS", Seattle.Merged$Name.Ret)) # 2014-11
unique(Seattle.Merged[j,1])
# 1 AV / BELL ST
j <- which(grepl("1 AV / BELL ST", Seattle.Merged$Address.Narc))
Seattle.Merged[j, "Month.Narc"]
which(grepl("1 AV / BELL ST", Seattle.Merged$Address.Narc) &
        grepl("2014-12", Seattle.Merged$Month.Narc)) # earliest date -> observation 113
which(j==113) # 71th j
j <- j[-c(1:70)]
Seattle.Merged$Est[j] <- 1

# Add 1 for all of UNCLE IKE'S
j <- which(grepl("UNCLE IKE'S", Seattle.Merged$Name.Ret)) # 2014-09
unique(Seattle.Merged[j,1])
# 21XX BLOCK OF S NORMAN ST
j <- which(grepl("21XX BLOCK OF S NORMAN ST", Seattle.Merged$Address.Narc))
Seattle.Merged[j, "Month.Narc"]
which(grepl("21XX BLOCK OF S NORMAN ST", Seattle.Merged$Address.Narc) &
        grepl("2014-11", Seattle.Merged$Month.Narc)) # earliest date -> observation 35807
which(j==35807) # 18th j
j <- j[-c(1:17)]
Seattle.Merged$Est[j] <- 1

# For each Retailer founded since 2016, simply add a 1 to the newly created Est variable
# Left are: "DIEGO PELLICER", "HERBAN LEGENDS" (2016-04), "POT SHOP" (2016-03), "POT STOP (2016-03)", "UNCLE IKES (2016-06)", "VELA" (2016-08)

# Add 1 for all of DIEGO PELLICER 
j <- which(grepl("DIEGO PELLICER", Seattle.Merged$Name.Ret)) # 2016-10
unique(Seattle.Merged[j,1])
# 3XX BLOCK OF PIKE ST
j <- which(grepl("3XX BLOCK OF PIKE ST", Seattle.Merged$Address.Narc))
Seattle.Merged[j, "Month.Narc"]
which(grepl("3XX BLOCK OF PIKE ST", Seattle.Merged$Address.Narc) &
        grepl("2016-10", Seattle.Merged$Month.Narc)) # earliest date -> observation 64348
which(j==64348) # 254h j
j <- j[-c(1:253)]
Seattle.Merged$Est[j] <- 1

# Add 1 for all of HERBAN LEGENDS 
j <- which(grepl("HERBAN LEGENDS", Seattle.Merged$Name.Ret)) # 2016-04
unique(Seattle.Merged[j,1])
# 6XX BLOCK OF 2 AV
j <- which(grepl("6XX BLOCK OF 2 AV", Seattle.Merged$Address.Narc))
Seattle.Merged[j, "Month.Narc"]
which(grepl("6XX BLOCK OF 2 AV", Seattle.Merged$Address.Narc) &
        grepl("2016-04", Seattle.Merged$Month.Narc)) # earliest date -> observation 86192
which(j==86192) # 23th j
j <- j[-c(1:22)]
Seattle.Merged$Est[j] <- 1

# Add 1 for all of POT SHOP 
j <- which(grepl("POT SHOP", Seattle.Merged$Name.Ret)) # 2016-03
unique(Seattle.Merged[j,1])
# 20XX BLOCK OF WESTERN AVE
j <- which(grepl("20XX BLOCK OF WESTERN AVE", Seattle.Merged$Address.Narc))
Seattle.Merged[j, "Month.Narc"]
which(grepl("20XX BLOCK OF WESTERN AVE", Seattle.Merged$Address.Narc) &
        grepl("2016-03", Seattle.Merged$Month.Narc)) # earliest date -> observation 34825
which(j==34825) # 38th j
j <- j[-c(1:37)]
Seattle.Merged$Est[j] <- 1

# Add 1 for all of POT STOP 
j <- which(grepl("POT STOP", Seattle.Merged$Name.Ret)) # 2016-03
unique(Seattle.Merged[j,1])
# 75XX BLOCK OF SEWARD PARK AVE S
j <- which(grepl("75XX BLOCK OF SEWARD PARK AVE S", Seattle.Merged$Address.Narc))
Seattle.Merged[j, "Month.Narc"]
which(grepl("75XX BLOCK OF SEWARD PARK AVE S", Seattle.Merged$Address.Narc) &
        grepl("2016-03", Seattle.Merged$Month.Narc)) # earliest date -> observation 89441
which(j==89441) # 2th j
j <- j[-c(1)]
Seattle.Merged$Est[j] <- 1

# Add 1 for all of UNCLE IKES
j <- which(grepl("UNCLE IKES", Seattle.Merged$Name.Ret)) # 2016-06
unique(Seattle.Merged[j,1])
# XX BLOCK OF THOMAS ST
j <- which(grepl("XX BLOCK OF THOMAS ST", Seattle.Merged$Address.Narc))
Seattle.Merged[j, "Month.Narc"]
which(grepl("XX BLOCK OF THOMAS ST", Seattle.Merged$Address.Narc) &
        grepl("2016-06", Seattle.Merged$Month.Narc)) # earliest date -> observation 65125
which(j==65125) # 53th j
j <- j[-c(1:52)]
Seattle.Merged$Est[j] <- 1

# Add 1 for all of VELA
j <- which(grepl("VELA", Seattle.Merged$Name.Ret)) # 2016-08
unique(Seattle.Merged[j,1])
# 15XX BLOCK OF 2ND AVE
j <- which(grepl("15XX BLOCK OF 2ND AVE", Seattle.Merged$Address.Narc))
Seattle.Merged[j, "Month.Narc"]
which(grepl("15XX BLOCK OF 2ND AVE", Seattle.Merged$Address.Narc) &
        grepl("2016-08", Seattle.Merged$Month.Narc)) # earliest date -> observation 14329
which(j==14329) # 330th j
j <- j[-c(1:329)]
Seattle.Merged$Est[j] <- 1

rm(j)

# How many counts
table(Seattle.Merged$Est)