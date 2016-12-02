# Setting our working directory
wrkdir <- c('C:/Users/Benji/Desktop/Statistics/Git/Repositories/E1180Project', 
            '~/Hertie School/Fall 2016/CollaborativeSocialScienceDataAnalysis/CSSR')
repmis::set_valid_wd(wrkdir)

# Installing packages
packages <- c('dplyr', 'repmis', 'fields', 'ggmap', 'ggplot2', 'doBy')
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
# Data (last updated on November 25th)
###

# Clean both Retailer and Producer/Processor Data
source("./Codes/Second Step - Cleaning/Cleaning_Retailers.R")
source("./Codes/Second Step - Cleaning/Cleaning_ProdProc.R")
# Locations of Seattle's retailers are not sourced each time, as the geo-caching 
# command using Google's API is not 100 % reliable (albeit accurate) and will create NAs from time to time
Seattle.Retailer <- read.table("./Data/Location.txt", sep=",", stringsAsFactors = FALSE) 
# Seattle.Crime is not sourced, as the data frame is too large; 
Seattle.Crime <- read.table(gzfile("./Data/CrimeSeattle.gz"),row.names=1)
source("./Codes/First Step - Scraping/Scraping_USCensusData.R") 

###
# Begin merging process
###

# Create an average of longitude and latitude to dissect into 132 blocks
Seattle.Crime$avg <- (Seattle.Crime$lon + Seattle.Crime$lat)/2

# Create a new spatial variable that dissects the data into 132 spatial points
Seattle.Crime$hundred <- cut(Seattle.Crime$avg, seq(-37.456, -37.264, 0.001275)) 

# Individual GEOIDs with centroid location
x1 <- aggregate(cbind(centroid_lon, centroid_lat) ~ GEOID, data=Seattle.SOE.Analysis, FUN=function(x) mean(range(x)))
# Individual Hundred Blocks for each Crime
x2 <- aggregate(cbind(lon, lat) ~ hundred, data = Seattle.Crime, FUN=function(x) mean(range(x)))
# Individual Addresses for each Retailer
x3 <- aggregate(cbind(lon.Ret, lat.Ret) ~ Address.Ret, data=Seattle.Retailer, FUN=function(x) mean(range(x)))

# Create a loop to get the minimum distance values for each "hundred" to the centroid of districts
# Hundred Blocks would have yielded more than 3.5 million entries of a matrix (too large)
for (loop in (1:nrow(x2))){
  a <- rdist.earth(matrix(c(x1$centroid_lon, x1$centroid_lat), ncol=2),
                   matrix(c(x2[1:loop,"lon"], x2[1:loop,"lat"]), ncol=2),
                   miles=FALSE, R=6371)
}
a <- as.data.frame(a)

# Calculate the minimum value for each column and create a new column "value" with the value of observation
for (value in (1:ncol(a))){
  a[value, "value"] <- which.min(a[,value])
}

# Clean df and rename into f for distinction purposes
b <- na.omit(a)
b <- as.data.frame(b$value)
length(unique(b$`b$value`)) # 35 districts have crime incidents

# Add the two variables of interest to our df b and drop value column
b$hundred <- x2$hundred
b$GEOID <- x1[b$`b$value`,"GEOID"]
b <- b[,-1] # no longer need the column "value"

# Merge df "b" with the Crime data set in order to incorporate GEOIDs in this data set
Seattle.Crime <- merge(Seattle.Crime, b, by="hundred")

# Next, we can merge crime incidents with socio-economic data given that we have GEOIDs in both
# 1. Get rid of lon/lat in Seattle.SOE.Analysis
Seattle.SOE.Analysis <- Seattle.SOE.Analysis[,-c(2:3)]
# 2. Merge
Seattle.Merged <- merge(Seattle.SOE.Analysis, Seattle.Crime, by="GEOID", all=TRUE)

###
# Continue merging process with Retailers
###

# Create a loop to get the minimum distance values for each "Retailer address" to the centroid of districts
for (loop in (1:nrow(x3))){
  c <- rdist.earth(matrix(c(x1$centroid_lon, x1$centroid_lat), ncol=2),
                   matrix(c(x3[1:loop,"lon.Ret"], x3[1:loop,"lat.Ret"]), ncol=2),
                   miles=FALSE, R=6371)
}
c <- as.data.frame(c)

# Calculate the minimum value for each column and create a new column "value" with the value of observation
for (value in (1:ncol(c))){
  c[value, "value"] <- which.min(c[,value])
}

# Clean df and rename into d for distinction purposes
d <- na.omit(c)
d <- as.data.frame(d$value)
length(unique(d$`d$value`)) # 8 districts host retailers

# Add the two variables of interest to our df d and drop value column
d$Address.Ret <- x3$Address.Ret
d$GEOID <- x1[d$`d$value`,"GEOID"]
d <- d[,-1] # no longer need the column "value"

# Merge df "d" with the Retailer data set in order to incorporate GEOIDs in this data set
Seattle.Retailer <- merge(Seattle.Retailer, d, by="Address.Ret")

# Rename column
colnames(Seattle.Retailer)[colnames(Seattle.Retailer)=="Month.Ret"] <- "Month"

# Merge df with SOE df
Seattle.Merged2 <- merge(Seattle.SOE.Analysis, Seattle.Retailer, by="GEOID", all=TRUE)
Seattle.Merged2 <- Seattle.Merged2[,c(1, 17:23)]

# Next, we can merge both dfs with each other
Seattle.Crime.Analysis <- merge(Seattle.Merged, Seattle.Merged2, by=c("GEOID", "Month"), all=TRUE)

# Drop all without information on Months
na_Month <- which(is.na(Seattle.Merged$Month))
Seattle.Merged <- Seattle.Merged[-na_Month,]

# Remove 
rm(a, b, c, d, x1, x2, x3, loop, value, na_Month, na_SOE, na_id, Seattle.Merged, Seattle.Merged2)

###
# Data Cleaning
###

Seattle.Crime.Analysis <- Seattle.Crime

# 1. Re-name variables to make them more humanly readable
colnames(Seattle.Crime.Analysis)[colnames(Seattle.Crime.Analysis)=="percenthighincome"] <- "share_over_$200k"
colnames(Seattle.Crime.Analysis)[colnames(Seattle.Crime.Analysis)=="Multiple"] <- "share_multiple_races"
colnames(Seattle.Crime.Analysis)[colnames(Seattle.Crime.Analysis)=="Poverty"] <- "share_poverty"

# 2. Which columns do we need
names(Seattle.Crime.Analysis)
# over_200, hundred, event.clearance.code, event.clearance.subgroup, event.clearance.group, event.clearance.date, 
# hundred.block.location, district.sector, zone.beat, avg can be removed (10, 18:20, 22:27, 31)
Seattle.Crime.Analysis <- Seattle.Crime.Analysis[,-c(10, 18, 20, 22:27, 31)]

# 3. Information from scraping the Retailer information, we know that the following shops were established in the following months
# "#HASHTAG":                 April 2015                                                                            
# "PONDER":                   August 2015
# "RUCKUS":                   December 2015
# "SEATTLE CANNABIS COMPANY": February 2015
# "CANNABIS CITY":            July 2014
# "AMERICAN MARY":            June 2015
# "VELA":                     Only August 2016 forwards, so drop 2015 information (mis-treated in the scraping)
# "OZ.":                      May 2015
# "GANJA GODDESS":            November 2014
# "UNCLE IKE'S":              September 2014
# For the remaining 5 Retailers, we already have the establishing months for 2016

#  When was first Retailer established in each GEOID
table(Seattle.Crime.Analysis[which(Seattle.Crime.Analysis$Name.Ret>=1), "GEOID"])
table(Seattle.Crime.Analysis[which(Seattle.Crime.Analysis$GEOID=="53033000402"), "Name.Ret"]) # POT STOP # 2016-03
table(Seattle.Crime.Analysis[which(Seattle.Crime.Analysis$GEOID=="53033001300"), "Name.Ret"]) # POT SHOP # 2016-03
table(Seattle.Crime.Analysis[which(Seattle.Crime.Analysis$GEOID=="53033001400"), "Name.Ret"]) # Earliest: RUCKUS 2015-12
table(Seattle.Crime.Analysis[which(Seattle.Crime.Analysis$GEOID=="53033001500"), "Name.Ret"]) # HERBAN LEGENDS # 2016-04
table(Seattle.Crime.Analysis[which(Seattle.Crime.Analysis$GEOID=="53033002900"), "Name.Ret"]) # Earliest: #HASHTAG 2015-04
table(Seattle.Crime.Analysis[which(Seattle.Crime.Analysis$GEOID=="53033008200"), "Name.Ret"]) # Earliest: UNCLE IKE'S 2014-09
table(Seattle.Crime.Analysis[which(Seattle.Crime.Analysis$GEOID=="53033008700"), "Name.Ret"]) # Earliest: CANNABIS CITY 2014-07
table(Seattle.Crime.Analysis[which(Seattle.Crime.Analysis$GEOID=="53033011800"), "Name.Ret"]) # AMERICAN MARY # 2015-06

# 4. Create a binary variable for when it has been established
Seattle.Crime.Analysis$Established <- 0

# Order by Month
Seattle.Crime.Analysis <- Seattle.Crime.Analysis[order(Seattle.Crime.Analysis$Month),]

# Code the binary for GEOID: 53033002900
j <- which(grepl("53033002900", Seattle.Crime.Analysis$GEOID))
which(grepl("53033002900", Seattle.Crime.Analysis$GEOID) &
        grepl("2015-04", Seattle.Crime.Analysis$Month)) # 2015-04 -> first observation 86256
l <- as.numeric(which(j==86256)) 
l <- l-1
j <- j[-c(1:l)]
Seattle.Crime.Analysis$Established[j] <- 1

# Code the binary for GEOID: 53033008700
j <- which(grepl("53033008700", Seattle.Crime.Analysis$GEOID))
which(grepl("53033008700", Seattle.Crime.Analysis$GEOID) &
        grepl("2014-07", Seattle.Crime.Analysis$Month)) # 2014-07 -> first observation 40573
l <- as.numeric(which(j==40573))
l <- l-1
j <- j[-c(1:l)]
Seattle.Crime.Analysis$Established[j] <- 1

# Code the binary for GEOID: 53033011800
j <- which(grepl("53033011800", Seattle.Crime.Analysis$GEOID))
which(grepl("53033011800", Seattle.Crime.Analysis$GEOID) &
        grepl("2015-06", Seattle.Crime.Analysis$Month)) # 2015-06 -> first observation 104993
l <- as.numeric(which(j==104993))
l <- l-1
j <- j[-c(1:l)]
Seattle.Crime.Analysis$Established[j] <- 1

# Code the binary for GEOID: 53033000402
j <- which(grepl("53033000402", Seattle.Crime.Analysis$GEOID))
which(grepl("53033000402", Seattle.Crime.Analysis$GEOID) &
        grepl("2016-03", Seattle.Crime.Analysis$Month)) # 2016-03 -> first observation 159747
l <- as.numeric(which(j==159747))
l <- l-1
j <- j[-c(1:l)]
Seattle.Crime.Analysis$Established[j] <- 1

# Code the binary for GEOID: 53033001300
j <- which(grepl("53033001300", Seattle.Crime.Analysis$GEOID))
which(grepl("53033001300", Seattle.Crime.Analysis$GEOID) &
        grepl("2016-03", Seattle.Crime.Analysis$Month)) # 2016-03 -> first observation 159918
l <- as.numeric(which(j==159918))
l <- l-1
j <- j[-c(1:l)]
Seattle.Crime.Analysis$Established[j] <- 1

# Code the binary for GEOID: 53033001400
j <- which(grepl("53033001400", Seattle.Crime.Analysis$GEOID))
which(grepl("53033001400", Seattle.Crime.Analysis$GEOID) &
        grepl("2015-12", Seattle.Crime.Analysis$Month)) # 2015-12 -> first observation 137993
l <- as.numeric(which(j==137993))
l <- l-1
j <- j[-c(1:l)]
Seattle.Crime.Analysis$Established[j] <- 1

# Code the binary for GEOID: 53033001500
j <- which(grepl("53033001500", Seattle.Crime.Analysis$GEOID))
which(grepl("53033001500", Seattle.Crime.Analysis$GEOID) &
        grepl("2016-04", Seattle.Crime.Analysis$Month)) # 2016-04 -> first observation 167735
l <- as.numeric(which(j==167735))
l <- l-1
j <- j[-c(1:l)]
Seattle.Crime.Analysis$Established[j] <- 1

# Code the binary for GEOID: 53033008200
j <- which(grepl("53033008200", Seattle.Crime.Analysis$GEOID))
which(grepl("53033008200", Seattle.Crime.Analysis$GEOID) &
        grepl("2014-12", Seattle.Crime.Analysis$Month)) # 2014-12 -> first observation 67765 (nothing before December 2014)
l <- as.numeric(which(j==67765))
l <- l-1
j <- j[-c(1:l)]
Seattle.Crime.Analysis$Established[j] <- 1

rm(j, l, n)

# Top 10 + Cato (violent and property)
table(Seattle.Crime.Analysis$Event.Clearance.Description)

Seattle.Crime.Analysis$Crime <- NA
Seattle.Crime.Analysis$Crime[Seattle.Crime.Analysis$Event.Clearance.Description=="ARMED ROBBERY"] <- "Violent Crime"
Seattle.Crime.Analysis$Crime[which(grepl("ASSAULTS", Seattle.Crime.Analysis$Event.Clearance.Description))] <- "Violent Crime"
Seattle.Crime.Analysis$Crime[Seattle.Crime.Analysis$Event.Clearance.Description=="DRIVE BY SHOOTING (NO INJURIES)"] <- "Violent Crime"
Seattle.Crime.Analysis$Crime[Seattle.Crime.Analysis$Event.Clearance.Description=="STRONG ARM ROBBERY"] <- "Violent Crime"
Seattle.Crime.Analysis$Crime[Seattle.Crime.Analysis$Event.Clearance.Description=="HOMICIDE"] <- "Violent Crime"
Seattle.Crime.Analysis$Crime[which(grepl("BURGLARY", Seattle.Crime.Analysis$Event.Clearance.Description))] <- "Burglary/Theft"
Seattle.Crime.Analysis$Crime[which(grepl("THEFT", Seattle.Crime.Analysis$Event.Clearance.Description))] <- "Burglary/Theft"
Seattle.Crime.Analysis$Crime[which(grepl("PROPERTY", Seattle.Crime.Analysis$Event.Clearance.Description))] <- "Property"

# plus for our drugs
Seattle.Crime.Analysis$Crime[which(grepl("NARCOTICS", Seattle.Crime.Analysis$Event.Clearance.Description))] <- "Narcotics"
Seattle.Crime.Analysis$Crime[which(grepl("MARIJUANA", Seattle.Crime.Analysis$Event.Clearance.Description))] <- "Marijuana"
Seattle.Crime.Analysis$Crime[which(grepl("LIQUOR", Seattle.Crime.Analysis$Event.Clearance.Description))] <- "Alcohol" 
Seattle.Crime.Analysis$Crime[which(grepl("DUI", Seattle.Crime.Analysis$Event.Clearance.Description))] <- "Alcohol"
Seattle.Crime.Analysis$Crime[which(grepl("DRUG RELATED", Seattle.Crime.Analysis$Event.Clearance.Description))] <- "Other Drug Related"

# Monthly counts for Crime
s1 <- summaryBy(Crime ~ GEOID + Month + Crime, data = Seattle.Crime.Analysis,
          FUN = function(x) { c(s = sum(x)) } )
colnames(s1)[colnames(s1)=="Crime.s"] <- "CrimePerMonth" # Crime per Month was erroneously coded in the old version
Seattle.Crime.Analysis$CrimePerMonth <- NULL

Seattle.Crime.Analysis <- merge(Seattle.Crime.Analysis, s1, by=c("GEOID", "Month", "Crime"))

Seattle.Crime.Analysis$populationSum <- Seattle.Crime.Analysis$CrimePerThousand <- NULL
Seattle.Crime.Analysis$CrimePerThousand <- Seattle.Crime.Analysis$CrimePerMonth / Seattle.Crime.Analysis$populationTotal

# make GEOID a factor variable
Seattle.Crime.Analysis$GEOID <- as.factor(Seattle.Crime.Analysis$GEOID)

# Create an age categorical variable
summary(Seattle.Crime.Analysis$Adult) # 72.36 % of population between 18-64 years old
summary(Seattle.Crime.Analysis$Kids)
summary(Seattle.Crime.Analysis$Retiree)

Seattle.Crime.Analysis$AgeCat <- NA
Seattle.Crime.Analysis$AgeCat[Seattle.Crime.Analysis$Adult>=72.36] <- "More than average Adults"
Seattle.Crime.Analysis$AgeCat[Seattle.Crime.Analysis$Adult<=72.36] <- "Below than average Adults"

Seattle.Crime.Analysis$AgeCat <- as.factor(Seattle.Crime.Analysis$AgeCat)

# Create an education categorical variable
summary(Seattle.Crime.Analysis$HighSchool)
summary(Seattle.Crime.Analysis$Degree)

Seattle.Crime.Analysis$EduCat <- NA
Seattle.Crime.Analysis$EduCat[Seattle.Crime.Analysis$Degree>=84.48] <- "More than average Graduates"
Seattle.Crime.Analysis$EduCat[Seattle.Crime.Analysis$Degree<=84.48] <- "Below than average Graduates"

Seattle.Crime.Analysis$EduCat <- as.factor(Seattle.Crime.Analysis$EduCat)

# Create a race categorical variable
summary(Seattle.Crime.Analysis$White)
summary(Seattle.Crime.Analysis$Black)
summary(Seattle.Crime.Analysis$share_multiple_races)

Seattle.Crime.Analysis$RaceCat <- NA
Seattle.Crime.Analysis$RaceCat[Seattle.Crime.Analysis$White>=73.18] <- "More than average Whites"
Seattle.Crime.Analysis$RaceCat[Seattle.Crime.Analysis$White<=73.18] <- "Below than average Blacks"

Seattle.Crime.Analysis$RaceCat <- as.factor(Seattle.Crime.Analysis$RaceCat)

# Write data set, so that it can be loaded more quickly
write.table(Seattle.Crime.Analysis,gzfile("./Data/SeattleCrimeAnalysis.gz"))
