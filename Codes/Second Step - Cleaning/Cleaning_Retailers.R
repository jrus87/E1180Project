# Setting our working directory
wrkdir <- c('C:/Users/Benji/Desktop/Statistics/Git/Repositories/E1180Project', 
            '~/Hertie School/Fall 2016/CollaborativeSocialScienceDataAnalysis/CSSR')
repmis::set_valid_wd(wrkdir)

# Installing packages
packages <- c('dplyr', 'repmis')
for (p in packages) {
  if (p %in% installed.packages()) require(p, character.only=TRUE) 
  else {
    install.packages(p)
    require(p, character.only=TRUE)
  }
}

# Creating a BibTex file
repmis::LoadandCite(packages, file = './BibTex/RpackageCitations_Cleaning.bib')

# Removing from global environment for better readability
rm(packages, p, wrkdir)

# Read table (alternatively use dynamic link if time is not an issue)
Retailer <- read.table("./Data/Retailer.txt", sep=",", stringsAsFactors = FALSE)

###
# Data Cleaning
###

# Get rid of $ signs
Retailer <- mutate(Retailer, Sales.Ret=sapply(strsplit(Retailer$Sales.Ret, split='$', fixed=TRUE), function(x) (x[2])))
Retailer <- mutate(Retailer, ExciseTax.Ret=sapply(strsplit(Retailer$ExciseTax.Ret, split='$', fixed=TRUE), function(x) (x[2])))
head(Retailer, n=4)

# Make Sales.Ret and Excise Tax into numerics
Retailer$Sales.Ret <- as.numeric(gsub(",","", Retailer$Sales.Ret))
Retailer$ExciseTax.Ret <- as.numeric(gsub(",","", Retailer$ExciseTax.Ret))
head(Retailer, n=4)

# Recoding Months and Years
Retailer$Month.Ret[Retailer$Month.Ret=="All-2014"] <- "2014-12" # /12 indicates all months (important for analysis)
Retailer$Month.Ret[Retailer$Month.Ret=="All-2015"] <- "2015-12" # /12 indicates all months (important for analysis)
Retailer$Month.Ret[Retailer$Month.Ret=="Jan-2016"] <- "2016-01"
Retailer$Month.Ret[Retailer$Month.Ret=="Feb-2016"] <- "2016-02"
Retailer$Month.Ret[Retailer$Month.Ret=="Mar-2016"] <- "2016-03"
Retailer$Month.Ret[Retailer$Month.Ret=="Apr-2016"] <- "2016-04"
Retailer$Month.Ret[Retailer$Month.Ret=="May-2016"] <- "2016-05"
Retailer$Month.Ret[Retailer$Month.Ret=="Jun-2016"] <- "2016-06"
Retailer$Month.Ret[Retailer$Month.Ret=="Jul-2016"] <- "2016-07"
Retailer$Month.Ret[Retailer$Month.Ret=="Aug-2016"] <- "2016-08"
Retailer$Month.Ret[Retailer$Month.Ret=="Sep-2016"] <- "2016-09"
Retailer$Month.Ret[Retailer$Month.Ret=="Oct-2016"] <- "2016-10"

###
# Data Issues
###

# We have 289 unique Retailer Names but 319 unique URLs => i.e. there are multiple shops with the same name
table(!duplicated(Retailer$Name.Ret)) # 289 out of 319 unique URLs
table(!duplicated(Retailer$RetailerURLLink)) # 319 

# Create a data frame with unique names
Retailer$NameDup <- NA
Retailer$NameDup[!duplicated(Retailer$Name.Ret)] <- 1
RetailerName <- Retailer[which(Retailer$NameDup=="1"),]
RetailerName$NameDup <- NULL

# Create a data frame with unique URLs
Retailer$LinkDup <- NA
Retailer$LinkDup[!duplicated(Retailer$RetailerURLLink)] <- 1
RetailerLink <- Retailer[which(Retailer$LinkDup=="1"),]
RetailerLink$NameDup <- NULL
RetailerLink$LinkDup <- NULL

# Get rid of auxillary variables in main data set and superfluous variables in two new data sets
Retailer$NameDup <- Retailer$LinkDup <- NULL
RetailerLink$Month.Ret <- RetailerLink$Sales.Ret <- RetailerLink$ExciseTax.Ret <- RetailerLink$Established <- NULL
RetailerName$Month.Ret <- RetailerName$Sales.Ret <- RetailerName$ExciseTax.Ret <- RetailerName$Established <- NULL

# Merge two new data frames to solve the conflicts
Conflict.Retailer.df <- merge(RetailerLink, RetailerName, by="Name.Ret", all=TRUE)

# Check which names are duplicated
table(!duplicated(Conflict.Retailer.df$Name.Ret))
which(duplicated(Conflict.Retailer.df$Name.Ret))
# The following observations have duplicates in their names:
##[1]  10 18 27 36 37 61 74  78  84 101 122 136 157 158 159 166 226 230 241 270 271 276 277 280 281 282 284 285 300
##[30] 317

# If we examine one of this observation, we can get the different URL links and thus find out differences
Conflict.Retailer.df[10,] # 365 RECREATIONAL CANNABIS
which(Retailer$Name.Ret=="365 RECREATIONAL CANNABIS")
Retailer[c(1370, 2804),"RetailerURLLink"]
# URLs for 365 RECREATIONAL CANNABIS are: "http://www.502data.com/license/423784" and
# "http://www.502data.com/license/413310"

###
# Either we correct for all of these by adding a specific characteristic at the end of one duplicate (e.g. county)
# or we continue with URL links as unique identifiers. For ease, we will opt for the latter.
###

# Subset Conflict.Retailer.df to King County (Seattle) to see if we have the same issue here
Conflict.KingCounty <- subset(Conflict.Retailer.df, County.Ret.x=="King County")
table(!duplicated(Conflict.KingCounty$Name.Ret))
which(duplicated(Conflict.KingCounty$Name.Ret))
# 5 duplicates in King County: 3, 16, 32, 58, 63
Conflict.KingCounty[3,] # 365 RECREATIONAL CANNABIS -> same as above
# URLs for 365 RECREATIONAL CANNABIS are: "http://www.502data.com/license/423784" and
# "http://www.502data.com/license/413310"

###
# So even for our subset of King County, we will fare better using the URLs as unique observations
###

###
# Create a new data set King County for later analysis with crime data and remove data sets that are no longer needed
###
KingCounty.Retailer <- subset(Retailer, County.Ret=="King County") # 641 out of 2856 observations (22.44 %) from King County
rm(Conflict.KingCounty, Conflict.Retailer.df, RetailerLink, RetailerName)
