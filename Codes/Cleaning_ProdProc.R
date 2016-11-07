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
ProdProc <- read.table("./Data/ProdProc.txt", sep=",", stringsAsFactors = FALSE)
ProdProc.table <- read.table("./Data/ProdProctable.txt", sep=",", stringsAsFactors = FALSE)
ProdProc.table <- as.matrix(ProdProc.table)

###
# Data Cleaning
###

# Get rid of $ signs
ProdProc <- mutate(ProdProc, Sales=sapply(strsplit(ProdProc$Sales, split='$', fixed=TRUE), function(x) (x[2])))
ProdProc <- mutate(ProdProc, ExciseTax=sapply(strsplit(ProdProc$ExciseTax, split='$', fixed=TRUE), function(x) (x[2])))
head(ProdProc, n=4)

# Make Sales and Excise Tax into numerics
ProdProc$Sales <- as.numeric(gsub(",","", ProdProc$Sales))
ProdProc$ExciseTax <- as.numeric(gsub(",","", ProdProc$ExciseTax))
head(ProdProc, n=4)

# Recoding Months and Years
ProdProc$Month[ProdProc$Month=="All-2014"] <- "2014"
ProdProc$Month[ProdProc$Month=="All-2015"] <- "2015"
ProdProc$Month[ProdProc$Month=="Jan-2016"] <- "2016/01"
ProdProc$Month[ProdProc$Month=="Feb-2016"] <- "2016/02"
ProdProc$Month[ProdProc$Month=="Mar-2016"] <- "2016/03"
ProdProc$Month[ProdProc$Month=="Apr-2016"] <- "2016/04"
ProdProc$Month[ProdProc$Month=="May-2016"] <- "2016/05"
ProdProc$Month[ProdProc$Month=="Jun-2016"] <- "2016/06"
ProdProc$Month[ProdProc$Month=="Jul-2016"] <- "2016/07"
ProdProc$Month[ProdProc$Month=="Aug-2016"] <- "2016/08"
ProdProc$Month[ProdProc$Month=="Sep-2016"] <- "2016/09"
ProdProc$Month[ProdProc$Month=="Oct-2016"] <- "2016/10"

###
# Data Issues
###

# In the initial scraping, we have 922 unique URL links but here we only have 918
# Fortunately though, we have the same amount of unique names and links (so do not have same issue as with Retailer)
table(!duplicated(ProdProc$Name)) # 918 unique names
table(!duplicated(ProdProc$ProdProcURLLink)) # 918 unique URL links

# The issue is already in the stage of scraping rather than in the stage of creating the data frame
table(!duplicated(ProdProc.table)) 
which(duplicated(ProdProc.table)) # duplicates are observation 9, 285, 374 and 787
head(ProdProc.table[c(9,285,374,787),])
# These are the duplicated links "/license/423396" "/license/413650" "/license/416982" "/license/415880"

# To check which ones are duplicated, we have to cross-reference them online via: http://www.502data.com/allproducerprocessors
# Step 1: Find name of these four links in our data frame
# Step 2: "Ctrl+F" the name on the 502data website and check for the name above/below if same href
# Step 3: Check if an issue for King County

# Podworks Corp same license number as TOP SHELF
which(grepl("/license/423396", ProdProc$ProdProcURLLink))
ProdProc[80,] # Podworks Corp (Snohomish County)
# "Missing Name" same as 'Oh' McDonald Farms
which(grepl("/license/413650", ProdProc$ProdProcURLLink))
ProdProc[2694,] # Name is missing (Whatcom County)
# CB2 Management same as The Greenery Extracts
which(grepl("/license/416982", ProdProc$ProdProcURLLink))
ProdProc[3463,] # CB2 Management (King County)
# Radness Labs same as NXT LVL
which(grepl("/license/415880", ProdProc$ProdProcURLLink))
ProdProc[5975,] # Radness Labs (Grant County)

###
# We cannot fix this problem because the underlying data for the duplicates do not exist
# If we want to follow for examle NXT LVL, then we are re-directed to Radness Labs
###

###
# Create a new data set King County for later analysis with crime data and remove data sets that are no longer needed
###
KingCounty.ProdProc <- subset(ProdProc, County=="King County") # 692 out of 6477 observations (10.68 %) from King County
rm(ProdProc.table)
