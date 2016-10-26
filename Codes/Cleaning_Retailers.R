# Setting our working directory
wrkdir <- c('C:/Users/Benji/Desktop/Statistics/Git/Repositories/E1180Project', 
            '~/Hertie School/Fall 2016/CollaborativeSocialScienceDataAnalysis/CSSR')
repmis::set_valid_wd(wrkdir)

# Installing packages
packages <- c('dplyr', 'magrittr', 'repmis', 'doBy')
for (p in packages) {
  if (p %in% installed.packages()) require(p, character.only=TRUE) 
  else {
    install.packages(p)
    require(p, character.only=TRUE)
  }
}

# Creating a BibTex file
repmis::LoadandCite(packages, file = './BibTex/RpackageCitations_RetailerClean.bib')

# Removing from global environment for better readability
rm(packages, p, wrkdir)

# Read table (alternatively use dynamic link if time is not an issue)
Retailer <- read.table("./Data/Retailer.txt", sep=",")

###
# Data Cleaning
###

# Get rid of $ signs
Retailer <- mutate(Retailer, Sales=as.character(Sales))
Retailer <- mutate(Retailer, Sales=sapply(strsplit(Retailer$Sales, split='$', fixed=TRUE), function(x) (x[2])))
Retailer <- mutate(Retailer, ExciseTax=as.character(ExciseTax))
Retailer <- mutate(Retailer, ExciseTax=sapply(strsplit(Retailer$ExciseTax, split='$', fixed=TRUE), function(x) (x[2])))
head(Retailer, n=4)

# Make Sales and Excise Tax into numerics
Retailer$Sales <- as.numeric(gsub(",","", Retailer$Sales))
Retailer$ExciseTax <- as.numeric(gsub(",","", Retailer$ExciseTax))
head(Retailer, n=4)

###
# Data Issues
###

head(Retailer, n=10)
table(!duplicated(Retailer$Name)) # why only 283 (data should have 311 observations)
table(!duplicated(Retailer$RetailerURLLink)) # 311 unique URLLinks

Retailer$NameDup <- NA
Retailer$NameDup[!duplicated(Retailer$Name)] <- 1
RetailerName <- Retailer[which(Retailer$NameDup=="1"),]
RetailerName$NameDup <- NULL

Retailer$LinkDup <- NA
Retailer$LinkDup[!duplicated(Retailer$RetailerURLLink)] <- 1
RetailerLink <- Retailer[which(Retailer$LinkDup=="1"),]
RetailerLink$NameDup <- NULL
RetailerLink$LinkDup <- NULL

total <- merge(RetailerLink, RetailerName, by="Name", all=TRUE)

table(!duplicated(total$Name))
which(duplicated(total$Name))
##[1]  17  26  35  36  60  72  76  82  98 119 133 152 153 154 161
##[16] 220 224 235 263 264 269 272 273 274 276 277 292 309

# We have 28 times the same name for different license numbers
total[17,]
which(total$Name=="8 BALL BARRISTER, LLC")
which(Retailer$Name=="8 BALL BARRISTER, LLC")
Retailer[c(1056, 2520),"RetailerURLLink"]
# Thus either correct those 28 by adding their County name in title or 
# use RetailerURLLink for future reference

###
# Collapse Data
###

# Get sum for all individual Retailers
summedup_Link <- summaryBy(Sales + ExciseTax ~ RetailerURLLink, data=Retailer,
                           FUN=function(x) {c(sum=sum(x))})
# either now match the names to the urllinks using merge from Retailer or find the right ones and correct for all
summedup_Name <- summaryBy(Sales + ExciseTax ~ Name, data=Retailer,
                           FUN=function(x) {c(sum=sum(x))})

retailer2014 <- Retailer[which(Retailer$Month=="All-2014"),]
retailer2015 <- Retailer[which(Retailer$Month=="All-2015"),]
retailer2016 <- Retailer[which(Retailer$Month!="All-2015"),]


Tax2014 <- summaryBy(Sales + ExciseTax ~ County, data=retailer2014,
                     FUN=function(x) {c(sum=sum(x))})
Tax2015 <- summaryBy(Sales + ExciseTax ~ County, data=retailer2015,
                     FUN=function(x) {c(sum=sum(x))})
Tax2016 <- summaryBy(Sales + ExciseTax ~ Month, data=retailer2016,
                     FUN=function(x) {c(sum=sum(x))})
Tax2016 <- Tax2016[which(Tax2016$Month!="All-2014"),]

# 2014 financial data
sum(Tax2014$Sales.sum)/1e+6 # 29.21 m US Dollar
sum(Tax2014$ExciseTax.sum)/1e+6 # 9.74 m US Dollar
(sum(Tax2014$ExciseTax.sum)/sum(Tax2014$Sales.sum)) # 1/3

# 2015 financial data
sum(Tax2015$Sales.sum)/1e+6 # 309.87 m. US Dollar
sum(Tax2015$ExciseTax.sum)/1e+6 # 110.97 m US Dollar
(sum(Tax2015$ExciseTax.sum)/sum(Tax2015$Sales.sum)) # 35,8 %

# 2016 financial data # 2014 data still in there
sum(Tax2016$Sales.sum)/1e+6 # 509.61 m. US Dollar (9 months)
sum(Tax2016$ExciseTax.sum)/1e+6 # 197.48 m US Dollar (9 months)
(sum(Tax2016$ExciseTax.sum)/sum(Tax2016$Sales.sum)) # 36,8 %

## Graph the months of 2016 and their revenues, taxes and quotient with barplots