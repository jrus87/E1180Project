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
repmis::LoadandCite(packages, file = './BibTex/RpackageCitations_ProdProcClean.bib')

# Removing from global environment for better readability
rm(packages, p, wrkdir)

# Read table (alternatively use dynamic link if time is not an issue)
ProdProc <- read.table("./Data/ProdProc.txt", sep=",")

###
# Data Cleaning
###

# Get rid of $ signs
ProdProc <- mutate(ProdProc, Sales=as.character(Sales))
ProdProc <- mutate(ProdProc, Sales=sapply(strsplit(ProdProc$Sales, split='$', fixed=TRUE), function(x) (x[2])))
ProdProc <- mutate(ProdProc, ExciseTax=as.character(ExciseTax))
ProdProc <- mutate(ProdProc, ExciseTax=sapply(strsplit(ProdProc$ExciseTax, split='$', fixed=TRUE), function(x) (x[2])))
head(ProdProc, n=4)

# Make Sales and Excise Tax into numerics
ProdProc$Sales <- as.numeric(gsub(",","", ProdProc$Sales))
ProdProc$ExciseTax <- as.numeric(gsub(",","", ProdProc$ExciseTax))
head(ProdProc, n=4)

###
# Data Issues
###

table(!duplicated(ProdProc$Name)) # why only 875 
table(!duplicated(ProdProc$ProdProcURLLink)) # 875 unique URLLinks

# How to get this result without having to re-run the long scraping again?
#table(!duplicated(ProdProc.table)) # 4 duplicates in the license numbers -> 16, 283, 309, 736
#which(duplicated(ProdProc.table))
#head(ProdProc.table[c(16,283,309,736),])
# [1] "/license/423396" "/license/416982" "/license/413650" "/license/415880"

# Podworks Corp same license number as TOP SHELF
#which(ProdProc.table=="/license/423396")
# CB2 Management same as The Greenery Extracts
#which(ProdProc.table=="/license/416982")
# "Missing Name" same as 'Oh' McDonald Farms
#which(ProdProc.table=="/license/413650")
# Radness Labs same as NXT LVL
#which(ProdProc.table=="/license/415880")

## in total it should thus be 879 producers

# either now match the names to the urllinks using merge from ProdProc or find the right ones and correct for all
summedup_Name <- summaryBy(Sales + ExciseTax ~ Name, data=ProdProc,
                           FUN=function(x) {c(sum=sum(x))})
head(summedup_Name, n=4) # should order first
