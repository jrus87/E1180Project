# Setting our working directory
wrkdir <- c('C:/Users/Benji/Desktop/Statistics/Git/Repositories/E1180Project', 
            '~/Hertie School/Fall 2016/CollaborativeSocialScienceDataAnalysis/CSSR')
repmis::set_valid_wd(wrkdir)

# Installing packages
packages <- c('rvest', 'xml2', 'dplyr', 'magrittr', 'RSelenium', 'repmis', 'psych', 'doBy')
for (p in packages) {
  if (p %in% installed.packages()) require(p, character.only=TRUE) 
  else {
    install.packages(p)
    require(p, character.only=TRUE)
  }
}
# Loads packages rvest, xml2, dplyr, margrittr, {RSelenium, RCurl, bitops, XML}, repmis

# Creating a BibTex file
repmis::LoadandCite(packages, file = 'RpackageCitations.bib')

# Removing from global environment for better readability
rm(packages, p, wrkdir)

# Connect to running Selenium Server phantomJS (requires the application in wd)
pJS <- phantom()
remDr <- remoteDriver(browserName = "phantomjs")
remDr$open(silent = FALSE)
remDr$navigate("http://www.502data.com/allproducerprocessors")

## Get a data frame of all link extensions
ProdProc.html <- read_html(remDr$getPageSource()[[1]]) 
ProdProc.text<-ProdProc.html%>% 
  html_nodes("a.ng-binding")%>%
  html_attr("href")

# Create a table out of ProdProc.text (at the moment it's a character)
ProdProc.table<-matrix(ProdProc.text,byrow=TRUE)
remDr$close()
pJS$stop()

#####
# ProdProcs
#####
ProdProcURLs <- paste0("http://www.502data.com/", ProdProc.table)
ProdProcURLs <- unique(ProdProcURLs)
ProdProcURLs <- na.omit(ProdProcURLs)
ProdProcURLs

# Create the data frame
ProdProcInfo <- lapply(ProdProcURLs, FUN=function(URLLink){
  ProdProc <- read_html(URLLink)
  
  Name <- ProdProc %>%
    html_nodes("h1") %>%
    html_text() %>%
    as.character()
  
  Month <- ProdProc %>%
    html_nodes("td:nth-child(1)") %>%
    html_text() %>%
    as.character()
  
  Sales <- ProdProc %>%
    html_nodes("td:nth-child(2)") %>%
    html_text() %>%
    as.character()
  
  ExciseTax <- ProdProc %>%
    html_nodes("td:nth-child(3)") %>%
    html_text() %>%
    as.character()
  
  County <- ProdProc %>%
    html_nodes(".text-muted a") %>%
    html_text() %>%
    as.character()
  
  Established <- ProdProc %>%
    html_nodes(".text-muted + h4") %>%
    html_text() %>%
    as.character()
  
  ProdProcURLLink <- URLLink
  
  Checker = data.frame(Name, Month, Sales, ExciseTax, County, Established, ProdProcURLLink)
  
}) # takes around 18 minutes

ProdProc <- do.call(rbind, ProdProcInfo)
head(ProdProc)

# plus count via plyr? = frequency count
table(ProdProc$County)

# Get rid of $ signs
ProdProc <- mutate(ProdProc, Sales=as.character(Sales))
ProdProc <- mutate(ProdProc, Sales=sapply(strsplit(ProdProc$Sales, split='$', fixed=TRUE), function(x) (x[2])))
ProdProc <- mutate(ProdProc, ExciseTax=as.character(ExciseTax))
ProdProc <- mutate(ProdProc, ExciseTax=sapply(strsplit(ProdProc$ExciseTax, split='$', fixed=TRUE), function(x) (x[2])))
head(ProdProc)

# Make Sales and Excise Tax into numerics
ProdProc$Sales <- as.numeric(gsub(",","", ProdProc$Sales))
ProdProc$ExciseTax <- as.numeric(gsub(",","", ProdProc$ExciseTax))
head(ProdProc)


## do By to collapse into smaller
## write table to have data frame in future
#write.table(ProdProc, file="ProdProc.txt", sep = ",")
#ProdProc <- read.table("ProdProc.txt", sep=",")

table(!duplicated(ProdProc$Name)) # why only 875 
table(!duplicated(ProdProc$ProdProcURLLink)) # 875 unique URLLinks
table(!duplicated(ProdProc.table)) # 4 duplicates in the license numbers -> 16, 283, 309, 736
which(duplicated(ProdProc.table))
head(ProdProc.table[c(16,283,309,736),])
# [1] "/license/423396" "/license/416982" "/license/413650" "/license/415880"

# Podworks Corp same license number as TOP SHELF
which(ProdProc.table=="/license/423396")
# CB2 Management same as The Greenery Extracts
which(ProdProc.table=="/license/416982")
# "Missing Name" same as 'Oh' McDonald Farms
which(ProdProc.table=="/license/413650")
# Radness Labs same as NXT LVL
which(ProdProc.table=="/license/415880")

## in total it should thus be 879 producers

##
describeBy(ProdProc, ProdProc$Name)

# either now match the names to the urllinks using merge from ProdProc or find the right ones and correct for all
summedup_Name <- summaryBy(Sales + ExciseTax ~ Name, data=ProdProc,
                           FUN=function(x) {c(sum=sum(x))})
head(summedup_Name, n=10) # should order first