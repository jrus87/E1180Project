# Setting our working directory
wrkdir <- c('C:/Users/Benji/Desktop/Statistics/Git/Repositories/E1180Project', 
            '~/Hertie School/Fall 2016/CollaborativeSocialScienceDataAnalysis/CSSR')
repmis::set_valid_wd(wrkdir)

# Installing packages
packages <- c('rvest', 'xml2', 'dplyr', 'magrittr', 'RSelenium', 'repmis', 'psych', 'doBy', 'ggplot2')
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

# PhantomJS is a headless WebKit scriptable with a JavaScript API. 

# ftp://cran.r-project.org/pub/R/web/packages/RSelenium/vignettes/RSelenium-headless.html

# Start the pJS (which is?; can it be done without the application in the folder?)
pJS <- phantom()
remDr <- remoteDriver(browserName = "phantomjs")
remDr$open(silent = FALSE)
remDr$navigate("http://www.502data.com/retailers")

# Get a data frame of all link extensions
Retailer.html <- read_html(remDr$getPageSource()[[1]]) #html is deprecated in new version of rvest
Retailer.text<-Retailer.html%>%
  html_nodes("a.ng-binding")%>%
  html_attr("href")
Retailer.table<-matrix(Retailer.text,byrow=TRUE)
remDr$close()
pJS$stop()

#####
# Retailers
#####
RetailerURLs <- paste0("http://www.502data.com/", Retailer.table)
RetailerURLs <- unique(RetailerURLs)
RetailerURLs <- na.omit(RetailerURLs)

# to check if function is correct, subset to 3 in order to 
# waste less time
#RetailerLinks<-data.frame(matrix(RetailerURLs,ncol=1,byrow=TRUE))
#RetailerLinksSubset <- RetailerLinks[sample(1:nrow(RetailerLinks), 25, replace=FALSE),]
#RetailerLinksSubset <- as.character(RetailerLinksSubset)

# Create the data frame
RetailerInfo <- lapply(RetailerURLs, FUN=function(URLLink){
  Retailer <- read_html(URLLink)

  Name <- Retailer %>%
    html_nodes("h1") %>%
    html_text() %>%
    as.character()
    
  Month <- Retailer %>%
    html_nodes("td:nth-child(1)") %>%
    html_text() %>%
    as.character()
  
  Sales <- Retailer %>%
    html_nodes("td:nth-child(2)") %>%
    html_text() %>%
    as.character()
  
  ExciseTax <- Retailer %>%
    html_nodes("td:nth-child(3)") %>%
    html_text() %>%
    as.character()
  
  County <- Retailer %>%
    html_nodes(".text-muted a") %>%
    html_text() %>%
    as.character()
  
  Established <- Retailer %>%
    html_nodes(".text-muted + h4") %>%
    html_text() %>%
    as.character()
  
  RetailerURLLink <- URLLink
  
  Checker = data.frame(Name, Month, Sales, ExciseTax, County, Established, RetailerURLLink)
  
}) # takes around 6 minutes

Retailer <- do.call(rbind, RetailerInfo)
head(Retailer)

###
# Data Cleaning
###

# Get rid of $ signs
Retailer <- mutate(Retailer, Sales=as.character(Sales))
Retailer <- mutate(Retailer, Sales=sapply(strsplit(Retailer$Sales, split='$', fixed=TRUE), function(x) (x[2])))
Retailer <- mutate(Retailer, ExciseTax=as.character(ExciseTax))
Retailer <- mutate(Retailer, ExciseTax=sapply(strsplit(Retailer$ExciseTax, split='$', fixed=TRUE), function(x) (x[2])))
head(Retailer)

# Make Sales and Excise Tax into numerics
Retailer$Sales <- as.numeric(gsub(",","", Retailer$Sales))
Retailer$ExciseTax <- as.numeric(gsub(",","", Retailer$ExciseTax))
head(Retailer)

# Save Data Frame
write.table(Retailer, file="Retailer.txt", sep = ",")
Retailer <- read.table("Retailer.txt", sep=",")

###
# Collapse Data
###

# Descriptive Statistic per Name
describeBy(Retailer, Retailer$Name)

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

Tax2016$Month <- as.character(Tax2016$Month)
str(Tax2016$ExciseTax.sum)

graph <- ggplot(Tax2016, aes(Month, ExciseTax.sum)) +
  geom_point()
graph # order the months correct

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