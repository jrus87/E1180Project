# Setting our working directory
wrkdir <- c('C:/Users/Benji/Desktop/Statistics/Git/Repositories/E1180Project', 
            '~/Hertie School/Fall 2016/CollaborativeSocialScienceDataAnalysis/CSSR')
repmis::set_valid_wd(wrkdir)

# Installing packages
packages <- c('rvest', 'xml2', 'dplyr', 'magrittr', 'RSelenium')
for (p in packages) {
  if (p %in% installed.packages()) require(p, character.only=TRUE) 
  else {
    install.packages(p)
    require(p, character.only=TRUE)
  }
}
# Creating a BibTex file
repmis::LoadandCite(packages, file = 'RpackageCitations.bib')

# Removing from global environment for better readability
rm(packages, p, wrkdir)

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

# Subset 5 to make it more readable in quest for function
#Retailer.table <- Retailer.table[sample(1:nrow(Retailer.table), 2, replace=FALSE),]
#Retailer.table
# Now use license numbers (list of 311) as reference point?

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

# plus count via plyr? = frequency count
table(unique(Retailer$RetailerURLLink))
table(Retailer$County)

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