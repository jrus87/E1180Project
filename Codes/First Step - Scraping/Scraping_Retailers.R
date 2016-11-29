# Setting our working directory
wrkdir <- c('C:/Users/Benji/Desktop/Statistics/Git/Repositories/E1180Project', 
            '~/Hertie School/Fall 2016/CollaborativeSocialScienceDataAnalysis/CSSR')
repmis::set_valid_wd(wrkdir)

# Installing packages
packages <- c('rvest', 'magrittr', 'RSelenium', 'repmis')
for (p in packages) {
  if (p %in% installed.packages()) require(p, character.only=TRUE) 
  else {
    install.packages(p)
    require(p, character.only=TRUE)
  }
}

# Creating a BibTex file
repmis::LoadandCite(packages, file = './BibTex/RpackageCitations_Scraping.bib')

# Removing from global environment for better readability
rm(packages, p, wrkdir)

#####
# Web-Scraping
#####

# For web-scraping the JavaScript of "502data.com", we use the Phantom JS binary file (note: only works for Windows).
# For more information, see: ftp://cran.r-project.org/pub/R/web/packages/RSelenium/vignettes/RSelenium-headless.html
# As this only works for Windows at the moment, we will provide the scraped data in an unadultered form as .txt-file

# Start the pJS 
pJS <- phantom()
remDr <- remoteDriver(browserName = "phantomjs")
remDr$open(silent = FALSE)
remDr$navigate("http://www.502data.com/retailers")

# Get a matrix table of all link extensions (i.e. each retailer has its unique link extension)
Retailer.html <- read_html(remDr$getPageSource()[[1]]) 
Retailer.text<-Retailer.html%>%
  html_nodes("a.ng-binding")%>%
  html_attr("href")
Retailer.table<-matrix(Retailer.text,byrow=TRUE)
remDr$close()
pJS$stop()

# add link extension on base URL
RetailerURLs <- paste0("http://www.502data.com", Retailer.table)
RetailerURLs <- unique(RetailerURLs)
RetailerURLs <- na.omit(RetailerURLs)
head(RetailerURLs, n=4)

# Start timer
ptm <- proc.time()
# Use following function to get all the relevant data for each Retailer
RetailerInfo <- lapply(RetailerURLs, FUN=function(URLLink){
  Retailer <- read_html(URLLink)

  Name.Ret <- Retailer %>%
    html_nodes("h1") %>%
    html_text() %>%
    as.character()
    
  Month.Ret <- Retailer %>%
    html_nodes("td:nth-child(1)") %>%
    html_text() %>%
    as.character()
  
  Sales.Ret <- Retailer %>%
    html_nodes("td:nth-child(2)") %>%
    html_text() %>%
    as.character()
  
  ExciseTax.Ret <- Retailer %>%
    html_nodes("td:nth-child(3)") %>%
    html_text() %>%
    as.character()
  
  County.Ret <- Retailer %>%
    html_nodes(".text-muted a") %>%
    html_text() %>%
    as.character()
  
  Established.Ret <- Retailer %>%
    html_nodes(".text-muted + h4") %>%
    html_text() %>%
    as.character()
  
  RetailerURLLink <- URLLink
  
  Checker = data.frame(Name.Ret, Month.Ret, Sales.Ret, ExciseTax.Ret, County.Ret, Established.Ret, RetailerURLLink)
  
}) 
# Stop timer
proc.time() - ptm 
# User:     94.35
# System:   154.57
# Elapsed:  252.37 -> The code takes around 4 minutes and 12 seconds to run

# Create Data Frame
Retailer <- do.call(rbind, RetailerInfo)
head(Retailer, n=4)

# Save Data Frame
write.table(Retailer, file="./Data/Retailer.txt", sep = ",")