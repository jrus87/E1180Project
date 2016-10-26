# Setting our working directory
wrkdir <- c('C:/Users/Benji/Desktop/Statistics/Git/Repositories/E1180Project', 
            '~/Hertie School/Fall 2016/CollaborativeSocialScienceDataAnalysis/CSSR')
repmis::set_valid_wd(wrkdir)

# Installing packages
packages <- c('rvest', 'dplyr', 'magrittr', 'RSelenium', 'repmis')
for (p in packages) {
  if (p %in% installed.packages()) require(p, character.only=TRUE) 
  else {
    install.packages(p)
    require(p, character.only=TRUE)
  }
}

# Creating a BibTex file
repmis::LoadandCite(packages, file = './BibTex/RpackageCitations_RetailerScrap.bib')

# Removing from global environment for better readability
rm(packages, p, wrkdir)

#####
# Web-Scraping
#####

# For web-scraping the JavaScript of "502data.com", I use the Phantom JS binary file in sub-folder "Codes" (note: only works for Windows).
# For more information, see: ftp://cran.r-project.org/pub/R/web/packages/RSelenium/vignettes/RSelenium-headless.html

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
RetailerURLs <- paste0("http://www.502data.com/", Retailer.table)
RetailerURLs <- unique(RetailerURLs)
RetailerURLs <- na.omit(RetailerURLs)
head(RetailerURLs, n=4)

# Use following function to get all the relevant data for each Retailer
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

# Create Data Frame
Retailer <- do.call(rbind, RetailerInfo)
head(Retailer, n=4)

# Save Data Frame
write.table(Retailer, file="./Data/Retailer.txt", sep = ",")