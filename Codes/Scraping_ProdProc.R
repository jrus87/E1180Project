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
repmis::LoadandCite(packages, file = './BibTex/RpackageCitations_ProdProcScrap.bib')

# Removing from global environment for better readability
rm(packages, p, wrkdir)

#####
# Web-Scraping
#####

# For web-scraping the JavaScript of "502data.com", I use the Phantom JS binary file in sub-folder "Codes" (note: only works for Windows).
# For more information, see: ftp://cran.r-project.org/pub/R/web/packages/RSelenium/vignettes/RSelenium-headless.html

# Connect to running Selenium Server phantomJS (requires the application in wd)
pJS <- phantom()
remDr <- remoteDriver(browserName = "phantomjs")
remDr$open(silent = FALSE)
remDr$navigate("http://www.502data.com/allproducerprocessors")

# Get a matrix table of all link extensions (i.e. each ProdProc has its unique link extension)
ProdProc.html <- read_html(remDr$getPageSource()[[1]]) 
ProdProc.text<-ProdProc.html%>% 
  html_nodes("a.ng-binding")%>%
  html_attr("href")
ProdProc.table<-matrix(ProdProc.text,byrow=TRUE)
remDr$close()
pJS$stop()

# add link extension on base URL
ProdProcURLs <- paste0("http://www.502data.com/", ProdProc.table)
ProdProcURLs <- unique(ProdProcURLs)
ProdProcURLs <- na.omit(ProdProcURLs)
head(ProdProcURLs, n=4)

# Use following function to get all the relevant data for each Produceer/Processor
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
  
}) # takes around 13 minutes

# Create Data Frame
ProdProc <- do.call(rbind, ProdProcInfo)
head(ProdProc, n=4)

# Save Data Frame
write.table(ProdProc, file="./Data/ProdProc.txt", sep = ",")