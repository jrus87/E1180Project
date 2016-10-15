# Setting our working directory
wrkdir <- c('C:/Users/Benji/Desktop/Statistics/Git/Repositories/guns-data/Own code', 
            '~/Hertie School/Fall 2016/CollaborativeSocialScienceDataAnalysis/CSSR')
repmis::set_valid_wd(wrkdir)

# Installing packages
packages <- c('rvest', 'xml2')
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

# Retailer 1 "Main Street Marijuana
MainStreetMarijuana <- read_html("http://www.502data.com/license/414876")

# Download Variable Description
title <- MainStreetMarijuana%>%html_nodes("thead th")%>%html_text()
Month <- MainStreetMarijuana%>%html_nodes("td:nth-child(1)")%>%html_text()
Sales <- MainStreetMarijuana%>%html_nodes("td:nth-child(2)")%>%html_text()
ExciseTax <- MainStreetMarijuana%>%html_nodes("td:nth-child(3)")%>%html_text()
County <- MainStreetMarijuana%>%html_nodes(".text-muted a")%>%html_text()
Retailer <- MainStreetMarijuana%>%html_nodes(".text-muted + h4")%>%html_text()

# Create a Data Frame
MSM <- data.frame(Month, Sales, ExciseTax, County, Retailer)
head(MSM, n=4)

# Get rid of the $ signs in Sales and Excise Tax
library(dplyr)
MSM <- mutate(MSM, Sales=as.character(Sales))
MSM <- mutate(MSM, ExciseTax=as.character(ExciseTax))
MSM <- mutate(MSM, Sales=sapply(strsplit(MSM$Sales, split='$', fixed=TRUE), function(x) (x[2])))
MSM <- mutate(MSM, ExciseTax=sapply(strsplit(MSM$ExciseTax, split='$', fixed=TRUE), function(x) (x[2])))

# Make Sales and Excise Tax into numerics
MSM$Sales <- as.numeric(gsub(",","", MSM$Sales))
MSM$ExciseTax <- as.numeric(gsub(",","", MSM$ExciseTax))

## Now deal with Time?
## Retailer since variable?


# Retailer 1 "Main Street Marijuana
UncleIkes <- read_html("http://www.502data.com/license/414533")

# Download Variable Description
title <- UncleIkes%>%html_nodes("thead th")%>%html_text()
Month <- UncleIkes%>%html_nodes("td:nth-child(1)")%>%html_text()
Sales <- UncleIkes%>%html_nodes("td:nth-child(2)")%>%html_text()
ExciseTax <- UncleIkes%>%html_nodes("td:nth-child(3)")%>%html_text()
County <- UncleIkes%>%html_nodes(".text-muted a")%>%html_text()
Retailer <- UncleIkes%>%html_nodes(".text-muted + h4")%>%html_text()

# Create a Data Frame
UI <- data.frame(Month, Sales, ExciseTax, County, Retailer)
head(UI, n=4)

# Get rid of the $ signs in Sales and Excise Tax
library(dplyr)
UI <- mutate(UI, Sales=as.character(Sales))
UI <- mutate(UI, ExciseTax=as.character(ExciseTax))
UI <- mutate(UI, Sales=sapply(strsplit(UI$Sales, split='$', fixed=TRUE), function(x) (x[2])))
UI <- mutate(UI, ExciseTax=sapply(strsplit(UI$ExciseTax, split='$', fixed=TRUE), function(x) (x[2])))

# Make Sales and Excise Tax into numerics
UI$Sales <- as.numeric(gsub(",","", UI$Sales))
UI$ExciseTax <- as.numeric(gsub(",","", UI$ExciseTax))
sum(UI$Sales)



