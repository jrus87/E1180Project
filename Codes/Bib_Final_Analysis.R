# Setting our working directory
wrkdir <- c('C:/Users/Benji/Desktop/Statistics/Git/Repositories/E1180Project', 
            '~/Hertie School/Fall 2016/CollaborativeSocialScienceDataAnalysis/E1180Project')
repmis::set_valid_wd(wrkdir)

# Installing packages
packages <- c('acs', 'cowplot','plyr', 'dplyr', 'fields', 'gdata', 'ggmap', 'ggplot2', 'jsonlite', 'magrittr', 'maptools',
              'RCurl', 'repmis', 'rgdal', 'RJSONIO', 'RSelenium', 'rvest', 'scales', 'sp', 'tigris',
              'stargazer')
for (p in packages) {
  if (p %in% installed.packages()) require(p, character.only=TRUE) 
  else {
    install.packages(p)
    require(p, character.only=TRUE)
  }
}

# Creating a BibTex file
repmis::LoadandCite(packages, file = 'FinalAnalysis.bib')