# Setting our working directory
wrkdir <- c('C:/Users/Benji/Desktop/Statistics/Git/Repositories/E1180Project', 
            '~/Hertie School/Fall 2016/CollaborativeSocialScienceDataAnalysis/CSSR')
repmis::set_valid_wd(wrkdir)

# Installing packages
packages <- c('dplyr', 'repmis', 'acs', 'scales', 'sp', 'ggplot2', 'rgdal', 'maptools', 'tigris')
for (p in packages) {
  if (p %in% installed.packages()) require(p, character.only=TRUE) 
  else {
    install.packages(p)
    require(p, character.only=TRUE)
  }
}

# Creating a BibTex file
repmis::LoadandCite(packages, file = './BibTex/RpackageCitations_SOE.bib')

# Removing from global environment for better readability
rm(packages, p, wrkdir)

###
# Gettiing the keywords
###

acs.lookup(keyword = "Population", endyear = 2014)
# Population of one race: White "C02003_003"
# Population of one race: Black "C02003_004"
acs.lookup(keyword = "Median household income", endyear = 2014)
# Median Household Income in the Past 12 Months: "B19013"
# Household Income: Total: "B19001_001"

# Find the Table for the Data
# Using fips number as acs.fetch function does not work with county name

# grab the spatial data (tigris) # 53033 = fips code?
counties <- c(33)
tracts <- tracts(state = 'WA', county = c(33), cb=TRUE)

names(tracts)

# install api key
api.key.install(key="c44d5d23fb897a3d2e9e9f8daea97d67cff5e2f5")
geo<-geo.make(state=c("WA"),
              county=c(33), tract="*")

income<-acs.fetch(endyear = 2013, span = 5, geography = geo,
                  table.number = "B19001", col.names = "pretty")

# use of col.names = "pretty" above gives the full column definitions
# if you want Census variable IDs use col.names="auto". Here are the
# variables we want with pretty and auto results.
#"Household Income: Total:" ("B19001_001")
#"Household Income: $200,000 or more" ("B19001_017")

names(attributes(income))
attr(income, "acs.colnames")

# convert to a data.frame for merging
income_df <- data.frame(paste0(str_pad(income@geography$state, 2, "left", pad="0"), 
                               str_pad(income@geography$county, 3, "left", pad="0"), 
                               str_pad(income@geography$tract, 6, "left", pad="0")), 
                        income@estimate[,c("Household Income: Total:",
                                           "Household Income: $200,000 or more")], 
                        stringsAsFactors = FALSE)
income_df <- select(income_df, 1:3)
rownames(income_df)<-1:nrow(income_df)
names(income_df)<-c("GEOID", "total", "over_200")
income_df$percent <- 100*(income_df$over_200/income_df$total)

# Merge spatial and tabular data
income_merged <- geo_join(tracts, income_df, "GEOID", "GEOID")
income_merged <- income_merged[income_merged$ALAND>0,]

###
# Get latitude and longitude
###

# Which variable to use for merging? => GEOID
names(income_merged)
names(income_df)

# Are they similarly coded? => Yes, plus income_merged already contains information about lat/lon
head(income_df[,"GEOID"])
head(income_merged[,"GEOID"], n=1) 

# Create a data frame of income_merged with explicit latitude and longitude
income_merged_map <- ggplot2::fortify(income_merged, region="GEOID")

# Rename column names for later merges
colnames(income_merged_map)[colnames(income_merged_map)=="id"] <- "GEOID"
colnames(income_merged_map)[colnames(income_merged_map)=="long"] <- "lon"
# Drop superfluous variables
income_merged_map$order <- income_merged_map$hole <- income_merged_map$piece <- NULL

# Merge data frame for socio-economic information on Seattle
KingCounty.SOE <- left_join(x=income_merged_map, y=income_df, by="GEOID")

table(KingCounty.SOE$GEOID)

#https://www.census.gov/geo/reference/geoidentifiers.html
# state = 53
# King county = 033
# Tracts = 1:121?http://www.seattle.gov/dpd/cs/groups/pan/@pan/documents/web_informational/dpdd017051.pdf

Seattle.SOE <- KingCounty.SOE[which(KingCounty.SOE$GEOID>=53033000100 & KingCounty.SOE$GEOID <=53033012100),]

#http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3/
rm(counties, geo, income, income_merged, income_df, income_merged_map, tracts, KingCounty.SOE)