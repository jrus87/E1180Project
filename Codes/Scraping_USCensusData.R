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
# Keywords
###

# Find the Table for the Data
# Using fips number as acs.fetch function does not work with county name

# grab the spatial data (tigris) # 53033 = fips code?
counties <- c(33)
tracts <- tracts(state = 'WA', county = c(33), cb=TRUE)

# Use personal API key
api.key.install(key="c44d5d23fb897a3d2e9e9f8daea97d67cff5e2f5")
geo <- geo.make(state=c("WA"), county=c(33), tract="*")

###
# Fetch the data
###

# "col.names='pretty'" gives full column definitions in a nice, readable format

# Household income
income <- acs.fetch(endyear = 2014, span = 5, geography = geo,
                    table.number = "B19001", col.names = "pretty")
# What are the table descriptions for income?
names(attributes(income))
attr(income, "acs.colnames")

# Education
education <- acs.fetch(endyear = 2014, span = 5, geography = geo,
                       table.number = "B06009", col.names = "pretty")
# What are the table descriptions for education?
names(attributes(education))
attr(education, "acs.colnames")

# Race
race <- acs.fetch(endyear = 2014, span = 5, geography = geo,
                  table.number = "B02001", col.names = "pretty")
# What are the table descriptions for race?
names(attributes(race))
attr(race, "acs.colnames")

# poverty
poverty <- acs.fetch(endyear = 2014, span = 5, geography = geo,
                     table.number = "B17004", col.names = "pretty")
# What are the table descriptions for poverty?
names(attributes(poverty))
attr(poverty, "acs.colnames")

# age
age <- acs.fetch(endyear = 2014, span = 5, geography = geo,
                 table.number = "B01001", col.names = "pretty")
# What are the table descriptions for age?
names(attributes(age))
attr(age, "acs.colnames")

###
# Prepare for merger
###

# Income
income_df <- data.frame(paste0(str_pad(income@geography$state, 2, "left", pad="0"), 
                               str_pad(income@geography$county, 3, "left", pad="0"), 
                               str_pad(income@geography$tract, 6, "left", pad="0")), 
                        income@estimate[,c("B19001. Household Income in the Past 12 Months (in 2014 Inflation-Adjusted Dollars): Total:",
                                           "B19001. Household Income in the Past 12 Months (in 2014 Inflation-Adjusted Dollars): $200,000 or more")], 
                        stringsAsFactors = FALSE)
income_df <- select(income_df, 1:3)
rownames(income_df) <- 1:nrow(income_df)
names(income_df) <- c("GEOID", "IncomeTotal", "over_200")
income_df$percent <- 100*(income_df$over_200/income_df$IncomeTotal)

# Education
education_df <- data.frame(paste0(str_pad(education@geography$state, 2, "left", pad="0"), 
                                  str_pad(education@geography$county, 3, "left", pad="0"), 
                                  str_pad(education@geography$tract, 6, "left", pad="0")), 
                           education@estimate[,c("PLACE OF BIRTH BY EDUCATIONAL ATTAINMENT IN THE UNITED STATES: Total:",
                                                 "PLACE OF BIRTH BY EDUCATIONAL ATTAINMENT IN THE UNITED STATES: Less than high school graduate",
                                                 "PLACE OF BIRTH BY EDUCATIONAL ATTAINMENT IN THE UNITED STATES: High school graduate (includes equivalency)",
                                                 "PLACE OF BIRTH BY EDUCATIONAL ATTAINMENT IN THE UNITED STATES: Some college or associate's degree",
                                                 "PLACE OF BIRTH BY EDUCATIONAL ATTAINMENT IN THE UNITED STATES: Bachelor's degree",
                                                 "PLACE OF BIRTH BY EDUCATIONAL ATTAINMENT IN THE UNITED STATES: Graduate or professional degree")], 
                           stringsAsFactors = FALSE)
education_df <- select(education_df, 1:7)
rownames(education_df) <- 1:nrow(education_df)
names(education_df) <- c("GEOID", "EducationTotal", "Less than High School", "High School", "Associate", "Bachelor's", "Graduate")
education_df$HighSchool <- 100*((education_df$`Less than High School`+education_df$`High School`)/education_df$EducationTotal)
education_df$Degree <- 100*((education_df$Associate+education_df$Graduate+education_df$`Bachelor's`)/education_df$EducationTotal)
education_df <- education_df[,-c(3:7)]

# Race
race_df <- data.frame(paste0(str_pad(race@geography$state, 2, "left", pad="0"), 
                             str_pad(race@geography$county, 3, "left", pad="0"), 
                             str_pad(race@geography$tract, 6, "left", pad="0")), 
                      race@estimate[,c("Race: Total:",
                                       "Race: White alone",
                                       "Race: Black or African American alone",
                                       "Race: Two or more races:")], 
                      stringsAsFactors = FALSE)
race_df <- select(race_df, 1:5)
rownames(race_df) <- 1:nrow(race_df)
names(race_df) <- c("GEOID", "RaceTotal", "White alone", "Black or African American alone", "Two or more races")
race_df$White <- 100*(race_df$`White alone`/race_df$RaceTotal)
race_df$Black <- 100*(race_df$`Black or African American alone`/race_df$RaceTotal)
race_df$Multiple <- 100*(race_df$`Two or more races`/race_df$RaceTotal)
race_df <- race_df[,-c(3:5)]

# poverty
poverty_df <- data.frame(paste0(str_pad(poverty@geography$state, 2, "left", pad="0"), 
                                str_pad(poverty@geography$county, 3, "left", pad="0"), 
                                str_pad(poverty@geography$tract, 6, "left", pad="0")), 
                         poverty@estimate[,c("POVERTY STATUS IN THE PAST 12 MONTHS OF INDIVIDUALS BY SEX BY WORK EXPERIENCE: Total:",
                                             "POVERTY STATUS IN THE PAST 12 MONTHS OF INDIVIDUALS BY SEX BY WORK EXPERIENCE: Income in the past 12 months below poverty level: Male:",
                                             "POVERTY STATUS IN THE PAST 12 MONTHS OF INDIVIDUALS BY SEX BY WORK EXPERIENCE: Income in the past 12 months below poverty level: Female:")], 
                         stringsAsFactors = FALSE)
poverty_df <- select(poverty_df, 1:4)
rownames(poverty_df) <- 1:nrow(poverty_df)
names(poverty_df) <- c("GEOID", "PovertyTotal", "Male", "Female")
poverty_df$Poverty <- 100*((poverty_df$Male+poverty_df$Female)/poverty_df$PovertyTotal)
poverty_df <- poverty_df[,-c(3:4)]

# age
age_df <- data.frame(paste0(str_pad(age@geography$state, 2, "left", pad="0"), 
                            str_pad(age@geography$county, 3, "left", pad="0"), 
                            str_pad(age@geography$tract, 6, "left", pad="0")), 
                     age@estimate[,c(1:49)],
                     stringsAsFactors = FALSE)
age_df <- select(age_df, 1:50)
rownames(age_df) <- 1:nrow(age_df)
age_df <- age_df[,c(1:2, 8:27, 32:50)] # between 18 and 64 years of age
age_df$YoungMale <- (age_df$Sex.by.Age..Male..18.and.19.years +
                       age_df$Sex.by.Age..Male..20.years +
                       age_df$Sex.by.Age..Male..21.years +
                       age_df$Sex.by.Age..Male..22.to.24.years +
                       age_df$Sex.by.Age..Male..25.to.29.years +
                       age_df$Sex.by.Age..Male..30.to.34.years +
                       age_df$Sex.by.Age..Male..35.to.39.years +
                       age_df$Sex.by.Age..Male..40.to.44.years +
                       age_df$Sex.by.Age..Male..45.to.49.years +
                       age_df$Sex.by.Age..Male..50.to.54.years +
                       age_df$Sex.by.Age..Male..55.to.59.years +
                       age_df$Sex.by.Age..Male..60.and.61.years +
                       age_df$Sex.by.Age..Male..62.to.64.years)
age_df$OldMale <- (age_df$Sex.by.Age..Male..65.and.66.years +
                     age_df$Sex.by.Age..Male..67.to.69.years + 
                     age_df$Sex.by.Age..Male..70.to.74.years +
                     age_df$Sex.by.Age..Male..75.to.79.years +
                     age_df$Sex.by.Age..Male..80.to.84.years +
                     age_df$Sex.by.Age..Male..85.years.and.over)
age_df$YoungFemale <- (age_df$Sex.by.Age..Female..18.and.19.years +
                         age_df$Sex.by.Age..Female..20.years +
                         age_df$Sex.by.Age..Female..21.years +
                         age_df$Sex.by.Age..Female..22.to.24.years +
                         age_df$Sex.by.Age..Female..25.to.29.years +
                         age_df$Sex.by.Age..Female..30.to.34.years +
                         age_df$Sex.by.Age..Female..35.to.39.years +
                         age_df$Sex.by.Age..Female..40.to.44.years +
                         age_df$Sex.by.Age..Female..45.to.49.years +
                         age_df$Sex.by.Age..Female..50.to.54.years +
                         age_df$Sex.by.Age..Female..55.to.59.years +
                         age_df$Sex.by.Age..Female..60.and.61.years +
                         age_df$Sex.by.Age..Female..62.to.64.years)
age_df$OldFemale <- (age_df$Sex.by.Age..Female..65.and.66.years +
                       age_df$Sex.by.Age..Female..67.to.69.years + 
                       age_df$Sex.by.Age..Female..70.to.74.years +
                       age_df$Sex.by.Age..Female..75.to.79.years +
                       age_df$Sex.by.Age..Female..80.to.84.years +
                       age_df$Sex.by.Age..Female..85.years.and.over)
age_df <- age_df[,c(1:2, 42:45)]
names(age_df) <- c("GEOID", "AgeTotal", "YoungMale", "OldMale", "YoungFemale", "OldFemale")
age_df$Adult <- 100*((age_df$YoungMale+age_df$YoungFemale)/age_df$AgeTotal)
age_df$Retiree <- 100*((age_df$OldMale+age_df$OldFemale)/age_df$AgeTotal)
age_df$Kids <- 100-(age_df$Adult+age_df$Retiree)
age_df <- age_df[,-c(3:6)]

###
# Merge table and spatial data
###

# here merge first all SOEs
income_merged <- geo_join(tracts, income_df, "GEOID", "GEOID")
income_merged <- income_merged[income_merged$ALAND>0,]

# Create a data frame of income_merged with explicit latitude and longitude
income_merged_map <- ggplot2::fortify(income_merged, region="GEOID")

# Rename column names for later merges
colnames(income_merged_map)[colnames(income_merged_map)=="id"] <- "GEOID"
colnames(income_merged_map)[colnames(income_merged_map)=="long"] <- "lon"
# Drop superfluous variables
income_merged_map$order <- income_merged_map$hole <- income_merged_map$piece <- NULL

# merge other dfs by GEOID first

SOE_df1 <- merge(age_df, education_df, by="GEOID")
names(SOE_df1)
SOE_df2 <- merge(SOE_df1, income_df, by="GEOID")
names(SOE_df2)
SOE_df3 <- merge(SOE_df2, race_df, by="GEOID")
names(SOE_df3)
SOE_df <- merge(SOE_df3, poverty_df, by="GEOID")
names(SOE_df)

rm(SOE_df1, SOE_df2, SOE_df3, age, education, geo, counties, income, income_merged, poverty, race, tracts,
   age_df, education_df, income_df, poverty_df, race_df)

# Merge data frame for socio-economic information on Seattle
KingCounty.SOE <- left_join(x=income_merged_map, y=SOE_df, by="GEOID")

table(KingCounty.SOE$GEOID)

#https://www.census.gov/geo/reference/geoidentifiers.html
# state = 53
# King county = 033
# Tracts = 1:121?http://www.seattle.gov/dpd/cs/groups/pan/@pan/documents/web_informational/dpdd017051.pdf

Seattle.SOE <- KingCounty.SOE[which(KingCounty.SOE$GEOID>=53033000100 & KingCounty.SOE$GEOID <=53033012100),]

rm(income_merged_map, KingCounty.SOE, SOE_df)