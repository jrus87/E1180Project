# Setting our working directory
wrkdir <- c('C:/Users/Benji/Desktop/Statistics/Git/Repositories/E1180Project', 
            '~/Hertie School/Fall 2016/CollaborativeSocialScienceDataAnalysis/CSSR')
repmis::set_valid_wd(wrkdir)

# Installing packages
packages <- c('readr')
for (p in packages) {
  if (p %in% installed.packages()) require(p, character.only=TRUE) 
  else {
    install.packages(p)
    require(p, character.only=TRUE)
  }
}

# Creating a BibTex file
repmis::LoadandCite(packages, file = './BibTex/RpackageCitations_PolicingDenver.bib')

# Removing from global environment for better readability
rm(packages, p, wrkdir)

# Load Data (Open Denver Data)
CrimeDenver <- read_csv("./Data/crimeDenver.csv")
head(CrimeDenver, n=10)

# Descriptive
table(CrimeDenver$OFFENSE_CATEGORY_ID) # NON-CRIMINAL 211449
CrimeDenver2 <- CrimeDenver[which(CrimeDenver$OFFENSE_CATEGORY_ID=="drug-alcohol"),]

table(CrimeDenver2$OFFENSE_TYPE_ID)
# cultivation 114; possess 2562; sell 404
CrimeDenver2 <- CrimeDenver2[order(CrimeDenver2$OFFENSE_TYPE_ID),]
which(CrimeDenver2$OFFENSE_TYPE_ID=="drug-marijuana-cultivation") # 7072:7185
which(CrimeDenver2$OFFENSE_TYPE_ID=="drug-marijuana-possess") # 7186:9747
which(CrimeDenver2$OFFENSE_TYPE_ID=="drug-marijuana-sell") # 9748:10151

CrimeDenver3 <- CrimeDenver2[c(7072:10151),]
table(CrimeDenver3$OFFENSE_TYPE_ID)
# cultivation 114; possess 2562; sell 404

# all other drugs
CrimeDenver4 <- CrimeDenver2[c(1:7071, 10152:25114),]
table(CrimeDenver4$OFFENSE_TYPE_ID)

# which year?
table(CrimeDenver3$FIRST_OCCURRENCE_DATE)
CrimeDenver3 <- CrimeDenver3[order(CrimeDenver3$FIRST_OCCURRENCE_DATE),]
which(grepl("2011", CrimeDenver3$FIRST_OCCURRENCE_DATE)) # 1:184
which(grepl("2012", CrimeDenver3$FIRST_OCCURRENCE_DATE)) # 185:360
which(grepl("2013", CrimeDenver3$FIRST_OCCURRENCE_DATE)) # 361:835
which(grepl("2014", CrimeDenver3$FIRST_OCCURRENCE_DATE)) # 836:1582
which(grepl("2015", CrimeDenver3$FIRST_OCCURRENCE_DATE)) # 1583:2574
which(grepl("2016", CrimeDenver3$FIRST_OCCURRENCE_DATE)) # 2575:3080

# which year?
table(CrimeDenver4$FIRST_OCCURRENCE_DATE)
CrimeDenver4 <- CrimeDenver4[order(CrimeDenver4$FIRST_OCCURRENCE_DATE),]
which(grepl("2011", CrimeDenver4$FIRST_OCCURRENCE_DATE)) # 1:1232
which(grepl("2012", CrimeDenver4$FIRST_OCCURRENCE_DATE)) # 1233:2760
which(grepl("2013", CrimeDenver4$FIRST_OCCURRENCE_DATE)) # 2761:7033
which(grepl("2014", CrimeDenver4$FIRST_OCCURRENCE_DATE)) # 7034:12347
which(grepl("2015", CrimeDenver4$FIRST_OCCURRENCE_DATE)) # 12348:17511
which(grepl("2016", CrimeDenver4$FIRST_OCCURRENCE_DATE)) # 17511:22034


#subset year
Y2011 <- CrimeDenver3[c(1:184),] # 184 observations
Y2012 <- CrimeDenver3[c(185:360),] # 176 observations
Y2013 <- CrimeDenver3[c(361:835),] # 475 observations
Y2014 <- CrimeDenver3[c(836:1582),] # 747 observations
Y2015 <- CrimeDenver3[c(1583:2574),] # 992 observations
Y2016 <- CrimeDenver3[c(2575:3080),] # 506 observations (October)

#subset year
Y2011 <- CrimeDenver4[c(1:1232),] # 1232 observations
Y2012 <- CrimeDenver4[c(1233:2760),] # 1528 observations
Y2013 <- CrimeDenver4[c(2761:7033),] # 4273 observations
Y2014 <- CrimeDenver4[c(7034:12347),] # 5314 observations
Y2015 <- CrimeDenver4[c(12348:17511),] # 5164 observations
Y2016 <- CrimeDenver4[c(17511:22034),] # 4524 observations (October)

# 
table(Y2011$OFFENSE_TYPE_ID) # 23 cultivation; 76 possess;  85 sell   | cocaine possess
table(Y2012$OFFENSE_TYPE_ID) # 18 cultivation; 68 possess;  90 sell   | cocaine possess
table(Y2013$OFFENSE_TYPE_ID) # 6  cultivation; 406 possess; 63 sell   | liquor posession (cocaine and meth close)
table(Y2014$OFFENSE_TYPE_ID) # 18 cultivation; 667 possess; 62 sell   | liquor posession (paraphernalia; cocaine and meth close)
table(Y2015$OFFENSE_TYPE_ID) # 22 cultivation; 913 possess; 57 sell   | liquor posession (paraphernalia; meth and cocaine close)
table(Y2016$OFFENSE_TYPE_ID) # 27 cultivation; 432 possess; 47 sell   | liquor posession (paraphernalia; meth)

# 

### All other drugs reduced or increased?
