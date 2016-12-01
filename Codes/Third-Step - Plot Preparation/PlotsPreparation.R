# Setting our working directory
wrkdir <- c('C:/Users/Benji/Desktop/Statistics/Git/Repositories/E1180Project', 
            '~/Hertie School/Fall 2016/CollaborativeSocialScienceDataAnalysis/CSSR')
repmis::set_valid_wd(wrkdir)
rm(wrkdir)

# Installing packages
packages <- c('dplyr', 'repmis', 'fields', 'cowplot', 'ggmap', 'ggplot2', 'stargazer', 'scales', 'doBy')
for (p in packages) {
  if (p %in% installed.packages()) require(p, character.only=TRUE) 
  else {
    install.packages(p)
    require(p, character.only=TRUE)
  }
}

# Creating a BibTex file
repmis::LoadandCite(packages, file = './BibTex/Analysis.bib')

# Removing from global environment for better readability
rm(packages, p, wrkdir)

# Source data for barplots
source("./Codes/Second Step - Cleaning/Cleaning_Retailers.R") # For first barplot
source("./Codes/Second Step - Cleaning/Cleaning_ProdProc.R") # For second barplot

# Load data for mapping and analysis
Seattle.Crime.Analysis <- read.table(gzfile("./Data/SeattleCrimeAnalysis.gz"),row.names=1)
Seattle.SOE.Map <- read.table(gzfile("./Data/SeattleMap.gz"),row.names=1)

###
# Descriptive Statistics Washington State - Producers/Processors and Retailers
###

# 1. How many Producers/Processors and Retailers per year for Washington

# 1.a Create unique Producers/Processors and Retailers lengths for barplot
Retailer2014 <- Retailer[which(Retailer$Month=="2014-12"),]
Retailer2015 <- Retailer[which(Retailer$Month=="2015-12"),]
Retailer2016 <- Retailer[which(Retailer$Month!="2015-12" & Retailer$Month!="2014-12"),]

Retailer2014.uniq <- as.numeric(length(which(!duplicated(Retailer2014$RetailerURLLink))))
Retailer2015.uniq <- as.numeric(length(which(!duplicated(Retailer2015$RetailerURLLink))))
Retailer2016.uniq <- as.numeric(length(which(!duplicated(Retailer2016$RetailerURLLink))))

ProdProc2014 <- ProdProc[which(ProdProc$Month=="2014-12"),]
ProdProc2015 <- ProdProc[which(ProdProc$Month=="2015-12"),]
ProdProc2016 <- ProdProc[which(ProdProc$Month!="2015-12" & ProdProc$Month!="2014-12"),]

ProdProc2014.uniq <- as.numeric(length(which(!duplicated(ProdProc2014$Name))))
ProdProc2015.uniq <- as.numeric(length(which(!duplicated(ProdProc2015$Name))))
ProdProc2016.uniq <- as.numeric(length(which(!duplicated(ProdProc2016$Name))))

# 1.b Create the barplot
Year <- c("2014", "2015", "Oct-2016")
WA.numbers <- c(ProdProc2014.uniq, ProdProc2015.uniq, ProdProc2016.uniq,
                Retailer2014.uniq, Retailer2015.uniq, Retailer2016.uniq)
WA.name <- c("Producer / Processor", "Producer / Processor", "Producer / Processor",
             "Retailer", "Retailer", "Retailer")
WA.bar <- data.frame(Year, WA.numbers, WA.name, stringsAsFactors = FALSE)


washington.barplot <- ggplot(WA.bar, aes(x=Year, y=WA.numbers, fill=WA.name)) +
  geom_bar(stat="identity", colour="black",width=.8, position=position_dodge()) + 
  theme_bw() + xlab("Year") + ylab("Frequency") + scale_fill_hue(name="") +
  ggtitle("Number of Firms in WA")

# Growth pattern:

# Number of Producers and businesses: 
# 2015: increase by 210 % (from 185 to 574)
# 2016: increase by 53 % (from 574 to 881)

# Number of Retailers:
# 2015: increase by 130 % (from 82 to 189)
# 2016: increase by 69 % (from 189 to 319)

rm(Retailer2016.uniq, Retailer2015.uniq, Retailer2014.uniq, Retailer2016, Retailer2015, Retailer2014,
   ProdProc2016.uniq, ProdProc2015.uniq, ProdProc2014.uniq, ProdProc2016, ProdProc2015, ProdProc2014,
   WA.bar, WA.name, WA.numbers)

# 2. How many Producers/Processors and Retailers per year for King County
Sales2014 <- sum(Retailer$Sales.Ret[Retailer$Month.Ret=="2014-12"])/1e+6
Sales2015 <- sum(Retailer$Sales.Ret[Retailer$Month.Ret=="2015-12"])/1e+6
Sales2016 <- (sum(Retailer$Sales.Ret[Retailer$Month.Ret=="2016-01"]) +
                sum(Retailer$Sales.Ret[Retailer$Month.Ret=="2016-02"]) +
                sum(Retailer$Sales.Ret[Retailer$Month.Ret=="2016-03"]) +
                sum(Retailer$Sales.Ret[Retailer$Month.Ret=="2016-04"]) +
                sum(Retailer$Sales.Ret[Retailer$Month.Ret=="2016-05"]) +
                sum(Retailer$Sales.Ret[Retailer$Month.Ret=="2016-06"]) +
                sum(Retailer$Sales.Ret[Retailer$Month.Ret=="2016-07"]) +
                sum(Retailer$Sales.Ret[Retailer$Month.Ret=="2016-08"]) +
                sum(Retailer$Sales.Ret[Retailer$Month.Ret=="2016-09"]) +
                sum(Retailer$Sales.Ret[Retailer$Month.Ret=="2016-10"]))/1e+6

ExciseTax2014 <- sum(Retailer$ExciseTax.Ret[Retailer$Month.Ret=="2014-12"])/1e+6
ExciseTax2015 <- sum(Retailer$ExciseTax.Ret[Retailer$Month.Ret=="2015-12"])/1e+6
ExciseTax2016 <- (sum(Retailer$ExciseTax.Ret[Retailer$Month.Ret=="2016-01"]) +
                    sum(Retailer$ExciseTax.Ret[Retailer$Month.Ret=="2016-02"]) +
                    sum(Retailer$ExciseTax.Ret[Retailer$Month.Ret=="2016-03"]) +
                    sum(Retailer$ExciseTax.Ret[Retailer$Month.Ret=="2016-04"]) +
                    sum(Retailer$ExciseTax.Ret[Retailer$Month.Ret=="2016-05"]) +
                    sum(Retailer$ExciseTax.Ret[Retailer$Month.Ret=="2016-06"]) +
                    sum(Retailer$ExciseTax.Ret[Retailer$Month.Ret=="2016-07"]) +
                    sum(Retailer$ExciseTax.Ret[Retailer$Month.Ret=="2016-08"]) +
                    sum(Retailer$ExciseTax.Ret[Retailer$Month.Ret=="2016-09"]) +
                    sum(Retailer$ExciseTax.Ret[Retailer$Month.Ret=="2016-10"]))/1e+6

# 1.b Create the barplot
WA.numbers.e <- c(Sales2014, Sales2015, Sales2016,
                  ExciseTax2014, ExciseTax2015, ExciseTax2016)
WA.name.e <- c("Sales", "Sales", "Sales",
               "Excise Tax", "Excise Tax", "Excise Tax")
WA.bar.e <- data.frame(Year, WA.numbers.e, WA.name.e, stringsAsFactors = FALSE)


washington.barplot.e <- ggplot(WA.bar.e, aes(x=Year, y=WA.numbers.e, fill=WA.name.e)) +
  geom_bar(stat="identity", colour="black",width=.8, position=position_dodge()) + 
  theme_bw() + xlab("Year") + ylab("Amount in Mio. US-Dollars") + 
  scale_fill_hue(name="") +
  ggtitle("Cannabis Sales and Tax Revenues in WA")

# Growth pattern:

# Increase of Sales Revenue: 
# 2015: increase by 960 % (from 29.21 to 309.86)
# 2016: increase by 77 % (from 309.86 to 549.93)

# Increase of Tax Revenue:
# 2015: increase by 1040 % (from 9.74 to 110.97)
# 2016: increase by 83 % (from 110.97 to 203.47)

rm(Sales2014, Sales2015, Sales2016, ExciseTax2014, ExciseTax2015, ExciseTax2016,
   WA.bar.e, WA.name.e, WA.numbers.e, Year)

# 3. Create a crime subset for Month October 2016

CrimeOct2016 <- Seattle.Crime.Analysis[Seattle.Crime.Analysis$Month=="2016-10",]

# 4. Crime development in Seattle
# Data set will have
# Alcohol
# Burglary/Theft
# Marijuana
# Narcotics
# Other Drug Related
# Property
# Violent Crime
# Frequency of crime per Month

str(Seattle.Crime.Analysis$CrimePerThousand)

Crime.lines <- aggregate(cbind(CrimePerThousand) ~ Crime + Month, data=Seattle.Crime.Analysis, FUN=function(x) mean(range(x)))
Crime.lines$log <- log(Crime.lines$CrimePerThousand)

# 5. Analysis

# 1. Create specific crime variables for the analysis of crime subsets

table(Seattle.Crime.Analysis$Crime)

Seattle.Crime.Analysis$AlcCrime <- NA
Seattle.Crime.Analysis$AlcCrime[Seattle.Crime.Analysis$Crime=="Alcohol"] <- Seattle.Crime.Analysis$CrimePerThousand[Seattle.Crime.Analysis$Crime=="Alcohol"]

Seattle.Crime.Analysis$BurCrime <- NA
Seattle.Crime.Analysis$BurCrime[Seattle.Crime.Analysis$Crime=="Burglary/Theft"] <- Seattle.Crime.Analysis$CrimePerThousand[Seattle.Crime.Analysis$Crime=="Burglary/Theft"]

Seattle.Crime.Analysis$MarCrime <- NA
Seattle.Crime.Analysis$MarCrime[Seattle.Crime.Analysis$Crime=="Marijuana"] <- Seattle.Crime.Analysis$CrimePerThousand[Seattle.Crime.Analysis$Crime=="Marijuana"]

Seattle.Crime.Analysis$NarcCrime <- NA
Seattle.Crime.Analysis$NarcCrime[Seattle.Crime.Analysis$Crime=="Narcotics"] <- Seattle.Crime.Analysis$CrimePerThousand[Seattle.Crime.Analysis$Crime=="Narcotics"]

Seattle.Crime.Analysis$OtherCrime <- NA
Seattle.Crime.Analysis$OtherCrime[Seattle.Crime.Analysis$Crime=="Other Drug Related"] <- Seattle.Crime.Analysis$CrimePerThousand[Seattle.Crime.Analysis$Crime=="Other Drug Related"]

Seattle.Crime.Analysis$PropCrime <- NA
Seattle.Crime.Analysis$PropCrime[Seattle.Crime.Analysis$Crime=="Property"] <- Seattle.Crime.Analysis$CrimePerThousand[Seattle.Crime.Analysis$Crime=="Property"]

Seattle.Crime.Analysis$ViolCrime <- NA
Seattle.Crime.Analysis$ViolCrime[Seattle.Crime.Analysis$Crime=="Violent Crime"] <- Seattle.Crime.Analysis$CrimePerThousand[Seattle.Crime.Analysis$Crime=="Violent Crime"]

# 2. Make sure that GEOID is coded as a factor variable
Seattle.Crime.Analysis$GEOID <- as.factor(Seattle.Crime.Analysis$GEOID)

# 3. Barplots for all different subsets of crime
Box3 <- ggplot(Seattle.Crime.Analysis, aes(factor(Established), log(AlcCrime)))
Box3 <- Box3 + geom_boxplot(aes(fill=Established)) +
  xlab("Alcohol-related crime (insignificant)") + ylab("")  +
  ylim(-6,4) + theme(legend.position='none')

Box4 <- ggplot(Seattle.Crime.Analysis, aes(factor(Established), log(BurCrime)))
Box4 <- Box4 + geom_boxplot(aes(fill=Established)) +
  xlab("Burglary/Theft") + ylab("")  +
  ylim(-6,4) + theme(legend.position='none')

Box5 <- ggplot(Seattle.Crime.Analysis, aes(factor(Established), log(MarCrime)))
Box5 <- Box5 + geom_boxplot(aes(fill=Established)) +
  xlab("Marijuana-related crime") + ylab("")  +
  ylim(-6,4) + theme(legend.position='none')

Box6 <- ggplot(Seattle.Crime.Analysis, aes(factor(Established), log(NarcCrime)))
Box6 <- Box6 + geom_boxplot(aes(fill=Established)) +
  xlab("Narcotics-related crime") + ylab("Natural logarithm of Crime")  +
  ylim(-6,4) + theme(legend.position='none')

Box7 <- ggplot(Seattle.Crime.Analysis, aes(factor(Established), log(OtherCrime)))
Box7 <- Box7 + geom_boxplot(aes(fill=Established)) +
  xlab("Other drug-related crime") + ylab("")  +
  ylim(-6,4) + theme(legend.position='none')

Box8 <- ggplot(Seattle.Crime.Analysis, aes(factor(Established), log(PropCrime)))
Box8 <- Box8 + geom_boxplot(aes(fill=Established)) +
  xlab("Property crime") + ylab("")  +
  ylim(-6,4) + theme(legend.position='none')

Box9 <- ggplot(Seattle.Crime.Analysis, aes(factor(Established), log(ViolCrime)))
Box9 <- Box9 + geom_boxplot(aes(fill=Established)) +
  xlab("Violent crime") + ylab("")  +
  ylim(-6,4) + theme(legend.position='none')

