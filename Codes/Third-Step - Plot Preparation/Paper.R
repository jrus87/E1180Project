# Setting our working directory
wrkdir <- c('C:/Users/Benji/Desktop/Statistics/Git/Repositories/E1180Project', 
            '~/Hertie School/Fall 2016/CollaborativeSocialScienceDataAnalysis/CSSR')
repmis::set_valid_wd(wrkdir)
rm(wrkdir)

# Installing packages
packages <- c('dplyr', 'repmis', 'fields', 'cowplot', 'ggmap', 'ggplot2', 'stargazer', 'scales', 'doBy', 'xtable', 'plm')
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
  ggtitle("Number of Firms in WA") +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom")  

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
  ggtitle("Cannabis Sales and Tax Revenues in WA") +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") 

# Growth pattern:

# Increase of Sales Revenue: 
# 2015: increase by 960 % (from 29.21 to 309.86)
# 2016: increase by 77 % (from 309.86 to 549.93)

# Increase of Tax Revenue:
# 2015: increase by 1040 % (from 9.74 to 110.97)
# 2016: increase by 83 % (from 110.97 to 203.47)

rm(Sales2014, Sales2015, Sales2016, ExciseTax2014, ExciseTax2015, ExciseTax2016,
   WA.bar.e, WA.name.e, WA.numbers.e, Year)

# 2. Table of variables
Variables <- c("Producer/Processor", "Retailer", "Excise Tax", "Sales", "Crime Categories", "Established", "Poverty Rate",
               "Age", "Diversity", "Education", "GEOID", "Time")
Operationalisation <- c("Count of Producers/Processors in WA", "Count of Retailers in WA", "Sum of taxes paid by retailers",
                        "Sum of revenue made by retailers", "Aggregates of Crime incidents from Seattle",
                        "Treatment variable: 0 if no dispensary, 1 otherwise", "Share of People < $15,000/year as percent of total people in district",
                        "Shares of Adults in comparison to mean of Seattle", "Share of Whites in comparison to mean of Seattle", 
                        "Share of Degree holders in comparison to mean of Seattle","U.S. Census District", "Months from Jan 2014 to Oct 2016")
Table <- data.frame(Variables, Operationalisation, stringsAsFactors = TRUE)

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

crime.graph <- ggplot(Crime.lines, aes(Month, CrimePerThousand, group=Crime, colour = factor(Crime))) 
crime.graph <- crime.graph + geom_line() +
  xlab("Months") + ylab("Crime per 1,000 citizens") + theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  labs(colour = "Crime Categories") +
  ggtitle("Crime rates development in Seattle")

crime.graph.log <- ggplot(Crime.lines, aes(Month, log, group=Crime, colour = factor(Crime))) 
crime.graph.log <- crime.graph.log + geom_line() +
  xlab("Months") + ylab("ln(Crime per 1,000 citizens)") + theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  labs(colour = "Crime Categories") +
  ggtitle("Crime rates development in Seattle")

# 5. Analysis

# 1. Create specific crime variables for the analysis of crime subsets

table(Seattle.Crime.Analysis$Crime)

na_drop <- which(is.na(Seattle.Crime.Analysis$CrimePerThousand))
Seattle.Crime.Analysis <- Seattle.Crime.Analysis[-na_drop,]
rm(na_drop)

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
  xlab("Alcohol (insignificant)") + ylab("")  +
  ylim(-6,4) + theme(legend.position='none')

Box4 <- ggplot(Seattle.Crime.Analysis, aes(factor(Established), log(BurCrime)))
Box4 <- Box4 + geom_boxplot(aes(fill=Established)) +
  xlab("Burglary/Theft") + ylab("")  +
  ylim(-6,4) + theme(legend.position='none')

Box5 <- ggplot(Seattle.Crime.Analysis, aes(factor(Established), log(MarCrime)))
Box5 <- Box5 + geom_boxplot(aes(fill=Established)) +
  xlab("Marijuana") + ylab("")  +
  ylim(-6,4) + theme(legend.position='none')

Box6 <- ggplot(Seattle.Crime.Analysis, aes(factor(Established), log(NarcCrime)))
Box6 <- Box6 + geom_boxplot(aes(fill=Established)) +
  xlab("Narcotics") + ylab("Natural logarithm of Crime")  +
  ylim(-6,4) + theme(legend.position='none')

Box7 <- ggplot(Seattle.Crime.Analysis, aes(factor(Established), log(OtherCrime)))
Box7 <- Box7 + geom_boxplot(aes(fill=Established)) +
  xlab("Other drugs") + ylab("")  +
  ylim(-6,4) + theme(legend.position='none')

Box8 <- ggplot(Seattle.Crime.Analysis, aes(factor(Established), log(PropCrime)))
Box8 <- Box8 + geom_boxplot(aes(fill=Established)) +
  xlab("Property crime") + ylab("")  +
  ylim(-6,4) + theme(legend.position='none')

Box9 <- ggplot(Seattle.Crime.Analysis, aes(factor(Established), log(ViolCrime)))
Box9 <- Box9 + geom_boxplot(aes(fill=Established)) +
  xlab("Violent crime") + ylab("")  +
  ylim(-6,4) + theme(legend.position='none')

#####
## Inferential statistics
#####

Analysis <- Seattle.Crime.Analysis[,c(1:3,6:15,17,28, 30:38)]

# 1. Create Categorical Variables

# Education 
quantile(Analysis$Degree)

Analysis$EduCat <- NA
Analysis$EduCat[Analysis$Degree<76.56882] <- '# Graduates: Very low'
Analysis$EduCat[Analysis$Degree>=76.56882 & Analysis$Degree<84.29752] <- '# Graduates: Low'
Analysis$EduCat[Analysis$Degree>=84.29752 & Analysis$Degree<93.87976] <- '# Graduates: High'
Analysis$EduCat[Analysis$Degree>=93.87976] <- '# Graduates: Very High'
Analysis$EduCat <- as.factor(Analysis$EduCat)

table(Analysis$EduCat)
contrasts(Analysis$EduCat)
Analysis$EduCat <-relevel(Analysis$EduCat, "# Graduates: Low")


# Diversity
quantile(Analysis$White)

Analysis$Diversity <- NA
Analysis$Diversity[Analysis$White<60.769069] <- 'Diversity: Very high'
Analysis$Diversity[Analysis$White>=60.769069 & Analysis$White<82.327586] <- 'Diversity: High'
Analysis$Diversity[Analysis$White>=82.327586 & Analysis$White<90.541192] <- 'Diversity: Low'
Analysis$Diversity[Analysis$White>=90.541192] <- 'Diversity: Very Low'
Analysis$Diversity <- as.factor(Analysis$Diversity)

table(Analysis$Diversity)
contrasts(Analysis$Diversity)
Analysis$Diversity <-relevel(Analysis$Diversity, "Diversity: Low")

# Adults
quantile(Analysis$Adult)

Analysis$Adulthood <- NA
Analysis$Adulthood[Analysis$Adult<63.60825] <- '# Adults: Below average'
Analysis$Adulthood[Analysis$Adult>=63.60825 & Analysis$Adult<72.11816] <- '# Adults: Just below average'
Analysis$Adulthood[Analysis$Adult>=72.11816 & Analysis$Adult<77.97565] <- '# Adults: Just above average'
Analysis$Adulthood[Analysis$Adult>=77.97565] <- '# Adults: Above average'
Analysis$Adulthood <- as.factor(Analysis$Adulthood)

table(Analysis$Adulthood)
contrasts(Analysis$Adulthood)
Analysis$Adulthood <-relevel(Analysis$Adulthood, "# Adults: Just below average")

# 2. Clean data

names(Analysis)
Analysis <- Analysis[,c(1:3, 13, 15:27)]

# 3. Appendix

# A.1
CrimePerThousand <- ggplot(Analysis, aes(x=CrimePerThousand)) + geom_histogram(breaks=seq(0.0001296, 1.0660000, 0.005)) +
  theme_bw() + xlab("Crime Per 1000 citizens") + ylab("Frequency") +
  ggtitle("Crime Incidents")

LogCrimePerThousand <- ggplot(Analysis, aes(x=log(CrimePerThousand))) + geom_histogram(breaks=seq(-8.95100, 0.06375, 0.075)) +
  theme_bw() + xlab("Crime Per 1000 citizens") + ylab("Frequency") +
  ggtitle("Natural Logarithm of Crime Incidents")

#SharePoverty <- ggplot(Analysis, aes(x=factor(GEOID), y=share_poverty)) + stat_summary(fun.y="mean", geom="bar") +
  #geom_bar(stat="identity", colour="black",width=.8) +
  #theme_bw() + xlab("GEOID") + ylab("Share Poverty") +
  #ggtitle("Distribution of Poverty across Districts") +
  #theme(axis.text.x = element_text(angle=90))

# A.2
M1 <- lm(log(CrimePerThousand) ~ Established, data = Seattle.Crime.Analysis) 

CrimeAllBut201401 <- which(grepl("2014-01", Seattle.Crime.Analysis$Month))
CrimeAllBut201401 <- Seattle.Crime.Analysis[-CrimeAllBut201401, ]

M1.b <- lm(log(CrimePerThousand) ~ Established, data = CrimeAllBut201401)



# 4. Analysis

######################################################################################

ps.model1 <- glm(Established ~ log(CrimePerThousand), data = Analysis,family=binomial)
#summary(ps.model) # Pscores calculated by logit where outcome = binary treatment status
# covariates to be chosen that related to both treatment and potential outcomes
# Next calculate the pscores for each observations having a retailer
Analysis$pscore1 <- predict(ps.model1, newdata = Analysis, type = "response")

# Figure problem from last plot
par("mar")
par(mar=c(1,1,1,1))

# There seems to be a common support problem!
labs <- paste("Retailer in GEOID established:", c("Yes", "No"))
ComSup1 <- Analysis %>%
  mutate(Established = ifelse(Established == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pscore1)) +
  geom_histogram(color = "white", breaks=seq(0,1,by=0.025)) +
  facet_wrap(~Established) +
  xlab("Probability of having a Retailer in GEOID (univariate)") +
  theme_bw()

# range
range(Analysis$pscore1[Analysis$Established==0])
range(Analysis$pscore1[Analysis$Established==1])

# mod_match <- matchit(Established ~ CrimePerThousand, method = "nearest", data = Analysis)
Crime.pscore1 <- Analysis[Analysis$pscore1 >= 8.732437e-05 & Analysis$pscore1 <=4.631339e-01,]

M2 <- lm(log(CrimePerThousand) ~ Established, data = Analysis)
M3 <- lm(log(CrimePerThousand) ~ Established, data = Crime.pscore1)

######################################################################################


######################################################################################

ps.model2 <- glm(Established ~ log(CrimePerThousand) + share_poverty + Adulthood + EduCat + Diversity, data = Analysis,family=binomial)
#summary(ps.model) # Pscores calculated by logit where outcome = binary treatment status
# covariates to be chosen that related to both treatment and potential outcomes
# Next calculate the pscores for each observations having a retailer
Analysis$pscore2 <- predict(ps.model2, newdata = Analysis, type = "response")

# Figure problem from last plot
par("mar")
par(mar=c(1,1,1,1))

# There seems to be a common support problem!
labs <- paste("Retailer in GEOID established:", c("Yes", "No"))
ComSup2 <- Analysis %>%
  mutate(Established = ifelse(Established == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pscore2)) +
  geom_histogram(color = "white", breaks=seq(0,1,by=0.025)) +
  facet_wrap(~Established) +
  xlab("Probability of having a Retailer in GEOID (univariate)") +
  theme_bw()

# range
range(Analysis$pscore2[Analysis$Established==0])
summary(Analysis$pscore2[Analysis$Established==0]) # how to get data in quintiles?
range(Analysis$pscore2[Analysis$Established==1])

# mod_match <- matchit(Established ~ CrimePerThousand, method = "nearest", data = Analysis)
Crime.pscore2 <- Analysis[Analysis$pscore1 >= 0.04355676 & Analysis$pscore1 <=9.235078e-01,]

M4 <- lm(log(CrimePerThousand) ~ Established + share_poverty + Adulthood + EduCat + Diversity, data = Analysis)
M5 <- lm(log(CrimePerThousand) ~ Established + share_poverty + Adulthood + EduCat + Diversity, data = Crime.pscore2)
M5.b <- lm(log(CrimePerThousand) ~ share_poverty + Adulthood + EduCat + Diversity, data = Crime.pscore2)

#felm(log(CrimePerThousand)~Established + AgeCat + share_poverty + RaceCat | GEOID + Month, data = Analysis)
M6 <- plm(log(CrimePerThousand)~Established, data = Analysis, index=c("GEOID"), model="within")

######################################################################################

# OLS for each sub-category
M7.Alc <- lm(log(AlcCrime) ~ Established + share_poverty + Adulthood + EduCat + Diversity, data = Analysis)
M7.Bur <- lm(log(BurCrime) ~ Established + share_poverty + Adulthood + EduCat + Diversity, data = Analysis)
M7.Mar <- lm(log(MarCrime) ~ Established + share_poverty + Adulthood + EduCat + Diversity, data = Analysis)
M7.Narc <- lm(log(NarcCrime) ~ Established + share_poverty + Adulthood + EduCat + Diversity, data = Analysis)
M7.Other <- lm(log(OtherCrime) ~ Established + share_poverty + Adulthood + EduCat + Diversity, data = Analysis)
M7.Prop <- lm(log(PropCrime) ~ Established + share_poverty + Adulthood + EduCat + Diversity, data = Analysis)
M7.Viol <- lm(log(ViolCrime) ~ Established + share_poverty + Adulthood + EduCat + Diversity, data = Analysis)

tab.Alc <- xtable(M7.Alc, caption = "OLS Full Alcohol Crime")
tab.Bur <- xtable(M7.Bur, caption = "OLS Full Burglary/Theft")
tab.Mar <- xtable(M7.Mar, caption = "OLS Full Marijuana Crime")
tab.Narc <- xtable(M7.Narc, caption = "OLS Full Narcotics Crime")
tab.Other <- xtable(M7.Other, caption = "OLS Full Other-related Crime")
tab.Prop <- xtable(M7.Prop, caption = "OLS Full Property Crime")
tab.Viol <- xtable(M7.Viol, caption = "OLS Full Violent Crime")

# FE for each sub-category
M8.Alc <- plm(log(AlcCrime)~Established, data = Analysis, index=c("GEOID"), model="within")
M8.Bur <- plm(log(BurCrime)~Established, data = Analysis, index=c("GEOID"), model="within")
M8.Narc <- plm(log(NarcCrime)~Established, data = Analysis, index=c("GEOID"), model="within")
M8.Other <- plm(log(OtherCrime)~Established, data = Analysis, index=c("GEOID"), model="within")
M8.Prop <- plm(log(PropCrime)~Established, data = Analysis, index=c("GEOID"), model="within")
M8.Viol <- plm(log(ViolCrime)~Established, data = Analysis, index=c("GEOID"), model="within")