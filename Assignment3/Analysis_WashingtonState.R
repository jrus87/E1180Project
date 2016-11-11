# Setting our working directory
wrkdir <- c('C:/Users/Benji/Desktop/Statistics/Git/Repositories/E1180Project', 
            '~/Hertie School/Fall 2016/CollaborativeSocialScienceDataAnalysis/CSSR')
repmis::set_valid_wd(wrkdir)
rm(wrkdir)

source("./Codes/Cleaning_USCensusData.R")

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
Year <- as.character(c(2014:2016))
WA.numbers <- c(ProdProc2014.uniq, ProdProc2015.uniq, ProdProc2016.uniq,
                     Retailer2014.uniq, Retailer2015.uniq, Retailer2016.uniq)
WA.name <- c("Producer/Processor", "Producer/Processor", "Producer/Processor",
                   "Retailer", "Retailer", "Retailer")
WA.bar <- data.frame(Year, WA.numbers, WA.name, stringsAsFactors = FALSE)


washington.barplot <- ggplot(WA.bar, aes(x=Year, y=WA.numbers, fill=WA.name)) +
  geom_bar(stat="identity", colour="black",width=.8, position=position_dodge()) + 
  theme_bw() + xlab("Year") + ylab("Frequency") + scale_fill_hue(name="Form of business") +
  ggtitle("Number of Producers and Processors in WA")
washington.barplot

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
Year <- as.character(c(2014:2016))
WA.numbers.e <- c(Sales2014, Sales2015, Sales2016,
                ExciseTax2014, ExciseTax2015, ExciseTax2016)
WA.name.e <- c("Sales", "Sales", "Sales",
             "Excise Tax", "Excise Tax", "Excise Tax")
WA.bar.e <- data.frame(Year, WA.numbers.e, WA.name.e, stringsAsFactors = FALSE)


washington.barplot.e <- ggplot(WA.bar.e, aes(x=Year, y=WA.numbers.e, fill=WA.name.e)) +
  geom_bar(stat="identity", colour="black",width=.8, position=position_dodge()) + 
  theme_bw() + xlab("Year") + ylab("Amount in Mio. US-Dollars") +
  ggtitle("Cannabis related Sales and Tax Revenues in WA")
washington.barplot.e

# Growth pattern:

# Increase of Sales Revenue: 
# 2015: increase by 960 % (from 29.21 to 309.86)
# 2016: increase by 77 % (from 309.86 to 549.93)

# Increase of Tax Revenue:
# 2015: increase by 1040 % (from 9.74 to 110.97)
# 2016: increase by 83 % (from 110.97 to 203.47)

rm(Sales2014, Sales2015, Sales2016, ExciseTax2014, ExciseTax2015, ExciseTax2016,
   WA.bar.e, WA.name.e, WA.numbers.e)

# 3. Create a map only for Narcotics and Marijuana related drugs
Liquour <- which(grepl("LIQUOR", Seattle.Merged$Event.Clearance.Description))
DUI <- which(grepl("DUI", Seattle.Merged$Event.Clearance.Description))
Only.Narcotics <- Seattle.Merged[-c(Liquour, DUI),]

map.seattle_city <- qmap("seattle", zoom = 11, source="stamen", maptype="toner",darken = c(.3,"#BBBBBB"))
map.seattle_city +
  geom_polygon(data=Seattle.SOE, aes(x=lon,y=lat, group=group, fill=percent, color=muted("blue"), alpha=0.9)) +
  theme_nothing(legend=FALSE) +
  scale_fill_gradient2(breaks=pretty_breaks(n=5), guide=guide_legend(title="Percent living > $200k", keywidth = 2)) +
  geom_point(data=Only.Narcotics, aes(x=lon.Narc, y=lat.Narc), color="dark green", alpha=0.03, size=1.1) +
  geom_point(data=Seattle.Merged, aes(x=lon.Ret, y=lat.Ret), color="dark red", size=2) +
  labs(title = "Narcotic Crime in Seattle")

rm(Liquour, DUI, Only.Narcotics)

# 4. Crime development in Seattle
# Dissect Crime into three categories
Liquour <- which(grepl("LIQUOR", Seattle.Merged$Event.Clearance.Description))
DUI <- which(grepl("DUI", Seattle.Merged$Event.Clearance.Description))

Alcohol <- Seattle.Merged[c(Liquour, DUI),]
Only.Narcotics <- Seattle.Merged[-c(Liquour, DUI),]

Marijuana <- which(grepl("MARIJUANA", Only.Narcotics$Event.Clearance.Description))
Weed <- Seattle.Merged[Marijuana,]

# Clean categories before merging into data frame
AlcoholCrimePerMonth <- as.data.frame(table(Alcohol$Month.Narc))
which(AlcoholCrimePerMonth$Var1=="2014-01") # observation 41
AlcoholCrimePerMonth <- AlcoholCrimePerMonth[-c(1:40),2]
AlcoholCrimePerMonth <- as.character(AlcoholCrimePerMonth)

Only.NarcoticsCrimePerMonth <- as.data.frame(table(Only.Narcotics$Month.Narc))
which(Only.NarcoticsCrimePerMonth$Var1=="2014-01") # observation 38
Only.NarcoticsCrimePerMonth <- Only.NarcoticsCrimePerMonth[-c(1:37),2]
Only.NarcoticsCrimePerMonth <- as.character(Only.NarcoticsCrimePerMonth)

WeedCrimePerMonth <- as.data.frame(table(Weed$Month.Narc))
which(WeedCrimePerMonth$Var1=="2014-01") # observation 33
WeedCrimePerMonth <- WeedCrimePerMonth[-c(1:32),2]
WeedCrimePerMonth <- as.character(WeedCrimePerMonth)

Month <- unique(Seattle.Merged$Month.Narc)
Month <- Month[order(Month)]
Month <- as.character(na.omit(Month))
which(Month=="2014-01") # observation 42
Month <- Month[-c(1:41)]

AlcGroup <- c("AlcGroup")
NarcGroup <- c("NarcGroup")
WeedGroup <- c("WeedGroup")

# Merge data frame
Crime.lines <- data.frame(AlcoholCrimePerMonth, Only.NarcoticsCrimePerMonth, WeedCrimePerMonth, 
                          Month, AlcGroup, NarcGroup, WeedGroup, stringsAsFactors = FALSE)
Crime.lines$AlcoholCrimePerMonth <- as.numeric(Crime.lines$AlcoholCrimePerMonth)
Crime.lines$Only.NarcoticsCrimePerMonth <- as.numeric(Crime.lines$Only.NarcoticsCrimePerMonth)
Crime.lines$WeedCrimePerMonth <- as.numeric(Crime.lines$WeedCrimePerMonth)

Crime.lines$AlcoholCrimePerMonth <- log(Crime.lines$AlcoholCrimePerMonth)
Crime.lines$Only.NarcoticsCrimePerMonth <- log(Crime.lines$Only.NarcoticsCrimePerMonth)
Crime.lines$WeedCrimePerMonth <- log(Crime.lines$WeedCrimePerMonth)

# Create line graph
crime.graph <- ggplot(Crime.lines, aes(Month)) +
  geom_line(aes(y = AlcoholCrimePerMonth, colour ="AlcoholCrimePerMonth", group=AlcGroup)) +
  geom_line(aes(y = Only.NarcoticsCrimePerMonth, colour ="Only.NarcoticsCrimePerMonth", group=NarcGroup)) +
  geom_line(aes(y = WeedCrimePerMonth, colour = "WeedCrimePerMonth", group=WeedGroup)) +
  scale_colour_manual(name="Crime category",
                      labels=c("Alcohol", "Narcotics", "Weed"),
                      breaks=c("AlcoholCrimePerMonth", "Only.NarcoticsCrimePerMonth", "WeedCrimePerMonth"),
                      values=c("orange", "black", "green")) +
  xlab("Months") + ylab("Natural logarithm of Crime") + 
  labs(title="Crime development in Seattle, WA") + theme_bw() +
  theme(axis.text.x = element_text(angle=90))
crime.graph

rm(AlcoholCrimePerMonth, AlcGroup, Alcohol, Only.NarcoticsCrimePerMonth, Only.Narcotics, WeedCrimePerMonth, WeedGroup, Weed,
   NarcGroup, AlocholCrimePerMonth, DUI, Group, Liquour, Marijuana, Month, Year)

# 5. Univariate regression
Marijuana <- which(grepl("MARIJUANA", Seattle.Merged$Event.Clearance.Description))
Seattle.Merged$WeedCrime <- NA
Seattle.Merged$WeedCrime[-Marijuana] <- 0
Seattle.Merged$WeedCrime[Marijuana] <- 1

Seattle.Merged$OtherCrime <- NA
Seattle.Merged$OtherCrime[-Marijuana] <- 1
Seattle.Merged$OtherCrime[Marijuana] <- 0

fit <- lm(WeedCrime ~ Est, data=Seattle.Merged)
summary(fit)

fit2 <- lm(OtherCrime ~ Est, data=Seattle.Merged)
summary(fit2)
