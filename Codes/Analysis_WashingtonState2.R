# Setting our working directory
wrkdir <- c('C:/Users/Benji/Desktop/Statistics/Git/Repositories/E1180Project', 
            '~/Hertie School/Fall 2016/CollaborativeSocialScienceDataAnalysis/CSSR')
repmis::set_valid_wd(wrkdir)

# Installing packages
packages <- c('dplyr', 'repmis', 'fields', 'cowplot', 'ggmap', 'ggplot2', 'stargazer')
for (p in packages) {
  if (p %in% installed.packages()) require(p, character.only=TRUE) 
  else {
    install.packages(p)
    require(p, character.only=TRUE)
  }
}

# Creating a BibTex file
repmis::LoadandCite(packages, file = './BibTex/RpackageCitations_Analysis.bib')

# Removing from global environment for better readability
rm(packages, p, wrkdir)

# Load Data # more efficient than source??? do i need soe analysis
source("./Codes/Third Step - Plot Preparation/PlotsPreparation.R") # Analysis_Washington State has to be renamed
Seattle.Crime.Analysis <- read.table(gzfile("./Data/SeattleCrimeAnalysis.gz"),row.names=1)
Seattle.SOE.Map <- read.table(gzfile("./Data/SeattleMap.gz"),row.names=1)

###
# Descriptive Statistics
###

# Barplot for both frequency count of companies and their revenues 

cowplot::plot_grid(washington.barplot, washington.barplot.e, labels=c("",""))

# Growth pattern:

# Number of Producers and businesses:                 # Increase of Sales Revenue: 
# 2015: increase by 210 % (from 185 to 574)           # 2015: increase by 960 % (from 29.21 to 309.86)
# 2016: increase by 53 % (from 574 to 881)            # 2016: increase by 77 % (from 309.86 to 549.93)

# Number of Retailers:                                # Increase of Tax Revenue:
# 2015: increase by 130 % (from 82 to 189)            # 2015: increase by 1040 % (from 9.74 to 110.97)
# 2016: increase by 69 % (from 189 to 319)            # 2016: increase by 83 % (from 110.97 to 203.47)

# Map   

  # Map 1 represents all crime for October 2016 as well as our Retailer positions
Map1 <- qmap("seattle", zoom = 11, source="google", maptype="roadmap")

Map1 + geom_polygon(data=Seattle.SOE.Map, aes(x=lon, y=lat, group=GEOID, fill=populationTotal, color="lightcyan"), alpha=0.3) +
   scale_fill_gradient2(breaks=pretty_breaks(n=5), guide=guide_legend(title="Population Density", keywidth = 2)) +
   geom_point(data=CrimeOct2016, aes(x=lon, y=lat, colour = factor(Crime)), alpha=0.25, size=1.1) +
   geom_point(data=Seattle.Crime.Analysis, aes(x=lon.Ret, y=lat.Ret), color="dark red", size=2) +
   labs(colour = "Crime Categories")

# Line graph

crime.graph <- ggplot(Crime.lines, aes(Month, CrimePerThousand, group=Crime, colour = factor(Crime))) 
crime.graph + geom_line() +
  xlab("Months") + ylab("Crime frequency per 1000 inhabitants") + theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  labs(colour = "Crime Categories") +
  ggtitle("Crime rates development in Seattle")

crime.graph.log <- ggplot(Crime.lines, aes(Month, log, group=Crime, colour = factor(Crime))) 
crime.graph.log + geom_line() +
  xlab("Months") + ylab("Natural logarithm of Crime Rate per 1000 inhabitants") + theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  labs(colour = "Crime Categories") +
  ggtitle("Crime rates development in Seattle")

###
# Univariate Inferential Statistics - using log here because we are interested in increase, not absolute value
###

# 1. Simple t-test analysis

t.test(log(Seattle.Crime.Analysis$CrimePerThousand)~Seattle.Crime.Analysis$Established) # statistically significant

  # Put into graph
M1 <- ggplot(Seattle.Crime.Analysis, aes(factor(Established), log(CrimePerThousand)))
M1 + geom_boxplot(aes(fill=Established)) +
  xlab("Established") + ylab("Crime per 1000 inhabitants")  +
  ggtitle("Boxplot of Crime Development") +
  theme(legend.position='none')

# 2. Subset 2014-01 away # explanation see crime.graph.log (apparently 2014-01 seems to have had coding issues)

CrimeAllBut201401 <- which(grepl("2014-01", Seattle.Crime.Analysis$Month))
CrimeAllBut201401 <- Seattle.Crime.Analysis[-CrimeAllBut201401, ]

t.test(log(CrimeAllBut201401$CrimePerThousand)~CrimeAllBut201401$Established) # almost no difference - do not pursue

# 4. by specific crime category => maybe make several barplots next to each other
names(Seattle.Crime.Analysis)

t.test(log(Seattle.Crime.Analysis$AlcCrime)~Seattle.Crime.Analysis$Established) # significant but almost irrelevant
t.test(log(Seattle.Crime.Analysis$BurCrime)~Seattle.Crime.Analysis$Established) # significant and substantial
t.test(log(Seattle.Crime.Analysis$MarCrime)~Seattle.Crime.Analysis$Established) # significant and counter-intuitive?
t.test(log(Seattle.Crime.Analysis$NarcCrime)~Seattle.Crime.Analysis$Established) # insignificant
t.test(log(Seattle.Crime.Analysis$OtherCrime)~Seattle.Crime.Analysis$Established) # signficant and negative
t.test(log(Seattle.Crime.Analysis$PropCrime)~Seattle.Crime.Analysis$Established) # significant and substantial
t.test(log(Seattle.Crime.Analysis$ViolCrime)~Seattle.Crime.Analysis$Established) # significant and substantial

# 5. MatchIt


###
# Multivariate Inferential Statistics - using log here because we are interested in increase, not absolute value
###

# MatchIt -> propensity scores
library(MatchIt)
names(Seattle.Crime.Analysis)
subset <- aggregate(cbind(CrimePerThousand) ~ GEOID + Established + Month + , data=Seattle.Crime.Analysis, FUN=function(x) mean(range(x)))

ps.model <- glm(Established ~ log(CrimePerThousand), data = Seattle.Crime.Analysis,
                family=binomial(link="logit"),
                na.action=na.omit)
summary(ps.model)

Seattle.Crime.Analysis$pscore <- predict(ps.model, newdata = Seattle.Crime.Analysis, type = "response")
hist(Seattle.Crime.Analysis$pscore)
summary(Seattle.Crime.Analysis$pscore)

# restrict data to ps range .10 <= ps <= .90

Crime.pscore <- Seattle.Crime.Analysis[Seattle.Crime.Analysis$pscore >= .10 & Seattle.Crime.Analysis$pscore <=.90,]
summary(Crime.pscore$pscore)

# regression with controls on propensity score screened data set
summary(lm(log(CrimePerThousand)~Established + AgeCat + GEOID, data = Crime.pscore))

# unrestricted regression with controls
summary(lm(log(CrimePerThousand)~Established + AgeCat + GEOID, data = Seattle.Crime.Analysis))







rm(washington.barplot, washington.barplot.e, Map1, crime.graph, crime.graph.log)

