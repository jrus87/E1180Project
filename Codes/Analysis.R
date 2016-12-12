# Setting our working directory
wrkdir <- c('C:/Users/Benji/Desktop/Statistics/Git/Repositories/E1180Project', 
            '~/Hertie School/Fall 2016/CollaborativeSocialScienceDataAnalysis/CSSR')
repmis::set_valid_wd(wrkdir)

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

# Load Data # more efficient than source??? do i need soe analysis
source("./Codes/Third-Step - Plot Preparation/PlotsPreparation.R") 


names(Seattle.Crime.Analysis)
Analysis <- Seattle.Crime.Analysis[,c(1:3,16,28:29,30:33)]
which(is.na(Analysis))

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

Map2 <- qmap("seattle", zoom = 12, source="google", maptype="roadmap")
Map2 <- Map2 + scale_fill_gradient2(breaks=pretty_breaks(n=5), guide=guide_legend(title="Population Density", keywidth = 2)) +
  geom_point(data=CrimeOct2016, aes(x=lon, y=lat, colour = factor(Crime)), alpha=0.75, size=1.1) +
  geom_point(data=Seattle.Crime.Analysis, aes(x=lon.Ret, y=lat.Ret), color="dark red", size=3) +
  labs(colour = "Crime Categories Oct 2016") + guides(colour = guide_legend(override.aes = list(alpha = 1)))

Map3 <- qmap("seattle", zoom = 13, source="google", maptype="roadmap")
Map3 <- Map3 + scale_fill_gradient2(breaks=pretty_breaks(n=5), guide=guide_legend(title="Population Density", keywidth = 2)) +
  geom_point(data=CrimeOct2016, aes(x=lon, y=lat, colour = factor(Crime)), alpha=0.75, size=1.1) +
  geom_point(data=Seattle.Crime.Analysis, aes(x=lon.Ret, y=lat.Ret), color="dark red", size=3) +
  labs(colour = "Crime Categories Oct 2016") + guides(colour = guide_legend(override.aes = list(alpha = 1)))

# Line graph

crime.graph <- ggplot(Crime.lines, aes(Month, CrimePerThousand, group=Crime, colour = factor(Crime))) 
crime.graph <- crime.graph + geom_line() +
  xlab("Months") + ylab("Crime frequency per 1000 inhabitants") + theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  labs(colour = "Crime Categories") +
  ggtitle("Crime rates development in Seattle")

crime.graph.log <- ggplot(Crime.lines, aes(Month, log, group=Crime, colour = factor(Crime))) 
crime.graph.log <- crime.graph.log + geom_line() +
  xlab("Months") + ylab("Natural logarithm of Crime Rate per 1000 inhabitants") + theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  labs(colour = "Crime Categories") +
  ggtitle("Crime rates development in Seattle")

cowplot::plot_grid(crime.graph, crime.graph.log, labels=c("",""))

###
# Univariate Inferential Statistics - using log here because we are interested in increase, not absolute value
###

# 1. Simple t-test analysis

t.test(log(Seattle.Crime.Analysis$CrimePerThousand)~Seattle.Crime.Analysis$Established) # statistically significant

M1 <- lm(log(CrimePerThousand) ~ Established, data = Seattle.Crime.Analysis)
summary(M1) # Intercept = mean of first group of t-test; Intercept + Point estimate = mean of second group of t-test
confint(M1)

#M1.b <- lm(log(CrimePerThousand) ~ Sales.Ret, data = Seattle.Crime.Analysis) 
#summary(M1.b)
#confint(M1.b)
# => the Sales (as a proxy of popularity) of a shop has a negligible effect and does not drive the relationship above

  # Put into graph
Box1 <- ggplot(Seattle.Crime.Analysis, aes(factor(Established), log(CrimePerThousand)))
Box1 <- Box1 + geom_boxplot(aes(fill=Established)) +
  xlab("Established") + ylab("Crime per 1000 inhabitants")  +
  ggtitle("Boxplot of Crime Development") +
  theme(legend.position='none')
Box1

# 1.c Subset 2014-01 away # explanation see crime.graph.log (apparently 2014-01 seems to have had coding issues)

#table(Seattle.Crime.Analysis$Crime, Seattle.Crime.Analysis$Month)

#CrimeAllBut201401 <- which(grepl("2014-01", Seattle.Crime.Analysis$Month))
#CrimeAllBut201401 <- Seattle.Crime.Analysis[-CrimeAllBut201401, ]

#t.test(log(CrimeAllBut201401$CrimePerThousand)~CrimeAllBut201401$Established) # almost no difference - do not pursue
#M1.c <- lm(log(CrimePerThousand) ~ Established, data = CrimeAllBut201401)
#summary(M1.c) 
#confint(M1.c) # the point are not statistically different from each other -> graph? 

#Box2 <- ggplot(CrimeAllBut201401, aes(factor(Established), log(CrimePerThousand)))
#Box2 <- Box2 + geom_boxplot(aes(fill=Established)) +
  #xlab("Established") + ylab("Crime per 1000 inhabitants")  +
  #ggtitle("Boxplot of Crime Development without Jan 2014") +
  #theme(legend.position='none')

#cowplot::plot_grid(Box1, Box2, labels=c("",""))

# 2. By specific crime category => maybe make several barplots next to each other
t.test(log(Seattle.Crime.Analysis$AlcCrime)~Seattle.Crime.Analysis$Established) # insignificant
t.test(log(Seattle.Crime.Analysis$BurCrime)~Seattle.Crime.Analysis$Established) # significant and substantial
t.test(log(Seattle.Crime.Analysis$MarCrime)~Seattle.Crime.Analysis$Established) # significant and counter-intuitive?
t.test(log(Seattle.Crime.Analysis$NarcCrime)~Seattle.Crime.Analysis$Established) # significant
t.test(log(Seattle.Crime.Analysis$OtherCrime)~Seattle.Crime.Analysis$Established) # signficant and negative
t.test(log(Seattle.Crime.Analysis$PropCrime)~Seattle.Crime.Analysis$Established) # significant and substantial
t.test(log(Seattle.Crime.Analysis$ViolCrime)~Seattle.Crime.Analysis$Established) # significant and substantial

p <- cowplot::plot_grid(Box3, Box4, Box5, Box6, Box7, Box8, Box9, labels=c(""))
title <- ggdraw() + draw_label("Development of Crime per 1000 citizens", fontface="bold")
cowplot::plot_grid(title, p, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

# 4. MatchIt (http://stanford.edu/~ejdemyr/r-tutorials-archive/tutorial8.html)
ps.model <- glm(Established ~ log(CrimePerThousand), data = Seattle.Crime.Analysis,
                family=binomial,na.action=na.omit)
summary(ps.model) # Pscores calculated by logit where outcome = binary treatment status
# covariates to be chosen that related to both treatment and potential outcomes

# Next calculate the pscores for each observations having a retailer
Seattle.Crime.Analysis$pscore <- predict(ps.model, newdata = Seattle.Crime.Analysis, type = "response")

# Figure problem from last plot
par("mar")
par(mar=c(1,1,1,1))

# There seems to be a common support problem!
labs <- paste("Retailer in GEOID established:", c("Yes", "No"))
Seattle.Crime.Analysis %>%
  mutate(Established = ifelse(Established == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pscore)) +
  geom_histogram(color = "white", breaks=seq(0,1,by=0.025)) +
  facet_wrap(~Established) +
  xlab("Probability of having a Retailer in GEOID") +
  theme_bw()

# ranges of pscore for D = 0 and D = 1
range(Seattle.Crime.Analysis$pscore[Seattle.Crime.Analysis$Established==0])
range(Seattle.Crime.Analysis$pscore[Seattle.Crime.Analysis$Established==1])

# mod_match <- matchit(Established ~ CrimePerThousand, method = "nearest", data = Seattle.Crime.Analysis)
Crime.pscore <- Seattle.Crime.Analysis[Seattle.Crime.Analysis$pscore >= 0.0009535593 & Seattle.Crime.Analysis$pscore <=0.4070721037,]
summary(Crime.pscore$pscore)

# t-test and regression with controls on propensity score screened data set
t.test(log(CrimePerThousand) ~ Established, data = Crime.pscore)

M2 <- lm(log(CrimePerThousand) ~ Established, data = Crime.pscore)
summary(M2)
# compare to unrestricted regerssion
summary(M1) # the effect of Established more than halfs if we take similar units and compare them

# visualise resutls
Box_treat1 <- ggplot(Crime.pscore, aes(factor(Established), log(CrimePerThousand)))
Box_treat1 <- Box_treat1 + geom_boxplot(aes(fill=Established)) +
  xlab("Established") + ylab("Natural logarithm of Crime")  +
  theme(legend.position='none') +
  ggtitle("Boxplot of Crime Development of similar districts") 
Box_treat1

cowplot::plot_grid(Box1, Box_treat1, labels=c("",""))

rm(j, k, Crime.lines, Crime.pscore, CrimeAllBut201401, CrimeOct2016, KingCounty.ProdProc, KingCounty.Retailer, ProdProc,
   Retailer, Seattle.SOE.Map2, Box_treat1, Box1, Box2, Box3, Box4, Box5, Box6, Box7, Box8, Box9, crime.graph, crime.graph.log,
   labs, lm_treat1, Map1, Map2, Map3, nas, numeric_vector, numeric_vector2, numueric_vector, od, p, ps.model, title, url,
   washington.barplot, washington.barplot.e)

###
# Multivariate Inferential Statistics - using log here because we are interested in increase, not absolute value
###

# 1. Multivariate regression without pscore
str(Seattle.Crime.Analysis)

M3 <- lm(log(CrimePerThousand) ~ Established + share_poverty + AgeCat + RaceCat, data = Seattle.Crime.Analysis)
summary(M3) # when accounting for socio-economic variables, crime seems to be more affected than before (R² minimally better)
confint(M3) # but standard error increases
# compare
summary(M1) # Intercept = mean of first group of t-test; Intercept + Point estimate = mean of second group of t-test
confint(M1)

# 2. By specific crime category => maybe make several barplots next to each other
M3.Alc <- lm(log(AlcCrime) ~ Established + share_poverty + AgeCat + RaceCat, data = Seattle.Crime.Analysis)
M3.Bur <- lm(log(BurCrime) ~ Established + share_poverty + AgeCat + RaceCat, data = Seattle.Crime.Analysis)
M3.Mar <- lm(log(MarCrime) ~ Established + share_poverty + AgeCat + RaceCat, data = Seattle.Crime.Analysis)
M3.Narc <- lm(log(NarcCrime) ~ Established + share_poverty + AgeCat + RaceCat, data = Seattle.Crime.Analysis)
M3.Other <- lm(log(OtherCrime) ~ Established + share_poverty + AgeCat + RaceCat, data = Seattle.Crime.Analysis)
M3.Prop <- lm(log(PropCrime) ~ Established + share_poverty + AgeCat + RaceCat, data = Seattle.Crime.Analysis)
M3.Viol <- lm(log(ViolCrime) ~ Established + share_poverty + AgeCat + RaceCat, data = Seattle.Crime.Analysis)

# Significant in all
summary(M3.Alc)
confint(M3.Alc)

summary(M3.Bur)
confint(M3.Bur)

summary(M3.Mar)
confint(M3.Mar)

summary(M3.Narc)
confint(M3.Narc)

summary(M3.Other)
confint(M3.Other)

summary(M3.Prop)
confint(M3.Prop)

summary(M3.Viol)
confint(M3.Viol)

# 3. MatchIt (http://stanford.edu/~ejdemyr/r-tutorials-archive/tutorial8.html)
ps.model <- glm(Established ~ log(CrimePerThousand) + share_poverty + AgeCat + RaceCat, data = Seattle.Crime.Analysis,
                family=binomial,na.action=na.omit)
summary(ps.model) # Pscores calculated by logit where outcome = binary treatment status
# covariates to be chosen that related to both treatment and potential outcomes

# Next calculate the pscores for each observations having a retailer
Seattle.Crime.Analysis$pscore <- predict(ps.model, newdata = Seattle.Crime.Analysis, type = "response")

# Figure problem from last plot
par("mar")
par(mar=c(1,1,1,1))

# There seems to be a common support problem!
labs <- paste("Retailer in GEOID established:", c("Yes", "No"))
Seattle.Crime.Analysis %>%
  mutate(Established = ifelse(Established == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pscore)) +
  geom_histogram(color = "white", breaks=seq(0,1,by=0.025)) +
  facet_wrap(~Established) +
  xlab("Probability of having a Retailer in GEOID") +
  theme_bw()

# ranges of pscore for D = 0 and D = 1
range(Seattle.Crime.Analysis$pscore[Seattle.Crime.Analysis$Established==0])
range(Seattle.Crime.Analysis$pscore[Seattle.Crime.Analysis$Established==1])

# mod_match <- matchit(Established ~ CrimePerThousand, method = "nearest", data = Seattle.Crime.Analysis)
Crime.pscore <- Seattle.Crime.Analysis[Seattle.Crime.Analysis$pscore >= 0.0003133269 & Seattle.Crime.Analysis$pscore <= 0.7891571571,]
summary(Crime.pscore$pscore)

# regression with controls on propensity score screened data set
M4 <- lm(log(CrimePerThousand) ~ Established + share_poverty + AgeCat + RaceCat, data = Crime.pscore)
summary(M4)
# compare to unrestricted regerssion
summary(M3) # the effect of Established more halfs if we take similar units and compare them; drop in R² more substantial


# 4. analysis by Month and GEOID

M5 <- lm(log(CrimePerThousand) ~ Established + Month + share_poverty + AgeCat + RaceCat, data = Seattle.Crime.Analysis)
summary(M5)
confint(M5) # once accounting for month, the estimate is 0.06 lower 
confint(M3)

M5.b <- lm(log(CrimePerThousand) ~ Established * Month + share_poverty + AgeCat + RaceCat, data = Seattle.Crime.Analysis)
summary(M5.b)
confint(M5.b) # once accounting for month, the estimate is 0.85 higher 
confint(M3)


M6 <- lm(log(CrimePerThousand) ~ Established + GEOID + share_poverty + AgeCat + RaceCat, data = Seattle.Crime.Analysis)
summary(M6)
confint(M6) # once accounting for month, the estimate is 0.063 lower 
confint(M3)

M6.b <- lm(log(CrimePerThousand) ~ Established * GEOID + share_poverty + AgeCat + RaceCat, data = Seattle.Crime.Analysis)
summary(M6.b)
confint(M6.b) # once accounting for month, the estimate is 0.4 lower 
confint(M3)


M7 <- lm(log(CrimePerThousand) ~ Established + GEOID + Month + share_poverty + AgeCat + RaceCat, data = Seattle.Crime.Analysis)
summary(M7)
head(confint(M7)) # once accounting for month, the estimate is 0.075 lower + high R²
head(confint(M3))

#M7.b <- lm(log(CrimePerThousand) ~ Established + GEOID * Month + share_poverty + AgeCat + RaceCat, data = Seattle.Crime.Analysis)
#head(M7.b$effects)
#head(confint(M7.b)) # once accounting for month, the estimate is 0.4 lower 
#head(confint(M3))

rm(central, Crime.pscore, CrimeAllBut201401, drawn, drawn_sim, mp_coef, mp_vcov, est_range, labs,
   M7.b, pov_range, ps.model)



# 8. MatchIt (http://stanford.edu/~ejdemyr/r-tutorials-archive/tutorial8.html)
ps.model <- glm(Established ~ log(CrimePerThousand) + share_poverty + AgeCat + RaceCat + Month, data = Seattle.Crime.Analysis,
                family=binomial,na.action=na.omit)
summary(ps.model) # Pscores calculated by logit where outcome = binary treatment status
# covariates to be chosen that related to both treatment and potential outcomes

# Next calculate the pscores for each observations having a retailer
Seattle.Crime.Analysis$pscore <- predict(ps.model, newdata = Seattle.Crime.Analysis, type = "response")

# Figure problem from last plot
par("mar")
par(mar=c(1,1,1,1))

# There seems to be a common support problem!
labs <- paste("Retailer in GEOID established:", c("Yes", "No"))
Seattle.Crime.Analysis %>%
  mutate(Established = ifelse(Established == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pscore)) +
  geom_histogram(color = "white", breaks=seq(0,1,by=0.025)) +
  facet_wrap(~Established) +
  xlab("Probability of having a Retailer in GEOID") +
  theme_bw()

# ranges of pscore for D = 0 and D = 1
range(Seattle.Crime.Analysis$pscore[Seattle.Crime.Analysis$Established==0])
range(Seattle.Crime.Analysis$pscore[Seattle.Crime.Analysis$Established==1])

# mod_match <- matchit(Established ~ CrimePerThousand, method = "nearest", data = Seattle.Crime.Analysis)
Crime.pscore <- Seattle.Crime.Analysis[Seattle.Crime.Analysis$pscore >= 0.0003133269 & Seattle.Crime.Analysis$pscore <= 6.732709e-01,]
summary(Crime.pscore$pscore)

M8 <- lm(log(CrimePerThousand) ~ Established + share_poverty + AgeCat + RaceCat + Month, data = Crime.pscore)
summary(M8)
# compare to unrestricted regerssion
summary(M5)

# 9. MatchIt (http://stanford.edu/~ejdemyr/r-tutorials-archive/tutorial8.html)
ps.model <- glm(Established ~ log(CrimePerThousand) + share_poverty + AgeCat + RaceCat + GEOID, data = Seattle.Crime.Analysis,
                family=binomial,na.action=na.omit)
summary(ps.model) # Pscores calculated by logit where outcome = binary treatment status
# covariates to be chosen that related to both treatment and potential outcomes

# Next calculate the pscores for each observations having a retailer
Seattle.Crime.Analysis$pscore <- predict(ps.model, newdata = Seattle.Crime.Analysis, type = "response")

# Figure problem from last plot
par("mar")
par(mar=c(1,1,1,1))

# There seems to be a common support problem!
labs <- paste("Retailer in GEOID established:", c("Yes", "No"))
Seattle.Crime.Analysis %>%
  mutate(Established = ifelse(Established == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pscore)) +
  geom_histogram(color = "white", breaks=seq(0,1,by=0.025)) +
  facet_wrap(~Established) +
  xlab("Probability of having a Retailer in GEOID") +
  theme_bw()

# ranges of pscore for D = 0 and D = 1
range(Seattle.Crime.Analysis$pscore[Seattle.Crime.Analysis$Established==0])
range(Seattle.Crime.Analysis$pscore[Seattle.Crime.Analysis$Established==1])

# mod_match <- matchit(Established ~ CrimePerThousand, method = "nearest", data = Seattle.Crime.Analysis)
Crime.pscore <- Seattle.Crime.Analysis[Seattle.Crime.Analysis$pscore >= 0.0975710 & Seattle.Crime.Analysis$pscore <= 6.732709e-01,]
summary(Crime.pscore$pscore)

M9 <- lm(log(CrimePerThousand) ~ Established + share_poverty + AgeCat + RaceCat + GEOID, data = Crime.pscore)
summary(M9)
head(M9$terms)
# compare to unrestricted regerssion
summary(M6)




