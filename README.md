# Second Pair Assignment by Ben Gaiser & Jeremy Russell
28 October 2016

## Overview
This is the repository that [Jeremy](https://github.com/jrus87) and [Ben](https://github.com/BenjaminGaiser) are using for the assingments for [MPP-E1180: Introduction to Collaborative Social Science Data Analysis](https://github.com/HertieDataScience) at the Hertie School of Governance. We welcome any feedback or comments, and you can get in touch with us via our GitHub accounts or by submitting a pull request. 

## Repository
This repository contains the R code for webscraping, cleaning and analyzing the data we found. Currently we have data on marijuana production and distribution for the state of Washington, and crime and marijuana stats for Seattle and Denver. We also plan to supplement this data with U.S. census data on socio-economic factors per district/county as well.

## Assignment
The [second assignment](https://github.com/HertieDataScience/SyllabusAndLectures/blob/master/README.md) states the following:
>The second pair assignment is a proposal for your Collaborative Research Project. It is an opportunity for you to layout your collaborative research paper question, justify why it is interesting, provide basic literature review (properly cited using BibTeX), and identify data sources/methodologies that you can access to help answer your question. You will also demonstrate your understanding of literate programming technologies. Deadline 28 October, 2,000 words maximum, 10% of final grade.

## Research 
For the assignment, we briefly outline our research question, reference relevant literature on the topic, and describe our data and research methodology.

Our research concerns the effects of marijuana legalization in two U.S. states. In 2012, Colorado and Washington legalized recreational marijuana use; and in 2014 Oregon and Alaska followed suit. This November, five other states (Arizona, California, Maine, Massachusetts and Nevada) will consider legalizing the recreational use of marijuana, while four others (Arkansas, Florida, Montana and North Dakota) will decide on medical marijuana initiatives. Opponents of legalization claim legalization leads to an increase in marijuana and other drug use, "increases crime, diminishes traffic safety, harms public health, and lowers teen educational achievement," while advocates "think legalization reduces crime, raises tax revenue, lowers criminal justice expenditures, improves public health, bolsters traffic safety, and stimulates the economy" (http://www.cato.org/publications/policy-analysis/dose-reality-effect-state-marijuana-legalizations). As researchers have pointed out though, until now these claims have largely gone unexamined. This recent study concludes that evidence for either side's arguments is lacking, and "the absence of significant adverse consequences is especially striking given the somtimes dire predictions made by legalization opponents" (ibid). We would like to test the hypothesis that legalization does not significantly increase crime or other drug usage. We will look more closely at the two state capitals Seattle and Denver to test this.

Given the general lack of reliable, empirical research on the topic in relation to Washington and Colorado especially, we draw on the results of the Cato Institute's report as it is both recent and highly comparative in nature. Thus, we hope to contribute to the growing body of research on marijuana legalization and policy outcomes. Most research seems to run in a more medical vein. Authors writing for the RAND Corporation argue that it is too soon to adequately address the repurcusions of legalization; or more accurately, that since more data is needed, policy makers must acknowledge the need to work flexibly with issues at hand since what evidence we have is remarkably varying based on which country, state or metro region we analyze (http://www.rand.org/pubs/perspectives/PE149.html). Our contribution will be to look at initial data to inform stakeholders in this nascent field of policy.

Our research begins by attempting to validate the findings of a recent publication by the Cato Institute concerning the legalization of marijuana in Washington and Colorado. The topic is both interesting and relevant given the fact that next month nine other states will decide upon legalization of marijuana for recreational and medicinal purposes. We use statewide data on producers and retailers in the state of Washington and on crime rates and policing in the city of Seattle to reproduce the outcomes of the Cato study and compare the findings with those for Denver.

We use two main sets of data sources: the first is data on recreational cannabis use for both Seattle and Denver, e.g. the ['Washington Initiative 502']('http://www.502data.com'), the initiative to legalize recreational use; and the second is a set of .csv files on crime, e.g. from the city of [Denver's Open Data Catalog]('https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-crime'). The former lists the names and locations of producers/processors in the state and of retailers including YTD sales and tax revenue (at least for Washington State). Since the 502data site is scripted in Java, we use the phantomjs() function in the RSelenium package, which does not work on a Mac. For Denver's data, we downloaded the .csv file and used the readr package to load the data.