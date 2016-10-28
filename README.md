# Second Pair Assignment by Ben Gaiser & Jeremy Russell
28 October 2016

## Overview
This is the repository that [Jeremy](https://github.com/jrus87) and [Ben](https://github.com/BenjaminGaiser) are using for the assingments for [MPP-E1180: Introduction to Collaborative Social Science Data Analysis](https://github.com/HertieDataScience) at the Hertie School of Governance. We welcome any feedback or comments, and you can get in touch with us via our GitHub accounts or by submitting a pull request. 

## Repository
This repository contains the R code for webscraping, cleaning and analyzing the data we found. Currently we have data on marijuana production and distribution for the state of Washington, crime and marijuana stats for Seattle, and crime for Denver. We also hope to supplement this data with U.S. census data on socio-economic factors per district or county as well.

## Assignment
The [second assignment](https://github.com/HertieDataScience/SyllabusAndLectures/blob/master/README.md) states the following:
>The second pair assignment is a proposal for your Collaborative Research Project. It is an opportunity for you to layout your collaborative research paper question, justify why it is interesting, provide basic literature review (properly cited using BibTeX), and identify data sources/methodologies that you can access to help answer your question. You will also demonstrate your understanding of literate programming technologies. Deadline 28 October, 2,000 words maximum, 10% of final grade.

## Research 
For the assignment, we briefly outline our research question, reference relevant literature on the topic, and describe our data and research methodology.

Our research begins by attempting to validate the findings of a recent publication (http://www.cato.org/publications/policy-analysis/dose-reality-effect-state-marijuana-legalizations) concerning the legalization of marijuana in Washington and Colorado. The topic is both interesting and relevant given the fact that next month nine other states will decide upon legalization of recreational use (five states in total) and for medicinal purposes (four states). We use statewide data on producers and retailers in the state of Washington and on crime rates and policing in the city of Seattle to reproduce the outcomes of the Cato study and compare the findings with those for Denver.

We use two main data sources: the first is the website for the 'Washington Initiative 502,' the initiative to legalize recreational use (www.502data.com); and the second is a .csv file from the city of Denver's Open Data Catalog (https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-crime). The former lists the names and locations of producers/processors in the state and of retailers including YTD sales and tax revenue. Since the 502data site is scripted in Java, we use the phantomjs() function in the RSelenium package, which does not work on a Mac. For Denver's data, we downloaded the .csv file and used the readr package to load the data.

