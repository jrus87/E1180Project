source("./Codes/Cleaning_Retailers.R")

# Installing packages
packages <- c('RCurl', 'RJSONIO', 'plyr', 'rvest')
for (p in packages) {
  if (p %in% installed.packages()) require(p, character.only=TRUE) 
  else {
    install.packages(p)
    require(p, character.only=TRUE)
  }
}

# Set up URL for web-scraping
kush_url <- read_html("http://kushtourism.com/washington-recreational-marijuana-retail-map-and-directory/#Seattle")

# Get name and address for each Retailer in Seattle
Seattle.Retailer <- kush_url %>%
  html_nodes("table:nth-child(11) h5 a , table:nth-child(11) h2 a , table:nth-child(11) a:nth-child(2)") %>%
  html_text() %>%
  as.data.frame()

# Observation 25 has no name
Seattle.Retailer <- Seattle.Retailer[c(1:24, 26:35),]
Seattle.Retailer <- as.data.frame(Seattle.Retailer)

# Create a sequence of odd and even numbers to split data into two variables
even_indexes<-seq(2,34,2) # will become addresses
odd_indexes<-seq(1,33,2) # will become names
# Create new data frames to merge
Seattle.Retailer.name <- as.data.frame(Seattle.Retailer[odd_indexes,])
Seattle.Retailer.address <- as.data.frame(Seattle.Retailer[even_indexes,])
# Merge
Seattle.Retailer <- as.data.frame(cbind(Seattle.Retailer.name, Seattle.Retailer.address))

# Rename variable to allow to merge
colnames(Seattle.Retailer)[colnames(Seattle.Retailer)=="Seattle.Retailer[odd_indexes, ]"] <- "Name.Ret"
colnames(Seattle.Retailer)[colnames(Seattle.Retailer)=="Seattle.Retailer[even_indexes, ]"] <- "Address.Ret"
names(Seattle.Retailer)

# Create unique King County Retailers to merge with Seattle.Retailers to get addresses
unique_vector <- which(!duplicated(KingCounty.Retailer$Name))
KingCounty.Retailer.unique <- as.data.frame(KingCounty.Retailer[unique_vector,])

# Formatting the names of a few before merging
table(KingCounty.Retailer$Name.Ret)
table(Seattle.Retailer$Name.Ret)
# HERBAN LEGENDS, UNCLE IKE'S, UNCLE IKES, DIEGO PELLICER, SEATTLE CANNABIS COMPANY, #HASHTAG, OZ. 
# Queen Anne Cannabis Co. is not in the 502data
Seattle.Retailer$Name.Ret <- as.character(Seattle.Retailer$Name.Ret)
Seattle.Retailer[1,1] <- "HERBAN LEGENDS"
Seattle.Retailer[2,1] <- "UNCLE IKE'S"
Seattle.Retailer[3,1] <- "UNCLE IKES"
Seattle.Retailer[6,1] <- "DIEGO PELLICER"
Seattle.Retailer[11,1] <- "SEATTLE CANNABIS COMPANY"
Seattle.Retailer[12,1] <- "#HASHTAG"
Seattle.Retailer[17,1] <- "OZ."
Seattle.Retailer <- Seattle.Retailer[-14,]  # POT SHOP has moved to 1628 Dexter Ave N
Seattle.Retailer

# Merge
Retailer <- left_join(Retailer, Seattle.Retailer, by="Name.Ret")
KingCounty.Retailer <- left_join(KingCounty.Retailer, Seattle.Retailer, by="Name.Ret")

###
# Get Geo_locations
##

url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(c(NA,NA,NA, NA))
  }
}

# Only keep data for Seattle
Seattle.Retailer <- na.omit(KingCounty.Retailer) 
Seattle.Retailer <- as.data.frame(Seattle.Retailer)

# Specify vector for geo-location
address <- Seattle.Retailer$Address

ptm <- proc.time()
locations  <- ldply(address, function(x) geoCode(x))
names(locations)  <- c("lat.Ret","lon.Ret","location_type", "Address.Ret")
head(locations)
proc.time() - ptm 

# Do we have Na's? They seem to change over time, so look automatically for changes
#which(is.na(locations$location_type))
#na_address_vector <- Seattle.Retailer[c(na_vector),"Address.Ret"]
#locations2 <- ldply(na_address_vector, function(x) geoCode(x))
#names(locations2)  <- c("lat","lon","location_type", "Address.Ret")
# Sort out the formatting of Address.Ret
locations$Address.Ret[locations$Address.Ret=="1465 E Republican St, Seattle, WA 98112, USA"] <- "1465 E REPUBLICAN ST, SEATTLE, WA 98112"
locations$Address.Ret[locations$Address.Ret=="1628 Dexter Ave N, Seattle, WA 98109, USA"] <- "1628 Dexter Ave N, Seattle, WA 98100"
locations$Address.Ret[locations$Address.Ret=="1944 1st Avenue South, Seattle, WA 98134, USA"] <- "1944 1st Avenue South #100, Seattle, WA 98134"
locations$Address.Ret[locations$Address.Ret=="2215 4th Ave S, Seattle, WA 98134, USA"] <- "2215 4th Ave S, Seattle, WA 98134"
locations$Address.Ret[locations$Address.Ret=="2310 E Union St, Seattle, WA 98122, USA"] <- "2310 E UNION ST. SEATTLE, WA 98122"
locations$Address.Ret[locations$Address.Ret=="2413 E Union St, Seattle, WA 98122, USA"] <- "2413 East Union Street, Seattle, WA 98122, USA"
locations$Address.Ret[locations$Address.Ret=="2733 4th Ave S, Seattle, WA 98134, USA"] <- "2733 4TH AVE S, SEATTLE WA 98134"
locations$Address.Ret[locations$Address.Ret=="3207 1st Avenue South a, Seattle, WA 98134, USA"] <- "3207 1ST AVE S UNIT A, SEATTLE WA 98134"
locations$Address.Ret[locations$Address.Ret=="321 NE 45th St, Seattle, WA 98105, USA"] <- "321 NE 45TH ST, SEATTLE WA 98105"
locations$Address.Ret[locations$Address.Ret=="3230 1st Avenue South, Seattle, WA 98134, USA"] <- "3230 1ST AVE S, SEATTLE WA 98134"
locations$Address.Ret[locations$Address.Ret=="3540 Stone Way N, Seattle, WA 98103, USA"] <- "3540 STONE WAY N, SEATTLE WA 98103"
locations$Address.Ret[locations$Address.Ret=="3831 Stone Way N, Seattle, WA 98103, USA"] <- "3831 Stone Way N, Seattle, WA 98103, USA"
locations$Address.Ret[locations$Address.Ret=="4465 Fremont Ave N, Seattle, WA 98103, USA"] <- "4465 FREMONT AVE N, SEATTLE, WA 98103"
locations$Address.Ret[locations$Address.Ret=="501 15th Ave E, Seattle, WA 98112, USA"] <- "501 15th Ave E, Seattle, WA 98112"
locations$Address.Ret[locations$Address.Ret=="55 Bell St, Seattle, WA 98121, USA"] <- "55 Bell St, Seattle, WA 98121, USA"

#locations2$Address[locations2$Address=="2413 E Union St, Seattle, WA 98122, USA"] <- "2413 East Union Street, Seattle, WA 98122, USA"
#locations2$Address[locations2$Address=="3207 1st Avenue South a, Seattle, WA 98134, USA"] <- "3207 1ST AVE S UNIT A, SEATTLE WA 98134"
#locations2$Address[locations2$Address=="3230 1st Avenue South, Seattle, WA 98134, USA"] <- "3230 1ST AVE S, SEATTLE WA 98134"
# Merge now solved na's with initial location
Seattle.Retailer <- left_join(Seattle.Retailer, locations, by="Address.Ret")
#Seattle.Retailer <- left_join(Seattle.Retailer, locations2, by="Address")

# Drop superfluous variables
Seattle.Retailer$location_type <- NULL

# Only keep unique observations of name.ret for each month.ret
library(plyr)
unique_df <- count(Seattle.Retailer, c('Name.Ret', 'Month.Ret', 'Sales.Ret', 'ExciseTax.Ret', 'lat.Ret', 'lon.Ret', 'Address.Ret'))
unique_df <- unique_df[,-8]
unique_df <- unique_df[order(unique_df$Month.Ret),]

# Save as .txt-file
write.table(unique_df, file="./Data/Location.txt", sep = ",")

# Remove from environment
rm(even_indexes, odd_indexes, url, Seattle.Retailer.Address.Ret, Seattle.Retailer.name, KingCounty.Retailer.unique, unique_vector, 
   packages, p, kush_url, geoCode, locations, locations2, Address.Ret, na_address_vector, na_vector, ptm, unique_df)
