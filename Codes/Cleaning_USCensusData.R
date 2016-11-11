# Finding the hundred building blocks to merge the data from Seattle Retailers with
x3 <- aggregate(cbind(lon, lat) ~ GEOID, 
                data=Seattle.SOE, FUN=function(x) mean(range(x)))
x4 <- aggregate(cbind(lon.Narc, lat.Narc) ~ Address.Narc, data=Seattle.Merged, FUN=function(x) mean(range(x)))

# create a loop2 where each individual value of x1 is distance measured with x2
# and print the resulting 15 observations in a new object called "min.dist"
# This can then be used for merging along this point

# Create a loop2 to get the minimum distance values for each Retailer to Hundred Blocks
for (loop2 in (1:nrow(x3))){
  c <- rdist.earth(matrix(c(x4$lon.Narc, x4$lat.Narc), ncol=2),
                   matrix(c(x3[1:loop2,"lon"], x3[1:loop2,"lat"]), ncol=2),
                   miles=FALSE, R=6371)
}

# Create a vector "c" for all minimal distances
c <- as.data.frame(c)
for (value2 in (1:ncol(c))){
  c[value2, "value"] <- which.min(c[,value2])
}

# Clean vector for analysis
d <- na.omit(c)
d <- as.data.frame(d$value)
d

# Which Hundred blocks are concerned?
Seattle.Crime.Narcotics[d$`d$value`,c("Address.Narc", "lat.Narc", "lon.Narc")]

# Clean for mergers
d$GEOID <- x3$GEOID
d$Address.Narc <- Seattle.Crime.Narcotics[d$`d$value`,"Address.Narc"]
d <- d[,-1]
d
# Merge Addresses for narcotics with normal addresses
Seattle.SOE <- inner_join(Seattle.SOE, d, by="GEOID")
sub.Seattle.SOE <- Seattle.SOE[,c(7:8)]

# Merge also by Month
Seattle.Merged <- merge(Seattle.Merged, sub.Seattle.SOE, by="Address.Narc", all=TRUE)

# Remove 
rm(c, d, x3, x4, loop2, value4)
