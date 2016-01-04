require(tidyr)
require(dplyr)

# read in data
dat = read.delim("TotalCombinedDat.txt")

# split into spring and fall data
spring = dat[dat$Term == "spring",]
fall = dat[dat$Term == "fall",]

# take data and flatten it
fall2 = summarize(group_by(fall, Fix, Subno), logRT = mean(logRT))

flat = spread(fall2, Fix, logRT)

# Look at contrast that compares nose with average of eyes and forehead
contr1 = c(.5, .5, -1)
xc1=cbind(flat$eyes, flat$forehead, flat$nose)%*% contr1 
aov1=aov(xc1~1) 
summary(aov1,intercept=T)

