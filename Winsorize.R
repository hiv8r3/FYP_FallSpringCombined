# read in data

dat = read.delim("Block2dat.txt", stringsAsFactors=F)

# read in badSub and discard accordingly
badSubs = read.delim("badSubs.txt")
dat = dat[!(dat$Subno %in% badSubs$Subno),]       # returns T or F for every element in first array (F for NA)

# look at histogram of RTs
# skewed, might want to log transform data
qplot(RT, data = dat[dat$Subno == 1,], geom = "histogram")

# Look for outliers according to subject
highCount = data.frame(NULL)
for (i in unique(dat$Subno)) {
  subdat = dat[dat$Subno == i,]
  #create cut off
  cutoff = mean(subdat$RT) + 3*(sd(subdat$RT))
  # count up number above cutoff
  NumHigh = sum(subdat$RT > cutoff)
  highCount = rbind(highCount, data.frame("Subno" = subdat$Subno[1],
                                          "Mean" = mean(subdat$RT),
                                          "Cutoff" = cutoff,
                                          "NumHigh" = NumHigh))
}


# Winsorize data (input mean + 3*SD for values that are higher than that)
for (i in unique(dat$Subno)) {
  subdat = dat[dat$Subno == i,]
  #create cut off
  cutoff = mean(subdat$RT) + 3*(sd(subdat$RT))
  dat$RT[dat$Subno == i & dat$RT > cutoff] = cutoff
}

# check
checkHigh = data.frame(NULL)
for (i in unique(dat$Subno)) {
  subdat = dat[dat$Subno == i,]
  NumHigh = sum(subdat$RT > highCount$cutoff[i])
  checkHigh = rbind(checkHigh, data.frame("Subno" = i,
                                          "Mean" = highCount$Mean[i],
                                          "Cutoff" = highCount$Cutoff[i],
                                          "NumHigh" = NumHigh))
}

write.table(dat, file= "Winsorized_data.txt", sep="\t", row.names=FALSE)



# looking at distributions of winsorized data
qplot(RT, data = dat, geom = "histogram")
# still skewed
qplot(Mean, data = highCount, geom = "histogram", binwidth = 5)
# looks like subject 24 has a really high mean RT
qplot(RT, data = dat[dat$Subno == 24,], geom = "histogram")
# not sure if I should throw sub out
# according to data collection notes on 24, "kept looking down after the fix for the first quarter"

# distribution of RTs without Sub 24
qplot(RT, data = dat[dat$Subno != 24,], geom = "histogram")
#not as skewed but still skewed
