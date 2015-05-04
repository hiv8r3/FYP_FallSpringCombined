# read in data

dat = read.delim("Block2dat.txt", stringsAsFactors=F)

# read in badSub and discard accordingly
badSubs = read.delim("badSubs.txt")
dat = dat[!(dat$Subno %in% badSubs$Subno),]       # returns T or F for every element in first array (F for NA)

# look at histogram of RTs
# skewed, might want to log transform data
qplot(RT, data = dat, geom = "histogram", binwidth = 15)

# Shortest RT is 184 ms (acceptable)
# RTs are very positively skewed
# do log transform

#add column for logRT
dat$logRT = NULL

for (i in 1:length(row.names(dat))) {
  dat$logRT[i] = log(dat$RT[i])
}

# still looks skewed
qplot(logRT, data = dat, geom = "histogram", binwidth = .05)

qplot(logRT, data = dat[dat$Subno == 109,], geom = "histogram", binwidth = .05)

# Look for outliers according to subject
highCount = data.frame(NULL)
for (i in unique(dat$Subno)) {
  subdat = dat[dat$Subno == i,]
  #create cut off
  cutoff = mean(subdat$logRT) + 3*(sd(subdat$logRT))
  # count up number above cutoff
  NumHigh = sum(subdat$logRT > cutoff)
  highCount = rbind(highCount, data.frame("Subno" = subdat$Subno[1],
                                          "Mean" = mean(subdat$logRT),
                                          "Cutoff" = cutoff,
                                          "NumHigh" = NumHigh))
}

# Winsorize logRTs (input mean + 3*SD for values that are higher than that)
dat$logRT.winz = dat$logRT

for (i in unique(dat$Subno)) {
  subdat = dat[dat$Subno == i,]
  #create cut off
  cutoff = mean(subdat$logRT) + 3*(sd(subdat$logRT))
  dat$logRT.winz[dat$Subno == i & dat$logRT > cutoff] = cutoff
}

# looks more normal
qplot(logRT, data = dat, geom = "histogram", binwidth = .05)
qplot(logRT.winz, data = dat, geom = "histogram", binwidth = .05)


# add winsorized values too (not log transformed)
dat$RT.winz = dat$RT

for (i in unique(dat$Subno)) {
  subdat = dat[dat$Subno == i,]
  #create cut off
  cutoff = mean(subdat$RT) + 3*(sd(subdat$RT))
  dat$RT.winz[dat$Subno == i & dat$RT > cutoff] = cutoff
}

# looks more normal
qplot(RT.winz, data = dat, geom = "histogram", binwidth = 15)



write.table(dat, "logTransformed_data.txt", sep = "\t", row.names=F)

