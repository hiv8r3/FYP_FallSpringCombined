#Look for bad subs or trials

dat2 = read.delim("Block2dat.txt", stringsAsFactors=F)


# Look for people with insanely fast RTs -- they're just jamming a button
meanRT = tapply(dat2$RT, INDEX=dat2$Subno, FUN=mean)
meanRT
barplot(meanRT)
hist(meanRT, breaks=20)
# shows no one with super fast RTs
# maybe someone who's taking longer than normal (Sub 24)

#check for NA
countNA = function(x) {
  temp = is.na(x)
  count = sum(temp)
  return(count)
}
numNA = tapply(dat2$RT, INDEX=dat2$Subno, FUN=countNA)
numNA


# who's doing shitty?
# code won't work because error is not string instead of number- can't sum
dat2$Error = as.numeric(dat2$Error)

NumError = tapply(dat2$Error, INDEX=dat2$Subno, FUN=sum, na.rm=T)
NumError
barplot(NumError)
hist(NumError, breaks=10)
# Sub 21 is making a lot of errors

# How many errors is acceptable?
# Probability of getting trial correct by chance is 50%
# so what's the critical number of correct trials to be 
# not an asshole, p<.01?
alpha = .01
qbinom(p = 1-alpha, 
       size = 288,    #number of trials
       prob=.5)
# grab subjects who got <164 correct
NumCorrect = 288 - tapply(dat2$Error, dat2$Subno, FUN=sum)
badTab = NumCorrect[NumCorrect < 164]
# it's empty, so no bad subjects

# make a log of it
# badSubs = data.frame("Subno" = names(badTab),
#                       "correctCount" = badTab,
#                       "reason" = "Bad accuracy")

# add 036 to badSubs because she didn't complete (need to use rbind if there are eventually subs with high error counts)
badSubs = data.frame("Subno" = 136,
                      "correctCount" = NA,
                      "reason" = "Didn't complete")


write.table(badSubs, file="badSubs.txt", sep="\t", row.names=F)
