require(plyr)
require(reshape2)

# read in data
dat = read.delim("TotalCombinedDat.txt")

# split into spring and fall data
spring = dat[dat$Term == "spring",]
fall = dat[dat$Term == "fall",]


# 1. Do 2 x 2 Anova (Fix by Congruence) for fall data (just within eyes and forehead condition)
fall$Fix = as.character(fall$Fix)
mast.dat = fall[fall$Fix != "nose",]

#change format to do ANOVA
#different line for each value (so each variable has a column specifying which level of that variable
#the value is for)
f.wide= melt(tapply(mast.dat$logRT, INDEX=list(mast.dat$Fix, mast.dat$Congruent, #mast.dat$WordValence,
                                               #mast.dat$FaceRace, 
                                               mast.dat$Subno), 
                    FUN= mean))

f.wide = rename(f.wide, c("Var1" = "Fix", "Var2" = "Congruent", 
                          #"Var3" = "WordValence", "Var4" = "FaceRace", 
                          "Var3" = "Subno", "value" = "logRT.mean"))

#for within subjects ANOVAs, SUBJECT HAS TO BE A FACTOR
f.wide$Subno = as.factor(f.wide$Subno)

# create anova model
int <- aov(logRT.mean ~ (Fix*Congruent)+Error(Subno/(Fix*Congruent)), data = f.wide)
summary(int)  #displays Type 1 ANOVA, will be different from Type 3 ANOVA when unbalanced design

sink("./Output/FallANOVA_MastersAnalysis.txt", append=F)
"ANOVA of fall data (9.2.2015)"
"Code in Analysis_combinedDat.R"
"Doesn't include two bad subjects or fast trials (<50 ms)"
"Analysis done with log transformed RTs"
"Only includes eye and forehead conditions"
"------------------------------------------------------------------"
"OMNIBUS (Fix x Congruent)"
summary(int)
sink()

# 2. Do specific tests of effect of congruence within Fix conditions

# already done, in FallANOVA_PrimingBlockAnalysis.txt
# in FYP_FallSpringCombined --> Output


# 3. Do the same thing with t-tests, just to make sure I'm doing it right

# (1) T-test of congruence within eye condition
f.wide2.eyes = f.wide[f.wide$Fix == "eyes",]

# ANOVA
int2.eyes <- aov(logRT.mean ~ (Congruent)+Error(Subno/(Congruent)), data = f.wide2.eyes)
summary(int2.eyes)
# T-test
t.test(x=f.wide2.eyes$logRT.mean[f.wide2.eyes$Congruent=="Con"], 
       y=f.wide2.eyes$logRT.mean[f.wide2.eyes$Congruent=="Incon"])


# 4. Reshape data to send to Bruce

require(tidyr)

spread1 = spread(f.wide, Congruent, logRT.mean)
spread1 = rename(spread1, c("Con" = "Fix", "Var2" = "Congruent", 
                                         #"Var3" = "WordValence", "Var4" = "FaceRace", 
                                         "Var3" = "Subno", "value" = "logRT.mean"))

write.table(spread1, file = "DataForBruce.txt", sep = "\t", row.names = F)

# in excel, moved forehead up so that each subject was on one line


# 5. Try analyzing logRT.winz

# Create wide version of logRT.winz data
log.winz = melt(tapply(mast.dat$logRT.winz, INDEX=list(mast.dat$Fix, mast.dat$Congruent, #mast.dat$WordValence,
                                               #mast.dat$FaceRace, 
                                               mast.dat$Subno), 
                    FUN= mean))

log.winz = rename(log.winz, c("Var1" = "Fix", "Var2" = "Congruent", 
                          #"Var3" = "WordValence", "Var4" = "FaceRace", 
                          "Var3" = "Subno", "value" = "logRT.winz.mean"))

#for within subjects ANOVAs, SUBJECT HAS TO BE A FACTOR
log.winz$Subno = as.factor(log.winz$Subno)

# create anova model for full interaction term
int <- aov(logRT.winz.mean ~ (Fix*Congruent)+Error(Subno/(Fix*Congruent)), data = log.winz)
summary(int)  #displays Type 1 ANOVA, will be different from Type 3 ANOVA when unbalanced design


# just within eyes
log.winz.eyes = log.winz[log.winz$Fix == "eyes",]

int <- aov(logRT.winz.mean ~ (Congruent)+Error(Subno/(Congruent)), data = log.winz.eyes)
summary(int)  #displays Type 1 ANOVA, will be different from Type 3 ANOVA when unbalanced design

# just within forehead
log.winz.fore = log.winz[log.winz$Fix == "forehead",]

int <- aov(logRT.winz.mean ~ (Congruent)+Error(Subno/(Congruent)), data = log.winz.fore)
summary(int)  #displays Type 1 ANOVA, will be different from Type 3 ANOVA when unbalanced design


