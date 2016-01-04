require(plyr)
require(reshape2)

# read in data
dat = read.delim("TotalCombinedDat.txt")

# split into spring and fall data
spring = dat[dat$Term == "spring",]
fall = dat[dat$Term == "fall",]

# 1. Recreate fall analyses first (use log transformed data)

# (1) Omnibus ANOVA (FaceRace x WordValence x Fix)
#change format to do ANOVA
#different line for each value (so each variable has a column specifying which level of that variable
#the value is for)
f.wide= melt(tapply(fall$logRT, INDEX=list(fall$Fix, fall$FaceRace, fall$WordValence, fall$Subno), 
                  FUN= mean))

f.wide = rename(f.wide, c("Var1" = "Fix", "Var2" = "FaceRace", 
                      "Var3" = "WordValence", "Var4" = "Subno", "value" = "logRT.mean"))

#for within subjects ANOVAs, SUBJECT HAS TO BE A FACTOR
f.wide$Subno = as.factor(f.wide$Subno)

# create anova model
int <- aov(logRT.mean ~ (Fix*FaceRace*WordValence)+Error(Subno/(Fix*FaceRace*WordValence)), data = f.wide)
summary(int)  #displays Type 1 ANOVA, will be different from Type 3 ANOVA when unbalanced design

sink("./Output/FallANOVA_PrimingBlockAnalysis.txt", append=F)
"ANOVA of fall data (5.1.2015)"
"Code in Analysis_combinedDat.R"
"Doesn't include two bad subjects or fast trials (<50 ms)"
"Analysis done with log transformed RTs"
"------------------------------------------------------------------"
"OMNIBUS (FaceRace x WordValence x Fix)"
summary(int)
sink()

# Specific contrasts
# FaceRace x WordValence within eye condition
f.wide.eyes = f.wide[f.wide$Fix == "eyes",]

int.eyes <- aov(logRT.mean ~ (FaceRace*WordValence)+Error(Subno/(FaceRace*WordValence)), data = f.wide.eyes)
summary(int.eyes)

sink("./Output/FallANOVA_PrimingBlockAnalysis.txt", append=T)
"------------------------------------------------------------------"
"Specific contrast: (FaceRace x WordValence with Eyes condition)"
summary(int.eyes)
sink()

# FaceRace x WordValence within forehead condition
f.wide.fore = f.wide[f.wide$Fix == "forehead",]

int.fore <- aov(logRT.mean ~ (FaceRace*WordValence)+Error(Subno/(FaceRace*WordValence)), data = f.wide.fore)
summary(int.fore)

sink("./Output/FallANOVA_PrimingBlockAnalysis.txt", append=T)
"------------------------------------------------------------------"
"Specific contrast: (FaceRace x WordValence with Forehead condition)"
summary(int.fore)
sink()

# FaceRace x WordValence within nose condition
f.wide.nose = f.wide[f.wide$Fix == "nose",]

int.nose <- aov(logRT.mean ~ (FaceRace*WordValence)+Error(Subno/(FaceRace*WordValence)), data = f.wide.nose)
summary(int.nose)

sink("./Output/FallANOVA_PrimingBlockAnalysis.txt", append=T)
"------------------------------------------------------------------"
"Specific contrast: (FaceRace x WordValence with Nose condition)"
summary(int.nose)
sink()


 # (2) Omnibus ANOVA (Congruent x Fix)
#change format to do ANOVA
#different line for each value (so each variable has a column specifying which level of that variable
#the value is for)
f.wide2= melt(tapply(fall$logRT, INDEX=list(fall$Fix, fall$Congruent, fall$Subno), 
                    FUN= mean))

f.wide2 = rename(f.wide2, c("Var1" = "Fix", "Var2" = "Congruent", 
                          "Var3" = "Subno", "value" = "logRT.mean"))

#for within subjects ANOVAs, SUBJECT HAS TO BE A FACTOR
f.wide2$Subno = as.factor(f.wide2$Subno)

# create anova model
int <- aov(logRT.mean ~ (Fix*Congruent)+Error(Subno/(Fix*Congruent)), data = f.wide2)
summary(int)  #displays Type 1 ANOVA, will be different from Type 3 ANOVA when unbalanced design

sink("./Output/FallANOVA_PrimingBlockAnalysis.txt", append=T)
"------------------------------------------------------------------"
"------------------------------------------------------------------"
"OMNIBUS (Congruent x Fix)"
summary(int)
sink()


# Specific contrasts
# Congruence within eye condition
f.wide2.eyes = f.wide2[f.wide2$Fix == "eyes",]

int2.eyes <- aov(logRT.mean ~ (Congruent)+Error(Subno/(Congruent)), data = f.wide2.eyes)
summary(int2.eyes)

sink("./Output/FallANOVA_PrimingBlockAnalysis.txt", append=T)
"------------------------------------------------------------------"
"Specific contrast: (Congruent within Eyes condition)"
summary(int2.eyes)
sink()

# Congruence within forehead condition
f.wide2.fore = f.wide2[f.wide2$Fix == "forehead",]

int2.fore <- aov(logRT.mean ~ (Congruent)+Error(Subno/(Congruent)), data = f.wide2.fore)
summary(int2.fore)

sink("./Output/FallANOVA_PrimingBlockAnalysis.txt", append=T)
"------------------------------------------------------------------"
"Specific contrast: (Congruent within Forehead condition)"
summary(int2.fore)
sink()

# Congruence within nose condition
f.wide2.nose = f.wide2[f.wide2$Fix == "nose",]

int2.nose <- aov(logRT.mean ~ (Congruent)+Error(Subno/(Congruent)), data = f.wide2.nose)
summary(int2.nose)

sink("./Output/FallANOVA_PrimingBlockAnalysis.txt", append=T)
"------------------------------------------------------------------"
"Specific contrast: (Congruent within Nose condition)"
summary(int2.nose)
sink()













# 2. Do spring analyses (use log transformed data)

# (1) Omnibus ANOVA (FaceRace x WordValence x Fix)
#change format to do ANOVA
#different line for each value (so each variable has a column specifying which level of that variable
#the value is for)
s.wide= melt(tapply(spring$logRT, INDEX=list(spring$Fix, spring$FaceRace, spring$WordValence, spring$Subno), 
                    FUN= mean))

s.wide = rename(s.wide, c("Var1" = "Fix", "Var2" = "FaceRace", 
                          "Var3" = "WordValence", "Var4" = "Subno", "value" = "logRT.mean"))

#for within subjects ANOVAs, SUBJECT HAS TO BE A FACTOR
s.wide$Subno = as.factor(s.wide$Subno)

# create anova model
s.int <- aov(logRT.mean ~ (Fix*FaceRace*WordValence)+Error(Subno/(Fix*FaceRace*WordValence)), data = s.wide)
summary(s.int)  #displays Type 1 ANOVA, will be different from Type 3 ANOVA when unbalanced design

sink("./Output/SpringANOVA_PrimingBlockAnalysis.txt", append=F)
"ANOVA of spring data (5.1.2015)"
"Code in Analysis_combinedDat.R"
"Includes all subjects, analysis done on log transformed RTs"
"------------------------------------------------------------------"
"OMNIBUS (FaceRace x WordValence x Fix)"
summary(s.int)
sink()







# 3. Compare spring and fall data to each other

# (1) Omnibus ANOVA (Fix x Congruent x Term)
# Create wide form
wide= melt(tapply(dat$logRT, INDEX=list(dat$Fix, dat$Congruent, dat$Term, dat$Subno), 
                    FUN= mean))

wide = rename(wide, c("Var1" = "Fix", "Var2" = "Congruent", 
                          "Var3" = "Term", "Var4" = "Subno", "value" = "logRT.mean"))

#for within subjects ANOVAs, SUBJECT HAS TO BE A FACTOR
wide$Subno = as.factor(wide$Subno)

# create anova model
# Fix and Congruent are within Ss, Term is between Ss
int <- aov(logRT.mean ~ (Fix*Congruent*Term)+Error(Subno/(Fix*Congruent)), data = wide)
summary(int)  #displays Type 1 ANOVA, will be different from Type 3 ANOVA when unbalanced design

sink("combinedANOVA_PrimingBlock.txt", append=F)
"ANOVA of spring and fall data together (5.4.2015)"
"Code in Analysis_combinedDat.R"
"Includes all subjects for Spring, eliminates bad subjects and fast trials for Fall"
"Analysis done on log transformed RTs"
"------------------------------------------------------------------"
"OMNIBUS (Fix x Congruent x Term)"
summary(int)
sink()


# (2) Omnibus ANOVA (Fix x FaceRace x WordValence x Term)
# Create wide form
a.wide= melt(tapply(dat$logRT, INDEX=list(dat$Fix, dat$FaceRace, dat$WordValence, dat$Term, dat$Subno), 
                  FUN= mean))

a.wide = rename(a.wide, c("Var1" = "Fix", "Var2" = "FaceRace", 
                      "Var3" = "WordValence", "Var4" = "Term", "Var5" = "Subno", "value" = "logRT.mean"))

#for within subjects ANOVAs, SUBJECT HAS TO BE A FACTOR
a.wide$Subno = as.factor(a.wide$Subno)

# create anova model
# Fix and Congruent are within Ss, Term is between Ss
a.int <- aov(logRT.mean ~ (Fix*FaceRace*WordValence*Term)+Error(Subno/(Fix*WordValence*FaceRace)), data = a.wide)
summary(a.int)  #displays Type 1 ANOVA, will be different from Type 3 ANOVA when unbalanced design

sink("combinedANOVA_PrimingBlock.txt", append=T)
"------------------------------------------------------------------"
"------------------------------------------------------------------"
"OMNIBUS (Fix x FaceRace x WordValence x Term)"
summary(a.int)
sink()