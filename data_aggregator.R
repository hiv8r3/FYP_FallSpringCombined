require(dplyr)
require(plyr)

# put all files into one file
# plug in number of subjects
NumFiles = 54

dat = data.frame(NULL) # make empty container
for (i in 1:9) {
  fileName = paste("./Individual_data/", 0, 0, i, ".dat", sep="") # make filename. Paste() combines strings together.
  temp = read.delim(file=fileName, sep="\t", stringsAsFactors=F) # read data with filename
  temp$Subno = 100 + i # mark data with subject number
  dat = rbind(dat, temp) # staple it to previous
}

for (i in 10:NumFiles) {
  fileName = paste("./Individual_data/", 0, i, ".dat", sep="") # make filename. Paste() combines strings together.
  temp = read.delim(file=fileName, stringsAsFactors=F) # read data with filename
  temp$Subno = 100 + i # mark data with subject number
  dat = rbind(dat, temp) # staple it to previous
}

# add participant race information
demog = read.delim("Demog.txt", stringsAsFactors=F)

for (i in unique(dat$Subno)) {
  dat$ParRace[dat$Subno == i] = demog$Race[demog$Subject == (i-100)]
}

# add participant gender information
for (i in unique(dat$Subno)) {
  dat$ParGender[dat$Subno == i] = demog$Gender[demog$Subject == (i-100)]
}

# rename Race column as FaceRace

dat = rename(dat, c("Race" = "FaceRace"))

# create data set with just block 2 data
# 288 trials per subject
Block2dat = dat[dat$Block == 2,]


# to check number of trials for each fixation
table(Block2dat$Condition[Block2dat$Subno != 136], Block2dat$Block[Block2dat$Subno != 136])

# to change Error column
for (i in 1:length(row.names(Block2dat))) {
  if (Block2dat$WordValence[i] == (Block2dat$Key[i]-3)){
    Block2dat$Error[i] = "0"
  }
  else if (Block2dat$WordValence[i] != (Block2dat$Key[i]-3)){
    Block2dat$Error[i] = "1"
  }
}

# add column for congruence
for (i in 1:length(row.names(Block2dat))) {
  if (Block2dat$FaceRace[i] == Block2dat$WordValence[i]) {
    Block2dat$Congruent[i] = "Con"
  } else if (Block2dat$FaceRace[i] != Block2dat$WordValence[i]) {
    Block2dat$Congruent[i] = "Incon"
  }
}

# Change FaceRace to words instead of numbers
for (i in 1:length(row.names(Block2dat))) {
  if (Block2dat$FaceRace[i] == 1) {
    Block2dat$FaceRace[i] = "Black"
  } else if (Block2dat$FaceRace[i] == 2) {
    Block2dat$FaceRace[i] = "White"
  }
}

# Change WordValence to words instead of numbers
for (i in 1:length(row.names(Block2dat))) {
  if (Block2dat$WordValence[i] == 1) {
    Block2dat$WordValence[i] = "Neg"
  } else if (Block2dat$WordValence[i] == 2) {
    Block2dat$WordValence[i] = "Pos"
  }
}


write.table(Block2dat, file = "Block2dat.txt", sep = "\t", row.names = FALSE)

