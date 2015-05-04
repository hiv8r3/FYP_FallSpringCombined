require(dplyr)
require(ggplot2)


# read in file
mtcp = read.delim("./MTCP/MTCPdat.txt", stringsAsFactors=F)

# create MTCP score for each subject
# reverse score items 2, 5, 8, 9, 16, 17
rev.score <- function(x) {
  h <- unique(x)
  a <- seq(min(h, na.rm=T), max(h, na.rm=T))
  b <- rev(a)
  dat <- data.frame(a, b)
  dat[match(x, dat[, 'a']), 2]
}

mtcp$MTCP2.rev = rev.score(mtcp$MTCP2)
mtcp$MTCP5.rev = rev.score(mtcp$MTCP5)
mtcp$MTCP8.rev = rev.score(mtcp$MTCP8)
mtcp$MTCP9.rev = rev.score(mtcp$MTCP9)
mtcp$MTCP16.rev = rev.score(mtcp$MTCP16)
mtcp$MTCP17.rev = rev.score(mtcp$MTCP17)

# take out original items that got reverse-scored
mtcp = select(mtcp, -c(MTCP2, MTCP5, MTCP8, MTCP9, MTCP16, MTCP17))
# average across all items
mtcp$MTCP.avg = rowMeans(select(mtcp, -Subno), na.rm=T)

# do the same, but just with internal questions:
# Items 1, 3, 6, 11, 12, 13 
temp  = select(mtcp, c(MTCP1, MTCP3, MTCP6, MTCP11, MTCP12, MTCP13))
mtcp$MTCP.int = rowMeans(temp, na.rm=T)

# Standardize MTCP scores
mtcp$stand1 = mtcp$MTCP1 - 4
mtcp$stand2 = mtcp$MTCP2.rev - 4
mtcp$stand3 = mtcp$MTCP3 - 4
mtcp$stand4 = mtcp$MTCP4 - 4
mtcp$stand5 = mtcp$MTCP5.rev - 4
mtcp$stand6 = mtcp$MTCP6 - 4
mtcp$stand7 = mtcp$MTCP7 - 4
mtcp$stand8 = mtcp$MTCP8.rev - 4
mtcp$stand9 = mtcp$MTCP9.rev - 4
mtcp$stand10 = mtcp$MTCP10 - 4
mtcp$stand11 = mtcp$MTCP11 - 4
mtcp$stand12 = mtcp$MTCP12 - 4
mtcp$stand13 = mtcp$MTCP13 - 4
mtcp$stand14 = mtcp$MTCP14 - 4
mtcp$stand15 = mtcp$MTCP15 - 4
mtcp$stand16 = mtcp$MTCP16.rev - 4
mtcp$stand17 = mtcp$MTCP17.rev- 4

standard = select(mtcp, c(Subno, stand1, stand2, stand3, stand4, stand5, stand6, stand7, stand8, stand9, stand10, stand11,
                          stand12, stand13, stand14, stand15, stand16, stand17))

write.table(standard, "./MTCP/StandardizedMTCP.txt", sep = "\t", row.names=F)


# Look at relationship with priming data
dat = read.delim("Winsorized_data.txt")

new.dat = data.frame("Subno" = mtcp$Subno,
                     "MTCP.avg" = mtcp$MTCP.avg,
                     "MTCP.int" = mtcp$MTCP.int,
                     "Black.priming" = NA,
                     "White.priming" = NA)


# calculate priming score (RT to positive - RT to negative) for Black and White primes separately
# higher priming score = faster to negative
Wdat = dat[dat$FaceRace == "White",]
Bdat = dat[dat$FaceRace == "Black",]

for (i in unique(mtcp$Subno)) {
  subsetW = Wdat[Wdat$Subno == i + 100,]
  posW = mean(subsetW$RT[subsetW$WordValence == "Pos"])
  negW = mean(subsetW$RT[subsetW$WordValence == "Neg"])
  new.dat$White.priming[i] = posW-negW
  
  subsetB = Bdat[Bdat$Subno == i + 100,]
  posB = mean(subsetB$RT[subsetB$WordValence == "Pos"])
  negB = mean(subsetB$RT[subsetB$WordValence == "Neg"])
  new.dat$Black.priming[i] = posB-negB
}

# Look at relationship between priming effect and MTCP.avg
# No relationship found
ggplot(new.dat, aes(MTCP.avg, Black.priming)) +
  geom_point()

ggplot(new.dat, aes(MTCP.avg, White.priming)) +
  geom_point()

ggplot(new.dat, aes(MTCP.avg, Black.priming-White.priming)) +
  geom_point()

summary(lm(new.dat$MTCP.avg ~ new.dat$Black.priming))
summary(lm(new.dat$MTCP.avg ~ new.dat$White.priming))
summary(lm(new.dat$MTCP.avg ~ (new.dat$Black.priming-new.dat$White.priming)))


# Look at relationship between priming effect and MTCP.int
# No relationship found
ggplot(new.dat, aes(MTCP.int, Black.priming)) +
  geom_point()

ggplot(new.dat, aes(MTCP.int, White.priming)) +
  geom_point()

ggplot(new.dat, aes(MTCP.int, Black.priming-White.priming)) +
  geom_point()

summary(lm(new.dat$MTCP.int ~ new.dat$Black.priming))
summary(lm(new.dat$MTCP.int ~ new.dat$White.priming))
summary(lm(new.dat$MTCP.int ~ (new.dat$Black.priming-new.dat$White.priming)))


# Visualization of priming effect following Blacks - most hovers around or under 0
# < 0 priming effect = positive bias
ggplot(new.dat, aes(Subno, Black.priming)) +
  geom_point()







