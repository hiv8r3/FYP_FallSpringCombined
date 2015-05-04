# 1. Create factor scores for each participant for each separate factor

standard = read.delim("./MTCP/StandardizedMTCP.txt")

# 2. read in factor loadings for each item
load = read.delim("./MTCP/FactorLoadings.txt")

# 3. Create factor scores for each subject with Dunton & Fazio's factor loadings
# for factor 1
for (i in unique(standard$Subno)) {
  item.sum = 0
  for (j in 1:17) {
    curr.item = standard[i,j+1] * load$F1.Dunton[j] 
    item.sum = item.sum + curr.item
    #    print(item.sum)
  }
  standard$F1.Dunton[i] = item.sum
}

# for factor 2
for (i in unique(standard$Subno)) {
  item.sum = 0
  for (j in 1:17) {
    curr.item = standard[i,j+1] * load$F2.Dunton[j] 
    item.sum = item.sum + curr.item
    #    print(item.sum)
  }
  standard$F2.Dunton[i] = item.sum
}

# 4. Create factor scores for each subject with current data's factor loadings
# for factor 1
for (i in unique(standard$Subno)) {
  item.sum = 0
  for (j in 1:17) {
    curr.item = standard[i,j+1] * load$F1.curr[j] 
    item.sum = item.sum + curr.item
    #    print(item.sum)
  }
  standard$F1.curr[i] = item.sum
}

# for factor 2
for (i in unique(standard$Subno)) {
  item.sum = 0
  for (j in 1:17) {
    curr.item = standard[i,j+1] * load$F2.curr[j] 
    item.sum = item.sum + curr.item
    # print(item.sum)
  }
  standard$F2.curr[i] = item.sum
}

# 5. Create factor scores for each subject with one factor solution loading from current data
# for factor 1
for (i in unique(standard$Subno)) {
  item.sum = 0
  for (j in 1:17) {
    curr.item = standard[i,j+1] * load$OneFactorSol[j] 
    item.sum = item.sum + curr.item
    # print(item.sum)
  }
  standard$OneFactor[i] = item.sum
}


# Put individual factor scores with priming scores
stand.dat = data.frame("Subno" = standard$Subno,
                       "D.Concern" = standard$F1.Dunton,
                       "D.Avoid" = standard$F2.Dunton,
                       "Curr.Concern" = standard$F1.curr,
                       "Curr.Avoid" = standard$F2.curr,
                       "OneFactor" = standard$OneFactor,
                       "Black.priming" = NA,
                       "White.priming" = NA)


# 6. Add priming scores
# calculate priming score (RT to positive - RT to negative) for Black and White primes separately
# higher priming score = faster to negative
dat = read.delim("Winsorized_data.txt")

Wdat = dat[dat$FaceRace == "White",]
Bdat = dat[dat$FaceRace == "Black",]

for (i in unique(stand.dat$Subno)) {
  subsetW = Wdat[Wdat$Subno == i + 100,]
  posW = mean(subsetW$RT[subsetW$WordValence == "Pos"])
  negW = mean(subsetW$RT[subsetW$WordValence == "Neg"])
  stand.dat$White.priming[i] = posW-negW
  
  subsetB = Bdat[Bdat$Subno == i + 100,]
  posB = mean(subsetB$RT[subsetB$WordValence == "Pos"])
  negB = mean(subsetB$RT[subsetB$WordValence == "Neg"])
  stand.dat$Black.priming[i] = posB-negB
}

stand.dat$diff = stand.dat$Black.priming-stand.dat$White.priming

# Visualization of priming effect following Blacks - most hovers around or under 0
# < 0 priming effect = positive bias
ggplot(stand.dat, aes(Subno, Black.priming)) +
  geom_point()


# 7. Look at priming scores and factor scores from F&D

# Factor 1
# Black priming
ggplot(stand.dat, aes(D.Concern, Black.priming)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "red") +
  ggtitle("F&D Factor 1 + Black priming")

# White priming
ggplot(stand.dat, aes(D.Concern, White.priming)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "blue")

# diff priming
ggplot(stand.dat, aes(D.Concern, diff)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "purple")

summary(lm(stand.dat$D.Concern ~ stand.dat$Black.priming))
summary(lm(stand.dat$D.Concern ~ stand.dat$White.priming))
summary(lm(stand.dat$D.Concern ~ stand.dat$diff))


# Factor 2
ggplot(stand.dat, aes(D.Avoid, Black.priming)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "red")

ggplot(stand.dat, aes(D.Avoid, White.priming)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "red")

ggplot(stand.dat, aes(D.Avoid, diff)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "red")

summary(lm(stand.dat$D.Avoid ~ stand.dat$Black.priming))
summary(lm(stand.dat$D.Avoid ~ stand.dat$White.priming))
summary(lm(stand.dat$D.Avoid ~ stand.dat$diff))


# 7. Look at priming scores and factor scores extracted from current data

# Factor 1
# Black priming
ggplot(stand.dat, aes(Curr.Concern, Black.priming)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Curr data Factor 1 + Black priming")

# White priming
ggplot(stand.dat, aes(Curr.Concern, White.priming)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "blue")

# diff priming
ggplot(stand.dat, aes(Curr.Concern, diff)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "purple")

summary(lm(stand.dat$Curr.Concern ~ stand.dat$Black.priming))
summary(lm(stand.dat$Curr.Concern ~ stand.dat$White.priming))
summary(lm(stand.dat$Curr.Concern ~ stand.dat$diff))


# Factor 2
ggplot(stand.dat, aes(Curr.Avoid, Black.priming)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "red")

ggplot(stand.dat, aes(Curr.Avoid, White.priming)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "red")

ggplot(stand.dat, aes(Curr.Avoid, diff)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "red")

summary(lm(stand.dat$Curr.Avoid ~ stand.dat$Black.priming))
summary(lm(stand.dat$Curr.Avoid ~ stand.dat$White.priming))
summary(lm(stand.dat$Curr.Avoid ~ stand.dat$diff))



# 8. Look at priming scores and factor scores extracted from one factor solution (current data)

# Factor 1
# Black priming
ggplot(stand.dat, aes(OneFactor, Black.priming)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "red") +
  ggtitle("One factor solution + Black priming")

# White priming
ggplot(stand.dat, aes(OneFactor, White.priming)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "blue")

# diff priming
ggplot(stand.dat, aes(OneFactor, diff)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "purple")

summary(lm(stand.dat$OneFactor ~ stand.dat$Black.priming))
summary(lm(stand.dat$OneFactor ~ stand.dat$White.priming))
summary(lm(stand.dat$OneFactor ~ stand.dat$diff))






