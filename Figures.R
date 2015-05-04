require(ggplot2)

# read in data
# Winsorized
dat = read.delim("logTransformed_data.txt", stringsAsFactors = F)

# change condition to factors
dat$Condition = as.factor(dat$Condition)


# all subjects together (Fix x FaceRace x WordValence)
bar = ggplot(dat, aes(FaceRace, RT.winz, fill = WordValence))
bar + 
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~ParRace + Fix) +
  coord_cartesian(ylim=c(500,650)) +
  ylab("Reaction Time") +
  theme_bw() +
  scale_fill_manual("WordValence", values = c("Neg" = "brown", "Pos" = "lightblue")) +
  ggtitle("All subjects, all trials (Winsorized)- Spring")

ggsave("./Figures/AllSub_Winz_FixXFaceRaceXWordVal.tiff")


ggplot(dat, aes(FaceRace, logRT, fill = WordValence)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Fix) +
  coord_cartesian(ylim=c(6,6.5)) +
  theme_bw() +
  scale_fill_manual("WordValence", values = c("Neg" = "brown", "Pos" = "lightblue")) +
  ggtitle("All subjects, all trials (log transformed)- Spring")

line = ggplot(dat, aes(FaceRace, RT, color = WordValence))
line + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line", aes(group=WordValence), linetype = "dashed") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .2) +
  facet_wrap(~ParRace + Fix)


# Just for white participants
Whitedat = dat[dat$ParRace == "W",]

lineW = ggplot(Whitedat, aes(FaceRace, RT, color = WordValence))
lineW + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line", aes(group=WordValence), linetype = "dashed") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .2) +
  facet_wrap(~Fix)

barW = ggplot(Whitedat, aes(FaceRace, RT.winz, fill = WordValence))
barW + 
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~ParRace + Fix) +
  coord_cartesian(ylim=c(500,600)) +
  theme_bw() +
  scale_fill_manual("WordValence", values = c("Neg" = "brown", "Pos" = "lightblue"))


# All participants, just correct trials
corTrials = dat[dat$Error == 0,]
ggplot(corTrials, aes(FaceRace, RT.winz, fill = WordValence)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~ParRace + Fix) +
  coord_cartesian(ylim=c(500,650)) +
  theme_bw() +
  scale_fill_manual("WordValence", values = c("Neg" = "brown", "Pos" = "lightblue")) +
  ggtitle("All subjects, just correct trials (Winsorized)")

# All participants, not separated by race, just correct trials
ggplot(corTrials, aes(FaceRace, RT.winz, fill = WordValence)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Fix) +
  coord_cartesian(ylim=c(500,600)) +
  theme_bw() +
  scale_fill_manual("WordValence", values = c("Neg" = "brown", "Pos" = "lightblue")) +
  ggtitle("All subjects, just correct trials (Winsorized)")

# All participants, not separated by race, just correct trials, not separated by fix
ggplot(corTrials, aes(FaceRace, RT.winz, fill = WordValence)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
#  facet_wrap(~Fix) +
  coord_cartesian(ylim=c(500,600)) +
  theme_bw() +
  scale_fill_manual("WordValence", values = c("Neg" = "brown", "Pos" = "lightblue")) +
  ggtitle("All subjects, just correct trials (Winsorized)")

# Fix x Congruence x ParRace: all subjects, correct trials
ggplot(corTrials, aes(Fix, RT, fill = Congruent)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  coord_cartesian(ylim=c(500,600)) +
  facet_wrap(~ParRace) +
  theme_bw() +
  scale_fill_manual("Congruent", values = c("Con" = "purple", "Incon" = "red")) +
  ggtitle("All subjects, correct trials (Winsorized)- Spring")



