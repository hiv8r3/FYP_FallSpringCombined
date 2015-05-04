require(ggplot)

# read in data
dat = read.delim("TotalCombinedDat.txt")
# data excludes two subjects from fall (fast RT/high error rate)
# includes raw RT, logRT, logRT.winz and RT.winz

# separate out fall and spring 
fall = dat[dat$Term == "fall",]
spring = dat[dat$Term == "spring",]

# 1. Figures for Fall semester
# Just use correct trials
corFall = fall[fall$Error == 0,]

apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'),
        legend.title=element_blank())

formatting = theme_bw(base_size = 16) + 
  theme(axis.text.x = element_text(size = 16),
        plot.title = element_text(vjust = 1.5, size = 16))


# (1) FaceRace x WordValence x Fix: all subjects (minus bad) together, correct trials
ggplot(corFall, aes(FaceRace, RT, fill = WordValence)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Fix) +
  coord_cartesian(ylim=c(600,775)) +
  formatting +
  scale_fill_manual("WordValence", values = c("Neg" = "brown", "Pos" = "lightblue")) +
  ggtitle("FaceRace x WordValence x Fix (correct trials, raw RT): Fall")
ggsave("./Figures/For SPAM presentation/FaceRace x WordVal x Fix_Fall.wmf")

# logRT (correct)
ggplot(corFall, aes(FaceRace, logRT, fill = WordValence)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Fix) +
  coord_cartesian(ylim=c(6.3,6.6)) +
  formatting +
  scale_fill_manual("WordValence", values = c("Neg" = "brown", "Pos" = "lightblue")) +
  ggtitle("FaceRace x WordValence x Fix (correct trials, logRT): Fall")


# (2) Congruence x Fix: all subjects (minus bad) together, correct trials
# USE THIS
ggplot(corFall, aes(Fix, RT, fill = Congruent)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge", bar_width = .1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  coord_cartesian(ylim=c(650,750)) +
  formatting +
  labs(x ="Fixation", y = "Reaction Time") +
  theme(plot.title = element_blank()) +
  scale_fill_manual("Congruent", values = c("Con" = "darkgreen", "Incon" = "plum")) +
  ggtitle("Congruent x Fix (correct trials, raw RT): Fall")
ggsave("./Figures/For SPAM presentation/Congruent x Fix_Fall.wmf")

# logRT (correct)
ggplot(corFall, aes(Fix, logRT, fill = Congruent)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  coord_cartesian(ylim=c(6.4,6.6)) +
  formatting +
  scale_fill_manual("Congruent", values = c("Con" = "darkgreen", "Incon" = "plum")) +
  ggtitle("Congruent x Fix (correct trials, logRT): Fall")



# 2. Figures for Spring semester
# Just use correct trials
corSpring = spring[spring$Error == 0,]


# (1) FaceRace x WordValence x Fix: all subjects (minus bad) together, correct trials
ggplot(corSpring, aes(FaceRace, RT, fill = WordValence)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Fix) +
  coord_cartesian(ylim=c(475,650)) +
  formatting +
  scale_fill_manual("WordValence", values = c("Neg" = "brown", "Pos" = "lightblue")) +
  ggtitle("FaceRace x WordValence x Fix (correct trials, raw RT): Spring")
ggsave("./Figures/For SPAM presentation/FaceRace x WordVal x Fix_Spring.wmf")

# logRT (correct)
ggplot(corSpring, aes(FaceRace, logRT, fill = WordValence)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Fix) +
  coord_cartesian(ylim=c(6.2,6.4)) +
  formatting +
  scale_fill_manual("WordValence", values = c("Neg" = "brown", "Pos" = "lightblue")) +
  ggtitle("FaceRace x WordValence x Fix (correct trials, logRT): Spring")


# (2) Congruence x Fix: all subjects (minus bad) together, correct trials
# USE THIS
ggplot(corSpring, aes(Fix, RT, fill = Congruent)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge", geom_width = .1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  coord_cartesian(ylim=c(525,625)) +
  formatting +
  labs(x ="Fixation", y = "Reaction Time") +
  theme(plot.title = element_blank()) +
  scale_fill_manual("Congruent", values = c("Con" = "darkgreen", "Incon" = "plum")) +
  ggtitle("Congruent x Fix (correct trials, raw RT): Spring")
ggsave("./Figures/For SPAM presentation/Congruent x Fix_Spring.wmf")

# logRT (correct)
ggplot(corSpring, aes(Fix, logRT, fill = Congruent)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  coord_cartesian(ylim=c(6.4,6.6)) +
  formatting +
  scale_fill_manual("Congruent", values = c("Con" = "darkgreen", "Incon" = "plum")) +
  ggtitle("Congruent x Fix (correct trials, logRT): Spring")

