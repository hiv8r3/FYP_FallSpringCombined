require(ggplot2)

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
        axis.title = element_text(face = "bold"),
        plot.title = element_text(vjust = 1.5, size = 16))


# (1) FaceRace x WordValence x Fix: all subjects (minus bad) together, correct trials
# Used this for SPAM presentation
ggplot(corFall, aes(FaceRace, RT, fill = WordValence)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Fix) +
  coord_cartesian(ylim=c(600,775)) +
  formatting +
  scale_fill_manual("WordValence", values = c("Neg" = "brown", "Pos" = "lightblue")) 
ggsave("./Figures/ForSpamPresentation/FaceRace x WordVal x Fix_Fall.tiff", width = 9, height = 6)

# (adjusted height to match spring data)
ggplot(corFall, aes(FaceRace, RT, fill = WordValence)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Fix) +
  coord_cartesian(ylim=c(575,775)) +
  formatting +
  scale_fill_manual("WordValence", values = c("Neg" = "brown", "Pos" = "lightblue")) 
ggsave("./Figures/ForSpamPresentation/FaceRace x WordVal x Fix_Fall_adjscale.tiff", width = 9, height = 6)

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
# Used this for SPAM presentation
ggplot(corFall, aes(Fix, RT, fill = Congruent)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge", bar_width = .1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  coord_cartesian(ylim=c(650,750)) +
  formatting +
  labs(x ="Fixation", y = "Reaction Time") +
  theme(plot.title = element_blank()) +
  scale_fill_manual("Congruent", values = c("Con" = "darkgreen", "Incon" = "plum")) +
  ggtitle("Congruent x Fix (correct trials, raw RT): Fall")
ggsave("./Figures/ForSpamPresentation/Congruent x Fix_Fall.tiff", width = 7, height = 6)

# logRT (correct)
ggplot(corFall, aes(Fix, logRT, fill = Congruent)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  coord_cartesian(ylim=c(6.4,6.6)) +
  formatting +
  scale_fill_manual("Congruent", values = c("Con" = "darkgreen", "Incon" = "plum")) +
  ggtitle("Congruent x Fix (correct trials, logRT): Fall")



# Just playing around, looking at means
ggplot(corFall, aes(Fix, logRT)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge", bar_width = .1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  coord_cartesian(ylim=c(6.25, 6.6))
  








# 2. Figures for Spring semester
# Just use correct trials
corSpring = spring[spring$Error == 0,]


# (1) FaceRace x WordValence x Fix: all subjects (minus bad) together, correct trials
ggplot(corSpring, aes(FaceRace, RT, fill = WordValence)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Fix) +
  coord_cartesian(ylim=c(500, 700)) +
  formatting +
  scale_fill_manual("WordValence", values = c("Neg" = "brown", "Pos" = "lightblue")) 
ggsave("./Figures/ForSpamPresentation/FaceRace x WordVal x Fix_Spring.tiff", width = 9, height = 6)

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

# logRT (correct)
ggplot(corSpring, aes(Fix, logRT, fill = Congruent)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  coord_cartesian(ylim=c(6.2,6.4)) +
  formatting +
  scale_fill_manual("Congruent", values = c("Con" = "darkgreen", "Incon" = "plum")) +
  ggtitle("Congruent x Fix (correct trials, logRT): Spring")


# 3. Combined data together
corData = dat[dat$Error == 0,]

ggplot(corData, aes(Fix, RT, fill = Congruent)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge", geom_width = .1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  coord_cartesian(ylim=c(525,750)) +
  formatting +
  facet_wrap(~Term) +
  labs(x ="Fixation", y = "Reaction Time") +
  theme(plot.title = element_blank()) +
  scale_fill_manual("Congruent", values = c("Con" = "darkgreen", "Incon" = "plum"))
ggsave("./Figures/ForSpamPresentation/Fix x Congruent x Term_allData.tiff", width = 9, height = 6)










# 4. Redone with SE instead of 95% CI
formatting2 = theme_bw(base_size = 16) + 
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(vjust = 1.5, size = 16))


ggplot(corFall, aes(FaceRace, RT, fill = WordValence)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2, mult = 1) + # mult = 1 argument makes it SE instead of 95% CI  
  facet_wrap(~Fix) +
  coord_cartesian(ylim=c(600,775)) +
  formatting2 +
  scale_fill_manual("WordValence", values = c("Neg" = "brown", "Pos" = "lightblue")) +
  labs (y = "Reaction Time", x = "Race of Face Prime", fill = "Word Valence")
ggsave("./Figures/ForSpamPresentation/FaceRace x WordVal x Fix_Fall_SE.tiff", width = 8, height = 5)

ggplot(corFall, aes(Fix, RT, fill = Congruent)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge", bar_width = .1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2, mult = 1) +
  coord_cartesian(ylim=c(650,750)) +
  formatting2 +
  labs(x ="Fixation", y = "Reaction Time") +
  theme(plot.title = element_blank()) +
  scale_fill_manual("Congruent", values = c("Con" = "darkgreen", "Incon" = "plum")) +
  ggtitle("Congruent x Fix (correct trials, raw RT): Fall")
ggsave("./Figures/ForSpamPresentation/Congruent x Fix_Fall_SE.tiff", width = 6, height = 5)

ggplot(corSpring, aes(FaceRace, RT, fill = WordValence)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2, mult = 1) +
  facet_wrap(~Fix) +
  coord_cartesian(ylim=c(500, 700)) +
  formatting2 +
  scale_fill_manual("WordValence", values = c("Neg" = "brown", "Pos" = "lightblue")) +
  labs (y = "Reaction Time", x = "Race of Face Prime", fill = "Word Valence")
ggsave("./Figures/ForSpamPresentation/FaceRace x WordVal x Fix_Spring_SE.tiff", width = 8, height = 5)

ggplot(corData, aes(Fix, RT, fill = Congruent)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge", geom_width = .1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2, mult = 1) +
  coord_cartesian(ylim=c(525,740)) +
  formatting2 +
  facet_wrap(~Term) +
  labs(x ="Fixation", y = "Reaction Time") +
  theme(plot.title = element_blank()) +
  scale_fill_manual("Congruent", values = c("Con" = "darkgreen", "Incon" = "plum"))
ggsave("./Figures/ForSpamPresentation/Fix x Congruent x Term_allData_SE.tiff", width = 9, height = 6)
