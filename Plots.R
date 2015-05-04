require(ggplot2)


#read in cleaned data set
Block2dat = read.delim("Block2dat.txt", header = T, stringsAsFactors=F)


# Bar graph
ggplot(Block2dat, aes(Race, RT, fill = Congruent)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") 


