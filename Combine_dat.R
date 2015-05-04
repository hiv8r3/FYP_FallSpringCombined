# Combine data from spring and from fall

# read in data
spring = read.delim("logTransformed_data.txt")
fall = read.delim("Fall_Block2Reorganized_log_winz.txt")

# add column specifying which term the data were collected in
spring$Term = "spring"
fall$Term = "fall"

# combine the two datasets

dat = rbind(fall, spring)

write.table(dat, "TotalCombinedDat.txt", sep="\t", row.names=F)
