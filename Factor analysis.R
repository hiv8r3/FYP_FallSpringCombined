require(dplyr)

# Factor analysis

mtcp = read.delim("./MTCP/StandardizedMTCP.txt")
mtcp = select(mtcp, -c(Factor1score, Factor2score))
mtcp$Subno = as.factor(mtcp$Subno)

noNA = mtcp[!is.na(mtcp$stand1),]
noNA = noNA[!is.na(noNA$stand8),]
noNA = noNA[!is.na(noNA$stand15),]

noNA = select(noNA, -c(Subno))

# Factor analysis
# Maximum Likelihood Factor Analysis
# entering raw data and extracting 2 factors, 
# with varimax rotation 
fit <- factanal(noNA, 2, rotation="varimax", scores="regression")
print(fit, digits=2, cutoff=.001)


# for one factor solution
fit2 <- factanal(noNA, 1, rotation="varimax", scores="regression")
print(fit2, digits=2, cutoff=.001)

