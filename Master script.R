# Master script

# 1. Aggregate all individual data files into master file
# need to change number of files in script
>> data_aggregator.R
# corrects error column
# adds column for congruence
# adds participant's gender and race information
# changes WordValence and FaceRace to words instead of numbers
# outputs as 
Block2dat.txt

# 2. Check for bad subjects (who are making too many errors)
>> Badsubs.R
# currently none

# 3. Winsorize
>> Winsorize.R
# eliminates subject 036
# winsorizes values that are 3 SD above mean for each subject
# outputs as
Winsorized_data.txt
# Winsorizeds RTs are still skewed. 
# Need to decide whether to exclude sub 24 (high mean RT). Still skewed without 24 but less

# 4. Look at MTCP scores with priming data
>> MTCP.R
# reverse scores items 2, 5, 8, 9, 16, and 17
# averages across all items
# standardizes, writes to file "StandardizedMTCP.txt"
# makes scatterplots looking at relationship between MTCP and
## Black priming effect (Black pos - Black neg (higher numbers is positive bias))
## and White priming effect
# See general positivity bias
# No relationship with MTCP though
# Did same with only "internalizing" scores (items 1, 3, 6, 11, 13)
# Still no relationship

# 5. Factor analysis
>> Factor analysis.R
# Extracts 2 factor and 1 factor solutions
# loadings were manually copy/pasted into "FactorLoadings.txt"

# 6. Look at individual factor scores with priming data
>> MTCP_factorscores.R
# still no relationships with priming data

# 7. Do log transform (instead of Winsorize)
>> log_transform.R
# reads in "Block2dat.txt"
# adds columns for logRT, logRT.winz, and RT.winz
# log transform helps skew but it's still skewed

# 8. Combine Fall and Spring data
>> Combine_dat.R
# n = 25 for spring (2 subs discarded as bad subjects because of fast RT, high error rates)
# n = 54 for fall
# includes raw RT, logRT, logRT.winz, RT.winz
# all trial information plus which term it was collected in
# Outputs:
"TotalCombinedDat.txt"

# 9. Create figures for spring and fall data
>> Figures_combinedData.R

# 10. Do analysis for spring and fall data
>> Analysis_combinedDat.R
# Takes "TotalCombinedDat.txt"