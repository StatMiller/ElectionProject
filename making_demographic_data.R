###
###  Produces an RData file with precinct-level demographic data, from
###    the original .zip files from <https://er.ncsbe.gov/downloads.html>
###


## set base-level github directory:
setwd(baseDir <- "~/Dropbox/733/exams/final/733votingProject/")  


##  zip --> txt
library(plyr)
dataDir <- "./raw_data/precinct_level_demographic_data/"
zipF <- list.files(path = dataDir, pattern = "*.zip", full.names = TRUE)
ldply(.data = zipF, .fun = unzip, exdir = dataDir); rm(zipF)
txt_files <- list.files(path = dataDir, pattern = "*.txt")


##  txt --> R
for (i in 1:9) {
  filename <- paste(dataDir, txt_files, sep="")[i]
  dfName <- sub(".txt", "", sub(dataDir, "", filename))
  # cat("\nfile ", i, "\n"); system(paste("head -n 1", filename))
  if (i==1) { # tab delimited, no header
    assign(dfName, read.delim(filename, header=F))
  } else { # tab delimited, with header
    assign(dfName, read.delim(filename, header=T))
  } 
}; rm(filename, dfName, txt_files, i)
colnames(voter_stats_20021105) <- colnames(voter_stats_20041102)


## dropping irrelevant cols
voter_stats_20021105 <- voter_stats_20021105[,c(1:8)]
voter_stats_20041102 <- voter_stats_20041102[,c(1:8)]
voter_stats_20061107 <- voter_stats_20061107[,c(1:8)]
voter_stats_20081104 <- voter_stats_20081104[,c(1:8)]
voter_stats_20101102 <- voter_stats_20101102[,c(1:8)]
voter_stats_20121106 <- voter_stats_20121106[,c(1:9)]
voter_stats_20141104 <- voter_stats_20141104[,c(1,4,5,10,6:9,11)]
voter_stats_20161108 <- voter_stats_20161108[,c(1,4,5,10,6:9,11)]
voter_stats_20181106 <- voter_stats_20181106[,c(1,4,5,10,6:9,11)]


## creating the variables of interest

# 2002
voter_stats_20021105$county_desc <- trimws(voter_stats_20021105$county_desc)
voter_stats_20021105$precinct_abbrv <- trimws(voter_stats_20021105$precinct_abbrv)
voter_stats_20021105$race_code <- trimws(voter_stats_20021105$race_code)
voter_stats_20021105$ethnic_code <- trimws(voter_stats_20021105$ethnic_code)
uniqVals <- unique(voter_stats_20021105[,c("county_desc", "precinct_abbrv")])
demo_2002 <- setNames(data.frame(matrix(ncol=20,nrow=nrow(uniqVals))), 
    c("county","precinct_abbrv", "total_voters","ageInvalid","age18_25","age26_40","age41_65","age65_",
      "race_black", "race_AmIndian", "race_other_noRspns", "race_white", "race_asian", "race_multiple",
      "ethn_hisp", "ethn_nonHisp", "ethn_noRspns", "sex_male", "sex_female", "sex_noRspnse"))
for (i in 1:nrow(uniqVals)) {
  Cval <- as.character(uniqVals[i,][1,1])
  Pval <- as.character(uniqVals[i,][1,2])
  df <- voter_stats_20021105[voter_stats_20021105$precinct_abbrv==Pval & voter_stats_20021105$county_desc==Cval,]
  demo_2002$county[i] <- Cval
  demo_2002$precinct_abbrv[i] <- Pval
  demo_2002$total_voters[i] <- sum(df$total_voters)
  demo_2002$ageInvalid[i] <- sum(df[as.character(df$age)=="Age  < 18 Or Invalid Birth Dates",]$total_voters)
  demo_2002$age18_25[i]   <- sum(df[as.character(df$age)=="Age 18 - 25",]$total_voters)
  demo_2002$age26_40[i]   <- sum(df[as.character(df$age)=="Age 26 - 40",]$total_voters)
  demo_2002$age41_65[i]   <- sum(df[as.character(df$age)=="Age 41 - 65",]$total_voters)
  demo_2002$age65_[i]     <- sum(df[as.character(df$age)=="Age Over 66",]$total_voters)
  demo_2002$race_black[i]         <- sum(df[as.character(df$race_code)=="B",]$total_voters)
  demo_2002$race_AmIndian[i]      <- sum(df[as.character(df$race_code)=="I",]$total_voters)
  demo_2002$race_other_noRspns[i] <- sum(df[is.na(df$race_code) | df$race_code %in% c("O", "U"),]$total_voters)
  demo_2002$race_white[i]         <- sum(df[as.character(df$race_code)=="W",]$total_voters)
  demo_2002$race_asian[i]         <- sum(df[as.character(df$race_code)=="A",]$total_voters)
  demo_2002$race_multiple[i]      <- sum(df[as.character(df$race_code)=="M",]$total_voters)
  demo_2002$ethn_hisp[i]    <- sum(df[as.character(df$ethnic_code)=="HL",]$total_voters)
  demo_2002$ethn_nonHisp[i] <- sum(df[as.character(df$ethnic_code)=="NL",]$total_voters)
  demo_2002$ethn_noRspns[i] <- sum(df[is.na(df$ethnic_code) | as.character(df$ethnic_code)=="UN",]$total_voters)
  demo_2002$sex_male[i]     <- sum(df[as.character(df$sex_code)=="M",]$total_voters)
  demo_2002$sex_female[i]   <- sum(df[as.character(df$sex_code)=="M",]$total_voters)
  demo_2002$sex_noRspnse[i] <- sum(df[is.na(df$sex_code) | as.character(df$sex_code)=="U",]$total_voters)
}; rm(df, voter_stats_20021105)
demo_2002 <- demo_2002[with(demo_2002, order(county, precinct_abbrv)), ]

# 2004
voter_stats_20041102$county_desc <- trimws(voter_stats_20041102$county_desc)
voter_stats_20041102$precinct_abbrv <- trimws(voter_stats_20041102$precinct_abbrv)
voter_stats_20041102$race_code <- trimws(voter_stats_20041102$race_code)
voter_stats_20041102$ethnic_code <- trimws(voter_stats_20041102$ethnic_code)
uniqVals <- unique(voter_stats_20041102[,c("county_desc", "precinct_abbrv")])
demo_2004 <- setNames(data.frame(matrix(ncol=20,nrow=nrow(uniqVals))), 
                      c("county","precinct_abbrv", "total_voters","ageInvalid","age18_25","age26_40","age41_65","age65_",
                        "race_black", "race_AmIndian", "race_other_noRspns", "race_white", "race_asian", "race_multiple",
                        "ethn_hisp", "ethn_nonHisp", "ethn_noRspns", "sex_male", "sex_female", "sex_noRspnse"))
for (i in 1:nrow(uniqVals)) {
  Cval <- as.character(uniqVals[i,][1,1])
  Pval <- as.character(uniqVals[i,][1,2])
  df <- voter_stats_20041102[voter_stats_20041102$precinct_abbrv==Pval & voter_stats_20041102$county_desc==Cval,]
  demo_2004$county[i] <- Cval
  demo_2004$precinct_abbrv[i] <- Pval
  demo_2004$total_voters[i] <- sum(df$total_voters)
  demo_2004$ageInvalid[i] <- sum(df[as.character(df$age)=="Age  < 18 Or Invalid Birth Dates",]$total_voters)
  demo_2004$age18_25[i]   <- sum(df[as.character(df$age)=="Age 18 - 25",]$total_voters)
  demo_2004$age26_40[i]   <- sum(df[as.character(df$age)=="Age 26 - 40",]$total_voters)
  demo_2004$age41_65[i]   <- sum(df[as.character(df$age)=="Age 41 - 65",]$total_voters)
  demo_2004$age65_[i]     <- sum(df[as.character(df$age)=="Age Over 66",]$total_voters)
  demo_2004$race_black[i]         <- sum(df[as.character(df$race_code)=="B",]$total_voters)
  demo_2004$race_AmIndian[i]      <- sum(df[as.character(df$race_code)=="I",]$total_voters)
  demo_2004$race_other_noRspns[i] <- sum(df[is.na(df$race_code) | df$race_code %in% c("O", "U"),]$total_voters)
  demo_2004$race_white[i]         <- sum(df[as.character(df$race_code)=="W",]$total_voters)
  demo_2004$race_asian[i]         <- sum(df[as.character(df$race_code)=="A",]$total_voters)
  demo_2004$race_multiple[i]      <- sum(df[as.character(df$race_code)=="M",]$total_voters)
  demo_2004$ethn_hisp[i]    <- sum(df[as.character(df$ethnic_code)=="HL",]$total_voters)
  demo_2004$ethn_nonHisp[i] <- sum(df[as.character(df$ethnic_code)=="NL",]$total_voters)
  demo_2004$ethn_noRspns[i] <- sum(df[is.na(df$ethnic_code) | as.character(df$ethnic_code)=="UN",]$total_voters)
  demo_2004$sex_male[i]     <- sum(df[as.character(df$sex_code)=="M",]$total_voters)
  demo_2004$sex_female[i]   <- sum(df[as.character(df$sex_code)=="M",]$total_voters)
  demo_2004$sex_noRspnse[i] <- sum(df[is.na(df$sex_code) | as.character(df$sex_code)=="U",]$total_voters)
}; rm(df, voter_stats_20041102)
demo_2004 <- demo_2004[with(demo_2004, order(county, precinct_abbrv)), ]

# 2006
voter_stats_20061107$county_desc <- trimws(voter_stats_20061107$county_desc)
voter_stats_20061107$precinct_abbrv <- trimws(voter_stats_20061107$precinct_abbrv)
voter_stats_20061107$race_code <- trimws(voter_stats_20061107$race_code)
voter_stats_20061107$ethnic_code <- trimws(voter_stats_20061107$ethnic_code)
uniqVals <- unique(voter_stats_20061107[,c("county_desc", "precinct_abbrv")])
demo_2006 <- setNames(data.frame(matrix(ncol=20,nrow=nrow(uniqVals))), 
                      c("county","precinct_abbrv", "total_voters","ageInvalid","age18_25","age26_40","age41_65","age65_",
                        "race_black", "race_AmIndian", "race_other_noRspns", "race_white", "race_asian", "race_multiple",
                        "ethn_hisp", "ethn_nonHisp", "ethn_noRspns", "sex_male", "sex_female", "sex_noRspnse"))
for (i in 1:nrow(uniqVals)) {
  Cval <- as.character(uniqVals[i,][1,1])
  Pval <- as.character(uniqVals[i,][1,2])
  df <- voter_stats_20061107[voter_stats_20061107$precinct_abbrv==Pval & voter_stats_20061107$county_desc==Cval,]
  demo_2006$county[i] <- Cval
  demo_2006$precinct_abbrv[i] <- Pval
  demo_2006$total_voters[i] <- sum(df$total_voters)
  demo_2006$ageInvalid[i] <- sum(df[as.character(df$age)=="Age  < 18 Or Invalid Birth Dates",]$total_voters)
  demo_2006$age18_25[i]   <- sum(df[as.character(df$age)=="Age 18 - 25",]$total_voters)
  demo_2006$age26_40[i]   <- sum(df[as.character(df$age)=="Age 26 - 40",]$total_voters)
  demo_2006$age41_65[i]   <- sum(df[as.character(df$age)=="Age 41 - 65",]$total_voters)
  demo_2006$age65_[i]     <- sum(df[as.character(df$age)=="Age Over 66",]$total_voters)
  demo_2006$race_black[i]         <- sum(df[as.character(df$race_code)=="B",]$total_voters)
  demo_2006$race_AmIndian[i]      <- sum(df[as.character(df$race_code)=="I",]$total_voters)
  demo_2006$race_other_noRspns[i] <- sum(df[is.na(df$race_code) | df$race_code %in% c("O", "U"),]$total_voters)
  demo_2006$race_white[i]         <- sum(df[as.character(df$race_code)=="W",]$total_voters)
  demo_2006$race_asian[i]         <- sum(df[as.character(df$race_code)=="A",]$total_voters)
  demo_2006$race_multiple[i]      <- sum(df[as.character(df$race_code)=="M",]$total_voters)
  demo_2006$ethn_hisp[i]    <- sum(df[as.character(df$ethnic_code)=="HL",]$total_voters)
  demo_2006$ethn_nonHisp[i] <- sum(df[as.character(df$ethnic_code)=="NL",]$total_voters)
  demo_2006$ethn_noRspns[i] <- sum(df[is.na(df$ethnic_code) | as.character(df$ethnic_code)=="UN",]$total_voters)
  demo_2006$sex_male[i]     <- sum(df[as.character(df$sex_code)=="M",]$total_voters)
  demo_2006$sex_female[i]   <- sum(df[as.character(df$sex_code)=="M",]$total_voters)
  demo_2006$sex_noRspnse[i] <- sum(df[is.na(df$sex_code) | as.character(df$sex_code)=="U",]$total_voters)
}; rm(df, voter_stats_20061107)
demo_2006 <- demo_2006[with(demo_2006, order(county, precinct_abbrv)), ]

# 2008
voter_stats_20081104$county_desc <- trimws(voter_stats_20081104$county_desc)
voter_stats_20081104$precinct_abbrv <- trimws(voter_stats_20081104$precinct_abbrv)
voter_stats_20081104$race_code <- trimws(voter_stats_20081104$race_code)
voter_stats_20081104$ethnic_code <- trimws(voter_stats_20081104$ethnic_code)
uniqVals <- unique(voter_stats_20081104[,c("county_desc", "precinct_abbrv")])
demo_2008 <- setNames(data.frame(matrix(ncol=20,nrow=nrow(uniqVals))), 
                      c("county","precinct_abbrv", "total_voters","ageInvalid","age18_25","age26_40","age41_65","age65_",
                        "race_black", "race_AmIndian", "race_other_noRspns", "race_white", "race_asian", "race_multiple",
                        "ethn_hisp", "ethn_nonHisp", "ethn_noRspns", "sex_male", "sex_female", "sex_noRspnse"))
for (i in 1:nrow(uniqVals)) {
  Cval <- as.character(uniqVals[i,][1,1])
  Pval <- as.character(uniqVals[i,][1,2])
  df <- voter_stats_20081104[voter_stats_20081104$precinct_abbrv==Pval & voter_stats_20081104$county_desc==Cval,]
  demo_2008$county[i] <- Cval
  demo_2008$precinct_abbrv[i] <- Pval
  demo_2008$total_voters[i] <- sum(df$total_voters)
  demo_2008$ageInvalid[i] <- sum(df[as.character(df$age)=="Age  < 18 Or Invalid Birth Dates",]$total_voters)
  demo_2008$age18_25[i]   <- sum(df[as.character(df$age)=="Age 18 - 25",]$total_voters)
  demo_2008$age26_40[i]   <- sum(df[as.character(df$age)=="Age 26 - 40",]$total_voters)
  demo_2008$age41_65[i]   <- sum(df[as.character(df$age)=="Age 41 - 65",]$total_voters)
  demo_2008$age65_[i]     <- sum(df[as.character(df$age)=="Age Over 66",]$total_voters)
  demo_2008$race_black[i]         <- sum(df[as.character(df$race_code)=="B",]$total_voters)
  demo_2008$race_AmIndian[i]      <- sum(df[as.character(df$race_code)=="I",]$total_voters)
  demo_2008$race_other_noRspns[i] <- sum(df[is.na(df$race_code) | df$race_code %in% c("O", "U"),]$total_voters)
  demo_2008$race_white[i]         <- sum(df[as.character(df$race_code)=="W",]$total_voters)
  demo_2008$race_asian[i]         <- sum(df[as.character(df$race_code)=="A",]$total_voters)
  demo_2008$race_multiple[i]      <- sum(df[as.character(df$race_code)=="M",]$total_voters)
  demo_2008$ethn_hisp[i]    <- sum(df[as.character(df$ethnic_code)=="HL",]$total_voters)
  demo_2008$ethn_nonHisp[i] <- sum(df[as.character(df$ethnic_code)=="NL",]$total_voters)
  demo_2008$ethn_noRspns[i] <- sum(df[is.na(df$ethnic_code) | as.character(df$ethnic_code)=="UN",]$total_voters)
  demo_2008$sex_male[i]     <- sum(df[as.character(df$sex_code)=="M",]$total_voters)
  demo_2008$sex_female[i]   <- sum(df[as.character(df$sex_code)=="M",]$total_voters)
  demo_2008$sex_noRspnse[i] <- sum(df[is.na(df$sex_code) | as.character(df$sex_code)=="U",]$total_voters)
}; rm(df, voter_stats_20081104)
demo_2008 <- demo_2008[with(demo_2008, order(county, precinct_abbrv)), ]

# 2010
voter_stats_20101102$county_desc <- trimws(voter_stats_20101102$county_desc)
voter_stats_20101102$precinct_abbrv <- trimws(voter_stats_20101102$precinct_abbrv)
voter_stats_20101102$race_code <- trimws(voter_stats_20101102$race_code)
voter_stats_20101102$ethnic_code <- trimws(voter_stats_20101102$ethnic_code)
uniqVals <- unique(voter_stats_20101102[,c("county_desc", "precinct_abbrv")])
demo_2010 <- setNames(data.frame(matrix(ncol=20,nrow=nrow(uniqVals))), 
                      c("county","precinct_abbrv", "total_voters","ageInvalid","age18_25","age26_40","age41_65","age65_",
                        "race_black", "race_AmIndian", "race_other_noRspns", "race_white", "race_asian", "race_multiple",
                        "ethn_hisp", "ethn_nonHisp", "ethn_noRspns", "sex_male", "sex_female", "sex_noRspnse"))
for (i in 1:nrow(uniqVals)) {
  Cval <- as.character(uniqVals[i,][1,1])
  Pval <- as.character(uniqVals[i,][1,2])
  df <- voter_stats_20101102[voter_stats_20101102$precinct_abbrv==Pval & voter_stats_20101102$county_desc==Cval,]
  demo_2010$county[i] <- Cval
  demo_2010$precinct_abbrv[i] <- Pval
  demo_2010$total_voters[i] <- sum(df$total_voters)
  demo_2010$ageInvalid[i] <- sum(df[as.character(df$age)=="Age  < 18 Or Invalid Birth Dates",]$total_voters)
  demo_2010$age18_25[i]   <- sum(df[as.character(df$age)=="Age 18 - 25",]$total_voters)
  demo_2010$age26_40[i]   <- sum(df[as.character(df$age)=="Age 26 - 40",]$total_voters)
  demo_2010$age41_65[i]   <- sum(df[as.character(df$age)=="Age 41 - 65",]$total_voters)
  demo_2010$age65_[i]     <- sum(df[as.character(df$age)=="Age Over 66",]$total_voters)
  demo_2010$race_black[i]         <- sum(df[as.character(df$race_code)=="B",]$total_voters)
  demo_2010$race_AmIndian[i]      <- sum(df[as.character(df$race_code)=="I",]$total_voters)
  demo_2010$race_other_noRspns[i] <- sum(df[is.na(df$race_code) | df$race_code %in% c("O", "U"),]$total_voters)
  demo_2010$race_white[i]         <- sum(df[as.character(df$race_code)=="W",]$total_voters)
  demo_2010$race_asian[i]         <- sum(df[as.character(df$race_code)=="A",]$total_voters)
  demo_2010$race_multiple[i]      <- sum(df[as.character(df$race_code)=="M",]$total_voters)
  demo_2010$ethn_hisp[i]    <- sum(df[as.character(df$ethnic_code)=="HL",]$total_voters)
  demo_2010$ethn_nonHisp[i] <- sum(df[as.character(df$ethnic_code)=="NL",]$total_voters)
  demo_2010$ethn_noRspns[i] <- sum(df[is.na(df$ethnic_code) | as.character(df$ethnic_code)=="UN",]$total_voters)
  demo_2010$sex_male[i]     <- sum(df[as.character(df$sex_code)=="M",]$total_voters)
  demo_2010$sex_female[i]   <- sum(df[as.character(df$sex_code)=="M",]$total_voters)
  demo_2010$sex_noRspnse[i] <- sum(df[is.na(df$sex_code) | as.character(df$sex_code)=="U",]$total_voters)
}; rm(df, voter_stats_20101102)
demo_2010 <- demo_2010[with(demo_2010, order(county, precinct_abbrv)), ]

# 2012
voter_stats_20121106$county_desc <- trimws(voter_stats_20121106$county_desc)
voter_stats_20121106$precinct_abbrv <- trimws(voter_stats_20121106$precinct_abbrv)
voter_stats_20121106$vtd_abbrv <- trimws(voter_stats_20121106$vtd_abbrv)
voter_stats_20121106$race_code <- trimws(voter_stats_20121106$race_code)
voter_stats_20121106$ethnic_code <- trimws(voter_stats_20121106$ethnic_code)
uniqVals <- unique(voter_stats_20121106[,c("county_desc", "precinct_abbrv")])
demo_2012 <- setNames(data.frame(matrix(ncol=21,nrow=nrow(uniqVals))), 
                      c("county","precinct_abbrv", "vtd_abbrv", "total_voters","ageInvalid","age18_25","age26_40","age41_65","age65_",
                        "race_black", "race_AmIndian", "race_other_noRspns", "race_white", "race_asian", "race_multiple",
                        "ethn_hisp", "ethn_nonHisp", "ethn_noRspns", "sex_male", "sex_female", "sex_noRspnse"))
for (i in 1:nrow(uniqVals)) {
  Cval <- as.character(uniqVals[i,][1,1])
  Pval <- as.character(uniqVals[i,][1,2])
  df <- voter_stats_20121106[voter_stats_20121106$precinct_abbrv==Pval & voter_stats_20121106$county_desc==Cval,]
  demo_2012$county[i] <- Cval
  demo_2012$precinct_abbrv[i] <- Pval
  demo_2012$vtd_abbrv[i] <- df$vtd_abbrv[1]
  demo_2012$total_voters[i] <- sum(df$total_voters)
  demo_2012$ageInvalid[i] <- sum(df[as.character(df$age)=="Age  < 18 Or Invalid Birth Dates",]$total_voters)
  demo_2012$age18_25[i]   <- sum(df[as.character(df$age)=="Age 18 - 25",]$total_voters)
  demo_2012$age26_40[i]   <- sum(df[as.character(df$age)=="Age 26 - 40",]$total_voters)
  demo_2012$age41_65[i]   <- sum(df[as.character(df$age)=="Age 41 - 65",]$total_voters)
  demo_2012$age65_[i]     <- sum(df[as.character(df$age)=="Age Over 66",]$total_voters)
  demo_2012$race_black[i]         <- sum(df[as.character(df$race_code)=="B",]$total_voters)
  demo_2012$race_AmIndian[i]      <- sum(df[as.character(df$race_code)=="I",]$total_voters)
  demo_2012$race_other_noRspns[i] <- sum(df[is.na(df$race_code) | df$race_code %in% c("O", "U"),]$total_voters)
  demo_2012$race_white[i]         <- sum(df[as.character(df$race_code)=="W",]$total_voters)
  demo_2012$race_asian[i]         <- sum(df[as.character(df$race_code)=="A",]$total_voters)
  demo_2012$race_multiple[i]      <- sum(df[as.character(df$race_code)=="M",]$total_voters)
  demo_2012$ethn_hisp[i]    <- sum(df[as.character(df$ethnic_code)=="HL",]$total_voters)
  demo_2012$ethn_nonHisp[i] <- sum(df[as.character(df$ethnic_code)=="NL",]$total_voters)
  demo_2012$ethn_noRspns[i] <- sum(df[is.na(df$ethnic_code) | as.character(df$ethnic_code)=="UN",]$total_voters)
  demo_2012$sex_male[i]     <- sum(df[as.character(df$sex_code)=="M",]$total_voters)
  demo_2012$sex_female[i]   <- sum(df[as.character(df$sex_code)=="M",]$total_voters)
  demo_2012$sex_noRspnse[i] <- sum(df[is.na(df$sex_code) | as.character(df$sex_code)=="U",]$total_voters)
}; rm(df, voter_stats_20121106)
demo_2012 <- demo_2012[with(demo_2012, order(county, precinct_abbrv)), ]

# 2014
voter_stats_20141104$county_desc <- trimws(voter_stats_20141104$county_desc)
voter_stats_20141104$precinct_abbrv <- trimws(voter_stats_20141104$precinct_abbrv)
voter_stats_20141104$vtd_abbrv <- trimws(voter_stats_20141104$vtd_abbrv)
voter_stats_20141104$race_code <- trimws(voter_stats_20141104$race_code)
voter_stats_20141104$ethnic_code <- trimws(voter_stats_20141104$ethnic_code)
uniqVals <- unique(voter_stats_20141104[,c("county_desc", "precinct_abbrv")])
demo_2014 <- setNames(data.frame(matrix(ncol=21,nrow=nrow(uniqVals))), 
                      c("county","precinct_abbrv", "vtd_abbrv", "total_voters","ageInvalid","age18_25","age26_40","age41_65","age65_",
                        "race_black", "race_AmIndian", "race_other_noRspns", "race_white", "race_asian", "race_multiple",
                        "ethn_hisp", "ethn_nonHisp", "ethn_noRspns", "sex_male", "sex_female", "sex_noRspnse"))
for (i in 1:nrow(uniqVals)) {
  Cval <- as.character(uniqVals[i,][1,1])
  Pval <- as.character(uniqVals[i,][1,2])
  df <- voter_stats_20141104[voter_stats_20141104$precinct_abbrv==Pval & voter_stats_20141104$county_desc==Cval,]
  demo_2014$county[i] <- Cval
  demo_2014$precinct_abbrv[i] <- Pval
  demo_2014$vtd_abbrv[i] <- df$vtd_abbrv[1]
  demo_2014$total_voters[i] <- sum(df$total_voters)
  demo_2014$ageInvalid[i] <- sum(df[as.character(df$age)=="Age  < 18 Or Invalid Birth Dates",]$total_voters)
  demo_2014$age18_25[i]   <- sum(df[as.character(df$age)=="Age 18 - 25",]$total_voters)
  demo_2014$age26_40[i]   <- sum(df[as.character(df$age)=="Age 26 - 40",]$total_voters)
  demo_2014$age41_65[i]   <- sum(df[as.character(df$age)=="Age 41 - 65",]$total_voters)
  demo_2014$age65_[i]     <- sum(df[as.character(df$age)=="Age Over 66",]$total_voters)
  demo_2014$race_black[i]         <- sum(df[as.character(df$race_code)=="B",]$total_voters)
  demo_2014$race_AmIndian[i]      <- sum(df[as.character(df$race_code)=="I",]$total_voters)
  demo_2014$race_other_noRspns[i] <- sum(df[is.na(df$race_code) | df$race_code %in% c("O", "U"),]$total_voters)
  demo_2014$race_white[i]         <- sum(df[as.character(df$race_code)=="W",]$total_voters)
  demo_2014$race_asian[i]         <- sum(df[as.character(df$race_code)=="A",]$total_voters)
  demo_2014$race_multiple[i]      <- sum(df[as.character(df$race_code)=="M",]$total_voters)
  demo_2014$ethn_hisp[i]    <- sum(df[as.character(df$ethnic_code)=="HL",]$total_voters)
  demo_2014$ethn_nonHisp[i] <- sum(df[as.character(df$ethnic_code)=="NL",]$total_voters)
  demo_2014$ethn_noRspns[i] <- sum(df[is.na(df$ethnic_code) | as.character(df$ethnic_code)=="UN",]$total_voters)
  demo_2014$sex_male[i]     <- sum(df[as.character(df$sex_code)=="M",]$total_voters)
  demo_2014$sex_female[i]   <- sum(df[as.character(df$sex_code)=="M",]$total_voters)
  demo_2014$sex_noRspnse[i] <- sum(df[is.na(df$sex_code) | as.character(df$sex_code)=="U",]$total_voters)
}; rm(df, voter_stats_20141104)
demo_2014 <- demo_2014[with(demo_2014, order(county, precinct_abbrv)), ]

# 2016
voter_stats_20161108$county_desc <- trimws(voter_stats_20161108$county_desc)
voter_stats_20161108$precinct_abbrv <- trimws(voter_stats_20161108$precinct_abbrv)
voter_stats_20161108$vtd_abbrv <- trimws(voter_stats_20161108$vtd_abbrv)
voter_stats_20161108$race_code <- trimws(voter_stats_20161108$race_code)
voter_stats_20161108$ethnic_code <- trimws(voter_stats_20161108$ethnic_code)
uniqVals <- unique(voter_stats_20161108[,c("county_desc", "precinct_abbrv")])
demo_2016 <- setNames(data.frame(matrix(ncol=21,nrow=nrow(uniqVals))), 
                      c("county","precinct_abbrv", "vtd_abbrv", "total_voters","ageInvalid","age18_25","age26_40","age41_65","age65_",
                        "race_black", "race_AmIndian", "race_other_noRspns", "race_white", "race_asian", "race_multiple",
                        "ethn_hisp", "ethn_nonHisp", "ethn_noRspns", "sex_male", "sex_female", "sex_noRspnse"))
for (i in 1:nrow(uniqVals)) {
  Cval <- as.character(uniqVals[i,][1,1])
  Pval <- as.character(uniqVals[i,][1,2])
  df <- voter_stats_20161108[voter_stats_20161108$precinct_abbrv==Pval & voter_stats_20161108$county_desc==Cval,]
  demo_2016$county[i] <- Cval
  demo_2016$precinct_abbrv[i] <- Pval
  demo_2016$vtd_abbrv[i] <- df$vtd_abbrv[1]
  demo_2016$total_voters[i] <- sum(df$total_voters)
  demo_2016$ageInvalid[i] <- sum(df[as.character(df$age)=="Age  < 18 Or Invalid Birth Dates",]$total_voters)
  demo_2016$age18_25[i]   <- sum(df[as.character(df$age)=="Age 18 - 25",]$total_voters)
  demo_2016$age26_40[i]   <- sum(df[as.character(df$age)=="Age 26 - 40",]$total_voters)
  demo_2016$age41_65[i]   <- sum(df[as.character(df$age)=="Age 41 - 65",]$total_voters)
  demo_2016$age65_[i]     <- sum(df[as.character(df$age)=="Age Over 66",]$total_voters)
  demo_2016$race_black[i]         <- sum(df[as.character(df$race_code)=="B",]$total_voters)
  demo_2016$race_AmIndian[i]      <- sum(df[as.character(df$race_code)=="I",]$total_voters)
  demo_2016$race_other_noRspns[i] <- sum(df[is.na(df$race_code) | df$race_code %in% c("O", "U"),]$total_voters)
  demo_2016$race_white[i]         <- sum(df[as.character(df$race_code)=="W",]$total_voters)
  demo_2016$race_asian[i]         <- sum(df[as.character(df$race_code)=="A",]$total_voters)
  demo_2016$race_multiple[i]      <- sum(df[as.character(df$race_code)=="M",]$total_voters)
  demo_2016$ethn_hisp[i]    <- sum(df[as.character(df$ethnic_code)=="HL",]$total_voters)
  demo_2016$ethn_nonHisp[i] <- sum(df[as.character(df$ethnic_code)=="NL",]$total_voters)
  demo_2016$ethn_noRspns[i] <- sum(df[is.na(df$ethnic_code) | as.character(df$ethnic_code)=="UN",]$total_voters)
  demo_2016$sex_male[i]     <- sum(df[as.character(df$sex_code)=="M",]$total_voters)
  demo_2016$sex_female[i]   <- sum(df[as.character(df$sex_code)=="M",]$total_voters)
  demo_2016$sex_noRspnse[i] <- sum(df[is.na(df$sex_code) | as.character(df$sex_code)=="U",]$total_voters)
}; rm(df, voter_stats_20161108)
demo_2016 <- demo_2016[with(demo_2016, order(county, precinct_abbrv)), ]

# 2018
voter_stats_20181106$county_desc <- trimws(voter_stats_20181106$county_desc)
voter_stats_20181106$precinct_abbrv <- trimws(voter_stats_20181106$precinct_abbrv)
voter_stats_20181106$vtd_abbrv <- trimws(voter_stats_20181106$vtd_abbrv)
voter_stats_20181106$race_code <- trimws(voter_stats_20181106$race_code)
voter_stats_20181106$ethnic_code <- trimws(voter_stats_20181106$ethnic_code)
uniqVals <- unique(voter_stats_20181106[,c("county_desc", "precinct_abbrv")])
demo_2018 <- setNames(data.frame(matrix(ncol=21,nrow=nrow(uniqVals))), 
                      c("county","precinct_abbrv", "vtd_abbrv", "total_voters","ageInvalid","age18_25","age26_40","age41_65","age65_",
                        "race_black", "race_AmIndian", "race_other_noRspns", "race_white", "race_asian", "race_multiple",
                        "ethn_hisp", "ethn_nonHisp", "ethn_noRspns", "sex_male", "sex_female", "sex_noRspnse"))
for (i in 1:nrow(uniqVals)) {
  Cval <- as.character(uniqVals[i,][1,1])
  Pval <- as.character(uniqVals[i,][1,2])
  df <- voter_stats_20181106[voter_stats_20181106$precinct_abbrv==Pval & voter_stats_20181106$county_desc==Cval,]
  demo_2018$county[i] <- Cval
  demo_2018$precinct_abbrv[i] <- Pval
  demo_2018$vtd_abbrv[i] <- df$vtd_abbrv[1]
  demo_2018$total_voters[i] <- sum(df$total_voters)
  demo_2018$ageInvalid[i] <- sum(df[as.character(df$age)=="Age  < 18 Or Invalid Birth Dates",]$total_voters)
  demo_2018$age18_25[i]   <- sum(df[as.character(df$age)=="Age 18 - 25",]$total_voters)
  demo_2018$age26_40[i]   <- sum(df[as.character(df$age)=="Age 26 - 40",]$total_voters)
  demo_2018$age41_65[i]   <- sum(df[as.character(df$age)=="Age 41 - 65",]$total_voters)
  demo_2018$age65_[i]     <- sum(df[as.character(df$age)=="Age Over 66",]$total_voters)
  demo_2018$race_black[i]         <- sum(df[as.character(df$race_code)=="B",]$total_voters)
  demo_2018$race_AmIndian[i]      <- sum(df[as.character(df$race_code)=="I",]$total_voters)
  demo_2018$race_other_noRspns[i] <- sum(df[is.na(df$race_code) | df$race_code %in% c("O", "U"),]$total_voters)
  demo_2018$race_white[i]         <- sum(df[as.character(df$race_code)=="W",]$total_voters)
  demo_2018$race_asian[i]         <- sum(df[as.character(df$race_code)=="A",]$total_voters)
  demo_2018$race_multiple[i]      <- sum(df[as.character(df$race_code)=="M",]$total_voters)
  demo_2018$ethn_hisp[i]    <- sum(df[as.character(df$ethnic_code)=="HL",]$total_voters)
  demo_2018$ethn_nonHisp[i] <- sum(df[as.character(df$ethnic_code)=="NL",]$total_voters)
  demo_2018$ethn_noRspns[i] <- sum(df[is.na(df$ethnic_code) | as.character(df$ethnic_code)=="UN",]$total_voters)
  demo_2018$sex_male[i]     <- sum(df[as.character(df$sex_code)=="M",]$total_voters)
  demo_2018$sex_female[i]   <- sum(df[as.character(df$sex_code)=="M",]$total_voters)
  demo_2018$sex_noRspnse[i] <- sum(df[is.na(df$sex_code) | as.character(df$sex_code)=="U",]$total_voters)
}; rm(df, voter_stats_20181106, uniqVals, i, Pval, Cval)
demo_2018 <- demo_2018[with(demo_2018, order(county, precinct_abbrv)), ]




