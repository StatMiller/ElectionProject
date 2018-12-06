###
###  Produces an RData file with precinct-level voting results, from
###    the original .zip files from <https://er.ncsbe.gov/downloads.html>
###


## set base-level github directory:
setwd(baseDir <- "~/Dropbox/733/exams/final/733votingProject/")  


##  zip --> txt
library(plyr)
dataDir <- "./raw_data/precinct_level_voting_data/"
zipF <- list.files(path = dataDir, pattern = "*.zip", full.names = TRUE)
ldply(.data = zipF, .fun = unzip, exdir = dataDir); rm(zipF)
txt_files <- list.files(path = dataDir, pattern = "*.txt")
  txt_files <- txt_files[ txt_files != "Readme.txt"]


##  txt --> R
for (i in 1:37) {
  filename <- paste(dataDir, txt_files, sep="")[i]
  dfName <- sub(".txt", "", sub(dataDir, "", filename))
  
  ## unfortunately, a few different formats:
  #cat("\nfile ", i, "\n"); system(paste("head -n 1", filename))
  if (i %in% c(1,2,10)) { # tab delimited, no header
    assign(dfName, read.delim(filename, header=F))
  } else if (i %in% c(3:5,11:22)) { # comma delimited, with header
    assign(dfName, read.csv(filename, header=T))
  } else if (i %in% c(6:9,23:37)) { # tab delimited, with header
    assign(dfName, read.delim(filename, header=T))
  } 
}; rm(filename, dfName, txt_files, i)

  
## adding headers to the no-headers ones
colnames(results_pct_20020910) <- colnames(results_pct_20041102)
  colnames(results_pct_20020910)[4] <- "precinct"
colnames(results_pct_20021105) <- colnames(results_pct_20020910)
colnames(results_pct_20080506) <- c("County", "Election.Date", "Precinct", 
      "Contest.Name", "Choice", "Party", "VotingTypeA?", "VotingTypeB?", 
      "VotingTypeC?", "Total.Votes")  # <-- had to guess a bit


## just keeping the US House races;  setting the columns to 
results_pct_20020910 <- results_pct_20020910[grepl("US HOUSE", as.character(results_pct_20020910$contest_name)),]
results_pct_20021105 <- results_pct_20021105[grepl("US HOUSE", as.character(results_pct_20021105$contest_name)),]
results_pct_20040720 <- results_pct_20040720[grepl("US CONGRESS", as.character(results_pct_20040720$contest_name)),]
results_pct_20040817 <- results_pct_20040817[grepl("US CONGRESS", as.character(results_pct_20040817$contest_name)),]
results_pct_20041102 <- results_pct_20041102[grepl("US CONGRESS", as.character(results_pct_20041102$contest_name)),]
results_pct_20060502 <- results_pct_20060502[grepl("US CONGRESS", as.character(results_pct_20060502$contest_name)),]
rm(results_pct_20060530)  # just state races
rm(results_pct_20060912)  # just state races
results_pct_20061107 <- results_pct_20061107[grepl("US CONGRESS", as.character(results_pct_20061107$contest_name)),]
results_pct_20080506 <- results_pct_20080506[grepl("US CONGRESS", as.character(results_pct_20080506$Contest.Name)),]
results_pct_20081104 <- results_pct_20081104[grepl("US HOUSE", as.character(results_pct_20081104$contest)),]
results_pct_20100504 <- results_pct_20100504[grepl("US HOUSE", as.character(results_pct_20100504$contest)),]
results_pct_20100622 <- results_pct_20100622[grepl("US HOUSE", as.character(results_pct_20100622$contest)),]
results_pct_20101102 <- results_pct_20101102[grepl("US HOUSE", as.character(results_pct_20101102$contest)),]
rm(results_pct_20110913)  # just city stuff
rm(results_pct_20111011)  # just city/county stuff
rm(results_pct_20111108)  # just local stuff
results_pct_20120508 <- results_pct_20120508[grepl("US HOUSE", as.character(results_pct_20120508$contest)),]
results_pct_20120717 <- results_pct_20120717[grepl("US HOUSE", as.character(results_pct_20120717$contest)),]
results_pct_20121106 <- results_pct_20121106[grepl("US HOUSE", as.character(results_pct_20121106$contest)),]
rm(results_pct_20130910)  # just local stuff
rm(results_pct_20131008)  # just local stuff
results_pct_20140506 <- results_pct_20140506[grepl("US HOUSE", as.character(results_pct_20140506$Contest.Name)),]
results_pct_20140715 <- results_pct_20140715[grepl("US HOUSE", as.character(results_pct_20140715$Contest.Name)),]
results_pct_20141104 <- results_pct_20141104[grepl("US HOUSE", as.character(results_pct_20141104$Contest.Name)),]
rm(results_pct_20150915)  # just local
rm(results_pct_20151006)  # just local
rm(results_pct_20151103)  # just local
rm(results_pct_20160315)  # just primaries?? no House contests...
results_pct_20160607 <- results_pct_20160607[grepl("US HOUSE", as.character(results_pct_20160607$Contest.Name)),]
results_pct_20161108 <- results_pct_20161108[grepl("US HOUSE", as.character(results_pct_20161108$Contest.Name)),]
rm(results_pct_20170912)  # just local
rm(results_pct_20171010)  # just local
rm(results_pct_20171107)  # just local
results_pct_20180508 <- results_pct_20180508[grepl("US HOUSE", as.character(results_pct_20180508$Contest.Name)),]
rm(results_pct_20180626)  # local
results_pct_20181106 <- results_pct_20181106[grepl("US HOUSE", as.character(results_pct_20181106$Contest.Name)),]


## only keeping the relevant variables: 
#    county   precinct_code   precinct  party   ballot_counts (this can be >=1 vars)
for (dfName in ls(pattern="results_pct_.*")[1:7]) {
  df <- get(dfName)
  df <- df[,c(1,3,4,7,8)]
  assign(dfName, df)
}; rm(df, dfName)
results_pct_20080506 <- results_pct_20080506[,c(1,3,6,7:10)]
results_pct_20081104 <- results_pct_20081104[,c(1,2,9,10:13)]
for (dfName in ls(pattern="results_pct_.*")[10:15]) {
  df <- get(dfName)
  df <- df[,c(1,2,9,10:14)]
  assign(dfName, df)
}; rm(dfName)
for (dfName in ls(pattern="results_pct_.*")[16:21]) {
  df <- get(dfName)
  df <- df[,c(1,3,8,10:14)]
  assign(dfName, df)
}; rm(df, dfName)
results_pct_20181106 <- results_pct_20181106[,c(1,3,8,10:15)]


## ignoring the primaries for now: keeping only November elections
rm(results_pct_20020910, results_pct_20040720, results_pct_20040817, results_pct_20060502,
   results_pct_20080506, results_pct_20100504, results_pct_20100622, results_pct_20120508,
   results_pct_20120717, results_pct_20140506, results_pct_20140715, results_pct_20160607,
   results_pct_20180508)


## generating the variables of interest at the precinct level
#
#   precint/county info      DEM_count   REP_count   total_count

# 2002
results_pct_20021105$county <- trimws(results_pct_20021105$county)
results_pct_20021105$precinct_abbrv <- trimws(results_pct_20021105$precinct_abbrv)
results_pct_20021105$precinct <- trimws(results_pct_20021105$precinct)
uniqVals <- unique(results_pct_20021105[,c("county", "precinct_abbrv")])
votes_2002 <- setNames(data.frame(matrix(ncol=6,nrow=nrow(uniqVals))), 
    c("county","precinct_abbrv","precinct","dem_votes","rep_votes","total_votes"))
for (i in 1:nrow(uniqVals)) {
  Cval <- as.character(uniqVals[i,][1,1])
  Pval <- as.character(uniqVals[i,][1,2])
  df <- results_pct_20021105[results_pct_20021105$precinct_abbrv==Pval & results_pct_20021105$county==Cval ,]
  votes_2002$county[i] <- as.character(df$county[1])
  votes_2002$precinct_abbrv[i] <- as.character(df$precinct_abbrv[1])
  votes_2002$precinct[i] <- as.character(df$precinct[1])
  votes_2002$dem_votes[i] <- sum(df[as.character(df$party_cd) == "DEM",]$ballot_count)
  votes_2002$rep_votes[i] <- sum(df[as.character(df$party_cd) == "REP",]$ballot_count)
  votes_2002$total_votes[i] <- sum(df$ballot_count)
}; rm(df, results_pct_20021105)
votes_2002 <- votes_2002[with(votes_2002, order(county, precinct_abbrv)), ]

# 2004
results_pct_20041102$county <- trimws(results_pct_20041102$county)
results_pct_20041102$precinct_abbrv <- trimws(results_pct_20041102$precinct_abbrv)
results_pct_20041102$precinct <- trimws(results_pct_20041102$precinct)
uniqVals <- unique(results_pct_20041102[,c("county", "precinct_abbrv")])   
votes_2004 <- setNames(data.frame(matrix(ncol=6,nrow=nrow(uniqVals))), 
    c("county","precinct_abbrv","precinct","dem_votes","rep_votes","total_votes"))
for (i in 1:nrow(uniqVals)) {
  Cval <- as.character(uniqVals[i,][1,1])
  Pval <- as.character(uniqVals[i,][1,2])
  df <- results_pct_20041102[results_pct_20041102$precinct_abbrv==Pval & results_pct_20041102$county==Cval,]
  votes_2004$county[i] <- as.character(df$county[1])
  votes_2004$precinct_abbrv[i] <- as.character(df$precinct_abbrv[1])
  votes_2004$precinct[i] <- as.character(df$precinct[1])
  votes_2004$dem_votes[i] <- sum(df[as.character(df$party_cd) == "DEM",]$ballot_count)
  votes_2004$rep_votes[i] <- sum(df[as.character(df$party_cd) == "REP",]$ballot_count)
  votes_2004$total_votes[i] <- sum(df$ballot_count)
}; rm(df, results_pct_20041102)
votes_2004 <- votes_2004[with(votes_2004, order(county, precinct_abbrv)), ]

# 2006
results_pct_20061107$county <- trimws(results_pct_20061107$county)
results_pct_20061107$precinct_abbrv <- trimws(results_pct_20061107$precinct_abbrv)
results_pct_20061107$precinct <- trimws(results_pct_20061107$precinct)
uniqVals <- unique(results_pct_20061107[,c("county", "precinct_abbrv")])
votes_2006 <- setNames(data.frame(matrix(ncol=6,nrow=nrow(uniqVals))), 
    c("county","precinct_abbrv","precinct","dem_votes","rep_votes","total_votes"))
for (i in 1:nrow(uniqVals)) {
  Cval <- as.character(uniqVals[i,][1,1])
  Pval <- as.character(uniqVals[i,][1,2])
  df <- results_pct_20061107[results_pct_20061107$precinct_abbrv==Pval & results_pct_20061107$county==Cval,]
  votes_2006$county[i] <- as.character(df$county[1])
  votes_2006$precinct_abbrv[i] <- as.character(df$precinct_abbrv[1])
  votes_2006$precinct[i] <- as.character(df$precinct[1])
  votes_2006$dem_votes[i] <- sum(df[as.character(df$party_cd) == "DEM",]$ballot_count)
  votes_2006$rep_votes[i] <- sum(df[as.character(df$party_cd) == "REP",]$ballot_count)
  votes_2006$total_votes[i] <- sum(df$ballot_count)
}; rm(df, results_pct_20061107)
votes_2006 <- votes_2006[with(votes_2006, order(county, precinct_abbrv)), ]

# 2008
results_pct_20081104$county <- trimws(results_pct_20081104$county)
results_pct_20081104$precinct <- trimws(results_pct_20081104$precinct)
uniqVals <- unique(results_pct_20081104[,c("county", "precinct")])
votes_2008 <- setNames(data.frame(matrix(ncol=5,nrow=nrow(uniqVals))), 
    c("county","precinct","dem_votes","rep_votes","total_votes"))
for (i in 1:nrow(uniqVals)) {
  Cval <- as.character(uniqVals[i,][1,1])
  Pval <- as.character(uniqVals[i,][1,2])
  df <- results_pct_20081104[results_pct_20081104$precinct==Pval & results_pct_20081104$county==Cval,]
  votes_2008$county[i] <- as.character(df$county[1])
  votes_2008$precinct[i] <- as.character(df$precinct[1])
  votes_2008$dem_votes[i] <- sum(df[as.character(df$party) == "DEM",]$total.votes)
  votes_2008$rep_votes[i] <- sum(df[as.character(df$party) == "REP",]$total.votes)
  votes_2008$total_votes[i] <- sum(df$total.votes)
}; rm(df, results_pct_20081104)
votes_2008 <- votes_2008[with(votes_2008, order(county, precinct)), ]

# 2010
results_pct_20101102$county <- trimws(results_pct_20101102$county)
results_pct_20101102$precinct <- trimws(results_pct_20101102$precinct)
uniqVals <- unique(results_pct_20101102[,c("county", "precinct")])
votes_2010 <- setNames(data.frame(matrix(ncol=5,nrow=nrow(uniqVals))), 
    c("county","precinct","dem_votes","rep_votes","total_votes"))
for (i in 1:nrow(uniqVals)) {
  Cval <- as.character(uniqVals[i,][1,1])
  Pval <- as.character(uniqVals[i,][1,2])
  df <- results_pct_20101102[results_pct_20101102$precinct==Pval & results_pct_20101102$county==Cval,]
  votes_2010$county[i] <- as.character(df$county[1])
  votes_2010$precinct[i] <- as.character(df$precinct[1])
  votes_2010$dem_votes[i] <- sum(df[as.character(df$party) == "DEM",]$total.votes)
  votes_2010$rep_votes[i] <- sum(df[as.character(df$party) == "REP",]$total.votes)
  votes_2010$total_votes[i] <- sum(df$total.votes)
}; rm(df, results_pct_20101102)
votes_2010 <- votes_2010[with(votes_2010, order(county, precinct)), ]

# 2012
results_pct_20121106$county <- trimws(results_pct_20121106$county)
results_pct_20121106$precinct <- trimws(results_pct_20121106$precinct)
uniqVals <- unique(results_pct_20121106[,c("county", "precinct")])
votes_2012 <- setNames(data.frame(matrix(ncol=5,nrow=nrow(uniqVals))), 
    c("county","precinct","dem_votes","rep_votes","total_votes"))
for (i in 1:nrow(uniqVals)) {
  Cval <- as.character(uniqVals[i,][1,1])
  Pval <- as.character(uniqVals[i,][1,2])
  df <- results_pct_20121106[results_pct_20121106$precinct==Pval & results_pct_20121106$county==Cval,]  
  votes_2012$county[i] <- as.character(df$county[1])
  votes_2012$precinct[i] <- as.character(df$precinct[1])
  votes_2012$dem_votes[i] <- sum(df[as.character(df$party) == "DEM",]$total.votes)
  votes_2012$rep_votes[i] <- sum(df[as.character(df$party) == "REP",]$total.votes)
  votes_2012$total_votes[i] <- sum(df$total.votes)
}; rm(df, results_pct_20121106)
votes_2012 <- votes_2012[with(votes_2012, order(county, precinct)), ]

# 2014
results_pct_20141104$County <- trimws(results_pct_20141104$County)
results_pct_20141104$Precinct <- trimws(results_pct_20141104$Precinct)
uniqVals <- unique(results_pct_20141104[,c("County", "Precinct")])
votes_2014 <- setNames(data.frame(matrix(ncol=5,nrow=nrow(uniqVals))), 
    c("county","precinct","dem_votes","rep_votes","total_votes"))
for (i in 1:nrow(uniqVals)) {
  Cval <- as.character(uniqVals[i,][1,1])
  Pval <- as.character(uniqVals[i,][1,2])
  df <- results_pct_20141104[results_pct_20141104$Precinct==Pval & results_pct_20141104$County==Cval,]
  votes_2014$county[i] <- as.character(df$County[1])
  votes_2014$precinct[i] <- as.character(df$Precinct[1])
  votes_2014$dem_votes[i] <- sum(df[as.character(df$Choice.Party) == "DEM",]$Total.Votes)
  votes_2014$rep_votes[i] <- sum(df[as.character(df$Choice.Party) == "REP",]$Total.Votes)
  votes_2014$total_votes[i] <- sum(df$Total.Votes)
}; rm(df, results_pct_20141104, uniqVals)
votes_2014 <- votes_2014[with(votes_2014, order(county, precinct)), ]

# 2016
results_pct_20161108$County <- trimws(results_pct_20161108$County)
results_pct_20161108$Precinct <- trimws(results_pct_20161108$Precinct)
uniqVals <- unique(results_pct_20161108[,c("County", "Precinct")])
votes_2016 <- setNames(data.frame(matrix(ncol=5,nrow=nrow(uniqVals))), 
    c("county","precinct","dem_votes","rep_votes","total_votes"))
for (i in 1:nrow(uniqVals)) {
  Cval <- as.character(uniqVals[i,][1,1])
  Pval <- as.character(uniqVals[i,][1,2])
  df <- results_pct_20161108[results_pct_20161108$Precinct==Pval & results_pct_20161108$County==Cval,]
  votes_2016$county[i] <- as.character(df$County[1])
  votes_2016$precinct[i] <- as.character(df$Precinct[1])
  votes_2016$dem_votes[i] <- sum(df[as.character(df$Choice.Party) == "DEM",]$Total.Votes)
  votes_2016$rep_votes[i] <- sum(df[as.character(df$Choice.Party) == "REP",]$Total.Votes)
  votes_2016$total_votes[i] <- sum(df$Total.Votes)
}; rm(df, results_pct_20161108)
votes_2016 <- votes_2016[with(votes_2016, order(county, precinct)), ]

# 2018
results_pct_20181106$County <- trimws(results_pct_20181106$County)
results_pct_20181106$Precinct <- trimws(results_pct_20181106$Precinct)
uniqVals <- unique(results_pct_20181106[,c("County", "Precinct")])
votes_2018 <- setNames(data.frame(matrix(ncol=6,nrow=nrow(uniqVals))), 
    c("county","precinct","dem_votes","rep_votes","total_votes","real_precinct"))
for (i in 1:nrow(uniqVals)) {
  Cval <- as.character(uniqVals[i,][1,1])
  Pval <- as.character(uniqVals[i,][1,2])
  df <- results_pct_20181106[results_pct_20181106$Precinct==Pval & results_pct_20181106$County==Cval,]
  votes_2018$county[i] <- as.character(df$County[1])
  votes_2018$precinct[i] <- as.character(df$Precinct[1])
  votes_2018$dem_votes[i] <- sum(df[as.character(df$Choice.Party) == "DEM",]$Total.Votes)
  votes_2018$rep_votes[i] <- sum(df[as.character(df$Choice.Party) == "REP",]$Total.Votes)
  votes_2018$total_votes[i] <- sum(df$Total.Votes)
  votes_2018$real_precinct[i] <- as.character(df$Real.Precinct[1])
}; rm(df, results_pct_20181106, i, uniqVals, Pval, Cval)
votes_2018 <- votes_2018[with(votes_2018, order(county, precinct)), ]


# save.image("./final_data/votingData.RData")