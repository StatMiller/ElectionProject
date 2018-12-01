###
###  Produces an RData file with precinct-level voting results, from
###    the original .zip files from <https://er.ncsbe.gov/downloads.html>
###
rm(list=ls())


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


## including only DEM/REP votes
results_pct_20021105 <- results_pct_20021105[as.character(results_pct_20021105$party_cd) %in% c("DEM","REP"),]
results_pct_20041102 <- results_pct_20041102[as.character(results_pct_20041102$party_cd) %in% c("DEM","REP"),]
results_pct_20061107 <- results_pct_20061107[as.character(results_pct_20061107$party_cd) %in% c("DEM","REP"),]
results_pct_20081104 <- results_pct_20081104[as.character(results_pct_20081104$party) %in% c("DEM","REP"),]
results_pct_20101102 <- results_pct_20101102[as.character(results_pct_20101102$party) %in% c("DEM","REP"),]
results_pct_20121106 <- results_pct_20121106[as.character(results_pct_20121106$party) %in% c("DEM","REP"),]
results_pct_20141104 <- results_pct_20141104[as.character(results_pct_20141104$Choice.Party) %in% c("DEM","REP"),]
results_pct_20161108 <- results_pct_20161108[as.character(results_pct_20161108$Choice.Party) %in% c("DEM","REP"),]
results_pct_20181106 <- results_pct_20181106[as.character(results_pct_20181106$Choice.Party) %in% c("DEM","REP"),]


## generating the variables of interest at the precinct level
precinct_2002 <- setNames(data.frame(matrix(ncol=6,nrow=length(unique(results_pct_20021105$precinct)))), 
    c("county","precinct_abbrv","precinct","dem_votes","rep_votes","pct_dem"))
for (i in 1:length(unique(results_pct_20021105$precinct))) {
  df <- results_pct_20021105[results_pct_20021105$precinct==unique(results_pct_20021105$precinct)[i],]
  precinct_2002$county[i] <- as.character(df$county[1])
  precinct_2002$precinct_abbrv[i] <- as.character(df$precinct_abbrv[1])
  precinct_2002$precinct[i] <- as.character(df$precinct[1])
  precinct_2002$dem_votes[i] <- sum(df[as.character(df$party_cd) == "DEM",]$ballot_count)
  precinct_2002$rep_votes[i] <- sum(df[as.character(df$party_cd) == "REP",]$ballot_count)
  precinct_2002$pct_dem[i] <- precinct_2002$dem_votes[i]/(precinct_2002$dem_votes[i] + precinct_2002$rep_votes[i])
}; rm(df, results_pct_20021105)
   
precinct_2004 <- setNames(data.frame(matrix(ncol=6,nrow=length(unique(results_pct_20041102$precinct)))), 
                          c("county","precinct_abbrv","precinct","dem_votes","rep_votes","pct_dem"))
for (i in 1:length(unique(results_pct_20041102$precinct))) {
  df <- results_pct_20041102[results_pct_20041102$precinct==unique(results_pct_20041102$precinct)[i],]
  precinct_2004$county[i] <- as.character(df$county[1])
  precinct_2004$precinct_abbrv[i] <- as.character(df$precinct_abbrv[1])
  precinct_2004$precinct[i] <- as.character(df$precinct[1])
  precinct_2004$dem_votes[i] <- sum(df[as.character(df$party_cd) == "DEM",]$ballot_count)
  precinct_2004$rep_votes[i] <- sum(df[as.character(df$party_cd) == "REP",]$ballot_count)
  precinct_2004$pct_dem[i] <- precinct_2004$dem_votes[i]/(precinct_2004$dem_votes[i] + precinct_2004$rep_votes[i])
}; rm(df, results_pct_20041102)

precinct_2006 <- setNames(data.frame(matrix(ncol=6,nrow=length(unique(results_pct_20061107$precinct)))), 
                          c("county","precinct_abbrv","precinct","dem_votes","rep_votes","pct_dem"))
for (i in 1:length(unique(results_pct_20061107$precinct))) {
  df <- results_pct_20061107[results_pct_20061107$precinct==unique(results_pct_20061107$precinct)[i],]
  precinct_2006$county[i] <- as.character(df$county[1])
  precinct_2006$precinct_abbrv[i] <- as.character(df$precinct_abbrv[1])
  precinct_2006$precinct[i] <- as.character(df$precinct[1])
  precinct_2006$dem_votes[i] <- sum(df[as.character(df$party_cd) == "DEM",]$ballot_count)
  precinct_2006$rep_votes[i] <- sum(df[as.character(df$party_cd) == "REP",]$ballot_count)
  precinct_2006$pct_dem[i] <- precinct_2006$dem_votes[i]/(precinct_2006$dem_votes[i] + precinct_2006$rep_votes[i])
}; rm(df, results_pct_20061107)

precinct_2008 <- setNames(data.frame(matrix(ncol=8,nrow=length(unique(results_pct_20081104$precinct)))), 
                          c("county","precinct","dem_votes","rep_votes","pct_dem","pct_electionDay",
                            "pct_absentee.oneStop","pct_provisional"))
for (i in 1:length(unique(results_pct_20081104$precinct))) {
  df <- results_pct_20081104[results_pct_20081104$precinct==unique(results_pct_20081104$precinct)[i],]
  precinct_2008$county[i] <- as.character(df$county[1])
  precinct_2008$precinct[i] <- as.character(df$precinct[1])
  precinct_2008$dem_votes[i] <- sum(df[as.character(df$party) == "DEM",]$total.votes)
  precinct_2008$rep_votes[i] <- sum(df[as.character(df$party) == "REP",]$total.votes)
  precinct_2008$pct_dem[i] <- precinct_2008$dem_votes[i]/(precinct_2008$dem_votes[i] + precinct_2008$rep_votes[i])
  precinct_2008$pct_electionDay[i] <- sum(df$Election.Day)/sum(df$total.votes)
  precinct_2008$pct_absentee.oneStop[i] <- sum(df$Absentee...One.Stop)/sum(df$total.votes)
  precinct_2008$pct_provisional[i] <- sum(df$Provisional)/sum(df$total.votes)
}; rm(df, results_pct_20081104)

precinct_2010 <- setNames(data.frame(matrix(ncol=9,nrow=length(unique(results_pct_20101102$precinct)))), 
                          c("county","precinct","dem_votes","rep_votes","pct_dem","pct_electionDay",
                            "pct_absentee","pct_oneStop","pct_provisional"))
for (i in 1:length(unique(results_pct_20101102$precinct))) {
  df <- results_pct_20101102[results_pct_20101102$precinct==unique(results_pct_20101102$precinct)[i],]
  precinct_2010$county[i] <- as.character(df$county[1])
  precinct_2010$precinct[i] <- as.character(df$precinct[1])
  precinct_2010$dem_votes[i] <- sum(df[as.character(df$party) == "DEM",]$total.votes)
  precinct_2010$rep_votes[i] <- sum(df[as.character(df$party) == "REP",]$total.votes)
  precinct_2010$pct_dem[i] <- precinct_2010$dem_votes[i]/(precinct_2010$dem_votes[i] + precinct_2010$rep_votes[i])
  precinct_2010$pct_electionDay[i] <- sum(df$Election.Day)/sum(df$total.votes)
  precinct_2010$pct_absentee[i] <- sum(df$Absentee.by.Mail)/sum(df$total.votes)
  precinct_2010$pct_oneStop[i] <- sum(df$One.Stop)/sum(df$total.votes)
  precinct_2010$pct_provisional[i] <- sum(df$Provisional)/sum(df$total.votes)
}; rm(df, results_pct_20101102)

precinct_2012 <- setNames(data.frame(matrix(ncol=9,nrow=length(unique(results_pct_20121106$precinct)))), 
                          c("county","precinct","dem_votes","rep_votes","pct_dem","pct_electionDay",
                            "pct_absentee","pct_oneStop","pct_provisional"))
for (i in 1:length(unique(results_pct_20121106$precinct))) {
  df <- results_pct_20121106[results_pct_20121106$precinct==unique(results_pct_20121106$precinct)[i],]
  precinct_2012$county[i] <- as.character(df$county[1])
  precinct_2012$precinct[i] <- as.character(df$precinct[1])
  precinct_2012$dem_votes[i] <- sum(df[as.character(df$party) == "DEM",]$total.votes)
  precinct_2012$rep_votes[i] <- sum(df[as.character(df$party) == "REP",]$total.votes)
  precinct_2012$pct_dem[i] <- precinct_2012$dem_votes[i]/(precinct_2012$dem_votes[i] + precinct_2012$rep_votes[i])
  precinct_2012$pct_electionDay[i] <- sum(df$Election.Day)/sum(df$total.votes)
  precinct_2012$pct_absentee[i] <- sum(df$Absentee.by.Mail)/sum(df$total.votes)
  precinct_2012$pct_oneStop[i] <- sum(df$One.Stop)/sum(df$total.votes)
  precinct_2012$pct_provisional[i] <- sum(df$Provisional)/sum(df$total.votes)
}; rm(df, results_pct_20121106)

precinct_2014 <- setNames(data.frame(matrix(ncol=9,nrow=length(unique(results_pct_20141104$Precinct)))), 
                          c("county","precinct","dem_votes","rep_votes","pct_dem","pct_electionDay",
                            "pct_absentee","pct_oneStop","pct_provisional"))
for (i in 1:length(unique(results_pct_20141104$Precinct))) {
  df <- results_pct_20141104[results_pct_20141104$Precinct==unique(results_pct_20141104$Precinct)[i],]
  precinct_2014$county[i] <- as.character(df$County[1])
  precinct_2014$precinct[i] <- as.character(df$Precinct[1])
  precinct_2014$dem_votes[i] <- sum(df[as.character(df$Choice.Party) == "DEM",]$Total.Votes)
  precinct_2014$rep_votes[i] <- sum(df[as.character(df$Choice.Party) == "REP",]$Total.Votes)
  precinct_2014$pct_dem[i] <- precinct_2014$dem_votes[i]/(precinct_2014$dem_votes[i] + precinct_2014$rep_votes[i])
  precinct_2014$pct_electionDay[i] <- sum(df$Election.Day)/sum(df$Total.Votes)
  precinct_2014$pct_absentee[i] <- sum(df$Absentee.by.Mail)/sum(df$Total.Votes)
  precinct_2014$pct_oneStop[i] <- sum(df$One.Stop)/sum(df$Total.Votes)
  precinct_2014$pct_provisional[i] <- sum(df$Provisional)/sum(df$Total.Votes)
}; rm(df, results_pct_20141104)

precinct_2016 <- setNames(data.frame(matrix(ncol=9,nrow=length(unique(results_pct_20161108$Precinct)))), 
                          c("county","precinct","dem_votes","rep_votes","pct_dem","pct_electionDay",
                            "pct_absentee","pct_oneStop","pct_provisional"))
for (i in 1:length(unique(results_pct_20161108$Precinct))) {
  df <- results_pct_20161108[results_pct_20161108$Precinct==unique(results_pct_20161108$Precinct)[i],]
  precinct_2016$county[i] <- as.character(df$County[1])
  precinct_2016$precinct[i] <- as.character(df$Precinct[1])
  precinct_2016$dem_votes[i] <- sum(df[as.character(df$Choice.Party) == "DEM",]$Total.Votes)
  precinct_2016$rep_votes[i] <- sum(df[as.character(df$Choice.Party) == "REP",]$Total.Votes)
  precinct_2016$pct_dem[i] <- precinct_2016$dem_votes[i]/(precinct_2016$dem_votes[i] + precinct_2016$rep_votes[i])
  precinct_2016$pct_electionDay[i] <- sum(df$Election.Day)/sum(df$Total.Votes)
  precinct_2016$pct_absentee[i] <- sum(df$Absentee.by.Mail)/sum(df$Total.Votes)
  precinct_2016$pct_oneStop[i] <- sum(df$One.Stop)/sum(df$Total.Votes)
  precinct_2016$pct_provisional[i] <- sum(df$Provisional)/sum(df$Total.Votes)
}; rm(df, results_pct_20161108)

precinct_2018 <- setNames(data.frame(matrix(ncol=10,nrow=length(unique(results_pct_20181106$Precinct)))), 
                          c("county","precinct","dem_votes","rep_votes","pct_dem","pct_electionDay",
                            "pct_absentee","pct_oneStop","pct_provisional","real_precinct"))
for (i in 1:length(unique(results_pct_20181106$Precinct))) {
  df <- results_pct_20181106[results_pct_20181106$Precinct==unique(results_pct_20181106$Precinct)[i],]
  precinct_2018$county[i] <- as.character(df$County[1])
  precinct_2018$precinct[i] <- as.character(df$Precinct[1])
  precinct_2018$dem_votes[i] <- sum(df[as.character(df$Choice.Party) == "DEM",]$Total.Votes)
  precinct_2018$rep_votes[i] <- sum(df[as.character(df$Choice.Party) == "REP",]$Total.Votes)
  precinct_2018$pct_dem[i] <- precinct_2018$dem_votes[i]/(precinct_2018$dem_votes[i] + precinct_2018$rep_votes[i])
  precinct_2018$pct_electionDay[i] <- sum(df$Election.Day)/sum(df$Total.Votes)
  precinct_2018$pct_absentee[i] <- sum(df$Absentee.by.Mail)/sum(df$Total.Votes)
  precinct_2018$pct_oneStop[i] <- sum(df$One.Stop)/sum(df$Total.Votes)
  precinct_2018$pct_provisional[i] <- sum(df$Provisional)/sum(df$Total.Votes)
  precinct_2018$real_precinct[i] <- as.character(df$Real.Precinct[1])
}; rm(df, results_pct_20181106, i)














################################################# scratch ######################

## next steps:
# 
#    - do this for the files that contain the demographic data
#    

# count <- 0
# for (dfName in ls(pattern="results_pct_.*")) {
#   count <- count + 1
#   df <- get(dfName)
#   cat("\n(", count, ") ", dfName, "\n", sep="")
#   print(names(df))
#   cat("\n")
# }
