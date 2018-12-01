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
ldply(.data = zipF, .fun = unzip, exdir = dataDir)
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
}

  
## fixing the no-headers ones
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



## next steps:
#    
#    - further breakdown these dataframe into the data that we need
#    
#    - do this for the files that contain the demographic data
#    








# #####
# results_pct_20020910 
# results_pct_20021105 
# results_pct_20040720 
# results_pct_20040817 
# results_pct_20041102
# results_pct_20060502 
# results_pct_20060530 
# results_pct_20060912 
# results_pct_20061107 
# results_pct_20080506
# results_pct_20081104 
# results_pct_20100504 
# results_pct_20100622 
# results_pct_20101102 
# results_pct_20110913
# results_pct_20111011 
# results_pct_20111108 
# results_pct_20120508 
# results_pct_20120717 
# results_pct_20121106
# results_pct_20130910 
# results_pct_20131008 
# results_pct_20140506 
# results_pct_20140715 
# results_pct_20141104
# results_pct_20150915 
# results_pct_20151006 
# results_pct_20151103 
# results_pct_20160315 
# results_pct_20160607
# results_pct_20161108 
# results_pct_20170912 
# results_pct_20171010 
# results_pct_20171107 
# results_pct_20180508
# results_pct_20180626 
# results_pct_20181106 