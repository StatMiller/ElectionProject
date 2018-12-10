###
###  Combines the 3 final data sources (votes_20**, demo_20**, vtd_spatial),
###    into a single dataframe, where each row corresponds to a precint/year
###    combination.
###
rm(list=ls())


## setting base level repo directory
setwd("~/Dropbox/733/exams/final/733votingProject/")


load("./final_data/votingData.RData")
load("./final_data/demographicData.RData")
rm(baseDir,dataDir)
load("./final_data/vtd_spatial.RData")



###
###  Making "id" -- a uniform county/precinct var, across all 19 datasets
###

## votes_20** datasets
df <- votes_2002
df <- df[df$county != "" & df$precinct_abbrv != "" & !is.na(df$county) & !is.na(df$precinct_abbrv),]
df[df$county=="CABARRUS",]$precinct_abbrv <- 
      paste(substr(df[df$county=="CABARRUS",]$precinct_abbrv,start=1,stop=2),
            substr(df[df$county=="CABARRUS",]$precinct_abbrv,start=3,stop=4),sep="-")
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")   
votes_2002 <- df[with(df, order(id)), ]

df <- votes_2004 
df <- df[df$county != "" & df$precinct_abbrv != "" & !is.na(df$county) & !is.na(df$precinct_abbrv),]
df[df$county=="CABARRUS",]$precinct_abbrv <- 
  paste(substr(df[df$county=="CABARRUS",]$precinct_abbrv,start=1,stop=2),
        substr(df[df$county=="CABARRUS",]$precinct_abbrv,start=3,stop=4),sep="-")
df[df$county=="WAYNE",]$precinct_abbrv <- substr(df[df$county=="WAYNE",]$precinct_abbrv, start=1, stop=2)
df[df$county=="YANCEY",]$precinct_abbrv <- substr(df[df$county=="YANCEY",]$precinct_abbrv,start=1, stop=2)
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")   
votes_2004 <- df[with(df, order(id)), ]

df <- votes_2006
df <- df[df$county != "" & df$precinct_abbrv != "" & !is.na(df$county) & !is.na(df$precinct_abbrv),]
df[df$county=="YANCEY",]$precinct_abbrv <- substr(df[df$county=="YANCEY",]$precinct_abbrv,start=1, stop=2)
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")   
votes_2006 <- df[with(df, order(id)), ]

df <- votes_2008 
df <- df[df$county != "" & df$precinct != "" & !is.na(df$county) & !is.na(df$precinct),]
df$precinct_abbrv <- NA;  df <- df[,c(1,6,2:5)]
df[df$county=="CUMBERLAND",]$precinct <- gsub("-.*", "", df[df$county=="CUMBERLAND",]$precinct)
for (c in unique(df$county)) {
  for (p in unique(df$precinct)) {
    df$precinct_abbrv[df$county==c & df$precinct==p] <- 
      votes_2006[votes_2006$county==c & votes_2006$precinct==p,]$precinct_abbrv[1]
  }
};  rm(c,p)
df[df$county=="BUNCOMBE",]$precinct_abbrv <- gsub(" - .*", "", df[df$county=="BUNCOMBE",]$precinct)
df[df$county=="BURKE",]$precinct_abbrv <- gsub(" - .*", "", df[df$county=="BURKE",]$precinct)
df[df$county=="CABARRUS",]$precinct_abbrv <- df[df$county=="CABARRUS",]$precinct
df[df$county=="COLUMBUS",]$precinct_abbrv <- df[df$county=="COLUMBUS",]$precinct
df[df$county=="DURHAM",]$precinct_abbrv <- gsub(" - .*", "", df[df$county=="DURHAM",]$precinct)
df[df$county=="FORSYTH",]$precinct_abbrv <- gsub(" - .*", "", df[df$county=="FORSYTH",]$precinct)
df[df$county=="MOORE",]$precinct_abbrv <- gsub(" - .*", "", df[df$county=="MOORE",]$precinct)
df[df$county=="NEW HANOVER",]$precinct_abbrv <- df[df$county=="NEW HANOVER",]$precinct
df[df$county=="UNION",]$precinct_abbrv <- gsub(" - .*", "", df[df$county=="UNION",]$precinct)
df[df$county=="WAKE",]$precinct_abbrv <- gsub("PRECINCT ", "", df[df$county=="WAKE",]$precinct)
df[df$county=="WAYNE",]$precinct_abbrv <- df[df$county=="WAYNE",]$precinct
df[df$county=="YANCEY",]$precinct_abbrv <- substr(df[df$county=="YANCEY",]$precinct_abbrv,start=1, stop=2)
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")   
votes_2008 <- df[with(df, order(id)), ]

df <- votes_2010 
df <- df[df$county != "" & df$precinct != "" & !is.na(df$county) & !is.na(df$precinct),]
df$precinct_abbrv <- NA;  df <- df[,c(1,6,2:5)]
df$precinct_abbrv <- gsub("_.*", "", df$precinct)
df[df$county=="YANCEY",]$precinct_abbrv <- substr(df[df$county=="YANCEY",]$precinct_abbrv,start=1, stop=2)
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")   
votes_2010 <- df[with(df, order(id)), ]

df <- votes_2012 
df <- df[df$county != "" & df$precinct != "" & !is.na(df$county) & !is.na(df$precinct),]
df$precinct_abbrv <- NA;  df <- df[,c(1,6,2:5)]
df$precinct_abbrv <- gsub("_.*", "", df$precinct)
df[df$county=="YANCEY",]$precinct_abbrv <- substr(df[df$county=="YANCEY",]$precinct_abbrv,start=1, stop=2)
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")   
votes_2012 <- df[with(df, order(id)), ]

df <- votes_2014 
df <- df[df$county != "" & df$precinct != "" & !is.na(df$county) & !is.na(df$precinct),]
df[df$county=="YANCEY",]$precinct <- substr(df[df$county=="YANCEY",]$precinct,start=1, stop=2)
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct)), sep="_")   
votes_2014 <- df[with(df, order(id)), ]

df <- votes_2016 
df <- df[df$county != "" & df$precinct != "" & !is.na(df$county) & !is.na(df$precinct),]
df[df$county=="YANCEY",]$precinct <- substr(df[df$county=="YANCEY",]$precinct,start=1, stop=2)
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct)), sep="_")   
votes_2016 <- df[with(df, order(id)), ]

df <- votes_2018 
df <- df[df$county != "" & df$precinct != "" & !is.na(df$county) & !is.na(df$precinct),]
df[df$county=="YANCEY",]$precinct <- substr(df[df$county=="YANCEY",]$precinct,start=1, stop=2)
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct)), sep="_")   
votes_2018 <- df[with(df, order(id)), ]

length(   # total number of id's shared across demo data sets
  Reduce(intersect, list(votes_2002$id, votes_2004$id, votes_2006$id, votes_2008$id, 
           votes_2010$id, votes_2012$id, votes_2014$id, votes_2016$id, votes_2018$id))
)

## demo_20** datasets
df <- demo_2002
df <- df[df$county != "" & df$precinct_abbrv != "" & !is.na(df$county) & !is.na(df$precinct_abbrv),]
df[df$county=="CABARRUS",]$precinct_abbrv <- 
  paste(substr(df[df$county=="CABARRUS",]$precinct_abbrv,start=1,stop=2),
        substr(df[df$county=="CABARRUS",]$precinct_abbrv,start=3,stop=4),sep="-")
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")
demo_2002 <- df[with(df, order(id)), ]

df <- demo_2004
df <- df[df$county != "" & df$precinct_abbrv != "" & !is.na(df$county) & !is.na(df$precinct_abbrv),]
df[df$county=="CABARRUS",]$precinct_abbrv <- 
  paste(substr(df[df$county=="CABARRUS",]$precinct_abbrv,start=1,stop=2),
        substr(df[df$county=="CABARRUS",]$precinct_abbrv,start=3,stop=4),sep="-")
df[df$county=="WAYNE",]$precinct_abbrv <- substr(df[df$county=="WAYNE",]$precinct_abbrv, start=1, stop=2)
df[df$county=="YANCEY",]$precinct_abbrv <- substr(df[df$county=="YANCEY",]$precinct_abbrv,start=1, stop=2)
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")
demo_2004 <- df[with(df, order(id)), ]

df <- demo_2006
df <- df[df$county != "" & df$precinct_abbrv != "" & !is.na(df$county) & !is.na(df$precinct_abbrv),]
df[df$county=="YANCEY",]$precinct_abbrv <- substr(df[df$county=="YANCEY",]$precinct_abbrv,start=1, stop=2)
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")
demo_2006 <- df[with(df, order(id)), ]

df <- demo_2008
df <- df[df$county != "" & df$precinct_abbrv != "" & !is.na(df$county) & !is.na(df$precinct_abbrv),]
df[df$county=="YANCEY",]$precinct_abbrv <- substr(df[df$county=="YANCEY",]$precinct_abbrv,start=1, stop=2)
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")
demo_2008 <- df[with(df, order(id)), ]

df <- demo_2010
df <- df[df$county != "" & df$precinct_abbrv != "" & !is.na(df$county) & !is.na(df$precinct_abbrv),]
df[df$county=="YANCEY",]$precinct_abbrv <- substr(df[df$county=="YANCEY",]$precinct_abbrv,start=1, stop=2)
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")
demo_2010 <- df[with(df, order(id)), ]

df <- demo_2012
df <- df[df$county != "" & df$precinct_abbrv != "" & !is.na(df$county) & !is.na(df$precinct_abbrv),]
df[df$county=="YANCEY",]$precinct_abbrv <- substr(df[df$county=="YANCEY",]$precinct_abbrv,start=1, stop=2)
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")
df$id2 <- paste(tolower(trimws(df$county)), tolower(trimws(df$vtd_abbrv)), sep="_")
demo_2012 <- df[with(df, order(id)), ]

df <- demo_2014
df <- df[df$county != "" & df$precinct_abbrv != "" & !is.na(df$county) & !is.na(df$precinct_abbrv),]
df[df$county=="YANCEY",]$precinct_abbrv <- substr(df[df$county=="YANCEY",]$precinct_abbrv,start=1, stop=2)
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")
df$id2 <- paste(tolower(trimws(df$county)), tolower(trimws(df$vtd_abbrv)), sep="_")
demo_2014 <- df[with(df, order(id)), ]

df <- demo_2016
df <- df[df$county != "" & df$precinct_abbrv != "" & !is.na(df$county) & !is.na(df$precinct_abbrv),]
df[df$county=="YANCEY",]$precinct_abbrv <- substr(df[df$county=="YANCEY",]$precinct_abbrv,start=1, stop=2)
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")
df$id2 <- paste(tolower(trimws(df$county)), tolower(trimws(df$vtd_abbrv)), sep="_")
demo_2016 <- df[with(df, order(id)), ]

df <- demo_2018
df <- df[df$county != "" & df$precinct_abbrv != "" & !is.na(df$county) & !is.na(df$precinct_abbrv),]
df[df$county=="YANCEY",]$precinct_abbrv <- substr(df[df$county=="YANCEY",]$precinct_abbrv,start=1, stop=2)
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")
df$id2 <- paste(tolower(trimws(df$county)), tolower(trimws(df$vtd_abbrv)), sep="_")
demo_2018 <- df[with(df, order(id)), ]

length(   # total number of id's shared across demo data sets
  Reduce(intersect, list(demo_2002$id, demo_2004$id, demo_2006$id, demo_2008$id, 
            demo_2010$id, demo_2012$id, demo_2014$id, demo_2016$id, demo_2018$id))
)

## lining up
IDsIntersect <- Reduce(intersect, list(votes_2002$id, votes_2004$id, votes_2006$id, 
                                       votes_2008$id, votes_2010$id, votes_2012$id, votes_2014$id, 
                                       votes_2016$id, votes_2018$id, demo_2002$id,  demo_2004$id, 
                                       demo_2006$id,  demo_2008$id,  demo_2010$id,  demo_2012$id, 
                                       demo_2014$id, demo_2016$id, demo_2018$id))
length(IDsIntersect)   # total number of id's shared across votes AND demo datasets


## vtd_matching dataset
vtd_matching <- vtd_matching[,c("county","vtd_name","lat","long","dist_to_closest_city")]
vtd_matching[vtd_matching$county=="buncombe",]$vtd_name <- paste0(vtd_matching[vtd_matching$county=="buncombe",]$vtd_name, ".1")
vtd_matching$id <- paste(tolower(trimws(vtd_matching$county)), tolower(trimws(vtd_matching$vtd_name)), sep="_")
vtd_matching <- vtd_matching[with(vtd_matching, order(county, vtd_name)), ]

view <- function(cnty) {
  IDsIntersect <- Reduce(intersect, list(subset(votes_2002, tolower(county)==cnty, select = "id")$id, 
                                         subset(votes_2004, tolower(county)==cnty, select = "id")$id, 
                                         subset(votes_2006, tolower(county)==cnty, select = "id")$id, 
                                         subset(votes_2008, tolower(county)==cnty, select = "id")$id, 
                                         subset(votes_2010, tolower(county)==cnty, select = "id")$id, 
                                         subset(votes_2012, tolower(county)==cnty, select = "id")$id, 
                                         subset(votes_2014, tolower(county)==cnty, select = "id")$id, 
                                         subset(votes_2016, tolower(county)==cnty, select = "id")$id, 
                                         subset(votes_2018, tolower(county)==cnty, select = "id")$id, 
                                         subset(demo_2002,  tolower(county)==cnty, select = "id")$id,  
                                         subset(demo_2004,  tolower(county)==cnty, select = "id")$id, 
                                         subset(demo_2006,  tolower(county)==cnty, select = "id")$id,  
                                         subset(demo_2008,  tolower(county)==cnty, select = "id")$id,  
                                         subset(demo_2010,  tolower(county)==cnty, select = "id")$id,  
                                         subset(demo_2012,  tolower(county)==cnty, select = "id")$id, 
                                         subset(demo_2014,  tolower(county)==cnty, select = "id")$id, 
                                         subset(demo_2016,  tolower(county)==cnty, select = "id")$id, 
                                         subset(demo_2018,  tolower(county)==cnty, select = "id")$id))
  
  cat("==== number of precincts in intersection: ", length(IDsIntersect)," ====\n")
  cat("vote_2002: ", subset(votes_2002, tolower(county)==cnty, select = "id")$id[3:10], "\n")
  cat("vote_2004: ", subset(votes_2004, tolower(county)==cnty, select = "id")$id[3:10], "\n")
  cat("vote_2006: ", subset(votes_2006, tolower(county)==cnty, select = "id")$id[3:10], "\n")
  cat("vote_2008: ", subset(votes_2008, tolower(county)==cnty, select = "id")$id[3:10], "\n")
  cat("vote_2010: ", subset(votes_2010, tolower(county)==cnty, select = "id")$id[3:10], "\n")
  cat("vote_2012: ", subset(votes_2012, tolower(county)==cnty, select = "id")$id[3:10], "\n")
  cat("vote_2014: ", subset(votes_2014, tolower(county)==cnty, select = "id")$id[3:10], "\n")
  cat("vote_2016: ", subset(votes_2016, tolower(county)==cnty, select = "id")$id[3:10], "\n")
  cat("vote_2018: ", subset(votes_2018, tolower(county)==cnty, select = "id")$id[3:10], "\n")
  cat("demo_2002: ", subset(demo_2002,  tolower(county)==cnty, select = "id")$id[3:10], "\n")
  cat("demo_2004: ", subset(demo_2004,  tolower(county)==cnty, select = "id")$id[3:10], "\n")
  cat("demo_2006: ", subset(demo_2006,  tolower(county)==cnty, select = "id")$id[3:10], "\n")
  cat("demo_2008: ", subset(demo_2008,  tolower(county)==cnty, select = "id")$id[3:10], "\n")
  cat("demo_2010: ", subset(demo_2010,  tolower(county)==cnty, select = "id")$id[3:10], "\n")
  cat("demo_2012: ", subset(demo_2012,  tolower(county)==cnty, select = "id")$id[3:10], "\n")
  cat("demo_2014: ", subset(demo_2014,  tolower(county)==cnty, select = "id")$id[3:10], "\n")
  cat("demo_2016: ", subset(demo_2016,  tolower(county)==cnty, select = "id")$id[3:10], "\n")
  cat("demo_2018: ", subset(demo_2018,  tolower(county)==cnty, select = "id")$id[3:10], "\n")
  cat("vtd_match: ", subset(vtd_matching, tolower(county)==cnty, select = "id")$id[3:10], "\n")
}

# for (p in unique(vtd_matching$county)) {
#   view(p)
#   invisible(readline(prompt="Press [enter] to continue"))
# }

IDsIntersect <- Reduce(intersect, list(votes_2002$id, votes_2004$id, votes_2006$id,
                                       votes_2008$id, votes_2010$id, votes_2012$id, votes_2014$id,
                                       votes_2016$id, votes_2018$id, demo_2002$id,  demo_2004$id,
                                       demo_2006$id,  demo_2008$id,  demo_2010$id,  demo_2012$id,
                                       demo_2014$id, demo_2016$id, demo_2018$id, vtd_matching$id))
length(IDsIntersect)   # total number of id's shared across votes AND demo datasets
# 2045  <- NOT BAD!!


###
###  Merging by id
###

# make master list of id's to include:  already done:  IDsIntersect    (see above)

# take out all other obs from everything else  (make sure no duplicates)
votes_2002 <- votes_2002[votes_2002$id %in% IDsIntersect, ]
votes_2004 <- votes_2004[votes_2004$id %in% IDsIntersect, ]
votes_2006 <- votes_2006[votes_2006$id %in% IDsIntersect, ]
votes_2008 <- votes_2008[votes_2008$id %in% IDsIntersect, ]
votes_2010 <- votes_2010[votes_2010$id %in% IDsIntersect, ]
votes_2012 <- votes_2012[votes_2012$id %in% IDsIntersect, ]
votes_2014 <- votes_2014[votes_2014$id %in% IDsIntersect, ]
votes_2016 <- votes_2016[votes_2016$id %in% IDsIntersect, ]
votes_2018 <- votes_2018[votes_2018$id %in% IDsIntersect, ]
demo_2002 <- demo_2002[demo_2002$id %in% IDsIntersect, ]
demo_2004 <- demo_2004[demo_2004$id %in% IDsIntersect, ]
demo_2006 <- demo_2006[demo_2006$id %in% IDsIntersect, ]
demo_2008 <- demo_2008[demo_2008$id %in% IDsIntersect, ]
demo_2010 <- demo_2010[demo_2010$id %in% IDsIntersect, ]
demo_2012 <- demo_2012[demo_2012$id %in% IDsIntersect, ]
demo_2014 <- demo_2014[demo_2014$id %in% IDsIntersect, ]
demo_2016 <- demo_2016[demo_2016$id %in% IDsIntersect, ]
demo_2018 <- demo_2018[demo_2018$id %in% IDsIntersect, ]
vtd_matching <- vtd_matching[vtd_matching$id %in% IDsIntersect, ]

# make new variable for election year
votes_2002$year <- demo_2002$year <- 2002
votes_2004$year <- demo_2004$year <- 2004
votes_2006$year <- demo_2006$year <- 2006
votes_2008$year <- demo_2008$year <- 2008
votes_2010$year <- demo_2010$year <- 2010
votes_2012$year <- demo_2012$year <- 2012
votes_2014$year <- demo_2014$year <- 2014
votes_2016$year <- demo_2016$year <- 2016
votes_2018$year <- demo_2018$year <- 2018

# (temporarily forming one id/year combo variable to merge on)
votes_2002$idyear <- paste(votes_2002$id, votes_2002$year, sep="___")
votes_2004$idyear <- paste(votes_2004$id, votes_2004$year, sep="___")
votes_2006$idyear <- paste(votes_2006$id, votes_2006$year, sep="___")
votes_2008$idyear <- paste(votes_2008$id, votes_2008$year, sep="___")
votes_2010$idyear <- paste(votes_2010$id, votes_2010$year, sep="___")
votes_2012$idyear <- paste(votes_2012$id, votes_2012$year, sep="___")
votes_2014$idyear <- paste(votes_2014$id, votes_2014$year, sep="___")
votes_2016$idyear <- paste(votes_2016$id, votes_2016$year, sep="___")
votes_2018$idyear <- paste(votes_2018$id, votes_2018$year, sep="___")
demo_2002$idyear <- paste(demo_2002$id, demo_2002$year, sep="___")
demo_2004$idyear <- paste(demo_2004$id, demo_2004$year, sep="___")
demo_2006$idyear <- paste(demo_2006$id, demo_2006$year, sep="___")
demo_2008$idyear <- paste(demo_2008$id, demo_2008$year, sep="___")
demo_2010$idyear <- paste(demo_2010$id, demo_2010$year, sep="___")
demo_2012$idyear <- paste(demo_2012$id, demo_2012$year, sep="___")
demo_2014$idyear <- paste(demo_2014$id, demo_2014$year, sep="___")
demo_2016$idyear <- paste(demo_2016$id, demo_2016$year, sep="___")
demo_2018$idyear <- paste(demo_2018$id, demo_2018$year, sep="___")

# order by idyear
votes_2002 <- votes_2002[with(votes_2002, order(idyear)), ]
votes_2004 <- votes_2004[with(votes_2004, order(idyear)), ]
votes_2006 <- votes_2006[with(votes_2006, order(idyear)), ]
votes_2008 <- votes_2008[with(votes_2008, order(idyear)), ]
votes_2010 <- votes_2010[with(votes_2010, order(idyear)), ]
votes_2012 <- votes_2012[with(votes_2012, order(idyear)), ]
votes_2014 <- votes_2014[with(votes_2014, order(idyear)), ]
votes_2016 <- votes_2016[with(votes_2016, order(idyear)), ]
votes_2018 <- votes_2018[with(votes_2018, order(idyear)), ]
demo_2002  <- demo_2002[with(demo_2002, order(idyear)), ]
demo_2004  <- demo_2004[with(demo_2004, order(idyear)), ]
demo_2006  <- demo_2006[with(demo_2006, order(idyear)), ]
demo_2008  <- demo_2008[with(demo_2008, order(idyear)), ]
demo_2010  <- demo_2010[with(demo_2010, order(idyear)), ]
demo_2012  <- demo_2012[with(demo_2012, order(idyear)), ]
demo_2014  <- demo_2014[with(demo_2014, order(idyear)), ]
demo_2016  <- demo_2016[with(demo_2016, order(idyear)), ]
demo_2018  <- demo_2018[with(demo_2018, order(idyear)), ]

# checking that the 18 files are all in like row-order within their year pairs
sum(votes_2002$idyear==demo_2002$idyear)
sum(votes_2004$idyear==demo_2004$idyear)
sum(votes_2006$idyear==demo_2006$idyear)
sum(votes_2008$idyear==demo_2008$idyear)
sum(votes_2010$idyear==demo_2010$idyear)
sum(votes_2012$idyear==demo_2012$idyear)
sum(votes_2014$idyear==demo_2014$idyear)
sum(votes_2016$idyear==demo_2016$idyear)
sum(votes_2018$idyear==demo_2018$idyear)

# deleting extra columns in some of the datasets
demo_2012$id2 <- demo_2012$vtd_abbrv <- NULL
demo_2014$id2 <- demo_2014$vtd_abbrv <- NULL
demo_2016$id2 <- demo_2016$vtd_abbrv <- NULL
demo_2018$id2 <- demo_2018$vtd_abbrv <- NULL
votes_2002$precinct <- NULL
votes_2004$precinct <- NULL
votes_2006$precinct <- NULL
votes_2008$precinct <- NULL
votes_2010$precinct <- NULL
votes_2012$precinct <- NULL
colnames(votes_2014)[2] <- c("precinct_abbrv")
colnames(votes_2016)[2] <- c("precinct_abbrv")
colnames(votes_2018)[2] <- c("precinct_abbrv")
votes_2018$real_precinct <- NULL

# checking that the 18 files are all in like col-order within their votes/demo groups
sum(names(demo_2002) == names(demo_2004))
sum(names(demo_2002) == names(demo_2006))
sum(names(demo_2002) == names(demo_2008))
sum(names(demo_2002) == names(demo_2010))
sum(names(demo_2002) == names(demo_2012))
sum(names(demo_2002) == names(demo_2014))
sum(names(demo_2002) == names(demo_2016))
sum(names(demo_2002) == names(demo_2018))
sum(names(votes_2002) == names(votes_2004))
sum(names(votes_2002) == names(votes_2006))
sum(names(votes_2002) == names(votes_2008))
sum(names(votes_2002) == names(votes_2010))
sum(names(votes_2002) == names(votes_2012))
sum(names(votes_2002) == names(votes_2014))
sum(names(votes_2002) == names(votes_2016))
sum(names(votes_2002) == names(votes_2018))

# merging like years
both_2002 <- cbind(votes_2002[,c("id","year","idyear","county","precinct_abbrv","total_votes","dem_votes", "rep_votes")],demo_2002[,c(3:22)])
both_2004 <- cbind(votes_2004[,c("id","year","idyear","county","precinct_abbrv","total_votes","dem_votes", "rep_votes")],demo_2004[,c(3:22)])
both_2006 <- cbind(votes_2006[,c("id","year","idyear","county","precinct_abbrv","total_votes","dem_votes", "rep_votes")],demo_2006[,c(3:22)])
both_2008 <- cbind(votes_2008[,c("id","year","idyear","county","precinct_abbrv","total_votes","dem_votes", "rep_votes")],demo_2008[,c(3:22)])
both_2010 <- cbind(votes_2010[,c("id","year","idyear","county","precinct_abbrv","total_votes","dem_votes", "rep_votes")],demo_2010[,c(3:22)])
both_2012 <- cbind(votes_2012[,c("id","year","idyear","county","precinct_abbrv","total_votes","dem_votes", "rep_votes")],demo_2012[,c(3:22)])
both_2014 <- cbind(votes_2014[,c("id","year","idyear","county","precinct_abbrv","total_votes","dem_votes", "rep_votes")],demo_2014[,c(3:22)])
both_2016 <- cbind(votes_2016[,c("id","year","idyear","county","precinct_abbrv","total_votes","dem_votes", "rep_votes")],demo_2016[,c(3:22)])
both_2018 <- cbind(votes_2018[,c("id","year","idyear","county","precinct_abbrv","total_votes","dem_votes", "rep_votes")],demo_2018[,c(3:22)])

sum(names(both_2002) == names(both_2004))
sum(names(both_2002) == names(both_2006))
sum(names(both_2002) == names(both_2008))
sum(names(both_2002) == names(both_2010))
sum(names(both_2002) == names(both_2012))
sum(names(both_2002) == names(both_2014))
sum(names(both_2002) == names(both_2016))
sum(names(both_2002) == names(both_2018))

# merging over years
all_data <- rbind(
  both_2002,
  both_2004,
  both_2006,
  both_2008,
  both_2010,
  both_2012,
  both_2014,
  both_2016,
  both_2018
)

# merge in location data
all_data$lon <- all_data$lat <- all_data$dist_to_closest_city <- NA
for(i in unique(vtd_matching$id)) {
  all_data[all_data$id==i,]$lon <- vtd_matching$long[vtd_matching$id == i]
  all_data[all_data$id==i,]$lat <- vtd_matching$lat[vtd_matching$id == i]
  all_data[all_data$id==i,]$dist_to_closest_city <- vtd_matching$dist_to_closest_city[vtd_matching$id == i]
}  # View(all_data)
rm(list=setdiff(ls(), "all_data"))



###
###  Generate the variables of interest:
###

## responses:
#   pctVotes_D_of_DR
all_data$pctVotes_D_of_DR <- all_data$dem_votes / (all_data$dem_votes + all_data$rep_votes)
#   pctVotes_nonDR_of_total
all_data$pctVotes_nonDR_of_total <- 1 - (all_data$dem_votes + all_data$rep_votes)/all_data$total_votes
#   countVotes
all_data$countVotes <- all_data$total_votes
#   countVotes_D
all_data$countVotes_D <- all_data$dem_votes
#   countVotes_R
all_data$countVotes_DR <- (all_data$dem_votes + all_data$rep_votes)



## covariates:
#   pctRgstrd_age18_25, pctRgstrd_age26_40, pctRgstrd_age41_65, pctRgstrd_age66_
all_data$pctRgstrd_age18_25 <- all_data$age18_25 / all_data$total_voters
all_data$pctRgstrd_age26_40 <- all_data$age26_40 / all_data$total_voters
all_data$pctRgstrd_age41_65 <- all_data$age41_65 / all_data$total_voters
all_data$pctRgstrd_age66_   <- all_data$age65_ / all_data$total_voters
#   pctRgstrd_white
all_data$pctRgstrd_white <- all_data$race_white / all_data$total_voters
#   pctRgstrd_D_of_DR
all_data$pctRgstrd_D_of_DR <- all_data$party_D / (all_data$party_D + all_data$party_R)
#   pctRgstrd_nonDR_of_total
all_data$pctRgstrd_nonDR_of_total <- 1 - (all_data$party_D + all_data$party_R)/all_data$total_voters
#   countRgstrd
all_data$countRgstrd <- all_data$total_voters
#   pctRgstrd_male
all_data$pctRgstrd_male <- all_data$sex_male / all_data$total_voters
#   presElectionYear
all_data$presElectionYear <- ifelse( all_data$year%%4==0, 1, 0)
#   distance_to_closest_city
      # already in good form
#   lon, lat
      # already in good form


all_data <- all_data[,c("year","id","county","precinct_abbrv","lon","lat","presElectionYear",
                        "countVotes_D", "countVotes_DR",
                        "pctVotes_D_of_DR","pctVotes_nonDR_of_total","countVotes","pctRgstrd_age18_25",
                        "pctRgstrd_age26_40","pctRgstrd_age41_65","pctRgstrd_age66_","pctRgstrd_white",
                        "pctRgstrd_D_of_DR","pctRgstrd_nonDR_of_total","countRgstrd","dist_to_closest_city")]
# View(all_data)
# save.image("./final_data/final_data.RData")