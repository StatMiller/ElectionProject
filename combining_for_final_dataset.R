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


df <- votes_2002
head(df)
df <- df[df$county != "" & df$precinct_abbrv != "",]
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")   
votes_2002 <- df[with(df, order(id)), ]

df <- votes_2004 
head(df)
df <- df[df$county != "" & df$precinct_abbrv != "",]
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")   
votes_2004 <- df[with(df, order(id)), ]

df <- votes_2006
head(df)
df <- df[df$county != "" & df$precinct_abbrv != "",]
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")   
votes_2006 <- df[with(df, order(id)), ]

df <- votes_2008 
head(df)
df <- df[df$county != "" & df$precinct != "",]
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct)), sep="_")   
votes_2008 <- df[with(df, order(id)), ]

df <- votes_2010 
head(df)
df <- df[df$county != "" & df$precinct != "",]
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct)), sep="_")   
votes_2010 <- df[with(df, order(id)), ]

df <- votes_2012 
head(df)
df <- df[df$county != "" & df$precinct != "",]
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct)), sep="_")   
votes_2012 <- df[with(df, order(id)), ]

df <- votes_2014 
head(df)
df <- df[df$county != "" & df$precinct != "",]
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct)), sep="_")   
votes_2014 <- df[with(df, order(id)), ]

df <- votes_2016 
head(df)
df <- df[df$county != "" & df$precinct != "",]
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct)), sep="_")   
votes_2016 <- df[with(df, order(id)), ]

df <- votes_2018 
head(df)
df <- df[df$county != "" & df$precinct != "",]
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct)), sep="_")   
votes_2018 <- df[with(df, order(id)), ]

length(
  Reduce(intersect, list(votes_2002$id, votes_2004$id, votes_2006$id, votes_2008$id, votes_2010$id, 
                         votes_2012$id, votes_2014$id, votes_2016$id, votes_2018$id))
)



df <- demo_2002
head(df)
df <- df[df$county != "" & df$precinct_abbrv != "",]
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")   
demo_2002 <- df[with(df, order(id)), ]

df <- demo_2004 
head(df)
df <- df[df$county != "" & df$precinct_abbrv != "",]
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")   
demo_2004 <- df[with(df, order(id)), ]

df <- demo_2006
head(df)
df <- df[df$county != "" & df$precinct_abbrv != "",]
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")   
demo_2006 <- df[with(df, order(id)), ]

df <- demo_2008 
head(df)
df <- df[df$county != "" & df$precinct_abbrv != "",]
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")   
demo_2008 <- df[with(df, order(id)), ]

df <- demo_2010 
head(df)
df <- df[df$county != "" & df$precinct_abbrv != "",]
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")   
demo_2010 <- df[with(df, order(id)), ]

df <- demo_2012 
head(df)
df <- df[df$county != "" & df$precinct_abbrv != "",]
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")   
df$id2 <- paste(tolower(trimws(df$county)), tolower(trimws(df$vtd_abbrv)), sep="_")   
demo_2012 <- df[with(df, order(id)), ]

df <- demo_2014 
head(df)
df <- df[df$county != "" & df$precinct_abbrv != "",]
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")   
df$id2 <- paste(tolower(trimws(df$county)), tolower(trimws(df$vtd_abbrv)), sep="_")   
demo_2014 <- df[with(df, order(id)), ]

df <- demo_2016 
head(df)
df <- df[df$county != "" & df$precinct_abbrv != "",]
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")   
df$id2 <- paste(tolower(trimws(df$county)), tolower(trimws(df$vtd_abbrv)), sep="_")   
demo_2016 <- df[with(df, order(id)), ]

df <- demo_2018 
head(df)
df <- df[df$county != "" & df$precinct_abbrv != "",]
df$id <- paste(tolower(trimws(df$county)), tolower(trimws(df$precinct_abbrv)), sep="_")   
df$id2 <- paste(tolower(trimws(df$county)), tolower(trimws(df$vtd_abbrv)), sep="_")   
demo_2018 <- df[with(df, order(id)), ]

length(
  Reduce(intersect, list(demo_2002$id, demo_2004$id, demo_2006$id, demo_2008$id, demo_2010$id, 
                         demo_2012$id, demo_2014$id, demo_2016$id, demo_2018$id))
)

