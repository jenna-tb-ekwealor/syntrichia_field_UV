## this file demonstrates how to use a collection of functions to load and
## analyze ibutton temperature data
## written by 
## Andrew MacDonald, Feb 2012


# load in required packages -----------------------------------------------

library(reshape)
library(lattice)
library(ggplot2)
library(tidyverse)
library(lubridate)

setwd("/Users/jennaekwealor/Documents/dissertation_repositories/syntrichia_field_UV/data_loggers")

UV.filtered<-"#66C2A5" #teal
UV.transmitted<-"#FC8D62" #orange
Site.reference<-"#8DA0CB" #purple

# load my functions -------------------------------------------------------

source('ibutton.functions.R')


#### FEBRUARY 2019, 4 DAYS ####

ibutton.data.feb.temp <- read.ibutton.folder("ibutton_data/field/temp/feb2019")
ibutton.data.feb.RH <- read.ibutton.folder("ibutton_data/field/RH/feb2019")

## which ones failed? you could just check the number of rows in each, and pick 
## the ones which are suspiciously short, indicating that the ibutton stopped 
## recording temperature.  This function automates this process to make it a bit
## more objective: it points out ones which recorded less than the median number
## of datapoints for the experiment.  It assumes that all the ibuttons were
## supposed to run for equal amounts of time
id.broken(ibutton.data.feb.temp)
id.broken(ibutton.data.feb.RH)

## get the lengths for your own use:
sapply(ibutton.data.feb.temp,nrow)
sapply(ibutton.data.feb.RH,nrow)

## sometimes, fieldworkers record the data incorrectly -- for example, one 
## common mistake is to save data from the same ibutton twice with different 
## filenames.  However, each ibutton has a unique registration number.  check 
## for any number >1 in this table to identify this error. Additionally, if you
## recorded the registration numbers (written on the back of the ibuttons) you
## could use this to get them from the datafiles themselves

# table(get.registration.numbers("ibutton\ data/field/temp"))
# table(get.registration.numbers("ibutton\ data/field/RH"))

## correct the dates and make df 
## Now that the data is checked, this function takes the list of ibuttons and
## combines them together.  It also reformats the "Date.Time" variable, into a
## format that R recognizes as a date and time.
#ibutton.df <- as.data.frame(ibutton.data)
ibutton.df.temp.feb <- na.omit(ibuttons.to.data.frame(ibutton.data.feb.temp))
head(ibutton.df.temp.feb)
summary(ibutton.df.temp.feb)
# remove row names
rownames(ibutton.df.temp.feb) <- c()
# make new 'treatment' column
ibutton.df.temp.feb$treatment <- ifelse(grepl("-UV", ibutton.df.temp.feb$ibutton), "UV.filtered", ifelse(grepl("+UV", ibutton.df.temp.feb$ibutton), "UV.transmitted", ifelse(grepl("surface", ibutton.df.temp.feb$ibutton), "Site.reference", "")))

ibutton.df.RH.feb <- na.omit(ibuttons.to.data.frame(ibutton.data.feb.RH))
head(ibutton.df.RH.feb)
summary(ibutton.df.RH.feb)
# remove row names
rownames(ibutton.df.RH.feb) <- c()
# make new 'treatment' column
ibutton.df.RH.feb$treatment <- ifelse(grepl("-UV", ibutton.df.RH.feb$ibutton), "UV.filtered", ifelse(grepl("+UV", ibutton.df.RH.feb$ibutton), "UV.transmitted", ifelse(grepl("surface", ibutton.df.RH.feb$ibutton), "Site.reference", "")))



## want daily average high and low... need to be able to group by day


# fix format
ibutton.df.temp.feb$Date.Time <- as.POSIXct(ibutton.df.temp.feb$Date.Time)
ibutton.df.RH.feb$Date.Time <- as.POSIXct(ibutton.df.RH.feb$Date.Time)


# temp

# pull out the high TEMP from each day, for each treatment 
ibutton.df.temp.feb %>% 
  mutate(hour = hour(Date.Time)) %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, treatment) %>% 
  dplyr::summarize(high = max(Value), n = n(), sd.Value = sd(Value)) -> ibutton.df.temp.feb.dailyhigh

head(ibutton.df.temp.feb.dailyhigh)

ibutton.df.temp.feb.dailyhigh %>% 
  group_by(treatment) %>% 
  dplyr::summarize(meandailyhigh = mean(high), n = n(), sd.dailyhigh = sd(high)) -> ibutton.df.temp.feb.dailyhigh.mean
head(ibutton.df.temp.feb.dailyhigh.mean)


# high temp stats 
pwc.ibutton.df.temp.feb.dailyhigh <- ibutton.df.temp.feb.dailyhigh %>% ungroup() %>% rstatix::t_test(high ~ treatment, paired = TRUE,p.adjust.method = "BH")

pwc.ibutton.df.temp.feb.dailyhigh

# # A tibble: 1 x 8
# .y.   group1      group2            n1    n2 statistic    df      p
# * <chr> <chr>       <chr>          <int> <int>     <dbl> <dbl>  <dbl>
#   1 high  UV.filtered UV.transmitted     4     4     -3.10     3 0.0533

# not taking into account paired dates
anova.ibutton.df.temp.feb.dailyhigh <-  ibutton.df.temp.feb.dailyhigh %>% ungroup() %>% 
  rstatix::anova_test(high ~ treatment)

rstatix::get_anova_table(anova.ibutton.df.temp.feb.dailyhigh)

# ANOVA Table (type II tests)
# 
# Effect DFn DFd     F     p p<.05   ges
# 1 treatment   1   6 0.326 0.589       0.052







# pull out the low TEMP from each day, for each treatment 
ibutton.df.temp.feb %>% 
  mutate(hour = hour(Date.Time)) %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, treatment) %>% 
  dplyr::summarize(low = min(Value), n = n(), sd.Value = sd(Value)) -> ibutton.df.temp.feb.dailylow
head(ibutton.df.temp.feb.dailylow)

ibutton.df.temp.feb.dailylow %>% 
  group_by(treatment) %>% 
  dplyr::summarize(meandailylow = mean(low), n = n(), sd.dailylow = sd(low)) -> ibutton.df.temp.feb.dailylow.mean
head(ibutton.df.temp.feb.dailylow.mean)



# low temp stats 
pwc.ibutton.df.temp.feb.dailylow <- ibutton.df.temp.feb.dailylow %>% ungroup() %>% rstatix::t_test(low ~ treatment, paired = TRUE)

pwc.ibutton.df.temp.feb.dailylow

# # A tibble: 1 x 8
# .y.   group1      group2            n1    n2 statistic    df     p
# * <chr> <chr>       <chr>          <int> <int>     <dbl> <dbl> <dbl>
#   1 low   UV.filtered UV.transmitted     4     4    -0.938     3 0.417

# not taking into account paired dates
anova.ibutton.df.temp.feb.dailylow <-  ibutton.df.temp.feb.dailylow %>% ungroup() %>% 
  rstatix::anova_test(low ~ treatment)

rstatix::get_anova_table(anova.ibutton.df.temp.feb.dailylow)

# ANOVA Table (type II tests)
# 
# Effect DFn DFd     F     p p<.05   ges
# 1 treatment   1   6 0.007 0.936       0.001



# RH

# pull out the high RH from each day, for each treatment 
ibutton.df.RH.feb %>% 
  mutate(hour = hour(Date.Time)) %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, treatment) %>% 
  dplyr::summarize(high = max(Value), n = n(), sd.dailylow = sd(Value)) -> ibutton.df.RH.feb.dailyhigh

head(ibutton.df.RH.feb.dailyhigh)

ibutton.df.RH.feb.dailyhigh %>% 
  group_by(treatment) %>% 
  dplyr::summarize(meandailyhigh = mean(high), n = n(), sd.dailylow = sd(high)) -> ibutton.df.RH.feb.dailyhigh.mean
head(ibutton.df.RH.feb.dailyhigh.mean)



# high RH stats 
pwc.ibutton.df.RH.feb.dailyhigh <- ibutton.df.RH.feb.dailyhigh %>% ungroup() %>% rstatix::t_test(high ~ treatment, paired = TRUE)

pwc.ibutton.df.RH.feb.dailyhigh

# # A tibble: 1 x 8
# .y.   group1      group2            n1    n2 statistic    df      p
# * <chr> <chr>       <chr>          <int> <int>     <dbl> <dbl>  <dbl>
#   1 high  UV.filtered UV.transmitted     4     4      3.27     3 0.0468

# not taking into account paired dates
anova.ibutton.df.RH.feb.dailyhigh <-  ibutton.df.RH.feb.dailyhigh %>% ungroup() %>% 
  rstatix::anova_test(high ~ treatment)

rstatix::get_anova_table(anova.ibutton.df.RH.feb.dailyhigh)

# ANOVA Table (type II tests)
# 
# Effect DFn DFd      F     p p<.05  ges
# 1 treatment   1   6 10.689 0.017     * 0.64




# pull out the low RH from each day, for each treatment 
ibutton.df.RH.feb %>% 
  mutate(hour = hour(Date.Time)) %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, treatment) %>% 
  dplyr::summarize(low = min(Value), n = n(), sd.Value = sd(Value)) -> ibutton.df.RH.feb.dailylow
head(ibutton.df.RH.feb.dailylow)

ibutton.df.RH.feb.dailylow %>% 
  group_by(treatment) %>% 
  dplyr::summarize(meandailylow = mean(low), n = n(), sd.dailylow = sd(low)) -> ibutton.df.RH.feb.dailylow.mean
head(ibutton.df.RH.feb.dailylow.mean)



# low RH stats 
pwc.ibutton.df.RH.feb.dailylow <- ibutton.df.RH.feb.dailylow %>% ungroup() %>% rstatix::t_test(low ~ treatment, paired = TRUE)

pwc.ibutton.df.RH.feb.dailylow

# # A tibble: 1 x 8
# .y.   group1      group2            n1    n2 statistic    df     p
# * <chr> <chr>       <chr>          <int> <int>     <dbl> <dbl> <dbl>
#   1 low   UV.filtered UV.transmitted     4     4     -1.10     3 0.352

# not taking into account paired dates
anova.ibutton.df.RH.feb.dailylow <-  ibutton.df.RH.feb.dailylow %>% ungroup() %>% 
  rstatix::anova_test(low ~ treatment)

rstatix::get_anova_table(anova.ibutton.df.RH.feb.dailylow)

# ANOVA Table (type II tests)
# 
# Effect DFn DFd     F     p p<.05      ges
# 1 treatment   1   6 0.005 0.948       0.000769




##### SEPTEMBER 2019! 2 DAYS! #####

ibutton.data.sept.temp <- read.ibutton.folder("ibutton\ data/field/temp/sept2019")
ibutton.data.sept.RH <- read.ibutton.folder("ibutton\ data/field/RH/sept2019")

## which ones failed? you could just check the number of rows in each, and pick 
## the ones which are suspiciously short, indicating that the ibutton stopped 
## recording temperature.  This function automates this process to make it a bit
## more objective: it points out ones which recorded less than the median number
## of datapoints for the experiment.  It assumes that all the ibuttons were
## supposed to run for equal amounts of time
id.broken(ibutton.data.sept.temp)
id.broken(ibutton.data.sept.RH)

## get the lengths for your own use:
sapply(ibutton.data.sept.temp,nrow)
sapply(ibutton.data.sept.RH,nrow)

## sometimes, fieldworkers record the data incorrectly -- for example, one 
## common mistake is to save data from the same ibutton twice with different 
## filenames.  However, each ibutton has a unique registration number.  check 
## for any number >1 in this table to identify this error. Additionally, if you
## recorded the registration numbers (written on the back of the ibuttons) you
## could use this to get them from the datafiles themselves

# table(get.registration.numbers("ibutton\ data/field/temp"))
# table(get.registration.numbers("ibutton\ data/field/RH"))

## correct the dates and make df 
## Now that the data is checked, this function takes the list of ibuttons and
## combines them together.  It also reformats the "Date.Time" variable, into a
## format that R recognizes as a date and time.
#ibutton.df <- as.data.frame(ibutton.data)
ibutton.df.temp.sept <- na.omit(ibuttons.to.data.frame(ibutton.data.sept.temp))
head(ibutton.df.temp.sept)
summary(ibutton.df.temp.sept)
# remove row names
rownames(ibutton.df.temp.sept) <- c()
# make new 'treatment' column
ibutton.df.temp.sept$treatment <- ifelse(grepl("-UV", ibutton.df.temp.sept$ibutton), "UV.filtered", ifelse(grepl("+UV", ibutton.df.temp.sept$ibutton), "UV.transmitted", ifelse(grepl("surface", ibutton.df.temp.sept$ibutton), "Site.reference", "")))

ibutton.df.RH.sept <- na.omit(ibuttons.to.data.frame(ibutton.data.sept.RH))
head(ibutton.df.RH.sept)
summary(ibutton.df.RH.sept)
# remove row names
rownames(ibutton.df.RH.sept) <- c()
# make new 'treatment' column
ibutton.df.RH.sept$treatment <- ifelse(grepl("-UV", ibutton.df.RH.sept$ibutton), "UV.filtered", ifelse(grepl("+UV", ibutton.df.RH.sept$ibutton), "UV.transmitted", ifelse(grepl("surface", ibutton.df.RH.sept$ibutton), "Site.reference", "")))



## want daily average high and low... need to be able to group by day


# fix format
ibutton.df.temp.sept$Date.Time <- as.POSIXct(ibutton.df.temp.sept$Date.Time)
ibutton.df.RH.sept$Date.Time <- as.POSIXct(ibutton.df.RH.sept$Date.Time)


# temp

# pull out the high TEMP from each day, for each treatment 
ibutton.df.temp.sept %>% 
  mutate(hour = hour(Date.Time)) %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, treatment) %>% 
  dplyr::summarize(high = max(Value), n = n(), sd.Value = sd(Value)) -> ibutton.df.temp.sept.dailyhigh

head(ibutton.df.temp.sept.dailyhigh)

ibutton.df.temp.sept.dailyhigh %>% 
  group_by(treatment) %>% 
  dplyr::summarize(meandailyhigh = mean(high), n = n(), sd.dailyhigh = sd(high)) -> ibutton.df.temp.sept.dailyhigh.mean
head(ibutton.df.temp.sept.dailyhigh.mean)


# high temp stats 
pwc.ibutton.df.temp.sept.dailyhigh <- ibutton.df.temp.sept.dailyhigh %>% ungroup() %>% rstatix::t_test(high ~ treatment, paired = TRUE)

pwc.ibutton.df.temp.sept.dailyhigh

# # A tibble: 3 x 10
# .y.   group1         group2            n1    n2 statistic    df     p p.adj p.adj.signif
# * <chr> <chr>          <chr>          <int> <int>     <dbl> <dbl> <dbl> <dbl> <chr>       
#   1 high  Site.reference UV.filtered        2     2    -0.812     1 0.566     1 ns          
# 2 high  Site.reference UV.transmitted     2     2     1.70      1 0.338     1 ns          
# 3 high  UV.filtered    UV.transmitted     2     2     1.23      1 0.433     1 ns 

# not taking into account paired dates
anova.ibutton.df.temp.sept.dailyhigh <-  ibutton.df.temp.sept.dailyhigh %>% ungroup() %>% 
  rstatix::anova_test(high ~ treatment)

rstatix::get_anova_table(anova.ibutton.df.temp.sept.dailyhigh)

# ANOVA Table (type II tests)
# 
# Effect DFn DFd     F     p p<.05   ges
# 1 treatment   2   3 0.481 0.659       0.243







# pull out the low TEMP from each day, for each treatment 
ibutton.df.temp.sept %>% 
  mutate(hour = hour(Date.Time)) %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, treatment) %>% 
  dplyr::summarize(low = min(Value), n = n(), sd.Value = sd(Value)) -> ibutton.df.temp.sept.dailylow
head(ibutton.df.temp.sept.dailylow)

ibutton.df.temp.sept.dailylow %>% 
  group_by(treatment) %>% 
  dplyr::summarize(meandailylow = mean(low), n = n(), sd.dailylow = sd(low)) -> ibutton.df.temp.sept.dailylow.mean
head(ibutton.df.temp.sept.dailylow.mean)



# low temp stats 
pwc.ibutton.df.temp.sept.dailylow <- ibutton.df.temp.sept.dailylow %>% ungroup() %>% rstatix::t_test(low ~ treatment, paired = TRUE)

pwc.ibutton.df.temp.sept.dailylow

# Error in t.test.default(x = c(18.586, 14.582), y = c(21.139, 17.135),  : 
#                           data are essentially constant

# not taking into account paired dates
anova.ibutton.df.temp.sept.dailylow <-  ibutton.df.temp.sept.dailylow %>% ungroup() %>% 
  rstatix::anova_test(low ~ treatment)

rstatix::get_anova_table(anova.ibutton.df.temp.sept.dailylow)

# ANOVA Table (type II tests)
# 
# Effect DFn DFd     F     p p<.05   ges
# 1 treatment   2   3 0.647 0.584       0.301



# RH

# pull out the high RH from each day, for each treatment 
ibutton.df.RH.sept %>% 
  mutate(hour = hour(Date.Time)) %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, treatment) %>% 
  dplyr::summarize(high = max(Value), n = n(), sd.dailylow = sd(Value)) -> ibutton.df.RH.sept.dailyhigh

head(ibutton.df.RH.sept.dailyhigh)

ibutton.df.RH.sept.dailyhigh %>% 
  group_by(treatment) %>% 
  dplyr::summarize(meandailyhigh = mean(high), n = n(), sd.dailylow = sd(high)) -> ibutton.df.RH.sept.dailyhigh.mean
head(ibutton.df.RH.sept.dailyhigh.mean)



# high RH stats 
pwc.ibutton.df.RH.sept.dailyhigh <- ibutton.df.RH.sept.dailyhigh %>% ungroup() %>% rstatix::t_test(high ~ treatment, paired = TRUE)

pwc.ibutton.df.RH.sept.dailyhigh

# # A tibble: 3 x 10
# .y.   group1         group2            n1    n2 statistic    df     p p.adj p.adj.signif
# * <chr> <chr>          <chr>          <int> <int>     <dbl> <dbl> <dbl> <dbl> <chr>       
#   1 high  Site.reference UV.filtered        2     2      1.65     1 0.347     1 ns          
# 2 high  Site.reference UV.transmitted     2     2      1.51     1 0.372     1 ns          
# 3 high  UV.filtered    UV.transmitted     2     2     -1.02     1 0.494     1 ns  

# not taking into account paired dates
anova.ibutton.df.RH.sept.dailyhigh <-  ibutton.df.RH.sept.dailyhigh %>% ungroup() %>% 
  rstatix::anova_test(high ~ treatment)

rstatix::get_anova_table(anova.ibutton.df.RH.sept.dailyhigh)

# ANOVA Table (type II tests)
# 
# Effect DFn DFd     F     p p<.05   ges
# 1 treatment   2   3 0.727 0.553       0.327




# pull out the low RH from each day, for each treatment 
ibutton.df.RH.sept %>% 
  mutate(hour = hour(Date.Time)) %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, treatment) %>% 
  dplyr::summarize(low = min(Value), n = n(), sd.Value = sd(Value)) -> ibutton.df.RH.sept.dailylow
head(ibutton.df.RH.sept.dailylow)

ibutton.df.RH.sept.dailylow %>% 
  group_by(treatment) %>% 
  dplyr::summarize(meandailylow = mean(low), n = n(), sd.dailylow = sd(low)) -> ibutton.df.RH.sept.dailylow.mean
head(ibutton.df.RH.sept.dailylow.mean)



# low RH stats 
pwc.ibutton.df.RH.sept.dailylow <- ibutton.df.RH.sept.dailylow %>% ungroup() %>% rstatix::t_test(low ~ treatment, paired = TRUE, p.adjust.method = "BH" )

pwc.ibutton.df.RH.sept.dailylow

# # A tibble: 3 x 10
# .y.   group1        group2           n1    n2 statistic    df     p p.adj p.adj.signif
# * <chr> <chr>         <chr>         <int> <int>     <dbl> <dbl> <dbl> <dbl> <chr>       
#   1 low   Site.referen… UV.filtered       2     2      8.08     1 0.078 0.118 ns          
# 2 low   Site.referen… UV.transmitt…     2     2     17.9      1 0.036 0.106 ns          
# 3 low   UV.filtered   UV.transmitt…     2     2     -1.16     1 0.452 0.452 ns 

# not taking into account paired dates

anova.ibutton.df.RH.sept.dailylow <-  ibutton.df.RH.sept.dailylow %>% ungroup() %>% 
  rstatix::anova_test(low ~ treatment)

rstatix::get_anova_table(anova.ibutton.df.RH.sept.dailylow)

# ANOVA Table (type II tests)
# 
# Effect DFn DFd      F     p p<.05 ges
# 1 treatment   2   3 13.441 0.032     * 0.9


###### combined feb and sept data set for paired t test ######

# high temp stats 

ibutton.df.combined.temp.dailyhigh <- rbind(ibutton.df.temp.feb.dailyhigh,ibutton.df.temp.sept.dailyhigh)

# Site.reference not a good comparison here since it is only from winter, plus can't do paired

ibutton.df.combined.temp.dailyhigh %>% 
  dplyr::filter(!treatment == "Site.reference") %>% 
  ungroup() %>% 
  rstatix::t_test(high ~ treatment, paired = TRUE) -> pwc.ibutton.df.combined.temp.dailyhigh

pwc.ibutton.df.combined.temp.dailyhigh

# # A tibble: 1 x 8
# .y.   group1      group2            n1    n2 statistic    df     p
# * <chr> <chr>       <chr>          <int> <int>     <dbl> <dbl> <dbl>
#   1 high  UV.filtered UV.transmitted     6     6    -0.417     5 0.694 

# not taking into account paired dates
anova.ibutton.df.combined.temp.dailyhigh <-  ibutton.df.combined.temp.dailyhigh %>% ungroup() %>% 
  rstatix::anova_test(high ~ treatment)

rstatix::get_anova_table(anova.ibutton.df.combined.temp.dailyhigh)

# ANOVA Table (type II tests)
# 
# Effect DFn DFd     F     p p<.05   ges
# 1 treatment   2  11 1.308 0.309       0.192


# low temp stats

ibutton.df.combined.temp.dailylow <- rbind(ibutton.df.temp.feb.dailylow,ibutton.df.temp.sept.dailylow)

ibutton.df.combined.temp.dailylow %>% 
  dplyr::filter(!treatment == "Site.reference") %>% 
  ungroup() %>% 
  rstatix::t_test(low ~ treatment, paired = TRUE) -> pwc.ibutton.df.combined.temp.dailylow

pwc.ibutton.df.combined.temp.dailylow

# # A tibble: 1 x 8
# .y.   group1      group2            n1    n2 statistic    df      p
# * <chr> <chr>       <chr>          <int> <int>     <dbl> <dbl>  <dbl>
#   1 low   UV.filtered UV.transmitted     6     6     -2.16     5 0.0829

# not taking into account paired dates
anova.ibutton.df.combined.temp.dailylow <-  ibutton.df.combined.temp.dailylow %>% ungroup() %>% 
  rstatix::anova_test(low ~ treatment)

rstatix::get_anova_table(anova.ibutton.df.combined.temp.dailylow)

# ANOVA Table (type II tests)
# 
# Effect DFn DFd     F     p p<.05   ges
# 1 treatment   2  11 1.013 0.395       0.155



# high RH stats 

ibutton.df.combined.RH.dailyhigh <- rbind(ibutton.df.RH.feb.dailyhigh,ibutton.df.RH.sept.dailyhigh)

# Site.reference not a good comparison here since it is only from winter, plus can't do paired

ibutton.df.combined.RH.dailyhigh %>% 
  dplyr::filter(!treatment == "Site.reference") %>% 
  ungroup() %>% 
  rstatix::t_test(high ~ treatment, paired = TRUE) -> pwc.ibutton.df.combined.RH.dailyhigh

pwc.ibutton.df.combined.RH.dailyhigh

# # A tibble: 1 x 8
# .y.   group1      group2            n1    n2 statistic    df     p
# * <chr> <chr>       <chr>          <int> <int>     <dbl> <dbl> <dbl>
#   1 high  UV.filtered UV.transmitted     6     6    -0.417     5 0.694 

# not taking into account paired dates
anova.ibutton.df.combined.RH.dailyhigh <-  ibutton.df.combined.RH.dailyhigh %>% ungroup() %>% 
  rstatix::anova_test(high ~ treatment)

rstatix::get_anova_table(anova.ibutton.df.combined.RH.dailyhigh)

# ANOVA Table (type II tests)
# 
# Effect DFn DFd     F     p p<.05   ges
# 1 treatment   2  11 1.308 0.309       0.192


# low RH stats 

ibutton.df.combined.RH.dailylow <- rbind(ibutton.df.RH.feb.dailylow,ibutton.df.RH.sept.dailylow)

# Site.reference not a good comparison here since it is only from winter, plus can't do paired

ibutton.df.combined.RH.dailylow %>% 
  dplyr::filter(!treatment == "Site.reference") %>% 
  ungroup() %>% 
  rstatix::t_test(low ~ treatment, paired = TRUE) -> pwc.ibutton.df.combined.RH.dailylow

pwc.ibutton.df.combined.RH.dailylow

# # A tibble: 1 x 8
# .y.   group1      group2            n1    n2 statistic    df     p
# * <chr> <chr>       <chr>          <int> <int>     <dbl> <dbl> <dbl>
#   1 low   UV.filtered UV.transmitted     6     6     -1.75     5  0.14

# not taking into account paired dates
anova.ibutton.df.combined.RH.dailylow <-  ibutton.df.combined.RH.dailylow %>% ungroup() %>% 
  rstatix::anova_test(low ~ treatment)

rstatix::get_anova_table(anova.ibutton.df.combined.RH.dailylow)

# ANOVA Table (type II tests)
# 
# Effect DFn DFd     F     p p<.05  ges
# 1 treatment   2  11 0.054 0.948       0.01

