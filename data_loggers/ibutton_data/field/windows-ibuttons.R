## MODIFIED FROM and using Andrew MacDonald's (Feb 2012) ibutton temperature analysis script (https://github.com/aammd/ibutton.functions)

# load in required packages -----------------------------------------------
library(tidyverse)
library(lubridate)
library(rstatix)
library(dplyr)

 

setwd("/Users/jennaekwealor/Documents/dissertation_repositories/syntrichia_field_UV/data_loggers")

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
# add plot column
ibutton.df.temp.feb$plot <- ifelse(grepl("plot21", ibutton.df.temp.feb$ibutton), "twentyone", ifelse(grepl("plot22", ibutton.df.temp.feb$ibutton), "twentytwo", ifelse(grepl("plot23", ibutton.df.temp.feb$ibutton), "twentythree", "")))
ibutton.df.RH.feb$plot <- ifelse(grepl("plot21", ibutton.df.RH.feb$ibutton), "twentyone", ifelse(grepl("plot22", ibutton.df.RH.feb$ibutton), "twentytwo", ifelse(grepl("plot23", ibutton.df.RH.feb$ibutton), "twentythree", "")))

#### RH ####

# pull out the high RH from each day, for each treatment 
ibutton.df.RH.feb %>% 
  mutate(hour = hour(Date.Time)) %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, plot, treatment) %>% 
  dplyr::summarize(high = max(Value), n = n(), sd.Value = sd(Value)) -> ibutton.df.RH.feb.dailyhigh

head(ibutton.df.RH.feb.dailyhigh)

# find mean daily high for the whole measurement period, per plot
ibutton.df.RH.feb.dailyhigh %>% 
  group_by(plot, treatment) %>% 
  dplyr::summarize(meandailyhigh = mean(high), n = n(), sd.dailyhigh = sd(high)) -> ibutton.df.RH.feb.dailyhigh.mean
head(ibutton.df.RH.feb.dailyhigh.mean)


# one way anova with repeated measures (plot pairs/triplets)
anova.ibutton.df.RH.feb.dailyhigh <-  ibutton.df.RH.feb.dailyhigh %>% ungroup() %>%
  rstatix::anova_test(high ~ treatment, wid = plot)

rstatix::get_anova_table(anova.ibutton.df.RH.feb.dailyhigh)
# ANOVA Table (type II tests)
# 
# Effect DFn DFd     F     p p<.05   ges
# 1 treatment   1  22 2.306 0.143       0.095



# pull out the low RH from each day, for each treatment 
ibutton.df.RH.feb %>% 
  mutate(hour = hour(Date.Time)) %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, plot, treatment) %>% 
  dplyr::summarize(low = min(Value), n = n(), sd.Value = sd(Value)) -> ibutton.df.RH.feb.dailylow
head(ibutton.df.RH.feb.dailylow)

ibutton.df.RH.feb.dailylow %>% 
  group_by(plot, treatment) %>% 
  dplyr::summarize(meandailylow = mean(low), n = n(), sd.dailylow = sd(low)) -> ibutton.df.RH.feb.dailylow.mean
head(ibutton.df.RH.feb.dailylow.mean)


# one way anova with repeated measures (plot pairs/triplets)
anova.ibutton.df.RH.feb.dailylow <-  ibutton.df.RH.feb.dailylow %>% ungroup() %>%
  rstatix::anova_test(low ~ treatment, wid = plot)

rstatix::get_anova_table(anova.ibutton.df.RH.feb.dailylow)

# ANOVA Table (type II tests)
# 
# Effect DFn DFd     F     p p<.05   ges
# 1 treatment   1  22 0.057 0.814       0.003




##### SEPTEMBER 2019! 2 DAYS! #####

ibutton.data.sept.temp <- read.ibutton.folder("ibutton_data/field/temp/sept2019")
ibutton.data.sept.RH <- read.ibutton.folder("ibutton_data/field/RH/sept2019")

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
# add plot column
ibutton.df.temp.sept$plot <- ifelse(grepl("plot21", ibutton.df.temp.sept$ibutton), "twentyone", ifelse(grepl("plot22", ibutton.df.temp.sept$ibutton), "twentytwo", ifelse(grepl("plot23", ibutton.df.temp.sept$ibutton), "twentythree", "")))
ibutton.df.RH.sept$plot <- ifelse(grepl("plot21", ibutton.df.RH.sept$ibutton), "twentyone", ifelse(grepl("plot22", ibutton.df.RH.sept$ibutton), "twentytwo", ifelse(grepl("plot23", ibutton.df.RH.sept$ibutton), "twentythree", "")))




#### RH ####

# pull out the high RH from each day, for each treatment 
ibutton.df.RH.sept %>% 
  mutate(hour = hour(Date.Time)) %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, plot, treatment) %>% 
  dplyr::summarize(high = max(Value), n = n(), sd.Value = sd(Value)) -> ibutton.df.RH.sept.dailyhigh

head(ibutton.df.RH.sept.dailyhigh)

# find mean daily high for the whole measurement period, per plot
ibutton.df.RH.sept.dailyhigh %>% 
  group_by(plot, treatment) %>% 
  dplyr::summarize(meandailyhigh = mean(high), n = n(), sd.dailyhigh = sd(high)) -> ibutton.df.RH.sept.dailyhigh.mean
head(ibutton.df.RH.sept.dailyhigh.mean)


# one way anova with repeated measures (plot pairs/triplets)
anova.ibutton.df.RH.sept.dailyhigh <-  ibutton.df.RH.sept.dailyhigh %>% ungroup() %>%
  rstatix::anova_test(high ~ treatment, wid = plot)

rstatix::get_anova_table(anova.ibutton.df.RH.sept.dailyhigh)
# ANOVA Table (type II tests)
# 
# Effect DFn DFd     F     p p<.05   ges
# 1 treatment   2   3 0.727 0.553       0.327


# pull out the low TEMP from each day, for each treatment 
ibutton.df.RH.sept %>% 
  mutate(hour = hour(Date.Time)) %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, plot, treatment) %>% 
  dplyr::summarize(low = min(Value), n = n(), sd.Value = sd(Value)) -> ibutton.df.RH.sept.dailylow
head(ibutton.df.RH.sept.dailylow)

ibutton.df.RH.sept.dailylow %>% 
  group_by(plot, treatment) %>% 
  dplyr::summarize(meandailylow = mean(low), n = n(), sd.dailylow = sd(low)) -> ibutton.df.RH.sept.dailylow.mean
head(ibutton.df.RH.sept.dailylow.mean)


# one way anova with repeated measures (plot pairs/triplets)
anova.ibutton.df.RH.sept.dailylow <-  ibutton.df.RH.sept.dailylow %>% ungroup() %>%
  rstatix::anova_test(low ~ treatment, wid = plot)

rstatix::get_anova_table(anova.ibutton.df.RH.sept.dailylow)

# ANOVA Table (type II tests)
# 
# Effect DFn DFd      F     p p<.05 ges
# 1 treatment   2   3 13.441 0.032     * 0.9


# posthoc test to find which treatment 
pwc.ibutton.df.RH.sept.dailylow <- ibutton.df.RH.sept.dailylow  %>% ungroup() %>% rstatix::tukey_hsd(low ~ treatment, paired = TRUE,p.adjust.method = "NH")
pwc.ibutton.df.RH.sept.dailylow
# 
# # A tibble: 3 x 8
# term      group1         group2         estimate conf.low conf.high  p.adj p.adj.signif
# * <chr>     <chr>          <chr>             <dbl>    <dbl>     <dbl>  <dbl> <chr>       
#   1 treatment Site.reference UV.filtered      -13.2    -24.4     -1.92  0.033  *           
#   2 treatment Site.reference UV.transmitted   -10.6    -21.8      0.663 0.0584 ns          
# 3 treatment UV.filtered    UV.transmitted     2.58    -8.66    13.8   0.646  ns          


# #### temp ####
# # pull out the high TEMP from each day, for each treatment 
# ibutton.df.temp.sept %>% 
#   mutate(hour = hour(Date.Time)) %>% 
#   mutate(date = date(Date.Time)) %>% 
#   group_by(date, plot, treatment) %>% 
#   dplyr::summarize(high = max(Value), n = n(), sd.Value = sd(Value)) -> ibutton.df.temp.sept.dailyhigh
# 
# head(ibutton.df.temp.sept.dailyhigh)
# 
# # find mean daily high for the whole measurement period, per plot
# ibutton.df.temp.sept.dailyhigh %>% 
#   group_by(plot, treatment) %>% 
#   dplyr::summarize(meandailyhigh = mean(high), n = n(), sd.dailyhigh = sd(high)) -> ibutton.df.temp.sept.dailyhigh.mean
# head(ibutton.df.temp.sept.dailyhigh.mean)
# 
# 
# # one way anova with repeated measures (plot pairs/triplets)
# anova.ibutton.df.temp.sept.dailyhigh <-  ibutton.df.temp.sept.dailyhigh %>% ungroup() %>%
#   rstatix::anova_test(high ~ treatment, wid = plot)
# 
# rstatix::get_anova_table(anova.ibutton.df.temp.sept.dailyhigh)
# 
# # ANOVA Table (type II tests)
# # 
# # Effect DFn DFd     F     p p<.05   ges
# # 1 treatment   2   3 0.481 0.659       0.243
# 
# 
# 
# 
# 
# # pull out the low TEMP from each day, for each treatment 
# ibutton.df.temp.sept %>% 
#   mutate(hour = hour(Date.Time)) %>% 
#   mutate(date = date(Date.Time)) %>% 
#   group_by(date, plot, treatment) %>% 
#   dplyr::summarize(low = min(Value), n = n(), sd.Value = sd(Value)) -> ibutton.df.temp.sept.dailylow
# head(ibutton.df.temp.sept.dailylow)
# 
# ibutton.df.temp.sept.dailylow %>% 
#   group_by(plot, treatment) %>% 
#   dplyr::summarize(meandailylow = mean(low), n = n(), sd.dailylow = sd(low)) -> ibutton.df.temp.sept.dailylow.mean
# head(ibutton.df.temp.sept.dailylow.mean)
# 
# 
# # one way anova with repeated measures (plot pairs/triplets)
# anova.ibutton.df.temp.sept.dailylow <-  ibutton.df.temp.sept.dailylow %>% ungroup() %>%
#   rstatix::anova_test(low ~ treatment, wid = plot)
# 
# rstatix::get_anova_table(anova.ibutton.df.temp.sept.dailylow)
# 
# 
# # Effect DFn DFd     F     p p<.05   ges
# # 1 treatment   2   3 0.647 0.584       0.301

