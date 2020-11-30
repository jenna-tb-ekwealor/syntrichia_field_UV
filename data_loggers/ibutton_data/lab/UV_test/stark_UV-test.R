## this file demonstrates how to use a collection of functions to load and
## analyze ibutton temperature data
## written by 
## Andrew MacDonald, Feb 2012


# load in required packages -----------------------------------------------

library(reshape)
library(lattice)

setwd("/Users/jennaekwealor/Documents/dissertation_repositories/syntrichia_field_UV/data_loggers/")

# load my functions -------------------------------------------------------

source('ibutton.functions.R')

# analysis ----------------------------------------------------------------

ibutton.data.temp <- read.ibutton.folder("ibutton_data/lab/UV_test/temp")
ibutton.data.RH <- read.ibutton.folder("ibutton_data/lab/UV_test/RH")

## which ones failed? you could just check the number of rows in each, and pick 
## the ones which are suspiciously short, indicating that the ibutton stopped 
## recording temperature.  This function automates this process to make it a bit
## more objective: it points out ones which recorded less than the median number
## of datapoints for the experiment.  It assumes that all the ibuttons were
## supposed to run for equal amounts of time
id.broken(ibutton.data.temp)
id.broken(ibutton.data.RH)

## get the lengths for your own use:
sapply(ibutton.data.temp,nrow)
sapply(ibutton.data.RH,nrow)

## sometimes, fieldworkers record the data incorrectly -- for example, one 
## common mistake is to save data from the same ibutton twice with different 
## filenames.  However, each ibutton has a unique registration number.  check 
## for any number >1 in this table to identify this error. Additionally, if you
## recorded the registration numbers (written on the back of the ibuttons) you
## could use this to get them from the datafiles themselves

table(get.registration.numbers("ibutton_data/lab/recovery_chamber/temp"))
table(get.registration.numbers("ibutton_data/lab/recovery_chamber/RH"))

## correct the dates and make dataframe 
## Now that the data is checked, this function takes the list of ibuttons and
## combines them together.  It also reformats the "Date.Time" variable, into a
## format that R recognizes as a date and time.
#ibutton.dataframe <- as.data.frame(ibutton.data)
ibutton.dataframe.temp <- ibuttons.to.data.frame(ibutton.data.temp)
head(ibutton.dataframe.temp)
summary(ibutton.dataframe.temp)

ibutton.dataframe.RH <- ibuttons.to.data.frame(ibutton.data.RH)
head(ibutton.dataframe.RH)
summary(ibutton.dataframe.RH)

## Here are some ways to graph this data:
with(subset(ibutton.dataframe.temp,ibutton=="stark_UV_temp_06112019"),plot(Date.Time,Value,type="n"))
for(i in levels(ibutton.dataframe.temp$ibutton)){
  x <- which(ibutton.dataframe.temp$ibutton==i)
  lines(ibutton.dataframe.temp$Date.Time[x],ibutton.dataframe.temp$Value[x])
}

with(subset(ibutton.dataframe.RH,ibutton=="stark_UV_rh_06112019"),plot(Date.Time,Value,type="n"))
for(i in levels(ibutton.dataframe.RH$ibutton)){
  x <- which(ibutton.dataframe.RH$ibutton==i)
  lines(ibutton.dataframe.RH$Date.Time[x],ibutton.dataframe.RH$Value[x])
}

#### mean and stddev ####
mean(ibutton.dataframe.RH$Value)
# [1] 19.06311

sd(ibutton.dataframe.RH$Value)
# [1] 1.402376

mean(ibutton.dataframe.temp$Value)
# [1] 25.97545

sd(ibutton.dataframe.temp$Value)
# [1] 1.060471

