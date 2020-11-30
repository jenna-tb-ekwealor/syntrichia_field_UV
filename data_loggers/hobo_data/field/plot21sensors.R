library(stringr)
library(dplyr)

setwd("~/Documents/dissertation_repositories/syntrichia_field_UV/data_loggers/hobo_data/field")


## feb 9 - feb 12 2019 ##
twentyone.a.feb9 <- read.csv("Plot_21-A,_OP-3.csv")

twentyone.b.feb9 <- read.csv("Plot_21-B,_SUVT.csv")







## mar 26 2019 ##
twentyone.a.mar26 <- read.csv("Plot_21-A,_OP-3_0_03262019.csv")
# low = 6:30 am 
# high = 12:30 pm 

# to get all the highs and lows 
twentyone.a.highlow.mar26 = twentyone.a.mar26 %>%
  filter(str_detect(Date.Time, " 6:30| 12:30")) 

twentyone.b.mar26 <- read.csv("Plot_21-B,_SUVT_0_03262019.csv")
# low = 6:30 am 
# high = 12:30 pm 
# to get all the highs and lows 
twentyone.b.highlow.mar26 = twentyone.b.mar26 %>%
  filter(str_detect(Date.Time, " 6:30| 12:30")) 


?t.test
#compare temps
t.test(x = twentyone.a.highlow.mar26$Temp..C, y = twentyone.b.highlow.mar26$Temp..C, paired = TRUE)
#compare light
t.test(x = twentyone.a.highlow.mar26$Intensity.Lux, y = twentyone.b.highlow.mar26$Intensity.Lux, paired = TRUE)







## september 15 2019 ##
twentyone.a.sep15 <- read.csv("Plot_21-A,_OP-3_09152019.csv")
# low = 6:30 am 
# high = 12:30 pm 

# to get all the highs and lows 
twentyone.a.highlow.sep15 = twentyone.a.sep15 %>%
  filter(str_detect(Date.Time, " 6:30| 12:30")) 

twentyone.b.sep15 <- read.csv("Plot_21-B,_SUVT_09152019.csv")
# low = 6:30 am 
# high = 12:30 pm 
# to get all the highs and lows 
twentyone.b.highlow.sep15 = twentyone.b.sep15 %>%
  filter(str_detect(Date.Time, " 6:30| 12:30")) 


twentyone.c.sep15 <- read.csv("Plot_21-C,_control-ratmidden.csv")
# low = 6:30 am 
# high = 12:30 pm 
# to get all the highs and lows 
twentyone.c.highlow.sep15 = twentyone.c.sep15 %>%
  filter(str_detect(Date.Time, " 6:30| 12:30")) 

?t.test
#compare temps
t.test(x = twentyone.a.highlow.sep15$Temp..C, y = twentyone.b.highlow.sep15$Temp..C, paired = TRUE)
#compare light
t.test(x = twentyone.a.highlow.sep15$Intensity.Lux, y = twentyone.b.highlow.sep15$Intensity.Lux, paired = TRUE)
t.test(x = twentyone.b.highlow.sep15$Intensity.Lux, y = twentyone.c.highlow.sep15$Intensity.Lux, paired = TRUE)



