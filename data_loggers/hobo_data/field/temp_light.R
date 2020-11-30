library(tidyverse)
library(lubridate)
library(rstatix)
library(dplyr)

setwd("~/Documents/dissertation_repositories/syntrichia_field_UV/data_loggers/hobo_data/field")


# feb-june 2019
#### plot 21 ####

### a: UV filtered ###
# load files
twentyone.a.febjune <- read.csv("Plot_21-A,_OP-3_febjune2019.csv")
twentyone.a.june <- read.csv("Plot_21-A,_OP-3_june2019.csv")
# bind the files together since dates are not overlapping
twentyone.a.fjj <- rbind(twentyone.a.febjune, twentyone.a.june)
# make new 'plot' column for paired testing
twentyone.a.fjj$plot <- rep("twentyone")

### b: UV transmitted ###
# load files
twentyone.b.febjune <- read.csv("Plot_21-B,_SUVT_febjune2019.csv")
twentyone.b.june <- read.csv("Plot_21-B,_SUVT_june2019.csv")
# bind the files together since dates are not overlapping
twentyone.b.fjj <- rbind(twentyone.b.febjune, twentyone.b.june)
# make new 'plot' column for paired testing
twentyone.b.fjj$plot <- rep("twentyone")


#### plot 22 ####

### a: UV filtered ###
# load files
twentytwo.a.febjune <- read.csv("Plot_22-A,_OP-3_febjune2019.csv")
# make new 'plot' column for paired testing
twentytwo.a.febjune$plot <- rep("twentytwo")

### b: UV transmitted ###
# load files
twentytwo.b.febjune <- read.csv("Plot_22-B,_SUVT_febjune2019.csv")
# make new 'plot' column for paired testing
twentytwo.b.febjune$plot <- rep("twentytwo")
# fix column name to match the others
twentytwo.b.febjune %>% dplyr::rename(Intensity.Lux = Intensity.Lux.) -> twentytwo.b.febjune


#### plot 23 ####

### a: UV filtered ###
# load files
twentythree.a.febjune <- read.csv("Plot_23-A,_OP-3_febjune2019.csv")
twentythree.a.june <- read.csv("Plot_23-A,_OP-3_june2019.csv")
# fix names to all match
twentythree.a.june %>% dplyr::rename(Date.Time = Date.Time.GMT.08.00) -> twentythree.a.june
# bind the files together since dates are not overlapping
twentythree.a.fjj <- rbind(twentythree.a.febjune, twentythree.a.june)
# make new 'plot' column for paired testing
twentythree.a.fjj$plot <- rep("twentythree")


### b: UV transmitted ###
# load files
twentythree.b.febjune <- read.csv("Plot_23-B,_SUVT_febjune2019.csv")
twentythree.b.june <- read.csv("Plot_23-B,_SUVT_june2019.csv")
# fix names to all match
twentythree.b.june %>% dplyr::rename(Date.Time = Date.Time..GMT.08.00) -> twentythree.b.june
twentythree.b.febjune %>% dplyr::rename(Date.Time = Date.Time..GMT.08.00) -> twentythree.b.febjune
# bind the files together since dates are not overlapping
twentythree.b.fjj <- rbind(twentythree.b.febjune, twentythree.b.june)
# make new 'plot' column for paired testing
twentythree.b.fjj$plot <- rep("twentythree")
# fix names of other columns for rbind
twentythree.b.fjj %>% dplyr::rename(Temp..C = Temp...C, Intensity.Lux = Intensity..Lux) -> twentythree.b.fjj



### combine each single treatment type from all three plots ###
uv.filtered <- rbind(twentyone.a.fjj, twentytwo.a.febjune, twentythree.a.fjj)
# make new 'treatment' column
uv.filtered$treatment <- rep("UV.filtered")


uv.transmitted <- rbind(twentyone.b.fjj, twentytwo.b.febjune, twentythree.b.fjj)
# make new 'treatment' column
uv.transmitted$treatment <- rep("UV.transmitted")


# combine all three treatments into one data frame
alldata <- rbind(uv.filtered, uv.transmitted)
alldata <- na.omit(alldata)


# want daily average high and low... need to be able to group by day
# fix format
alldata$Date.Time <- as.character(alldata$Date.Time)
alldata$Date.Time <- as.POSIXct(alldata$Date.Time, format="%Y-%m-%d %H:%M:%S")
alldata <- na.omit(alldata)


#### TEMP ####
#### high ####
# pull out the high TEMP from each day, for each treatment 
alldata %>% 
  mutate(hour = lubridate::hour(Date.Time)) %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, plot, treatment) %>% 
  dplyr::summarize(high = max(Temp..C), n = n(), sd.Value = sd(Temp..C)) -> temp.dailyhigh

head(temp.dailyhigh)



# calculate mean daily high per month
temp.dailyhigh %>% 
  mutate(month = month(date)) %>% 
  group_by(month, plot, treatment) %>% 
  dplyr::summarize(meandailyhigh = mean(high), n = n(), sd.dailyhigh = sd(high)) -> temp.dailyhighpermonth.mean
head(temp.dailyhighpermonth.mean)
# 
# # calculate mean daily high (mean) per month for all 3 plots
# # data for Site.reference is only available for 2019-06-06, 2019-06-07, and 2019 06-08
mean.temp.dailyhighpermonth <- temp.dailyhigh %>% dplyr::filter(., treatment != "Site.reference") %>%
  mutate(month = month(date)) %>% group_by(month, treatment) %>% dplyr::summarize(mean = mean(high), n = n(), sd = sd(high))
mean.temp.dailyhighpermonth
#
# # A tibble: 10 x 5
# # Groups:   month [5]
# month treatment       mean     n    sd
# <dbl> <chr>          <dbl> <int> <dbl>
#   1     2 UV.filtered     24.2    60 10.8 
# 2     2 UV.transmitted  23.8    60 10.7 
# 3     3 UV.filtered     43.4    93  8.87
# 4     3 UV.transmitted  43.4    93  8.62
# 5     4 UV.filtered     61.5    90  7.22
# 6     4 UV.transmitted  61.4    90  6.99
# 7     5 UV.filtered     61.7    93  8.04
# 8     5 UV.transmitted  61.0    93  7.88
# 9     6 UV.filtered     68.0    22 10.4 
# 10     6 UV.transmitted  66.9    22  9.64
# 
# 




# calculate mean daily high (mean) per month for each plot separately
# data for Site.reference is only available for 2019-06-06, 2019-06-07, and 2019 06-08
mean.temp.dailyhighpermonth.plot <- temp.dailyhigh %>% dplyr::filter(., treatment != "Site.reference") %>%
  mutate(month = month(date)) %>% group_by(month, plot, treatment) %>% dplyr::summarize(mean = mean(high))
mean.temp.dailyhighpermonth.plot
# # A tibble: 30 x 4
# # Groups:   month, plot [15]
# month plot        treatment       mean
# <dbl> <chr>       <chr>          <dbl>
#   1     2 twentyone   UV.filtered     23.2
# 2     2 twentyone   UV.transmitted  24.4
# 3     2 twentythree UV.filtered     24.6
# 4     2 twentythree UV.transmitted  24.1
# 5     2 twentytwo   UV.filtered     24.9
# 6     2 twentytwo   UV.transmitted  22.9
# 7     3 twentyone   UV.filtered     41.7
# 8     3 twentyone   UV.transmitted  43.9
# 9     3 twentythree UV.filtered     46.0
# 10     3 twentythree UV.transmitted  43.9
# # … with 20 more rows


# # t test
# pwc.temp.dailyhighpermonth.plot <- mean.temp.dailyhighpermonth.plot %>% group_by(plot) %>%
#                               rstatix::t_test(mean ~ treatment, paired = T, detailed = T)
# 
# pwc.temp.dailyhighpermonth.plot %>% adjust_pvalue(method = "BH") -> pwc.temp.dailyhighpermonth.plot

# # A tibble: 3 x 15
# plot    estimate .y.   group1  group2     n1    n2 statistic     p    df conf.low conf.high method alternative p.adj
# <chr>      <dbl> <chr> <chr>   <chr>   <int> <int>     <dbl> <dbl> <dbl>    <dbl>     <dbl> <chr>  <chr>       <dbl>
#   1 twenty…  -0.0845 mean  UV.fil… UV.tra…     5     5   -0.104  0.923     4   -2.35       2.18 T-test two.sided   0.95 
# 2 twenty…   1.52   mean  UV.fil… UV.tra…     5     5    3.64   0.022     4    0.360      2.67 T-test two.sided   0.066
# 3 twenty…  -0.0395 mean  UV.fil… UV.tra…     5     5   -0.0664 0.95      4   -1.69       1.61 T-test two.sided   0.95 

#### anova ####
# one way anova with repeated measures (plot pairs/triplets)
anova.mean.temp.dailyhighpermonth.plot <-  mean.temp.dailyhighpermonth.plot %>% ungroup() %>%
  rstatix::anova_test(mean ~ treatment, wid = plot)

rstatix::get_anova_table(anova.mean.temp.dailyhighpermonth.plot)

# ANOVA Table (type II tests)
# 
# Effect DFn DFd     F     p p<.05      ges
# 1 treatment   1  28 0.006 0.939       0.000211




#### low ####

# pull out the low TEMP from each day, for each treatment 
alldata %>% 
  mutate(hour = hour(Date.Time)) %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, plot, treatment) %>% 
  dplyr::summarize(low = min(Temp..C), n = n(), sd.Value = sd(Temp..C)) -> temp.dailylow

head(temp.dailylow)



# calculate mean daily low per month
temp.dailylow %>% 
  mutate(month = month(date)) %>% 
  group_by(month, plot, treatment) %>% 
  dplyr::summarize(meandailylow = mean(low), n = n(), sd.dailylow = sd(low)) -> temp.dailylowpermonth.mean
head(temp.dailylowpermonth.mean)
# 
# calculate mean daily low (mean) per month for all 3 plots
# data for Site.reference is only available for 2019-06-06, 2019-06-07, and 2019 06-08
mean.temp.dailylowpermonth <- temp.dailylow %>% dplyr::filter(., treatment != "Site.reference") %>%
  mutate(month = month(date)) %>% group_by(month, treatment) %>% dplyr::summarize(mean = mean(low), n = n(), sd = sd(low))
mean.temp.dailylowpermonth
# 
# # A tibble: 10 x 5
# # Groups:   month [5]
# month treatment        mean     n    sd
# <dbl> <chr>           <dbl> <int> <dbl>
#   1     2 UV.filtered    -1.03     60  2.18
# 2     2 UV.transmitted -0.746    60  2.08
# 3     3 UV.filtered     3.59     93  2.25
# 4     3 UV.transmitted  3.83     93  2.17
# 5     4 UV.filtered     8.95     90  3.54
# 6     4 UV.transmitted  9.11     90  3.62
# 7     5 UV.filtered     9.70     93  3.11
# 8     5 UV.transmitted  9.84     93  3.16
# 9     6 UV.filtered    16.4      22  3.83
# 10     6 UV.transmitted 16.3      22  3.41
# 
# 

# calculate mean daily low (mean) per month for each plot separately
# data for Site.reference is only available for 2019-06-06, 2019-06-07, and 2019 06-08
mean.temp.dailylowpermonth.plot <- temp.dailylow %>% dplyr::filter(., treatment != "Site.reference") %>%
  mutate(month = month(date)) %>% group_by(month, plot, treatment) %>% dplyr::summarize(mean = mean(low))
mean.temp.dailylowpermonth.plot
# # A tibble: 30 x 4
# # Groups:   month, plot [15]
# month plot        treatment        mean
# <dbl> <chr>       <chr>           <dbl>
#   1     2 twentyone   UV.filtered    -0.836
# 2     2 twentyone   UV.transmitted -0.461
# 3     2 twentythree UV.filtered    -0.978
# 4     2 twentythree UV.transmitted -0.725
# 5     2 twentytwo   UV.filtered    -1.29 
# 6     2 twentytwo   UV.transmitted -1.05 
# 7     3 twentyone   UV.filtered     3.83 
# 8     3 twentyone   UV.transmitted  3.95 
# 9     3 twentythree UV.filtered     3.41 
# 10     3 twentythree UV.transmitted  3.91 


# # A tibble: 3 x 15
# plot   estimate .y.   group1 group2    n1    n2 statistic       p    df conf.low conf.high method alternative  p.adj
# <chr>     <dbl> <chr> <chr>  <chr>  <int> <int>     <dbl>   <dbl> <dbl>    <dbl>     <dbl> <chr>  <chr>        <dbl>
#   1 twent…   0.197  mean  UV.fi… UV.tr…     5     5     0.942 0.399       4   -0.383     0.777 T-test two.sided   0.598 
# 2 twent…  -0.593  mean  UV.fi… UV.tr…     5     5    -5.37  0.00580     4   -0.900    -0.287 T-test two.sided   0.0174
# 3 twent…  -0.0291 mean  UV.fi… UV.tr…     5     5    -0.433 0.687       4   -0.215     0.157 T-test two.sided   0.687 

#### anova ####
# one way anova with repeated measures (plot pairs/triplets)
anova.mean.temp.dailylowpermonth.plot <-  mean.temp.dailylowpermonth.plot %>% ungroup() %>%
  rstatix::anova_test(mean ~ treatment, wid = plot)

rstatix::get_anova_table(anova.mean.temp.dailylowpermonth.plot)

# ANOVA Table (type II tests)
# 
# Effect DFn DFd     F     p p<.05      ges
# 1 treatment   1  28 0.004 0.949       0.000147


#### LIGHT ####
#### high ####

# pull out the high TEMP from each day, for each treatment 
alldata %>% 
  mutate(hour = hour(Date.Time)) %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, plot, treatment) %>% 
  dplyr::summarize(high = max(Intensity.Lux), n = n(), sd.Value = sd(Intensity.Lux)) -> light.dailyhigh

head(light.dailyhigh)



# calculate mean daily high per month
light.dailyhigh %>% 
  mutate(month = month(date)) %>% 
  group_by(month, plot, treatment) %>% 
  dplyr::summarize(meandailyhigh = mean(high), n = n(), sd.dailyhigh = sd(high)) -> light.dailyhighpermonth.mean
head(light.dailyhighpermonth.mean)
# 
# calculate mean daily high (mean) per month for all 3 plots
# data for Site.reference is only available for 2019-06-06, 2019-06-07, and 2019 06-08
mean.light.dailyhighpermonth <- light.dailyhigh %>% dplyr::filter(., treatment != "Site.reference") %>%
  mutate(month = month(date)) %>% group_by(month, treatment) %>% dplyr::summarize(mean = mean(high))
mean.light.dailyhighpermonth
# # 
# # A tibble: 10 x 3
# # Groups:   month [5]
# month treatment         mean
# <dbl> <chr>            <dbl>
#   1     2 UV.filtered    135459.
# 2     2 UV.transmitted 143864.
# 3     3 UV.filtered    196890.
# 4     3 UV.transmitted 201394.
# 5     4 UV.filtered    230060.
# 6     4 UV.transmitted 216894.
# 7     5 UV.filtered    236565.
# 8     5 UV.transmitted 211616.
# 9     6 UV.filtered    213181.
# 10     6 UV.transmitted 170376.
# # 
# 
# 
# # paired t.test, per month, mean daily high light per month stats for only 
# res <- t.test(mean ~ treatment, data = mean.light.dailyhighpermonth, paired = TRUE)
# res



# calculate mean daily high (mean) per month for each plot separately
# data for Site.reference is only available for 2019-06-06, 2019-06-07, and 2019 06-08
mean.light.dailyhighpermonth.plot <- light.dailyhigh %>% dplyr::filter(., treatment != "Site.reference") %>%
  mutate(month = month(date)) %>% group_by(month, plot, treatment) %>% dplyr::summarize(mean = mean(high))
mean.light.dailyhighpermonth.plot
# # A tibble: 30 x 4
# # Groups:   month, plot [15]
# month plot        treatment         mean
# <dbl> <chr>       <chr>            <dbl>
#   1     2 twentyone   UV.filtered    116147.
# 2     2 twentyone   UV.transmitted 135273.
# 3     2 twentythree UV.filtered    147974.
# 4     2 twentythree UV.transmitted 143350.
# 5     2 twentytwo   UV.filtered    142256.
# 6     2 twentytwo   UV.transmitted 152969.
# 7     3 twentyone   UV.filtered    178046.
# 8     3 twentyone   UV.transmitted 181423.
# 9     3 twentythree UV.filtered    208535.
# 10     3 twentythree UV.transmitted 212090.
# # … with 20 more rows


# # t test
# pwc.light.dailyhighpermonth.plot <- mean.light.dailyhighpermonth.plot %>% group_by(plot) %>%
#                               rstatix::t_test(mean ~ treatment, paired = T, detailed = T)
# 
# pwc.light.dailyhighpermonth.plot %>% adjust_pvalue(method = "BH") -> pwc.light.dailyhighpermonth.plot

# # A tibble: 3 x 15
# plot   estimate .y.   group1  group2     n1    n2 statistic      p    df conf.low conf.high method alternative p.adj
# <chr>     <dbl> <chr> <chr>   <chr>   <int> <int>     <dbl>  <dbl> <dbl>    <dbl>     <dbl> <chr>  <chr>       <dbl>
#   1 twent…   17243. mean  UV.fil… UV.tra…     5     5     1.09  0.337      4  -26720.    61206. T-test two.sided   0.506
# 2 twent…   19053. mean  UV.fil… UV.tra…     5     5     2.43  0.0721     4   -2730.    40837. T-test two.sided   0.216
# 3 twent…    3725. mean  UV.fil… UV.tra…     5     5     0.541 0.617      4  -15403.    22852. T-test two.sided   0.617

#### anova ####
# one way anova with repeated measures (plot pairs/triplets)
anova.mean.light.dailyhighpermonth.plot <-  mean.light.dailyhighpermonth.plot %>% ungroup() %>%
  rstatix::anova_test(mean ~ treatment, wid = plot)

rstatix::get_anova_table(anova.mean.light.dailyhighpermonth.plot)

# ANOVA Table (type II tests)
# 
# Effect DFn DFd     F     p p<.05   ges
# 1 treatment   1  28 0.954 0.337       0.033


