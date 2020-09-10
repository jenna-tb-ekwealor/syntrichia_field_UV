library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(rstatix)

setwd("/Users/jennaekwealor/Box Sync/dissertation/project-field/sensor data")

data <- na.omit(read.csv(file = "light_quality_raw.csv"))
data$site <- as.factor(data$site)
data$measure <- as.numeric(data$measure)
 
#data <- data %>% ungroup()

#### PAR ####
data.par <- data %>% dplyr::filter(., metric == "PAR") %>% select(site,measure,treatment)


# summary stats
data.par %>%
  group_by(treatment) %>%
  get_summary_stats(measure, type = "mean_sd")

# # A tibble: 3 x 5
# treatment       variable     n  mean    sd
# <fct>           <chr>    <dbl> <dbl> <dbl>
#   1 Site Reference  measure     19 1472.  345.
# 2 UV Filtering    measure     19 1324.  348.
# 3 UV Transmitting measure     19 1343.  340.

# box plot
bxp <- ggboxplot(data.par, x = "treatment", y = "measure", add = "point")
bxp

# identify outliers
data.par %>%
  group_by(treatment) %>%
  identify_outliers(measure)


# normality assumption
# shapiro_test has a bug in the script that the measure you wanna test cannot be named "measure":
data.par %>%
  group_by(treatment) %>%
  rstatix::shapiro_test(measure)

# anova
res.aov.par <- rstatix::anova_test(
  data = data.par, dv = measure, wid = site,
  within = treatment
)
get_anova_table(res.aov.par)
# PAR was statistically significantly different at the different treatments F(2, 36) = 11.5, p < 0.001, eta2[g] = 0.37.
# where F Indicates that we are comparing to an F-distribution (F-test); 
# (2, 18) indicates the degrees of freedom in the numerator (DFn) and the denominator (DFd), respectively; 
# 55.5 indicates the obtained F-statistic value
# p specifies the p-value
# ges is the generalized effect size (amount of variability due to the within-subjects factor)



# post-hoc
# pairwise comparisons
pwc.par <- data.par %>%
  rstatix::pairwise_t_test(
    measure ~ treatment, paired = TRUE,
    p.adjust.method = "BH"
  )
pwc.par

# # A tibble: 3 x 10
# .y.     group1         group2             n1    n2 statistic    df         p     p.adj p.adj.signif
# * <chr>   <chr>          <chr>           <int> <int>     <dbl> <dbl>     <dbl>     <dbl> <chr>       
#   1 measure Site Reference UV Filtering       19    19     6.21     18 0.0000073 0.0000219 ****        
#   2 measure Site Reference UV Transmitting    19    19     3.28     18 0.004     0.006     **          
#   3 measure UV Filtering   UV Transmitting    19    19    -0.553    18 0.587     0.587     ns    




#### UV ####
data.UV <- data %>% dplyr::filter(., metric == "UV") %>% select(site,measure,treatment)


# summary stats
data.UV %>%
  group_by(treatment) %>%
  get_summary_stats(measure, type = "mean_sd")
# 
# # A tibble: 3 x 5
# treatment       variable     n   mean     sd
# <fct>           <chr>    <dbl>  <dbl>  <dbl>
#   1 Site Reference  measure     19 105.   34.5  
# 2 UV Filtering    measure     19   1.65  0.493
# 3 UV Transmitting measure     19  91.3  25.5  

# box plot
bxp <- ggboxplot(data.UV, x = "treatment", y = "measure", add = "point")
bxp

# identify outliers
data.UV %>%
  group_by(treatment) %>%
  identify_outliers(measure)


# normality assumption
# shapiro_test has a bug in the script that the measure you wanna test cannot be named "measure":
data.UV %>%
  group_by(treatment) %>%
  rstatix::shapiro_test(measure)
# UV filtering not normally distributed

# anova
res.aov.UV <- rstatix::anova_test(
  data = data.UV, dv = measure, wid = site,
  within = treatment
)
get_anova_table(res.aov.UV)
# UV was statistically significantly different at the different treatments F(1.23, 22.06) = 177.6, p < 0.0001, eta2[g] = 0.78.
# where F Indicates that we are comUVing to an F-distribution (F-test); 
# (1.23, 22.06) indicates the degrees of freedom in the numerator (DFn) and the denominator (DFd), respectively; 
# 177.6 indicates the obtained F-statistic value
# p specifies the p-value
# ges is the generalized effect size (amount of variability due to the within-subjects factor)



# post-hoc
# pairwise comparisons
pwc.UV <- data.UV %>%
  rstatix::pairwise_wilcox_test(
    measure ~ treatment, paired = TRUE,
    p.adjust.method = "BH"
  )
pwc.UV

# # A tibble: 3 x 9
# .y.     group1         group2             n1    n2 statistic          p      p.adj p.adj.signif
# * <chr>   <chr>          <chr>           <int> <int>     <dbl>      <dbl>      <dbl> <chr>       
#   1 measure Site Reference UV Filtering       19    19       190 0.00000381 0.00000572 ****        
#   2 measure Site Reference UV Transmitting    19    19       172 0.000965   0.000965   ***         
#   3 measure UV Filtering   UV Transmitting    19    19         0 0.00381 0.00000572 ****  

# does this mean degrees of freedom is 1



#   