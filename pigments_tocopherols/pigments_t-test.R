library(tidyverse)
library(ggbiplot)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(rstatix)

# for colors choose from colFun palette
source('colFun.R')

UV.filtered<-"#14d2dc" #turquoise
UV.transmitted<-"#fa7850" #orange
Site.reference<-"#8214a0" #purple
Lab.control<-"#a0fa82" #green

setwd("/Users/jennaekwealor/Documents/dissertation_repositories/syntrichia_field_UV/pigments_tocopherols")

# load data
norm <- read.csv("final-data.csv")

# split datasheet by sample & summary stats
samples <- filter(norm, Data.Type == "sample")
means <- filter(norm, Data.Type == "mean")
stddevs <- filter(norm, Data.Type == "dev")
summarystats <- rbind(means,stddevs)

# set levels to treatments
samples$Treatment <- factor(samples$Treatment,levels = c("UV Filtered", "UV Transmitted", "Site Reference", "Lab Control"))
samples$Site <- factor(samples$Site)

# remove any erroneous NA
data.slim <- na.omit(samples)

pigments <- colnames(data.slim[, -c(1:4)]) # drop sample columms

# list of only the pigments & antioxidants i'll be looking at
pigments.used <- c("violaxanthin","antheraxanthin","zeaxanthin","neoxanthin","lutein","chlorophyll.a","chlorophyll.b","beta.carotene.sum","alpha.tocopherol","beta.tocopherol")


# check for outliers

# An observation with Cook’s distance larger than three times the mean Cook’s distance might be an outlier
# http://r-statistics.co/Outlier-Treatment-With-R.html

# set up loop
outliers <- c()
data.loop <- data.frame()
pig <- NULL
trtmnt.type <- unique(data.slim$Treatment)

# loop through to find outliers per pigment/antioxidant per treatment   
for (trtmnt in trtmnt.type) {
  data.slim %>% group_by(Treatment) %>%
  filter(Treatment == trtmnt) -> data.trtmnt
  data.trtmnt %>% group_by(Site)

  
for (pig in pigments.used) {
  data.loop <- as.data.frame(cbind(data.trtmnt$Site, data.trtmnt$Treatment, data.trtmnt$Sample, data.trtmnt[pig]))
  names(data.loop)[names(data.loop) == "data.trtmnt$Site"] <- "Site" # creates a dataframe just for that loop iteration, one pigment and 3 sample variables 
  names(data.loop)[names(data.loop) == "data.trtmnt$Sample"] <- "Sample" # names the headers oddly so fixing it
  names(data.loop)[names(data.loop) == "data.trtmnt$Treatment"] <- "Treatment" # names the headers oddly so fixing it
  formula <- as.formula(paste(pig," ~ Site",sep = ""))
  mod <- lm(formula, data=data.loop)
  cooksd <- cooks.distance(mod)
  # plot 
  # plot(cooksd, pch="*", cex=2, main=paste("Influential Obs by Cooks distance: ",pig,sep = ""))  # plot cook's distance
  # abline(h = 35*mean(cooksd, na.rm=T), col="red")  # add cutoff line
  # text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>8*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
  n <- length(data.loop$Sample)
  influential <- as.numeric(names(cooksd)[(cooksd > (8)*mean(cooksd, na.rm=T))])  # influential row numbers
  y <- data.trtmnt[influential,1]
  outliers[[trtmnt]][[pig]] <- y
}}

##

# remove whole sample for outlier in a single pig/antioxidant
outliers_unique <- sort(unique(unlist(outliers)))

# count how many outliers
length(outliers_unique)
# [1] 17
data_no_outliers <- data.slim[!data.slim$Sample %in% outliers_unique, ]
length (data.slim$Sample)
# [1] 188
length(data_no_outliers$Sample)
# [1] 171

#### pca ####

# clean up data
wide_data <- data_no_outliers[,colSums(is.na(data_no_outliers))<nrow(data_no_outliers)]
wide_data <- na.omit(wide_data)

# select columns for pca
wide_data.only.want <- wide_data %>% select(Treatment, neoxanthin, violaxanthin, antheraxanthin, lutein, zeaxanthin, chlorophyll.a, chlorophyll.b, beta.carotene.sum, total.tocopherols)

# pca analysis
wide_data.only.want.pca <- prcomp(wide_data.only.want[,c(2:10)], center = TRUE,scale. = TRUE)
summary(wide_data.only.want.pca)

wide_data.only.want.pca

# plot pca
ggbiplot(wide_data.only.want.pca,ellipse=TRUE,ellipse.prob = 0.68, choices=c(1,2), obs.scale = 1, var.scale = 1,   groups=wide_data$Treatment, varname.size = 3,var.axes = TRUE) +
  scale_color_manual(values = c("UV Filtered" = UV.filtered, "UV Transmitted" = UV.transmitted, "Site Reference" = Site.reference, "Lab Control" = Lab.control)) +
  theme_minimal() + 
  labs(color="Treatment")


#### field pigments ####
#add chlorophyll a:b ratio column
data.slim$chl.ab.ratio <- data.slim$chlorophyll.a/data.slim$chlorophyll.b

# convert to long format
data_long <- gather(data.slim, Pigment, Amount, neoxanthin:chl.ab.ratio, factor_key=TRUE)

# remove unnecessary columns ###
# data_long <- data_long[, -c(1,3,4)] # don't run this one extra times 

# summary stats: mean, n, and std dev  
summary <- data_long %>%
  group_by(Pigment, Treatment, Site)  %>%
  dplyr::summarise(mean = mean(Amount), n = n(), sd = sd(Amount))

# write to csv
write_csv(summary, path = "pigments_summary_nooutliers.csv")

# remove site reference
data.nositeref <- filter(data_long, Treatment != "Site Reference")

# remove lab control
data.nolabcontrol <- filter(data_long, Treatment != "Lab Control")

# remove site reference and lab control
data.nositeref.nolabcontrol <- filter(data.nositeref, Treatment != "Lab Control")


# pigments only, no tocopherols
data.nolabcontrol.pigments <- filter(data.nolabcontrol, Pigment %in% c("neoxanthin", "violaxanthin", "antheraxanthin", "lutein", "zeaxanthin", "chlorophyll.a", "chlorophyll.b", "beta.carotene.sum", "V.A.Z.", "A.Z.V.A.Z", "total.bb", "total.be", "bb.be", "total.carotenoids", "chl.ab.ratio")) 


# do i need this?
# mean per treatment, per plot (mean of technical replicates)
# summary.nolabcontrol.pigments <- data.nolabcontrol.pigments %>%
#   group_by(Pigment, Treatment, Site)  %>%
#   dplyr::summarise(mean = mean(Amount), n = n(), sd = sd(Amount))
# 
# summary.nolabcontrol.pigments

# mean per treatment, per plot (mean of technical replicates) for pigments of interest
data.nolabcontrol.pigments.some <- filter(data.nolabcontrol, Pigment %in% c("violaxanthin", "antheraxanthin", "zeaxanthin", "neoxanthin","lutein", "chlorophyll.a", "chlorophyll.b", "beta.carotene.sum", "chl.ab.ratio"))
data.nolabcontrol.pigments.some$Pigment <- factor(data.nolabcontrol.pigments.some$Pigment,levels = c("violaxanthin", "antheraxanthin", "zeaxanthin", "neoxanthin","lutein", "beta.carotene.sum", "chlorophyll.a", "chlorophyll.b","chl.ab.ratio"))

summary.nolabcontrol.pigments.some <- na.omit(data.nolabcontrol.pigments.some) %>%
  group_by(Pigment, Treatment, Site)  %>%
  dplyr::summarise(mean = mean(Amount), n = n(), sd = sd(Amount))
summary.nolabcontrol.pigments.some$Pigment <- factor(summary.nolabcontrol.pigments.some$Pigment,levels = c("violaxanthin", "antheraxanthin", "zeaxanthin", "neoxanthin","lutein", "beta.carotene.sum", "chlorophyll.a", "chlorophyll.b","chl.ab.ratio"))


# do pairwise wilcox on MEAN of technical replicates, paired by site
summary.nolabcontrol.pigments.some %>% group_by(Pigment) %>%
  rstatix::pairwise_wilcox_test(data = ., mean ~ Treatment, paired = TRUE, p.adjust.method = "BH") -> stat.test.pigments

# add significance/pvalue position on plot 
stat.test.pigments <- stat.test.pigments %>% mutate(y.position = c(1.64, 1.84, 1.74, 
                                                                   2.15, 2.45, 2.3,
                                                                   10.5, 11.7, 11.1, 
                                                                   5.6, 6.4, 6, 
                                                                   25.5, 28.7, 27.1,
                                                                   20, 22.5, 21.25,
                                                                   50.25, 57.75, 54,
                                                                   20.7, 23.5, 22.1,
                                                                   5.4,6.2,5.8))
stat.test.pigments


# calculate the mean of all sites per treatment for plotting
summary.summary.nolabcontrol.pigments.some <- summary.nolabcontrol.pigments.some %>% 
  group_by(Pigment, Treatment) %>% 
  dplyr::summarise(sd = sd(mean), mean = mean(mean), n = n())

# make a paired box plot of pigments and field experiment
box.box.pigments.some<-ggboxplot(summary.nolabcontrol.pigments.some, x = "Treatment", y = "mean",
                                fill = "Treatment",
                                color = "gray30",
                                size = 0.5,
                                point.size = 1,
                                outlier.size = 0.75,
                                id = "Site",
                                line.color = "#4D4D4D66", 
                                facet.by = c("Pigment"),
                                scales = "free_y",
                           panel.labs = list(Pigment=c("Violaxanthin", "Antheraxanthin", "Zeaxanthin", "Neoxanthin","Lutein", "b-carotene", "Chlorophyll a", "Chlorophyll b", "Chl a : Chl b")), 
                           panel.labs.background = list(color = "black", fill = "black"),
                           panel.labs.font = list(size=14),
                           width = 0.7,
                           xlab = "",
                           ylab = "Normalized quantity (%)",
                           ggtheme = theme_light()) +
                          stat_pvalue_manual(stat.test.pigments, label = "p.adj.signif") 

box.box.pigments.some<-ggpar(box.box.pigments.some,
                             legend = "right",
                             legend.title = "Field Treatment",
                             font.legend = c(16, "plain"),
                             font.ytickslab =  c(16, "plain"),
                             font.y = c(18,"plain"),
                             font.x = c(18,"bold"),
                             font.title = c(20, "bold"),
                             font.xtickslab =  c(0, "plain")) +
expand_limits(x = 0, y = 0) +
  scale_fill_manual(values=c(UV.filtered, UV.transmitted, Site.reference)) 


box.box.pigments.some



#





#### field tocopherols ####

# pull out tocopherols 
data.nolabcontrol.tocopherols <- filter(data.nolabcontrol, Pigment %in% c("beta.tocopherol", "alpha.tocopherol")) 
data.nolabcontrol.tocopherols$Pigment <- factor(data.nolabcontrol.tocopherols$Pigment,levels = c("alpha.tocopherol", "beta.tocopherol"))

# caulate mean of technical replicates
summary.nolabcontrol.tocopherols <- data.nolabcontrol.tocopherols %>%
  group_by(Pigment, Treatment, Site)  %>%
  dplyr::summarise(mean = mean(Amount), n = n(), sd = sd(Amount))
summary.nolabcontrol.tocopherols$Pigment <- factor(summary.nolabcontrol.tocopherols$Pigment,levels = c("alpha.tocopherol", "beta.tocopherol"))

# t test, paired by site
summary.nolabcontrol.tocopherols %>% group_by(Pigment) %>%
  rstatix::pairwise_wilcox_test(data = ., mean ~ Treatment, paired = TRUE, p.adjust.method = "BH") -> stat.test.tocopherols

stat.test.tocopherols

# # A tibble: 6 x 10
# Pigment          .y.   group1         group2            n1    n2 statistic          p     p.adj p.adj.signif
# * <fct>            <chr> <chr>          <chr>          <int> <int>     <dbl>      <dbl>     <dbl> <chr>       
#   1 alpha.tocopherol mean  UV Filtered    UV Transmitted    19    19       151 0.023      0.023     *           
#   2 alpha.tocopherol mean  UV Filtered    Site Reference    19    19        20 0.001      0.002     **          
#   3 alpha.tocopherol mean  UV Transmitted Site Reference    19    19         0 0.00000381 0.0000114 ****        
#   4 beta.tocopherol  mean  UV Filtered    UV Transmitted    19    19       174 0.000645   0.000967  ***         
#   5 beta.tocopherol  mean  UV Filtered    Site Reference    19    19        80 0.568      0.568     ns          
# 6 beta.tocopherol  mean  UV Transmitted Site Reference    19    19        10 0.000164   0.000492  ***    

# mean of each treatment across sites for plotting
summary.summary.nolabcontrol.tocopherols <- summary.nolabcontrol.tocopherols %>% group_by(Pigment, Treatment) %>% dplyr::summarise(sd = sd(mean), mean = mean(mean), n = n())
summary.summary.nolabcontrol.tocopherols

# # A tibble: 6 x 5
# # Groups:   Pigment [2]
# Pigment          Treatment         sd  mean     n
# <fct>            <fct>          <dbl> <dbl> <int>
#   1 alpha.tocopherol UV Filtered     3.59 16.7     19
# 2 alpha.tocopherol UV Transmitted  3.43 14.7     19
# 3 alpha.tocopherol Site Reference  3.05 20.6     19
# 4 beta.tocopherol  UV Filtered     1.58  5.69    19
# 5 beta.tocopherol  UV Transmitted  1.54  4.17    19
# 6 beta.tocopherol  Site Reference  1.36  5.98    19


# add y-value position to stat.test tibble for plotting
 stat.test.tocopherols <- stat.test.tocopherols %>% mutate(y.position = c(27.5,30.5,29,
                                                                          9.1,10.1,9.6))

# plot
box.box.tocopherols<-ggboxplot(summary.nolabcontrol.tocopherols, x = "Treatment", y = "mean",
                              fill = "Treatment",
                              color = "gray30",
                              size = 0.5,
                              point.size = 0.75,
                              outlier.size = 0.25,
                              id = "Site",
                              line.color = "#4D4D4D66", 
                              facet.by = c("Pigment"),
                              scales = "free_y",
                                     panel.labs = list(Pigment=c("a-tocopherol", "b-tocopherol")), 
                                     panel.labs.background = list(color = "black", fill = "black"),
                                     panel.labs.font = list(size=14),
                                     width = 0.7,
                                     xlab = "",
                                     ylab = "Normalized quantity (mmol/mol)",
                                     ggtheme = theme_light()) +
                                  #   title = "Tocopherols decrease with UV radiation\n") +
                                stat_pvalue_manual(stat.test.tocopherols, label = "p.adj.signif") 

box.box.tocopherols<-ggpar(box.box.tocopherols,
                        legend = "right",
                        legend.title = "Field Treatment",
                        font.legend = c(16, "plain"),
                        font.ytickslab =  c(16, "plain"),
                        font.y = c(18,"plain"),
                        font.x = c(18,"bold"),
                        font.title = c(20, "bold"),
                        font.xtickslab =  c(0, "plain"))+
  expand_limits(x = 0, y = 0) +
  scale_fill_manual(values=c(UV.filtered, UV.transmitted, Site.reference)) 

box.box.tocopherols 




#




#### some extra plots ####

# stacked bar chart of FIELD REFERENCE vs LAB CONTROL, one for PIGMENTS and one for TOCOPHEROLS
# filter data for use
data_sitereference_labcontrol <- filter(data_long, Treatment %in% c("Site Reference", "Lab Control")) 
data_sitereference_labcontrol_pigments <- filter(data_sitereference_labcontrol, Pigment %in% c("violaxanthin", "antheraxanthin", "zeaxanthin","neoxanthin","beta.carotene.sum", "lutein","chlorophyll.a", "chlorophyll.b")) 

# calculate mean of technical replicates
summary_sitereference_labcontrol_pigments <- data_sitereference_labcontrol_pigments %>%
  group_by(Pigment, Treatment, Site)  %>%
  dplyr::summarise(mean = mean(Amount), n = n(), sd = sd(Amount))
summary_sitereference_labcontrol_pigments


# paired t test of pigments across window field treatment
data_sitereference_labcontrol_pigments %>%
  group_by(Pigment) %>%
rstatix::pairwise_wilcox_test(data = ., Amount ~ Treatment, paired = F, p.adjust.method = "BH") -> stat.test.lab.pigments
stat.test.lab.pigments

# # A tibble: 8 x 10
# Pigment           .y.    group1         group2         n1    n2 statistic          p      p.adj p.adj.signif
# * <fct>             <chr>  <chr>          <chr>       <int> <int>     <dbl>      <dbl>      <dbl> <chr>       
#   1 neoxanthin        Amount Site Reference Lab Control    61     9        77 0.000547   0.000547   ***         
#   2 violaxanthin      Amount Site Reference Lab Control    61     9         0 0.00000153 0.00000153 ****        
#   3 antheraxanthin    Amount Site Reference Lab Control    61     9       275 1          1          ns          
# 4 lutein            Amount Site Reference Lab Control    61     9       516 0.0000235  0.0000235  ****        
#   5 zeaxanthin        Amount Site Reference Lab Control    61     9       549 0.00000153 0.00000153 ****        
#   6 chlorophyll.b     Amount Site Reference Lab Control    61     9         0 0.00000153 0.00000153 ****        
#   7 chlorophyll.a     Amount Site Reference Lab Control    61     9       145 0.024      0.024      *           
#   8 beta.carotene.sum Amount Site Reference Lab Control    61     9       448 0.002      0.002      **   
# 

# order levels of pigments so they plot in order
summary_sitereference_labcontrol_pigments$Pigment <- factor(summary_sitereference_labcontrol_pigments$Pigment,levels = c("violaxanthin","antheraxanthin","zeaxanthin","neoxanthin","lutein","beta.carotene.sum","chlorophyll.a","chlorophyll.b"))



# Grouped stacked bar chart
my_title <- expression(paste("Photosynthetic Pigments and Antioxidants in Field and Lab Grown ", italic("Syntrichia caninervis")))

barplot <- ggplot(summary_sitereference_labcontrol_pigments, aes(fill=Pigment, y=mean, x=Treatment)) + 
  geom_bar(position="stack", stat="identity", colour="white") +
  #ggtitle("Photosynthetic Pigments and Antioxidants in Field and Lab Grown Plants") +
  theme_minimal() +
  xlab("") +
  ylab("Normalized quantity (%)") +
  scale_fill_manual(values = c("violaxanthin" = viola, "antheraxanthin" = anthera,"zeaxanthin" = zea,"neoxanthin" = neo, "lutein" = lutein, "beta.carotene.sum" = betac, "chlorophyll.a" = chla, "chlorophyll.b" = chlb), labels = c("Violaxanthin", "Antheraxanthin", "Zeaxanthin", "Neoxanthin", "Lutein", "b-carotene", "Chlorophyll a", "Chlorophyll b"))

barplot + theme(
  plot.title = element_text(size = 20, face = "bold"),
  axis.text.x = element_text(size = 18, face = "bold"),
  axis.text.y = element_text(size = 12),
  axis.title.y = element_text(size = 18),
  legend.text = element_text(size = 16),
  legend.title = element_text(size = 18)
)

# select site reference and lab control data
data_sitereference_labcontrol <- filter(data_long, Treatment %in% c("Site Reference", "Lab Control")) 
data_sitereference_labcontrol_tocopherols <- filter(data_sitereference_labcontrol, Pigment %in% c("alpha.tocopherol", "beta.tocopherol")) 

# calculate mean site reference and lab control technical replicates
summary_sitereference_labcontrol_tocopherols <- data_sitereference_labcontrol_tocopherols %>%
  group_by(Pigment, Treatment)  %>%
  dplyr::summarise(mean = mean(Amount), n = n(), sd = sd(Amount))

summary_sitereference_labcontrol_tocopherols
# # A tibble: 4 x 5
# # Groups:   Pigment [2]
# Pigment          Treatment       mean     n    sd
# <fct>            <fct>          <dbl> <int> <dbl>
#   1 beta.tocopherol  Site Reference  5.95    61 2.21 
# 2 beta.tocopherol  Lab Control     1.28     9 0.764
# 3 alpha.tocopherol Site Reference 20.6     61 6.27 
# 4 alpha.tocopherol Lab Control     9.56     9 3.08

data_sitereference_labcontrol_tocopherols %>%
  group_by(Pigment)  %>%
  rstatix::pairwise_wilcox_test(data = ., Amount ~ Treatment, paired = FALSE, p.adjust.method = "BH") -> stat.test.lab.tocopherols

stat.test.lab.tocopherols

# # A tibble: 2 x 10
# Pigment          .y.    group1         group2         n1    n2 statistic          p      p.adj p.adj.signif
# * <fct>            <chr>  <chr>          <chr>       <int> <int>     <dbl>      <dbl>      <dbl> <chr>       
#   1 beta.tocopherol  Amount Site Reference Lab Control    61     9       548 0.00000167 0.00000167 ****        
#   2 alpha.tocopherol Amount Site Reference Lab Control    61     9       528 0.00000903 0.00000903 ****        
  









#### extra plots for talks ####

# # order levels of pigments so they plot in order
# summary_sitereference_labcontrol_tocopherols$Pigment <- factor(summary_sitereference_labcontrol_tocopherols$Pigment,levels = c("alpha.tocopherol","beta.tocopherol"))
# 
# # Grouped stacked bar chart
# 
# my_title <- expression(paste("Antioxidants in Field and Lab Grown ", italic("Syntrichia caninervis")))
# 
# barplot <- ggplot(summary_sitereference_labcontrol_tocopherols, aes(fill=Pigment, y=mean, x=Treatment)) + 
#   geom_bar(position="stack", stat="identity", colour="white") +
#  # ggtitle("Antioxidants in Field and Lab Grown Plants") +
#   theme_minimal() +
#   xlab("") +
#   ylab("Normalized quantity (%)") +
#   scale_fill_manual(values = c("alpha.tocopherol" = alphat, "beta.tocopherol" = betat), labels = c("a-tocopherol", "b-tocopherol"))
# 
# # position = fill for percentage, stack for reg stack 
# 
# barplot + theme(
#   plot.title = element_text(size = 20, face = "bold"),
#   axis.text.x = element_text(size = 18, face = "bold"),
#   axis.text.y = element_text(size = 12),
#   axis.title.y = element_text(size = 18),
#   legend.text = element_text(size = 16),
#   legend.title = element_text(size = 18)
# ) + labs(fill = "Antioxidant")
# 

