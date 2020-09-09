library(ggpubr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(splitstackshape)
library(data.table)
library(ggfortify)
library(broom)
library(knitr)
library(tidyverse)
library(ggbiplot)
library(scales)

# display.brewer.all(colorblindFriendly = TRUE)

# display.brewer.pal(n = 8, name = 'Set2')
# brewer.pal(n = 8, name = "Set2")
#"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494" "#B3B3B3"
# teal      orange    purple    pink      green     yellow    tan       gray

UV.filtered<-"#66C2A5" #teal
UV.transmitted<-"#FC8D62" #orange
Site.reference<-"#8DA0CB" #purple
Lab.control<-"#A6D854" #green

display.brewer.pal(n = 11, name = 'BrBG')
brewer.pal(n = 11, name = "PiYG")

viola<- "#2a1802"
anthera<- "#543005"
zea<- "#8C510A"
neo<- "#BF812D"
lutein<- "#e5b960"
betac<-"#f0d285"
chla<- "#f5e0ab"
chlb<- "#f7e8c2"
"#F6E8C3"
alphat <- "#7FBC41"
betat <- "#E6F5D0"

show_col(viola)

setwd("/Users/jennaekwealor/Box Sync/dissertation/project-field")

# load data
norm <- read.csv("pigments/final-totalnorm.csv")

samples <- filter(norm, Data.Type == "sample")
means <- filter(norm, Data.Type == "mean")
stddevs <- filter(norm, Data.Type == "dev")
summarystats <- rbind(means,stddevs)


  
samples$Treatment <- factor(samples$Treatment,levels = c("UV Filtered", "UV Transmitted", "Site Reference", "Lab Control"))
samples$Site <- factor(samples$Site)

data <- na.omit(samples)


# check for outliers

# remove extra sample name columns
# data.slim <- na.omit(data[c(1,2,5,6:21)])
data.slim <- data # not sure if i even need the slimming anymore
#data.slim$sample_cat <- paste(data.slim$Site, data.slim$Treatment) # combine treatment columns
#data.slim <- as.data.frame(data.slim[c(3:17)])


# An observation with Cook’s distance larger than three times the mean Cook’s distance might be an outlier
# http://r-statistics.co/Outlier-Treatment-With-R.html

pigments <- colnames(data.slim[, -c(1:4)]) # drop sample columms
pigments.used <- c("violaxanthin","antheraxanthin","zeaxanthin","neoxanthin","lutein","chlorophyll.a","chlorophyll.b","beta.carotene.sum","alpha.tocopherol","beta.tocopherol")




outliers <- c()
data.loop <- data.frame()

pig <- NULL

trtmnt.type <- unique(data.slim$Treatment)
  
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
  #plot(cooksd, pch="*", cex=2, main=paste("Influential Obs by Cooks distance: ",pig,sep = ""))  # plot cook's distance
  #abline(h = 35*mean(cooksd, na.rm=T), col="red")  # add cutoff line
  #text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>35*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
  n <- length(data.loop$Sample)
  influential <- as.numeric(names(cooksd)[(cooksd > (8)*mean(cooksd, na.rm=T))])  # influential row numbers
  y <- data.trtmnt[influential,1]
  outliers[[trtmnt]][[pig]] <- y
}}

##

# to remove whole sample for one outlier
outliers_unique <- sort(unique(unlist(outliers)))

length(outliers_unique)
# [1] 15
data_no_outliers <- data[!data$Sample %in% outliers_unique, ]
length (data$Sample)
# [1] 186
length(data_no_outliers$Sample)
# [1] 171
# ###################################
#### pca ####
# remove columns of all na (until we get tocopherol data)


data <- data_no_outliers 

wide_data <- data[,colSums(is.na(data))<nrow(data)]
wide_data <- na.omit(wide_data)

wide_data.only.want <- wide_data %>% select(Treatment, neoxanthin, violaxanthin, antheraxanthin, lutein, zeaxanthin, chlorophyll.a, chlorophyll.b, beta.carotene.sum, total.tocopherols)
  


wide_data.only.want.pca <- prcomp(wide_data.only.want[,c(2:10)], center = TRUE,scale. = TRUE)
summary(wide_data.only.want.pca)

wide_data.only.want.pca

ggbiplot(wide_data.only.want.pca,ellipse=TRUE,ellipse.prob = 0.68, choices=c(1,2), obs.scale = 1, var.scale = 1,   groups=wide_data$Treatment, varname.size = 3,var.axes = TRUE) +
  scale_color_manual(values = c("UV Filtered" = UV.filtered, "UV Transmitted" = UV.transmitted, "Site Reference" = Site.reference, "Lab Control" = Lab.control)) +
  theme_minimal() + 
  labs(color="Treatment")
# 
# 
# 
# 
# # also want do do a plot of just treatment groups?
# 
# wide_data.nositeref <- filter(wide_data, Treatment != "Site Reference")
# 
# wide_data.nositeref.pca <- prcomp(wide_data.nositeref[,c(6:19)], center = TRUE,scale. = TRUE)
# summary(wide_data.nositeref.pca)
# 
# ggbiplot(wide_data.nositeref.pca,ellipse=TRUE,choices=c(1,2), obs.scale = 1, var.scale = 1,   groups=wide_data.nositeref$Treatment) +
#   scale_color_manual(values = c("UV Filtered" = UV.filtered, "UV Transmitted" = UV.transmitted)) +
#    theme_minimal()
# 
# ####################################
data <- data
#add chlorophyll a:b ratio column
data$chl.ab.ratio <- data$chlorophyll.a/data$chlorophyll.b

data_long <- gather(data, Pigment, Amount, neoxanthin:chl.ab.ratio, factor_key=TRUE)

# remove unnecessary columns ###

# data_long <- data_long[, -c(1,3,4)] # don't run this one extra times 
  
summary <- data_long %>%
  group_by(Pigment, Treatment, Site)  %>%
  dplyr::summarise(mean = mean(Amount), n = n(), sd = sd(Amount))

#class(summary)
write_csv(summary, path = "pigments_summary_nooutliers.csv")


data.nositeref <- filter(data_long, Treatment != "Site Reference")

summary.nositeref <- filter(summary, Treatment != "Site Reference")

data.nolabcontrol <- filter(data_long, Treatment != "Lab Control")


data.nositeref.nolabcontrol <- filter(data.nositeref, Treatment != "Lab Control")


# pigments only


data.nolabcontrol.pigments <- filter(data.nolabcontrol, Pigment %in% c("neoxanthin", "violaxanthin", "antheraxanthin", "lutein", "zeaxanthin", "chlorophyll.a", "chlorophyll.b", "beta.carotene.sum", "V.A.Z.", "A.Z.V.A.Z", "total.bb", "total.be", "bb.be", "total.carotenoids", "chl.ab.ratio")) 


summary.nolabcontrol.pigments <- data.nolabcontrol.pigments %>%
  group_by(Pigment, Treatment, Site)  %>%
  dplyr::summarise(mean = mean(Amount), n = n(), sd = sd(Amount))

summary.nolabcontrol.pigments

# make a paired box plot 

# SOME pigments only, figure 3?

data.nolabcontrol.pigments.some <- filter(data.nolabcontrol, Pigment %in% c("violaxanthin", "antheraxanthin", "zeaxanthin", "neoxanthin","lutein", "chlorophyll.a", "chlorophyll.b", "beta.carotene.sum", "chl.ab.ratio"))

data.nolabcontrol.pigments.some$Pigment <- factor(data.nolabcontrol.pigments.some$Pigment,levels = c("violaxanthin", "antheraxanthin", "zeaxanthin", "neoxanthin","lutein", "beta.carotene.sum", "chlorophyll.a", "chlorophyll.b","chl.ab.ratio"))


summary.nolabcontrol.pigments.some <- na.omit(data.nolabcontrol.pigments.some) %>%
  group_by(Pigment, Treatment, Site)  %>%
  dplyr::summarise(mean = mean(Amount), n = n(), sd = sd(Amount))
summary.nolabcontrol.pigments.some$Pigment <- factor(summary.nolabcontrol.pigments.some$Pigment,levels = c("violaxanthin", "antheraxanthin", "zeaxanthin", "neoxanthin","lutein", "beta.carotene.sum", "chlorophyll.a", "chlorophyll.b","chl.ab.ratio"))


# do pairwise wilcox on MEAN of technical replicates, paired by site

summary.nolabcontrol.pigments.some %>% group_by(Pigment) %>%
  
  rstatix::pairwise_wilcox_test(data = ., mean ~ Treatment, paired = TRUE, p.adjust.method = "BH") -> stat.test.pigments


stat.test.pigments <- stat.test.pigments %>% mutate(y.position = c(1.43, 1.64, 1.54, 
                                                                   1.85, 2.0, 1.925,
                                                                   9.4, 10.3, 9.85, 
                                                                   4.6, 4.9, 4.75, 
                                                                   23, 24.5, 23.75,
                                                                   17, 19, 18,
                                                                   46.4, 48, 47.2,
                                                                   29.7, 32.5, 31.1,
                                                                   3.1,3.4,3.25))


stat.test.pigments

summary.summary.nolabcontrol.pigments.some <- summary.nolabcontrol.pigments.some %>% group_by(Pigment, Treatment) %>% dplyr::summarise(sd = sd(mean), mean = mean(mean), n = n())





box.box.pigments.some<-ggboxplot(summary.nolabcontrol.pigments.some, x = "Treatment", y = "mean",
                                fill = "Treatment",
                                color = "gray30",
                                size = 0.5,
                                point.size = 1,
                                outlier.size = 0.75,
                                palette = "Set2",
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
expand_limits(x = 0, y = 0)


box.box.pigments.some













# tocopherols only 

data.nolabcontrol.tocopherols <- filter(data.nolabcontrol, Pigment %in% c("beta.tocopherol", "alpha.tocopherol")) 


data.nolabcontrol.tocopherols$Pigment <- factor(data.nolabcontrol.tocopherols$Pigment,levels = c("alpha.tocopherol", "beta.tocopherol"))




summary.nolabcontrol.tocopherols <- data.nolabcontrol.tocopherols %>%
  group_by(Pigment, Treatment, Site)  %>%
  dplyr::summarise(mean = mean(Amount), n = n(), sd = sd(Amount))

summary.nolabcontrol.tocopherols$Pigment <- factor(summary.nolabcontrol.tocopherols$Pigment,levels = c("alpha.tocopherol", "beta.tocopherol"))

summary.nolabcontrol.tocopherols %>% group_by(Pigment) %>%
  
  rstatix::pairwise_wilcox_test(data = ., mean ~ Treatment, paired = TRUE, p.adjust.method = "BH") -> stat.test.tocopherols

stat.test.tocopherols

# # A tibble: 6 x 10
# Pigment          .y.   group1         group2            n1    n2 statistic         p     p.adj p.adj.signif
# * <fct>            <chr> <chr>          <chr>          <int> <int>     <dbl>     <dbl>     <dbl> <chr>       
#   1 alpha.tocopherol mean  UV Filtered    UV Transmitted    19    19       150 0.026     0.026     *           
#   2 alpha.tocopherol mean  UV Filtered    Site Reference    19    19        16 0.000645  0.000967  ***         
#   3 alpha.tocopherol mean  UV Transmitted Site Reference    19    19         2 0.0000114 0.0000342 ****        
#   4 beta.tocopherol  mean  UV Filtered    UV Transmitted    19    19       170 0.001     0.002     **          
#   5 beta.tocopherol  mean  UV Filtered    Site Reference    19    19        56 0.123     0.123     ns          
# 6 beta.tocopherol  mean  UV Transmitted Site Reference    19    19         3 0.0000191 0.0000573 ****  


summary.summary.nolabcontrol.tocopherols <- summary.nolabcontrol.tocopherols %>% group_by(Pigment, Treatment) %>% dplyr::summarise(sd = sd(mean), mean = mean(mean), n = n())


summary.summary.nolabcontrol.tocopherols

# # A tibble: 6 x 5
# # Groups:   Pigment [2]
# Pigment          Treatment         sd  mean     n
# <fct>            <fct>          <dbl> <dbl> <int>
#   1 alpha.tocopherol UV Filtered    2.43   9.73    19
# 2 alpha.tocopherol UV Transmitted 1.47   8.32    19
# 3 alpha.tocopherol Site Reference 2.41  12.6     19
# 4 beta.tocopherol  UV Filtered    1.06   3.34    19
# 5 beta.tocopherol  UV Transmitted 0.758  2.36    19
# 6 beta.tocopherol  Site Reference 0.943  3.67    19


# add y-value position to stat.test tibble for plotting
 stat.test.tocopherols <- stat.test.tocopherols %>% mutate(y.position = c(17.9,19.1,18.5,
                                                                          5.9,6.3,6.1))


box.box.tocopherols<-ggboxplot(summary.nolabcontrol.tocopherols, x = "Treatment", y = "mean",
                              fill = "Treatment",
                              color = "gray30",
                              size = 0.5,
                              point.size = 0.75,
                              outlier.size = 0.25,
                              palette = "Set2",
                              id = "Site",
                              line.color = "#4D4D4D66", 
                              facet.by = c("Pigment"),
                              scales = "free_y",
                                     panel.labs = list(Pigment=c("a-tocopherol", "b-tocopherol")), 
                                     panel.labs.background = list(color = "black", fill = "black"),
                                     panel.labs.font = list(size=14),
                                     width = 0.7,
                                     xlab = "",
                                     ylab = "Normalized quantity (%)",
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
                        font.xtickslab =  c(0, "plain"))

box.box.tocopherols 











































#################



# 
# 
# p.nositeref <- ggplot(data = summary.nositeref, aes(x=Treatment, y=mean)) +
#   geom_col(width = 0.75, aes(fill=Treatment)) +
#   ylab("% of total pigments") +
#   facet_wrap(~Pigment,  ncol=5, scales = "free_y") +
#   scale_fill_manual(values = c("UV Filtered" = UV.filtered, "UV Transmitted" = UV.transmitted)) +
#   theme_light() +
#   geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) +
#   theme(
#     axis.text.x = element_blank(),
#     axis.title.x = element_blank(),
#     axis.ticks = element_blank())
# 
# p.nositeref
# 
# #currently paired but may want to change
# # stat.test.nositeref <- compare_means(mean ~ Treatment, summary.nositeref, 
# #                            group.by = "Pigment", 
# #                            method = "wilcox.test", 
# #                            comparisons = list(c("UV Filtered", "UV Transmitted")), 
# #                            paired = TRUE) 
# 
# ### Add manually p-values from stat.test data
# 
# # First specify the y.position of each comparison
# # need to add custom y position for each facet later
# stat.test.nositeref$y.position <- rep(0)
# 
# # add p values to plot p
# p.nositeref + stat_pvalue_manual(stat.test.nositeref, label = "p.signif", y.position = "y.position", step.group.by = "Pigment", hide.ns = FALSE)
# #  step.increase = .05,
# 
# # can add more info if i want
# # Customize the label with glue expression
# # (https://github.com/tidyverse/glue)
# # p.nositeref + stat_pvalue_manual(stat.test.nositeref, label = "p = {p.adj}")
# 




# 
# 
# 
# ggplot(summary.nolabcontrol.pigments.some, aes(fill=Pigment, y=mean, x=Site)) + 
#   geom_bar(position="dodge", stat="identity")
# 
# 
# p.nositeref <- ggplot(data = summary.nolabcontrol.pigments.some, aes(x=Treatment, y=mean)) +
#   geom_col(aes(fill=Pigment)) +
#   ylab("% of total pigments") +
#  # scale_fill_manual(values = c("UV Filtered" = UV.filtered, "UV Transmitted" = UV.transmitted)) +
#   theme_light() +
#  # geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) +
#   theme(
#     axis.text.x = element_blank(),
#     axis.title.x = element_blank(),
#     axis.ticks = element_blank())
# 
# p.nositeref



# stacked bar chart of FIELD REFERENCE vs LAB CONTROL, one for PIGMENTS and one for TOCOPHEROLS

data_sitereference_labcontrol <- filter(data_long, Treatment %in% c("Site Reference", "Lab Control")) 

data_sitereference_labcontrol_pigments <- filter(data_sitereference_labcontrol, Pigment %in% c("violaxanthin", "antheraxanthin", "zeaxanthin","neoxanthin","beta.carotene.sum", "lutein","chlorophyll.a", "chlorophyll.b")) 

summary_sitereference_labcontrol_pigments <- data_sitereference_labcontrol_pigments %>%
  group_by(Pigment, Treatment)  %>%
  dplyr::summarise(mean = mean(Amount), n = n(), sd = sd(Amount))

summary_sitereference_labcontrol_pigments
# # A tibble: 16 x 5
# # Groups:   Pigment [8]
# Pigment           Treatment        mean     n    sd
# <fct>             <fct>           <dbl> <int> <dbl>
#   1 neoxanthin        Site Reference  3.71     54 0.508
# 2 neoxanthin        Lab Control     4.05      9 0.271
# 3 violaxanthin      Site Reference  0.589    54 0.118
# 4 violaxanthin      Lab Control     3.72      9 0.701
# 5 antheraxanthin    Site Reference  1.43     54 0.193
# 6 antheraxanthin    Lab Control     1.29      9 0.269
# 7 lutein            Site Reference 19.2      54 1.90 
# 8 lutein            Lab Control    15.0       9 1.04 
# 9 zeaxanthin        Site Reference  7.54     54 1.08 
# 10 zeaxanthin        Lab Control     1.30      9 0.279
# 11 chlorophyll.b     Site Reference 17.9      54 2.48 
# 12 chlorophyll.b     Lab Control    28.4       9 0.905
# 13 chlorophyll.a     Site Reference 38.6      54 3.19 
# 14 chlorophyll.a     Lab Control    39.1       9 1.91 
# 15 beta.carotene.sum Site Reference 11.0      54 3.59 
# 16 beta.carotene.sum Lab Control     7.09      9 1.39 



data_sitereference_labcontrol_pigments %>%
  group_by(Pigment)  %>%
rstatix::pairwise_wilcox_test(data = ., Amount ~ Treatment, paired = FALSE, p.adjust.method = "BH") -> stat.test.lab.pigments

stat.test.lab.pigments

# # A tibble: 8 x 10
# Pigment           .y.    group1         group2         n1    n2 statistic          p      p.adj p.adj.signif
# * <fct>             <chr>  <chr>          <chr>       <int> <int>     <dbl>      <dbl>      <dbl> <chr>       
#   1 neoxanthin        Amount Site Reference Lab Control    54     9       101 0.005      0.005      **          
#   2 violaxanthin      Amount Site Reference Lab Control    54     9         0 0.00000191 0.00000191 ****        
#   3 antheraxanthin    Amount Site Reference Lab Control    54     9       288 0.382      0.382      ns          
# 4 lutein            Amount Site Reference Lab Control    54     9       479 0.00000373 0.00000373 ****        
#   5 zeaxanthin        Amount Site Reference Lab Control    54     9       486 0.00000191 0.00000191 ****        
#   6 chlorophyll.b     Amount Site Reference Lab Control    54     9         0 0.00000191 0.00000191 ****        
#   7 chlorophyll.a     Amount Site Reference Lab Control    54     9       226 0.746      0.746      ns          
# 8 beta.carotene.sum Amount Site Reference Lab Control    54     9       417 0.000655   0.000655   ***  
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

# position = fill for percentage, stack for reg stack 

barplot + theme(
  plot.title = element_text(size = 20, face = "bold"),
  axis.text.x = element_text(size = 18, face = "bold"),
  axis.text.y = element_text(size = 12),
  axis.title.y = element_text(size = 18),
  legend.text = element_text(size = 16),
  legend.title = element_text(size = 18)
)


# stacked bar chart lab and field reference / controls tocopherols

data_sitereference_labcontrol <- filter(data_long, Treatment %in% c("Site Reference", "Lab Control")) 

data_sitereference_labcontrol_tocopherols <- filter(data_sitereference_labcontrol, Pigment %in% c("alpha.tocopherol", "beta.tocopherol")) 

summary_sitereference_labcontrol_tocopherols <- data_sitereference_labcontrol_tocopherols %>%
  group_by(Pigment, Treatment)  %>%
  dplyr::summarise(mean = mean(Amount), n = n(), sd = sd(Amount))

summary_sitereference_labcontrol_tocopherols
# # A tibble: 4 x 5
# # Groups:   Pigment [2]
# Pigment          Treatment        mean     n    sd
# <fct>            <fct>           <dbl> <int> <dbl>
#   1 beta.tocopherol  Site Reference  3.74     54 1.36 
# 2 beta.tocopherol  Lab Control     0.678     9 0.405
# 3 alpha.tocopherol Site Reference 12.8      54 3.95 
# 4 alpha.tocopherol Lab Control     5.08      9 1.61 

data_sitereference_labcontrol_tocopherols %>%
  group_by(Pigment)  %>%
  rstatix::pairwise_wilcox_test(data = ., Amount ~ Treatment, paired = FALSE, p.adjust.method = "BH") -> stat.test.lab.tocopherols

stat.test.lab.tocopherols

# # A tibble: 2 x 10
# Pigment          .y.    group1         group2         n1    n2 statistic          p      p.adj p.adj.signif
# * <fct>            <chr>  <chr>          <chr>       <int> <int>     <dbl>      <dbl>      <dbl> <chr>       
#   1 beta.tocopherol  Amount Site Reference Lab Control    54     9       486 0.00000191 0.00000191 ****        
#   2 alpha.tocopherol Amount Site Reference Lab Control    54     9       482 0.00000281 0.00000281 ****   


# order levels of pigments so they plot in order
summary_sitereference_labcontrol_tocopherols$Pigment <- factor(summary_sitereference_labcontrol_tocopherols$Pigment,levels = c("alpha.tocopherol","beta.tocopherol"))



# Grouped stacked bar chart

my_title <- expression(paste("Antioxidants in Field and Lab Grown ", italic("Syntrichia caninervis")))

barplot <- ggplot(summary_sitereference_labcontrol_tocopherols, aes(fill=Pigment, y=mean, x=Treatment)) + 
  geom_bar(position="stack", stat="identity", colour="white") +
 # ggtitle("Antioxidants in Field and Lab Grown Plants") +
  theme_minimal() +
  xlab("") +
  ylab("Normalized quantity (%)") +
  scale_fill_manual(values = c("alpha.tocopherol" = alphat, "beta.tocopherol" = betat), labels = c("a-tocopherol", "b-tocopherol"))

# position = fill for percentage, stack for reg stack 

barplot + theme(
  plot.title = element_text(size = 20, face = "bold"),
  axis.text.x = element_text(size = 18, face = "bold"),
  axis.text.y = element_text(size = 12),
  axis.title.y = element_text(size = 18),
  legend.text = element_text(size = 16),
  legend.title = element_text(size = 18)
) + labs(fill = "Antioxidant")







