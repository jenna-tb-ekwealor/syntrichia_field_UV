library(tidyverse)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(rstatix)
library(Rmisc)

UV.filtered<-"#14d2dc" #turquoise
UV.transmitted<-"#fa7850" #orange
Site.reference<-"#8214a0" #purple

setwd("/Users/jennaekwealor/Documents/dissertation_repositories/syntrichia_field_UV/fluorescence")



field<-read.csv("data_subset_clean.csv")
field$Treatment <- factor(field$Treatment,levels = c("UV Filtered", "UV Transmitted", "Site Reference"))

field$site <- factor(field$site)

# ggplot(data = <DATA>, mapping = aes(<MAPPINGS>)) +  <GEOM_FUNCTION>()

field$Time <- factor(field$Time,levels = c("0", "24", "192"))


# compute and compare means for different time points and different experiemnts, by treatment group

field %>% group_by(Exp,Time) %>%
  pairwise_wilcox_test(data = ., Fv.Fm ~ Treatment, paired = TRUE, 
                       p.adjust.method = "BH") -> stat.test.FvFm

field %>% group_by(Treatment,Time) %>%
  pairwise_wilcox_test(data = ., Fv.Fm ~ Exp, paired = TRUE, 
                       p.adjust.method = "BH") -> stat.test.FvFm_UVTEST

stat.test.FvFm_UVTEST <- stat.test.FvFm_UVTEST %>%
  mutate(y.position = c(0,0,0,0,0.7,0,0,0,0)) %>%
  mutate(p.adj.signif.2 = gsub("ns", "", p.adj.signif))

# # A tibble: 9 x 11
# Time  Treatment      .y.   group1 group2     n1    n2 statistic     p p.adj p.adj.signif
# * <fct> <fct>          <chr> <chr>  <chr>   <int> <int>     <dbl> <dbl> <dbl> <chr>       
#   1 0     UV Filtered    Fv.Fm Field  UV Test    19    19      91   0.508 0.508 ns          
# 2 24    UV Filtered    Fv.Fm Field  UV Test    19    19     119   0.151 0.151 ns          
# 3 192   UV Filtered    Fv.Fm Field  UV Test    19    19     101   0.825 0.825 ns          
# 4 0     UV Transmitted Fv.Fm Field  UV Test    19    19      86   0.67  0.67  ns          
# 5 24    UV Transmitted Fv.Fm Field  UV Test    19    19     163   0.007 0.007 **          
#   6 192   UV Transmitted Fv.Fm Field  UV Test    19    19      86.5 0.747 0.747 ns          
# 7 0     Site Reference Fv.Fm Field  UV Test    19    19      85   0.709 0.709 ns          
# 8 24    Site Reference Fv.Fm Field  UV Test    19    19     105   0.702 0.702 ns          
# 9 192   Site Reference Fv.Fm Field  UV Test    19    19     124.  0.243 0.243 ns 

field %>% group_by(Treatment,Time) %>%
  pairwise_wilcox_test(data = ., e.PS2R ~ Exp, paired = TRUE, 
                       p.adjust.method = "BH") -> stat.test.e.PS2R_UVTEST

stat.test.e.PS2R_UVTEST <- stat.test.e.PS2R_UVTEST %>%
  mutate(y.position = c(0,0,0,0,0.7,0,0,0,0)) %>%
  mutate(p.adj.signif.2 = gsub("ns", "", p.adj.signif))

# # A tibble: 9 x 11
# Time  Treatment      .y.    group1 group2     n1    n2 statistic     p p.adj p.adj.signif
# * <fct> <fct>          <chr>  <chr>  <chr>   <int> <int>     <dbl> <dbl> <dbl> <chr>       
#   1 0     UV Filtered    e.PS2R Field  UV Test    19    19      90   0.862 0.862 ns          
# 2 24    UV Filtered    e.PS2R Field  UV Test    19    19     100   0.856 0.856 ns          
# 3 192   UV Filtered    e.PS2R Field  UV Test    19    19     138   0.087 0.087 ns          
# 4 0     UV Transmitted e.PS2R Field  UV Test    19    19      81   0.862 0.862 ns          
# 5 24    UV Transmitted e.PS2R Field  UV Test    19    19     148.  0.036 0.036 *           
#   6 192   UV Transmitted e.PS2R Field  UV Test    19    19     102.  0.778 0.778 ns          
# 7 0     Site Reference e.PS2R Field  UV Test    19    19      72   0.374 0.374 ns          
# 8 24    Site Reference e.PS2R Field  UV Test    19    19      79.5 0.811 0.811 ns          
# 9 192   Site Reference e.PS2R Field  UV Test    19    19      89.5 0.84  0.84  ns 

stat.test.FvFm <- stat.test.FvFm %>%
  mutate(y.position = rep(c(0.90, 1, 0.95),6))

stat.test.FvFm
# # A tibble: 18 x 12
# Time  Exp     .y.   group1      group2        n1    n2 statistic        p    p.adj p.adj.signif y.position
# * <fct> <fct>   <chr> <chr>       <chr>      <int> <int>     <dbl>    <dbl>    <dbl> <chr>             <dbl>
#   1 0     Field   Fv.Fm UV Filtered UV Transm…    19    19      68    7.05e-1  7.05e-1 ns                 0.9 
# 2 0     Field   Fv.Fm UV Filtered Site Refe…    19    19      31    1.90e-2  5.60e-2 ns                 1   
# 3 0     Field   Fv.Fm UV Transmi… Site Refe…    19    19      47.5  5.80e-2  8.80e-2 ns                 0.95
# 4 24    Field   Fv.Fm UV Filtered UV Transm…    19    19      16    6.45e-4  9.67e-4 ***                0.9 
# 5 24    Field   Fv.Fm UV Filtered Site Refe…    19    19       2    1.14e-5  3.42e-5 ****               1   
# 6 24    Field   Fv.Fm UV Transmi… Site Refe…    19    19      41.5  3.30e-2  3.30e-2 *                  0.95
# 7 192   Field   Fv.Fm UV Filtered UV Transm…    19    19      20.5  3.00e-3  3.00e-3 **                 0.9 
# 8 192   Field   Fv.Fm UV Filtered Site Refe…    19    19       4    2.70e-4  8.10e-4 ***                1   
# 9 192   Field   Fv.Fm UV Transmi… Site Refe…    19    19      11.5  8.35e-4  1.00e-3 **                 0.95
# 10 0     UV Test Fv.Fm UV Filtered UV Transm…    19    19      80    8.28e-1  8.28e-1 ns                 0.9 
# 11 0     UV Test Fv.Fm UV Filtered Site Refe…    19    19      19    4.00e-3  1.20e-2 *                  1   
# 12 0     UV Test Fv.Fm UV Transmi… Site Refe…    19    19      24    8.00e-3  1.20e-2 *                  0.95
# 13 24    UV Test Fv.Fm UV Filtered UV Transm…    19    19      25    5.00e-3  5.00e-3 **                 0.9 
# 14 24    UV Test Fv.Fm UV Filtered Site Refe…    19    19       0    3.81e-6  1.14e-5 ****               1   
# 15 24    UV Test Fv.Fm UV Transmi… Site Refe…    19    19      10    1.64e-4  2.46e-4 ***                0.95
# 16 192   UV Test Fv.Fm UV Filtered UV Transm…    19    19      33    1.10e-2  1.10e-2 *                  0.9 
# 17 192   UV Test Fv.Fm UV Filtered Site Refe…    19    19      10    6.72e-4  2.00e-3 **                 1   
# 18 192   UV Test Fv.Fm UV Transmi… Site Refe…    19    19      17    2.00e-3  3.00e-3 **                 0.95


summary.field <- field %>% group_by(Exp,Time,Treatment) %>%
dplyr::summarize(mean.Fv.Fm = mean(Fv.Fm), sd.Fv.Fm = sd(Fv.Fm), mean.e.PS2R = mean(e.PS2R), sd.e.PS2R = sd(e.PS2R))

summary.field

# # A tibble: 18 x 5
# # Groups:   Exp, Time [6]
# Exp     Time  Treatment        mean     sd
# <fct>   <fct> <fct>           <dbl>  <dbl>
#   1 Field   0     UV Filtered    0.0436 0.0356
# 2 Field   0     UV Transmitted 0.049  0.0455
# 3 Field   0     Site Reference 0.108  0.122 
# 4 Field   24    UV Filtered    0.532  0.0905
# 5 Field   24    UV Transmitted 0.599  0.0543
# 6 Field   24    Site Reference 0.631  0.0445
# 7 Field   192   UV Filtered    0.790  0.0231
# 8 Field   192   UV Transmitted 0.812  0.0149
# 9 Field   192   Site Reference 0.839  0.0212
# 10 UV Test 0     UV Filtered    0.0372 0.0432
# 11 UV Test 0     UV Transmitted 0.0386 0.0402
# 12 UV Test 0     Site Reference 0.118  0.103 
# 13 UV Test 24    UV Filtered    0.517  0.0720
# 14 UV Test 24    UV Transmitted 0.565  0.0655
# 15 UV Test 24    Site Reference 0.631  0.0604
# 16 UV Test 192   UV Filtered    0.788  0.0267
# 17 UV Test 192   UV Transmitted 0.811  0.0197
# 18 UV Test 192   Site Reference 0.833  0.0278



field %>% group_by(Exp,Time) %>%
  pairwise_wilcox_test(data = ., e.PS2R ~ Treatment, paired = TRUE) -> stat.test.ePS2R

stat.test.ePS2R <- stat.test.ePS2R %>%
  mutate(y.position = rep(c(0.74, 0.82, 0.78),6))

stat.test.ePS2R

# # A tibble: 18 x 12
# Time  Exp     .y.    group1        group2          n1    n2 statistic         p    p.adj p.adj.signif y.position
# * <fct> <fct>   <chr>  <chr>         <chr>        <int> <int>     <dbl>     <dbl>    <dbl> <chr>             <dbl>
#   1 0     Field   e.PS2R UV Filtered   UV Transmit…    19    19      57.5   2.31e-1  2.31e-1 ns                 0.74
# 2 0     Field   e.PS2R UV Filtered   Site Refere…    19    19      21.5   3.00e-3  1.00e-2 **                 0.82
# 3 0     Field   e.PS2R UV Transmitt… Site Refere…    19    19      40     2.80e-2  5.70e-2 ns                 0.78
# 4 24    Field   e.PS2R UV Filtered   UV Transmit…    19    19      15.5   1.00e-3  3.00e-3 **                 0.74
# 5 24    Field   e.PS2R UV Filtered   Site Refere…    19    19       0     3.81e-6  1.14e-5 ****               0.82
# 6 24    Field   e.PS2R UV Transmitt… Site Refere…    19    19      41     5.50e-2  5.50e-2 ns                 0.78
# 7 192   Field   e.PS2R UV Filtered   UV Transmit…    19    19      43.5   4.00e-2  8.00e-2 ns                 0.74
# 8 192   Field   e.PS2R UV Filtered   Site Refere…    19    19      20.5   3.00e-3  9.00e-3 **                 0.82
# 9 192   Field   e.PS2R UV Transmitt… Site Refere…    19    19      50     7.30e-2  8.00e-2 ns                 0.78
# 10 0     UV Test e.PS2R UV Filtered   UV Transmit…    19    19      82     8.96e-1  8.96e-1 ns                 0.74
# 11 0     UV Test e.PS2R UV Filtered   Site Refere…    19    19      37     1.80e-2  5.40e-2 ns                 0.82
# 12 0     UV Test e.PS2R UV Transmitt… Site Refere…    19    19      37     1.80e-2  5.40e-2 ns                 0.78
# 13 24    UV Test e.PS2R UV Filtered   UV Transmit…    19    19      29.5   9.00e-3  9.00e-3 **                 0.74
# 14 24    UV Test e.PS2R UV Filtered   Site Refere…    19    19       0     3.81e-6  1.14e-5 ****               0.82
# 15 24    UV Test e.PS2R UV Transmitt… Site Refere…    19    19      16.5   2.00e-3  3.00e-3 **                 0.78
# 16 192   UV Test e.PS2R UV Filtered   UV Transmit…    19    19      25     3.00e-3  7.00e-3 **                 0.74
# 17 192   UV Test e.PS2R UV Filtered   Site Refere…    19    19       5     3.16e-4  9.48e-4 ***                0.82
# 18 192   UV Test e.PS2R UV Transmitt… Site Refere…    19    19      49     6.70e-2  6.70e-2 ns                 0.78


field.field <- field %>%
  dplyr::filter(Exp == "Field") 

field.UV <- field %>%
  dplyr::filter(Exp == "UV Test") 

stat.test.FvFm.field <- stat.test.FvFm %>%
  dplyr::filter(Exp == "Field") 

box.box.fvfm<-ggboxplot(field.field, x = "Treatment", y = "Fv.Fm",
                        fill = "Treatment",
                        color = "gray30",
                        size = 0.5,
                        point.size = 1,
                        outlier.size = 1,
                        id = "Site",
                        line.color = "#4D4D4D66", 
         facet.by = c("Time"),
         panel.labs = list(Time=c("0 hours", "24 hours", "192 hours")), 
         panel.labs.background = list(color = "black", fill = "black"),
         panel.labs.font = list(size=14),
         width = 0.7,
         xlab = "Recovery time",
         ylab = expression("Maximum PSII quantum efficiency (F"[v]*"/F"[m]*")"),
         ggtheme = theme_light()) +
        scale_fill_manual(values=c(UV.filtered, UV.transmitted, Site.reference)) +
        stat_pvalue_manual(stat.test.FvFm.field, label = "p.adj.signif") 
        #title = "Recovery of Photosystem II Maximum Potential Efficiency\n") 


box.box.fvfm<-ggpar(box.box.fvfm,
                    legend = "right",
                    legend.title = "Field Treatment",
                    font.legend = c(16, "plain"),
                    font.ytickslab =  c(16, "plain"),
                    font.y = c(18,"plain"),
                    font.x = c(18,"bold"),
                    font.xtickslab =  c(0, "plain"))
                   # font.title = c(20, "bold"))
           


box.box.fvfm 
# %>% 
#   ggadd("dotplot", size = 0.25)











# 
# 
# #ALL FIELD TREATMENTS LAB REFERNCE LINE 
# 
# box.box.controls.all.fvfm<-ggboxplot(field.field, x = "Treatment", y = "Fv.Fm",
#                         fill = "Treatment",
#                         color = "gray30",
#                         size = 0.5,
#                         point.size = 1,
#                         outlier.size = 1,
#                         palette = "Set2",
#                         id = "Site",
#                         line.color = "#4D4D4D66", 
#                         facet.by = c("Time"),
#                         panel.labs = list(Time=c("0 hours", "24 hours", "192 hours")), 
#                         panel.labs.background = list(color = "black", fill = "black"),
#                         panel.labs.font = list(size=14),
#                         width = 0.7,
#                         xlab = "Recovery time",
#                         ylab = "Maximum chlorophyll photochemical efficiency (Fv/Fm)",
#                         ggtheme = theme_light(),
#                         title = "Recovery of Photosystem II Maximum Potential Efficiency\n") +
#   stat_compare_means(label = "p.signif", paired = TRUE, hide.ns = TRUE,
#                      comparisons = list(c("UV Filtered", "UV Transmitted"), c("UV Transmitted", "Site Reference"))
#   )
# 
# box.box.controls.all.fvfm<-ggpar(box.box.controls.only.fvfm,
#                     legend = "right",
#                     legend.title = "Field Treatment",
#                     font.legend = c(16, "plain"),
#                     font.ytickslab =  c(16, "plain"),
#                     font.y = c(18,"plain"),
#                     font.x = c(18,"bold"),
#                     font.title = c(20, "bold"),
#                     font.xtickslab =  c(0, "plain"))
# 
# h <- 0.764
# box.box.controls.all.fvfm + 
#   geom_hline(aes(yintercept=h, linetype = "Fv/Fm Cultured Plant"), colour="gray45",) +
# scale_linetype_manual(name = "Lab Control", values = 2, 
#                       guide = guide_legend(override.aes = list(color = "gray45")))
# 
# 
# 
# 
# #FIELD SITE REFERENCE ONLY PLUS LAB REFERNCE LINE 
# 
# box.box.controls.only.fvfm<-ggboxplot(subset(field.field, Treatment %in% "Site Reference"), x = "Treatment", y = "Fv.Fm",
#                                       fill = "Treatment",
#                                       color = "gray30",
#                                       size = 0.5,
#                                       point.size = 1,
#                                       outlier.size = 1,
#                                       palette = Site.reference,
#                                       id = "Site",
#                                       line.color = "#4D4D4D66", 
#                                       facet.by = c("Time"),
#                                       panel.labs = list(Time=c("0 hours", "24 hours", "192 hours")), 
#                                       panel.labs.background = list(color = "black", fill = "black"),
#                                       panel.labs.font = list(size=14),
#                                       width = 0.7,
#                                       xlab = "Recovery time",
#                                       ylab = "Maximum chlorophyll photochemical efficiency (Fv/Fm)",
#                                       ggtheme = theme_light(),
#                                       title = "Recovery of Photosystem II Maximum Potential Efficiency\n") 
#   # stat_compare_means(label = "p.signif", paired = TRUE, hide.ns = TRUE,
#   #                    comparisons = list(c("UV Filtered", "UV Transmitted"), c("UV Transmitted", "Site Reference"))
#   # )
# 
# box.box.controls.only.fvfm<-ggpar(box.box.controls.only.fvfm,
#                                   legend = "right",
#                                   legend.title = "Field Treatment",
#                                   font.legend = c(16, "plain"),
#                                   font.ytickslab =  c(16, "plain"),
#                                   font.y = c(18,"plain"),
#                                   font.x = c(18,"bold"),
#                                   font.title = c(20, "bold"),
#                                   font.xtickslab =  c(0, "plain"))
# 
# 
# h <- 0.764
# box.box.controls.only.fvfm + 
#   geom_hline(aes(yintercept=h, linetype = "Fv/Fm Cultured Plant"), colour="gray45",) +
#   scale_linetype_manual(name = "Lab Control", values = 2, 
#                         guide = guide_legend(override.aes = list(color = "gray45")))
#   
# 
# 





### phi-psII paired box plot field treatments, 



stat.test.ePS2R.field <- stat.test.ePS2R %>%
  dplyr::filter(Exp == "Field") 

box.box.ePS2R<-ggboxplot(field.field, x = "Treatment", y = "e.PS2R",
                        fill = "Treatment",
                        color = "gray30",
                        size = 0.5,
                        point.size = 1,
                        outlier.size = 1,
                        id = "Site",
                        line.color = "#4D4D4D66", 
                        facet.by = c("Time"),
                        panel.labs = list(Time=c("0 hours", "24 hours", "192 hours")), 
                        panel.labs.background = list(color = "black", fill = "black"),
                        panel.labs.font = list(size=14),
                        width = 0.7,
                        xlab = "Recovery time",
                        ylab = "PSII quantum efficiency (\u03A6PSII)",
                        ggtheme = theme_light()) +
  scale_fill_manual(values=c(UV.filtered, UV.transmitted, Site.reference)) +
  stat_pvalue_manual(stat.test.ePS2R.field, label = "p.adj.signif") 
#title = "Recovery of Photosystem II Quantum Efficiency\n") 


box.box.ePS2R<-ggpar(box.box.ePS2R,
                    legend = "right",
                    legend.title = "Field Treatment",
                    font.legend = c(16, "plain"),
                    font.ytickslab =  c(16, "plain"),
                    font.y = c(18,"plain"),
                    font.x = c(18,"bold"),
                    font.xtickslab =  c(0, "plain"))
# font.title = c(20, "bold"))



box.box.ePS2R 
# %>% 
#   ggadd("dotplot", size = 0.25)





######################################################################################################




field.field

# between and within summary stats for error bars, field samples, fv/fm

ffsse_fvfm<-summarySE(field.field, measurevar="Fv.Fm", groupvars= c("Time","Treatment","Exp"), na.rm=FALSE, conf.interval=.95)

ffssew_fvfm<-summarySEwithin(field.field, measurevar="Fv.Fm", withinvars= c("Time","Treatment","Exp"),
                idvar="site", na.rm=FALSE, conf.interval=.95)
# add measure type to last column so we can change headers to generic names 

ffsse_fvfm$Metric<-rep("Fv.Fm")
ffssew_fvfm$Metric<-rep("Fv.Fm")



# between and within summary stats for error bars, field samples, phi psii

ffsse_eps2r<-summarySE(field.field, measurevar="e.PS2R", groupvars= c("Time","Treatment","Exp"), na.rm=FALSE, conf.interval=.95)

ffssew_eps2r<-summarySEwithin(field.field, measurevar="e.PS2R", withinvars= c("Time","Treatment","Exp"),
                idvar="site", na.rm=FALSE, conf.interval=.95)

# add measure type to last column so we can change headers to generic names 

ffsse_eps2r$Metric<-rep("e.PS2R")
ffssew_eps2r$Metric<-rep("e.PS2R")


# change column headers to make generic
colnames(ffsse_fvfm)[colnames(ffsse_fvfm)=="Fv.Fm"] <- "Value"
colnames(ffssew_fvfm)[colnames(ffssew_fvfm)=="Fv.Fm"] <- "Value"
colnames(ffssew_fvfm)[colnames(ffssew_fvfm)=="Fv.Fm_norm"] <- "Value_norm"

colnames(ffsse_eps2r)[colnames(ffsse_eps2r)=="e.PS2R"] <- "Value"
colnames(ffssew_eps2r)[colnames(ffssew_eps2r)=="e.PS2R"] <- "Value"
colnames(ffssew_eps2r)[colnames(ffssew_eps2r)=="e.PS2R_norm"] <- "Value_norm"

#combine stats into one table for field field, each for regualr and for within stats
ffsse_combined_metrics<-rbind(ffsse_fvfm,ffsse_eps2r)
ffssew_combined_metrics<-rbind(ffssew_fvfm,ffssew_eps2r)





field.UV

# between and within summary stats for error bars, UV samples, fv/fm

fUVsse_fvfm<-summarySE(field.UV, measurevar="Fv.Fm", groupvars= c("Time","Treatment","Exp"), na.rm=FALSE, conf.interval=.95)

fUVssew_fvfm<-summarySEwithin(field.UV, measurevar="Fv.Fm", withinvars= c("Time","Treatment","Exp"),
                             idvar="site", na.rm=FALSE, conf.interval=.95)
# add measure type to last column so we can change headers to generic names 

fUVsse_fvfm$Metric<-rep("Fv.Fm")
fUVssew_fvfm$Metric<-rep("Fv.Fm")

# change column headers to make generic
colnames(fUVsse_fvfm)[colnames(fUVsse_fvfm)=="Fv.Fm"] <- "Value"
colnames(fUVssew_fvfm)[colnames(fUVssew_fvfm)=="Fv.Fm"] <- "Value"
colnames(fUVssew_fvfm)[colnames(fUVssew_fvfm)=="Fv.Fm_norm"] <- "Value_norm"




# between and within summary stats for error bars, UV samples, PhiPSII

fUVsse_eps2r<-summarySE(field.UV, measurevar="e.PS2R", groupvars= c("Time","Treatment","Exp"), na.rm=FALSE, conf.interval=.95)

fUVssew_eps2r<-summarySEwithin(field.UV, measurevar="e.PS2R", withinvars= c("Time","Treatment","Exp"),
                              idvar="site", na.rm=FALSE, conf.interval=.95)
# add measure type to last column so we can change headers to generic names 

fUVsse_eps2r$Metric<-rep("e.PS2R")
fUVssew_eps2r$Metric<-rep("e.PS2R")

# change column headers to make generic
colnames(fUVsse_eps2r)[colnames(fUVsse_eps2r)=="e.PS2R"] <- "Value"
colnames(fUVssew_eps2r)[colnames(fUVssew_eps2r)=="e.PS2R"] <- "Value"
colnames(fUVssew_eps2r)[colnames(fUVssew_eps2r)=="e.PS2R_norm"] <- "Value_norm"

#combine stats into one table for UV test, each for regualr and for within stats
fUVsse_combined_metrics<-rbind(fUVsse_fvfm,fUVsse_eps2r)
fUVssew_combined_metrics<-rbind(fUVssew_fvfm,fUVssew_eps2r)

ffUVsse_combined_metrics<-rbind(ffsse_combined_metrics,fUVsse_combined_metrics)
ffUVssew_combined_metrics<-rbind(ffssew_combined_metrics,fUVssew_combined_metrics)


colnames(ffUVsse_combined_metrics)[colnames(ffUVsse_combined_metrics)=="Exp"] <- "LabTreatment"
colnames(ffUVssew_combined_metrics)[colnames(ffUVssew_combined_metrics)=="Exp"] <- "LabTreatment"

ffUVsse_combined_metrics$LabTreatment <- factor(ffUVsse_combined_metrics$LabTreatment,levels = c("Field", "UV Test"))
ffUVssew_combined_metrics$LabTreatment <- factor(ffUVssew_combined_metrics$LabTreatment,levels = c("Field", "UV Test"))



LegendTitle_color <- "Field Treatment"
LegendTitle_line <- "Lab Treatment"


#### plot field and UV test line plot FvFm ####

ffUVsse_combined_metrics %>% filter(., Metric == "Fv.Fm") %>%
ggplot(., aes(x=Time, y=Value, group=interaction(LabTreatment, Treatment), color=Treatment)) +
  geom_line(size=0.7, aes(linetype=LabTreatment)) +
  labs(x = "Recovery Time (hours)", y = "Maximum potential PSII quantum efficency (Fv/Fm)", color = "Treatment", line = "Lab Treatment") +
  geom_errorbar(width=.1, aes(ymin=Value-se, ymax=Value+se)) + #betewen group error bars
  #geom_errorbar(width=.1, aes(ymin=Value-se, ymax=Value+se), color="black", data=ffssew_combined_metrics) + #within time group error bars
  ylim(0,0.9) +
  theme_minimal() +
  theme(legend.position="right") +
  scale_color_manual(values=c(UV.filtered, UV.transmitted, Site.reference)) +
  scale_linetype_discrete(name = "Lab Treatment", labels=c("UV Dose","None")) + 
  guides(linetype = guide_legend(reverse = TRUE)) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16,face="bold"),
        title = element_text(size = 20))+ 
  labs(
    color = LegendTitle_color,
    linetype = LegendTitle_line
  ) + 
  annotate("text", x = "24", y = 0.7, label = "**", size = 5)


#### plot field and UV test line plot PhipsII ####

ffUVsse_combined_metrics %>% filter(., Metric == "e.PS2R") %>%
  ggplot(., aes(x=Time, y=Value, group=interaction(LabTreatment, Treatment), color=Treatment)) +
  geom_line(size=0.7, aes(linetype=LabTreatment)) +
  labs(x = "Recovery Time (hours)", y = "PSII operating quantum efficency (PPSII)", color = "Treatment", line = "Lab Treatment") +
  geom_errorbar(width=.1, aes(ymin=Value-se, ymax=Value+se)) + #betewen group error bars
  #geom_errorbar(width=.1, aes(ymin=Value-se, ymax=Value+se), color="black", data=ffssew_combined_metrics) + #within time group error bars
  ylim(0,0.7) +
  theme_minimal() +
  theme(legend.position="right") +
  scale_color_manual(values=c(UV.filtered, UV.transmitted, Site.reference)) +
  scale_linetype_discrete(name = "Lab Treatment", labels=c("UV Dose","None")) + 
  guides(linetype = guide_legend(reverse = TRUE)) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16,face="bold"),
        title = element_text(size = 20))+ 
  labs(
    color = LegendTitle_color,
    linetype = LegendTitle_line
  )+ 
  annotate("text", x = "24", y = 0.47, label = "*", size = 5, color = UV.transmitted)






# ### fo paired box plot field treatments, need to add significance 
# 
# compare_means(Fo ~ Treatment, data = field.field.0, paired = TRUE)
# #
# # # A tibble: 3 x 8
# # .y.   group1       group2                p  p.adj p.format p.signif method  
# # <chr> <chr>        <chr>             <dbl>  <dbl> <chr>    <chr>    <chr>   
# #   1 Fo    UV Filtered UV Transmitted     0.760    0.76   0.76048  ns       Wilcoxon
# # 2 Fo    UV Filtered Site Reference 0.000523 0.0016 0.00052  ***      Wilcoxon
# # 3 Fo    UV Transmitted   Site Reference 0.0112   0.022  0.01122  *        Wilcoxon
# 
# compare_means(Fo ~ Treatment, data = field.field.24, paired = TRUE)
# #
# # # A tibble: 3 x 8
# # .y.   group1       group2               p  p.adj p.format p.signif method  
# # <chr> <chr>        <chr>            <dbl>  <dbl> <chr>    <chr>    <chr>   
# #   1 Fo    UV Filtered UV Transmitted     0.0947  0.095  0.0947   ns       Wilcoxon
# # 2 Fo    UV Filtered Site Reference 0.00120 0.0036 0.0012   **       Wilcoxon
# # 3 Fo    UV Transmitted   Site Reference 0.0124  0.025  0.0124   *        Wilcoxon
# 
# compare_means(Fo ~ Treatment, data = field.field.192, paired = TRUE)
# 
# # # A tibble: 3 x 8
# # .y.   group1       group2               p  p.adj p.format p.signif method  
# # <chr> <chr>        <chr>            <dbl>  <dbl> <chr>    <chr>    <chr>   
# #   1 Fo    UV Filtered UV Transmitted     0.0611  0.099  0.0611   ns       Wilcoxon
# # 2 Fo    UV Filtered Site Reference 0.00208 0.0062 0.0021   **       Wilcoxon
# # 3 Fo    UV Transmitted   Site Reference 0.0494  0.099  0.0494   *        Wilcoxon
# 
# 
# mylabelsfo <- data.frame(Time = c(0, 24, 192), 
#                            sig = c("           ", "***         *", "  **       ***"))
# mylabelsfo$Time <- factor(mylabelsfo$Time,levels = c("0", "24", "192"))
# 
# box.lines.fo<-ggpaired(field.field, x = "Treatment", y = "Fo",
#                          color = "Treatment", 
#                          point.size = 2.5,
#                          line.size = 0.6,
#                          palette = "Set2",
#                          id = "site",
#                          line.color = "gray30", 
#                          facet.by = c("Time"),
#                          panel.labs = list(Time=c("0 hours", "24 hours", "192 hours")), 
#                          panel.labs.background = list(color = "black", fill = "black"),
#                          panel.labs.font = list(size=14),
#                          width = 0.7,
#                          xlab = "Recovery time",
#                          ylab = "(Fo)",
#                          ggtheme = theme_light(),
#                          title = "Recovery of Fo by Treatment\n") +
#   stat_compare_means(label = "p.signif", paired = TRUE, hide.ns = TRUE,
#                      comparisons = list(c("UV Filtered", "UV Transmitted"), c("UV Transmitted", "Site Reference"))
#   )
# 
# box.lines.fo<-ggpar(box.lines.fo,
#                       legend = "right",
#                       legend.title = "Field Treatment",
#                       font.legend = c(16, "plain"),
#                       font.ytickslab =  c(16, "plain"),
#                       font.y = c(18,"plain"),
#                       font.x = c(18,"bold"),
#                       font.title = c(20),
#                       font.xtickslab =  c(0, "plain"))
# 
# 
# box.lines.fo
# 
# adjustcolor( "gray30", alpha.f = 0.4)
# 
# # #4D4D4D66
# 
# box.box.fo<-ggpaired(field.field, x = "Treatment", y = "Fo",
#                        fill = "Treatment",
#                        color = "gray30",
#                        point.size = 1.5,
#                        line.size = 0.4,
#                        palette = "Set2",
#                        id = "site",
#                        line.color = "#4D4D4D66", 
#                        facet.by = c("Time"),
#                        panel.labs = list(Time=c("0 hours", "24 hours", "192 hours")), 
#                        panel.labs.background = list(color = "black", fill = "black"),
#                        panel.labs.font = list(size=14),
#                        width = 0.7,
#                        xlab = "Recovery time",
#                        ylab = "(Fo)",
#                        ggtheme = theme_light(),
#                        title = "Fo by Treatment\n") +
#   stat_compare_means(label = "p.signif", paired = TRUE, hide.ns = TRUE,
#                      comparisons = list(c("UV Filtered", "UV Transmitted"), c("UV Transmitted", "Site Reference"))
#   )
# 
# box.box.fo<-ggpar(box.box.fo,
#                     legend = "right",
#                     legend.title = "Field Treatment",
#                     font.legend = c(16, "plain"),
#                     font.ytickslab =  c(16, "plain"),
#                     font.y = c(18,"plain"),
#                     font.x = c(18,"bold"),
#                     font.title = c(20),
#                     font.xtickslab =  c(0, "plain"))
# 
# 
# box.box.fo
# 
# 
# 
# 
# 
# 
# 
# ### fo paired box plot field treatments, need to add significance 
# 
# compare_means(Fm ~ Treatment, data = field.field.0, paired = TRUE)
# #
# # # A tibble: 3 x 8
# # .y.   group1       group2                p  p.adj p.format p.signif method  
# # <chr> <chr>        <chr>             <dbl>  <dbl> <chr>    <chr>    <chr>   
# #   1 Fm    UV Filtered UV Transmitted     0.778    0.78   0.77816  ns       Wilcoxon
# # 2 Fm    UV Filtered Site Reference 0.000645 0.0019 0.00064  ***      Wilcoxon
# # 3 Fm    UV Transmitted   Site Reference 0.0141   0.028  0.01407  *        Wilcoxon
# 
# compare_means(Fm ~ Treatment, data = field.field.24, paired = TRUE)
# #
# # # A tibble: 3 x 8
# # .y.   group1       group2              p p.adj p.format p.signif method  
# # <chr> <chr>        <chr>           <dbl> <dbl> <chr>    <chr>    <chr>   
# #   1 Fm    UV Filtered UV Transmitted     0.601   0.6  0.60     ns       Wilcoxon
# # 2 Fm    UV Filtered Site Reference 0.0799  0.21 0.08     ns       Wilcoxon
# # 3 Fm    UV Transmitted   Site Reference 0.0701  0.21 0.07     ns       Wilcoxon
# 
# compare_means(Fm ~ Treatment, data = field.field.192, paired = TRUE)
# 
# # # A tibble: 3 x 8
# # .y.   group1       group2              p p.adj p.format p.signif method  
# # <chr> <chr>        <chr>           <dbl> <dbl> <chr>    <chr>    <chr>   
# #   1 Fm    UV Filtered UV Transmitted     0.587   0.59 0.59     ns       Wilcoxon
# # 2 Fm    UV Filtered Site Reference 0.0602  0.18 0.06     ns       Wilcoxon
# # 3 Fm    UV Transmitted   Site Reference 0.196   0.39 0.20     ns       Wilcoxon
# 
# 
# mylabelsfm <- data.frame(Time = c(0, 24, 192), 
#                          sig = c("           ", "***         *", "  **       ***"))
# mylabelsfm$Time <- factor(mylabelsfm$Time,levels = c("0", "24", "192"))
# 
# box.lines.fm<-ggpaired(field.field, x = "Treatment", y = "Fm",
#                        color = "Treatment", 
#                        point.size = 2.5,
#                        line.size = 0.6,
#                        palette = "Set2",
#                        id = "site",
#                        line.color = "gray30", 
#                        facet.by = c("Time"),
#                        panel.labs = list(Time=c("0 hours", "24 hours", "192 hours")), 
#                        panel.labs.background = list(color = "black", fill = "black"),
#                        panel.labs.font = list(size=14),
#                        width = 0.7,
#                        xlab = "Recovery time",
#                        ylab = "(Fm)",
#                        ggtheme = theme_light(),
#                        title = "Recovery of Fm by Treatment\n") +
#   stat_compare_means(label = "p.signif", paired = TRUE, hide.ns = TRUE,
#                      comparisons = list(c("UV Filtered", "UV Transmitted"), c("UV Transmitted", "Site Reference"))
#   )
# 
# box.lines.fm<-ggpar(box.lines.fm,
#                     legend = "right",
#                     legend.title = "Field Treatment",
#                     font.legend = c(16, "plain"),
#                     font.ytickslab =  c(16, "plain"),
#                     font.y = c(18,"plain"),
#                     font.x = c(18,"bold"),
#                     font.title = c(20),
#                     font.xtickslab =  c(0, "plain"))
# 
# 
# box.lines.fm
# 
# adjustcolor( "gray30", alpha.f = 0.4)
# 
# # #4D4D4D66
# 
# box.box.fm<-ggpaired(field.field, x = "Treatment", y = "Fm",
#                      fill = "Treatment",
#                      color = "gray30",
#                      point.size = 1.5,
#                      line.size = 0.4,
#                      palette = "Set2",
#                      id = "site",
#                      line.color = "#4D4D4D66", 
#                      facet.by = c("Time"),
#                      panel.labs = list(Time=c("0 hours", "24 hours", "192 hours")), 
#                      panel.labs.background = list(color = "black", fill = "black"),
#                      panel.labs.font = list(size=14),
#                      width = 0.7,
#                      xlab = "Recovery time",
#                      ylab = "(Fm)",
#                      ggtheme = theme_light(),
#                      title = "Fm by Treatment\n") +
#   stat_compare_means(label = "p.signif", paired = TRUE, hide.ns = TRUE,
#                      comparisons = list(c("UV Filtered", "UV Transmitted"), c("UV Transmitted", "Site Reference"))
#   )
# 
# box.box.fm<-ggpar(box.box.fm,
#                   legend = "right",
#                   legend.title = "Field Treatment",
#                   font.legend = c(16, "plain"),
#                   font.ytickslab =  c(16, "plain"),
#                   font.y = c(18,"plain"),
#                   font.x = c(18,"bold"),
#                   font.title = c(20),
#                   font.xtickslab =  c(0, "plain"))
# 
# 
# box.box.fm
# 
# 
# 
# #########
# 
##### line graph for fo and fm on field samples #####


field.field

# between and within summary stats for error bars, field samples, fo

ffsse_fo<-summarySE(field.field, measurevar="Fo", groupvars= c("Time","Treatment"), na.rm=FALSE, conf.interval=.95)

ffssew_fo<-summarySEwithin(field.field, measurevar="Fo", withinvars= c("Time","Treatment"),
                             idvar="site", na.rm=FALSE, conf.interval=.95)
# add measure type to last column so we can change headers to generic names

ffsse_fo$Metric<-rep("Fo")
ffssew_fo$Metric<-rep("Fo")



# between and within summary stats for error bars, field samples, fm

ffsse_fm<-summarySE(field.field, measurevar="Fm", groupvars= c("Time","Treatment"), na.rm=FALSE, conf.interval=.95)

ffssew_fm<-summarySEwithin(field.field, measurevar="Fm", withinvars= c("Time","Treatment"),
                              idvar="site", na.rm=FALSE, conf.interval=.95)

# add measure type to last column so we can change headers to generic names

ffsse_fm$Metric<-rep("Fm")
ffssew_fm$Metric<-rep("Fm")


# change column headers to make generic
colnames(ffsse_fo)[colnames(ffsse_fo)=="Fo"] <- "Value"
colnames(ffssew_fo)[colnames(ffssew_fo)=="Fo"] <- "Value"
colnames(ffssew_fo)[colnames(ffssew_fo)=="Fo_norm"] <- "Value_norm"

colnames(ffsse_fm)[colnames(ffsse_fm)=="Fm"] <- "Value"
colnames(ffssew_fm)[colnames(ffssew_fm)=="Fm"] <- "Value"
colnames(ffssew_fm)[colnames(ffssew_fm)=="Fm_norm"] <- "Value_norm"

#combine stats into one table for field field, each for regualr and for within stats
ffsse_combined_metrics_fofm<-rbind(ffsse_fo,ffsse_fm)
ffssew_combined_metrics_fofm<-rbind(ffssew_fo,ffssew_fm)


ggplot(ffsse_combined_metrics_fofm, aes(x=Time, y=Value, group=interaction(Treatment,Metric), color=Treatment)) +
  geom_line(size=0.7, aes(linetype=Metric)) +
  labs(title = "Photosynthetic Performance Over Recovery Time\n", x = "Recovery Time (hours)", y = "Photosynthetic Performance\n", color = "Treatment", line = "Metric") +
  geom_errorbar(width=.1, aes(ymin=Value-se, ymax=Value+se)) + #betewen group error bars
  #geom_errorbar(width=.1, aes(ymin=Value-se, ymax=Value+se), color="black", data=ffssew_combined_metrics) + #within time group error bars
  # ylim(0,0.9) +
  theme_minimal() +
  theme(legend.position="right") +
  scale_color_manual(values=c(UV.filtered, UV.transmitted, Site.reference)) +
  scale_linetype_discrete(name = "Metric", labels =c("Fm", "Fo")) +
  guides(linetype = guide_legend(reverse = TRUE)) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16,face="bold"),
        title = element_text(size = 20)) + 
  ylim(0,1050)






#########

##### line graph for fo and fm on UV test samples #####


field.UV

# between and within summary stats for error bars, UV test samples, fo

fUVsse_fo<-summarySE(field.UV, measurevar="Fo", groupvars= c("Time","Treatment"), na.rm=FALSE, conf.interval=.95)

fUVssew_fo<-summarySEwithin(field.UV, measurevar="Fo", withinvars= c("Time","Treatment"),
                           idvar="site", na.rm=FALSE, conf.interval=.95)
# add measure type to last column so we can change headers to generic names

fUVsse_fo$Metric<-rep("Fo")
fUVssew_fo$Metric<-rep("Fo")



# between and within summary stats for error bars, field samples, fm

fUVsse_fm<-summarySE(field.UV, measurevar="Fm", groupvars= c("Time","Treatment"), na.rm=FALSE, conf.interval=.95)

fUVssew_fm<-summarySEwithin(field.UV, measurevar="Fm", withinvars= c("Time","Treatment"),
                           idvar="site", na.rm=FALSE, conf.interval=.95)

# add measure type to last column so we can change headers to generic names

fUVsse_fm$Metric<-rep("Fm")
fUVssew_fm$Metric<-rep("Fm")


# change column headers to make generic
colnames(fUVsse_fo)[colnames(fUVsse_fo)=="Fo"] <- "Value"
colnames(fUVssew_fo)[colnames(fUVssew_fo)=="Fo"] <- "Value"
colnames(fUVssew_fo)[colnames(fUVssew_fo)=="Fo_norm"] <- "Value_norm"

colnames(fUVsse_fm)[colnames(fUVsse_fm)=="Fm"] <- "Value"
colnames(fUVssew_fm)[colnames(fUVssew_fm)=="Fm"] <- "Value"
colnames(fUVssew_fm)[colnames(fUVssew_fm)=="Fm_norm"] <- "Value_norm"

#combine stats into one table for field field, each for regualr and for within stats
fUVsse_combined_metrics_fofm<-rbind(fUVsse_fo,fUVsse_fm)
fUVssew_combined_metrics_fofm<-rbind(fUVssew_fo,fUVssew_fm)


ggplot(fUVsse_combined_metrics_fofm, aes(x=Time, y=Value, group=interaction(Treatment,Metric), color=Treatment)) +
  geom_line(size=0.7, aes(linetype=Metric)) +
  labs(title = "", x = "Recovery Time (hours)", y = "Chlorophyll fluorescence (bits)", color = "Treatment", line = "Metric") +
  geom_errorbar(width=.1, aes(ymin=Value-se, ymax=Value+se)) + #betewen group error bars
  #geom_errorbar(width=.1, aes(ymin=Value-se, ymax=Value+se), color="black", data=ffssew_combined_metrics) + #within time group error bars
  # ylim(0,0.9) +
  theme_minimal() +
  theme(legend.position="right") +
  scale_color_manual(values=c(UV.filtered, UV.transmitted, Site.reference)) +
  scale_linetype_discrete(name = "Metric", labels =c("Fm", "Fo")) +
  guides(linetype = guide_legend(reverse = TRUE)) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16,face="bold"),
        title = element_text(size = 20))+ 
  ylim(0,1050)


# 


#### line graph for fv/fm on field and UV test samples #####


field


ggplot(fsse_combined_metrics_fvfm, aes(x=Time, y=Value, group=interaction(Treatment,Metric), color=Treatment)) +
  geom_line(size=0.7, aes(linetype=Metric)) +
  labs(title = "UVTEST samples: Photosynthetic Performance Over Recovery Time\n", x = "Recovery Time (hours)", y = "Photosynthetic Performance\n", color = "Treatment", line = "Metric") +
  geom_errorbar(width=.1, aes(ymin=Value-se, ymax=Value+se)) + #betewen group error bars
  #geom_errorbar(width=.1, aes(ymin=Value-se, ymax=Value+se), color="black", data=ffssew_combined_metrics) + #within time group error bars
  # ylim(0,0.9) +
  theme_minimal() +
  theme(legend.position="right") +
  scale_color_manual(values=c(UV.filtered, UV.transmitted, Site.reference)) +
  scale_linetype_discrete(name = "Metric", labels =c("Fm", "Fo")) + 
  guides(linetype = guide_legend(reverse = TRUE)) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16,face="bold"),
        title = element_text(size = 20)) 






# compare UV Test and Field Fv.Fm


field %>% group_by(Treatment,Time) %>%
  pairwise_wilcox_test(data = ., Fv.Fm ~ Exp, paired = TRUE) -> stat.test.FvFm.Exp

view(stat.test.FvFm.Exp)




# compare UV Test and Field ePS2R

field %>% group_by(Treatment,Time) %>%
  pairwise_wilcox_test(data = ., e.PS2R ~ Exp, paired = TRUE) -> stat.test.ePS2R.Exp

view(stat.test.ePS2R.Exp)






ggpubr::ggarrange(sp,                                                 # First row with scatter plot
          ggarrange(bxp, dp, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
          nrow = 2, 
          labels = "A"                                        # Labels of the scatter plot
) 



