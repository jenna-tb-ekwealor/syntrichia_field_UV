#field_transcriptomics
library(DESeq2)
library(dplyr)
library(ggplot2)
library(pheatmap)


UV.filtered<-"#14d2dc" #turquoise
UV.transmitted<-"#fa7850" #orange


setwd("/Users/jennaekwealor/Box Sync/dissertation/project-field/transcriptomics/")

directory <- ("htseq-count_10142020/")

# load sample table
sampleTable <- read.csv("sample_key.csv")
sampleTable$site <- factor(sampleTable$site)

# load annotations
func_annot <- read.csv(file = "NEW_func_annot_GO_terms_SC.csv")

#### deseq #####
dds <- DESeqDataSetFromHTSeqCount(sampleTable = sampleTable,
                                     directory = directory,
                                     design = ~ site + UV) #  the UV effect represents the overall effect controlling for differences due to site

# relevel treatments so that one is the 'reference' in differential abundance analyses
dds$UV <- relevel(dds$UV, ref = "transmitted") # level treatments so one is marked as 'zero' as the baseline in terms of diff abundance

# run the DE analysis
dds <- DESeq(dds)
resultsNames(dds)
res <- results(dds)
summary(res)
# 6885 total genes


# filter out rows with less than 10 reads
keep <- rowSums(counts(dds)) >= 10 #  rows with at least 10 reads
dds <- dds[keep,] # keep rows from above


# plot dispersion
plotDispEsts(dds)

# plot PCA
vsd <- vst(dds, blind=T)
plotPCA(vsd, intgroup=c("site", "UV")) + theme_minimal()
# ntop = number of top genes to use for principal components, selected by highest row variance

# use returnData = TRUE should the function only return the data.frame of PC1 and PC2 with intgroup covariates for custom plotting
pca_df <- plotPCA(vsd, intgroup=c("site", "UV"), returnData = TRUE) 


p_pca_all <- pca_df %>% group_by(UV) %>% ggplot(.,aes(x=PC1,y=PC2))
p_pca_all <- p_pca_all +
  geom_point(aes(color = UV), size = 3) + 
  geom_line(aes(group = site),color="grey") +
  scale_color_manual(name = "UV Treatment", labels = c("UV-Filtered", "UV-Transmitted"),values = c(UV.filtered, UV.transmitted)) +
  theme_minimal() +
  theme(legend.text.align = 0) +
  guides(shape = FALSE) +
  xlab("PC1 (41%)") + ylab("PC2 (24%)") 
p_pca_all

pdf("p_pca_all.pdf") 
p_pca_all
dev.off()

# #### plot heat map top varying 1000 GENES ####
# topVarGenes <- head(order(rowVars(assay(vsd)), decreasing = TRUE), 1000)
# mat  <- assay(vsd)[ topVarGenes, ]
# mat  <- mat - rowMeans(mat)
# anno <- as.data.frame(colData(vsd)[, c("UV")])
# ann_colors = list(time = c(filtered = UV.filtered, transmitted = UV.transmitted))
# 
# heatmap <- pheatmap(mat = mat, 
#                        annotation_col = anno, 
#                        annotation_colors = ann_colors, 
#                        show_rownames = FALSE,
#                        show_colnames = F, 
#                        drop_levels = TRUE,
#                        legend = TRUE, 
#                        annotation_legend = TRUE,
#                        fontsize = 16,
#                        main = "TOP 1000 Heatmap")
# heatmap
# 
# pdf("heatmap_1000.pdf") 
# heatmap
# dev.off()
# 


### UV filtered vs transmitted (controlling for differences due to site)

resultsNames(dds) # gives a list of "name" comparisons you can use as shortcut instead of "contrasts"
results(dds, name = "UV_filtered_vs_transmitted")  -> UV_filtered_vs_transmitted
# make df of UV genes with pvalue of less than 0.05 and shrunken LFC of at least 2
UV_filtered_vs_transmitted %>%
  as.data.frame() %>%
  mutate(transcript_id = rownames(.) ) %>%
 # filter(log2FoldChange > 1 | log2FoldChange < -1) %>%
  filter(padj < 0.005) -> UV_filtered_vs_transmitted_df

# join annotations to results file
inner_join(x = func_annot, y = UV_filtered_vs_transmitted_df, by = "transcript_id") %>%
  distinct(transcript_id,  .keep_all = T) -> UV_filtered_vs_transmitted_ann

write.csv(UV_filtered_vs_transmitted_ann, "UV_filtered_vs_transmitted_ann.csv")

length(UV_filtered_vs_transmitted_ann$transcript_id)
# 19 genes

UV_filtered_vs_transmitted_ann %>%
  mutate(abs_log2FoldChange = abs(log2FoldChange)) %>%
  arrange(abs_log2FoldChange) %>%
  tail(n = 10) -> UV_filtered_vs_transmitted_ann_strict
write.csv(UV_filtered_vs_transmitted_ann_strict, "UV_filtered_vs_transmitted_ann_strict.csv")


### Plot LFC  

# lfcshrink 
lfcshrink_UV_filtered_vs_transmitted <- lfcShrink(dds, coef="UV_filtered_vs_transmitted", lfcThreshold=2, type="apeglm")

# MAplot 
plotMA(lfcshrink_UV_filtered_vs_transmitted, ylim=c(-4,4), cex=0.5, alpha = 0.005)
abline(h=c(-2,2), col="lightgray", lwd=2) 
while (!is.null(dev.list()))  dev.off()

pdf("maplot_lfcshrink_UV_filtered_vs_transmitted.pdf") 
plotMA(lfcshrink_UV_filtered_vs_transmitted, ylim=c(-4,4), cex=0.8, alpha = 0.005,
       xlab = "Mean of Normalized Counts",
       ylab = "Log Fold Change")
abline(h=c(-2,2), col="lightgray", lwd=2) 
while (!is.null(dev.list()))  dev.off()


#### plot normalized counts of genes of interest ####

#### light effect ####
# filter the top lfc from the sig data
UV_filtered_vs_transmitted_ann_strict_5 <- UV_filtered_vs_transmitted_ann_strict[order(UV_filtered_vs_transmitted_ann_strict$abs_log2FoldChange),] %>% dplyr::top_n(., 10, abs_log2FoldChange)

# will need to get the list of genes then plotCounts(), looping over the the list
# creating a data frame for ggplot to facet plot them all at once 
num_transcripts <- length(UV_filtered_vs_transmitted_ann_strict_5$transcript_id)
count_data <- NULL
final_counts_UV_filtered_vs_transmitted <- NULL

for(g in 1:num_transcripts) {
  
  one_transcript <- UV_filtered_vs_transmitted_ann_strict_5$transcript_id[g]
  
  # returnData = true returns data frame so it can be plotted with ggplot2, etc.
  count_data <- plotCounts(dds, gene=one_transcript, intgroup=c("site", "UV"), 
                           returnData=TRUE)
  count_data$transcript_id <- rep(one_transcript)
  
  # convert rownames to column and group by treatments
  count_data <- count_data %>% tibble::rownames_to_column(., var = "sample") 
  final_counts_UV_filtered_vs_transmitted <- rbind(final_counts_UV_filtered_vs_transmitted, count_data)
}
final_counts_UV_filtered_vs_transmitted

# log transform the counts
final_counts_UV_filtered_vs_transmitted <- dplyr::mutate(final_counts_UV_filtered_vs_transmitted, log2_count = log2(count))

# add gene names
final_counts_UV_filtered_vs_transmitted <- left_join(final_counts_UV_filtered_vs_transmitted, func_annot, by = "transcript_id", keep.both)

# final_counts_UV_filtered_vs_transmitted$UV <- ordered(final_counts_UV_filtered_vs_transmitted$UV, levels = c("filtered", "transmitted"))
# 
# 
# # t test, paired by site
# final_counts_UV_filtered_vs_transmitted %>% group_by(transcript_id) %>%
#   rstatix::pairwise_wilcox_test(data = ., log2_count ~ UV, paired = TRUE, p.adjust.method = "BH") -> stat.test.top10
# 
# stat.test.top10
# 
# # # A tibble: 10 x 10
# # transcript_id .y.        group1   group2         n1    n2 statistic     p p.adj p.adj.signif
# # * <chr>         <chr>      <chr>    <chr>       <int> <int>     <dbl> <dbl> <dbl> <chr>       
# #   1 Sc_g01390     log2_count filtered transmitted     6     6        21 0.031 0.031 *           
# #   2 Sc_g05612     log2_count filtered transmitted     6     6        21 0.031 0.031 *           
# #   3 Sc_g06438     log2_count filtered transmitted     6     6        21 0.031 0.031 *           
# #   4 Sc_g07907     log2_count filtered transmitted     6     6        20 0.062 0.062 ns          
# # 5 Sc_g07909     log2_count filtered transmitted     6     6        20 0.062 0.062 ns          
# # 6 Sc_g08662     log2_count filtered transmitted     6     6        21 0.031 0.031 *           
# #   7 Sc_g11598     log2_count filtered transmitted     6     6         0 0.031 0.031 *           
# #   8 Sc_g13420     log2_count filtered transmitted     6     6         0 0.031 0.031 *           
# #   9 Sc_g13500     log2_count filtered transmitted     6     6        21 0.031 0.031 *           
# #   10 Sc_g15405     log2_count filtered transmitted     6     6        21 0.031 0.031 *           


# mean of each treatment across sites 
summary.final_counts_UV_filtered_vs_transmitted <- final_counts_UV_filtered_vs_transmitted %>% 
  dplyr::select (transcript_id, site, UV, log2_count) %>% group_by(transcript_id, UV) %>% 
  dplyr::summarise(sd = sd(log2_count), mean = mean(log2_count), n = n())

summary.final_counts_UV_filtered_vs_transmitted

# # A tibble: 20 x 5
# # Groups:   transcript_id [10]
# transcript_id UV             sd  mean     n
# <chr>         <ord>       <dbl> <dbl> <int>
#   1 Sc_g01390     filtered    0.724  7.66     6
# 2 Sc_g01390     transmitted 0.847  6.18     6
# 3 Sc_g05612     filtered    0.696 10.9      6
# 4 Sc_g05612     transmitted 0.557 10.1      6
# 5 Sc_g06438     filtered    0.592  8.83     6
# 6 Sc_g06438     transmitted 0.712  7.63     6
# 7 Sc_g07907     filtered    0.623 14.0      6
# 8 Sc_g07907     transmitted 0.780 12.9      6
# 9 Sc_g07909     filtered    1.16   9.22     6
# 10 Sc_g07909     transmitted 0.819  8.04     6
# 11 Sc_g08662     filtered    0.521  8.78     6
# 12 Sc_g08662     transmitted 0.506  8.03     6
# 13 Sc_g11598     filtered    0.659  7.31     6
# 14 Sc_g11598     transmitted 0.353  8.23     6
# 15 Sc_g13420     filtered    0.394  8.04     6
# 16 Sc_g13420     transmitted 0.374  8.81     6
# 17 Sc_g13500     filtered    0.707 11.7      6
# 18 Sc_g13500     transmitted 0.961 10.9      6
# 19 Sc_g15405     filtered    1.02   9.03     6
# 20 Sc_g15405     transmitted 1.04   8.13     6
# 
# 
# # add y-value position to stat.test tibble for plotting
# stat.test.top10 <- stat.test.top10 %>% mutate(y.position = c(16, 16, 16, 16, 16, 16, 16, 16, 16, 16))


ggpubr::ggboxplot(final_counts_UV_filtered_vs_transmitted, 
                  x = "UV", 
                  y = "log2_count", 
                  fill = "UV",
                  color = "gray30",
                  size = 0.5,
                  point.size = 0.75,
                  outlier.size = 0.25,
                  id = "site",
                  line.color = "#4D4D4D66", 
                  facet.by = "transcript_id",
                  scales = "free_x", 
                  panel.labs.background = list(color = "black", fill = "black"),
                  panel.labs.font = list(size=8),
                  width = 0.7,
                  outlier.size = 1) +
                 facet_wrap(~ transcript_id, ncol = 10, scales = "free_x") +
  scale_y_continuous(position = "left", limits = c(0,15)) +
  scale_fill_manual(values=c(filtered = UV.filtered, transmitted = UV.transmitted),
                     name="UV Treatment", labels=c("filtered" = "UV-Filtered", "transmitted" = "UV-Transmitted")) +
  # ggpubr::stat_pvalue_manual(stat.test.top10, label = "p.adj.signif", remove.bracket = F, hide.ns = T, position = position_dodge(0.8)) +
  theme_light() + 
  theme(text = element_text(size = 11),
        legend.position="bottom",
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        strip.text.y = element_text(face = "italic"),
        axis.title.y=element_text(size = 12),
        strip.background = element_rect(fill = "black"),
        axis.ticks.x = element_blank(),) +
  scale_x_discrete(labels=c("filtered" = "UV-Filtered", "transmitted" = "UV-Transmitted")) +
  labs(x=NULL, y = "Log2 Transformed Normalized Transcript Count\n") +
  ggtitle("Top 10 Transcripts \n") -> ggbox

ggbox 

ggsave("ggbox_field.pdf", ggbox,, width = 6, height = 3, units = "in", scale = 1.3)







