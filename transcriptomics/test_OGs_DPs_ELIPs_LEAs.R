library("dplyr")

setwd("/Users/jennaekwealor/Box Sync/dissertation/project-field/transcriptomics/")


UV <- read.csv(file = "UV_filtered_vs_transmitted_ann.csv") %>% select(transcript_id:padj)

elip <- read.csv(file = "caninervis_elips.csv")
lea <- read.csv(file = "caninervis_leas.csv")


#### check if elips in each of above ####
UV_elip <- UV %>% filter(transcript_id %in% elip$transcript_id)
length(unique(UV$transcript_id))
# [1] 19
length(unique(UV_elip$transcript_id))
# [1] 0


#### check if leas in each of above ####
UV_lea <- UV %>% filter(transcript_id %in% lea$transcript_id)
length(unique(UV$transcript_id))
# [1] 19
length(unique(UV_lea$transcript_id))
# [1] 0
