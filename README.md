# syntrichia_field_UV
data and code for the paper: https://academic.oup.com/jxb/advance-article-abstract/doi/10.1093/jxb/erab051/6141413


 * the script `data_loggers/hobo_data/field/temp_light.R` processes the temperature and light from feb-june 2019 in uv-filtering and uv-transmitting windows 

 * the script `data_loggers/ibutton_data/field/windows-ibuttons.R` processses the 2- and 3-day temperature and relative humidity from uv-filtering windows, uv-transmitting windows, and site reference microsites. 

 * the script `data_loggers/light_quality.R` processes the point measurements of PAR and UV-A/B under uv-filtering windows, uv-transmitting windows, and site reference microsites. 

 * the script `pigments_tocopherols/pigments_t-test.R` processes pigments and tocopherol data for all field treatments and lab cultures. 
 
 * the script `fluorescence/fluorescence.R` processes a subset of the full fluorescence data (only Fv/Fm, Fo, Fm, and PHI PSII) from field and lab experiments. 
  
 * the script `transcriptomics/bowtie2_index_build_caninervis_genome.sh` builds a bowtie2 index of the S. caninervis reference genome (Silva et al. 2020, To dry perchance to live: insights from the genome of the desiccation-tolerant biocrust moss Syntrichia caninervis, The Plant Journal). 

 * the script `transcriptomics/clean_rna_field.sh` trims and quality-filters raw RNAseq data. 
  
 * the script `transcriptomics/tophat_paired-end_field.sh` assembles RNAseq data into transcriptomes, using the S. caninervis reference genome. 

 * the script `transcriptomics/htseq-count_field.sh` estimates normalized counts of each gene in each sample. 

 * the script `transcriptomics/field_transcriptomics.R` performs differential abundance ("expression") analyses and makes transcriptome-related figures. 

 * the script `transcriptomics/test_OGs_DPs_ELIPs_LEAs.R` checks for ELIPs and LEAs in the differentially abundant transcript sets. 

some of these scripts may call upon other accessory scripts, many of which i've also included in this this repository. 
