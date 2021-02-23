#!/bin/bash
# Job name:
#SBATCH --job-name=tophat
#
# Account:
#SBATCH --account=fc_phylodiv
#
# Partition:
#SBATCH --partition=savio
#
# Quality of Service:
#SBATCH --qos=savio_normal
#
# Wall clock limit:
#SBATCH --time=72:00:00
#
## Command(s) to run:

export PATH=$PATH:/global/home/groups/fc_phylodiv/modules/
export PATH=$PATH:/global/home/groups/fc_phylodiv/modfiles/

# run from "clean" directory

module load bowtie2

for sample in F*/; do
 	samplebname=$(basename "$sample")
	mkdir tophat_out_"$samplebname"
	echo "the sample is" "$sample"
	echo "the basename is" "$samplebname"
	echo "the forward read is" "$sample"*_1_paired.fastq
	echo "the forward read is" "$sample"*_2_paired.fastq
	/global/home/groups/fc_phylodiv/modules/tophat/tophat-2.1.1.Linux_x86_64/tophat -r 275 -o tophat_out_"$samplebname" -p 14 --no-coverage-search --transcriptome-index /global/scratch/jbaughman/caninervis_genome_assembly/bowtie_index/s_caninervis/transcriptome_index /global/scratch/jbaughman/caninervis_genome_assembly/bowtie_index/s_caninervis "$sample"*_1_paired.fastq "$sample"*_2_paired.fastq
 done