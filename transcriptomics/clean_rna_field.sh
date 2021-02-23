#!/bin/bash
# Job name:
#SBATCH --job-name=clean_rna
#
# Account:
#SBATCH --account=fc_phylodiv
#
# Partition:
#SBATCH --partition=savio2
#
# Quality of Service:
#SBATCH --qos=savio_normal
#
# Wall clock limit:
#SBATCH --time=40:00:00
#
## Command(s) to run:




# run trimmomatic to trim qual <30 off both ends, and sliding window <20 [Bolger et al., 2014]
# java -jar <path to trimmomatic.jar> PE [-threads <threads] [-phred33 | -phred64] [-trimlog <logFile>] <input 1> <input 2> <paired output 1> <unpaired output 1> <paired output 2> <unpaired output 2> <step 1> ...
# [-threads <threads]

module load java

mkdir clean
for sample in F*/; do
    samplebname=$(basename "$sample")
	java -jar /global/home/groups/fc_phylodiv/scripts/trimmomatic-0.30.jar PE -phred33 \
	"$sample"*_1.fq.gz "$sample"*_2.fq.gz \
	clean/"$samplebname"/"$samplebname"_1_paired.fastq \
	clean/"$samplebname"/"$samplebname"_1_unpaired.fastq \
	clean/"$samplebname"/"$samplebname"_2_paired.fastq \
        clean/"$samplebname"/"$samplebname"_2_unpaired.fastq \
   LEADING:3 TRAILING:3 SLIDINGWINDOW:4:20 MINLEN:20
    done