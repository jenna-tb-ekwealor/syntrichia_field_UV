#!/bin/bash
# Job name:htseq-count-makeup
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

module load htseq/0.9.1
module load samtools/1.8

	for replicate in F*/; do
		replicatebname=$(basename "$replicate"); 
		for file in tophat_out_"$replicatebname"/aln.sorted.bam; do
		    echo 'the file is' "$file"
		    echo 'the replicate basename is' "$replicatebname"
	      	    htseq-count -f bam --stranded=no -m union "$file" /global/scratch/jbaughman/caninervis_genome_assembly/caninervis_genome.gtf > htseq-count_10142020/"$replicatebname".count	
		done
	done