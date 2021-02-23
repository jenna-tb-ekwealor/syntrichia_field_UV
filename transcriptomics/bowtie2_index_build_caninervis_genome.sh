#!/bin/bash
# Job name:
#SBATCH --job-name=bowtie2_index

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
#SBATCH --time=11:00:00
#
## Command(s) to run:

module load bowtie2

bowtie2-build s_caninervis.fa s_caninervis