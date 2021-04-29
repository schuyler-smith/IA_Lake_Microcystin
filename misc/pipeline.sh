cyanobacteria notes



for file in $(ls /mnt/home/smithsch/Lakes/2018/1-*_L001_R1_001.fastq)
do \

directory=$(dirname "$file") 
sample="$(basename -- $file)"
sample=$(echo "$sample" | cut -f 1 -d '.')
sample=$(echo "$sample" | sed s/'_L001_R1_001'//g)
sample=$(echo "$sample" | cut -f 1 -d '_')

mikdir -p ${directory}/../assembled/../error_files/../clustered/../finalized_reads/../classified

pear \
-j 8 \
-p 0.05 \
-v 10 \
-n 225 \
-m 290 \
-f ${file} \
-r ${directory}/${sample}*_R2_* \
-o ${directory}/../assembled/${sample}

cutadapt \
-l 225 \
-m 225 \
-o ${directory}/../assembled/${sample}.assembled.trimmed.fastq \
${directory}/../assembled/${sample}.assembled.fastq

amplici \
--fastq ${directory}/../assembled/${sample}.assembled.trimmed.fastq \
--outfile ${directory}/../error_files/${sample}.error \
-error

amplici \
--fastq ${directory}/../assembled/${sample}.assembled.trimmed.fastq \
--profile ${directory}/../error_files/${sample}.error \
--outfile ${directory}/../clustered/${sample}.fasta

/mnt/home/smithsch/software/vsearch-2.15.1/bin/vsearch \
--sortbysize ${directory}/../clustered/${sample}.fasta \
--output ${directory}/../clustered/${sample}.sorted.fasta

/mnt/home/smithsch/software/vsearch-2.15.1/bin/vsearch \
--uchime_denovo ${directory}/../clustered/${sample}.sorted.fasta \
--nonchimeras ${directory}/../finalized_reads/${sample}.nonchimeras.fasta

java -Xmx24g -jar /mnt/home/smithsch/software/RDPTools/classifier.jar \
classify \
-c 0.5 \
-f filterbyconf \
-o ${directory}/../classified/${sample}_classifications.txt \
${directory}/../finalized_reads/${sample}.nonchimeras.fasta


done



rsync -vtrh smithsch@gateway.hpcc.msu.edu:/mnt/home/smithsch/Lakes /media/schuyler/work_HD/Lake_16s/


16S rRNA gene (copies/mL)	Microcystis mcyA gene (copies/mL)	Aanabaena mcyA gene (copies/mL)	Planktothrix mcyA gene (copies/mL)


for file in $(ls ./*)
do \
sample="$(basename -- $file)"
mv $file 18-${sample}
done