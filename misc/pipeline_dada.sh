cyanobacteria notes



for file in $(ls /mnt/home/smithsch/Lakes/2018/1-*_L001_R1_001.fastq)
do \

directory=$(dirname "$file") 
sample="$(basename -- $file)"
sample=$(echo "$sample" | cut -f 1 -d '.')
sample=$(echo "$sample" | sed s/'_L001_R1_001'//g)
sample=$(echo "$sample" | cut -f 1 -d '_')

mikdir -p ${directory}/../assembled/../error_files/../clustered/../finalized_reads/../classified

path <- "./2018"
fnFs <- sort(list.files(path, pattern="_R1_001.fastq", full.names = TRUE))
fnRs <- sort(list.files(path, pattern="_R2_001.fastq", full.names = TRUE))
sample.names <- sapply(strsplit(basename(fnFs), "_"), `[`, 1)
filtFs <- file.path(path, "filtered", paste0(sample.names, "_F_filt.fastq.gz"))
filtRs <- file.path(path, "filtered", paste0(sample.names, "_R_filt.fastq.gz"))
names(filtFs) <- sample.names
names(filtRs) <- sample.names
out <- filterAndTrim(fnFs, filtFs, fnRs, filtRs, truncLen=c(240,160),
              maxN=0, maxEE=c(2,2), truncQ=2, rm.phix=TRUE,
              compress=TRUE, multithread=10) 
errF <- learnErrors(filtFs, multithread=10)
errR <- learnErrors(filtRs, multithread=10)
dadaFs <- dada(filtFs, err=errF, multithread=10)
dadaRs <- dada(filtRs, err=errR, multithread=10)

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