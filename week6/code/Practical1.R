rm(list = ls())

setwd("/home/frcovell/CMEECourseWork/week6/code/")
df <- read.csv("../data/bears(1).csv", stringsAsFactors = F, header = F, colClasses = rep("character", 10000))


#### identifying SNPs ####
SNPs <- c()
for (i in 1:ncol(df)) {
  if(length(unique(df[,i]))==2){
    SNPs <- c(SNPs, i)
  }
}



#### Allele Frequency  ####
#sum(which(df[,SNPs] == "C"))/sum(which(df == "C"))
#sum(which(df[,SNPs] == "G"))/sum(which(df == "G"))
#sum(which(df[,SNPs] == "A"))/sum(which(df == "A"))
#sum(which(df[,SNPs] == "T"))/sum(which(df == "T"))


df <- df[,SNPs]

Alleles <- unique(df[,1])

freq_a1 <- length(which(df[,1] == Alleles[1]))/nrow(df)
freq_a2 <- length(which(df[,1] == Alleles[2]))/nrow(df)

minor_allele <- Alleles[which.min(c(freq_a1,freq_a2))]
freq_minor_allele <- c(freq_a1, freq_a2)[which.min(c(freq_a1,freq_a2))]



frequencies <- c()
for (i in 1:ncol(df)) {
  
  Alleles <- sort(unique(df[,i]))
  cat("\nSNP", i, "with alleles", Alleles)
  
  
  freq_a1 <- length(which(df[,i] == Alleles[1]))/nrow(df)
  freq_a2 <- length(which(df[,i] == Alleles[2]))/nrow(df)
  
  minor_allele <- Alleles[which.min(c(freq_a1,freq_a2))]
  freq_minor_allele <- c(freq_a1, freq_a2)[which.min(c(freq_a1,freq_a2))]
  
  cat(" the minor allele is",minor_allele ,"and the minor allele frequency (MAF) is", freq_minor_allele)
  frequencies <- c(frequencies, freq_minor_allele)
}

frequencies

hist(frequencies)
plot(frequencies, type = "h")


#### Genotype frequency ####

#df <- read.csv("../data/bears(1).csv", stringsAsFactors = F, header = F, colClasses = rep("character", 10000))

frequencies <- c()
for (i in 1:ncol(df)) {
  
  Alleles <- sort(unique(df[,i]))
  cat("\nSNP", i, "with alleles", Alleles)
  
  
  freq_a1 <- length(which(df[,i] == Alleles[1]))/nrow(df)
  freq_a2 <- length(which(df[,i] == Alleles[2]))/nrow(df)
  
  minor_allele <- Alleles[which.min(c(freq_a1,freq_a2))]
  freq_minor_allele <- c(freq_a1, freq_a2)[which.min(c(freq_a1,freq_a2))]
  
  cat(" the minor allele is",minor_allele ,"and the minor allele frequency (MAF) is", freq_minor_allele)
  frequencies <- c(frequencies, freq_minor_allele)

genotype_count <- c(0, 0, 0)

nsamples <- 20
for (j in 1:nsamples) {
  
  haplotype_index <- c((j*2)-1, (j*2))


genotype <- length(which(df[haplotype_index, i] == minor_allele))

genotype_index=genotype+1

genotype_count[genotype_index] <- genotype_count[genotype_index]+1
}

}

Alleles

