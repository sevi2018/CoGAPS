#packages and libraries
library(CoGAPS)
install.packages('inflection')
library('inflection')
library(readxl)
library(corrplot)
library(caret)

gaps<-readRDS('~/DBDorsey/Aishwarya/NMF/CoGAPS/CoGAPS counts/cgresult_5.rds')
cogaps_res_df <- data.frame(gaps@sampleFactors)
cogaps_res_df$ID = row.names(cogaps_res_df)

path <- "~/DBDorsey/Aishwarya/NMF/clinical_variables.xlsx" #enter your path here
cv <- read_excel(path)

##Select only TMD 
rows=grep("TMD",cv$ID)
tmd=cv[rows,]

####Merge patterns to clinical variables
merged_res <- merge(cogaps_res_df, tmd, by="ID")
dim(merged_res)
#############
###Check immune data
full_dataset = read.csv("~/DBDorsey/Aishwarya/NMF/Clinico_demo_Genomics_390ids.csv")
nmf_data1 = full_dataset[c(2,3,5,22:24,4,32:53,89:94)]
nearZeroVar(nmf_data1)
data1 = nmf_data1[-c(15,17, 22, 24, 27, 28, 32)]
rows=grep("TMD",data1$ID)
imm=data1[rows,]
####Merge patterns + clinical variables to immune data
imm_clin <- merge(merged_res, imm, by="ID")
write.csv(imm_clin,"~/DBDorsey/Aishwarya/resullts/dataset032325.csv")
corr_cg <- cor(imm_clin[-c(1,15)],use="complete.obs",method="spearman")
pdf("~/DBDorsey/Aishwarya/resullts/correlation_patterns_metadata.pdf")
corrplot(corr_cg, type='upper',tl.cex=0.6,tl.col='black', tl.srt=90,number.cex = 0.6,cl.cex = 0.6)
dev.off()


