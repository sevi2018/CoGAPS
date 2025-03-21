
###################Prepare input data
#######################################
setwd("~/DBDorsey/new_placebo_samples/counts/TMD")
files=list.files(path="~/DBDorsey/new_placebo_samples/counts/TMD",pattern="Aligned.out.sorted_by_name.exon.counts", all.files = T, full.names=F)
#data2=lapply(files, read.table, header=TRUE)
s = list()
for (i in 1:length(files)){
  data<-read.table(files[i],header=FALSE)
  data$name=files[i]
  h1=as.data.frame(do.call(rbind, strsplit(data$name, "_")))
  h2=h1[c(1)]
  h2$name=gsub("Aligned.out.sorted","",h2$V1)
  data1=cbind(data,h2)
  colnames(data1)[2]=data1[1,5]
  dt=data1[c(1,2)]
  s[[i]]=dt
}
datas=do.call(cbind,s)
vv=seq(3,350,2)
dat=datas[-c(vv)]

############################################################
###Add symbol to genes and select only protein coding genes
############################################################
gg=read.table('~/DBDorsey/race_placebo/new_method/adj_pain_score/mart_export(1).txt',1,sep='\t')
colnames(dat)[1]='Gene.stable.ID'
dat1=merge(dat,gg,by='Gene.stable.ID')
dat2=dat1[which(dat1$Gene.type=="protein_coding"),]
dat3=dat2[!duplicated(dat2$Gene.stable.ID),]
dat4=dat3[!duplicated(dat3$Gene.name),]
row.names(dat4)=dat4$Gene.name
dat5=dat4[-c(1,177:182)]
names=data.frame("ID"=c(colnames(dat5)))
names$ord=seq(1:nrow(names))
log=log(dat5+1) ####Input counts normalized
###  19341 genes (rows) and 175 patients (columns )
#############################################################
######Run CoGAPS#############################################
#############################################################
library(CoGAPS)

###Run the example data to calibrate 
data('modsimdata')
params <- new("CogapsParams", nPatterns=6)
cogapsresult <- CoGAPS(modsimdata, params, outputFrequency = 10000)
cogapsresult@sampleFactors
cogapsresult@featureLoadings

sink("CG_TMD.txt")
for(i in 10:20){
  params <- new("CogapsParams", nPatterns=i)
#  -- Standard Parameters --
  #    nPatterns            6 
  #  nIterations          50000 
  #  seed                 760 
  #sparseOptimization   FALSE 
  
  #-- Sparsity Parameters --
  #  alpha          0.01 
  #maxGibbsMass   100 
  # get the value for a specific parameter
  
  cogapsresult <- CoGAPS(log, params, outputFrequency = 10000)###We did not understand what outputFrequency means
  print(i)
  print(cogapsresult)
  print(patternMarkers(cogapsresult,threshold="all"))
  print(patternMarkers(cogapsresult,threshold="all",axis=2))
}
