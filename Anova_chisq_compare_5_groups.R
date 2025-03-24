data=read_csv("~/DBDorsey/Aishwarya/resullts/dataset032325.csv")
###Remove pattern columns because now we are considering patterns as separate groups of ids instead of continuos
data1=data[-c(1,3:7)]

pattern1=data.frame("ID"=c("TMD111","TMD163","TMD548","TMD366","TMD291","TMD324","TMD596","TMD356",
"TMD169","TMD384","TMD408","TMD137","TMD506","TMD550","TMD606","TMD584",
"TMD179","TMD600","TMD462","TMD444","TMD139","TMD328","TMD500","TMD314",
"TMD177","TMD404","TMD542","TMD496","TMD426","TMD304","TMD618","TMD442",
"TMD592","TMD620","TMD536","TMD143","TMD434","TMD364","TMD308","TMD113",
"TMD181","TMD406","TMD386","TMD138","TMD380","TMD518","TMD446","TMD310",
"TMD352","TMD141","TMD362","TMD185","TMD594","TMD322"))


pattern2=data.frame("ID"=c("TMD512","TMD129","TMD416","TMD382","TMD376","TMD544","TMD516","TMD175",
"TMD145","TMD580","TMD418","TMD578","TMD438","TMD522","TMD348","TMD556",
"TMD428","TMD520","TMD440","TMD131","TMD448","TMD374","TMD494","TMD254",
"TMD165","TMD410","TMD558","TMD414"))

pattern3=data.frame("ID"=c("TMD554","TMD576","TMD193","TMD488","TMD400","TMD197","TMD045","TMD460",
"TMD238","TMD622","TMD478","TMD508","TMD396","TMD456","TMD566","TMD612",
"TMD458","TMD604","TMD133","TMD498","TMD167","TMD156","TMD528","TMD534",
"TMD187","TMD510","TMD480"))

pattern4=data.frame("ID"=c("TMD312","TMD109","TMD117","TMD344","TMD590","TMD350","TMD135","TMD334",
"TMD127","TMD464","TMD392","TMD087","TMD342","TMD119","TMD155","TMD454",
"TMD115","TMD394","TMD452","TMD540","TMD552","TMD466","TMD338","TMD340",
"TMD378","TMD546","TMD199","TMD320","TMD346","TMD388","TMD360","TMD157",
"TMD562","TMD588","TMD125","TMD330","TMD474","TMD107","TMD538","TMD302",
"TMD358","TMD560","TMD171","TMD390"))

pattern5=data.frame("ID"=c("TMD173","TMD470","TMD570","TMD526","TMD436","TMD482","TMD586","TMD616",
"TMD402","TMD121","TMD398","TMD492","TMD572","TMD412","TMD183","TMD306",
"TMD484","TMD530","TMD490","TMD574","TMD504","TMD430"))


mtc1=merge(pattern1,data1,by='ID')
mtc2=merge(pattern2,data1,by='ID')
mtc3=merge(pattern3,data1,by='ID')
mtc4=merge(pattern4,data1,by='ID')
mtc5=merge(pattern5,data1,by='ID')
mtc1$group="pattern1"
mtc2$group="pattern2"
mtc3$group="pattern3"
mtc4$group="pattern4"
mtc5$group="pattern5"
data_an=rbind(mtc1,mtc2,mtc3,mtc4,mtc5)
#pattern1 pattern2 pattern3 pattern4 pattern5 
# 52       28       27       44       22
##Divide predictors into binary and numeric

###BINARY
bin=data_an[c(3,10:14)]
bin1=bin[c(2,1,3:6)]
##analyz difference by chisq test
sink("~/DBDorsey/Aishwarya/resullts/Chisq.txt")
for(i in 2:6){
  data=bin1[c(1,i)]
  data1=data[which(data[,2]!='NA'),]
  print(colnames(data1[c(2)]))
print(chisq.test(table(data1$group,data1[,2])))
}
sink()
##CONTINUOS
num=data_an[-c(3,11:14)]
num1=num[c(9,2:8,10:31)]
for
##analyz difference by groups using ANOVA
num1$group<- factor(num1$group,levels=c("pattern1","pattern2","pattern3","pattern4"))
columns=names(num1)[2:30]

sink("~/DBDorsey/Aishwarya/resullts/anova.txt")
for(i in seq_along(columns)){
  x=reformulate( "group",columns[i])
  y=aov(x,data=num1)
  print(columns[[i]])
  print(summary(y))
  print(TukeyHSD(y))
}

sink("~/DBDorsey/Aishwarya/resullts/mean.txt")
for(i in seq_along(columns)){
dat=num1[c(1,i)]
dat=dat[complete.cases(dat),]
  group=dat$group
  score=dat[,2]
  print(columns[[i]])
  print(avg_scores <- tapply(score, group, mean))
}
sink()

sink("~/DBDorsey/Aishwarya/resullts/sd.txt")
for(i in seq_along(columns)){
  dat=num1[c(1,i)]
  dat=dat[complete.cases(dat),]
  group=dat$group
  score=dat[,2]
  print(columns[[i]])
  print(avg_scores <- tapply(score, group, sd))
}
sink()
  