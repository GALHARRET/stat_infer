#data<-read.csv("sujets_oraux/sujets_pyometre/chiens.csv",sep=";",dec=",")
# data$pathologieconcomitante[data$pathologieconcomitante=="aucun"]<-"absence"
# data$pathologieconcomitante[data$pathologieconcomitante!="absence"]<-"presence"
# table(data$pathologieconcomitante)
# data$eg<-as.factor(data$eg)
# for(j in colnames(data)[c(2,3,4,7,10,12,13,15,16,17,19,20,23:24)]){
#     data[,j]<-as.factor(data[,j])
# }
# summary(data)

#save(data,file="sujets_oraux/sujets_pyometre/data.RData")
load("sujets_oraux/sujets_pyometre/data.RData")
summary(data)
obs<-table(data$pupd,data$ecoulement_vulvaire)
obs
fisher.test(obs)
obs<-table(data$pupd,data$palpation_abdo)
obs
fisher.test(obs)
obs<-table(data$pupd,data$auscultationcardiaque)
obs
fisher.test(obs)
obs<-table(data$pupd,data$tbdigestif)
obs
fisher.test(obs)
obs<-table(data$type,data$pupd)
obs
fisher.test(obs)
obs<-table(data$type,data$eg)
obs
fisher.test(obs)
obs<-table(data$type,data$tbdigestif)
obs
fisher.test(obs)
obs<-table(data$eg,data$tbdigestif)
obs
fisher.test(obs)


cor.test(data$albumine,data$globuline)
cor.test(data$lymphocyte,data$uree)
cor.test(data$uree,data$creat)
