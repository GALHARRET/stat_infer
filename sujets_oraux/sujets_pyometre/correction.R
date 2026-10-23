# data<-read.csv("donnees.csv",sep=";",dec=",",header=T)
# data$pathologieconcomitante[data$pathologieconcomitante=="aucun"]<-"absence"
# data$pathologieconcomitante[data$pathologieconcomitante!="absence"]<-"presence"
# data$PBcardiaque<-ifelse(data$auscultationcardiaque=="absence","absence","presence")
# for(j in c("type","eg","pathologieconcomitante",
#             "tbdigestif","pupd","PBcardiaque",
#             "muqueuse","palpation_abdo","ecoulement_vulvaire",
#            "liquide","epanchement","traitement")){
#       data[,j]<-as.factor(data[,j])
#   }
# summary(data)

# data<-data[,-11]
# data$SIRS<-as.numeric(data$temp<36|data$temp>38)+as.numeric(data$fc>90)+as.numeric(data$fr>20) 

# save(data,file="data.RData")

# load("data.RData")

# summary(data)

load("pyometre_simule.RData")
colnames(donnees)

donnees$nlr<-donnees$gn/donnees$globuleblanc
donnees$plr<-donnees$plaquette/donnees$globuleblanc
donnees$mlr<-donnees$monocyte/donnees$globuleblanc
donnees$ssi<-donnees$gn*donnees$plaquette/donnees$globuleblanc
donnees$elr<-donnees$ge/donnees$globuleblanc



obs<-table(donnees$type,donnees$pathologie_concomitante)
obs
fisher.test(obs)

obs<-table(donnees$type,donnees$eg)
obs
fisher.test(obs)

obs<-table(donnees$type,donnees$tbdigestif)
obs
fisher.test(obs)

obs<-table(donnees$type,donnees$pupd)
obs
fisher.test(obs)

obs<-table(donnees$type,donnees$muqueuse)
obs
fisher.test(obs)

obs<-table(donnees$type,donnees$sirs)
obs
fisher.test(obs)

obs<-table(donnees$sirs,donnees$pathologie_concomitante)
obs
fisher.test(obs)

obs<-table(donnees$sirs,donnees$eg)
obs
chisq.test(obs)

obs<-table(donnees$sirs,donnees$tbdigestif)
obs
fisher.test(obs)

obs<-table(donnees$sirs,donnees$pupd)
obs
fisher.test(obs)

obs<-table(donnees$sirs,donnees$muqueuse)
obs
fisher.test(obs)

obs<-table(donnees$pupd,donnees$pathologie_concomitante)
obs
fisher.test(obs)

obs<-table(donnees$pupd,donnees$eg)
obs
chisq.test(obs)

obs<-table(donnees$pupd,donnees$tbdigestif)
obs
fisher.test(obs)


obs<-table(donnees$pupd,donnees$muqueuse)
obs
fisher.test(obs)


cor.test(donnees$albumine,donnees$globuline)
cor.test(donnees$lymphocyte,donnees$uree)
cor.test(donnees$uree,donnees$creat)

res<-lm(albumine~sirs,data=donnees)
car::Anova(res,type="III")
