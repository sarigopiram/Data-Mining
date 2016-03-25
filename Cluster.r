library(Hmisc)


setwd("D:\\HCUP Project")
hcupData <- read.csv(file="masterdataset_sample.csv", header=T, sep=",", row.names=1)
names(hcupData)

hcupData$dispuniform[hcupData$dispuniform<0]<-NA
hcupData$dispuniform=factor(hcupData$dispuniform)
test<-names(which.max(table(hcupData$dispuniform)))
hcupData$dispuniform[is.na(hcupData$dispuniform)]<-test
unique(hcupData$dispuniform)

dummy<-model.matrix(~dispuniform -1,data=hcupData)
head(joined)
joined = cbind(hcupData[match(rownames(dummy), rownames(hcupData)),], dummy)


hcupData<-subset(joined, select=-c(dispuniform))
dim(hcupData)

hcupData$age[hcupData$age<0]<-NA
summary(hcupData$age)
hcupData$age[is.na(hcupData$age)]<-48.72
hcupData$age<-as.integer(hcupData$age)
unique(hcupData$age)


#age_neonate -9 are missing values
hcupData$age_neonate[hcupData$age_neonate<0]<-NA
unique(hcupData$age_neonate)
#factorize neonate
#hcupData$age_neonate=factor(hcupData$age_neonate)
str(hcupData$age_neonate)
hcupData$age_neonate[is.na(hcupData$age_neonate)]<-0

hcupData$died[hcupData$died<0]<-NA
#hcupData$died=factor(hcupData$died)
test<-names(which.max(table(hcupData$died)))
hcupData$died[is.na(hcupData$died)]<-test
hcupData$died<-as.integer(hcupData$died)
unique(hcupData$died)


#2 levels - 0 and 1
hcupData$elective[hcupData$elective<0]=NA
test<-names(which.max(table(hcupData$elective)))
hcupData$elective[is.na(hcupData$elective)]<-test
hcupData$elective<-as.integer(hcupData$elective)
unique(hcupData$elective)



hcupData$hospbrth[hcupData$hospbrth<0]=NA
unique(hcupData$hospbrth)


#los is a continuous value, has to be factorozed
hcupData$los[hcupData$los<0]=NA
#impute los to mean
hcupData$los<-impute(hcupData$los,mean)
hcupData$los<-as.integer(hcupData$los)

hcupData$nchronic<-impute(hcupData$nchronic,mean)
hcupData$nchronic<-as.integer(hcupData$nchronic)
unique(hcupData$nchronic)


#discretize ndx- the number of diagnosis
unique(hcupData$ndx)
hcupData$ndx<-impute(hcupData$ndx,mean)
hcupData$ndx<-as.integer(hcupData$ndx)
unique(hcupData$ndx)



unique(hcupData$necode)
hcupData$necode<-impute(hcupData$necode,mean)
unique(hcupData$necode)


hcupData$neomat[hcupData$neomat<0]=NA
hcupData$neomat=factor(hcupData$neomat)
unique(hcupData$neomat)


unique(hcupData$npr)
hcupData$npr<-impute(hcupData$npr,mean)
hcupData$npr<-as.integer(hcupData$npr)
unique(hcupData$npr)


hcupData$orproc[hcupData$orproc<0]=NA
unique(hcupData$orproc)



hcupData$prday1<-impute(hcupData$prday1,mean)
hcupData$prday1<-as.integer(hcupData$prday1)
unique(hcupData$prday1)


#totcharges, values <0 is considered missing
hcupData$totchg[hcupData$totchg<0]=NA
hcupData$totchg<-impute(hcupData$totchg,mean)

summary(hcupData$totchg)

hcupData$days_from_admission[hcupData$days_from_admission<0]=NA
#impute los to mean
hcupData$days_from_admission<-impute(hcupData$days_from_admission,mean)
hcupData$days_from_admission<-as.integer(hcupData$days_from_admission)


hcupData$female[hcupData$female<0]=NA
test<-names(which.max(table(hcupData$female)))
hcupData$female[is.na(hcupData$female)]<-test
hcupData$female<-as.integer(hcupData$female)
unique(hcupData$female)


str(hcupData)

subsetVector=c(hcupData$age,hcupData$age_neonate,hcupData$female,hcupData$died,hcupData$elective,hcupData$hospbrth,hcupData$los,hcupData$nchronic,hcupData$ndx,hcupData$necode,hcupData$npr,hcupData$orproc,hcupData$prday1,hcupData$totchg,hcupData$days_from_admission,hcupData$dispuniform1,hcupData$dispuniform2,hcupData$dispuniform5,hcupData$dispuniform6,hcupData$dispuniform7,hcupData$dispuniform20,hcupData$dispuniform99)

new<-subset(hcupData,select=c(age,age_neonate,female,died,elective,hospbrth,los,nchronic,ndx,necode,npr,orproc,prday1,totchg,days_from_admission,dispuniform1,dispuniform2,dispuniform5,dispuniform6,dispuniform7,dispuniform20,dispuniform99))
str(new)


wss <- (nrow(new)-1)*sum(apply(new,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(new, 
                                     centers=i,iter.max=1000,algorithm="MacQueen")$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

k<-kmeans(new, centers=5,iter.max=1000,algorithm="MacQueen")
k$size
k$centers
k$withinss
summary(k)

aggregate(new,by=list(cluster=k$cluster), FUN=mean)
library(RColorBrewer)
library(rgl)
#plot(new$dispuniform1,new$totchg, col=k$clust)


clusternames<-unique(k$cluster)
names(clusternames)<-c("HIGH RISK","ROUTINE","CRITICAL","LESS CRITICAL", "VERY HIGH RISK")

layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow = TRUE))
plot(new$dispuniform1,new$totchg, col=k$clust,pch=8,ylab="TOTAL Charge", xlab="ROUTINE")
legend('center', pch=c(8,8,8,8,8),col=clusternames,legend=names(clusternames))
plot(new$dispuniform2,new$totchg, col=k$clust,pch=8,ylab="", xlab="TRANSFER TO SHORT-TERM HOSPITAL")
legend('center', pch=c(8,8,8,8,8),col=clusternames,legend=names(clusternames))
plot(new$dispuniform5,new$totchg, col=k$clust,pch=8,ylab="", xlab="OTHER TRANSFERS")
legend('center', pch=c(8,8,8,8,8),col=clusternames,legend=names(clusternames))
plot(new$dispuniform6,new$totchg, col=k$clust,pch=8,ylab="TOTAL CHARGE", xlab="HOME HEALTH CARE")
legend('center', pch=c(8,8,8,8,8),col=clusternames,legend=names(clusternames))
plot(new$dispuniform7,new$totchg, col=k$clust,pch=8,ylab="", xlab="AGAINST MEDICAL ADVICE")
legend('center', pch=c(8,8,8,8,8),col=clusternames,legend=names(clusternames))
plot(new$dispuniform20,new$totchg, col=k$clust,pch=8,ylab="", xlab="DIED IN HOSPITAL")
legend('center', pch=c(8,8,8,8,8),col=clusternames,legend=names(clusternames))

plot(new$dispuniform99,new$totchg, col=k$clust,pch=8,ylab="", xlab="DESTINATION UNKNOWN")
legend('center', pch=c(8,8,8,8,8),col=clusternames,legend=names(clusternames))


plot3d(new$totchg,new$npr,new$ndx,pch=8, col=k$clust)
legend3d('right', pch=c(8,8,8,8,8),col=clusternames,legend=clusternames)

str(new)
require(MASS)


new$hosp_region<-hcupData$hosp_region
new$hosp_region<-factor(new$hosp_region)
dummy<-model.matrix(~hosp_region-1,data=new)
head(joined)
joined = cbind(new[match(rownames(dummy), rownames(new)),], dummy)
new<-subset(joined, select=-c(hosp_region))
names(new)

new$hosp_bedsize<-hcupData$hosp_bedsize
new$hosp_bedsize<-factor(new$hosp_bedsize)
dummy<-model.matrix(~hosp_bedsize-1,data=new)
head(joined)
joined = cbind(new[match(rownames(dummy), rownames(new)),], dummy)
new<-subset(joined, select=-c(hosp_bedsize))

new$hosp_locteach<-hcupData$hosp_locteach
new$hosp_locteach<-factor(new$hosp_locteach)
dummy<-model.matrix(~hosp_locteach-1,data=new)
head(joined)
joined = cbind(new[match(rownames(dummy), rownames(new)),], dummy)
new<-subset(joined, select=-c(hosp_locteach))


new$h_contrl<-hcupData$h_contrl
new$h_contrl<-factor(new$h_contrl)
dummy<-model.matrix(~h_contrl-1,data=new)
head(joined)
joined = cbind(new[match(rownames(dummy), rownames(new)),], dummy)
new<-subset(joined, select=-c(h_contrl))


new$mdc<-hcupData$mdc
new$mdc<-factor(new$mdc)
dummy<-model.matrix(~mdc-1,data=new)
head(joined)
joined = cbind(new[match(rownames(dummy), rownames(new)),], dummy)
new<-subset(joined, select=-c(mdc))

new$pay1<-hcupData$pay1
new$pay1<-factor(new$pay1)
dummy<-model.matrix(~pay1-1,data=new)
head(joined)
joined = cbind(new[match(rownames(dummy), rownames(new)),], dummy)
new<-subset(joined, select=-c(pay1))


names(new)

#testlda<-lda(k$cluster~(pay11+pay12+pay13+pay14+pay15+pay16+hosp_region1+hosp_region2+hosp_region3+hosp_region4+hosp_bedsize1+hosp_bedsize2+hosp_bedsize3+hosp_locteach1+hosp_locteach2+hosp_locteach3+h_contrl1+h_contrl2+h_contrl3+mdc1+mdc2+mdc3+mdc4+mdc5+mdc6+mdc7+mdc8+mdc9+mdc10+mdc11+mdc12+mdc13+mdc14+mdc15+mdc16+mdc17+mdc18+mdc19+mdc20+mdc21+mdc22+mdc23+mdc24+mdc25), data=new)

testlda<-lda(k$cluster~(mdc1+mdc2+mdc3+mdc4+mdc5+mdc6+mdc7+mdc8+mdc9+mdc10+mdc11+mdc12+mdc13+mdc14+mdc15+mdc16+mdc17+mdc18+mdc19+mdc20+mdc21+mdc22+mdc23+mdc24+mdc25), data=new)

require(ggplot2)
new$clust<-k$cluster
new$clust<-factor(new$clust)

require(MASS)

install.packages("devtools")
library(devtools)
install_github("fawda123/ggord")
library(ggord)
ggord(testlda, k$cluster, col='green')

testlda2<-lda(clust~(mdc1+mdc2+mdc3+mdc4+mdc5+mdc6+mdc7+mdc8+mdc9+mdc10+mdc11+mdc12+mdc13+mdc14+mdc15+mdc16+mdc17+mdc18+mdc19+mdc20+mdc21+mdc22+mdc23+mdc24+mdc25), data=new,prior = c(1,1,82,15,1)/100)

testlda2<-qda(clust~(age), data=new)


pred = predict(testlda2,new)

names(pred)


table(new$clust,pred$class)


summary(sc)
library(kernlab)

sc <- specc(new, centers=5)
plot(new, col=sc, pch=4)            # estimated classes (x)
points(new, col=obj$classes, pch=5) 