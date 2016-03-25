library(mice)
library(VIM)
library(Hmisc)


setwd("D:\\HCUP Project")
hcupsample <- read.csv(file="masterdataset_sample.csv", header=T, sep=",", row.names=1)
str(hcupsample)

# for age, values -66, -99 are actually missing values
hcupsample$age[hcupsample$age<0]<-NA
hcupsample$age<-impute(hcupsample$age,mean)
hcupsample$age<-as.integer(hcupsample$age)
age_fact<-cut(hcupsample$age, breaks=c(0,1,5,10,20,35,50,65,80,90),include.lowest = TRUE)
hcupsample$age<-age_fact
unique(hcupsample$age)


#age_neonate -9 are missing values
hcupsample$age_neonate[hcupsample$age_neonate<0]<-NA
unique(hcupsample$age_neonate)

#factorize neonate
hcupsample$age_neonate=factor(hcupsample$age_neonate)
str(hcupsample$age_neonate)
hcupsample$age_neonate[is.na(hcupsample$age_neonate)]<-0



#Defning missing values and factorizing amonth
hcupsample$amonth[hcupsample$amonth<0]<-NA
hcupsample$amonth=factor(hcupsample$amonth)
test<-names(which.max(table(hcupsample$amonth)))
hcupsample$amonth[is.na(hcupsample$amonth)]<-test
unique(hcupsample$amonth)



#Defining missing values and factorizing aweekend
hcupsample$aweekend[hcupsample$aweekend<0]<-NA
hcupsample$aweekend=factor(hcupsample$aweekend)
test<-names(which.max(table(hcupsample$aweekend)))
hcupsample$aweekend[is.na(hcupsample$aweekend)]<-test
unique(hcupsample$aweekend)


#Defining missing values and factorizing died flag
unique(hcupsample$died)
hcupsample$died[hcupsample$died<0]<-NA
hcupsample$died=factor(hcupsample$died)
test<-names(which.max(table(hcupsample$died)))
hcupsample$died[is.na(hcupsample$died)]<-test
unique(hcupsample$died)


#Defining missing values and factorizing dispuniform
hcupsample$dispuniform[hcupsample$dispuniform<0]<-NA
hcupsample$dispuniform=factor(hcupsample$dispuniform)
test<-names(which.max(table(hcupsample$dispuniform)))
hcupsample$dispuniform[is.na(hcupsample$dispuniform)]<-test
unique(hcupsample$dispuniform)


hcupsample$dqtr[hcupsample$dqtr<0]=NA
hcupsample$dqtr=factor(hcupsample$dqtr)
test<-names(which.max(table(hcupsample$dqtr)))
hcupsample$dqtr[is.na(hcupsample$dqtr)]<-test
unique(hcupsample$dqtr)


#there are 999 levels for this variable
hcupsample$drg[hcupsample$drg<0]=NA
hcupsample$drg=factor(hcupsample$drg)
unique(hcupsample$drg)

#579 levels
hcupsample$drg24[hcupsample$drg24<0]=NA
hcupsample$drg24=factor(hcupsample$drg24)
unique(hcupsample$drg24)

#2 levels- 29 and 30
hcupsample$drgver[hcupsample$drgver<0]=NA
hcupsample$drgver=factor(hcupsample$drgver)
unique(hcupsample$drgver)

#99 levels
hcupsample$drg_nopoa[hcupsample$drg_nopoa<0]=NA
hcupsample$drg_nopoa=factor(hcupsample$drg_nopoa)
unique(hcupsample$drg_nopoa)

#3145 levels -This data is rolled up into hcc, so we can ignore
str(hcupsample$dx1)

#has 670 levels, same as diagnosis, but better rolled up based on 64 hcc groupings
hcupsample$dxccs1[hcupsample$dxccs1<0]=NA
hcupsample$dxccs1=factor(hcupsample$dxccs1)
unique(hcupsample$dxccs1)

#2 levels - 0 and 1
hcupsample$elective[hcupsample$elective<0]=NA
hcupsample$elective=factor(hcupsample$elective)
test<-names(which.max(table(hcupsample$elective)))
hcupsample$elective[is.na(hcupsample$elective)]<-test
unique(hcupsample$elective)


#2 leevels 0 and 1
hcupsample$female[hcupsample$female<0]=NA
hcupsample$female=factor(hcupsample$female)
test<-names(which.max(table(hcupsample$female)))
hcupsample$female[is.na(hcupsample$female)]<-test
unique(hcupsample$female)


hcupsample$hcup_ed[hcupsample$hcup_ed<0]=NA
hcupsample$hcup_ed=factor(hcupsample$hcup_ed)
unique(hcupsample$hcup_ed)

hcupsample$hospbrth[hcupsample$hospbrth<0]=NA
hcupsample$hospbrth=factor(hcupsample$hospbrth)
unique(hcupsample$hospbrth)


hcupsample$hosp_division[hcupsample$hosp_division<0]=NA
hcupsample$hosp_division=factor(hcupsample$hosp_division)
unique(hcupsample$hosp_division)

#hosp nis number
#need not be very significant in analysis, but can be used for viz
unique(hcupsample$hosp_nis)

#los is a continuous value, has to be factorozed
hcupsample$los[hcupsample$los<0]=NA
#impute los to mean
hcupsample$los<-impute(hcupsample$los,mean)
hcupsample$los<-as.integer(hcupsample$los)
los_fact<-cut(hcupsample$los, breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,366),include.lowest = TRUE, labels = c(1,2,3,4,5,6,7,8,9,10,11,12))
hcupsample$los<-los_fact

# next 3 variables have 25 levels
hcupsample$mdc[hcupsample$mdc<0]=NA
hcupsample$mdc=factor(hcupsample$mdc)
unique(hcupsample$mdc)

hcupsample$mdc24[hcupsample$mdc24<0]=NA
hcupsample$mdc24=factor(hcupsample$mdc24)
unique(hcupsample$mdc24)

hcupsample$mdc_nopoa[hcupsample$mdc_nopoa<0]=NA
hcupsample$mdc_nopoa=factor(hcupsample$mdc_nopoa)
unique(hcupsample$mdc_nopoa)

#discretize the nchronic variables
nchronic_fact<-cut(hcupsample$nchronic, breaks=c(0,5,10,15,20,25),include.lowest = TRUE, labels = c("0-5","6-10","11-15","16-20","21-25"))
test<-names(which.max(table(hcupsample$nchronic)))
hcupsample$nchronic[is.na(hcupsample$nchronic)]<-test
unique(hcupsample$nchronic)


#discretize ndx- the number of diagnosis
unique(hcupsample$ndx)
ndx_fact<-cut(hcupsample$ndx, breaks=seq(from=0, to=74, by=10),include.lowest = TRUE)
hcupsample$ndx<-ndx_fact
test<-names(which.max(table(hcupsample$ndx)))
hcupsample$ndx[is.na(hcupsample$ndx)]<-test
unique(hcupsample$ndx)



unique(necode_fact)
names(which.max(table(hcupsample$necode_fact)))
necode_fact<-cut(hcupsample$necode, breaks=c(0,5,10,15),include.lowest = TRUE)
names(which.max(table(hcupsample$necode_fact)))
hcupsample$necode<-necode_fact


hcupsample$neomat[hcupsample$neomat<0]=NA
hcupsample$neomat=factor(hcupsample$neomat)
unique(hcupsample$neomat)


#196 levels or stratum of hospital
hcupsample$nis_stratum[hcupsample$nis_stratum<0]=NA
hcupsample$nis_stratum=factor(hcupsample$nis_stratum)
unique(hcupsample$nis_stratum)

#discretize npr
unique(npr_fact)
npr_fact<-cut(hcupsample$npr, breaks=seq(from=0, to=50, by=5),include.lowest = TRUE)
hcupsample$npr<-npr_fact


hcupsample$orproc[hcupsample$orproc<0]=NA
hcupsample$orproc=factor(hcupsample$orproc)
unique(hcupsample$orproc)

hcupsample$pay1[hcupsample$pay1<0]=NA
hcupsample$pay1=factor(hcupsample$pay1)
test<-names(which.max(table(hcupsample$pay1)))
hcupsample$race[is.na(hcupsample$pay1)]<-test
unique(hcupsample$pay1)


hcupsample$pl_nchs2006[hcupsample$pl_nchs2006<0]=NA
hcupsample$pl_nchs2006=factor(hcupsample$pl_nchs2006)
test<-names(which.max(table(hcupsample$pl_nchs2006)))
hcupsample$pl_nchs2006[is.na(hcupsample$pl_nchs2006)]<-test
unique(hcupsample$pl_nchs2006)

# procedures, rolled up to ccs, can be ignored
str(hcupsample$pr1)
str(hcupsample$prccs1)

#discretize prday1
unique(hcupsample$prday1)
prday1_fact<-cut(hcupsample$prday1, breaks=c(0,15,30,45,60,90),include.lowest = TRUE, labels = c("0-15","15-30","30-1=45","46-60","61-90"))
test<-names(which.max(table(prday1_fact)))
hcupsample$prday1<-prday1_fact
hcupsample$prday1[is.na(hcupsample$prday1)]<-test

hcupsample$race[hcupsample$race<0]=NA
hcupsample$race=factor(hcupsample$race)
test<-names(which.max(table(hcupsample$race)))
hcupsample$race[is.na(hcupsample$race)]<-test
unique(hcupsample$race)

#totcharges, values <0 is considered missing
hcupsample$totchg[hcupsample$totchg<0]=NA
hcupsample$totchg<-impute(hcupsample$totchg,mean)
summary(hcupsample$totchg)
unique(totchg_fact)
hist(log(hcupsample$totchg))
totchg_fact<-cut(hcupsample$totchg, breaks=c(157,18563.5,27766.75,36970,2147485,4258000),include.lowest = TRUE,labels=c("very-low","low","medium","high","very-high"))
hcupData$totchg<-totchg_fact


hcupsample$tran_in[hcupsample$tran_in<0]=NA
hcupsample$tran_in=factor(hcupsample$tran_in)
test<-names(which.max(table(hcupsample$tran_in)))
hcupsample$tran_in[is.na(hcupsample$tran_in)]<-test
unique(hcupsample$tran_in)

hcupsample$tran_out[hcupsample$tran_out<0]=NA
hcupsample$tran_out=factor(hcupsample$tran_out)
test<-names(which.max(table(hcupsample$tran_out)))
hcupsample$tran_out[is.na(hcupsample$tran_out)]<-test
unique(hcupsample$tran_out)


#year can be ignored

hcupsample$zipinc_qrtl[hcupsample$zipinc_qrtl<0]=NA
hcupsample$zipinc_qrtl=factor(hcupsample$zipinc_qrtl)
test<-names(which.max(table(hcupsample$zipinc_qrtl)))
hcupsample$zipinc_qrtl[is.na(hcupsample$zipinc_qrtl)]<-test
unique(hcupsample$zipinc_qrtl)

#ignore dxmccs1 e_mccs1 prmccs1

unique(hcupsample$aprdrg)# retained as integer

hcupsample$aprdrg_risk_mortality[hcupsample$aprdrg_risk_mortality<0]=NA
hcupsample$aprdrg_risk_mortality=factor(hcupsample$aprdrg_risk_mortality)
unique(hcupsample$aprdrg_risk_mortality)



hcupsample$aprdrg_severity[hcupsample$aprdrg_severity<0]=NA
hcupsample$aprdrg_severity=factor(hcupsample$aprdrg_severity)
unique(hcupsample$aprdrg_severity)



hcupsample$hosp_bedsize[hcupsample$hosp_bedsize<0]=NA
hcupsample$hosp_bedsize=factor(hcupsample$hosp_bedsize)
unique(hcupsample$hosp_bedsize)



hcupsample$hosp_locteach[hcupsample$hosp_locteach<0]=NA
hcupsample$hosp_locteach=factor(hcupsample$hosp_locteach)
unique(hcupsample$hosp_locteach)



hcupsample$hosp_region[hcupsample$hosp_region<0]=NA
hcupsample$hosp_region=factor(hcupsample$hosp_region)
unique(hcupsample$hosp_region)



hcupsample$h_contrl[hcupsample$h_contrl<0]=NA
hcupsample$h_contrl=factor(hcupsample$h_contrl)
unique(hcupsample$h_contrl)


#retaining n_disc_u, n_hosp_u, s_disc_u, s_hosp_u, total_disc as int

unique(hcupsample$chronic_cond_indicator)
hcupsample$chronic_cond_indicator[hcupsample$chronic_cond_indicator<0]=NA
hcupsample$chronic_cond_indicator=factor(hcupsample$chronic_cond_indicator)
test<-names(which.max(table(hcupsample$chronic_cond_indicator)))
hcupsample$chronic_cond_indicator[is.na(hcupsample$chronic_cond_indicator)]<-test
unique(hcupsample$chronic_cond_indicator)


hcupsample$days_from_admission[hcupsample$days_from_admission<0]=NA
#impute los to mean
hcupsample$days_from_admission<-impute(hcupsample$days_from_admission,mean)
hcupsample$days_from_admission<-as.integer(hcupsample$days_from_admission)
days_from_admission_fact<-cut(hcupsample$days_from_admission, breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,366),include.lowest = TRUE, labels = c(1,2,3,4,5,6,7,8,9,10,11,12))
hcupsample$days_from_admission<-days_from_admission_fact

hcupsample$hcc_1[is.na(hcupsample$hcc_1)]<-0
hcupsample$hcc_1=factor(hcupsample$hcc_1)


hcupsample$hcc_10[is.na(hcupsample$hcc_10)]<-0
hcupsample$hcc_10=factor(hcupsample$hcc_10)



hcupsample$hcc_100[is.na(hcupsample$hcc_100)]<-0
hcupsample$hcc_100=factor(hcupsample$hcc_100)



hcupsample$hcc_103[is.na(hcupsample$hcc_103)]<-0
hcupsample$hcc_103=factor(hcupsample$hcc_103)



hcupsample$hcc_104[is.na(hcupsample$hcc_104)]<-0
hcupsample$hcc_104=factor(hcupsample$hcc_104)



hcupsample$hcc_106[is.na(hcupsample$hcc_106)]<-0
hcupsample$hcc_106=factor(hcupsample$hcc_106)



hcupsample$hcc_107[is.na(hcupsample$hcc_107)]<-0
hcupsample$hcc_107=factor(hcupsample$hcc_107)



hcupsample$hcc_108[is.na(hcupsample$hcc_108)]<-0
hcupsample$hcc_108=factor(hcupsample$hcc_108)



hcupsample$hcc_11[is.na(hcupsample$hcc_11)]<-0
hcupsample$hcc_11=factor(hcupsample$hcc_11)
unique(hcupsample$hcc_11)


hcupsample$hcc_110[is.na(hcupsample$hcc_110)]<-0
hcupsample$hcc_110=factor(hcupsample$hcc_110)



hcupsample$hcc_111[is.na(hcupsample$hcc_111)]<-0
hcupsample$hcc_111=factor(hcupsample$hcc_111)



hcupsample$hcc_112[is.na(hcupsample$hcc_112)]<-0
hcupsample$hcc_112=factor(hcupsample$hcc_112)



hcupsample$hcc_114[is.na(hcupsample$hcc_114)]<-0
hcupsample$hcc_114=factor(hcupsample$hcc_114)



hcupsample$hcc_115[is.na(hcupsample$hcc_115)]<-0
hcupsample$hcc_115=factor(hcupsample$hcc_115)



hcupsample$hcc_12[is.na(hcupsample$hcc_12)]<-0
hcupsample$hcc_12=factor(hcupsample$hcc_12)



hcupsample$hcc_122[is.na(hcupsample$hcc_122)]<-0
hcupsample$hcc_122=factor(hcupsample$hcc_122)



hcupsample$hcc_124[is.na(hcupsample$hcc_124)]<-0
hcupsample$hcc_124=factor(hcupsample$hcc_124)



hcupsample$hcc_134[is.na(hcupsample$hcc_134)]<-0
hcupsample$hcc_134=factor(hcupsample$hcc_134)



hcupsample$hcc_135[is.na(hcupsample$hcc_135)]<-0
hcupsample$hcc_135=factor(hcupsample$hcc_135)



hcupsample$hcc_136[is.na(hcupsample$hcc_136)]<-0
hcupsample$hcc_136=factor(hcupsample$hcc_136)



hcupsample$hcc_137[is.na(hcupsample$hcc_137)]<-0
hcupsample$hcc_137=factor(hcupsample$hcc_137)



hcupsample$hcc_157[is.na(hcupsample$hcc_157)]<-0
hcupsample$hcc_157=factor(hcupsample$hcc_157)



hcupsample$hcc_158[is.na(hcupsample$hcc_158)]<-0
hcupsample$hcc_158=factor(hcupsample$hcc_158)



hcupsample$hcc_161[is.na(hcupsample$hcc_161)]<-0
hcupsample$hcc_161=factor(hcupsample$hcc_161)



hcupsample$hcc_162[is.na(hcupsample$hcc_162)]<-0
hcupsample$hcc_162=factor(hcupsample$hcc_162)



hcupsample$hcc_166[is.na(hcupsample$hcc_166)]<-0
hcupsample$hcc_166=factor(hcupsample$hcc_166)



hcupsample$hcc_167[is.na(hcupsample$hcc_167)]<-0
hcupsample$hcc_167=factor(hcupsample$hcc_167)



hcupsample$hcc_169[is.na(hcupsample$hcc_169)]<-0
hcupsample$hcc_169=factor(hcupsample$hcc_169)



hcupsample$hcc_17[is.na(hcupsample$hcc_17)]<-0
hcupsample$hcc_17=factor(hcupsample$hcc_17)



hcupsample$hcc_170[is.na(hcupsample$hcc_170)]<-0
hcupsample$hcc_170=factor(hcupsample$hcc_170)



hcupsample$hcc_173[is.na(hcupsample$hcc_173)]<-0
hcupsample$hcc_173=factor(hcupsample$hcc_173)



hcupsample$hcc_176[is.na(hcupsample$hcc_176)]<-0
hcupsample$hcc_176=factor(hcupsample$hcc_176)



hcupsample$hcc_18[is.na(hcupsample$hcc_18)]<-0
hcupsample$hcc_18=factor(hcupsample$hcc_18)



hcupsample$hcc_186[is.na(hcupsample$hcc_186)]<-0
hcupsample$hcc_186=factor(hcupsample$hcc_186)



hcupsample$hcc_188[is.na(hcupsample$hcc_188)]<-0
hcupsample$hcc_188=factor(hcupsample$hcc_188)



hcupsample$hcc_189[is.na(hcupsample$hcc_189)]<-0
hcupsample$hcc_189=factor(hcupsample$hcc_189)



hcupsample$hcc_19[is.na(hcupsample$hcc_19)]<-0
hcupsample$hcc_19=factor(hcupsample$hcc_19)



hcupsample$hcc_2[is.na(hcupsample$hcc_2)]<-0
hcupsample$hcc_2=factor(hcupsample$hcc_2)



hcupsample$hcc_21[is.na(hcupsample$hcc_21)]<-0
hcupsample$hcc_21=factor(hcupsample$hcc_21)



hcupsample$hcc_22[is.na(hcupsample$hcc_22)]<-0
hcupsample$hcc_22=factor(hcupsample$hcc_22)



hcupsample$hcc_23[is.na(hcupsample$hcc_23)]<-0
hcupsample$hcc_23=factor(hcupsample$hcc_23)



hcupsample$hcc_27[is.na(hcupsample$hcc_27)]<-0
hcupsample$hcc_27=factor(hcupsample$hcc_27)



hcupsample$hcc_28[is.na(hcupsample$hcc_28)]<-0
hcupsample$hcc_28=factor(hcupsample$hcc_28)



hcupsample$hcc_29[is.na(hcupsample$hcc_29)]<-0
hcupsample$hcc_29=factor(hcupsample$hcc_29)



hcupsample$hcc_33[is.na(hcupsample$hcc_33)]<-0
hcupsample$hcc_33=factor(hcupsample$hcc_33)



hcupsample$hcc_34[is.na(hcupsample$hcc_34)]<-0
hcupsample$hcc_34=factor(hcupsample$hcc_34)



hcupsample$hcc_35[is.na(hcupsample$hcc_35)]<-0
hcupsample$hcc_35=factor(hcupsample$hcc_35)



hcupsample$hcc_39[is.na(hcupsample$hcc_39)]<-0
hcupsample$hcc_39=factor(hcupsample$hcc_39)



hcupsample$hcc_40[is.na(hcupsample$hcc_40)]<-0
hcupsample$hcc_40=factor(hcupsample$hcc_40)



hcupsample$hcc_46[is.na(hcupsample$hcc_46)]<-0
hcupsample$hcc_46=factor(hcupsample$hcc_46)



hcupsample$hcc_47[is.na(hcupsample$hcc_47)]<-0
hcupsample$hcc_47=factor(hcupsample$hcc_47)



hcupsample$hcc_48[is.na(hcupsample$hcc_48)]<-0
hcupsample$hcc_48=factor(hcupsample$hcc_48)



hcupsample$hcc_54[is.na(hcupsample$hcc_54)]<-0
hcupsample$hcc_54=factor(hcupsample$hcc_54)



hcupsample$hcc_55[is.na(hcupsample$hcc_55)]<-0
hcupsample$hcc_55=factor(hcupsample$hcc_55)



hcupsample$hcc_57[is.na(hcupsample$hcc_57)]<-0
hcupsample$hcc_57=factor(hcupsample$hcc_57)



hcupsample$hcc_58[is.na(hcupsample$hcc_58)]<-0
hcupsample$hcc_58=factor(hcupsample$hcc_58)



hcupsample$hcc_6[is.na(hcupsample$hcc_6)]<-0
hcupsample$hcc_6=factor(hcupsample$hcc_6)



hcupsample$hcc_70[is.na(hcupsample$hcc_70)]<-0
hcupsample$hcc_70=factor(hcupsample$hcc_70)



hcupsample$hcc_71[is.na(hcupsample$hcc_71)]<-0
hcupsample$hcc_71=factor(hcupsample$hcc_71)



hcupsample$hcc_72[is.na(hcupsample$hcc_72)]<-0
hcupsample$hcc_72=factor(hcupsample$hcc_72)



hcupsample$hcc_73[is.na(hcupsample$hcc_73)]<-0
hcupsample$hcc_73=factor(hcupsample$hcc_73)



hcupsample$hcc_74[is.na(hcupsample$hcc_74)]<-0
hcupsample$hcc_74=factor(hcupsample$hcc_74)



hcupsample$hcc_75[is.na(hcupsample$hcc_75)]<-0
hcupsample$hcc_75=factor(hcupsample$hcc_75)



hcupsample$hcc_76[is.na(hcupsample$hcc_76)]<-0
hcupsample$hcc_76=factor(hcupsample$hcc_76)



hcupsample$hcc_77[is.na(hcupsample$hcc_77)]<-0
hcupsample$hcc_77=factor(hcupsample$hcc_77)



hcupsample$hcc_78[is.na(hcupsample$hcc_78)]<-0
hcupsample$hcc_78=factor(hcupsample$hcc_78)



hcupsample$hcc_79[is.na(hcupsample$hcc_79)]<-0
hcupsample$hcc_79=factor(hcupsample$hcc_79)



hcupsample$hcc_8[is.na(hcupsample$hcc_8)]<-0
hcupsample$hcc_8=factor(hcupsample$hcc_8)



hcupsample$hcc_80[is.na(hcupsample$hcc_80)]<-0
hcupsample$hcc_80=factor(hcupsample$hcc_80)



hcupsample$hcc_82[is.na(hcupsample$hcc_82)]<-0
hcupsample$hcc_82=factor(hcupsample$hcc_82)



hcupsample$hcc_83[is.na(hcupsample$hcc_83)]<-0
hcupsample$hcc_83=factor(hcupsample$hcc_83)



hcupsample$hcc_84[is.na(hcupsample$hcc_84)]<-0
hcupsample$hcc_84=factor(hcupsample$hcc_84)



hcupsample$hcc_85[is.na(hcupsample$hcc_85)]<-0
hcupsample$hcc_85=factor(hcupsample$hcc_85)



hcupsample$hcc_86[is.na(hcupsample$hcc_86)]<-0
hcupsample$hcc_86=factor(hcupsample$hcc_86)



hcupsample$hcc_87[is.na(hcupsample$hcc_87)]<-0
hcupsample$hcc_87=factor(hcupsample$hcc_87)



hcupsample$hcc_88[is.na(hcupsample$hcc_88)]<-0
hcupsample$hcc_88=factor(hcupsample$hcc_88)



hcupsample$hcc_9[is.na(hcupsample$hcc_9)]<-0
hcupsample$hcc_9=factor(hcupsample$hcc_9)



hcupsample$hcc_96[is.na(hcupsample$hcc_96)]<-0
hcupsample$hcc_96=factor(hcupsample$hcc_96)



hcupsample$hcc_99[is.na(hcupsample$hcc_99)]<-0
hcupsample$hcc_99=factor(hcupsample$hcc_99)



hcupsample$comorb_htn_c[is.na(hcupsample$comorb_htn_c)]<-0
hcupsample$comorb_htn_c=factor(hcupsample$comorb_htn_c)



hcupsample$comorb_arth[is.na(hcupsample$comorb_arth)]<-0
hcupsample$comorb_arth=factor(hcupsample$comorb_arth)



hcupsample$comorb_perivasc[is.na(hcupsample$comorb_perivasc)]<-0
hcupsample$comorb_perivasc=factor(hcupsample$comorb_perivasc)



hcupsample$comorb_dmcx[is.na(hcupsample$comorb_dmcx)]<-0
hcupsample$comorb_dmcx=factor(hcupsample$comorb_dmcx)



hcupsample$comorb_pulmcirc[is.na(hcupsample$comorb_pulmcirc)]<-0
hcupsample$comorb_pulmcirc=factor(hcupsample$comorb_pulmcirc)



hcupsample$comorb_liver[is.na(hcupsample$comorb_liver)]<-0
hcupsample$comorb_liver=factor(hcupsample$comorb_liver)



hcupsample$comorb_drug[is.na(hcupsample$comorb_drug)]<-0
hcupsample$comorb_drug=factor(hcupsample$comorb_drug)



hcupsample$comorb_neuro[is.na(hcupsample$comorb_neuro)]<-0
hcupsample$comorb_neuro=factor(hcupsample$comorb_neuro)



hcupsample$comorb_chf[is.na(hcupsample$comorb_chf)]<-0
hcupsample$comorb_chf=factor(hcupsample$comorb_chf)



hcupsample$comorb_renlfail[is.na(hcupsample$comorb_renlfail)]<-0
hcupsample$comorb_renlfail=factor(hcupsample$comorb_renlfail)



hcupsample$comorb_wghtloss[is.na(hcupsample$comorb_wghtloss)]<-0
hcupsample$comorb_wghtloss=factor(hcupsample$comorb_wghtloss)



hcupsample$comorb_ulcer[is.na(hcupsample$comorb_ulcer)]<-0
hcupsample$comorb_ulcer=factor(hcupsample$comorb_ulcer)



hcupsample$comorb_obese[is.na(hcupsample$comorb_obese)]<-0
hcupsample$comorb_obese=factor(hcupsample$comorb_obese)



hcupsample$comorb_chrnlung[is.na(hcupsample$comorb_chrnlung)]<-0
hcupsample$comorb_chrnlung=factor(hcupsample$comorb_chrnlung)



hcupsample$comorb_mets[is.na(hcupsample$comorb_mets)]<-0
hcupsample$comorb_mets=factor(hcupsample$comorb_mets)



hcupsample$comorb_dm[is.na(hcupsample$comorb_dm)]<-0
hcupsample$comorb_dm=factor(hcupsample$comorb_dm)



hcupsample$comorb_lymph[is.na(hcupsample$comorb_lymph)]<-0
hcupsample$comorb_lymph=factor(hcupsample$comorb_lymph)



hcupsample$comorb_tumor[is.na(hcupsample$comorb_tumor)]<-0
hcupsample$comorb_tumor=factor(hcupsample$comorb_tumor)



hcupsample$comorb_aids[is.na(hcupsample$comorb_aids)]<-0
hcupsample$comorb_aids=factor(hcupsample$comorb_aids)



hcupsample$comorb_hypothy[is.na(hcupsample$comorb_hypothy)]<-0
hcupsample$comorb_hypothy=factor(hcupsample$comorb_hypothy)



hcupsample$comorb_alcohol[is.na(hcupsample$comorb_alcohol)]<-0
hcupsample$comorb_alcohol=factor(hcupsample$comorb_alcohol)



hcupsample$comorb_valve[is.na(hcupsample$comorb_valve)]<-0
hcupsample$comorb_valve=factor(hcupsample$comorb_valve)



hcupsample$comorb_depress[is.na(hcupsample$comorb_depress)]<-0
hcupsample$comorb_depress=factor(hcupsample$comorb_depress)



hcupsample$comorb_lytes[is.na(hcupsample$comorb_lytes)]<-0
hcupsample$comorb_lytes=factor(hcupsample$comorb_lytes)



hcupsample$comorb_para[is.na(hcupsample$comorb_para)]<-0
hcupsample$comorb_para=factor(hcupsample$comorb_para)



hcupsample$comorb_coag[is.na(hcupsample$comorb_coag)]<-0
hcupsample$comorb_coag=factor(hcupsample$comorb_coag)



hcupsample$comorb_psych[is.na(hcupsample$comorb_psych)]<-0
hcupsample$comorb_psych=factor(hcupsample$comorb_psych)



hcupsample$comorb_anemdef[is.na(hcupsample$comorb_anemdef)]<-0
hcupsample$comorb_anemdef=factor(hcupsample$comorb_anemdef)



hcupsample$comorb_bldloss[is.na(hcupsample$comorb_bldloss)]<-0
hcupsample$comorb_bldloss=factor(hcupsample$comorb_bldloss)



hcupsample$procccs_1[is.na(hcupsample$procccs_1)]<-0
hcupsample$procccs_1=factor(hcupsample$procccs_1)



hcupsample$procccs_2[is.na(hcupsample$procccs_2)]<-0
hcupsample$procccs_2=factor(hcupsample$procccs_2)



hcupsample$procccs_3[is.na(hcupsample$procccs_3)]<-0
hcupsample$procccs_3=factor(hcupsample$procccs_3)



hcupsample$procccs_4[is.na(hcupsample$procccs_4)]<-0
hcupsample$procccs_4=factor(hcupsample$procccs_4)



hcupsample$procccs_5[is.na(hcupsample$procccs_5)]<-0
hcupsample$procccs_5=factor(hcupsample$procccs_5)



hcupsample$procccs_6[is.na(hcupsample$procccs_6)]<-0
hcupsample$procccs_6=factor(hcupsample$procccs_6)



hcupsample$procccs_7[is.na(hcupsample$procccs_7)]<-0
hcupsample$procccs_7=factor(hcupsample$procccs_7)



hcupsample$procccs_8[is.na(hcupsample$procccs_8)]<-0
hcupsample$procccs_8=factor(hcupsample$procccs_8)



hcupsample$procccs_9[is.na(hcupsample$procccs_9)]<-0
hcupsample$procccs_9=factor(hcupsample$procccs_9)



hcupsample$procccs_10[is.na(hcupsample$procccs_10)]<-0
hcupsample$procccs_10=factor(hcupsample$procccs_10)



hcupsample$procccs_11[is.na(hcupsample$procccs_11)]<-0
hcupsample$procccs_11=factor(hcupsample$procccs_11)



hcupsample$procccs_12[is.na(hcupsample$procccs_12)]<-0
hcupsample$procccs_12=factor(hcupsample$procccs_12)



hcupsample$procccs_13[is.na(hcupsample$procccs_13)]<-0
hcupsample$procccs_13=factor(hcupsample$procccs_13)



hcupsample$procccs_14[is.na(hcupsample$procccs_14)]<-0
hcupsample$procccs_14=factor(hcupsample$procccs_14)



hcupsample$procccs_15[is.na(hcupsample$procccs_15)]<-0
hcupsample$procccs_15=factor(hcupsample$procccs_15)



hcupsample$procccs_16[is.na(hcupsample$procccs_16)]<-0
hcupsample$procccs_16=factor(hcupsample$procccs_16)



hcupsample$procccs_17[is.na(hcupsample$procccs_17)]<-0
hcupsample$procccs_17=factor(hcupsample$procccs_17)



hcupsample$procccs_18[is.na(hcupsample$procccs_18)]<-0
hcupsample$procccs_18=factor(hcupsample$procccs_18)



hcupsample$procccs_19[is.na(hcupsample$procccs_19)]<-0
hcupsample$procccs_19=factor(hcupsample$procccs_19)



hcupsample$procccs_20[is.na(hcupsample$procccs_20)]<-0
hcupsample$procccs_20=factor(hcupsample$procccs_20)



hcupsample$procccs_21[is.na(hcupsample$procccs_21)]<-0
hcupsample$procccs_21=factor(hcupsample$procccs_21)



hcupsample$procccs_22[is.na(hcupsample$procccs_22)]<-0
hcupsample$procccs_22=factor(hcupsample$procccs_22)



hcupsample$procccs_23[is.na(hcupsample$procccs_23)]<-0
hcupsample$procccs_23=factor(hcupsample$procccs_23)



hcupsample$procccs_24[is.na(hcupsample$procccs_24)]<-0
hcupsample$procccs_24=factor(hcupsample$procccs_24)



hcupsample$procccs_25[is.na(hcupsample$procccs_25)]<-0
hcupsample$procccs_25=factor(hcupsample$procccs_25)



hcupsample$procccs_26[is.na(hcupsample$procccs_26)]<-0
hcupsample$procccs_26=factor(hcupsample$procccs_26)



hcupsample$procccs_27[is.na(hcupsample$procccs_27)]<-0
hcupsample$procccs_27=factor(hcupsample$procccs_27)



hcupsample$procccs_28[is.na(hcupsample$procccs_28)]<-0
hcupsample$procccs_28=factor(hcupsample$procccs_28)



hcupsample$procccs_29[is.na(hcupsample$procccs_29)]<-0
hcupsample$procccs_29=factor(hcupsample$procccs_29)



hcupsample$procccs_30[is.na(hcupsample$procccs_30)]<-0
hcupsample$procccs_30=factor(hcupsample$procccs_30)



hcupsample$procccs_31[is.na(hcupsample$procccs_31)]<-0
hcupsample$procccs_31=factor(hcupsample$procccs_31)



hcupsample$procccs_32[is.na(hcupsample$procccs_32)]<-0
hcupsample$procccs_32=factor(hcupsample$procccs_32)



hcupsample$procccs_33[is.na(hcupsample$procccs_33)]<-0
hcupsample$procccs_33=factor(hcupsample$procccs_33)



hcupsample$procccs_34[is.na(hcupsample$procccs_34)]<-0
hcupsample$procccs_34=factor(hcupsample$procccs_34)



hcupsample$procccs_35[is.na(hcupsample$procccs_35)]<-0
hcupsample$procccs_35=factor(hcupsample$procccs_35)



hcupsample$procccs_36[is.na(hcupsample$procccs_36)]<-0
hcupsample$procccs_36=factor(hcupsample$procccs_36)



hcupsample$procccs_37[is.na(hcupsample$procccs_37)]<-0
hcupsample$procccs_37=factor(hcupsample$procccs_37)



hcupsample$procccs_38[is.na(hcupsample$procccs_38)]<-0
hcupsample$procccs_38=factor(hcupsample$procccs_38)



hcupsample$procccs_39[is.na(hcupsample$procccs_39)]<-0
hcupsample$procccs_39=factor(hcupsample$procccs_39)



hcupsample$procccs_40[is.na(hcupsample$procccs_40)]<-0
hcupsample$procccs_40=factor(hcupsample$procccs_40)



hcupsample$procccs_41[is.na(hcupsample$procccs_41)]<-0
hcupsample$procccs_41=factor(hcupsample$procccs_41)



hcupsample$procccs_42[is.na(hcupsample$procccs_42)]<-0
hcupsample$procccs_42=factor(hcupsample$procccs_42)



hcupsample$procccs_43[is.na(hcupsample$procccs_43)]<-0
hcupsample$procccs_43=factor(hcupsample$procccs_43)



hcupsample$procccs_44[is.na(hcupsample$procccs_44)]<-0
hcupsample$procccs_44=factor(hcupsample$procccs_44)



hcupsample$procccs_45[is.na(hcupsample$procccs_45)]<-0
hcupsample$procccs_45=factor(hcupsample$procccs_45)



hcupsample$procccs_46[is.na(hcupsample$procccs_46)]<-0
hcupsample$procccs_46=factor(hcupsample$procccs_46)



hcupsample$procccs_47[is.na(hcupsample$procccs_47)]<-0
hcupsample$procccs_47=factor(hcupsample$procccs_47)



hcupsample$procccs_48[is.na(hcupsample$procccs_48)]<-0
hcupsample$procccs_48=factor(hcupsample$procccs_48)



hcupsample$procccs_49[is.na(hcupsample$procccs_49)]<-0
hcupsample$procccs_49=factor(hcupsample$procccs_49)



hcupsample$procccs_50[is.na(hcupsample$procccs_50)]<-0
hcupsample$procccs_50=factor(hcupsample$procccs_50)



hcupsample$procccs_51[is.na(hcupsample$procccs_51)]<-0
hcupsample$procccs_51=factor(hcupsample$procccs_51)



hcupsample$procccs_52[is.na(hcupsample$procccs_52)]<-0
hcupsample$procccs_52=factor(hcupsample$procccs_52)



hcupsample$procccs_53[is.na(hcupsample$procccs_53)]<-0
hcupsample$procccs_53=factor(hcupsample$procccs_53)



hcupsample$procccs_54[is.na(hcupsample$procccs_54)]<-0
hcupsample$procccs_54=factor(hcupsample$procccs_54)



hcupsample$procccs_55[is.na(hcupsample$procccs_55)]<-0
hcupsample$procccs_55=factor(hcupsample$procccs_55)



hcupsample$procccs_56[is.na(hcupsample$procccs_56)]<-0
hcupsample$procccs_56=factor(hcupsample$procccs_56)



hcupsample$procccs_57[is.na(hcupsample$procccs_57)]<-0
hcupsample$procccs_57=factor(hcupsample$procccs_57)



hcupsample$procccs_58[is.na(hcupsample$procccs_58)]<-0
hcupsample$procccs_58=factor(hcupsample$procccs_58)



hcupsample$procccs_59[is.na(hcupsample$procccs_59)]<-0
hcupsample$procccs_59=factor(hcupsample$procccs_59)



hcupsample$procccs_60[is.na(hcupsample$procccs_60)]<-0
hcupsample$procccs_60=factor(hcupsample$procccs_60)



hcupsample$procccs_61[is.na(hcupsample$procccs_61)]<-0
hcupsample$procccs_61=factor(hcupsample$procccs_61)



hcupsample$procccs_62[is.na(hcupsample$procccs_62)]<-0
hcupsample$procccs_62=factor(hcupsample$procccs_62)



hcupsample$procccs_63[is.na(hcupsample$procccs_63)]<-0
hcupsample$procccs_63=factor(hcupsample$procccs_63)



hcupsample$procccs_64[is.na(hcupsample$procccs_64)]<-0
hcupsample$procccs_64=factor(hcupsample$procccs_64)



hcupsample$procccs_65[is.na(hcupsample$procccs_65)]<-0
hcupsample$procccs_65=factor(hcupsample$procccs_65)



hcupsample$procccs_66[is.na(hcupsample$procccs_66)]<-0
hcupsample$procccs_66=factor(hcupsample$procccs_66)



hcupsample$procccs_67[is.na(hcupsample$procccs_67)]<-0
hcupsample$procccs_67=factor(hcupsample$procccs_67)



hcupsample$procccs_68[is.na(hcupsample$procccs_68)]<-0
hcupsample$procccs_68=factor(hcupsample$procccs_68)



hcupsample$procccs_69[is.na(hcupsample$procccs_69)]<-0
hcupsample$procccs_69=factor(hcupsample$procccs_69)



hcupsample$procccs_70[is.na(hcupsample$procccs_70)]<-0
hcupsample$procccs_70=factor(hcupsample$procccs_70)



hcupsample$procccs_71[is.na(hcupsample$procccs_71)]<-0
hcupsample$procccs_71=factor(hcupsample$procccs_71)



hcupsample$procccs_72[is.na(hcupsample$procccs_72)]<-0
hcupsample$procccs_72=factor(hcupsample$procccs_72)



hcupsample$procccs_73[is.na(hcupsample$procccs_73)]<-0
hcupsample$procccs_73=factor(hcupsample$procccs_73)



hcupsample$procccs_74[is.na(hcupsample$procccs_74)]<-0
hcupsample$procccs_74=factor(hcupsample$procccs_74)



hcupsample$procccs_75[is.na(hcupsample$procccs_75)]<-0
hcupsample$procccs_75=factor(hcupsample$procccs_75)



hcupsample$procccs_76[is.na(hcupsample$procccs_76)]<-0
hcupsample$procccs_76=factor(hcupsample$procccs_76)



hcupsample$procccs_77[is.na(hcupsample$procccs_77)]<-0
hcupsample$procccs_77=factor(hcupsample$procccs_77)



hcupsample$procccs_78[is.na(hcupsample$procccs_78)]<-0
hcupsample$procccs_78=factor(hcupsample$procccs_78)



hcupsample$procccs_79[is.na(hcupsample$procccs_79)]<-0
hcupsample$procccs_79=factor(hcupsample$procccs_79)



hcupsample$procccs_80[is.na(hcupsample$procccs_80)]<-0
hcupsample$procccs_80=factor(hcupsample$procccs_80)



hcupsample$procccs_81[is.na(hcupsample$procccs_81)]<-0
hcupsample$procccs_81=factor(hcupsample$procccs_81)



hcupsample$procccs_82[is.na(hcupsample$procccs_82)]<-0
hcupsample$procccs_82=factor(hcupsample$procccs_82)



hcupsample$procccs_83[is.na(hcupsample$procccs_83)]<-0
hcupsample$procccs_83=factor(hcupsample$procccs_83)



hcupsample$procccs_84[is.na(hcupsample$procccs_84)]<-0
hcupsample$procccs_84=factor(hcupsample$procccs_84)



hcupsample$procccs_85[is.na(hcupsample$procccs_85)]<-0
hcupsample$procccs_85=factor(hcupsample$procccs_85)



hcupsample$procccs_86[is.na(hcupsample$procccs_86)]<-0
hcupsample$procccs_86=factor(hcupsample$procccs_86)



hcupsample$procccs_87[is.na(hcupsample$procccs_87)]<-0
hcupsample$procccs_87=factor(hcupsample$procccs_87)



hcupsample$procccs_88[is.na(hcupsample$procccs_88)]<-0
hcupsample$procccs_88=factor(hcupsample$procccs_88)



hcupsample$procccs_89[is.na(hcupsample$procccs_89)]<-0
hcupsample$procccs_89=factor(hcupsample$procccs_89)



hcupsample$procccs_90[is.na(hcupsample$procccs_90)]<-0
hcupsample$procccs_90=factor(hcupsample$procccs_90)



hcupsample$procccs_91[is.na(hcupsample$procccs_91)]<-0
hcupsample$procccs_91=factor(hcupsample$procccs_91)



hcupsample$procccs_92[is.na(hcupsample$procccs_92)]<-0
hcupsample$procccs_92=factor(hcupsample$procccs_92)



hcupsample$procccs_93[is.na(hcupsample$procccs_93)]<-0
hcupsample$procccs_93=factor(hcupsample$procccs_93)



hcupsample$procccs_94[is.na(hcupsample$procccs_94)]<-0
hcupsample$procccs_94=factor(hcupsample$procccs_94)



hcupsample$procccs_95[is.na(hcupsample$procccs_95)]<-0
hcupsample$procccs_95=factor(hcupsample$procccs_95)



hcupsample$procccs_96[is.na(hcupsample$procccs_96)]<-0
hcupsample$procccs_96=factor(hcupsample$procccs_96)



hcupsample$procccs_97[is.na(hcupsample$procccs_97)]<-0
hcupsample$procccs_97=factor(hcupsample$procccs_97)



hcupsample$procccs_98[is.na(hcupsample$procccs_98)]<-0
hcupsample$procccs_98=factor(hcupsample$procccs_98)



hcupsample$procccs_99[is.na(hcupsample$procccs_99)]<-0
hcupsample$procccs_99=factor(hcupsample$procccs_99)



hcupsample$procccs_100[is.na(hcupsample$procccs_100)]<-0
hcupsample$procccs_100=factor(hcupsample$procccs_100)



hcupsample$procccs_101[is.na(hcupsample$procccs_101)]<-0
hcupsample$procccs_101=factor(hcupsample$procccs_101)



hcupsample$procccs_102[is.na(hcupsample$procccs_102)]<-0
hcupsample$procccs_102=factor(hcupsample$procccs_102)



hcupsample$procccs_103[is.na(hcupsample$procccs_103)]<-0
hcupsample$procccs_103=factor(hcupsample$procccs_103)



hcupsample$procccs_104[is.na(hcupsample$procccs_104)]<-0
hcupsample$procccs_104=factor(hcupsample$procccs_104)



hcupsample$procccs_105[is.na(hcupsample$procccs_105)]<-0
hcupsample$procccs_105=factor(hcupsample$procccs_105)



hcupsample$procccs_106[is.na(hcupsample$procccs_106)]<-0
hcupsample$procccs_106=factor(hcupsample$procccs_106)



hcupsample$procccs_107[is.na(hcupsample$procccs_107)]<-0
hcupsample$procccs_107=factor(hcupsample$procccs_107)



hcupsample$procccs_108[is.na(hcupsample$procccs_108)]<-0
hcupsample$procccs_108=factor(hcupsample$procccs_108)



hcupsample$procccs_109[is.na(hcupsample$procccs_109)]<-0
hcupsample$procccs_109=factor(hcupsample$procccs_109)



hcupsample$procccs_110[is.na(hcupsample$procccs_110)]<-0
hcupsample$procccs_110=factor(hcupsample$procccs_110)



hcupsample$procccs_111[is.na(hcupsample$procccs_111)]<-0
hcupsample$procccs_111=factor(hcupsample$procccs_111)



hcupsample$procccs_112[is.na(hcupsample$procccs_112)]<-0
hcupsample$procccs_112=factor(hcupsample$procccs_112)



hcupsample$procccs_113[is.na(hcupsample$procccs_113)]<-0
hcupsample$procccs_113=factor(hcupsample$procccs_113)



hcupsample$procccs_114[is.na(hcupsample$procccs_114)]<-0
hcupsample$procccs_114=factor(hcupsample$procccs_114)



hcupsample$procccs_115[is.na(hcupsample$procccs_115)]<-0
hcupsample$procccs_115=factor(hcupsample$procccs_115)



hcupsample$procccs_116[is.na(hcupsample$procccs_116)]<-0
hcupsample$procccs_116=factor(hcupsample$procccs_116)



hcupsample$procccs_117[is.na(hcupsample$procccs_117)]<-0
hcupsample$procccs_117=factor(hcupsample$procccs_117)



hcupsample$procccs_118[is.na(hcupsample$procccs_118)]<-0
hcupsample$procccs_118=factor(hcupsample$procccs_118)



hcupsample$procccs_119[is.na(hcupsample$procccs_119)]<-0
hcupsample$procccs_119=factor(hcupsample$procccs_119)



hcupsample$procccs_120[is.na(hcupsample$procccs_120)]<-0
hcupsample$procccs_120=factor(hcupsample$procccs_120)



hcupsample$procccs_121[is.na(hcupsample$procccs_121)]<-0
hcupsample$procccs_121=factor(hcupsample$procccs_121)



hcupsample$procccs_122[is.na(hcupsample$procccs_122)]<-0
hcupsample$procccs_122=factor(hcupsample$procccs_122)



hcupsample$procccs_123[is.na(hcupsample$procccs_123)]<-0
hcupsample$procccs_123=factor(hcupsample$procccs_123)



hcupsample$procccs_124[is.na(hcupsample$procccs_124)]<-0
hcupsample$procccs_124=factor(hcupsample$procccs_124)



hcupsample$procccs_125[is.na(hcupsample$procccs_125)]<-0
hcupsample$procccs_125=factor(hcupsample$procccs_125)



hcupsample$procccs_126[is.na(hcupsample$procccs_126)]<-0
hcupsample$procccs_126=factor(hcupsample$procccs_126)



hcupsample$procccs_127[is.na(hcupsample$procccs_127)]<-0
hcupsample$procccs_127=factor(hcupsample$procccs_127)



hcupsample$procccs_128[is.na(hcupsample$procccs_128)]<-0
hcupsample$procccs_128=factor(hcupsample$procccs_128)



hcupsample$procccs_129[is.na(hcupsample$procccs_129)]<-0
hcupsample$procccs_129=factor(hcupsample$procccs_129)



hcupsample$procccs_130[is.na(hcupsample$procccs_130)]<-0
hcupsample$procccs_130=factor(hcupsample$procccs_130)



hcupsample$procccs_131[is.na(hcupsample$procccs_131)]<-0
hcupsample$procccs_131=factor(hcupsample$procccs_131)



hcupsample$procccs_132[is.na(hcupsample$procccs_132)]<-0
hcupsample$procccs_132=factor(hcupsample$procccs_132)



hcupsample$procccs_133[is.na(hcupsample$procccs_133)]<-0
hcupsample$procccs_133=factor(hcupsample$procccs_133)



hcupsample$procccs_134[is.na(hcupsample$procccs_134)]<-0
hcupsample$procccs_134=factor(hcupsample$procccs_134)



hcupsample$procccs_135[is.na(hcupsample$procccs_135)]<-0
hcupsample$procccs_135=factor(hcupsample$procccs_135)



hcupsample$procccs_136[is.na(hcupsample$procccs_136)]<-0
hcupsample$procccs_136=factor(hcupsample$procccs_136)



hcupsample$procccs_137[is.na(hcupsample$procccs_137)]<-0
hcupsample$procccs_137=factor(hcupsample$procccs_137)



hcupsample$procccs_138[is.na(hcupsample$procccs_138)]<-0
hcupsample$procccs_138=factor(hcupsample$procccs_138)



hcupsample$procccs_139[is.na(hcupsample$procccs_139)]<-0
hcupsample$procccs_139=factor(hcupsample$procccs_139)



hcupsample$procccs_140[is.na(hcupsample$procccs_140)]<-0
hcupsample$procccs_140=factor(hcupsample$procccs_140)



hcupsample$procccs_141[is.na(hcupsample$procccs_141)]<-0
hcupsample$procccs_141=factor(hcupsample$procccs_141)



hcupsample$procccs_142[is.na(hcupsample$procccs_142)]<-0
hcupsample$procccs_142=factor(hcupsample$procccs_142)



hcupsample$procccs_143[is.na(hcupsample$procccs_143)]<-0
hcupsample$procccs_143=factor(hcupsample$procccs_143)



hcupsample$procccs_144[is.na(hcupsample$procccs_144)]<-0
hcupsample$procccs_144=factor(hcupsample$procccs_144)



hcupsample$procccs_145[is.na(hcupsample$procccs_145)]<-0
hcupsample$procccs_145=factor(hcupsample$procccs_145)



hcupsample$procccs_146[is.na(hcupsample$procccs_146)]<-0
hcupsample$procccs_146=factor(hcupsample$procccs_146)



hcupsample$procccs_147[is.na(hcupsample$procccs_147)]<-0
hcupsample$procccs_147=factor(hcupsample$procccs_147)



hcupsample$procccs_148[is.na(hcupsample$procccs_148)]<-0
hcupsample$procccs_148=factor(hcupsample$procccs_148)



hcupsample$procccs_149[is.na(hcupsample$procccs_149)]<-0
hcupsample$procccs_149=factor(hcupsample$procccs_149)



hcupsample$procccs_150[is.na(hcupsample$procccs_150)]<-0
hcupsample$procccs_150=factor(hcupsample$procccs_150)



hcupsample$procccs_151[is.na(hcupsample$procccs_151)]<-0
hcupsample$procccs_151=factor(hcupsample$procccs_151)



hcupsample$procccs_152[is.na(hcupsample$procccs_152)]<-0
hcupsample$procccs_152=factor(hcupsample$procccs_152)



hcupsample$procccs_153[is.na(hcupsample$procccs_153)]<-0
hcupsample$procccs_153=factor(hcupsample$procccs_153)



hcupsample$procccs_154[is.na(hcupsample$procccs_154)]<-0
hcupsample$procccs_154=factor(hcupsample$procccs_154)



hcupsample$procccs_155[is.na(hcupsample$procccs_155)]<-0
hcupsample$procccs_155=factor(hcupsample$procccs_155)



hcupsample$procccs_156[is.na(hcupsample$procccs_156)]<-0
hcupsample$procccs_156=factor(hcupsample$procccs_156)



hcupsample$procccs_157[is.na(hcupsample$procccs_157)]<-0
hcupsample$procccs_157=factor(hcupsample$procccs_157)



hcupsample$procccs_158[is.na(hcupsample$procccs_158)]<-0
hcupsample$procccs_158=factor(hcupsample$procccs_158)



hcupsample$procccs_159[is.na(hcupsample$procccs_159)]<-0
hcupsample$procccs_159=factor(hcupsample$procccs_159)



hcupsample$procccs_160[is.na(hcupsample$procccs_160)]<-0
hcupsample$procccs_160=factor(hcupsample$procccs_160)



hcupsample$procccs_161[is.na(hcupsample$procccs_161)]<-0
hcupsample$procccs_161=factor(hcupsample$procccs_161)



hcupsample$procccs_162[is.na(hcupsample$procccs_162)]<-0
hcupsample$procccs_162=factor(hcupsample$procccs_162)



hcupsample$procccs_163[is.na(hcupsample$procccs_163)]<-0
hcupsample$procccs_163=factor(hcupsample$procccs_163)



hcupsample$procccs_164[is.na(hcupsample$procccs_164)]<-0
hcupsample$procccs_164=factor(hcupsample$procccs_164)



hcupsample$procccs_165[is.na(hcupsample$procccs_165)]<-0
hcupsample$procccs_165=factor(hcupsample$procccs_165)



hcupsample$procccs_166[is.na(hcupsample$procccs_166)]<-0
hcupsample$procccs_166=factor(hcupsample$procccs_166)



hcupsample$procccs_167[is.na(hcupsample$procccs_167)]<-0
hcupsample$procccs_167=factor(hcupsample$procccs_167)



hcupsample$procccs_168[is.na(hcupsample$procccs_168)]<-0
hcupsample$procccs_168=factor(hcupsample$procccs_168)



hcupsample$procccs_169[is.na(hcupsample$procccs_169)]<-0
hcupsample$procccs_169=factor(hcupsample$procccs_169)



hcupsample$procccs_170[is.na(hcupsample$procccs_170)]<-0
hcupsample$procccs_170=factor(hcupsample$procccs_170)



hcupsample$procccs_171[is.na(hcupsample$procccs_171)]<-0
hcupsample$procccs_171=factor(hcupsample$procccs_171)



hcupsample$procccs_172[is.na(hcupsample$procccs_172)]<-0
hcupsample$procccs_172=factor(hcupsample$procccs_172)



hcupsample$procccs_173[is.na(hcupsample$procccs_173)]<-0
hcupsample$procccs_173=factor(hcupsample$procccs_173)



hcupsample$procccs_174[is.na(hcupsample$procccs_174)]<-0
hcupsample$procccs_174=factor(hcupsample$procccs_174)



hcupsample$procccs_175[is.na(hcupsample$procccs_175)]<-0
hcupsample$procccs_175=factor(hcupsample$procccs_175)



hcupsample$procccs_176[is.na(hcupsample$procccs_176)]<-0
hcupsample$procccs_176=factor(hcupsample$procccs_176)



hcupsample$procccs_177[is.na(hcupsample$procccs_177)]<-0
hcupsample$procccs_177=factor(hcupsample$procccs_177)



hcupsample$procccs_178[is.na(hcupsample$procccs_178)]<-0
hcupsample$procccs_178=factor(hcupsample$procccs_178)



hcupsample$procccs_179[is.na(hcupsample$procccs_179)]<-0
hcupsample$procccs_179=factor(hcupsample$procccs_179)



hcupsample$procccs_180[is.na(hcupsample$procccs_180)]<-0
hcupsample$procccs_180=factor(hcupsample$procccs_180)



hcupsample$procccs_181[is.na(hcupsample$procccs_181)]<-0
hcupsample$procccs_181=factor(hcupsample$procccs_181)



hcupsample$procccs_182[is.na(hcupsample$procccs_182)]<-0
hcupsample$procccs_182=factor(hcupsample$procccs_182)



hcupsample$procccs_183[is.na(hcupsample$procccs_183)]<-0
hcupsample$procccs_183=factor(hcupsample$procccs_183)



hcupsample$procccs_184[is.na(hcupsample$procccs_184)]<-0
hcupsample$procccs_184=factor(hcupsample$procccs_184)



hcupsample$procccs_185[is.na(hcupsample$procccs_185)]<-0
hcupsample$procccs_185=factor(hcupsample$procccs_185)



hcupsample$procccs_186[is.na(hcupsample$procccs_186)]<-0
hcupsample$procccs_186=factor(hcupsample$procccs_186)



hcupsample$procccs_187[is.na(hcupsample$procccs_187)]<-0
hcupsample$procccs_187=factor(hcupsample$procccs_187)



hcupsample$procccs_188[is.na(hcupsample$procccs_188)]<-0
hcupsample$procccs_188=factor(hcupsample$procccs_188)



hcupsample$procccs_189[is.na(hcupsample$procccs_189)]<-0
hcupsample$procccs_189=factor(hcupsample$procccs_189)



hcupsample$procccs_190[is.na(hcupsample$procccs_190)]<-0
hcupsample$procccs_190=factor(hcupsample$procccs_190)



hcupsample$procccs_191[is.na(hcupsample$procccs_191)]<-0
hcupsample$procccs_191=factor(hcupsample$procccs_191)



hcupsample$procccs_192[is.na(hcupsample$procccs_192)]<-0
hcupsample$procccs_192=factor(hcupsample$procccs_192)



hcupsample$procccs_193[is.na(hcupsample$procccs_193)]<-0
hcupsample$procccs_193=factor(hcupsample$procccs_193)



hcupsample$procccs_194[is.na(hcupsample$procccs_194)]<-0
hcupsample$procccs_194=factor(hcupsample$procccs_194)



hcupsample$procccs_195[is.na(hcupsample$procccs_195)]<-0
hcupsample$procccs_195=factor(hcupsample$procccs_195)



hcupsample$procccs_196[is.na(hcupsample$procccs_196)]<-0
hcupsample$procccs_196=factor(hcupsample$procccs_196)



hcupsample$procccs_197[is.na(hcupsample$procccs_197)]<-0
hcupsample$procccs_197=factor(hcupsample$procccs_197)



hcupsample$procccs_198[is.na(hcupsample$procccs_198)]<-0
hcupsample$procccs_198=factor(hcupsample$procccs_198)



hcupsample$procccs_199[is.na(hcupsample$procccs_199)]<-0
hcupsample$procccs_199=factor(hcupsample$procccs_199)



hcupsample$procccs_200[is.na(hcupsample$procccs_200)]<-0
hcupsample$procccs_200=factor(hcupsample$procccs_200)



hcupsample$procccs_201[is.na(hcupsample$procccs_201)]<-0
hcupsample$procccs_201=factor(hcupsample$procccs_201)



hcupsample$procccs_202[is.na(hcupsample$procccs_202)]<-0
hcupsample$procccs_202=factor(hcupsample$procccs_202)



hcupsample$procccs_203[is.na(hcupsample$procccs_203)]<-0
hcupsample$procccs_203=factor(hcupsample$procccs_203)



hcupsample$procccs_204[is.na(hcupsample$procccs_204)]<-0
hcupsample$procccs_204=factor(hcupsample$procccs_204)



hcupsample$procccs_205[is.na(hcupsample$procccs_205)]<-0
hcupsample$procccs_205=factor(hcupsample$procccs_205)



hcupsample$procccs_206[is.na(hcupsample$procccs_206)]<-0
hcupsample$procccs_206=factor(hcupsample$procccs_206)



hcupsample$procccs_207[is.na(hcupsample$procccs_207)]<-0
hcupsample$procccs_207=factor(hcupsample$procccs_207)



hcupsample$procccs_208[is.na(hcupsample$procccs_208)]<-0
hcupsample$procccs_208=factor(hcupsample$procccs_208)



hcupsample$procccs_209[is.na(hcupsample$procccs_209)]<-0
hcupsample$procccs_209=factor(hcupsample$procccs_209)



hcupsample$procccs_210[is.na(hcupsample$procccs_210)]<-0
hcupsample$procccs_210=factor(hcupsample$procccs_210)



hcupsample$procccs_211[is.na(hcupsample$procccs_211)]<-0
hcupsample$procccs_211=factor(hcupsample$procccs_211)



hcupsample$procccs_212[is.na(hcupsample$procccs_212)]<-0
hcupsample$procccs_212=factor(hcupsample$procccs_212)



hcupsample$procccs_213[is.na(hcupsample$procccs_213)]<-0
hcupsample$procccs_213=factor(hcupsample$procccs_213)



hcupsample$procccs_214[is.na(hcupsample$procccs_214)]<-0
hcupsample$procccs_214=factor(hcupsample$procccs_214)



hcupsample$procccs_215[is.na(hcupsample$procccs_215)]<-0
hcupsample$procccs_215=factor(hcupsample$procccs_215)



hcupsample$procccs_216[is.na(hcupsample$procccs_216)]<-0
hcupsample$procccs_216=factor(hcupsample$procccs_216)



hcupsample$procccs_217[is.na(hcupsample$procccs_217)]<-0
hcupsample$procccs_217=factor(hcupsample$procccs_217)



hcupsample$procccs_218[is.na(hcupsample$procccs_218)]<-0
hcupsample$procccs_218=factor(hcupsample$procccs_218)



hcupsample$procccs_219[is.na(hcupsample$procccs_219)]<-0
hcupsample$procccs_219=factor(hcupsample$procccs_219)



hcupsample$procccs_220[is.na(hcupsample$procccs_220)]<-0
hcupsample$procccs_220=factor(hcupsample$procccs_220)



hcupsample$procccs_221[is.na(hcupsample$procccs_221)]<-0
hcupsample$procccs_221=factor(hcupsample$procccs_221)



hcupsample$procccs_222[is.na(hcupsample$procccs_222)]<-0
hcupsample$procccs_222=factor(hcupsample$procccs_222)



hcupsample$procccs_223[is.na(hcupsample$procccs_223)]<-0
hcupsample$procccs_223=factor(hcupsample$procccs_223)



hcupsample$procccs_224[is.na(hcupsample$procccs_224)]<-0
hcupsample$procccs_224=factor(hcupsample$procccs_224)



hcupsample$procccs_225[is.na(hcupsample$procccs_225)]<-0
hcupsample$procccs_225=factor(hcupsample$procccs_225)



hcupsample$procccs_226[is.na(hcupsample$procccs_226)]<-0
hcupsample$procccs_226=factor(hcupsample$procccs_226)



hcupsample$procccs_227[is.na(hcupsample$procccs_227)]<-0
hcupsample$procccs_227=factor(hcupsample$procccs_227)



hcupsample$procccs_228[is.na(hcupsample$procccs_228)]<-0
hcupsample$procccs_228=factor(hcupsample$procccs_228)



hcupsample$procccs_229[is.na(hcupsample$procccs_229)]<-0
hcupsample$procccs_229=factor(hcupsample$procccs_229)



hcupsample$procccs_230[is.na(hcupsample$procccs_230)]<-0
hcupsample$procccs_230=factor(hcupsample$procccs_230)



hcupsample$procccs_231[is.na(hcupsample$procccs_231)]<-0
hcupsample$procccs_231=factor(hcupsample$procccs_231)


save(hcupsample,file="hcupsample.Rda")


#X------------------------DATA LOADING IS COMPLETE--------------------------X

#hcupmiss<-hcupsample

library(VIM)
ggr_plot <- aggr(hcupmiss, col=c('white','white'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.5, gap=3, ylab=c("Histogram of missing data","Pattern"))

ggr_plot2 <- aggr(hcupsample, col=c('white','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.5, gap=3, ylab=c("Histogram of missing data","Pattern"))

write.csv(hcupsample,file = "imputed.csv")

library(FSelector)

#remove duplicate and irrelevant fields


hcupsample<-subset(hcupsample,select=-c(discwt,drg24,drgver,drg_nopoa,dx1,dxccs1,hosp_nis,mdc,mdc24,mdc_nopoa,pr1,prccs1,year,dxmccs1,e_mccs1,prmccs1,aprdrg,n_disc_u,n_hosp_u,s_disc_u,s_hosp_u,total_disc))

weights <- chi.squared(dispuniform~., hcupsample)
print(weights)

subset <- cutoff.k(weights, 21)
typeof(subset)

hcup_subset<-subset(hcupsample,select=subset)
hcup_subset$dispuniform<-hcupsample$dispuniform
str(hcup_subset)



#splittig the dataset to training, test and validation models(50-30-20)
indexes = sample(1:nrow(hcup_subset), size=0.2*nrow(hcup_subset))
hcuptest=hcup_subset[indexes,]

nrow(hcuptest)

write.csv(hcup_subset, file = "naiveBayesData.csv")

trainandVal=hcup_subset[-indexes,]
indexes = sample(1:nrow(trainandVal), size=0.9*nrow(trainandVal))
hcuptrain=trainandVal[indexes,]
nrow(hcuptrain)
hcupval=trainandVal[-indexes,]
nrow(hcupval)
#hcuptrain=trainandVal


unique(hcupsample$nis_stratum)

summary(hcuptest$dispuniform)

#Implementing naive bayes model
library(e1071)
?predict

model <- naiveBayes(dispuniform ~ ., data = hcuptrain, laplace = 1)
nbpredict<-predict(model,hcuptest)
confmatrix<-table(nbpredict,hcuptest$dispuniform)


library(caret) 
library(Rcpp)
confusionMatrix(confmatrix)
#nbvalidation<-predict(model,hcupval)
#val.confmatrix<-table(nbvalidation,hcupval$dispuniform)
#confusionMatrix(val.confmatrix)

train_control <- trainControl(method="cv", number=3)
# train the model 
model <- train(dispuniform~., data=hcup_subset, trControl=train_control, method="nb")
# make predictions
predictions <- predict(model, hcup_subset)
# summarize results
confusionMatrix(predictions, hcup_subset$dispuniform)

