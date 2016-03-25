library(mice)
library(VIM)
library(Hmisc)
library(fmsb)


setwd("D:\\HCUP Project")
hcupData <- read.csv(file="masterdataset_sample.csv", header=T, sep=",", row.names=1)
str(hcupData)
hcupData<-subset(hcupData, select = -c(hcc_1,hcc_10,hcc_100,hcc_103,hcc_104,hcc_106,hcc_107,hcc_108,hcc_11,hcc_110,hcc_111,hcc_112,hcc_114,hcc_115,hcc_12,hcc_122,hcc_124,hcc_134,hcc_135,hcc_136,hcc_137,hcc_157,hcc_158,hcc_161,hcc_162,hcc_166,hcc_167,hcc_169,hcc_17,hcc_170,hcc_173,hcc_176,hcc_18,hcc_186,hcc_188,hcc_189,hcc_19,hcc_2,hcc_21,hcc_22,hcc_23,hcc_27,hcc_28,hcc_29,hcc_33,hcc_34,hcc_35,hcc_39,hcc_40,hcc_46,hcc_47,hcc_48,hcc_54,hcc_55,hcc_57,hcc_58,hcc_6,hcc_70,hcc_71,hcc_72,hcc_73,hcc_74,hcc_75,hcc_76,hcc_77,hcc_78,hcc_79,hcc_8,hcc_80,hcc_82,hcc_83,hcc_84,hcc_85,hcc_86,hcc_87,hcc_88,hcc_9,hcc_96,hcc_99))

# for age, values -66, -99 are actually missing values
hcupData$age[hcupData$age<0]<-NA
summary(hcupData$age)
hcupData$age[is.na(hcupData$age)]<-48.72
hcupData$age<-as.integer(hcupData$age)
unique(hcupData$age)


#age_neonate -9 are missing values
hcupData$age_neonate[hcupData$age_neonate<0]<-NA
unique(hcupData$age_neonate)

#factorize neonate
hcupData$age_neonate=factor(hcupData$age_neonate)
str(hcupData$age_neonate)
hcupData$age_neonate[is.na(hcupData$age_neonate)]<-0



#Defning missing values and factorizing amonth
hcupData$amonth[hcupData$amonth<0]<-NA
hcupData$amonth=factor(hcupData$amonth)
test<-names(which.max(table(hcupData$amonth)))
hcupData$amonth[is.na(hcupData$amonth)]<-test
unique(hcupData$amonth)



#Defining missing values and factorizing aweekend
hcupData$aweekend[hcupData$aweekend<0]<-NA
hcupData$aweekend=factor(hcupData$aweekend)
test<-names(which.max(table(hcupData$aweekend)))
hcupData$aweekend[is.na(hcupData$aweekend)]<-test
unique(hcupData$aweekend)


#Defining missing values and factorizing died flag
unique(hcupData$died)
hcupData$died[hcupData$died<0]<-NA
hcupData$died=factor(hcupData$died)
test<-names(which.max(table(hcupData$died)))
hcupData$died[is.na(hcupData$died)]<-test
unique(hcupData$died)


#Defining missing values and factorizing dispuniform
hcupData$dispuniform[hcupData$dispuniform<0]<-NA
hcupData$dispuniform=factor(hcupData$dispuniform)
test<-names(which.max(table(hcupData$dispuniform)))
hcupData$dispuniform[is.na(hcupData$dispuniform)]<-test
unique(hcupData$dispuniform)


hcupData$dqtr[hcupData$dqtr<0]=NA
hcupData$dqtr=factor(hcupData$dqtr)
test<-names(which.max(table(hcupData$dqtr)))
hcupData$dqtr[is.na(hcupData$dqtr)]<-test
unique(hcupData$dqtr)


#there are 999 levels for this variable
hcupData$drg[hcupData$drg<0]=NA
hcupData$drg=factor(hcupData$drg)


#3145 levels -This data is rolled up into hcc, so we can ignore
str(hcupData$dx1)

#has 670 levels, same as diagnosis, but better rolled up based on 64 hcc groupings
hcupData$dxccs1[hcupData$dxccs1<0]=NA
hcupData$dxccs1=factor(hcupData$dxccs1)
unique(hcupData$dxccs1)

#2 levels - 0 and 1
hcupData$elective[hcupData$elective<0]=NA
hcupData$elective=factor(hcupData$elective)
test<-names(which.max(table(hcupData$elective)))
hcupData$elective[is.na(hcupData$elective)]<-test
unique(hcupData$elective)


#2 leevels 0 and 1
hcupData$female[hcupData$female<0]=NA
hcupData$female=factor(hcupData$female)
test<-names(which.max(table(hcupData$female)))
hcupData$female[is.na(hcupData$female)]<-test
unique(hcupData$female)


hcupData$hcup_ed[hcupData$hcup_ed<0]=NA
hcupData$hcup_ed=factor(hcupData$hcup_ed)
unique(hcupData$hcup_ed)

hcupData$hospbrth[hcupData$hospbrth<0]=NA
hcupData$hospbrth=factor(hcupData$hospbrth)
unique(hcupData$hospbrth)


hcupData$hosp_division[hcupData$hosp_division<0]=NA
hcupData$hosp_division=factor(hcupData$hosp_division)
unique(hcupData$hosp_division)

#hosp nis number
#need not be very significant in analysis, but can be used for viz
unique(hcupData$hosp_nis)

#los is a continuous value, has to be factorozed
hcupData$los[hcupData$los<0]=NA
summary(hcupData$los)
#impute los to mean
hcupData$los[is.na(hcupData$los)]<-4.558
hcupData$los<-as.integer(hcupData$los)

# next 3 variables have 25 levels
hcupData$mdc[hcupData$mdc<0]=NA
hcupData$mdc=factor(hcupData$mdc)
unique(hcupData$mdc)

#discretize the nchronic variables
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


#196 levels or stratum of hospital
hcupData$nis_stratum[hcupData$nis_stratum<0]=NA
hcupData$nis_stratum=factor(hcupData$nis_stratum)
unique(hcupData$nis_stratum)

unique(hcupData$npr)
hcupData$npr<-impute(hcupData$npr,mean)
hcupData$npr<-as.integer(hcupData$npr)
unique(hcupData$npr)


hcupData$orproc[hcupData$orproc<0]=NA
hcupData$orproc=factor(hcupData$orproc)
unique(hcupData$orproc)

hcupData$pay1[hcupData$pay1<0]=NA
hcupData$pay1=factor(hcupData$pay1)
test<-names(which.max(table(hcupData$pay1)))
hcupData$race[is.na(hcupData$pay1)]<-test
unique(hcupData$pay1)


hcupData$pl_nchs2006[hcupData$pl_nchs2006<0]=NA
hcupData$pl_nchs2006=factor(hcupData$pl_nchs2006)
test<-names(which.max(table(hcupData$pl_nchs2006)))
hcupData$pl_nchs2006[is.na(hcupData$pl_nchs2006)]<-test
unique(hcupData$pl_nchs2006)

# procedures, rolled up to ccs, can be ignored
str(hcupData$pr1)
str(hcupData$prccs1)


hcupData$prday1<-impute(hcupData$prday1,mean)
hcupData$prday1<-as.integer(hcupData$prday1)
unique(hcupData$prday1)

hcupData$race[hcupData$race<0]=NA
hcupData$race=factor(hcupData$race)
test<-names(which.max(table(hcupData$race)))
hcupData$race[is.na(hcupData$race)]<-test
unique(hcupData$race)

#totcharges, values <0 is considered missing
hcupData$totchg[hcupData$totchg<0]=NA
hcupData$totchg[is.na(hcupData$totchg)]<-36970
summary(hcupData$totchg)



hcupData$tran_in[hcupData$tran_in<0]=NA
hcupData$tran_in=factor(hcupData$tran_in)
test<-names(which.max(table(hcupData$tran_in)))
hcupData$tran_in[is.na(hcupData$tran_in)]<-test
unique(hcupData$tran_in)

hcupData$tran_out[hcupData$tran_out<0]=NA
hcupData$tran_out=factor(hcupData$tran_out)
test<-names(which.max(table(hcupData$tran_out)))
hcupData$tran_out[is.na(hcupData$tran_out)]<-test
str(hcupData$tran_out)


#year can be ignored

hcupData$zipinc_qrtl[hcupData$zipinc_qrtl<0]=NA
hcupData$zipinc_qrtl=factor(hcupData$zipinc_qrtl)
test<-names(which.max(table(hcupData$zipinc_qrtl)))
hcupData$zipinc_qrtl[is.na(hcupData$zipinc_qrtl)]<-test
unique(hcupData$zipinc_qrtl)

#ignore dxmccs1 e_mccs1 prmccs1

unique(hcupData$aprdrg)# retained as integer

hcupData$aprdrg_risk_mortality[hcupData$aprdrg_risk_mortality<0]=NA
hcupData$aprdrg_risk_mortality=factor(hcupData$aprdrg_risk_mortality)
unique(hcupData$aprdrg_risk_mortality)



hcupData$aprdrg_severity[hcupData$aprdrg_severity<0]=NA
hcupData$aprdrg_severity=factor(hcupData$aprdrg_severity)
unique(hcupData$aprdrg_severity)



hcupData$hosp_bedsize[hcupData$hosp_bedsize<0]=NA
hcupData$hosp_bedsize=factor(hcupData$hosp_bedsize)
unique(hcupData$hosp_bedsize)



hcupData$hosp_locteach[hcupData$hosp_locteach<0]=NA
hcupData$hosp_locteach=factor(hcupData$hosp_locteach)
unique(hcupData$hosp_locteach)



hcupData$hosp_region[hcupData$hosp_region<0]=NA
hcupData$hosp_region=factor(hcupData$hosp_region)
unique(hcupData$hosp_region)



hcupData$h_contrl[hcupData$h_contrl<0]=NA
hcupData$h_contrl=factor(hcupData$h_contrl)
unique(hcupData$h_contrl)


#retaining n_disc_u, n_hosp_u, s_disc_u, s_hosp_u, total_disc as int

unique(hcupData$chronic_cond_indicator)
hcupData$chronic_cond_indicator[hcupData$chronic_cond_indicator<0]=NA
hcupData$chronic_cond_indicator=factor(hcupData$chronic_cond_indicator)
test<-names(which.max(table(hcupData$chronic_cond_indicator)))
hcupData$chronic_cond_indicator[is.na(hcupData$chronic_cond_indicator)]<-test
unique(hcupData$chronic_cond_indicator)


hcupData$days_from_admission[hcupData$days_from_admission<0]=NA
#impute los to mean
str(hcupData$days_from_admission)
summary(hcupData$days_from_admission)
hcupData$days_from_admission[is.na(hcupData$days_from_admission)]<-2.08
hcupData$days_from_admission<-as.integer(hcupData$days_from_admission)

hcupData$comorb_htn_c[is.na(hcupData$comorb_htn_c)]<-0
hcupData$comorb_htn_c=factor(hcupData$comorb_htn_c)



hcupData$comorb_arth[is.na(hcupData$comorb_arth)]<-0
hcupData$comorb_arth=factor(hcupData$comorb_arth)



hcupData$comorb_perivasc[is.na(hcupData$comorb_perivasc)]<-0
hcupData$comorb_perivasc=factor(hcupData$comorb_perivasc)



hcupData$comorb_dmcx[is.na(hcupData$comorb_dmcx)]<-0
hcupData$comorb_dmcx=factor(hcupData$comorb_dmcx)



hcupData$comorb_pulmcirc[is.na(hcupData$comorb_pulmcirc)]<-0
hcupData$comorb_pulmcirc=factor(hcupData$comorb_pulmcirc)



hcupData$comorb_liver[is.na(hcupData$comorb_liver)]<-0
hcupData$comorb_liver=factor(hcupData$comorb_liver)



hcupData$comorb_drug[is.na(hcupData$comorb_drug)]<-0
hcupData$comorb_drug=factor(hcupData$comorb_drug)



hcupData$comorb_neuro[is.na(hcupData$comorb_neuro)]<-0
hcupData$comorb_neuro=factor(hcupData$comorb_neuro)



hcupData$comorb_chf[is.na(hcupData$comorb_chf)]<-0
hcupData$comorb_chf=factor(hcupData$comorb_chf)



hcupData$comorb_renlfail[is.na(hcupData$comorb_renlfail)]<-0
hcupData$comorb_renlfail=factor(hcupData$comorb_renlfail)



hcupData$comorb_wghtloss[is.na(hcupData$comorb_wghtloss)]<-0
hcupData$comorb_wghtloss=factor(hcupData$comorb_wghtloss)



hcupData$comorb_ulcer[is.na(hcupData$comorb_ulcer)]<-0
hcupData$comorb_ulcer=factor(hcupData$comorb_ulcer)



hcupData$comorb_obese[is.na(hcupData$comorb_obese)]<-0
hcupData$comorb_obese=factor(hcupData$comorb_obese)



hcupData$comorb_chrnlung[is.na(hcupData$comorb_chrnlung)]<-0
hcupData$comorb_chrnlung=factor(hcupData$comorb_chrnlung)



hcupData$comorb_mets[is.na(hcupData$comorb_mets)]<-0
hcupData$comorb_mets=factor(hcupData$comorb_mets)



hcupData$comorb_dm[is.na(hcupData$comorb_dm)]<-0
hcupData$comorb_dm=factor(hcupData$comorb_dm)



hcupData$comorb_lymph[is.na(hcupData$comorb_lymph)]<-0
hcupData$comorb_lymph=factor(hcupData$comorb_lymph)



hcupData$comorb_tumor[is.na(hcupData$comorb_tumor)]<-0
hcupData$comorb_tumor=factor(hcupData$comorb_tumor)



hcupData$comorb_aids[is.na(hcupData$comorb_aids)]<-0
hcupData$comorb_aids=factor(hcupData$comorb_aids)



hcupData$comorb_hypothy[is.na(hcupData$comorb_hypothy)]<-0
hcupData$comorb_hypothy=factor(hcupData$comorb_hypothy)



hcupData$comorb_alcohol[is.na(hcupData$comorb_alcohol)]<-0
hcupData$comorb_alcohol=factor(hcupData$comorb_alcohol)



hcupData$comorb_valve[is.na(hcupData$comorb_valve)]<-0
hcupData$comorb_valve=factor(hcupData$comorb_valve)



hcupData$comorb_depress[is.na(hcupData$comorb_depress)]<-0
hcupData$comorb_depress=factor(hcupData$comorb_depress)



hcupData$comorb_lytes[is.na(hcupData$comorb_lytes)]<-0
hcupData$comorb_lytes=factor(hcupData$comorb_lytes)



hcupData$comorb_para[is.na(hcupData$comorb_para)]<-0
hcupData$comorb_para=factor(hcupData$comorb_para)



hcupData$comorb_coag[is.na(hcupData$comorb_coag)]<-0
hcupData$comorb_coag=factor(hcupData$comorb_coag)



hcupData$comorb_psych[is.na(hcupData$comorb_psych)]<-0
hcupData$comorb_psych=factor(hcupData$comorb_psych)



hcupData$comorb_anemdef[is.na(hcupData$comorb_anemdef)]<-0
hcupData$comorb_anemdef=factor(hcupData$comorb_anemdef)



hcupData$comorb_bldloss[is.na(hcupData$comorb_bldloss)]<-0
hcupData$comorb_bldloss=factor(hcupData$comorb_bldloss)



hcupData$procccs_1[is.na(hcupData$procccs_1)]<-0
hcupData$procccs_1=factor(hcupData$procccs_1)



hcupData$procccs_2[is.na(hcupData$procccs_2)]<-0
hcupData$procccs_2=factor(hcupData$procccs_2)



hcupData$procccs_3[is.na(hcupData$procccs_3)]<-0
hcupData$procccs_3=factor(hcupData$procccs_3)



hcupData$procccs_4[is.na(hcupData$procccs_4)]<-0
hcupData$procccs_4=factor(hcupData$procccs_4)



hcupData$procccs_5[is.na(hcupData$procccs_5)]<-0
hcupData$procccs_5=factor(hcupData$procccs_5)



hcupData$procccs_6[is.na(hcupData$procccs_6)]<-0
hcupData$procccs_6=factor(hcupData$procccs_6)



hcupData$procccs_7[is.na(hcupData$procccs_7)]<-0
hcupData$procccs_7=factor(hcupData$procccs_7)



hcupData$procccs_8[is.na(hcupData$procccs_8)]<-0
hcupData$procccs_8=factor(hcupData$procccs_8)



hcupData$procccs_9[is.na(hcupData$procccs_9)]<-0
hcupData$procccs_9=factor(hcupData$procccs_9)



hcupData$procccs_10[is.na(hcupData$procccs_10)]<-0
hcupData$procccs_10=factor(hcupData$procccs_10)



hcupData$procccs_11[is.na(hcupData$procccs_11)]<-0
hcupData$procccs_11=factor(hcupData$procccs_11)



hcupData$procccs_12[is.na(hcupData$procccs_12)]<-0
hcupData$procccs_12=factor(hcupData$procccs_12)



hcupData$procccs_13[is.na(hcupData$procccs_13)]<-0
hcupData$procccs_13=factor(hcupData$procccs_13)



hcupData$procccs_14[is.na(hcupData$procccs_14)]<-0
hcupData$procccs_14=factor(hcupData$procccs_14)



hcupData$procccs_15[is.na(hcupData$procccs_15)]<-0
hcupData$procccs_15=factor(hcupData$procccs_15)



hcupData$procccs_16[is.na(hcupData$procccs_16)]<-0
hcupData$procccs_16=factor(hcupData$procccs_16)



hcupData$procccs_17[is.na(hcupData$procccs_17)]<-0
hcupData$procccs_17=factor(hcupData$procccs_17)



hcupData$procccs_18[is.na(hcupData$procccs_18)]<-0
hcupData$procccs_18=factor(hcupData$procccs_18)



hcupData$procccs_19[is.na(hcupData$procccs_19)]<-0
hcupData$procccs_19=factor(hcupData$procccs_19)



hcupData$procccs_20[is.na(hcupData$procccs_20)]<-0
hcupData$procccs_20=factor(hcupData$procccs_20)



hcupData$procccs_21[is.na(hcupData$procccs_21)]<-0
hcupData$procccs_21=factor(hcupData$procccs_21)



hcupData$procccs_22[is.na(hcupData$procccs_22)]<-0
hcupData$procccs_22=factor(hcupData$procccs_22)



hcupData$procccs_23[is.na(hcupData$procccs_23)]<-0
hcupData$procccs_23=factor(hcupData$procccs_23)



hcupData$procccs_24[is.na(hcupData$procccs_24)]<-0
hcupData$procccs_24=factor(hcupData$procccs_24)



hcupData$procccs_25[is.na(hcupData$procccs_25)]<-0
hcupData$procccs_25=factor(hcupData$procccs_25)



hcupData$procccs_26[is.na(hcupData$procccs_26)]<-0
hcupData$procccs_26=factor(hcupData$procccs_26)



hcupData$procccs_27[is.na(hcupData$procccs_27)]<-0
hcupData$procccs_27=factor(hcupData$procccs_27)



hcupData$procccs_28[is.na(hcupData$procccs_28)]<-0
hcupData$procccs_28=factor(hcupData$procccs_28)



hcupData$procccs_29[is.na(hcupData$procccs_29)]<-0
hcupData$procccs_29=factor(hcupData$procccs_29)



hcupData$procccs_30[is.na(hcupData$procccs_30)]<-0
hcupData$procccs_30=factor(hcupData$procccs_30)



hcupData$procccs_31[is.na(hcupData$procccs_31)]<-0
hcupData$procccs_31=factor(hcupData$procccs_31)



hcupData$procccs_32[is.na(hcupData$procccs_32)]<-0
hcupData$procccs_32=factor(hcupData$procccs_32)



hcupData$procccs_33[is.na(hcupData$procccs_33)]<-0
hcupData$procccs_33=factor(hcupData$procccs_33)



hcupData$procccs_34[is.na(hcupData$procccs_34)]<-0
hcupData$procccs_34=factor(hcupData$procccs_34)



hcupData$procccs_35[is.na(hcupData$procccs_35)]<-0
hcupData$procccs_35=factor(hcupData$procccs_35)



hcupData$procccs_36[is.na(hcupData$procccs_36)]<-0
hcupData$procccs_36=factor(hcupData$procccs_36)



hcupData$procccs_37[is.na(hcupData$procccs_37)]<-0
hcupData$procccs_37=factor(hcupData$procccs_37)



hcupData$procccs_38[is.na(hcupData$procccs_38)]<-0
hcupData$procccs_38=factor(hcupData$procccs_38)



hcupData$procccs_39[is.na(hcupData$procccs_39)]<-0
hcupData$procccs_39=factor(hcupData$procccs_39)



hcupData$procccs_40[is.na(hcupData$procccs_40)]<-0
hcupData$procccs_40=factor(hcupData$procccs_40)



hcupData$procccs_41[is.na(hcupData$procccs_41)]<-0
hcupData$procccs_41=factor(hcupData$procccs_41)



hcupData$procccs_42[is.na(hcupData$procccs_42)]<-0
hcupData$procccs_42=factor(hcupData$procccs_42)



hcupData$procccs_43[is.na(hcupData$procccs_43)]<-0
hcupData$procccs_43=factor(hcupData$procccs_43)



hcupData$procccs_44[is.na(hcupData$procccs_44)]<-0
hcupData$procccs_44=factor(hcupData$procccs_44)



hcupData$procccs_45[is.na(hcupData$procccs_45)]<-0
hcupData$procccs_45=factor(hcupData$procccs_45)



hcupData$procccs_46[is.na(hcupData$procccs_46)]<-0
hcupData$procccs_46=factor(hcupData$procccs_46)



hcupData$procccs_47[is.na(hcupData$procccs_47)]<-0
hcupData$procccs_47=factor(hcupData$procccs_47)



hcupData$procccs_48[is.na(hcupData$procccs_48)]<-0
hcupData$procccs_48=factor(hcupData$procccs_48)



hcupData$procccs_49[is.na(hcupData$procccs_49)]<-0
hcupData$procccs_49=factor(hcupData$procccs_49)



hcupData$procccs_50[is.na(hcupData$procccs_50)]<-0
hcupData$procccs_50=factor(hcupData$procccs_50)



hcupData$procccs_51[is.na(hcupData$procccs_51)]<-0
hcupData$procccs_51=factor(hcupData$procccs_51)



hcupData$procccs_52[is.na(hcupData$procccs_52)]<-0
hcupData$procccs_52=factor(hcupData$procccs_52)



hcupData$procccs_53[is.na(hcupData$procccs_53)]<-0
hcupData$procccs_53=factor(hcupData$procccs_53)



hcupData$procccs_54[is.na(hcupData$procccs_54)]<-0
hcupData$procccs_54=factor(hcupData$procccs_54)



hcupData$procccs_55[is.na(hcupData$procccs_55)]<-0
hcupData$procccs_55=factor(hcupData$procccs_55)



hcupData$procccs_56[is.na(hcupData$procccs_56)]<-0
hcupData$procccs_56=factor(hcupData$procccs_56)



hcupData$procccs_57[is.na(hcupData$procccs_57)]<-0
hcupData$procccs_57=factor(hcupData$procccs_57)



hcupData$procccs_58[is.na(hcupData$procccs_58)]<-0
hcupData$procccs_58=factor(hcupData$procccs_58)



hcupData$procccs_59[is.na(hcupData$procccs_59)]<-0
hcupData$procccs_59=factor(hcupData$procccs_59)



hcupData$procccs_60[is.na(hcupData$procccs_60)]<-0
hcupData$procccs_60=factor(hcupData$procccs_60)



hcupData$procccs_61[is.na(hcupData$procccs_61)]<-0
hcupData$procccs_61=factor(hcupData$procccs_61)



hcupData$procccs_62[is.na(hcupData$procccs_62)]<-0
hcupData$procccs_62=factor(hcupData$procccs_62)



hcupData$procccs_63[is.na(hcupData$procccs_63)]<-0
hcupData$procccs_63=factor(hcupData$procccs_63)



hcupData$procccs_64[is.na(hcupData$procccs_64)]<-0
hcupData$procccs_64=factor(hcupData$procccs_64)



hcupData$procccs_65[is.na(hcupData$procccs_65)]<-0
hcupData$procccs_65=factor(hcupData$procccs_65)



hcupData$procccs_66[is.na(hcupData$procccs_66)]<-0
hcupData$procccs_66=factor(hcupData$procccs_66)



hcupData$procccs_67[is.na(hcupData$procccs_67)]<-0
hcupData$procccs_67=factor(hcupData$procccs_67)



hcupData$procccs_68[is.na(hcupData$procccs_68)]<-0
hcupData$procccs_68=factor(hcupData$procccs_68)



hcupData$procccs_69[is.na(hcupData$procccs_69)]<-0
hcupData$procccs_69=factor(hcupData$procccs_69)



hcupData$procccs_70[is.na(hcupData$procccs_70)]<-0
hcupData$procccs_70=factor(hcupData$procccs_70)



hcupData$procccs_71[is.na(hcupData$procccs_71)]<-0
hcupData$procccs_71=factor(hcupData$procccs_71)



hcupData$procccs_72[is.na(hcupData$procccs_72)]<-0
hcupData$procccs_72=factor(hcupData$procccs_72)



hcupData$procccs_73[is.na(hcupData$procccs_73)]<-0
hcupData$procccs_73=factor(hcupData$procccs_73)



hcupData$procccs_74[is.na(hcupData$procccs_74)]<-0
hcupData$procccs_74=factor(hcupData$procccs_74)



hcupData$procccs_75[is.na(hcupData$procccs_75)]<-0
hcupData$procccs_75=factor(hcupData$procccs_75)



hcupData$procccs_76[is.na(hcupData$procccs_76)]<-0
hcupData$procccs_76=factor(hcupData$procccs_76)



hcupData$procccs_77[is.na(hcupData$procccs_77)]<-0
hcupData$procccs_77=factor(hcupData$procccs_77)



hcupData$procccs_78[is.na(hcupData$procccs_78)]<-0
hcupData$procccs_78=factor(hcupData$procccs_78)



hcupData$procccs_79[is.na(hcupData$procccs_79)]<-0
hcupData$procccs_79=factor(hcupData$procccs_79)



hcupData$procccs_80[is.na(hcupData$procccs_80)]<-0
hcupData$procccs_80=factor(hcupData$procccs_80)



hcupData$procccs_81[is.na(hcupData$procccs_81)]<-0
hcupData$procccs_81=factor(hcupData$procccs_81)



hcupData$procccs_82[is.na(hcupData$procccs_82)]<-0
hcupData$procccs_82=factor(hcupData$procccs_82)



hcupData$procccs_83[is.na(hcupData$procccs_83)]<-0
hcupData$procccs_83=factor(hcupData$procccs_83)



hcupData$procccs_84[is.na(hcupData$procccs_84)]<-0
hcupData$procccs_84=factor(hcupData$procccs_84)



hcupData$procccs_85[is.na(hcupData$procccs_85)]<-0
hcupData$procccs_85=factor(hcupData$procccs_85)



hcupData$procccs_86[is.na(hcupData$procccs_86)]<-0
hcupData$procccs_86=factor(hcupData$procccs_86)



hcupData$procccs_87[is.na(hcupData$procccs_87)]<-0
hcupData$procccs_87=factor(hcupData$procccs_87)



hcupData$procccs_88[is.na(hcupData$procccs_88)]<-0
hcupData$procccs_88=factor(hcupData$procccs_88)



hcupData$procccs_89[is.na(hcupData$procccs_89)]<-0
hcupData$procccs_89=factor(hcupData$procccs_89)



hcupData$procccs_90[is.na(hcupData$procccs_90)]<-0
hcupData$procccs_90=factor(hcupData$procccs_90)



hcupData$procccs_91[is.na(hcupData$procccs_91)]<-0
hcupData$procccs_91=factor(hcupData$procccs_91)



hcupData$procccs_92[is.na(hcupData$procccs_92)]<-0
hcupData$procccs_92=factor(hcupData$procccs_92)



hcupData$procccs_93[is.na(hcupData$procccs_93)]<-0
hcupData$procccs_93=factor(hcupData$procccs_93)



hcupData$procccs_94[is.na(hcupData$procccs_94)]<-0
hcupData$procccs_94=factor(hcupData$procccs_94)



hcupData$procccs_95[is.na(hcupData$procccs_95)]<-0
hcupData$procccs_95=factor(hcupData$procccs_95)



hcupData$procccs_96[is.na(hcupData$procccs_96)]<-0
hcupData$procccs_96=factor(hcupData$procccs_96)



hcupData$procccs_97[is.na(hcupData$procccs_97)]<-0
hcupData$procccs_97=factor(hcupData$procccs_97)



hcupData$procccs_98[is.na(hcupData$procccs_98)]<-0
hcupData$procccs_98=factor(hcupData$procccs_98)



hcupData$procccs_99[is.na(hcupData$procccs_99)]<-0
hcupData$procccs_99=factor(hcupData$procccs_99)



hcupData$procccs_100[is.na(hcupData$procccs_100)]<-0
hcupData$procccs_100=factor(hcupData$procccs_100)



hcupData$procccs_101[is.na(hcupData$procccs_101)]<-0
hcupData$procccs_101=factor(hcupData$procccs_101)



hcupData$procccs_102[is.na(hcupData$procccs_102)]<-0
hcupData$procccs_102=factor(hcupData$procccs_102)



hcupData$procccs_103[is.na(hcupData$procccs_103)]<-0
hcupData$procccs_103=factor(hcupData$procccs_103)



hcupData$procccs_104[is.na(hcupData$procccs_104)]<-0
hcupData$procccs_104=factor(hcupData$procccs_104)



hcupData$procccs_105[is.na(hcupData$procccs_105)]<-0
hcupData$procccs_105=factor(hcupData$procccs_105)



hcupData$procccs_106[is.na(hcupData$procccs_106)]<-0
hcupData$procccs_106=factor(hcupData$procccs_106)



hcupData$procccs_107[is.na(hcupData$procccs_107)]<-0
hcupData$procccs_107=factor(hcupData$procccs_107)



hcupData$procccs_108[is.na(hcupData$procccs_108)]<-0
hcupData$procccs_108=factor(hcupData$procccs_108)



hcupData$procccs_109[is.na(hcupData$procccs_109)]<-0
hcupData$procccs_109=factor(hcupData$procccs_109)



hcupData$procccs_110[is.na(hcupData$procccs_110)]<-0
hcupData$procccs_110=factor(hcupData$procccs_110)



hcupData$procccs_111[is.na(hcupData$procccs_111)]<-0
hcupData$procccs_111=factor(hcupData$procccs_111)



hcupData$procccs_112[is.na(hcupData$procccs_112)]<-0
hcupData$procccs_112=factor(hcupData$procccs_112)



hcupData$procccs_113[is.na(hcupData$procccs_113)]<-0
hcupData$procccs_113=factor(hcupData$procccs_113)



hcupData$procccs_114[is.na(hcupData$procccs_114)]<-0
hcupData$procccs_114=factor(hcupData$procccs_114)



hcupData$procccs_115[is.na(hcupData$procccs_115)]<-0
hcupData$procccs_115=factor(hcupData$procccs_115)



hcupData$procccs_116[is.na(hcupData$procccs_116)]<-0
hcupData$procccs_116=factor(hcupData$procccs_116)



hcupData$procccs_117[is.na(hcupData$procccs_117)]<-0
hcupData$procccs_117=factor(hcupData$procccs_117)



hcupData$procccs_118[is.na(hcupData$procccs_118)]<-0
hcupData$procccs_118=factor(hcupData$procccs_118)



hcupData$procccs_119[is.na(hcupData$procccs_119)]<-0
hcupData$procccs_119=factor(hcupData$procccs_119)



hcupData$procccs_120[is.na(hcupData$procccs_120)]<-0
hcupData$procccs_120=factor(hcupData$procccs_120)



hcupData$procccs_121[is.na(hcupData$procccs_121)]<-0
hcupData$procccs_121=factor(hcupData$procccs_121)



hcupData$procccs_122[is.na(hcupData$procccs_122)]<-0
hcupData$procccs_122=factor(hcupData$procccs_122)



hcupData$procccs_123[is.na(hcupData$procccs_123)]<-0
hcupData$procccs_123=factor(hcupData$procccs_123)



hcupData$procccs_124[is.na(hcupData$procccs_124)]<-0
hcupData$procccs_124=factor(hcupData$procccs_124)



hcupData$procccs_125[is.na(hcupData$procccs_125)]<-0
hcupData$procccs_125=factor(hcupData$procccs_125)



hcupData$procccs_126[is.na(hcupData$procccs_126)]<-0
hcupData$procccs_126=factor(hcupData$procccs_126)



hcupData$procccs_127[is.na(hcupData$procccs_127)]<-0
hcupData$procccs_127=factor(hcupData$procccs_127)



hcupData$procccs_128[is.na(hcupData$procccs_128)]<-0
hcupData$procccs_128=factor(hcupData$procccs_128)



hcupData$procccs_129[is.na(hcupData$procccs_129)]<-0
hcupData$procccs_129=factor(hcupData$procccs_129)



hcupData$procccs_130[is.na(hcupData$procccs_130)]<-0
hcupData$procccs_130=factor(hcupData$procccs_130)



hcupData$procccs_131[is.na(hcupData$procccs_131)]<-0
hcupData$procccs_131=factor(hcupData$procccs_131)



hcupData$procccs_132[is.na(hcupData$procccs_132)]<-0
hcupData$procccs_132=factor(hcupData$procccs_132)



hcupData$procccs_133[is.na(hcupData$procccs_133)]<-0
hcupData$procccs_133=factor(hcupData$procccs_133)



hcupData$procccs_134[is.na(hcupData$procccs_134)]<-0
hcupData$procccs_134=factor(hcupData$procccs_134)



hcupData$procccs_135[is.na(hcupData$procccs_135)]<-0
hcupData$procccs_135=factor(hcupData$procccs_135)



hcupData$procccs_136[is.na(hcupData$procccs_136)]<-0
hcupData$procccs_136=factor(hcupData$procccs_136)



hcupData$procccs_137[is.na(hcupData$procccs_137)]<-0
hcupData$procccs_137=factor(hcupData$procccs_137)



hcupData$procccs_138[is.na(hcupData$procccs_138)]<-0
hcupData$procccs_138=factor(hcupData$procccs_138)



hcupData$procccs_139[is.na(hcupData$procccs_139)]<-0
hcupData$procccs_139=factor(hcupData$procccs_139)



hcupData$procccs_140[is.na(hcupData$procccs_140)]<-0
hcupData$procccs_140=factor(hcupData$procccs_140)



hcupData$procccs_141[is.na(hcupData$procccs_141)]<-0
hcupData$procccs_141=factor(hcupData$procccs_141)



hcupData$procccs_142[is.na(hcupData$procccs_142)]<-0
hcupData$procccs_142=factor(hcupData$procccs_142)



hcupData$procccs_143[is.na(hcupData$procccs_143)]<-0
hcupData$procccs_143=factor(hcupData$procccs_143)



hcupData$procccs_144[is.na(hcupData$procccs_144)]<-0
hcupData$procccs_144=factor(hcupData$procccs_144)



hcupData$procccs_145[is.na(hcupData$procccs_145)]<-0
hcupData$procccs_145=factor(hcupData$procccs_145)



hcupData$procccs_146[is.na(hcupData$procccs_146)]<-0
hcupData$procccs_146=factor(hcupData$procccs_146)



hcupData$procccs_147[is.na(hcupData$procccs_147)]<-0
hcupData$procccs_147=factor(hcupData$procccs_147)



hcupData$procccs_148[is.na(hcupData$procccs_148)]<-0
hcupData$procccs_148=factor(hcupData$procccs_148)



hcupData$procccs_149[is.na(hcupData$procccs_149)]<-0
hcupData$procccs_149=factor(hcupData$procccs_149)



hcupData$procccs_150[is.na(hcupData$procccs_150)]<-0
hcupData$procccs_150=factor(hcupData$procccs_150)



hcupData$procccs_151[is.na(hcupData$procccs_151)]<-0
hcupData$procccs_151=factor(hcupData$procccs_151)



hcupData$procccs_152[is.na(hcupData$procccs_152)]<-0
hcupData$procccs_152=factor(hcupData$procccs_152)



hcupData$procccs_153[is.na(hcupData$procccs_153)]<-0
hcupData$procccs_153=factor(hcupData$procccs_153)



hcupData$procccs_154[is.na(hcupData$procccs_154)]<-0
hcupData$procccs_154=factor(hcupData$procccs_154)



hcupData$procccs_155[is.na(hcupData$procccs_155)]<-0
hcupData$procccs_155=factor(hcupData$procccs_155)



hcupData$procccs_156[is.na(hcupData$procccs_156)]<-0
hcupData$procccs_156=factor(hcupData$procccs_156)



hcupData$procccs_157[is.na(hcupData$procccs_157)]<-0
hcupData$procccs_157=factor(hcupData$procccs_157)



hcupData$procccs_158[is.na(hcupData$procccs_158)]<-0
hcupData$procccs_158=factor(hcupData$procccs_158)



hcupData$procccs_159[is.na(hcupData$procccs_159)]<-0
hcupData$procccs_159=factor(hcupData$procccs_159)



hcupData$procccs_160[is.na(hcupData$procccs_160)]<-0
hcupData$procccs_160=factor(hcupData$procccs_160)



hcupData$procccs_161[is.na(hcupData$procccs_161)]<-0
hcupData$procccs_161=factor(hcupData$procccs_161)



hcupData$procccs_162[is.na(hcupData$procccs_162)]<-0
hcupData$procccs_162=factor(hcupData$procccs_162)



hcupData$procccs_163[is.na(hcupData$procccs_163)]<-0
hcupData$procccs_163=factor(hcupData$procccs_163)



hcupData$procccs_164[is.na(hcupData$procccs_164)]<-0
hcupData$procccs_164=factor(hcupData$procccs_164)



hcupData$procccs_165[is.na(hcupData$procccs_165)]<-0
hcupData$procccs_165=factor(hcupData$procccs_165)



hcupData$procccs_166[is.na(hcupData$procccs_166)]<-0
hcupData$procccs_166=factor(hcupData$procccs_166)



hcupData$procccs_167[is.na(hcupData$procccs_167)]<-0
hcupData$procccs_167=factor(hcupData$procccs_167)



hcupData$procccs_168[is.na(hcupData$procccs_168)]<-0
hcupData$procccs_168=factor(hcupData$procccs_168)



hcupData$procccs_169[is.na(hcupData$procccs_169)]<-0
hcupData$procccs_169=factor(hcupData$procccs_169)



hcupData$procccs_170[is.na(hcupData$procccs_170)]<-0
hcupData$procccs_170=factor(hcupData$procccs_170)



hcupData$procccs_171[is.na(hcupData$procccs_171)]<-0
hcupData$procccs_171=factor(hcupData$procccs_171)



hcupData$procccs_172[is.na(hcupData$procccs_172)]<-0
hcupData$procccs_172=factor(hcupData$procccs_172)



hcupData$procccs_173[is.na(hcupData$procccs_173)]<-0
hcupData$procccs_173=factor(hcupData$procccs_173)



hcupData$procccs_174[is.na(hcupData$procccs_174)]<-0
hcupData$procccs_174=factor(hcupData$procccs_174)



hcupData$procccs_175[is.na(hcupData$procccs_175)]<-0
hcupData$procccs_175=factor(hcupData$procccs_175)



hcupData$procccs_176[is.na(hcupData$procccs_176)]<-0
hcupData$procccs_176=factor(hcupData$procccs_176)



hcupData$procccs_177[is.na(hcupData$procccs_177)]<-0
hcupData$procccs_177=factor(hcupData$procccs_177)



hcupData$procccs_178[is.na(hcupData$procccs_178)]<-0
hcupData$procccs_178=factor(hcupData$procccs_178)



hcupData$procccs_179[is.na(hcupData$procccs_179)]<-0
hcupData$procccs_179=factor(hcupData$procccs_179)



hcupData$procccs_180[is.na(hcupData$procccs_180)]<-0
hcupData$procccs_180=factor(hcupData$procccs_180)



hcupData$procccs_181[is.na(hcupData$procccs_181)]<-0
hcupData$procccs_181=factor(hcupData$procccs_181)



hcupData$procccs_182[is.na(hcupData$procccs_182)]<-0
hcupData$procccs_182=factor(hcupData$procccs_182)



hcupData$procccs_183[is.na(hcupData$procccs_183)]<-0
hcupData$procccs_183=factor(hcupData$procccs_183)



hcupData$procccs_184[is.na(hcupData$procccs_184)]<-0
hcupData$procccs_184=factor(hcupData$procccs_184)



hcupData$procccs_185[is.na(hcupData$procccs_185)]<-0
hcupData$procccs_185=factor(hcupData$procccs_185)



hcupData$procccs_186[is.na(hcupData$procccs_186)]<-0
hcupData$procccs_186=factor(hcupData$procccs_186)



hcupData$procccs_187[is.na(hcupData$procccs_187)]<-0
hcupData$procccs_187=factor(hcupData$procccs_187)



hcupData$procccs_188[is.na(hcupData$procccs_188)]<-0
hcupData$procccs_188=factor(hcupData$procccs_188)



hcupData$procccs_189[is.na(hcupData$procccs_189)]<-0
hcupData$procccs_189=factor(hcupData$procccs_189)



hcupData$procccs_190[is.na(hcupData$procccs_190)]<-0
hcupData$procccs_190=factor(hcupData$procccs_190)



hcupData$procccs_191[is.na(hcupData$procccs_191)]<-0
hcupData$procccs_191=factor(hcupData$procccs_191)



hcupData$procccs_192[is.na(hcupData$procccs_192)]<-0
hcupData$procccs_192=factor(hcupData$procccs_192)



hcupData$procccs_193[is.na(hcupData$procccs_193)]<-0
hcupData$procccs_193=factor(hcupData$procccs_193)



hcupData$procccs_194[is.na(hcupData$procccs_194)]<-0
hcupData$procccs_194=factor(hcupData$procccs_194)



hcupData$procccs_195[is.na(hcupData$procccs_195)]<-0
hcupData$procccs_195=factor(hcupData$procccs_195)



hcupData$procccs_196[is.na(hcupData$procccs_196)]<-0
hcupData$procccs_196=factor(hcupData$procccs_196)



hcupData$procccs_197[is.na(hcupData$procccs_197)]<-0
hcupData$procccs_197=factor(hcupData$procccs_197)



hcupData$procccs_198[is.na(hcupData$procccs_198)]<-0
hcupData$procccs_198=factor(hcupData$procccs_198)



hcupData$procccs_199[is.na(hcupData$procccs_199)]<-0
hcupData$procccs_199=factor(hcupData$procccs_199)



hcupData$procccs_200[is.na(hcupData$procccs_200)]<-0
hcupData$procccs_200=factor(hcupData$procccs_200)



hcupData$procccs_201[is.na(hcupData$procccs_201)]<-0
hcupData$procccs_201=factor(hcupData$procccs_201)



hcupData$procccs_202[is.na(hcupData$procccs_202)]<-0
hcupData$procccs_202=factor(hcupData$procccs_202)



hcupData$procccs_203[is.na(hcupData$procccs_203)]<-0
hcupData$procccs_203=factor(hcupData$procccs_203)



hcupData$procccs_204[is.na(hcupData$procccs_204)]<-0
hcupData$procccs_204=factor(hcupData$procccs_204)



hcupData$procccs_205[is.na(hcupData$procccs_205)]<-0
hcupData$procccs_205=factor(hcupData$procccs_205)



hcupData$procccs_206[is.na(hcupData$procccs_206)]<-0
hcupData$procccs_206=factor(hcupData$procccs_206)



hcupData$procccs_207[is.na(hcupData$procccs_207)]<-0
hcupData$procccs_207=factor(hcupData$procccs_207)



hcupData$procccs_208[is.na(hcupData$procccs_208)]<-0
hcupData$procccs_208=factor(hcupData$procccs_208)



hcupData$procccs_209[is.na(hcupData$procccs_209)]<-0
hcupData$procccs_209=factor(hcupData$procccs_209)



hcupData$procccs_210[is.na(hcupData$procccs_210)]<-0
hcupData$procccs_210=factor(hcupData$procccs_210)



hcupData$procccs_211[is.na(hcupData$procccs_211)]<-0
hcupData$procccs_211=factor(hcupData$procccs_211)



hcupData$procccs_212[is.na(hcupData$procccs_212)]<-0
hcupData$procccs_212=factor(hcupData$procccs_212)



hcupData$procccs_213[is.na(hcupData$procccs_213)]<-0
hcupData$procccs_213=factor(hcupData$procccs_213)



hcupData$procccs_214[is.na(hcupData$procccs_214)]<-0
hcupData$procccs_214=factor(hcupData$procccs_214)



hcupData$procccs_215[is.na(hcupData$procccs_215)]<-0
hcupData$procccs_215=factor(hcupData$procccs_215)



hcupData$procccs_216[is.na(hcupData$procccs_216)]<-0
hcupData$procccs_216=factor(hcupData$procccs_216)



hcupData$procccs_217[is.na(hcupData$procccs_217)]<-0
hcupData$procccs_217=factor(hcupData$procccs_217)



hcupData$procccs_218[is.na(hcupData$procccs_218)]<-0
hcupData$procccs_218=factor(hcupData$procccs_218)



hcupData$procccs_219[is.na(hcupData$procccs_219)]<-0
hcupData$procccs_219=factor(hcupData$procccs_219)



hcupData$procccs_220[is.na(hcupData$procccs_220)]<-0
hcupData$procccs_220=factor(hcupData$procccs_220)



hcupData$procccs_221[is.na(hcupData$procccs_221)]<-0
hcupData$procccs_221=factor(hcupData$procccs_221)



hcupData$procccs_222[is.na(hcupData$procccs_222)]<-0
hcupData$procccs_222=factor(hcupData$procccs_222)



hcupData$procccs_223[is.na(hcupData$procccs_223)]<-0
hcupData$procccs_223=factor(hcupData$procccs_223)



hcupData$procccs_224[is.na(hcupData$procccs_224)]<-0
hcupData$procccs_224=factor(hcupData$procccs_224)



hcupData$procccs_225[is.na(hcupData$procccs_225)]<-0
hcupData$procccs_225=factor(hcupData$procccs_225)



hcupData$procccs_226[is.na(hcupData$procccs_226)]<-0
hcupData$procccs_226=factor(hcupData$procccs_226)



hcupData$procccs_227[is.na(hcupData$procccs_227)]<-0
hcupData$procccs_227=factor(hcupData$procccs_227)



hcupData$procccs_228[is.na(hcupData$procccs_228)]<-0
hcupData$procccs_228=factor(hcupData$procccs_228)



hcupData$procccs_229[is.na(hcupData$procccs_229)]<-0
hcupData$procccs_229=factor(hcupData$procccs_229)



hcupData$procccs_230[is.na(hcupData$procccs_230)]<-0
hcupData$procccs_230=factor(hcupData$procccs_230)



hcupData$procccs_231[is.na(hcupData$procccs_231)]<-0
hcupData$procccs_231=factor(hcupData$procccs_231)


save(hcupData,file="hcupData.Rda")


#X------------------------DATA LOADING IS COMPLETE--------------------------X

library(VIM)
ggr_plot <- aggr(hcupData, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.5, gap=3, ylab=c("Histogram of missing data","Pattern"))


library(FSelector)

#remove duplicate and irrelevant fields


hcupData<-subset(hcupData,select=-c(discwt,drg,drg24,drgver,drg_nopoa,dx1,dxccs1,hosp_nis,mdc24,mdc_nopoa,pr1,prccs1,year,dxmccs1,e_mccs1,prmccs1,aprdrg,n_disc_u,n_hosp_u,s_disc_u,s_hosp_u,total_disc,nis_stratum))
hcupData$logtotchg<-log(hcupData$totchg)
hcupData<-subset(hcupData,select=-c(totchg,los,dispuniform,prday1,days_from_admission))
hcupData<-subset(hcupData,select=-c(tran_out))

weights_1 <- information.gain(logtotchg~., hcupData)
print(weights_1)

regrsubset <- cutoff.k(weights_1, 35)
hcupreg_subset<-subset(hcupData,select=regrsubset)
hcupreg_subset$logtotchg<-hcupData$logtotchg

names(hcupreg_subset)
#library(corrgram)
#corrgram(hcupreg_subset, order=NULL, lower.panel=panel.shade,
#        upper.panel=panel.ellipse, text.panel=panel.txt,
#       main="data sorted")

#cor(hcupreg_subset[sapply(hcupreg_subset, is.numeric)])
#library(PerformanceAnalytics)

#chart.Correlation(hcupreg_subset[sapply(hcupreg_subset, is.numeric)])

hcupreg_subset=subset(hcupreg_subset,select=-c(nchronic,aprdrg_risk_mortality))

indexes = sample(1:nrow(hcupreg_subset), size=0.2*nrow(hcupreg_subset))
hcupRegtest=hcupreg_subset[indexes,]
nrow(hcupRegtest)
hcupRegtrainandval=hcupreg_subset[-indexes,]
nrow(hcupRegtrainandval)

indexes = sample(1:nrow(hcupRegtrainandval), size=0.2*nrow(hcupRegtrainandval))
hcupRegval=hcupRegtrainandval[indexes,]
nrow(hcupRegval)
hcupRegtrain=hcupRegtrainandval[-indexes,]
nrow(hcupRegtrain)




lm.r<-lm(logtotchg~.,  data=hcupRegtrain) 
summary(lm.r)

library(DAAG)

vifvec<-vif(lm.r)



fcast <- forecast(lm.r, newdata=hcupRegtrain)
anova(lm.r)
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(lm.r)

library(car)
vif(lm.r)



outlierTest(lm.r)
library(MASS)

terms_to_keep<-vif_func(in_frame=hcupRegtrain,thresh = 10,trace = 'T')
form.in<-paste('logtotchg ~',paste(terms_to_keep,collapse='+'))
mod2<-lm(form.in,data=hcupRegtrain)
summary(mod2)

regpredict<-predict(lm.r,newdata = hcupRegtest)
length(regpredict)
length(hcupRegtest$logtotchg)
confmatrixr<-table(regpredict,hcupRegtest$logtotchg)

postResample(regpredict, hcupRegtest$logtotchg)
dim(confmatrixr)
library(caret) 
confusionMatrix(regpredict,hcupRegtest$logtotchg)






