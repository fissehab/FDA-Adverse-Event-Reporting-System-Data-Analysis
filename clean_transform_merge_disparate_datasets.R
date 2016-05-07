

rm(list=ls())

library(dplyr)
library(data.table)
library(sqldf)
library(compare)
library(stringi)


###############
#### DRUG
##############

drug_old=c()

for(i in seq(4,12)){
  z=ifelse(i>9,paste0('DRUG',i),paste0('DRUG',0,i))
  for(j in seq(1,4)){
    if(i==12 & j==4) {break} else{
      drug=fread(paste0(z,'Q',j,'.csv'))
      
      drug_old=rbind(drug_old,drug,fill=TRUE)
    }
  }}

drug_old= select(drug_old, -c(VAL_VBM,DOSE_VBM,DECHAL,RECHAL,LOT_NUM,EXP_DT,NDA_NUM,V13,V14))



drug_new=c()
for(i in seq(12,15)){
  z=paste0('DRUG',i)
  for(j in seq(1,4)){
    if(i==12 & j<4) {next}
    else if(i==15 & j==4){break} else{
      
      
      drug=fread(paste0(z,'Q',j,'.TXT'),sep="$",header=TRUE,select=c("primaryid","caseid","drug_seq","role_cod","drugname",
                                                                     "route","VAL_VBM","DOSE_VBM","CUM_DOSE_CHR","CUM_DOS_UNIT",
                                                                     "DECHAL","RECHAL","LOT_NUM","EXP_DT",
                                                                     "NDA_NUM","DOSE_AMT","DOSE_UNIT","DOSE_FORM","DOSE_FREQ"))
      drug=fread(paste0(z,'Q',j,'.TXT'),sep="$",header=TRUE)
      drug_new=rbind(drug_new,drug)
    }
  }}

drug_new = select(drug_new,-c(role_cod,val_vbm,dose_vbm, cum_dose_chr,cum_dose_unit,dechal,
                              rechal,lot_nbr,nda_num,dose_amt,dose_unit,dose_form,dose_freq))  # modift it when needed

#######################
## DRUG END
######################



###############
### DEMO
##############


demo_old=c()

for(i in seq(4,12)){
  z=ifelse(i>9,paste0('DEMO',i),paste0('DEMO',0,i))
  for(j in seq(1,4)){
    if(i==12 & j==4) {break} else{
      demo=fread(paste0(z,'Q',j,'.csv'))
      
      demo_old=rbind(demo_old,demo,fill=TRUE)
    }
  }}

demo_old=select(demo_old,-c(V24,FOLL_SEQ,E_SUB,TO_MFR,TO_MFR,DEATH_DT,IMAGE))




demo_new=c()

for(i in seq(12,15)){
  z=paste0('DEMO',i)
  for(j in seq(1,4)){
    if(i==12 & j<4) {next}
    else if(i==15 & j==4){break} else{
      
    demo=fread(paste0(z,'Q',j,'.csv'))
      
      demo_new=rbind(demo_new,demo,fill=TRUE)
      
    }
  }}

demo_new$sex = ifelse(is.na(demo_new$sex),demo_new$gndr_cod,demo_new$sex)
demo_new= select(demo_new, -c(gndr_cod,i_f_code,e_sub,lit_ref,auth_num,to_mfr))


#######################
##DEMO END
######################



########################
#### INDI
#########################

indi_old=c()
for(i in seq(04,12)){
  z=ifelse(i>9,paste0('INDI',i),paste0('INDI',0,i))
  for(j in seq(1,4)){
    if(i==12 & j==4) {break} else{
      indi=fread(paste0(z,'Q',j,'.TXT'),sep="$",header=TRUE)
      
      indi_old=rbind(indi_old,indi)}
  }}




indi_new = c()
for(i in seq(12,15)){
  z=paste0('INDI',i)
  for(j in seq(1,4)){
    if(i==12 & j<4) {next}
    else if(i==15 & j==4){break} else{
      indi=fread(paste0(z,'Q',j,'.TXT'),sep="$",header=TRUE)
      
      indi_new=rbind(indi_new,indi)
    }
  }}

########################
#### INDI END
#########################



########################
#### OUTC
#########################

outc_old=c()
for(i in seq(04,12)){
  z=ifelse(i>9,paste0('OUTC',i),paste0('OUTC',0,i))
  for(j in seq(1,4)){
    if(i==12 & j==4) {break} else{
      
      outc=fread(paste0(z,'Q',j,'.csv'))
      
      outc_old=rbind(outc_old,outc)}
  }}



outc_new=c()
for(i in seq(12,15)){
  z=paste0('OUTC',i)
  for(j in seq(1,4)){
    if(i==12 & j<4) {next}
    else if(i==15 & j==4){break} else{
      outc=fread(paste0(z,'Q',j,'.csv'))
      
      names(outc)=c("primaryid","caseid","outc_cod")
      
      outc_new=rbind(outc_new,outc)
    }
  }}
########################
#### OUTC END
#########################


########################
#### Reaction
#########################

reac_old=c()
for(i in seq(04,12)){
  z=ifelse(i>9,paste0('REAC',i),paste0('REAC',0,i))
  for(j in seq(1,4)){
    if(i==12 & j==4) {break} else{
      reac=fread(paste0(z,'Q',j,'.CSV'))
      reac_old=rbind(reac_old,reac)
    }
  }}




reac_new=c()
for(i in seq(12,15)){
  z=paste0('REAC',i)
  for(j in seq(1,4)){
    if(i==12 & j<4) {next}
    else if(i==15 & j==4){break} else{
      reac=fread(paste0(z,'Q',j,'.CSV'))
      reac_new=rbind(reac_new,reac,fill=TRUE)
      
    }
  }}

reac_new = select(reac_new,-c(V1,drug_rec_act))


########################
#### Reaction  END
#########################





#######
# Combining   drug and indication NEW
#######

names(indi_new)=c("primaryid","caseid","drug_seq", "indi_pt")



drug_indi_new= sqldf("SELECT d.primaryid as primaryid, d.caseid as caseid, d.drug_seq as drug_seq,
 d.drugname as drugname,
                       d.route as route,i.indi_pt as indi_pt
                       FROM drug_new d
                       INNER JOIN indi_new i
                      ON d.primaryid= i.primaryid AND d.drug_seq=i.drug_seq
                      ORDER BY primaryid DESC,drug_seq DESC,drugname ASC, i.indi_pt ASC")



## add reaction and outcome

reac_outc_new=sqldf("SELECT r.*, o.outc_cod as outc_cod
                     FROM reac_new r 
                     INNER JOIN outc_new o
                     ON r.primaryid=o.primaryid AND r.caseid=o.caseid
                     ORDER BY r.primaryid,r.caseid,r.pt,o.outc_cod")


#######
# Combining   drug and indication old
#######

#class harmonization


drug_old$ISR=as.integer(drug_old$ISR)
drug_old$DRUG_SEQ=as.integer(drug_old$DRUG_SEQ)




drug_indi_old= sqldf("SELECT d.*,i.INDI_PT
                      FROM drug_old d
                      INNER JOIN indi_old i
                      ON d.ISR= i.ISR AND d.DRUG_SEQ=i.DRUG_SEQ
                      ORDER BY d.ISR, d.DRUG_SEQ,d.DRUGNAME,i.INDI_PT ")



## add reaction and outcome

reac_outc_old=sqldf("SELECT r.*, o.OUTC_COD as OUTC_COD
                     FROM reac_old r 
                     INNER JOIN outc_old o
                     ON r.ISR=o.ISR
                     ORDER BY r.ISR,r.PT,o.OUTC_COD")

####################
## Remove duplicates


drug_indi_new=sqldf("SELECT DISTINCT * 
                     FROM drug_indi_new")

reac_outc_new= sqldf("SELECT DISTINCT * 
                      FROM reac_outc_new")




drug_indi_old=sqldf("SELECT DISTINCT * 
                     FROM drug_indi_old")

reac_outc_old= sqldf("SELECT DISTINCT * 
                      FROM reac_outc_old")


demo_new=sqldf("SELECT DISTINCT * 
               FROM demo_new")


demo_old=sqldf("SELECT DISTINCT * 
               FROM demo_old")



#demo_new=na.omit(demo_new)
#demo_old=na.omit(demo_old)

###################
# Merging drug_indi with reac_out

# add reaction and outcome

###################
## Merging drug_indi with reac_out







except_demo_old=sqldf("SELECT di.*, ro.PT AS PT, ro.OUTC_COD AS OUTC_COD
                       FROM drug_indi_old di 
                       INNER JOIN reac_outc_old ro
                       ON di.ISR=ro.ISR")


except_demo_new=sqldf("SELECT di.*, ro.pt AS pt, outc_cod AS outc_cod
                       FROM drug_indi_new di 
                      INNER JOIN reac_outc_new ro
               ON di.primaryid=ro.primaryid AND di.caseid=ro.caseid")


all_old=sqldf("SELECT de.*, edo.DRUG_SEQ AS DRUG_SEQ,edo.DRUGNAME as DRUGNAME,
                            edo.ROUTE as ROUTE,edo.INDI_PT as INDI_PT,edo.PT as PT, edo.OUTC_COD as OUTC_COD
                FROM demo_old de 
                INNER JOIN except_demo_old edo
                ON de.ISR=edo.ISR")


all_new= sqldf("SELECT de.*, edo.drug_seq AS drug_seq, edo.drugname AS drugname,
                   edo.route AS route, edo.indi_pt AS indi_pt, edo.pt AS pt,edo.outc_cod AS outc_cod
               FROM demo_new de
               INNER JOIN except_demo_new edo
               ON de.primaryid=edo.primaryid AND de.caseid=edo.caseid")



all_new=sqldf("SELECT DISTINCT * 
               FROM all_new")

all_old=sqldf("SELECT DISTINCT * 
               FROM all_old")

my_db <- src_sqlite("Merged_adverse_events_FDA", create = TRUE) # create database


adverse_events_FEARS = all_new
adverse_events_AERS = all_old

copy_to(my_db,adverse_events_FEARS,temporary = FALSE)

copy_to(my_db,adverse_events_AERS,temporary = FALSE)


rm(list=ls())


my_db <- src_sqlite("Merged_adverse_events_FDA", create = FALSE)
# create is false now because I am connecting to an existing database

src_tbls(my_db)


FEARS = tbl(my_db,"adverse_events_FEARS" )

AERS = tbl(my_db,"adverse_events_AERS" )

class(FEARS)


head(FEARS,3)


head(AERS,3)
