library(rsconnect)
library(tidyverse)

#read in datasets
data=read.csv("full_antibiotics.csv",header = T)
subs=read.csv("matched subjects 2019_05_24.csv",header = T)
mh=read.table("20190529_VAP_MH.txt",header=T,sep="\t")
med_group <- read.csv("Antibiotic groups 6-18-19.csv",header=T)
antibiotics<-read.csv("_vapantimicrobiallog.csv",header=T)



#create indicator for matched subset
subs$matched="yes"
subs=subs%>%select(SubjectID,matched)

#name split function for taxa names
name_split<-function(names){unlist(lapply(lapply(strsplit(names,"\\."), tail, n = 2L),function(x){paste(x[[1]],x[[2]],sep="_")}))}

#create indicator for antibotics
data=data%>%
  mutate(AgeIntubation=round(AgeIntubation,1),
         Classifcation=ifelse(is.na(Classifcation),"No antibiotic",as.character(Classifcation)))



#filter out taxa below limit of detection and select variables for display
all=data%>%
  gather(taxa,count,contains("Bacteria")) %>%
  mutate(ra=round(count/root,4),
         taxa=name_split(taxa))%>%
  filter(ra>0.01 & keep_sample=="Yes")%>%
  group_by(num,SubjectID)%>% 
  filter(is.na(root)==F)%>%
  select(SubjectID, Med.group,num,time_since,taxa,ra,ShannonH.Median,AgeIntubation,Classifcation,
         ShannonE.Median,Sobs.Median,lq_all,NonInfectiousAdmittingDX,AdmitPrimaryDx,
         PRISMScore,VAP3,VAPDay_full,Score.Total)
        
#filter to top 10 taxa per subject and merge with matched subset
taxa=all%>%
  distinct(SubjectID,time_since, taxa, .keep_all = TRUE)%>%
    arrange(SubjectID,desc(ra))%>%
  filter(row_number() <= 10)%>%
  left_join(subs,c("SubjectID"="SubjectID"))

#create dataset for cases only
Case=all%>%
  filter(VAP3=="Yes",Classifcation=="Antibacterial"|Classifcation=="No Antibiotic")%>%
  group_by(num,SubjectID,Med.group)%>%
  slice(which.max(ra))

#create data for controls
Control=all%>%
  filter(VAP3=="No",Classifcation=="Antibacterial"|Classifcation=="No Antibiotic")%>%
  group_by(num,SubjectID,Med.group)%>%
  slice(which.max(ra))

#filter subjects for antibiotics
all_med=all%>%
  filter(Classifcation=="Antibacterial"|Classifcation=="No Antibiotic")%>%
  group_by(num,SubjectID,Med.group)%>%
  slice(which.max(ra))

#create load data
load=data%>%
  group_by(num,SubjectID)%>% 
  filter(is.na(load)==F)%>%
  select(SubjectID, Med.group,num,ShannonH.Median,
         ShannonE.Median,Sobs.Median,lq_all,NonInfectiousAdmittingDX,AdmitPrimaryDx,
         PRISMScore,VAP3,VAPDay_full)%>%
  distinct(SubjectID,num, lq_all, .keep_all = TRUE)%>%
  group_by(SubjectID)%>% 
  mutate(samples_collected=length(unique(lq_all)))


#format mh data
mh$SubjectID<-colnames(mh)

val=NULL
i=0
for(i in 1:dim(mh)[1]){val[i]=mh[(i),i+1]}

mh_scores=cbind.data.frame(mh$SubjectID,val)
colnames(mh_scores)=c("SubjectID","MH")

mh_test<-data%>%
  distinct(SubjectID,num,Molecular.ID)%>%
  right_join(mh_scores,c("Molecular.ID"="SubjectID"))%>%
  arrange(SubjectID,num)%>%
  group_by(SubjectID)%>%
  filter(num<=(max(num)-1))

names=as.character(unique(data$SubjectID))

####format Antibiotics

antibiotics_unique_merge <- left_join(med_group,antibiotics,c("X_ANTIMICROBIALMED"="X_ANTIMICROBIALMED"))%>%add_count()

antibiotics_unique_merge <- mutate(antibiotics_unique_merge, 
                                   duration = as.Date(X_ANTIMICROBIALSTOPDATE,format="%Y-%m-%d")-as.Date(X_ANTIMICROBIALSTARTDATE,format="%Y-%m-%d"),
                                   full_duration=ifelse(is.na(duration),14,duration)) %>%
                            filter(full_duration > 0 & full_duration <= 20)

expand_antibiotics <- antibiotics_unique_merge%>%
                     mutate(stop_days=as.Date(as.Date(X_ANTIMICROBIALSTARTDATE,format="%Y-%m-%d")+as.numeric(as.character(full_duration))),
                            variable=full_duration+1)

expand_antibiotic <- expandRows(expand_antibiotics,"variable")

expand_antibiotic$column=do.call(c,(map2(as.Date(expand_antibiotics$X_ANTIMICROBIALSTARTDATE,format="%Y-%m-%d"),as.Date(expand_antibiotics$stop_days,format="%Y-%m-%d"),seq,by="days")))  

expand_antibiotic=expand_antibiotic%>%
  group_by(SubjectID,Med.group)%>%mutate(num=row_number(),anti=1)

# write.csv(taxa,"taxa.csv",row.names = F)
# write.csv(all_med,"all_med.csv",row.names = F)
# write.csv(mh_test,"mh_test.csv",row.names = F)
# write.csv(load,"load.csv",row.names = F)
# write.csv(expand_antibiotic,"expand_antibotic.csv",row.names = F)

shinyApp(ui, server)
deployApp()




