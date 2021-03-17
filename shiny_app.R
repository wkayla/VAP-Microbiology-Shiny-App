library(rsconnect)
library(tidyverse)

data=read.csv("full_antibiotics.csv",header = T)
subs=read.csv("matched subjects 2019_05_24.csv",header = T)

subs$matched="yes"

subs=subs%>%select(SubjectID,matched)
# mh=read.table("20190529_VAP_MH.txt",header=T)



name_split<-function(names){unlist(lapply(lapply(strsplit(names,"\\."), tail, n = 2L),function(x){paste(x[[1]],x[[2]],sep="_")}))}

data=data%>%
  mutate(AgeIntubation=round(AgeIntubation,1),
         Classifcation=ifelse(is.na(Classifcation),"No antibiotic",as.character(Classifcation)))

# top_taxa1_CASE=data%>%
#   filter(VAP3=="Yes",Classifcation=="Antibacterial")%>%
#   gather(taxa,count,contains("Bacteria")) %>%
#   mutate(ra=count/root,
#          taxa=name_split(taxa))%>%
#   group_by(num,SubjectID,Med.group)%>% 
#   slice(which.max(ra)) %>%
#   select(SubjectID, num,Med.group,taxa,ra,ShannonH.Median,AgeIntubation,
#          ShannonE.Median,Sobs.Median,lq_all,NonInfectiousAdmittingDX,AdmitPrimaryDx,
#          PRISMScore,VAP3,VAPDay_full)
# 
# top_taxa1_CONTROL=data%>%
#   filter(VAP3=="No",Classifcation=="Antibacterial")%>%
#   gather(taxa,count,contains("Bacteria")) %>%
#   mutate(ra=count/root,
#          taxa=name_split(taxa))%>%
#   group_by(num,SubjectID,Med.group)%>%
#   slice(which.max(ra))%>%
#   select(SubjectID, num,Med.group,taxa,ra,ShannonH.Median,AgeIntubation,
#          ShannonE.Median,Sobs.Median,lq_all,NonInfectiousAdmittingDX,AdmitPrimaryDx,
#          PRISMScore,VAP3,VAPDay_full)%>%
# group_by(SubjectID)%>% 
#   left_join(subs,c("SubjectID"="SubjectID"))
# 
# top_taxa=rbind.fill(top_taxa1_CASE,top_taxa1_CONTROL)
# 
# top_taxa=top_taxa%>%
#   mutate(anti=1)

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
        

taxa=all%>%
  distinct(SubjectID,time_since, taxa, .keep_all = TRUE)%>%
    arrange(SubjectID,desc(ra))%>%
  filter(row_number() <= 10)%>%
  left_join(subs,c("SubjectID"="SubjectID"))


Case=all%>%
  filter(VAP3=="Yes",Classifcation=="Antibacterial"|Classifcation=="No Antibiotic")%>%
  group_by(num,SubjectID,Med.group)%>%
  slice(which.max(ra))

Control=all%>%
  filter(VAP3=="No",Classifcation=="Antibacterial"|Classifcation=="No Antibiotic")%>%
  group_by(num,SubjectID,Med.group)%>%
  slice(which.max(ra))

all_med=all%>%
  filter(Classifcation=="Antibacterial"|Classifcation=="No Antibiotic")%>%
  group_by(num,SubjectID,Med.group)%>%
  slice(which.max(ra))

load=data%>%
  group_by(num,SubjectID)%>% 
  filter(is.na(load)==F)%>%
  select(SubjectID, Med.group,num,ShannonH.Median,
         ShannonE.Median,Sobs.Median,lq_all,NonInfectiousAdmittingDX,AdmitPrimaryDx,
         PRISMScore,VAP3,VAPDay_full)%>%
  distinct(SubjectID,num, lq_all, .keep_all = TRUE)%>%
group_by(SubjectID)%>% 
  mutate(samples_collected=length(unique(lq_all)))



mh<-read.table("20190529_VAP_MH.txt",header=T,sep="\t")

mh$SubjectID<-colnames(mh)





val=NULL
i=0
for(i in 1:dim(mh)[1]){val[i]=mh[(i),i+1]}

mh_scores=cbind.data.frame(mh$SubjectID,val)
colnames(mh_scores)=c("SubjectID","MH")

# mh_scores$MH=1-as.numeric(as.character(mh_scores$MH))


mh_test<-data%>%
  distinct(SubjectID,num,Molecular.ID)%>%
  right_join(mh_scores,c("Molecular.ID"="SubjectID"))%>%
  arrange(SubjectID,num)%>%
  group_by(SubjectID)%>%
  filter(num<=(max(num)-1))


names=as.character(unique(data$SubjectID))

####Antibiotics

med_group <- read.csv("Antibiotic groups 6-18-19.csv",header=T)



antibiotics<-read.csv("_vapantimicrobiallog.csv",header=T)

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




