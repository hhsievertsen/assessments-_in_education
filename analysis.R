library("tidyverse")
library("haven")
library("ggpubr")
library("gridExtra")
rm(list=ls())
setwd("C:\\Users\\hs17922\\Dropbox\\Apps\\Overleaf\\AssessmentsInEducation\\Data")
#setwd("/Users/hhs/Dropbox/Data")
# Enrollment data
pop<-read_csv("WB data/20e914d7-eb2b-4c51-a7bc-178e3d1f3965_Data.csv")%>%
     rename(CNT=`Country Code`, year=Time,population=Value)%>%
     select(CNT,year,population)

# 2018
df2018<-read_sas("2018/cy07_msu_sch_qqq.sas7bdat")
df2018<-df2018%>%select(CNTRYID,CNT,SC154Q02WA, SC154Q05WA,SC154Q07WA,SC154Q01HA)%>%
               mutate(test_natcomparison=ifelse(SC154Q05WA==1,1,0),
                      test_childprogress=ifelse(SC154Q02WA==1,1,0),
                      test_teachereffect=ifelse(SC154Q07WA==1,1,0),
                      test_learning=ifelse(SC154Q01HA==1,1,0))%>%
               rename(CNTID=CNTRYID)%>%
               select(CNTID,CNT,starts_with("test_"))%>%
               group_by(CNTID,CNT)%>%
               summarise(test_natcomparison=mean(test_natcomparison,na.rm=T),
                         test_childprogress=mean(test_childprogress,na.rm=T),
                         test_teachereffect=mean(test_teachereffect,na.rm=T),
                         test_learning=mean(test_learning,na.rm=T),year=2018)

# 2000
df2000<-read_delim("2000/intscho.txt",delim = ";",  col_names = F)
df2000<-df2000%>%mutate(CNTID=as.numeric(substr(X1,2,4)),
                        test_natcomparison=as.numeric(substr(X1,199,199)),
                        test_childprogress=as.numeric(substr(X1,196,196)),
                        test_teachereffect=as.numeric(substr(X1,201,201)),
                        test_natcomparison=ifelse(test_natcomparison%in%c(1,2),ifelse(test_natcomparison==1,1,0),NA),
                        test_childprogress=ifelse(test_childprogress%in%c(1,2),ifelse(test_childprogress==1,1,0),NA),
                        test_teachereffect=ifelse(test_teachereffect%in%c(1,2),ifelse(test_teachereffect==1,1,0),NA))%>%
              select(-X1)%>%
              group_by(CNTID)%>%
            summarise(test_natcomparison=mean(test_natcomparison,na.rm=T),
                      test_childprogress=mean(test_childprogress,na.rm=T),
                      test_teachereffect=mean(test_teachereffect,na.rm=T))%>%
             mutate(test_learning=NA,year=2000)
cid<-df2018%>%select(CNTID,CNT)
df2000<-merge(df2000,cid,by="CNTID")

# Merge
df<-merge(df2000,df2018,by="CNTID")

fig1<-ggplot(df,aes(x=test_natcomparison.x,y=test_natcomparison.y))+
     ylim(0,1)+xlim(0,1)+
    geom_text(aes(x=test_natcomparison.x,y=test_natcomparison.y,label=CNT.x),size=1)+
  geom_abline(intercept = 0, slope = 1, size = 0.5) +
  labs(x="Share 2000", title="(C) National comparisons",y="")+
  theme_bw()+
  theme( plot.title = element_text(hjust = .5),text = element_text(size=5))



fig2<-ggplot(df,aes(x=test_childprogress.x,y=test_childprogress.y))+
  geom_text(aes(x=test_childprogress.x,y=test_childprogress.y,label=CNT.x),size=1)+
  geom_abline(intercept = 0, slope = 1, size = 0.5) +
  labs(x="Share 2000", title="(A) Inform parents",y="Share 2018",)+
  ylim(0.7,1)+xlim(0.7,1)+
  theme_bw()+
  theme( plot.title = element_text(hjust = .5),text = element_text(size=5))



fig3<-ggplot(df,aes(x=test_teachereffect.x,y=test_teachereffect.y))+
  geom_text(aes(x=test_teachereffect.x,y=test_teachereffect.y,label=CNT.x),size=1)+
  geom_abline(intercept = 0, slope = 1, size = 0.5)  +
  labs(x="Share 2000",y=" ", title="(B) Teacher effectiveness")+
ylim(0,1)+xlim(0,1)+
  theme_bw()+
  theme( plot.title = element_text(hjust = .5),text = element_text(size=5))




ggarrange(fig2, fig3,fig1,
          ncol = 3, nrow = 1 )
ggsave("fig.png",width = 1280,height=512, dpi=300,units="px")
ggsave("fig_old.png",width = 10,height=4)

