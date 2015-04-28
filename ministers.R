rm(list=ls())

setwd("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\Data")
library(foreign);library(dplyr);library(zoo);library(stargazer);library(ggplot2);library(survival)
library(coxme);library(reshape2);library(gridExtra);library(xtable);library(simPH);library(xlsx)
library(car);library(texreg);library(MASS);library(pscl);library(lme4)

theme_set(theme_bw())

#load("ministers.rda")

#####################
######Functions######
#####################
#####
#Hazard ratio increase for survival model
hazper<-function(b, x1, x0){
  y<-round(((exp(b*x1)-exp(b*x0))/exp(b*x0))*100, digits=3)
  y
}

#Unit increase function for count model
percincrease<-function(beta, delta){
  100*(exp(beta*delta)-1)
}


#####################
#####Read data#######
#####################

#NSD-data
nsd<-read.table("Statsraad.txt", sep="\t", header=T)

#Cabinet-data
load("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\Data\\norCabinet.rda")

cabinet<-norCabinet %>%
  filter(From>"1945-10-05" & To<"2013-10-01")
rm(norCabinet)

#Parliament data
load("C:/Users/Martin/Dropbox/Master/Vitass/Stortingsprosjektet/Stortinget/EilertsenMasterData/masterData.rda")

#My data
ministers<-read.csv("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\Data\\Ministers\\ministers_my.csv", sep=",")
resigcalls<-read.csv("RC\\resigcalls.csv")

#Duration variables
ministers$duration<-as.numeric(as.Date(as.character(ministers$end))-as.Date(as.character(ministers$start)))

#####################
###Fix NSD-data######
#####################

#changing variable names on nsd-data
colnames(nsd)<-c("id", "first_name", "last_name", "birth", "death", "party_at_start", "name_deviation", "dep_id", "dep_code",
                 "dep_hv", "hv", "title_hv", "position_deviation", "eng_pos_deviation", "position_sorted", "led_minister", 
                 "start", "end", "external_comment", "eng_ext_comment",
                 "min_reg", "internal_comment")

#Removing hours, mins etc from start and end variables
nsd$start<- gsub(" 00:00:00.000", "", nsd$start)
nsd$end <- gsub(" 00:00:00.000", "", nsd$end)

nsd$start<-as.Date(nsd$start)
nsd$end<-as.Date(nsd$end)

nsd$duration<-as.numeric(nsd$end-nsd$start)

#Making experience variables
nsd_pre45 <- nsd[which(nsd$end<"1945-11-05"),c("id", "duration")]

nsd_pre45 <- nsd_pre45 %>%
  group_by(id) %>%
  summarise(pre45_duration=sum(duration))

nsd<-merge(x=nsd, y=nsd_pre45, by=c("id"), all.x=TRUE)

nsd$pre45_exp <- ifelse(is.na(nsd$pre45_duration), 0, nsd$pre45_duration)

#Subsetting period after 1945-10-05
nsd <- nsd %>%
  group_by(id) %>%
  filter(start > "1945-10-05") %>%
  summarize(birth = birth[1],
            pre45_exp = pre45_exp[1]) 

rm(nsd_pre45)

#######################
#######Cabinet#########
#######################
cabinet$cabinet_short<-c("Gerhardsen II", "Gerhardsen III", "Torp I", "Torp II", "Gerhardsen IV", "Gerhardsen V", "Gerhardsen VI",
                         "Lyng I", "Gerhardsen VII", "Borten I", "Borten II", "Bratteli I", "Korvald I", "Bratteli II", "Nordli I", 
                         "Nordli II", "Brundtland I", "Willoch I", "Willoch II", "Willoch III", "Brundtland II", "Syse I",
                         "Brundtland III", "Brundtland IV", "Jagland I", "Bondevik I", "Stoltenberg I", "Bondevik II",
                         "Stoltenberg II", "Stoltenberg III")


parties<-strsplit(cabinet$CabinetPartiesNor, "\\+")

for(i in 1:30){
cabinet$structure[i]<-ifelse(is.na(parties[[i]][2])==FALSE, "Coalition", "Single-party")
}
rm(parties)


cabinet$cabinet_duration<-as.numeric(as.Date(cabinet$To)-as.Date(cabinet$From))

#######################
#######Ministers#######
#######################
#Merging
ministers<-merge(x=ministers, y=nsd, by.x="nsd_id", by.y="id", all.x=TRUE)
ministers<-merge(x=ministers, y=cabinet[,c("cabinet_short", "CabinetType", "structure", "From", "To", "cabinet_duration", 
                                           "CabinetPartiesNor")], by.x="cabinet_name", by.y="cabinet_short", all.x=TRUE)
rm(nsd, cabinet)

#Turning cabinet variables
ministers$CabinetType<-factor(ministers$CabinetType, levels=c("Minority", "Majority"))
ministers$structure<-factor(ministers$structure, levels=c("Single-party", "Coalition"))

#some missing birth dates and ids
#Inga Marte Thorkildsen
ministers$nsd_id<-ifelse(ministers$first_name=="Inga Marte", 32001, ministers$nsd_id)
ministers$birth<-ifelse(ministers$first_name=="Inga Marte", 1976, ministers$birth)

#Ola Borten Moe
ministers$nsd_id<-ifelse(ministers$first_name=="Ola Borten", 32002, ministers$nsd_id)
ministers$birth<-ifelse(ministers$first_name=="Ola Borten", 1976, ministers$birth)

#Espen Barth Eide
ministers$nsd_id<-ifelse(ministers$first_name=="Espen Barth", 32003, ministers$nsd_id)
ministers$birth<-ifelse(ministers$first_name=="Espen Barth", 1964, ministers$birth)

#Heikki Holm?s
ministers$nsd_id<-ifelse(ministers$first_name=="Heikki Eidsvoll", 32004, ministers$nsd_id)
ministers$birth<-ifelse(ministers$first_name=="Heikki Eidsvoll", 1972, ministers$birth)

#Trygve Vedum
ministers$nsd_id<-ifelse(ministers$first_name=="Trygve Slagsvold", 32005, ministers$nsd_id)
ministers$birth<-ifelse(ministers$first_name=="Trygve Slagsvold", 1978, ministers$birth)

#Hadia Tajik
ministers$nsd_id<-ifelse(ministers$first_name=="Hadia", 32006, ministers$nsd_id)
ministers$birth<-ifelse(ministers$first_name=="Hadia", 1983, ministers$birth)

#Year of start/end/election
ministers$year_start<-as.numeric(format(as.Date(ministers$start), "%Y"))
ministers$year_end<-as.numeric(format(as.Date(ministers$end), "%Y"))
ministers$election_year<-as.numeric(format(as.Date(ministers$election_date), "%Y"))

##############################
####Resignation calls#########
##############################
#resigcalls<-read.csv("RC\\resigcalls.csv")

resigcalls[,6:9]<-data.frame(apply(resigcalls[,6:9], 2, function(x) ifelse(x=="Not done", NA, x)))
resigcalls$rc<-ifelse(is.na(resigcalls$rc_from)==FALSE, 1, 0)

rc_agg<-resigcalls %>%
  group_by(nsd_id, start, end) %>%
  summarize(first_name=first_name[1],
            last_name=last_name[1],
            resigcalls=sum(rc),
            post=post[1],
            rc_opposition=sum(rc[which(rc_from=="Opposition")]),
            rc_paper=sum(rc[which(rc_from=="Newspaper" | rc_from=="Editorial")]),
            rc_organization=sum(rc[which(rc_from=="Organization")]),
            rc_party=sum(rc[which(rc_from=="Coalition partner" | rc_from=="coalition partner" | rc_from=="Own party")]),
            rc_expert=sum(rc[which(rc_from=="Expert" | rc_from=="Expert opinion" | rc_from=="Professor")]),
            rc_citizens=sum(rc[which(rc_from=="Citizens" | rc_from=="Opinion poll" | rc_from=="Constituency"
                                     | rc_from=="Popular opinion" | rc_from=="Other")]),
            rc_voteofconf=sum(rc[which(rc_from=="Vote of confidence")])) %>%
  arrange(as.numeric(start), last_name)

rc_agg<-rc_agg %>%
  group_by(nsd_id) %>%
  mutate(resigcalls_cum=cumsum(resigcalls),
         rc_cum_lag=lag(resigcalls_cum),
         rc_cum_lag=ifelse(is.na(rc_cum_lag)==TRUE, 0, rc_cum_lag))%>%
  arrange(last_name, start)

rc_agg<-data.frame(rc_agg[, c("nsd_id", "start", "end", "post", "resigcalls", "resigcalls_cum", "rc_cum_lag",
                               "rc_opposition", "rc_paper", "rc_organization", "rc_party", "rc_expert", "rc_citizens",
                               "rc_voteofconf")])

ministers<-merge(x=ministers, y=rc_agg, by=c("nsd_id", "start", "end"), all.x=TRUE)

rm(resigcalls, rc_agg)

#############################
####Parliament experience####
#############################
#load("C:/Users/Martin/Dropbox/Master/Vitass/Stortingsprosjektet/Stortinget/EilertsenMasterData/masterData.rda")

masterData$period_start<-as.Date(paste(masterData$period, "10-01", sep="-"))

masterData$start<-masterData$period_start+masterData$begin
masterData$"stop"<-masterData$period_start+masterData$end

masterData$test<-masterData$parlTen-(masterData$"stop"-masterData$start)

masterData2<-masterData %>%
  group_by(icprsLegis, period) %>%
  summarize(name=name[1],
            parlTen=sum(parlTen),
            youthCen=max(youthCen),
            youthLoc=max(youthLoc))

masterData2<-masterData2 %>%
  arrange(icprsLegis, period) %>%
  group_by(icprsLegis) %>%
  mutate(parlTen_cum=cumsum(parlTen),
         parlTen_cum_y=cumsum(parlTen)/365.25,
         parlTen_cum_lag=lag(parlTen_cum),
         parlTen_cum_y_lag=lag(parlTen_cum_y))

masterData2$period_start<-paste(masterData2$period, "10-01", sep="-")
masterData2$period_end<-paste(masterData2$period+4, "10-01", sep="-")

ministers$parl_start<-paste(format(as.Date(ministers$election_date), "%Y"), "10-01", sep="-")

ministers<-merge(x=ministers, y=masterData2[,c("icprsLegis", "period", "parlTen_cum", "youthCen", "youthLoc")],
                 by.x=c("nsd_id", "election_year"), by.y=c("icprsLegis", "period"), all.x=TRUE)

rm(masterData, masterData2)

#############################
##########Ministers##########
#############################

for(i in 2:nrow(ministers)){
  ministers$parlTen_cum[i]<-ifelse(ministers$nsd_id[i]==ministers$nsd_id[i-1] & is.na(ministers$parlTen_cum[i])==TRUE, 
                                   ministers$parlTen_cum[i-1], ministers$parlTen_cum[i])  
}

ministers$parlTen_cum<-ifelse(is.na(ministers$parlTen_cum)==TRUE, 0, ministers$parlTen_cum)

ministers<-ministers %>%
  group_by(nsd_id) %>%
  arrange(start) %>%
  mutate(minister_exp_cum_y_lag=lag((cumsum(duration)+pre45_exp)/365.25))

ministers$minister_exp_cum_y_lag<-ifelse(is.na(ministers$minister_exp_cum_y_lag)==TRUE, 0, ministers$minister_exp_cum_y_lag)

ministers$parlTen_dum<-ifelse(ministers$parlTen_cum>0, 1, 0)

#Start year
ministers$start_year<-as.numeric(format(as.Date(ministers$start), "%Y"))

#Age at start and age centering
ministers$age<-ministers$start_year-ministers$birth
ministers$age_cen<-ministers$age-mean(ministers$age)

#Making duration variable
ministers$start<-as.Date(ministers$start)
ministers$end<-as.Date(ministers$end)

#Specifying prime minister of each unit
ministers$pm_name<-sapply(strsplit(as.character(ministers$cabinet_name), " "), "[[", 1)

#Arranging data
ministers<-arrange(ministers, last_name, first_name, start)

#Dummy of education
ministers$education_dum <- ifelse(ministers$education == "Higher education", "Higher", "Lower")

#Reshuffle
ministers$reshuffle <- ifelse(is.na(ministers$reshuffle), 0, ministers$reshuffle)
ministers$reshuffle<-as.factor(ministers$reshuffle)

#(Start, stop]
ministers$dur_start<-as.numeric(ministers$start-ministers$From)
ministers$dur_end<-as.numeric(ministers$end-ministers$From)


#Leace of absence variable
ministers$leave <- ifelse(is.na(lapply(strsplit(as.character(ministers$cause), "/"), 
                                       function(x) match(x, "Leave of absence")))==FALSE, 1, 0)

#Fixing event variable
ministers$cause<-as.character(ministers$cause)
ministers$cause<-ifelse(is.na(ministers$cause)==TRUE, "Not dismissed", ministers$cause)

ministers$event2<-ministers$event
ministers$event2<-ifelse(ministers$cause=="Death", 0, ministers$event2)
ministers$event2<-ifelse(ministers$cause=="Leave of absence", 0, ministers$event2)
ministers$event2<-ifelse(ministers$cause=="Leave of absence/Death", 0, ministers$event2)
ministers$event2<-ifelse(is.na(ministers$event2)==TRUE, 0, ministers$event2)

#Setting more broad categories for minister posts
ministers$jurisdiction<-recode(ministers$post, "
                               'Administrasjonsminister' = 'Administration';
                               'Arbeids- og administrasjonsminister' = 'Administration';
                               'Arbeids- og inkluderingsminister' = 'Administration';
                               'Arbeidsminister' = 'Administration';
                               'Barne- og familieminister' = 'Social';
                               'Bistandsminister' = 'Foreign affairs';
                               'Familie- og forbruksminister' = 'Social';
                               'Finansminister' = 'Finance';
                               'Fiskeriminister' = 'Sea and fish';
                               'Forbruker- og administrasjonsminister' = 'Administration';
                               'Fornyingsminister' = 'Administration';
                               'Forsknings- og høyere utdanningsminister' = 'Education';
                               'Forsvarsminister' = 'Defence';
                               'Forsyning- og gjenreisningsminister' = 'Administration';
                               'Handelsminister' = 'Trade';
                               'Havrettsminister' = 'Sea and fish';
                               'Helseminister' = 'Health';
                               'Industriminister' = 'Industry';
                               'Justisminister' = 'Justice';
                               'Kirke- og undervisningsminister' = 'Education';
                               'Kommunalminister' = 'Regional';
                               'Kulturminister' = 'Culture';
                               'Kunnskapsminister' = 'Education';
                               'Landbruksminister' = 'Agriculture';
                               'Menneskerettighetsminister' = 'Foreign affairs';
                               'Miljøvernminister' = 'Environment';
                               'Minister for gjenoppbygning av Finnmark' = 'Regional';
                               'Næringsminister' = 'Industry';
                               'Olje- og energiminister' = 'Industry';
                               'Planleggingsminister' = 'Administration';
                               'Prisminister' = 'Finance';
                               'Samferdselsminister' = 'Transport';
                               'Skipsfartsminister' = 'Sea and fish';
                               'SMK' = 'Administration';
                               'Sosialminister' = 'Social';
                               'Statsminister' = 'Prime minister';
                               'Utenriksminister' = 'Foreign affairs';
                               'Utviklingsminister' = 'Foreign affairs'
                               ")

#Dummies of resigcalls from different actors
ministers$rc_paper_dum<-ifelse(ministers$rc_paper>0, 1, 0)
ministers$rc_party_dum<-ifelse(ministers$rc_party>0, 1, 0)
ministers$rc_organization_dum<-ifelse(ministers$rc_organization>0, 1, 0)
ministers$rc_opposition_dum<-ifelse(ministers$rc_opposition>0, 1, 0)

#Fixing variable for youth experience
ministers$youthCen<-ifelse(is.na(ministers$youthCen)==TRUE, 0, ministers$youthCen)
ministers$youthLoc<-ifelse(is.na(ministers$youthLoc)==TRUE, 0, ministers$youthLoc)

#Time for interaction
ministers$timeint<-log((as.numeric(ministers$start-as.Date("1945-11-04")))/365.25)

#Fixing gender variable
ministers$gender<-factor(ministers$gender, levels=c("Male", "Female"))

rm(i)

#Deleting bad variables
ministers$event<-NULL;ministers$cab_non_conf<-NULL;ministers$non_conf<-NULL;ministers$dep_short<-NULL
ministers$party_leader<-NULL;ministers$parl_elected<-NULL

save(ministers, file="ministers.rda")
#load("ministers.rda")

ministers<-ministers[-which(ministers$nsd_id==299 | ministers$prime_minister==1),]

######################
#####Some figures#####
######################

#Cabinet composition
cab_comp<-ministers %>%
  group_by(cabinet_name) %>%
  summarize(comp=CabinetPartiesNor[1],
            type=CabinetType[1]) %>%
  group_by(comp, type) %>%
  summarize(n_cab=length(comp))
cab_comp2<-expand.grid(comp=levels(factor(cab_comp$comp)), type=levels(factor(cab_comp$type)))
cab_comp<-merge(x=cab_comp, y=cab_comp2, by=c("comp", "type"), all=T)
cab_comp$n_cab<-ifelse(is.na(cab_comp$n_cab)==TRUE, 0, cab_comp$n_cab)

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\cab_comp.pdf", width=9)
ggplot(cab_comp, aes(x=comp, y=n_cab, fill=type)) +
  geom_bar(stat="identity", position=position_dodge(width=.5)) +
  scale_y_continuous(limits=c(0,13), breaks=seq(0,13,2), expand=c(0,0))+
  scale_fill_manual(values=c("#252525", "#cccccc"))+
  labs(y="Number of cabinets", x="Party composition", fill="Parliamentary basis")+
  theme(legend.position=c(.9,.845),
        legend.key=element_blank(),
        legend.key.width=unit(1.5, units="cm"),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        axis.title.x=element_text(vjust=.25),
        axis.title.y=element_text(vjust=1))
dev.off()

rm(cab_comp, cab_comp2)

#Duration and event
event<-ministers %>%
  group_by(cabinet_name, nsd_id) %>%
  summarise(event2=event2[length(nsd_id)]) %>%
  group_by(cabinet_name) %>%
  summarise(Dismissals=sum(event2),
            Survivors=length(which(event2==0)))
event<-melt(event, id.vars="cabinet_name")

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\event.pdf", height=6)
ggplot(event, aes(x=cabinet_name, y=value, group=variable, color=variable))+
  geom_line(size=1)+
  scale_x_discrete(limits=c("Gerhardsen II", "Gerhardsen III", "Torp I", "Torp II", "Gerhardsen IV", "Gerhardsen V", "Gerhardsen VI", 
                            "Lyng I", "Gerhardsen VII", "Borten I", "Borten II", "Bratteli I", "Korvald I", "Bratteli II", "Nordli I", 
                            "Nordli II", "Brundtland I", "Willoch I", "Willoch II", "Willoch III", "Brundtland II", "Syse I", 
                            "Brundtland III", "Brundtland IV", "Jagland I", "Bondevik I", "Stoltenberg I", "Bondevik II",
                            "Stoltenberg II", "Stoltenberg III"),
                   expand=c(0,.5))+
  scale_y_continuous(breaks=seq(0,50,2), expand=c(0,.2))+
  labs(y="Number of ministers", x=NULL, color=NULL)+
  scale_color_manual(values=rev(c("#004C49", "#0ACCC1"))) +
  theme(legend.position=c(.15,.93),
        legend.key=element_blank(),
        legend.key.width=unit(1.5, units="cm"),
        axis.line=element_line(),
        axis.title.y=element_text(vjust=1.5),
        axis.text.y=element_text(),
        axis.text.x=element_text(angle=45, vjust=1.05, hjust=1.1),
        panel.border=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_blank(),
        plot.margin=unit(c(1,1,1,1), units="cm"))
dev.off()

rm(event)

#Resignation calls
rc<-ministers %>%
  group_by(cabinet_name, nsd_id) %>%
  summarize(rc=sum(resigcalls),
            event2=max(event2)) %>%
  group_by(cabinet_name, event2) %>%
  summarize(rc=mean(rc))
rc<-data.frame(rc)

rc$event2<-factor(rc$event2, labels=c("Survivors", "Dismissals"))

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\rc_desc_plot.pdf", width=9)
ggplot(rc, aes(x=cabinet_name, y=rc, ymin=0, group=factor(event2), fill=factor(event2), color=factor(event2))) +
  geom_smooth(method="loess", se=FALSE, size=.75, span=.75)+
  geom_point(position=position_dodge(width=.25), size=3)+
  scale_color_manual(values=c("#004C49", "#0ACCC1")) +
  scale_y_continuous(breaks=seq(0,24,.25), expand=c(0,.1)) +
  scale_x_discrete(limits=c("Gerhardsen II", "Gerhardsen III", "Torp I", "Torp II", "Gerhardsen IV", "Gerhardsen V", "Gerhardsen VI", 
                            "Lyng I", "Gerhardsen VII", "Borten I", "Borten II", "Bratteli I", "Korvald I", "Bratteli II", "Nordli I", 
                            "Nordli II", "Brundtland I", "Willoch I", "Willoch II", "Willoch III", "Brundtland II", "Syse I",
                            "Brundtland III", "Brundtland IV", "Jagland I", "Bondevik I", "Stoltenberg I", "Bondevik II",
                            "Stoltenberg II", "Stoltenberg III"),
                   expand=c(0,.1)) +
  labs(x=NULL, y="Mean resignation calls", fill=NULL, color=NULL) +
  theme(legend.position=c(.10,.925),
        legend.key=element_blank(),
        legend.key.width=unit(1.5, units="cm"),
        axis.line=element_line(),
        axis.title.y=element_text(vjust=1.5),
        axis.text.y=element_text(),
        axis.text.x=element_text(angle=45, vjust=1.05, hjust=1.1),
        panel.border=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_blank(),
        plot.margin=unit(c(1,1,1,1), units="cm"))
dev.off()

rm(rc)

#RC grouped
n_rc<-ministers[, c("resigcalls", "rc_opposition", "rc_paper", "rc_party")]

n_rc<-melt(n_rc)
levels(n_rc$variable)<-c("Pooled", "Opposition", "Newspaper", "Own party")

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\n_rc.pdf", width=10)
ggplot(n_rc, aes(x=variable, y=value)) +
  geom_jitter(aes(color=value), position=position_jitter(width=.3)) +
  scale_color_gradient2(low="dark slate gray", mid="dark cyan", high="cyan", midpoint=3)+
  scale_y_continuous(limits=c(-.499,6.499), breaks=seq(0,6,1), expand=c(0,0))+
  scale_x_discrete(expand=c(0,.5))+
  labs(x=NULL, y="Resignation calls", color=NULL)+
  theme(legend.position="none",
        panel.grid.minor=element_line(color="black", linetype="dashed"),
        panel.grid.major=element_blank(),
        axis.ticks=element_blank(),
        panel.border=element_blank(),
        axis.title.y=element_text(vjust=1.75))
dev.off()

rm(n_rc)
  
#Prime minister -- duration is summed on the PM (all terms)
pm_name <-ministers %>%
  group_by(cabinet_name) %>%
  summarize(duration=mean(duration),
            cabinet_duration=mean(cabinet_duration))
pm_name$percent<-(pm_name$duration/pm_name$cabinet_duration)*100

pm_name<-melt(pm_name, id.vars=c("cabinet_name"), measure.vars=c("cabinet_duration", "duration"))
levels(pm_name$variable)<-c("Mean cabinet durataion", "Mean minister duration")


pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\pm_name.pdf")
ggplot(pm_name, aes(x=factor(cabinet_name), y=value, group=variable, fill=variable))+
  geom_bar(position=position_dodge(width=.5), stat="identity")+
  scale_y_continuous(limits=c(0,1600), breaks=seq(0,15000,200), expand=c(0,0))+
  scale_x_discrete(limits=c("Gerhardsen II", "Gerhardsen III", "Torp I", "Torp II", "Gerhardsen IV", "Gerhardsen V", "Gerhardsen VI", 
                            "Lyng I", "Gerhardsen VII", "Borten I", "Borten II", "Bratteli I", "Korvald I", "Bratteli II", "Nordli I", 
                            "Nordli II", "Brundtland I", "Willoch I", "Willoch II", "Willoch III", "Brundtland II", "Syse I",
                            "Brundtland III", "Brundtland IV", "Jagland I", "Bondevik I", "Stoltenberg I", "Bondevik II",
                            "Stoltenberg II", "Stoltenberg III"))+
  scale_fill_manual(values=c("#cccccc", "#252525"))+
  labs(x="", y="Duration", fill="")+
  guides(fill=guide_legend(nrow=1, ncol=2, label.position="left"))+
  theme(legend.position="top",
        axis.line=element_line(),
        axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=1),
        axis.title=element_text(face="bold", size=14),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_blank())
dev.off()

rm(pm_name)


#Cabinet experience
cab_exp<-ministers %>%
  group_by(cabinet_name, nsd_id) %>%
  summarize(election_year=election_year[1],
            event2=max(event2),
            cab_exp=mean(minister_exp_cum_y_lag)) %>%
  group_by(cabinet_name, event2) %>%
  summarize(cab_exp=mean(cab_exp))
cab_exp$event2<-factor(cab_exp$event2, labels=c("Survivors", "Dismissals"))

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\cab_exp_plot.pdf", height=5)
ggplot(cab_exp, aes(x=cabinet_name, y=cab_exp, ymin=.9, group=factor(event2), color=factor(event2))) +
  geom_smooth(method="loess", se=FALSE, size=.75, span=.75)+
  geom_point(size=3, position=position_dodge(width=.1))+
  scale_y_continuous(breaks=seq(0,20,1), expand=c(0,.2))+
  scale_x_discrete(limits=c("Gerhardsen II", "Gerhardsen III", "Torp I", "Torp II", "Gerhardsen IV", "Gerhardsen V", "Gerhardsen VI", 
                            "Lyng I", "Gerhardsen VII", "Borten I", "Borten II", "Bratteli I", "Korvald I", "Bratteli II", "Nordli I", 
                            "Nordli II", "Brundtland I", "Willoch I", "Willoch II", "Willoch III", "Brundtland II", "Syse I", 
                            "Brundtland III", "Brundtland IV", "Jagland I", "Bondevik I", "Stoltenberg I", "Bondevik II",
                            "Stoltenberg II", "Stoltenberg III"),
                     expand=c(0,.2))+
  scale_color_manual(values=c("#004C49", "#0ACCC1")) +
  labs(y="Mean cabinet experience (in years)", x=NULL, color=NULL) +
  theme(legend.position=c(.875,.91),
        legend.key=element_blank(),
        legend.key.width=unit(1.5, units="cm"),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        axis.text.x=element_text(angle=45, vjust=1.05, hjust=1.1),
        axis.line=element_line())
dev.off()

rm(cab_exp)

#Parliamentary experience
parl_exp<-ministers %>%
  group_by(cabinet_name, nsd_id) %>%
  summarize(parlTen_dum=parlTen_dum[1],
            duration=mean(duration),
            event=event2[length(nsd_id)]) %>%
  group_by(parlTen_dum, event) %>%
  summarise(n_ministers=length(nsd_id))


parl_exp$event<-factor(parl_exp$event, labels=c("Survivors", "Dismissals"))
parl_exp$parlTen_dum<-factor(parl_exp$parlTen_dum, labels=c("No experience", "Some experience"))
parl_exp$percent<-paste(round((parl_exp$n_ministers/sum(parl_exp$n_ministers))*100, digits=0), "%")

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\parl_exp_plot.pdf", height=5)
ggplot(parl_exp, aes(x=factor(parlTen_dum), y=n_ministers, fill=factor(event)))+
  geom_bar(stat="identity", position=position_dodge(width=.25))+
  scale_x_discrete(expand=c(0,.75))+
  scale_y_continuous(limits=c(0,250), breaks=seq(0,500,25), expand=c(0,.1))+
  scale_fill_manual(values=c("#252525", "darkcyan"))+
  labs(y="Number of ministers", x=NULL, fill=NULL)+
  geom_text(aes(label=percent), color="white", position=position_dodge(width=.25), vjust=1.5)+
  guides(fill=guide_legend(direction="vertical", label.position="left"))+
  theme(legend.position=c(.9,.95),
        legend.key=element_blank(),
        legend.key.width=unit(1.5, units="cm"),
        panel.border=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank())
dev.off()

rm(parl_exp)

#Youth experience
youth<-ministers %>%
  group_by(party) %>%
  mutate(total_youth=ifelse(youthLoc==1 | youthCen==1, 1, 0)) %>%
  summarize(youthLoc=mean(youthLoc)*100,
            youthCen=mean(youthCen)*100,
            total=mean(total_youth)*100)

youth<-melt(youth, id.vars="party")
youth$variable<-factor(youth$variable, levels=c("total", "youthLoc", "youthCen"), labels=c("Total", "Local", "Central"))

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\youth_plot.pdf", height=6)
ggplot(youth, aes(x=party, y=value, group=variable, fill=variable))+
  geom_bar(stat="identity", position=position_dodge(width=.75))+
  scale_y_continuous(limits=c(0,40), breaks=seq(0,100,5), expand=c(0,0))+
  scale_x_discrete(expand=c(0,.5))+
  scale_fill_manual(values=c("#cccccc", "#252525", "darkcyan"))+
  guides(fill=guide_legend(label.position="left"))+
  labs(y="Youth experience (% of all ministers)", x="Party", fill=NULL)+
  theme(legend.position=c(.9,.9),
        legend.key=element_blank(),
        legend.key.width=unit(1.5, units="cm"),
        panel.border=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        axis.title.x=element_text(vjust=.25),
        axis.title.y=element_text(vjust=1.25),
        axis.ticks.x=element_blank())
dev.off()

rm(youth)

#Gender time trend
gender_trend <- ministers %>%
  group_by(cabinet_name, nsd_id) %>%
  summarize(gender=gender[1]) %>%
  group_by(cabinet_name) %>%
  summarize(female=length(which(gender=="Female")),
            male=length(which(gender=="Male"))) %>%
  mutate(perc_female=100*(female/(male+female)),
         perc_male=100*(male/(male+female)))

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\gender_plot.pdf", height=5)
ggplot(gender_trend, aes(x=cabinet_name, y=perc_female, group=1))+
  geom_line(size=.5) +
  geom_smooth(size=1, method="loess", se=FALSE, color="#1B5400")+
  scale_y_continuous(limits=c(0,55), breaks=seq(0,55,5), expand=c(0,0), minor_breaks=seq(0,60,10))+
  scale_x_discrete(limits=c("Gerhardsen II", "Gerhardsen III", "Torp I", "Torp II", "Gerhardsen IV", "Gerhardsen V", "Gerhardsen VI", 
                            "Lyng I", "Gerhardsen VII", "Borten I", "Borten II", "Bratteli I", "Korvald I", "Bratteli II", "Nordli I", 
                            "Nordli II", "Brundtland I", "Willoch I", "Willoch II", "Willoch III", "Brundtland II", "Syse I",
                            "Brundtland III", "Brundtland IV", "Jagland I", "Bondevik I", "Stoltenberg I", "Bondevik II", 
                            "Stoltenberg II", "Stoltenberg III"),
                   expand=c(0,0)) +
  labs(y="Percent female ministers", x=NULL) +
  theme(axis.text.x=element_text(angle=45, vjust=1.05, hjust=1.1),
        panel.border=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major=element_blank(),
        axis.line=element_line(),
        plot.margin=unit(c(1,1,1,1), units="cm"))
dev.off()

rm(gender_trend)

#Cabinet type plot
cab_type<-ministers %>%
  group_by(CabinetType, structure, nsd_id) %>%
  summarize(CabinetType2=CabinetType[1],
            structure2=structure[1],
            event=max(event2)) %>%
  group_by(CabinetType2, structure2) %>%
  summarize(n_ministers=length(nsd_id),
            n_event=length(which(event==1)))

cab_type$percent<-paste(round((cab_type$n_event/cab_type$n_ministers)*100, digits=2), "%", sep="")

cab_type2<-melt(cab_type, id.vars=c("CabinetType2", "structure2"), measure.vars=c("n_ministers", "n_event"))
cab_type2$cab<-paste(cab_type2$CabinetType2, cab_type2$structure2)
cab_type2$percent<-c(rep("100%", 4), cab_type$percent)
cab_type2$variable<-factor(cab_type2$variable, labels=c("Total", "Dismissals"))

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\cabtype_plot.pdf", height=5)
ggplot(cab_type2, aes(x=cab, y=value, group=variable, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge(width=.25)) +
  geom_text(aes(label=percent), position=position_dodge(width=.25), vjust=-.5)+
  scale_y_continuous(limits=c(0,155), breaks=seq(0,150,20), expand=c(0,0)) +
  scale_fill_manual(values=c("#cccccc", "#252525"))+
  labs(x=NULL, y="Number of ministers", fill=NULL)+
  theme(legend.position=c(.15,.84),
        legend.key=element_blank(),
        legend.key.width=unit(1.5, units="cm"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank())
dev.off()

rm(cab_type, cab_type2)

#Age plot
age<-ministers %>%
  group_by(cabinet_name, nsd_id) %>%
  summarize(age=min(age),
            event2=max(event2)) %>%
  group_by(cabinet_name, event2) %>%
  summarise(age=mean(age))

age$event2<-factor(age$event2, labels=c("Survivors", "Dismissals"))

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\age_plot.pdf", height=4.75)
ggplot(age, aes(x=cabinet_name, y=age, ymax=mean(age), group=factor(event2), color=factor(event2)))+
  geom_point(position=position_dodge(width=.25), size=3)+
  geom_smooth(method="loess", position=position_dodge(width=.5), se=FALSE, size=.75)+
  scale_x_discrete(limits=c("Gerhardsen II", "Gerhardsen III", "Torp I", "Torp II", "Gerhardsen IV", "Gerhardsen V", "Gerhardsen VI", 
                            "Lyng I", "Gerhardsen VII", "Borten I", "Borten II", "Bratteli I", "Korvald I", "Bratteli II", "Nordli I", 
                            "Nordli II", "Brundtland I", "Willoch I", "Willoch II", "Willoch III", "Brundtland II", "Syse I", 
                            "Brundtland III","Brundtland IV", "Jagland I", "Bondevik I", "Stoltenberg I", "Bondevik II", 
                            "Stoltenberg II", "Stoltenberg III"),
                   expand=c(0,.25))+
  scale_y_continuous(limits=c(42,64), breaks=seq(0,100,2)) +
  scale_color_manual(values=c("#004C49", "#0ACCC1"))+
  labs(x=NULL, y="Mean cabinet age", fill=NULL, color=NULL)+
  theme(legend.position=c(.875,.91),
        legend.key=element_blank(),
        legend.key.width=unit(1.5, units="cm"),
        legend.key.height=unit(.65, units="cm"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        axis.title.x=element_text(vjust=0),
        axis.title.y=element_text(vjust=1.5),
        axis.text.x=element_text(angle=45, vjust=1, hjust=1))
dev.off()

rm(age)

#Education
education<-ministers %>%
  group_by(cabinet_name, nsd_id) %>%
  summarize(education=education_dum[1],
            event=max(event2)) %>%
  group_by(education) %>%
  summarize(n_ministers=length(nsd_id),
            n_event=length(event[which(event==1)]))
education$percent<-paste(as.character(round((education$n_event/education$n_ministers)*100, digits=1)), "%", sep="")

education<-melt(education)
education$percent[1:2]<-"100%"
education$variable<-factor(education$variable, labels=c("Total", "Dismissed"))

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\education_plot.pdf", height=5)
ggplot(education, aes(x=education, y=value, ymax=max(value)+50, group=factor(variable), fill=factor(variable)))+
  geom_bar(stat="identity", position=position_dodge(width=.5))+
  geom_text(aes(label=percent), position=position_dodge(width=.5), vjust=-.25)+
  scale_y_continuous(breaks=seq(0,500,50), expand=c(0,0))+
  scale_fill_manual(values=c("#cccccc", "#252525"))+
  labs(x="Education", y="Number of ministers", fill=NULL)+
  theme(legend.position=c(.9,.9),
        axis.title.y=element_text(vjust=1.5),
        legend.key=element_blank(),
        legend.key.width=unit(1.5, units="cm"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank())
dev.off()

rm(education)

#Reshuffles
reshuffle<-ministers %>%
  group_by(pm_name) %>%
  arrange(election_year) %>%
  summarize(reshuffle=sum(as.numeric(as.character(reshuffle))),
            cabinet_duration=mean(duration),
            election_year=election_year[1])

reshuffle$pm_name<-factor(reshuffle$pm_name, levels=c("Gerhardsen", "Torp", "Lyng", "Borten", "Bratteli", "Korvald", "Nordli",
                                                      "Brundtland", "Willoch", "Syse", "Jagland", "Bondevik", "Stoltenberg"))

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\reshuffle_plot.pdf", height=5)
ggplot(reshuffle, aes(x=pm_name, y=reshuffle, group=1, color=cabinet_duration))+
  geom_text(aes(label=pm_name), size=6, fontface="bold")+
  scale_y_continuous(breaks=seq(0,12,1))+
  scale_x_discrete(expand=c(0,2))+
  scale_color_gradient2(limits=c(0,850),low="#DBDBDB", high="#092111", mid="#258545", midpoint=400)+
  labs(y="Reshuffles", color="Mean duration")+
  guides(color=guide_colorbar(ticks=FALSE, direction="horizontal", title.position="top", barwidth=10))+
  theme(legend.position=c(.20,.95),
        axis.title.x=element_blank(),
        axis.title.y=element_text(vjust=1.25),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        panel.border=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_line(linetype="dashed", color="gray"),
        panel.grid.major=element_blank(),
        axis.line=element_line())
dev.off()

rm(reshuffle)

########################
########Tables##########
########################

fjeld<-ministers %>%
  filter(last_name=="Fjeld", cabinet_name=="Gerhardsen II")
fjeld<-data.frame(fjeld)

storberget<-ministers %>%
  filter(last_name=="Storberget", cabinet_name=="Stoltenberg III")
storberget<-data.frame(storberget)
df_ex<-rbind(fjeld, storberget)
df_ex$duration<-with(df_ex, as.character(Surv(dur_start, dur_end, event2)))

stargazer(df_ex[,c("last_name", "start", "end", "duration", "From", "To")], 
          summary=FALSE,
          table.placement="!h",
          title="Extract of the data",
          out="C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Tables\\fjeld.tex",
          label="df_ex",
          covariate.labels=c("", "Last name", "Start", "End", "Duration", "Cabinet start", "Cabinet end"))
rm(fjeld, storberget, df_ex)

# Descriptive stats
desc<-with(ministers, data.frame(cbind(duration, event2, resigcalls, timeint, rc_opposition_dum, rc_party_dum, rc_paper_dum, 
                                       minister_exp=minister_exp_cum_y_lag, parlTen=parlTen_dum, youthCen, youthLoc, 
                                       CabinetType=as.character(CabinetType), structure=as.character(structure), 
                                       age, gender=as.character(gender), education=as.character(education_dum), reshuffle)))

desc$gender <- ifelse(desc$gender=="Male", 0, 1)
desc$education <- ifelse(desc$education=="Lower", 1, 0)
desc$CabinetType <- ifelse(desc$CabinetType=="Minority", 0, 1)
desc$structure <- ifelse(desc$structure=="Single-party", 0, 1)

desc<-data.frame(sapply(desc[,1:ncol(desc)], function(x) as.numeric(as.character(x))))
desc$reshuffle<-(desc$reshuffle)-1

desc2<-data.frame(Variable=colnames(desc),
                  Class=c("Numeric", "Dichotomy", "Count", "Numeric", "Dichotomy", "Dichotomy", "Dichotomy", "Numeric", "Dichotomy",
                          "Dichotomy", "Dichotomy", "Dichotomy", "Dichotomy", "Numeric", "Dichotomy", "Dichotomy", "Dichotomy"),
                  N=nrow(desc),
                  Mean=sapply(desc, mean),
                  St.Dev=sapply(desc, sd),
                  Min=sapply(desc, min),
                  Median=sapply(desc, median),
                  Max=sapply(desc, max),
                  row.names=NULL)
desc2$Variable<-c("Duration", "Event", "Resignation calls (RC)", "Time (logged)", "RC opposition", "RC own party", "RC newspaper",
                  "Cabinet exp.", "Parl. exp", "Youth exp. (central)", "Youth exp. (local)", "Cabinet type (majority)", 
                  "Cabinet structure (coalition)", "Age", "Gender (female)", "Education (lower)", "Reshuffle")

desc2[,3:ncol(desc2)]<-data.frame(sapply(desc2[,3:ncol(desc2)], function(x) as.character(round(x, digits=3))))
desc2$St.Dev<-ifelse(desc2$Max=="1", "-", as.character(desc2$St.Dev))

stargazer(desc2,
          summary=FALSE,
          title="Descriptive statistics",
          table.placement="!h",
          label="desc",
          align=TRUE,
          rownames=FALSE,
          font.size="footnotesize",
          column.sep.width="2pt",
          out="C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Tables\\desc.tex")

rm(desc, desc2)

############################
#######Regression###########
############################

cab_reg2<-coxph(Surv(duration, event2) ~ rc_opposition_dum*timeint + rc_paper_dum*timeint + rc_party_dum*timeint +
                  age_cen + factor(gender) + factor(youthCen) + factor(youthLoc) + minister_exp_cum_y_lag + factor(parlTen_dum) +
                  factor(education_dum) + factor(reshuffle) + factor(CabinetType) + factor(structure) + frailty(jurisdiction),
               data=ministers, subset=prime_minister==0)
summary(cab_reg2)

cab_reg3<-coxph(Surv(dur_start, dur_end, event2) ~ resigcalls + age_cen + factor(gender) + 
                  factor(youthCen) + factor(youthLoc) + minister_exp_cum_y_lag + factor(parlTen_dum) + 
                  factor(education_dum) + factor(reshuffle) + factor(CabinetType) + factor(structure) + frailty(jurisdiction),
                data=ministers, subset=prime_minister==0)

summary(cab_reg3)


test<-data.frame(cbind(resid(cab_reg3, type="deviance"), ministers$duration[which(ministers$prime_minister==0 & ministers$nsd_id!=299)]))

ggplot(test, aes(x=X2, y=X1))+
  geom_point() +
  geom_abline(intercept=0, slope=0)+
  geom_smooth(method="loess", se=TRUE, fill="blue", color="black", alpha=.1)

rm(test)

#phtest_1<-cox.zph(cab_reg1)
phtest_2<-cox.zph(cab_reg2)
phtest_3<-cox.zph(cab_reg3)

phtest_2;phtest_3

AIC(cab_reg2, cab_reg3)


#Coef plot
coef1<-data.frame(coef=coef(cab_reg3), se=sqrt(diag(cab_reg3$var)))
coef2<-data.frame(coef=coef(cab_reg2), se=sqrt(diag(cab_reg2$var)))

coef1<-coef1 %>%
  mutate(upper=coef+1.96*se,
         lower=coef-1.96*se,
         mod="Model 1",
         var=names(coef(cab_reg3)))

coef2<-coef2 %>%
  mutate(upper=coef+1.96*se,
         lower=coef-1.96*se,
         mod="Model 2",
         var=names(coef(cab_reg2)))

coef_plot<-data.frame(rbind(coef1, coef2))

coef_plot$var<-recode(coef_plot$var, "
       'resigcalls' = 'RC pooled';
       'rc_opposition_dum' = 'RC opposition';
       'rc_party_dum' = 'RC own party';
       'rc_paper_dum' = 'RC newspaper';
       'age_cen' = 'Age (centered)';
       'factor(gender)Female' = 'Gender (female)';
       'factor(youthCen)1' = 'Youth exp. (central)';
       'factor(youthLoc)1' = 'Youth exp. (local)';
       'minister_exp_cum_y_lag' = 'Cabinet exp.';
       'factor(parlTen_dum)1' = 'Parliamentary exp.';
       'factor(education_dum)Lower' = 'Education (lower)';
       'factor(reshuffle)1' = 'Reshuffle';
       'factor(CabinetType)Majority' = 'Cabinet type (majority)';
       'factor(structure)Coalition' = 'Cabinet structure (coalition)';
       'timeint' = 'Time'")

coef_plot$var<-ifelse(coef_plot$var=="rc_opposition_dum:timeint", "RC opposition:time", coef_plot$var)
coef_plot$var<-ifelse(coef_plot$var=="timeint:rc_paper_dum", "RC newspaper:time", coef_plot$var)
coef_plot$var<-ifelse(coef_plot$var=="timeint:rc_party_dum", "RC own party:time", coef_plot$var)
levels(factor(coef_plot$var))

coef_plot$var<-factor(coef_plot$var, levels=rev(c("RC pooled",  
                                                  "Time", "RC opposition", "RC own party", "RC newspaper",
                                                  "RC opposition:time", "RC own party:time", "RC newspaper:time",
                                                  "Cabinet exp.", "Parliamentary exp.",
                                                  "Youth exp. (central)", "Youth exp. (local)",
                                                  "Cabinet type (majority)", "Cabinet structure (coalition)", 
                                                  "Age (centered)", "Gender (female)", "Education (lower)", "Reshuffle")))

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\model1_plot.pdf", height=6)
ggplot(coef_plot[which(coef_plot$mod=="Model 1"),], aes(x=var, y=coef))+
  geom_pointrange(aes(ymax=upper, ymin=lower), position=position_dodge(width=-.5), size=.75, color="#004C49")+
  #geom_text(aes(label=round(coef, digits=3)), vjust=-1, size=4)+
  geom_abline(intercept=0, slope=0, size=.5)+
  geom_vline(xintercept=c(1.5, 4.5, 6.5, 10.5), slope=0, linetype="dashed")+
  scale_x_discrete(limits=levels(var), expand=c(0,.5))+
  scale_y_continuous(breaks=seq(-3,4,.5), expand=c(0,.05))+
  coord_flip()+
  labs(y="Hazard rates", x=NULL, fill=NULL, color=NULL)+
  theme(legend.position=c(.9,.95),
        legend.key=element_blank(),
        legend.key.width=unit(1.5, units="cm"),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line())
dev.off()

coef_plot

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\coef_plot.pdf", height=6)
ggplot(coef_plot, aes(x=var, y=coef, group=mod, color=mod, fill=mod))+
  geom_pointrange(aes(ymax=upper, ymin=lower), position=position_dodge(width=.5), size=.75)+
#  geom_text(aes(label=ifelse(mod=="Model 1", NA, round(coef, digits=3))), vjust=-1, size=4, color="black")+
  geom_vline(xintercept=c(1.5, 4.5, 6.5, 10.5), slope=0, linetype="dashed")+
  geom_abline(intercept=0, slope=0, size=.5)+
  scale_x_discrete(limits=levels(var), expand=c(0,.5))+
  scale_y_continuous(breaks=seq(-3,4,.5), expand=c(0,.1))+
  scale_color_manual(values=c("#0ACCC1", "#004C49"))+
  coord_flip()+
  labs(y="Hazard rates", x=NULL, fill=NULL, color=NULL)+
  theme(legend.position=c(.9,.95),
        legend.key=element_blank(),
        legend.key.width=unit(1.5, units="cm"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line())
dev.off()  

rm(coef1, coef2, coef_plot)

#####################################
######Post-estimation analyses#######
#####################################


#Frailty estimates table
frail_tab<-data.frame(jurisdiction=levels(factor(ministers$jurisdiction)), variance_mod1=cab_reg3$frail, variance_mod2=cab_reg2$frail)

stargazer(frail_tab, summary=FALSE, rownames=FALSE,
          covariate.labels=c("", "Model 1", "Model 2"),
          align=TRUE,
          title="Frailty terms",
          label="frail",
          out="C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Tables\\frail.tex")
rm(frail_tab)

#Proportional hazards tests
rownames(phtest_3$table)<-c("RC pooled", "Age", "Gender (male)", "Youth experience (central)", "Youth experience (local)", "Cabinet exp.",
                            "Parliamentary exp.", "Education (lower)", "Reshuffle", "Cabinet type (majority)", 
                            "Cabinet structure (coalition)", 
                            "Global")

rownames(phtest_2$table)<-c("RC opposition", "Time", "RC newspaper", "RC own party",
                            "Age", "Gender (male)", "Youth experience (central)", "Youth experience (local)", "Cabinet exp.",
                            "Parliamentary exp.", "Education (lower)", "Reshuffle", "Cabinet type (majority)", 
                            "Cabinet structure (coalition)", 
                            "Time:RC opposition", "Time:RC newspaper", "Time:RC own party", "Global")

stargazer(phtest_3$table, phtest_2$table, summary=FALSE, align=TRUE, 
          title=c("Global proportional hazards test -- Model 1",
                  "Global proportional hazards test -- Model 2"),
          label=c("phtest_mod1", "phtest_mod2"),
          out="C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Tables\\ph_test.tex")
rm(phtest_3, phtest_2)

######################
####Predicted risk####
######################

modpred<-with(ministers, data.frame(duration, event2))

modpred$pred2<-predict(cab_reg2, type="risk")
modpred$pred3<-predict(cab_reg3, type="risk")

modpred<-melt(modpred, measure.vars=c("pred2", "pred3"))
modpred$variable<-factor(modpred$variable, labels=c("Model 1", "Model 2"))

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\pred_plot.pdf", height=4)
ggplot(modpred, aes(x=duration, y=value,
                    group=factor(event2, labels=c("Non-resignation", "Forced exits")),
                    color=factor(event2, labels=c("Non-resignation", "Forced exits")),
                    fill=factor(event2, labels=c("Non-resignation", "Forced exits"))))+
  geom_smooth(method="lm")+
  facet_wrap(~variable)+
  scale_y_continuous(breaks=seq(0,10,.2), expand=c(0,0))+
  scale_x_continuous(breaks=seq(0,1600,200), expand=c(0,6))+
  scale_color_manual(values=c("#0ACCC1", "#004C49"))+
  scale_fill_manual(values=c("#0ACCC1", "#004C49"))+
  labs(x="Duration", y="Predicted risk", color=NULL, fill=NULL)+
  theme(legend.position=c(.9,.85),
        legend.key=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        strip.background=element_rect(fill="white", color="black"))
dev.off()

rm(modpred)

##################################
#####Influential case analysis####
##################################

#Model1####
influence<-data.frame(id=1:625, resid(cab_reg3, type = "dfbeta"))

influence<-melt(influence, id.vars="id")
influence$variable<-factor(influence$variable, labels=c("RC pooled", "Age (cen.)", "Gender (male)", "Youth exp. (central)",
                                                        "Youth exp. (local)", "Cabinet exp.", "Parliamentary exp.", "Education (lower)",
                                                        "Reshuffle", "Cab. type (min.)", "Cab. str. (sing.part.)"))


pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\influence_plot1.pdf", height=6)
ggplot(influence, aes(x=id, y=value))+
  geom_point()+
  geom_linerange(aes(ymin=0, ymax=value))+
  geom_hline(yintercept=0, color="darkcyan", size=1)+
  scale_y_continuous(limits=c(-.5,.5), breaks=seq(-1,1,.25))+
  facet_wrap(~variable, drop=FALSE)+
  labs(x="Minister id", y="Change in coefficient")+
  theme(strip.background=element_rect(fill="white", color="black"),
        axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=.25),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank())
dev.off()

#####

#Model 2####
influence<-data.frame(id=1:625, resid(cab_reg2, type = "dfbeta"))

exclude<-which(influence$X3>.1 | influence$X3<(-.1))

influence<-melt(influence, id.vars="id")
influence$variable<-factor(influence$variable, labels=c("RC opposition", "Time", "RC newspaper", "RC own party",
                                                        "Age (cen.)", "Gender (male)", "Youth exp. (cen.)", "Youth exp. (loc.)", 
                                                        "Cabinet exp.", "Parl. exp.", "Education (lower)", "Reshuffle", 
                                                        "Cab. type (maj.)", "Cab. str. (coal.)", 
                                                        "Time:RC opp.", "Time:RC news.", "Time:RC own par."))


pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\influence_plot2.pdf", height=6)
ggplot(influence, aes(x=id, y=value))+
  geom_point()+
  geom_linerange(aes(ymin=0, ymax=value))+
  geom_hline(yintercept=0, color="darkcyan", size=1)+
  facet_wrap(~variable, drop=FALSE)+
  labs(x="Minister id", y="Change in coefficient")+
  theme(strip.background=element_rect(fill="white", color="black"),
        axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=.25),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank())
dev.off()

exclude_data<-ministers[-exclude,]

exclude_paper_reg<-coxph(Surv(duration, event2) ~ rc_opposition_dum*timeint + rc_paper_dum*timeint + rc_party_dum*timeint +
                  age_cen + factor(gender) + factor(youthCen) + factor(youthLoc) + minister_exp_cum_y_lag + factor(parlTen_dum) +
                  factor(education_dum) + factor(reshuffle) + factor(CabinetType) + factor(structure) + frailty(jurisdiction),
                data=ministers, subset=prime_minister==0)
summary(exclude_paper_reg)

#RC newspaper####
pred1<-with(ministers, data.frame(rc_opposition_dum=0,
                                  rc_paper_dum=0,
                                  rc_party_dum=0,
                                  rc_organization_dum=0,
                                  age_cen=median(age_cen),
                                  gender="Male",
                                  minister_exp_cum_y_lag=0,
                                  parlTen_dum=0,
                                  youthCen=0,
                                  youthLoc=0,
                                  education_dum="Higher",
                                  reshuffle=0,
                                  CabinetType="Minority",
                                  structure="Single-party",
                                  timeint=seq(min(timeint), max(timeint), .1),
                                  jurisdiction="Justice"))

pred2<-with(ministers, data.frame(rc_opposition_dum=0,
                                  rc_paper_dum=1,
                                  rc_party_dum=0,
                                  rc_organization_dum=0,
                                  age_cen=median(age_cen),
                                  gender="Male",
                                  minister_exp_cum_y_lag=0,
                                  parlTen_dum=0,
                                  youthCen=0,
                                  youthLoc=0,
                                  education_dum="Higher",
                                  reshuffle=0,
                                  CabinetType="Minority",
                                  structure="Single-party",
                                  timeint=seq(min(timeint), max(timeint), .1),
                                  jurisdiction="Justice"))


pred1<-data.frame(predict(exclude_paper_reg, newdata=pred1, type="risk", se=TRUE, reference="sample"))
pred1$mod<-"RC paper=0"

pred2<-data.frame(predict(exclude_paper_reg, newdata=pred2, type="risk", se=TRUE, reference="sample"))
pred2$mod<-"RC paper=1"


pred_plot<-rbind(pred1, pred2)

pred_plot$upper<-pred_plot$fit+1.96*pred_plot$se.fit
pred_plot$lower<-pred_plot$fit-1.96*pred_plot$se.fit
pred_plot$rc<-rep(seq(min(ministers$timeint), max(ministers$timeint), .1), 2)
pred_plot$rc<-exp(pred_plot$rc)+1945.8

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\rcpaper_eff_plot.pdf", height=5)
ggplot(pred_plot[which(pred_plot$rc>=1949),], aes(x=rc, y=fit, group=mod, color=mod, fill=mod))+
  geom_line()+
  geom_ribbon(aes(ymax=upper, ymin=lower, color=NULL),alpha=.2) +
  geom_hline(aes(yintercept=1), linetype="dashed")+
  labs(y="Hazard Ratios", x="Year", color=NULL, fill=NULL)+
  scale_x_continuous(breaks=seq(1945,2015,4), expand=c(0,0))+
  scale_y_continuous(limits=c(0,5), breaks=seq(0,20,.5), expand=c(0,0))+
  scale_color_manual(values=c("#0000FF", "#009E00"))+
  scale_fill_manual(values=c("#0000FF", "#009E00"))+
  guides(color=guide_legend(label.position="left", keywidth=2, keyheight=1))+
  theme(legend.position=c(.91,.95),
        legend.key=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.border=element_blank(),
        strip.background=element_blank(),
        axis.title.y=element_text(vjust=1.5, siz=12),
        axis.title.x=element_text(vjust=0, size=12),
        axis.line=element_line())
dev.off()




rm(influence)
#####

############################
#######RC count model#######
############################

#Restructure the data
ministers2<-ministers %>%
  group_by(cabinet_name, nsd_id) %>%
  summarize(resigcalls=sum(resigcalls),
            rc_cum_lag=rc_cum_lag[1],
            duration=sum(duration),
            gender=gender[1],
            age=age[1],
            minister_exp_cum_y_lag=minister_exp_cum_y_lag[1],
            parlTen_dum=parlTen_dum[1],
            education_dum=education_dum[1],
            jurisdiction=jurisdiction[1],
            youthLoc=youthLoc[1],
            youthCen=youthCen[1],
            CabinetType=CabinetType[1],
            structure=structure[1]) %>%
  mutate(dur_cen=duration-mean(duration),
         dur_cen_y=dur_cen/365.25,
         age_cen=age-mean(age),
         gender=factor(gender, levels=c("Male", "Female")),
         CabinetType=factor(CabinetType, levels=c("Minority", "Majority")),
         structure=factor(structure, levels=c("Single-party", "Coalition")))


#Negative binomial count model
negbin1<-glm.nb(resigcalls~rc_cum_lag + dur_cen_y + factor(gender) + age_cen + minister_exp_cum_y_lag + factor(parlTen_dum) + 
                  factor(education_dum) + factor(youthCen) + factor(youthLoc)+ factor(CabinetType) + factor(structure), data=ministers2)
summary(negbin1)

#Regression table
stargazer(negbin1, out="C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Tables\\rc_reg.tex",
          ci=TRUE, star.char="", omit.stat="ll", notes.append=FALSE, no.space=TRUE,
          title="Negative binomial regression model", label="negbinreg", dep.var.labels="Resignation calls",
          notes=c("Estimates from negative binomial regression\\\ ", "model with confidence intervals in parentheses."),
          order=c(10, 1, 2, 5, 6, 8, 9, 11, 12, 4, 3, 7),
          covariate.labels=c("Intercept", "Cumulative RCs", "Duration (centered years)", 
                             "Cabinet exp.", "Parliamentary exp.", "Youth experience (central)", "Youth experience (local)",
                             "Cabinet type (majority)", "Cabinet structure (coalition)",
                             "Age (centered)", "Gender (female)", "Education (lower)"))


#Coef plot#####
coef3<-data.frame(coef=coef(negbin1), se=sqrt(diag(vcov(negbin1))))

coef3<-coef3 %>%
  mutate(upper=coef+1.96*se,
         lower=coef-1.96*se,
         mod="Negative binomial model",
         var=names(coef(negbin1)))

coef_plot<-data.frame(rbind(coef3))

coef_plot$var<-recode(coef_plot$var, "
                      '(Intercept)' = 'Intercept';
                      'rc_cum_lag' = 'Cumulative RCs';
                      'age_cen' = 'Age (centered)';
                      'factor(gender)Female' = 'Gender (female)';
                      'factor(youthCen)1' = 'Youth exp. (central)';
                      'factor(youthLoc)1' = 'Youth exp. (local)';
                      'minister_exp_cum_y_lag' = 'Cabinet exp.';
                      'factor(parlTen_dum)1' = 'Parliamentary exp.';
                      'factor(education_dum)Lower' = 'Education (lower)';
                      'dur_cen_y' = 'Duration (centered years)';
                      'factor(CabinetType)Majority' = 'Cabinet type (majority)';
                      'factor(structure)Coalition' = 'Cabinet structure (coalition)'")


coef_plot$var<-factor(coef_plot$var, levels=rev(c("Intercept", "Cumulative RCs", "Duration (centered years)", 
                                                  "Cabinet exp.", "Parliamentary exp.", 
                                                  "Youth exp. (central)", "Youth exp. (local)",
                                                  "Cabinet type (majority)", "Cabinet structure (coalition)",
                                                  "Age (centered)", "Gender (female)", "Education (lower)")))

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\model3_plot.pdf", height=6)
ggplot(coef_plot, aes(x=var, y=coef))+
  geom_pointrange(aes(ymax=upper, ymin=lower), size=.75, color="#004C49")+
  geom_abline(intercept=0, slope=0, size=.5)+
  #geom_text(aes(label=round(coef, digits=3)), vjust=-1, size=4)+
  scale_x_discrete(limits=levels(var), expand=c(0,.75))+
  scale_y_continuous(breaks=seq(-3,4,.5), expand=c(0,.05))+
  coord_flip()+
  labs(y="Coefficients", x=NULL, fill=NULL, color=NULL)+
  theme(panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line())
dev.off()
#####

#Model frame#####
modframe<-model.frame(negbin1)
colnames(modframe)[c(4,7,8:12)]<-c("gender", "parlTen_dum", "education_dum", "youthCen", "youthLoc", "CabinetType", "structure")

modframe$pred<-predict(negbin1, type="response", newdata=modframe)

modframe$predobs<-modframe$resigcalls-modframe$pred

#RMSE
sqrt(mean((modframe$resigcalls-modframe$pred^2),na.rm = TRUE))

#Predicet-Observed count plot
ggplot(modframe, aes(x=1:nrow(modframe), y=predobs))+
  geom_linerange(aes(ymin=0, ymax=predobs))


modframe2<-modframe[,c("resigcalls", "pred")]

modframe2<-melt(modframe2)

#Predicted vs. observed counts
ggplot(modframe2, aes(x=value, color=variable))+
  geom_freqpoly(binwidth=1, origin=-.50, size=.75)+
  scale_x_continuous(breaks=seq(0,6,1), expand=c(0,0))+
  scale_y_continuous(breaks=seq(0,700,50), expand=c(0,0))

percincrease(coef(negbin1), 1)
hazper(coef(negbin1), 4, 1.5)

rm(coef_plot, modframe, modframe2, coef3)
######

##################################
####Alternative specifications####
##################################

#Age as skill measure####
ministers3<-ministers %>%
  group_by(nsd_id) %>%
  arrange(start) %>%
  mutate(age_first=age[1])

ministers3$age_first_cen<-ministers3$age_first-mean(ministers3$age_first)

agefirst_reg<-coxph(Surv(dur_start, dur_end, event2) ~ resigcalls + age_cen + age_first_cen + factor(gender) + 
                  factor(youthCen) + factor(youthLoc) + minister_exp_cum_y_lag + factor(parlTen_dum) + 
                  factor(education_dum) + factor(reshuffle) + factor(CabinetType) + factor(structure) + frailty(jurisdiction),
                data=ministers3, subset=prime_minister==0)

summary(agefirst_reg)
cox.zph(agefirst_reg)

#Effectplot
pred1<-with(ministers3, data.frame(resigcalls=0,
                                  age_cen=median(age_cen),
                                  age_first_cen=min(age_first_cen):max(age_first_cen),
                                  gender="Male",
                                  minister_exp_cum_y_lag=0,
                                  parlTen_dum=0,
                                  youthCen=0,
                                  youthLoc=0,
                                  education_dum="Higher",
                                  reshuffle=0,
                                  CabinetType="Minority",
                                  structure="Single-party"))

pred1<-data.frame(predict(agefirst_reg, newdata=pred1, type="risk", se=TRUE, reference="sample"))

pred_plot<-rbind(pred1)

pred_plot$upper<-pred_plot$fit+1.96*pred_plot$se.fit
pred_plot$lower<-pred_plot$fit-1.96*pred_plot$se.fit
pred_plot$rc<-rep(min(ministers3$age_first_cen):max(ministers3$age_first_cen)+mean(ministers3$age_first), 1)


pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\agefirst_eff_plot.pdf", height=5)
ggplot(pred_plot, aes(x=rc, y=fit))+
  geom_line(stat="identity", color="#0A7500")+
  geom_ribbon(aes(ymax=upper, ymin=lower, color=NULL), alpha=.2, fill="#0A7500") +
  geom_hline(aes(yintercept=1), linetype="dashed")+
  labs(y="Hazard Ratios", x="Age at first cabinet")+
  scale_x_continuous(breaks=seq(30,70,5), expand=c(0,0), limits=c(28,70))+
  scale_y_continuous(breaks=seq(0,10,.5), expand=c(0,.21))+
  theme(legend.position=c(.15,.9),
        panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        strip.background=element_blank(),
        panel.margin=unit(1, "cm"),
        axis.line=element_line(),
        axis.title.y=element_text(vjust=1.5, siz=12),
        axis.title.x=element_text(vjust=0, size=12))
dev.off()

rm(agefirst_reg)
#####


#Polynomial age####
polyage_reg<-agefirst_reg<-coxph(Surv(dur_start, dur_end, event2) ~ resigcalls + poly(age_cen, 2, raw=TRUE) + factor(gender) + 
                                   factor(youthCen) + factor(youthLoc) + minister_exp_cum_y_lag + factor(parlTen_dum) + 
                                   factor(education_dum) + factor(reshuffle) + factor(CabinetType) + factor(structure) + frailty(jurisdiction),
                                 data=ministers3, subset=prime_minister==0)

summary(polyage_reg)
cox.zph(polyage_reg)

pred1<-with(ministers3, data.frame(resigcalls=0,
                                  age_cen=min(age_cen):max(age_cen),
                                  gender="Male",
                                  minister_exp_cum_y_lag=0,
                                  parlTen_dum=0,
                                  youthCen=0,
                                  youthLoc=0,
                                  education_dum="Higher",
                                  reshuffle=0,
                                  CabinetType="Minority",
                                  structure="Single-party"))


pred1<-data.frame(predict(polyage_reg, newdata=pred1, type="lp", se=TRUE, reference="sample"))

pred_plot<-rbind(pred1)

pred_plot$upper<-pred_plot$fit+1.96*pred_plot$se.fit
pred_plot$lower<-pred_plot$fit-1.96*pred_plot$se.fit
pred_plot$rc<-with(ministers3, rep(min(age_cen):max(age_cen)+mean(age), 1))


pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\polyage_eff_plot.pdf", height=5)
ggplot(pred_plot, aes(x=rc, y=fit))+
  geom_line(stat="identity", color="#0A7500")+
  geom_ribbon(aes(ymax=upper, ymin=lower, color=NULL), alpha=.2, fill="#0A7500") +
  geom_hline(aes(yintercept=0), linetype="dashed")+
  labs(y="Hazard Rate", x="Age (second-degree polynomial)")+
  scale_x_continuous(breaks=seq(30,75,5), expand=c(0,0))+
  scale_y_continuous(breaks=seq(-10,10,.5), expand=c(0,.21))+
  theme(legend.position=c(.15,.9),
        panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        strip.background=element_blank(),
        panel.margin=unit(1, "cm"),
        axis.line=element_line(),
        axis.title.y=element_text(vjust=1.5, siz=12),
        axis.title.x=element_text(vjust=0, size=12))
dev.off()

rm(pred1, polyage_reg, pred_plot)

#####

#RC per t####
ministers3$rc_per<-ministers$resigcalls/((as.numeric(ministers$end-ministers$start))/365.25)

rcper_reg<-agefirst_reg<-coxph(Surv(dur_start, dur_end, event2) ~ rc_per + age_cen + factor(gender) + 
                                   factor(youthCen) + factor(youthLoc) + minister_exp_cum_y_lag + factor(parlTen_dum) + 
                                   factor(education_dum) + factor(reshuffle) + factor(CabinetType) + factor(structure) + frailty(jurisdiction),
                                 data=ministers3, subset=prime_minister==0 & rc_per<5)
ministers3[which(ministers3$rc_per>5),c(3:7, 35, ncol(ministers3), 21)]

hazper(coef(rcper_reg), 1, 0)

summary(rcper_reg)
cox.zph(rcper_reg)
AIC(cab_reg3, rcper_reg)

#Regression tables
mods<-list(extract(cab_reg3, include.missings=FALSE, include.rsquared=FALSE, include.maxrs=FALSE, include.zph = FALSE),
           extract(cab_reg2, include.missings=FALSE, include.rsquared=FALSE, include.maxrs=FALSE, include.zph = FALSE),
           extract(rcper_reg, include.missings=FALSE, include.rsquared=FALSE, include.maxrs=FALSE, include.zph = FALSE))
texreg(mods, file="C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Tables\\regs.tex", use.packages=FALSE,
       digits=3, caption.above=TRUE, caption="Cox proportional hazard models", label="coxtable", dcolumn=TRUE,
       fontsize="small", single.row=TRUE, float.pos="!h",
       reorder.coef=c(1,19,13,12,15,14,16,18,17, 6,7,4,5, 10,11, 2,3,9,8), reorder.gof=c(1,2,3),
       groups=list("\\textbf{Performance}"=c(1:9), "\\textbf{Experience}"=c(10:13), "\\textbf{Cabinet}"=14:15, 
                   "\\textbf{Personal}"=c(16:19)),
       custom.coef.names=c("RC pooled", "Age (centered)", "Gender (female)", "Youth experience (central)", "Youth experience (local)",
                           "Cabinet exp.", "Parliamentary exp.", "Education (lower)", "Reshuffle", "Cabinet type (majority)", 
                           "Cabinet structure (coalition)", 
                           "RC opposition", "Time", "RC newspaper", "RC own party", 
                           "Time:RC opposition", "Time:RC newspaper", "Time:RC own party", "RC per year"),
       custom.note="%stars. Cox Proportion Hazards models where estimates are in hazard rates,
                    and standard errors in parentheses.")

rm(mods)

pred1<-with(ministers3, data.frame(rc_per=seq(0,2,.25),
                                  age_cen=median(age_cen),
                                  gender="Male",
                                  minister_exp_cum_y_lag=0,
                                  parlTen_dum=0,
                                  youthCen=0,
                                  youthLoc=0,
                                  education_dum="Higher",
                                  reshuffle=0,
                                  CabinetType="Minority",
                                  structure="Single-party"))


pred1<-data.frame(predict(rcper_reg, newdata=pred1, type="risk", se=TRUE, reference="sample"))

pred_plot<-rbind(pred1)

pred_plot$upper<-pred_plot$fit+1.96*pred_plot$se.fit
pred_plot$lower<-pred_plot$fit-1.96*pred_plot$se.fit
pred_plot$rc<-seq(0,2,.25)


pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\rcper_eff_plot.pdf", height=5)
ggplot(pred_plot, aes(x=rc, y=fit))+
  geom_line(stat="identity", color="#0A7500")+
  geom_ribbon(aes(ymax=upper, ymin=lower, color=NULL), alpha=.2, fill="#0A7500") +
  geom_hline(aes(yintercept=1), linetype="dashed")+
  labs(y="Hazard Ratios", x="Resignation calls (per year)")+
  scale_x_continuous(breaks=seq(0,5,.25), expand=c(0,0))+
  scale_y_continuous(breaks=seq(0,10,.5), expand=c(0,.21))+
  theme(legend.position=c(.15,.9),
        panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        strip.background=element_blank(),
        panel.margin=unit(1, "cm"),
        axis.line=element_line(),
        axis.title.y=element_text(vjust=1.5, siz=12),
        axis.title.x=element_text(vjust=0, size=12))
dev.off()

rm(pred1, pred_plot, rcper_reg)


#One party state exclusion####
ops_reg<-agefirst_reg<-coxph(Surv(dur_start, dur_end, event2) ~ resigcalls + age_cen + factor(gender) + 
                                 factor(youthCen) + factor(youthLoc) + minister_exp_cum_y_lag + factor(parlTen_dum) + 
                                 factor(education_dum) + factor(reshuffle) + factor(CabinetType) + factor(structure) + frailty(jurisdiction),
                               data=ministers, subset=start>"1965-10-11")

summary(ops_reg)

#Low duration cabinet duration exclusion####
lowdur_reg<-agefirst_reg<-coxph(Surv(dur_start, dur_end, event2) ~ resigcalls + age_cen + factor(gender) + 
                               factor(youthCen) + factor(youthLoc) + minister_exp_cum_y_lag + factor(parlTen_dum) + 
                               factor(education_dum) + factor(reshuffle) + factor(CabinetType) + factor(structure) + frailty(jurisdiction),
                             data=ministers, subset=cabinet_duration>365)
levels(factor(ministers$cabinet_duration))
round(summary(lowdur_reg)$coefficients[,1], digits=3)


##################################
###Survival models effect plots###
##################################


#RC (pooled)#####
pred1<-with(ministers, data.frame(resigcalls=0:5,
                                  age_cen=median(age_cen),
                                  gender="Male",
                                  minister_exp_cum_y_lag=0,
                                  parlTen_dum=0,
                                  youthCen=0,
                                  youthLoc=0,
                                  education_dum="Higher",
                                  reshuffle=0,
                                  CabinetType="Minority",
                                  structure="Single-party"))


pred1<-data.frame(predict(cab_reg3, newdata=pred1, type="risk", se=TRUE, reference="sample"))

pred_plot<-rbind(pred1)

pred_plot$upper<-pred_plot$fit+1.96*pred_plot$se.fit
pred_plot$lower<-pred_plot$fit-1.96*pred_plot$se.fit
pred_plot$rc<-rep(0:5, 1)


pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\rc_eff_plot.pdf", height=5)
ggplot(pred_plot, aes(x=rc, y=fit))+
  geom_line(stat="identity", color="#0A7500")+
  geom_ribbon(aes(ymax=upper, ymin=lower, color=NULL), alpha=.2, fill="#0A7500") +
  geom_hline(aes(yintercept=1), linetype="dashed")+
  labs(y="Hazard Ratios", x="Resignation calls")+
  scale_x_continuous(breaks=seq(0,5,1), expand=c(0,0))+
  scale_y_continuous(breaks=seq(0,10,.5), expand=c(0,.21))+
  theme(legend.position=c(.15,.9),
        panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        strip.background=element_blank(),
        panel.margin=unit(1, "cm"),
        axis.line=element_line(),
        axis.title.y=element_text(vjust=1.5, siz=12),
        axis.title.x=element_text(vjust=0, size=12))
dev.off()

#Cab exp.####
pred1<-with(ministers, data.frame(resigcalls=0,
                                  age_cen=median(age_cen),
                                  gender="Male",
                                  minister_exp_cum_y_lag=min(minister_exp_cum_y_lag):10,
                                  parlTen_dum=0,
                                  youthCen=0,
                                  youthLoc=0,
                                  education_dum="Higher",
                                  reshuffle=0,
                                  CabinetType="Minority",
                                  structure="Single-party"))


pred1<-data.frame(predict(cab_reg3, newdata=pred1, type="risk", se=TRUE, reference="sample"))

pred_plot<-rbind(pred1)

pred_plot$upper<-pred_plot$fit+1.96*pred_plot$se.fit
pred_plot$lower<-pred_plot$fit-1.96*pred_plot$se.fit
pred_plot$rc<-rep(min(ministers$minister_exp_cum_y_lag):10, 1)

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\cabexp_eff_plot.pdf", height=5)
ggplot(pred_plot, aes(x=rc, y=fit))+
  geom_line(stat="identity", color="#0A7500")+
  geom_ribbon(aes(ymax=upper, ymin=lower, color=NULL), alpha=.2, fill="#0A7500") +
  geom_hline(aes(yintercept=1), linetype="dashed")+
  labs(y="Hazard Ratios", x="Years of previous cabinet experience")+
  scale_x_continuous(breaks=seq(0,50,1), expand=c(0,0))+
  scale_y_continuous(limits=c(.5,4), breaks=seq(0,10,.5), expand=c(0,0))+
  theme(legend.position=c(.15,.9),
        panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        strip.background=element_blank(),
        panel.margin=unit(1, "cm"),
        axis.line=element_line(),
        axis.title.y=element_text(vjust=1.5, siz=12),
        axis.title.x=element_text(vjust=0, size=12))
dev.off()

#Parlimentary experience####
pred1<-with(ministers, data.frame(resigcalls=0,
                                  age_cen=median(age_cen),
                                  gender="Male",
                                  minister_exp_cum_y_lag=0,
                                  parlTen_dum=0,
                                  youthCen=0,
                                  youthLoc=0,
                                  education_dum="Higher",
                                  reshuffle=0,
                                  CabinetType="Minority",
                                  structure="Single-party"))

pred2<-with(ministers, data.frame(resigcalls=0,
                                  rc_cum_lag=0,
                                  age_cen=median(age_cen),
                                  gender="Male",
                                  minister_exp_cum_y_lag=0,
                                  parlTen_dum=1,
                                  youthCen=0,
                                  youthLoc=0,
                                  education_dum="Higher",
                                  reshuffle=0,
                                  CabinetType="Minority",
                                  structure="Single-party"))


pred1<-data.frame(predict(cab_reg3, newdata=pred1, type="risk", se=TRUE, reference="sample"))
pred1$mod<-"None"

pred2<-data.frame(predict(cab_reg3, newdata=pred2, type="risk", se=TRUE, reference="sample"))
pred2$mod<-"Parliamentary exp."

pred_plot<-rbind(pred1, pred2)

pred_plot$upper<-pred_plot$fit+1.96*pred_plot$se.fit
pred_plot$lower<-pred_plot$fit-1.96*pred_plot$se.fit

pred_plot$mod<-factor(pred_plot$mod, levels=c("None", "Parliamentary exp."))

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\parl_eff_plot.pdf", height=5)
ggplot(pred_plot, aes(x=mod, y=fit, group=1))+
  geom_point(size=5, shape=16, color=c("#0A7500", "darkcyan"))+
  geom_errorbar(aes(ymax=upper, ymin=lower), width=.5, color=c("#0A7500", "darkcyan"), size=2)+
  geom_hline(aes(yintercept=1), linetype="dashed")+
  labs(y="Hazard Ratios", x="Parliament experience")+
  scale_x_discrete(expand=c(0,.5))+
  scale_y_continuous(limits=c(-.04,1.5), breaks=seq(0,10,.25), expand=c(0,0))+
  theme(legend.position=c(.15,.9),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.border=element_blank(),
        strip.background=element_blank(),
        panel.margin=unit(1, "cm"),
        axis.title.y=element_text(vjust=1.5, siz=12),
        axis.title.x=element_text(vjust=0, size=12),
        axis.line=element_line())
dev.off()

#RC newspaper####
pred1<-with(ministers, data.frame(rc_opposition_dum=0,
                                  rc_paper_dum=0,
                                  rc_party_dum=0,
                                  rc_organization_dum=0,
                                  age_cen=median(age_cen),
                                  gender="Male",
                                  minister_exp_cum_y_lag=0,
                                  parlTen_dum=0,
                                  youthCen=0,
                                  youthLoc=0,
                                  education_dum="Higher",
                                  reshuffle=0,
                                  CabinetType="Minority",
                                  structure="Single-party",
                                  timeint=seq(min(timeint), max(timeint), .1),
                                  jurisdiction="Justice"))

pred2<-with(ministers, data.frame(rc_opposition_dum=0,
                                  rc_paper_dum=1,
                                  rc_party_dum=0,
                                  rc_organization_dum=0,
                                  age_cen=median(age_cen),
                                  gender="Male",
                                  minister_exp_cum_y_lag=0,
                                  parlTen_dum=0,
                                  youthCen=0,
                                  youthLoc=0,
                                  education_dum="Higher",
                                  reshuffle=0,
                                  CabinetType="Minority",
                                  structure="Single-party",
                                  timeint=seq(min(timeint), max(timeint), .1),
                                  jurisdiction="Justice"))


pred1<-data.frame(predict(cab_reg2, newdata=pred1, type="risk", se=TRUE, reference="sample"))
pred1$mod<-"RC paper=0"

pred2<-data.frame(predict(cab_reg2, newdata=pred2, type="risk", se=TRUE, reference="sample"))
pred2$mod<-"RC paper=1"


pred_plot<-rbind(pred1, pred2)

pred_plot$upper<-pred_plot$fit+1.96*pred_plot$se.fit
pred_plot$lower<-pred_plot$fit-1.96*pred_plot$se.fit
pred_plot$rc<-rep(seq(min(ministers$timeint), max(ministers$timeint), .1), 2)
pred_plot$rc<-exp(pred_plot$rc)+1945.8

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\rcpaper_eff_plot.pdf", height=5)
ggplot(pred_plot[which(pred_plot$rc>=1949),], aes(x=rc, y=fit, group=mod, color=mod, fill=mod))+
  geom_line()+
  geom_ribbon(aes(ymax=upper, ymin=lower, color=NULL),alpha=.2) +
  geom_hline(aes(yintercept=1), linetype="dashed")+
  labs(y="Hazard Ratios", x="Year", color=NULL, fill=NULL)+
  scale_x_continuous(breaks=seq(1945,2015,4), expand=c(0,0))+
  scale_y_continuous(limits=c(0,5), breaks=seq(0,20,.5), expand=c(0,0))+
  scale_color_manual(values=c("#0000FF", "#009E00"))+
  scale_fill_manual(values=c("#0000FF", "#009E00"))+
  guides(color=guide_legend(label.position="left", keywidth=2, keyheight=1))+
  theme(legend.position=c(.91,.95),
        legend.key=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.border=element_blank(),
        strip.background=element_blank(),
        axis.title.y=element_text(vjust=1.5, siz=12),
        axis.title.x=element_text(vjust=0, size=12),
        axis.line=element_line())
dev.off()

#RC opposision####

pred3<-with(ministers, data.frame(rc_opposition_dum=0,
                                  rc_paper_dum=0,
                                  rc_party_dum=0,
                                  rc_organization_dum=0,
                                  age_cen=median(age_cen),
                                  gender="Male",
                                  minister_exp_cum_y_lag=0,
                                  parlTen_dum=0,
                                  youthCen=0,
                                  youthLoc=0,
                                  education_dum="Higher",
                                  reshuffle=0,
                                  CabinetType="Minority",
                                  structure="Single-party",
                                  timeint=seq(min(timeint), max(timeint), .1),
                                  jurisdiction="Justice"))

pred4<-with(ministers, data.frame(rc_opposition_dum=1,
                                  rc_paper_dum=0,
                                  rc_party_dum=0,
                                  rc_organization_dum=0,
                                  age_cen=median(age_cen),
                                  gender="Male",
                                  minister_exp_cum_y_lag=0,
                                  parlTen_dum=0,
                                  youthCen=0,
                                  youthLoc=0,
                                  education_dum="Higher",
                                  reshuffle=0,
                                  CabinetType="Minority",
                                  structure="Single-party",
                                  timeint=seq(min(timeint), max(timeint), .1),
                                  jurisdiction="Justice"))


pred3<-data.frame(predict(cab_reg2, newdata=pred3, type="risk", se=TRUE, reference="sample"))
pred3$mod<-"RC opposition=0"

pred4<-data.frame(predict(cab_reg2, newdata=pred4, type="risk", se=TRUE, reference="sample"))
pred4$mod<-"RC opposition=1"

pred_plot<-rbind(pred3, pred4)

pred_plot$upper<-pred_plot$fit+1.96*pred_plot$se.fit
pred_plot$lower<-pred_plot$fit-1.96*pred_plot$se.fit
pred_plot$rc<-rep(seq(min(ministers$timeint), max(ministers$timeint), .1), 2)
pred_plot$rc<-exp(pred_plot$rc)+1945.8

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\rcopp_eff_plot.pdf", height=5)
ggplot(pred_plot[which(pred_plot$rc>=1950),], aes(x=rc, y=fit, group=mod, color=mod, fill=mod))+
  geom_line()+
  geom_ribbon(aes(ymax=upper, ymin=lower, color=NULL),alpha=.2) +
  geom_hline(aes(yintercept=1), linetype="dashed")+
  labs(y="Hazard Ratios", x="Year", color=NULL, fill=NULL)+
  scale_x_continuous(breaks=seq(1945,2015,4), expand=c(0,0))+
  scale_y_continuous(breaks=seq(0,20,.5), expand=c(0,0))+
  scale_color_manual(values=c("#0000FF", "#009E00"))+
  scale_fill_manual(values=c("#0000FF", "#009E00"))+
  guides(color=guide_legend(label.position="left", keywidth=2, keyheight=1))+
  theme(legend.position=c(.9,.912),
        legend.key=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.border=element_blank(),
        strip.background=element_blank(),
        axis.title.y=element_text(vjust=1.5, siz=12),
        axis.title.x=element_text(vjust=0, size=12),
        axis.line=element_line())
dev.off()

#RC own party####

pred3<-with(ministers, data.frame(rc_opposition_dum=0,
                                  rc_paper_dum=0,
                                  rc_party_dum=0,
                                  rc_organization_dum=0,
                                  age_cen=median(age_cen),
                                  gender="Male",
                                  minister_exp_cum_y_lag=0,
                                  parlTen_dum=0,
                                  youthCen=0,
                                  youthLoc=0,
                                  education_dum="Higher",
                                  reshuffle=0,
                                  CabinetType="Minority",
                                  structure="Single-party",
                                  timeint=seq(min(timeint), max(timeint), .1),
                                  jurisdiction="Justice"))

pred4<-with(ministers, data.frame(rc_opposition_dum=0,
                                  rc_paper_dum=0,
                                  rc_party_dum=1,
                                  rc_organization_dum=0,
                                  age_cen=median(age_cen),
                                  gender="Male",
                                  minister_exp_cum_y_lag=0,
                                  parlTen_dum=0,
                                  youthCen=0,
                                  youthLoc=0,
                                  education_dum="Higher",
                                  reshuffle=0,
                                  CabinetType="Minority",
                                  structure="Single-party",
                                  timeint=seq(min(timeint), max(timeint), .1),
                                  jurisdiction="Justice"))


pred3<-data.frame(predict(cab_reg2, newdata=pred3, type="risk", se=TRUE, reference="sample"))
pred3$mod<-"RC party=0"

pred4<-data.frame(predict(cab_reg2, newdata=pred4, type="risk", se=TRUE, reference="sample"))
pred4$mod<-"RC party=1"

pred_plot<-rbind(pred3, pred4)

pred_plot$upper<-pred_plot$fit+1.96*pred_plot$se.fit
pred_plot$lower<-pred_plot$fit-1.96*pred_plot$se.fit
pred_plot$rc<-rep(seq(min(ministers$timeint), max(ministers$timeint), .1), 2)
pred_plot$rc<-exp(pred_plot$rc)+1945.8

pdf("C:\\Users\\Martin\\Dropbox\\Master\\Masteroppgave\\LaTex\\Plots\\rcparty_eff_plot.pdf", height=5)
ggplot(pred_plot[which(pred_plot$rc>=1954),], aes(x=rc, y=fit, group=mod, color=mod, fill=mod))+
  geom_line()+
  geom_ribbon(aes(ymax=upper, ymin=lower, color=NULL),alpha=.2) +
  geom_hline(aes(yintercept=1), linetype="dashed")+
  labs(y="Hazard Ratios", x="Year", color=NULL, fill=NULL)+
  scale_x_continuous(breaks=seq(1945,2015,4), expand=c(0,0))+
  scale_y_continuous(breaks=seq(0,20,.5), expand=c(0,0))+
  scale_color_manual(values=c("#0000FF", "#009E00"))+
  scale_fill_manual(values=c("#0000FF", "#009E00"))+
  guides(color=guide_legend(label.position="left", keywidth=2, keyheight=1))+
  theme(legend.position=c(.9,.95),
        legend.key=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.border=element_blank(),
        strip.background=element_blank(),
        axis.title.y=element_text(vjust=1.5, siz=12),
        axis.title.x=element_text(vjust=0, size=12),
        axis.line=element_line())
dev.off()


rm(pred1, pred2, pred3, pred4, pred_plot, ministers2, ministers3)

rm(sl, exclude_data, agefirst_reg, exclude, exclude_paper_reg, lowdur_reg, ops_reg, pred)

###########################
#######Copying files#######
###########################

