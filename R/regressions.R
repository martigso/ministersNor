library(survival);library(dplyr);library(MASS)

data(ministers, package="ministersNor")
head(ministers)

#Pooled RC model
model_1<-coxph(Surv(dur_start, dur_end, event2) ~ resigcalls + age_cen + factor(gender) + 
                 factor(youthCen) + factor(youthLoc) + minister_exp_cum_y_lag + factor(parlTen_dum) + 
                 factor(education_dum) + factor(reshuffle) + factor(CabinetType) + factor(structure) + frailty(jurisdiction),
               data=ministers, subset=prime_minister==0)

summary(model_1)

hazper(coef(model_1), 1, 0)

#Actor based RC model
model_2<-coxph(Surv(duration, event2) ~ rc_opposition_dum*timeint + rc_paper_dum*timeint + rc_party_dum*timeint +
                  age_cen + factor(gender) + factor(youthCen) + factor(youthLoc) + minister_exp_cum_y_lag + factor(parlTen_dum) +
                  factor(education_dum) + factor(reshuffle) + factor(CabinetType) + factor(structure) + frailty(jurisdiction),
                data=ministers, subset=prime_minister==0 & nsd_id!=299)
summary(model_2)

hazper(coef(model_2), 1, 0)


#Count model for resingation calls
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
            structure=structure[1],
            prime_minister=prime_minister[1]) %>%
  mutate(dur_cen=duration-mean(duration),
         dur_cen_y=dur_cen/365.25,
         age_cen=age-mean(age),
         gender=factor(gender, levels=c("Male", "Female")),
         CabinetType=factor(CabinetType, levels=c("Minority", "Majority")),
         structure=factor(structure, levels=c("Single-party", "Coalition")))


#Negative binomial count model
negbin1<-glm.nb(resigcalls~rc_cum_lag + dur_cen_y + factor(gender) + age_cen + minister_exp_cum_y_lag + factor(parlTen_dum) + 
                  factor(education_dum) + factor(youthCen) + factor(youthLoc)+ factor(CabinetType) + factor(structure),
                data=ministers2, subset=prime_minister==0 & nsd_id!=299)

summary(negbin1)

