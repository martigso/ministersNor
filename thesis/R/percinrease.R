#' Percentage unit increase from negative binomial count models
#' 
#' A function to calculate unit percentage increase from negative binomial count models
#' 
#' @param beta The regression coefficient(s)
#' @param delta Number of unit increases
#' 
#' @return None
#'
#' @seealso \code{\link{hazper}}
#' @keywords hm
#'
#' @examples
#' data(ministers)
#' 
#' ministers2<-ministers %>%
#' group_by(cabinet_name, nsd_id) %>%
#'  summarize(resigcalls=sum(resigcalls),
#'            rc_cum_lag=rc_cum_lag[1], 
#'            duration=sum(duration),
#'            gender=gender[1], 
#'            age=age[1],
#'            minister_exp_cum_y_lag=minister_exp_cum_y_lag[1],
#'            parlTen_dum=parlTen_dum[1],
#'            education_dum=education_dum[1],
#'            jurisdiction=jurisdiction[1],
#'            youthLoc=youthLoc[1],
#'            youthCen=youthCen[1], 
#'            CabinetType=CabinetType[1],
#'            structure=structure[1], 
#'            prime_minister=prime_minister[1]) %>%
#'  mutate(dur_cen=duration-mean(duration),
#'         dur_cen_y=dur_cen/365.25,
#'         age_cen=age-mean(age),
#'         gender=factor(gender, 
#'         levels=c("Male", "Female")),
#'         CabinetType=factor(CabinetType, levels=c("Minority", "Majority")), 
#'         structure=factor(structure, levels=c("Single-party", "Coalition")))
#' 
#' #Negative binomial count model
#' negbin1<-glm.nb(resigcalls~rc_cum_lag + dur_cen_y + factor(gender) + 
#'                  age_cen + minister_exp_cum_y_lag + factor(parlTen_dum) + 
#'                  factor(education_dum) + factor(youthCen) + 
#'                  factor(youthLoc)+ factor(CabinetType) + factor(structure),
#'                 data=ministers2, subset=prime_minister==0 & nsd_id!=299)
#'                                   
#' percincrease(coef(negbin1), 1)
#'
#' @export
#' @import MASS
#'

percincrease<-function(beta, delta){
  100*(exp(beta*delta)-1)
}
