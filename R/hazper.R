#' Return hazard ration from Cox PH model
#' 
#' A function to calculate unit increases from Cox Proportional Hazard models
#' 
#' 
#' @param b Coefficient(s) from a coxph-object
#' @param x1 Unit increase to 
#' @param X0 Unit increase from
#' 
#' @return A vector of percentage increase in hazard ratio
#' 
#' @seealso \code{\link{percincrease}}
#' 
#' @examples
#' data(ministers)
#' 
#' model_1<-coxph(Surv(dur_start, dur_end, event2) ~ resigcalls + age_cen + factor(gender) + 
#'                    factor(youthCen) + factor(youthLoc) + minister_exp_cum_y_lag + 
#'                    factor(parlTen_dum) + factor(education_dum) + factor(reshuffle) + 
#'                    factor(CabinetType) + factor(structure) + frailty(jurisdiction),
#'                 data=ministers, subset=prime_minister==0 & nsd_id!=299)
#' hazper(coef(model_1), 1, 0)
#' 
#' @export
#' 
hazper<-function(b, x1, x0){
  y<-round(((exp(b*x1)-exp(b*x0))/exp(b*x0))*100, digits=3)
  y
}