#' @export 
percincrease<-function(beta, delta){
  100*(exp(beta*delta)-1)
}
