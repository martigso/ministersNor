#' @export
hazper<-function(b, x1, x0){
  y<-round(((exp(b*x1)-exp(b*x0))/exp(b*x0))*100, digits=3)
  y
}