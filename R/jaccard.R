#'Función para calcular el coeficiente Jaccard.
#'
#'Esta función calcula el coeficiente Jaccard de las variables de una tabla.
#'@param x La tabla con los valores booleanos de cada variable
#'@export
#'@keywords jaccard

jaccard <- function(x) {
  m <- matrix(NA, nrow=ncol(x),ncol=ncol(x),dimnames=list(colnames(x),colnames(x)))
  jacc <- as.data.frame(m)
  for(i in 1:ncol(x)) {
    for(j in i:ncol(x)) {
      jacc[i,j]= length(which(x[,i] & x[,j])) / length(which(x[,i] | x[,j]))
      jacc[j,i]=jacc[i,j]        
    }
  }
  return(jacc)
}
