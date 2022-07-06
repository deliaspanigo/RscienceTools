#' @title Crear una lista con nombres predeterminados
#' @description Esta es una funcion de ejemplo para multiplicar un valor o vector por dos.
#' @param names es un vector que detalla los nombres a utilizar.
#' @details Esta funcion es parte del tutorial para hacer paquetes del curso de Metodos Multivariados 2017
#' en el ITAM. La funcion recibe un elemento  que puede ser un numero o un vector numerico y lo devuelve
#' multiplicado por dos. La documentacion de ayuda es generada usando roxygen2.
#' @examples
#' namel2(names = colnames(mtcars))
#' namel2(names = colnames(mtcars), vec = colnames(mtcars))
#' @export
#'
#helper function (convert vector to named list)
namel2<-function (names, vec = NA){

  if(length(vec) == 1) vec <- rep(vec[1], length(names))

  tmp <-as.list(vec)
  names(tmp) <- names
  tmp
}
