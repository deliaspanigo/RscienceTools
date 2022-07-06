#' @title Redondea como lo hacemos en la primaria
#' @description Esta es una funcion de ejemplo para multiplicar un valor o vector por dos.
#' @param x es un vector que detalla los nombres a utilizar.
#' @details Esta funcion es parte del tutorial para hacer paquetes del curso de Metodos Multivariados 2017
#' en el ITAM. La funcion recibe un elemento que puede ser un numero o un vector numerico y lo devuelve
#' multiplicado por dos. La documentacion de ayuda es generada usando roxygen2.
#' @examples
#' round(0.5)
#' round2(0.5)
#' @export
#'
# Round like a human
round2 <- function(x, n) {
  posneg <- sign(x)

  z <- abs(x)*10^n
  z <- z + 0.5
  z <- trunc(z)
  z <- z/10^n
  z*posneg
}
