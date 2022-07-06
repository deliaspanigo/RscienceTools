#' @title De letras a numeros
#' @description Esta es una funcion de ejemplo para multiplicar un valor o vector por dos.
#' @param x es un vector que detalla los nombres a utilizar.
#' @param lets es un vector que detalla los nombres a utilizar.
#' @details Esta funcion es parte del tutorial para hacer paquetes del curso de Metodos Multivariados 2017
#' en el ITAM. La funcion recibe un elemento que puede ser un numero o un vector numerico y lo devuelve
#' multiplicado por dos. La documentacion de ayuda es generada usando roxygen2.
#' @examples
#' num2let(x = 1)
#' num2let(x = c(3, 10, 256, 1000, 2))
#' @export
#'
#'
let2num <- function(x, lets = LETTERS) {
  base <- length(lets)
  s <- strsplit(x, "")
  sapply(s, function(x) sum((match(x, lets)) * base ^ seq(length(x) - 1, 0)))
}
