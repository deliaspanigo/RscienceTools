#' @title De numeros a letras
#' @description Esta es una funcion de ejemplo para multiplicar un valor o vectorpor dos.
#' @param n es un vector que detalla los nombres a utilizar.
#' @details Esta funcion es parte del tutorial para hacer paquetes del curso de Metodos Multivariados 2017
#' en el ITAM. La funcion recibe un elemento que puede ser un numero o un vector numerico y lo devuelve
#' multiplicado por dos. La documentacion de ayuda es generada usando roxygen2.
#' @examples
#' num2let(1)
#' num2let(c(3, 10, 256, 1000, 2))
#' @export
#'
#'
num2let <- function(n, lets = LETTERS) {
  base <- length(lets)
  if (length(n) > 1) return(sapply(n, num2let, lets = lets))
  stopifnot(n > 0)
  out <- ""
  repeat {
    if (n > base) {
      rem <- (n-1) %% base
      n <- (n-1) %/% base
      out <- paste0(lets[rem+1], out)
    } else return( paste0(lets[n], out) )
  }
}


