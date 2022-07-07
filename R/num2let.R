#' @title De numeros a letras
#' @description La función \code{namel2()} realiza la conversion de numero a letra o combinacion de letras
#' correspondientes al abecedario latino.
#' @param n es un vector numerico con valores mayores o iguales a 1.
#' @param lets es un vector que detalla las letras del abecedario en mayusculas.
#' @details El vector \code{n} solo soporta valores mayores o igual a 1 como parte del vector numerico.
#' Ante valores NULL, NA, valores numericos 0 o negativos, la funcion dara un mensaje de error.
#' El vector \code{lets} no soporta valores \code{NA} o NULL entre sus elementos.
#' @examples
#' num2let(1)
#' num2let(c(3, 10, 256, 1000, 2))
#' @export
#'
#'
num2let <- function(n, lets = LETTERS) {

  # Controles internos
  if(is.null(n)) stop("En la funcion num2let() el argumento 'n' no puede ser nulo.")
  if(is.null(lets)) stop("En la funcion num2let() el argumento 'lets' no puede ser nulo.")
  if(!is.vector(n)) stop("En la funcion num2let() el argumento 'n' debe ser un vector.")
  if(!is.vector(lets)) stop("En la funcion num2let() el argumento 'lets' debe ser un vector numerico.")
  if(!is.numeric(n)) stop("En la funcion num2let() el argumento 'n' debe ser un vector numerico.")
  if(!is.numeric(lets)) stop("En la funcion num2let() el argumento 'lets' debe ser un vector numerico.")
  if(sum(is.na(n)) > 0) stop("En la funcion num2let() el argumento 'n' no debe contener valores NA.")
  if(sum(is.na(lets)) > 0) stop("En la funcion num2let() el argumento 'lets' no debe contener valores NA.")
  if(length(n)) stop("En la funcion num2let() el argumento 'n' debe ser un vector numerico con al menos un elemento.")
  if(length(lets)) stop("En la funcion num2let() el argumento 'lets' debe ser un vector con al menos un elemento.")

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


