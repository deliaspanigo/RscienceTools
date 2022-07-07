#' @title Redondeo Humano
#' @description La función \code{round2()} es una funcion que realiza la tarea de redondear numeros
#' de la manera en que estamos acostumbrados a realizarlo. Es una opcion diferente al redondeo que ofrece
#' la funcion round() del paquete base de R.
#' @param x es un vector numerico, una matriz numerica o un data.frame numerico.
#' @param decimals es un vector numerico de un unico elemento que indica la cantidad de decimales a utilizar.
#' @details Realiza el redondeo de acuerdo a la cantidad de dígitos y los valores.
#' Si \code{x > 0} y el digito en la posicion decimal a redondear es mayor o igual a 5, redondea hacia
#' el valor positivo siguiente.
#' Si \code{x < 0} y el digito en la posicion decimal a redondear es mayor o igual a 5, redondea hacia
#' Si \code{x < 0}el valor negativo anterior
#' Si Si \code{x == 0}, se redondea a 0.
#' @examples
#' round(0.5)
#' round2(0.5)
#' @export
#'
# Round like a human
round2 <- function(x, decimals = 4) {

  # Controles internos
  # if(is.null(x)) stop("En la funcion round2() el argumento 'x' no puede ser nulo.")
  # if(is.null(decimals)) stop("En la funcion round2() el argumento 'decimals' no puede ser nulo.")
  # if(!is.vector(x)) stop("En la funcion round2() el argumento 'x' debe ser un vector numerico.")
  # if(!is.vector(decimals)) stop("En la funcion round2() el argumento 'lets' debe ser un vector numerico.")
  # if(!is.numeric(n)) stop("En la funcion round2() el argumento 'n' debe ser un vector numerico.")
  # if(!is.numeric(lets)) stop("En la funcion round2() el argumento 'lets' debe ser un vector numerico.")
  # if(sum(is.na(n)) > 0) stop("En la funcion round2() el argumento 'n' no debe contener valores NA.")
  # if(sum(is.na(lets)) > 0) stop("En la funcion round2() el argumento 'lets' no debe contener valores NA.")
  # if(length(n)) stop("En la funcion round2() el argumento 'n' debe ser un vector numerico con al menos un elemento.")
  # if(length(lets)) stop("En la funcion round2() el argumento 'lets' debe ser un vector con al menos un elemento.")

  posneg <- sign(x)

  z <- abs(x)*10^decimals
  z <- z + 0.5
  z <- trunc(z)
  z <- z/10^decimals
  z*posneg
}
