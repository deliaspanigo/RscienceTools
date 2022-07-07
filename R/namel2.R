#' @title Crear una lista con nombres predeterminados
#' @description La idea de la funcion \code{namel2()} es facilitar la creacion de listas, asignando un nombre a
#' cada elemento de la lista y un contenido inicial.
#' Si solo se detalla el vector \code{names} el contenido de las listas por defecto sera NA.
#' @param names es un vector que detalla los nombres a asignar a la lista.
#' @param vec es un vector que detalla el contenido minimo que tendra cada lista.
#' Puede ser un vector de un solo elemento o un vector con un elemento para cada lista.
#' El valor por defecto es NA.
#' @details Internamente, genera una lista con los elementos del vector \code{vec}, y le asigna a la lista
#' generada los nombres del vector \code{names}. El parametro \code{vec} por defecto es NA.
#' Si se asigna un vector a \code{vec} este vector debe contener un elemento unico o tener
#' la misma cantidad de elementos que \code{names}; en cualquier otor caso la funcion devuelve un aviso de error.
#' @examples
#' namel2(names = colnames(mtcars))
#' namel2(names = colnames(mtcars), vec = colnames(mtcars))
#' @export
#'
#helper function (convert vector to named list)
namel2 <- function (names, vec = NA){

  if(is.null(names)) stop("En la funcion namel2() el argumento 'names' no puede ser nulo.")
  if(is.null(vec)) stop("En la funcion namel2() el argumento 'vec' no puede ser nulo.")
  if(!is.vector(names)) stop("En la funcion namel2() el argumento 'names' debe ser un vector.")
  if(!is.vector(vec)) stop("En la funcion namel2() el argumento 'vec' debe ser un vector.")


  # Si no tienen el mismo tamanio y la cantidad de elementos de 'vec' es diferente de uno...
  # algo esta mal.
  if(length(names) != length(vec)) if(length(vec) != 1) {
        stop("En la funcion namel2() el argumento 'vec' debe contener un solo elemento o ser de la misma
  longitud que el argumento 'names'.")
    }
  # Si todo esta OK
  if(length(vec) == 1) vec <- rep(vec[1], length(names))

  # Creamos la lista y la nombramos
  tmp <- as.list(vec)
  names(tmp) <- names

  # Return Exitoso
  return(tmp)
}


