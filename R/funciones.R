# Define las funciones
es_positivo <- function(x) {
  return(x > 0)
}

leer_numero <- function() {
  num <- as.numeric(readline(prompt = "Ingresa un número: "))
  return(num)
}

cuadrado <- function(x) {
  return(x^2)
}

imprimir_valor <- function(x) {
  cat("El resultado es:", x, "\n")
}


if (es_positivo(numero)) {
  resultado <- cuadrado(numero)
  imprimir_valor(resultado)
} else {
  cat("El número no es positivo.\n")
}