#' Conversion of units.
#'
#' @param x Numeric.
#'
#' @return A numerical vector.
#' @details
#' \code{conv_g.ml_to_g.dl()}: g/ml to g/dl. \eqn{1/100}.
#'
#' \code{conv_g.dl_to_g.ml()}: g/dl to g/ml. x\eqn{100}.
#'
#' \code{conv_mul_to_mm3()}: \eqn{\mu}l to mm\eqn{^3}. x\eqn{1}.
#'
#' \code{conv_mm3_to_mul()}: mm\eqn{^3} to \eqn{\mu}l. x\eqn{1}.
#'
#' \code{conv_K.ml_to_K.mm3()}. count per ml to count per mm\eqn{^3}. \eqn{1/1000}.
#'
#' \code{conv_K.mm3_to_K.ml()}. count per mm\eqn{^3} to count per mL. x\eqn{1000}.
#'
#' \code{conv_cm_to_m()}. cm to m. \eqn{1/100}.
#'
#' \code{conv_cm_to_m()}. m to cm. x\eqn{100}.
#'
#' \code{conv_mumol.l_to_mg.dl()}. \eqn{\mu} mod per l to mg per dl. x \eqn{0.0113096584483149}
#'
#' \code{conv_mg.dl_to_mumol.l()}. mg per dl to \eqn{\mu} mod per l. \eqn{1/0.0113096584483149}
#'
#' @name convert
NULL


#'@export
#'@family convert
#'@rdname convert
conv_g.ml_to_g.dl <- function(x){
  x/100
}

#'@export
#'@family convert
#'@rdname convert
conv_g.dl_to_g.ml <- function(x){
  x*100
}

#'@export
#'@family convert
#'@rdname convert
conv_mul_to_mm3 <- function(x){
  x
}

#'@export
#'@family convert
#'@rdname convert
conv_mm3_to_mul <- function(x){
  x
}



#'@export
#'@family convert
#'@rdname convert
conv_K.ml_to_K.mm3 <- function(x){
  x/1000L
}

#'@export
#'@family convert
#'@rdname convert
conv_K.mm3_to_K.ml <- function(x){
  x*1000L
}

#'@export
#'@family convert
#'@rdname convert
conv_cm_to_m <- function(x){
  x/100L
}

#'@export
#'@family convert
#'@rdname convert
conv_m_to_cm <- function(x){
  x*100L
}


#'@export
#'@family convert
#'@rdname convert
conv_mumol.l_to_mg.dl <- function(x){
  x*0.0113096584483149
}

#'@export
#'@family convert
#'@rdname convert
conv_mg.dl_to_mumol.l <- function(x){
  x/0.0113096584483149
}

