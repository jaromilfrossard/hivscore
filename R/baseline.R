#' Baseline Function
#'
#' @param time Numeric. Time in year.
#'
#' @details \code{basesurv_hentzien2018} is given by Hentzien (2018):
#' \deqn{S_0(t) = 1-0.00562t - 0.00057679t^2}
#'
#' \code{basesurv_riskgroup_hentzien2018} is given by Hentzien (2018):
#' \deqn{S_0(t) = 1-0.00610t - 0.00050468t^2}
#'
#' @references Hentzien M, Delpierre C, Pugliese P, Allavena C, Jacomet C, Valantin M-A, et al. (2018) Derivation and internal validation of a mortality risk index for aged people living with HIV: The Dat'AIDS score. PLoS ONE 13(4): e0195725. https://doi.org/10.1371/journal.pone.0195725
#'
#' @name baseline



#' @export
#' @family dataids
#' @rdname baseline
basesurv_hentzien2018 <- function(time){
  1-0.00562*time - 0.00057679*time^2
}

#' @export
#' @family dataids
#' @rdname baseline
basesurv_riskgroup_hentzien2018 <- function(time){
  1-0.00610*time - 0.00050468*time^2
}
