#' Discretization of the numeric variables used in the DAT'aids and in the VACS Index 1
#'
#' @param x Numeric variable to discretize.
#'
#' @return Factor.
#' @name discretize
#' @examples
#' data(dataids)
#' dataids_d<-
#'   data.frame(
#'     age = dataids$age,
#'     age_d = discrete_age_dataids(dataids$age),
#'     cd4 = dataids$cd4_count,
#'     cd4_d = discrete_cd4_dataids(dataids$cd4_count),
#'     egfr = dataids$egfr,
#'     egfr_d = discrete_egfr_dataids(dataids$egfr))
#' head(dataids_d)
#'
#' data(vacs2)
#' vacs2_d <-
#'   data.frame(
#'     age = vacs2$AGE,
#'     age_d = discrete_age_vacs1(vacs2$AGE),
#'     cd4 = vacs2$CD4,
#'     cd4_d = discrete_cd4_vacs1(vacs2$CD4),
#'     vl = 10^vacs2$VL_LOG,
#'     vl_d = discrete_hiv1rna_vacs1(10^vacs2$VL_LOG),
#'     egfr = vacs2$EGFR,
#'     egfr_d = discrete_egfr_vacs1(vacs2$EGFR),
#'     fib4 = vacs2$EGFR,
#'     fib4_d = discrete_fib4_vacs1(vacs2$FIB4),
#'     hgb = vacs2$HGB,
#'     hgb_d = discrete_fib4_vacs1(vacs2$HGB))
#' head(vacs2_d)
NULL


#' @export
#' @family dataids
#' @rdname discretize
discrete_age_dataids <- function(x){
  cut(x,
      breaks = c(0L,60L,65L,75L,.Machine$integer.max),
      labels = c("<60","60-64","65-74",">=75"),
      right = FALSE,ordered_result=T)
}

#' @export
#' @family vacsindex1
#' @rdname discretize
discrete_age_vacs1 <- function(x){
  cut(x,
      breaks = c(0L,50L,65L,.Machine$integer.max),
      labels = c("<50","60-64",">=65"),
      right = FALSE,ordered_result=T)
}

#' @export
#' @family dataids
#' @rdname discretize
discrete_cd4_dataids <- function(x){
  cut(x,
      breaks = c(0L,200L,350L,500L,.Machine$integer.max),
      labels = c("<200","200-350","350-500",">=500"),
      right = FALSE,ordered_result=T)
}

#' @export
#' @family vacsindex1
#' @rdname discretize
discrete_cd4_vacs1 <- function(x){
  cut(x,
      breaks = c(0L,50L,100L,200L,350L,500L,.Machine$integer.max),
      labels = c("<50","50-99","100-199","200-349","350-499",">=500"),
      right = FALSE,ordered_result=T)
}

#' @export
#' @family dataids
#' @rdname discretize
discrete_egfr_dataids <- function(x){
  cut(x,
      breaks = c(0L,30L,60L,.Machine$integer.max),
      labels = c("<30","30-59",">=60"),
      right = FALSE,ordered_result=T)
}

#' @export
#' @family vacsindex1
#' @rdname discretize
discrete_egfr_vacs1 <- function(x){
  cut(x,
      breaks = c(0L,30L,45L,60L,.Machine$integer.max),
      labels = c("<30","30-44","45-59",">=60"),
      right = FALSE,ordered_result=T)
}

#' @export
#' @family vacsindex1
#' @rdname discretize
discrete_fib4_vacs1 <- function(x){
  cut(x,
      breaks = c(0,1.45,3.25,Inf),
      labels = c("<1.45","1.45-3.24",">=3.25"),
      right = FALSE,ordered_result=T)
}

#' @export
#' @family vacsindex1
#' @rdname discretize
discrete_hgb_vacs1 <- function(x){
  cut(x,
      breaks = c(0L,10L,12L,14L,Inf),
      labels = c("<10","10-11.9","12-13.9",">=14"),
      right = FALSE,ordered_result=T)
}

#' @export
#' @family vacsindex1
#' @rdname discretize
discrete_hiv1rna_vacs1 <- function(x){
  cut(x,
      breaks = c(0L,500L,100000L,.Machine$integer.max),
      labels = c("<500","500-1e5",">=1e5"),
      right = FALSE,ordered_result=T)
}




