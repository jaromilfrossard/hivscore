#' Compute the VACS Index 1
#'
# @param time Numeric. Time of prediction in year.
# @param baseline_fun Function of \code{time}.
# @param score Numeric. VACS index 1 score. If NULL, it will be computed based on the predictors.
#' @param age Numeric. Age in years.
#' @param cd4_count Numeric. CD4 cell count. cell/mm\eqn{^3}.
#' @param hiv1_rna Numeric. Viral load. copies/mL.
#' @param hgb Numeric. Hemoglobin. g/dL.
#' @param fib4 Numeric. Fibrosis-4 Index for Liver Fibrosis.
#' @param egfr Numeric. Estimated glomerular filtration rate. (ml/min/1.73m\eqn{^2}).
#' @param hepc Logical. TRUE for co-infection with hepatitis C virus.
#'
#' @return A numerical vector.
#'
#' @references Tate, J. P., Justice, A. C., Hughes, M. D., Bonnet, F., Reiss, P., Mocroft, A., ... & Sterne, J. A. (2013). An internationally generalizable risk index for mortality after one year of antiretroviral therapy. AIDS (London, England), 27(4), 563.
#' @references Bebu, I., Tate, J., Rimland, D., Mesner, O., Macalino, G. E., Ganesan, A., ... & Infectious Disease Clinical Research Program HIV Working Group. (2014). The VACS index predicts mortality in a young, healthy HIV population starting highly active antiretroviral therapy. Journal of acquired immune deficiency syndromes (1999), 65(2), 226.
#' @references Justice, A. C., Modur, S., Tate, J. P., Althoff, K. N., Jacobson, L. P., Gebo, K., ... & Gange, S. J. (2013). Predictive accuracy of the Veterans Aging Cohort Study (VACS) index for mortality with HIV infection: a north American cross cohort analysis. Journal of acquired immune deficiency syndromes (1999), 62(2), 149.
#'
#' @name vacsindex1
#' @examples
#' data("vacs1")
#' vacs1$score_vacs1 <- score_vacsindex1(
#'     age = vacs1$age, cd4_count = vacs1$cd4, hiv1_rna = vacs1$hiv1_rna,
#'     hgb = vacs1$hgb, fib4 = vacs1$fib4, egfr = vacs1$egfr,
#'     hepc = vacs1$hepc)
NULL

# #' @rdname vacsindex1
# surv_vacsindex1 <- function(time, baseline_fun, score=NULL,age=NULL, cd4_count=NULL,
#                             hiv1_rna=NULL, hgb=NULL, fib4=NULL, egfr=NULL,
#                             hepc=NULL){
#   if(is.null(score)){
#     score <- score_vacsindex1(age=age, cd4_count=cd4_count, hiv1_rna = hiv1_rna,
#                               hgb=hgb, fib4=fib4, egfr=egfr, hepc=hepc)
#     }
#   baseline_fun(time)^(exp(score))
#   }

#' @export
#' @rdname vacsindex1
#' @importFrom dplyr case_when
score_vacsindex1 <- function(age, cd4_count, hiv1_rna, hgb, fib4, egfr, hepc){
  validate_age(age)
  validate_cd4_count(cd4_count)
  validate_hiv1_rna(hiv1_rna)
  validate_hgb(hgb)
  validate_fib4(fib4)
  validate_egfr(egfr)
  validate_hepc(hepc)


  score_age <-
    case_when(
      age<0~NA_integer_,
      age<50~points_vacsindex1()$age_0_50,
      age<65~points_vacsindex1()$age_50_64,
      age>=65~points_vacsindex1()$age_65)

  score_cd4_count <-
    case_when(
      cd4_count>=500~points_vacsindex1()$cd4_count_500,
      cd4_count>=350~points_vacsindex1()$cd4_count_350_499,
      cd4_count>=200~points_vacsindex1()$cd4_count_200_349,
      cd4_count>=100~points_vacsindex1()$cd4_count_100_199,
      cd4_count>=50~points_vacsindex1()$cd4_count_50_99,
      cd4_count>=0~points_vacsindex1()$cd4_count_50,
      cd4_count<0~NA_integer_)

  score_hvi1_rna <-
    case_when(
      hiv1_rna<0~NA_integer_,
      hiv1_rna<=500~points_vacsindex1()$hiv1_rna_500,
      hiv1_rna<=1e5~points_vacsindex1()$hiv1_rna_500_1e5,
      hiv1_rna>1e5~points_vacsindex1()$hiv1_rna_1e5)

  score_hgb <-
    case_when(
      hgb>=14~points_vacsindex1()$hgb_14,
      hgb>=12~points_vacsindex1()$hgb_12_13.9,
      hgb>=10~points_vacsindex1()$hgb_10_11.9,
      hgb>=0~points_vacsindex1()$hgb_10,
      hgb<0~NA_integer_)

  score_fib4 <-
    case_when(
      fib4<0~NA_integer_,
      fib4<1.45~points_vacsindex1()$fib4_1.45,
      fib4<=3.25~points_vacsindex1()$fib4_1.45_3.25,
      fib4>3.25~points_vacsindex1()$fib4_3.25)


  score_egfr <- case_when(
    egfr>=60~points_vacsindex1()$egfr_60,
    egfr>=45~points_vacsindex1()$egfr_45_59.9,
    egfr>=30~points_vacsindex1()$egfr_30_44.9,
    egfr>=0~points_vacsindex1()$egfr_30,
    egfr<0~NA_integer_)

  score_hepc <- hepc*points_vacsindex1()$hepc

  score <- score_age+score_cd4_count+score_hvi1_rna+score_hgb+
    score_fib4+score_egfr+score_hepc
  return(score)


}


#' @export
#' @rdname vacsindex1
points_vacsindex1 <- function(){
  list(
    age_0_50 = 0L,
    age_50_64 = 12L,
    age_65 = 27L,
    cd4_count_500 = 0L,
    cd4_count_350_499 = 6L,
    cd4_count_200_349 = 6L,
    cd4_count_100_199 = 10L,
    cd4_count_50_99 = 28L,
    cd4_count_50 = 29L,
    hiv1_rna_500 = 0L,
    hiv1_rna_500_1e5 = 7L,
    hiv1_rna_1e5 = 14L,
    hgb_14 = 0L,
    hgb_12_13.9 = 10L,
    hgb_10_11.9 = 22L,
    hgb_10 = 38L,
    fib4_1.45 = 0L,
    fib4_1.45_3.25 = 6L,
    fib4_3.25 = 25L,
    egfr_60 = 0L,
    egfr_45_59.9 = 6L,
    egfr_30_44.9 = 8L,
    egfr_30 = 26L,
    hepc = 5L
  )

}

