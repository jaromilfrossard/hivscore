#' VACS Index 2
#'
#' Compute the VACS Index 2.
#'
# @param time Numeric.
# @param baseline_fun Function of \code{time}.
# @param score Numeric. Vacs Index 2 score. If NULL, it will be computed based on the predictors.
#' @param age Numeric. Year.
#' @param cd4_count Numeric. CD4 cell count. cell/mm\eqn{^3}.
#' @param vl_log Numeric. log 10 of viral load.
#' @param hgb Numeric. Hemoglobin. g/dL.
#' @param fib4 Numeric. Fibrosis-4 Index for Liver Fibrosis.
#' @param egfr Numeric. Estimated glomerular filtration rate. (ml/min/1.73m\eqn{^2}).
#' @param hepc Logical. TRUE for co-infection with hepatitis C virus.
#' @param alb Numeric. Albumin. g/dL.
#' @param bmi Numeric. Body mass index. m/kg\eqn{^2}.
#' @param wbc Numeric. White blood cell. \eqn{10^3} count/µL.
#'
#' @return \code{score_vacsindex2()} returns a numerical vector and \code{points_vacsindex2()} returns a list of the parameters used to compute the VACS Index 2.
#'
#' @references Tate, J. P., Sterne, J. A., Justice, A. C., Study, V. A. C., & Antiretroviral Therapy Cohort Collaboration (ART-CC. (2019). Albumin, white blood cell count, and body mass index improve discrimination of mortality in HIV-positive individuals. AIDS (London, England), 33(5), 903.
#' @references McGinnis, K. A., Justice, A. C., Moore, R. D., Silverberg, M. J., Althoff, K. N., Karris, M., ... & Study, V. A. C. (2021). Discrimination And Calibration Of The Vacs Index 2.0 For Predicting Mortality Among People With Hiv In North America. Clinical Infectious Diseases: an Official Publication of the Infectious Diseases Society of America.
#'
#' @name vacsindex2
#' @examples
#' data("vacs2")
#' vacs2$score_vacs2 <- score_vacsindex2(
#'   age = vacs2$AGE,cd4_count = vacs2$CD4,vl_log = vacs2$VL_LOG,
#'   hgb = vacs2$HGB,fib4 = vacs2$FIB4,egfr = vacs2$EGFR,
#'   hepc = vacs2$HEPC==1,alb = vacs2$ALB,bmi = vacs2$BMI,wbc = vacs2$WBC)
NULL


# #'@rdname vacsindex2
# surv_vacsindex2 <- function(time, baseline_fun, score=NULL,age=NULL, cd4_count=NULL,
#                             vl_log=NULL, hgb=NULL, fib4=NULL, egfr=NULL,
#                             hepc=NULL, alb=NULL, bmi=NULL, wbc=NULL){
#   if(is.null(score)){
#     score <- score_vacsindex2(age=age, cd4_count=cd4_count, vl_log = vl_log,
#                               hgb=hgb, fib4=fib4, egfr=egfr, hepc=hepc,
#                               alb=alb, bmi=bmi,wbc=wbc)
#   }
#   baseline_fun(time)^(exp(score))
# }



#' @export
#' @family vacsindex2
#' @rdname vacsindex2
score_vacsindex2 <- function(age, cd4_count, vl_log, hgb, fib4, egfr, hepc, alb, bmi, wbc){
  validate_age(age)
  validate_cd4_count(cd4_count)
  validate_vl_log(vl_log)
  validate_hgb(hgb)
  validate_fib4(fib4)
  validate_egfr(egfr)
  validate_hepc(hepc)
  validate_alb(alb)
  validate_bmi(bmi)
  validate_bmi(wbc)


  age_c <- (trim_num(age, 30, 75)-50)/5
  cd4_c <- log(1000-trim_num(cd4_count, 10, 1000)+0.1)
  vl_c <- trim_num(vl_log, 1.3, 5)-2
  hgb_c <- trim_num(hgb, 9, 16)-14
  fib4_c <- trim_num(fib4, 0.5, 7.5)

  egfrt <- trim_num(egfr, upperl = 180)
  egfr1 <- egfrt/10
  egfr2 <- ifelse(egfr>35,(egfrt-35)/10,0)
  egfr3 <- ifelse(egfr>65,(egfrt-65)/10,0)
  egfr4 <- ifelse(egfr>115,(egfrt-115)/10,0)

  alb_c <- trim_num(alb, 2, 5) -4
  bmi_c <- trim_num(bmi, 15, 35) -25

  wbc_c <- trim_num(wbc, 2.5, 11)-5.5


  score_age <- age_c*points_vacindex2()$age +
    age_c^2*points_vacindex2()$age_2 + age_c^3*points_vacindex2()$age_3

  score_cd4 <- cd4_c*points_vacindex2()$cd4+
    cd4_c^2*points_vacindex2()$cd4_2+cd4_c^3*points_vacindex2()$cd4_3

  score_vl <- vl_c*points_vacindex2()$vl+
    vl_c^2*points_vacindex2()$vl_2+vl_c^3*points_vacindex2()$vl_3

  score_hgb <- hgb_c*points_vacindex2()$hgb+
    hgb_c^2*points_vacindex2()$hgb_2+hgb_c^3*points_vacindex2()$hgb_3

  score_fib4 <- fib4_c*points_vacindex2()$fib4+fib4_c^2*points_vacindex2()$fib4_2

  score_egfr <- egfr1*points_vacindex2()$egfr+
    egfr2*points_vacindex2()$egfr_2+egfr3*points_vacindex2()$egfr_3+
    egfr4*points_vacindex2()$egfr_4

  score_hepc <- hepc*points_vacindex2()$hepc

  score_alb <- alb_c*points_vacindex2()$alb+
    alb_c^2*points_vacindex2()$alb_2+alb_c^3*points_vacindex2()$alb_3

  score_bmi <- bmi_c*points_vacindex2()$bmi+
    bmi_c^2*points_vacindex2()$bmi_2

  score_wbc <- wbc_c*points_vacindex2()$wbc+
    wbc_c^2*points_vacindex2()$wbc_2+wbc_c^3*points_vacindex2()$wbc_3

  score <- score_age+score_cd4+score_vl+score_hgb+score_fib4+
    score_egfr+score_hepc+score_alb+score_bmi+score_wbc

  score <- (score+3.25)/5.35*100

  return(score)
}




#'@export
#' @family vacsindex2
#'@rdname vacsindex2
points_vacindex2 <- function(){
  list(
    age    =  0.05593,
    age_2  = -0.00447,
    age_3  =  0.00518,
    cd4    = -0.05608,
    cd4_2  = -0.15344,
    cd4_3  =  0.02352,
    vl     =  0.51329,
    vl_2   = -0.42235,
    vl_3   =  0.09798,
    hgb    = -0.13364,
    hgb_2  =  0.02601,
    hgb_3  =  0.00456,
    fib4   =  0.22045,
    fib4_2 = -0.00875,
    egfr   = -0.03075,
    egfr_2 = -0.07668,
    egfr_3 =  0.10629,
    egfr_4 =  0.1325,
    hepc   =  0.34201,
    alb    = -0.44289,
    alb_2  =  0.10394,
    alb_3  =  0.02766,
    bmi    = -0.05452,
    bmi_2  =  0.00359,
    wbc    =  0.1257,
    wbc_2  =  0.01985,
    wbc_3  = -0.00438
  )

}
