#' Trim numerical vector
#'
#' @param x Numeric, vector.
#' @param lowerl Numeric, scalar. Lower limit.
#' @param upperl Numeric, scalar, Upper limit.
#'
#' @return a numerical vector
#' @importFrom dplyr case_when
#' @family utils
trim_num <- function(x, lowerl = NULL, upperl = NULL){
  if(!is.null(lowerl)){
    x <- case_when(
      is.na(x)~NA_real_,
      x<lowerl~as.numeric(lowerl),
      TRUE~as.numeric(x))
  }
  if(!is.null(upperl)){
    x <- case_when(
      is.na(x)~NA_real_,
      x>upperl~as.numeric(upperl),
      TRUE~as.numeric(x))
  }
  return(x)

}



#' Estimated glomerular filtration rate formula
#'
#' @param scr Numeric. Serum creatinine \eqn{\mu} mol/L
#' @param age Numeric. Year.
#' @param sex Character. Either \code{"male"} or \code{"female"}.
#' @param race Character. Either \code{"other"},\code{"white"}, \code{"black"}, \code{"hispano-american"}, \code{"asian"}, \code{"unknown"}. Specifying \code{race = "black"} change the formula of eGFR.
#'
#' @details See references for the formula.
#'
#' @references Levey, A. S., Stevens, L. A., Schmid, C. H., Zhang, Y., Castro III, A. F., Feldman, H. I., ... & CKD-EPI (Chronic Kidney Disease Epidemiology Collaboration)*. (2009). A new equation to estimate glomerular filtration rate. Annals of internal medicine, 150(9), 604-612.
#'
#' @return A numeric
#' @importFrom dplyr case_when
#' @name egfr
#' @family utils



#' @export
#' @rdname egfr
egfr <- function(scr, age, sex, race){
  validate_scr(scr)
  validate_age(age)
  validate_sex(sex)
  validate_race(race)

  # k <- ifelse(sex=="male",0.9,0.7)
  # alpha <- ifelse(sex=="male",-0.411,-0.329)
  # mult_sex <- ifelse(sex=="female",1.018,1)
  # mult_race <- ifelse(race=="black",1.159,1)
  # 141*pmin(scr/k,1)^alpha*pmax(scr/k,1)^(-1.209)*0.993^age*mult_sex*mult_race


  ## For scr in mu-mol/l
  # case_when(race=="black"&sex=="female"&scr <= 62 ~ 166*(scr/0.7)^(-0.329)*0.993^age,
  #           race=="black"&sex=="female"&scr > 62  ~ 166*(scr/0.7)^(-1.209)*0.993^age,
  #           race=="black"&sex=="male"&scr <= 80   ~ 163*(scr/0.9)^(-0.411)*0.993^age,
  #           race=="black"&sex=="male"&scr > 80    ~ 163*(scr/0.9)^(-1.209)*0.993^age,
  #           race!="black"&sex=="female"&scr <= 62 ~ 144*(scr/0.7)^(-0.329)*0.993^age,
  #           race!="black"&sex=="female"&scr > 62  ~ 144*(scr/0.7)^(-1.209)*0.993^age,
  #           race!="black"&sex=="male"&scr <= 80   ~ 141*(scr/0.9)^(-0.411)*0.993^age,
  #           race!="black"&sex=="male"&scr > 80    ~ 141*(scr/0.9)^(-1.209)*0.993^age)

  case_when(race=="black"&sex=="female"&scr <= 62 ~ 166*(scr/62)^(-0.329)*0.993^age,
            race=="black"&sex=="female"&scr > 62  ~ 166*(scr/62)^(-1.209)*0.993^age,
            race=="black"&sex=="male"  &scr <= 80 ~ 163*(scr/80)^(-0.411)*0.993^age,
            race=="black"&sex=="male"  &scr > 80  ~ 163*(scr/80)^(-1.209)*0.993^age,
            race!="black"&sex=="female"&scr <= 62 ~ 144*(scr/62)^(-0.329)*0.993^age,
            race!="black"&sex=="female"&scr > 62  ~ 144*(scr/62)^(-1.209)*0.993^age,
            race!="black"&sex=="male"  &scr <= 80 ~ 141*(scr/80)^(-0.411)*0.993^age,
            race!="black"&sex=="male"  &scr > 80  ~ 141*(scr/80)^(-1.209)*0.993^age)



}



#' @export
#' @rdname egfr
egfr_norace <- function(scr, age, sex){
  validate_scr(scr)
  validate_age(age)
  validate_sex(sex)

  egfr(scr,age,sex,"white")

}


#' BMI formula
#'
#' @param mass Numeric. Mass in Kg.
#' @param height Numeric. Height in m.
#'
#' @return numeric.
#' @export
#' @family utils
bmi <- function(mass,height){
  validate_mass(mass)
  validate_height(height)
  mass/height^2
}


#' Fibrosis-4 Index for Liver Fibrosis formula
#'
#' @param age Numeric. Years.
#' @param alt Numeric. Alanine aminotransferase. U/L.
#' @param ast Numeric. Aspartate aminotransferase. U/L.
#' @param plt Numeric. Platelet count. \eqn{\times} 10^9/L.
#'
#' @return The FIB-4 index.
#' @export
#' @family utils
fib4 <- function(age, alt, ast , plt){
  validate_age(age)
  validate_alt(alt)
  validate_ast(ast)
  validate_plt(plt)

  round(age*ast/(plt*sqrt(alt)),2)
}

#' Anemia
#'
#' @param hgb Numeric. Hemoglobine. g/dL.
#' @param sex Character.
#'
#' @return Logical. TRUE if hgb < 13 g/dL. For female, TRUE if hgb < 12 g/dL.
#' @export
#' @family utils
anemia <- function(hgb,sex){
  validate_hgb(hgb)
  validate_sex(sex)
  thr <- ifelse(sex=="male",13,12)
  hgb<thr
}



#' Low bmi
#'
#' @param bmi Numeric. Body Mass Index.
#'
#' @return Logical. TRUE if bmi < 18.5, FALSE otherwise.
#' @export
#' @family utils
low_bmi <- function(bmi){
  validate_bmi(bmi)
  bmi<18.5
}


#' Log 10 of the viral load
#'
#' @param hiv1_rna Numeric. Viral Load. count/ml.
#'
#' @return Numeric. The log 10 of the hiv1_rna.
#' @export
#' @family utils
vl_log <- function(hiv1_rna){
  validate_hiv1_rna(hiv1_rna)
  log10(hiv1_rna)
}
