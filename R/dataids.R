#' Compute the DAT'aids survival prediction
#'
#' @param time Numeric. Time of prediction in year.
#' @param basesurv_fun Function of \code{time}. Default is \code{basesurv_hentzien2018}.
#' @param beta_score Numberic. Default is \code{0.1008354015} for continuous score and \code{c("0-3"=0, "4-13"=0.81868, "14-19"=1.76034, ">=20"=2.61992)} for risk group. Parameter associated to the score.
#' @param eta_fun Function of the score which returns the linear predictor. Default is \code{eta_dataids} and \code{eta_riskgroupdataids} is available.
#' @param score Numeric. DAT'aids score. If NULL, it will be computed based on the covariates.
#' @param age Numeric. Age in year.
#' @param cd4_count Numeric. Count of cd4 cell/mm3.
#' @param nonhiv_cancer Logical. TRUE for diagnosis of non-hiv cancer.
#' @param cardio_disease Logical. TRUE for diagnosis of cardio vascular disease.
#' @param egfr Numeric. Estimated glomerular filtration rate. (ml/mn/1.73m2).
#' @param cirrhosis Logical. TRUE for cirrhosis diagnostic.
#' @param low_bmi Logical. TRUE if BMI < 18.5.
#' @param anemia Logical. TRUE for anemia defined as hemoglobin <12g/dL for female and <13g/dL for male.
#'
#' @details The prediction of survival at time t is:
#' \deqn{\hat{S}(t) = S_0(t)^{\exp{b*\text{score}}}},
#' where \eqn{S_0(t)} is \code{basesurv_fun} and \eqn{b} is \code{beta_score}.
#'
#' @references Hentzien M, Delpierre C, Pugliese P, Allavena C, Jacomet C, Valantin M-A, et al. (2018) Derivation and internal validation of a mortality risk index for aged people living with HIV: The Dat'AIDS score. PLoS ONE 13(4): e0195725. https://doi.org/10.1371/journal.pone.0195725
#'
#' @return Numeric. Predicted survival probability.
#' @name dataids_score
#' @examples
#' data(dataids)
#' dataids$dataids <- score_dataids(
#'     age = dataids$age, cd4_count = dataids$cd4_count,
#'     nonhiv_cancer = dataids$nonhiv_cancer, cardio_disease = dataids$cardio_disease,
#'     egfr = dataids$egfr, cirrhosis = dataids$cirrhosis, low_bmi = dataids$low_bmi,
#'     anemia = dataids$anemia)
NULL


#' @export
#' @rdname dataids_score
surv_dataids <- function(time,score = NULL,
                         basesurv_fun = basesurv_hentzien2018,
                         eta_fun = eta_dataids,
                         age = NULL, cd4_count = NULL,
                         nonhiv_cancer = NULL, cardio_disease = NULL,
                         egfr = NULL, cirrhosis = NULL,
                         low_bmi = NULL, anemia = NULL){

  if(is.null(score)){
    score<-score_dataids(
      age = age,cd4_count = cd4_count,nonhiv_cancer = nonhiv_cancer,
      cardio_disease = cardio_disease,egfr = egfr, cirrhosis = cirrhosis,
      low_bmi=low_bmi, anemia = anemia)
    }
  basesurv_fun(time)^exp(eta_fun(score=score))
}


#' @export
#' @rdname dataids_score
eta_dataids <- function(score = NULL,
                         beta_score = 0.1008354015,
                         age = NULL, cd4_count = NULL,
                         nonhiv_cancer = NULL, cardio_disease = NULL,
                         egfr = NULL, cirrhosis = NULL,
                         low_bmi = NULL, anemia = NULL){
  if(is.null(score)){
    score<-score_dataids(
      age = age,cd4_count = cd4_count,nonhiv_cancer = nonhiv_cancer,
      cardio_disease = cardio_disease,egfr = egfr, cirrhosis = cirrhosis,
      low_bmi=low_bmi, anemia = anemia)
  }

  score*beta_score
}


#' @export
#' @rdname dataids_score
eta_riskgroupdataids <- function(score = NULL,
                        beta_score = c(
                          "0-3" = 0, "4-13"  = 0.81868,
                          "14-19" = 1.76034, ">=20" = 2.61992),
                        age = NULL, cd4_count = NULL,
                        nonhiv_cancer = NULL, cardio_disease = NULL,
                        egfr = NULL, cirrhosis = NULL,
                        low_bmi = NULL, anemia = NULL){
  if(is.null(score)){
    score<-score_dataids(
      age = age,cd4_count = cd4_count,nonhiv_cancer = nonhiv_cancer,
      cardio_disease = cardio_disease,egfr = egfr, cirrhosis = cirrhosis,
      low_bmi=low_bmi, anemia = anemia)
  }


  beta_score[as.integer(riskgroup_dataids(score))]

}




#' @export
#' @importFrom dplyr case_when
#' @rdname dataids_score
score_dataids <- function(age, cd4_count, nonhiv_cancer, cardio_disease,
                          egfr,cirrhosis, low_bmi, anemia){
  validate_age(age)
  validate_cd4_count(cd4_count)
  validate_nonhiv_cancer(nonhiv_cancer)
  validate_cardio_disease(cardio_disease)
  validate_egfr(egfr)
  validate_cirrhosis(cirrhosis)
  validate_low_bmi(low_bmi)
  validate_anemia(anemia)

  score_age <-
    case_when(
      age<60~NA_integer_,
      age<65~points_dataids()$age_60_64,
      age<75~points_dataids()$age_65_74,
      age>=75~points_dataids()$age_75)

  score_cd4_count <-
    case_when(
      cd4_count>=500~points_dataids()$cd4_500,
      cd4_count>=350~points_dataids()$cd4_350_500,
      cd4_count>=200~points_dataids()$cd4_200_349,
      cd4_count>=0~points_dataids()$cd4_200,
      cd4_count<0~NA_integer_)

  score_egfr <-
    case_when(
      egfr>=60~points_dataids()$egfr_60,
      egfr>=30~points_dataids()$egfr_30_59,
      egfr>=0~points_dataids()$egfr_30,
      egfr<0~NA_integer_)

  score_non_hiv_cancer <- nonhiv_cancer*points_dataids()$nonhiv_cancer
  score_non_cardio_disease <- cardio_disease*points_dataids()$cardio_disease
  score_cirrhosis <- cirrhosis*points_dataids()$cirrhosis
  score_low_bmi <- low_bmi*points_dataids()$low_bmi
  score_anemia <- anemia*points_dataids()$anemia

  score <- score_age+score_cd4_count+score_egfr+
    score_non_hiv_cancer+
    score_non_cardio_disease+
    score_cirrhosis+
    score_low_bmi+
    score_anemia


  return(score)

}


#' @export
#' @rdname dataids_score
riskgroup_dataids <- function(score){
  cut(score,
      breaks = c(0L,4L,14L,20L,.Machine$integer.max),
      labels = c("0-3","4-13","14-19",">=20"),
      right = FALSE,ordered_result=T)
}



#' @export
#' @rdname dataids_score
points_dataids <- function(){
  list(
    age_60_64 = 0L,
    age_65_74 = 1L,
    age_75 = 8L,

    cd4_500 = 0L,
    cd4_350_500 = 0L,
    cd4_200_349 = 3L,
    cd4_200 = 6L,

    nonhiv_cancer = 6L,

    cardio_disease = 8L,

    egfr_60 = 0L,
    egfr_30_59 = 5L,
    egfr_30 = 16L,

    cirrhosis = 13L,

    low_bmi = 10L,
    anemia = 6L)
}




