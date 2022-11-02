#' Testing data for the VACS Index 1 and VACS Index 2
#'
#' A simulated dataset
#'
#' @format A data frame with 84 rows and 13 variables:
#' \describe{
#'   \item{age}{Integer. Years.}
#'   \item{cd4}{Integer. CD4 count. K/mm^3.}
#'   \item{log_hiv1_rna}{Numeric. HIV1 RNA. log K/ml.}
#'   \item{hgb}{Integer. Hemoglobin. g/dL.}
#'   \item{fib4}{Numeric. FIB-4.}
#'   \item{egfr}{Integer. eGFR.}
#'   \item{hepc}{Logical. Hepatitis C co-infection.}
#'   \item{alb}{Integer. Albumin. g/dL.}
#'   \item{wbc}{Numeric. White blood cell count. K/mL.}
#'   \item{bmi}{Numeric. Body mass Index. kg/m^2}
#'   \item{hiv1_rna}{Numeric. HIV1 RNA. K/ml.}
#'   \item{vacs1}{Integer. Score Vacs Index 1.}
#'   \item{vacs2}{Numeric. Score Vacs Index 2.}
#' }
"vacs1"


#' Testing data for the VACS Index 2
#'
#' A dataset containing the testing data in the supplementary material of doi: 10.1093/cid/ciab883
#'
#' @format A data frame with 94 rows and 13 variables:
#' \describe{
#'   \item{PARAM}{Numeric.}
#'   \item{SET}{Numeric.}
#'   \item{AGE}{Numeric. Age.}
#'   \item{CD4}{Numeric. CD4 count.}
#'   \item{VL_LOG}{Numeric. Log of viral load.}
#'   \item{HGB}{Numeric. Hemoglobin.}
#'   \item{FIB4}{Numeric. FIB-4.}
#'   \item{EGFR}{Numeric. eGFR.}
#'   \item{HEPC}{Numeric. Hepatitis C.}
#'   \item{ALB}{Numeric. Albumine.}
#'   \item{WBC}{Numeric. White Blood Cell.}
#'   \item{BMI}{Numeric. BMI.}
#'   \item{SCORE_V2}{Vacs Index 2.}
#' }
#' @source \url{https://pubmed.ncbi.nlm.nih.gov/34609485/}
"vacs2"




#' Test data the DAT'aids score
#'
#' A dataset for testing the DAT'aids score
#'
#' @format A data frame with 1152 rows and 9 variables:
#' \describe{
#'   \item{age}{Numeric. Age.}
#'   \item{cd4_count}{Numeric. CD4 count.}
#'   \item{nonhiv_cancer}{Logical. History of non-hiv cancer.}
#'   \item{cardio_disease}{Logical. History of cardiac disease.}
#'   \item{egfr}{Numeric. eGFR.}
#'   \item{cirrhosis}{Logical. History of cirrhosis.}
#'   \item{low_bmi}{Logical. Low BMI.}
#'   \item{anemia}{Logical. Anemia.}
#'   \item{SCORE_DATAIDS}{Numeric. Score Dat'Aids}
#' }
#' @source \url{https://doi.org/10.1371/journal.pone.0195725/}
"dataids"
