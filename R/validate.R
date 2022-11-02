
validate_age <- function(x){
  stopifnot(is.numeric(x),
            x>0|is.na(x),
            x<140|is.na(x))
}

validate_alb <- function(x){
  stopifnot(is.numeric(x))
}

validate_alt <- function(x){
  stopifnot(is.numeric(x))
}

validate_anemia <- function(x){
  stopifnot(is.logical(x))
}

validate_ast <- function(x){
  stopifnot(is.numeric(x))
}

validate_bmi <- function(x){
  stopifnot(is.numeric(x),
            x>0|is.na(x))
}

validate_cardio_disease <- function(x){
  stopifnot(is.logical(x))
}

validate_cd4_count <- function(x){
  stopifnot(is.numeric(x),
            x>0|is.na(x))
}

validate_cirrhosis <- function(x){
  stopifnot(is.logical(x))
}

validate_ckd_epi <- function(x){
  stopifnot(is.numeric(x),
            x>0|is.na(x))
}

validate_egfr <- function(x){
  stopifnot(is.numeric(x),
            x>=0|is.na(x))
}

validate_fib4 <- function(x){
  stopifnot(is.numeric(x),
            x>0|is.na(x))
}

validate_height <- function(x){
  stopifnot(is.numeric(x),
            x>0|is.na(x),
            x<2.2|is.na(x))
}

validate_hepc <- function(x){
  stopifnot(is.logical(x))
}

validate_hgb <- function(x){
  stopifnot(is.numeric(x),
            x>0|is.na(x))
}

validate_hiv1_rna <- function(x){
  stopifnot(is.numeric(x),
            x>=0L|is.na(x))
}

validate_low_bmi <- function(x){
  stopifnot(is.logical(x))
}

validate_mass <- function(x){
  stopifnot(is.numeric(x),
            x>0|is.na(x),
            x<240|is.na(x))
}

validate_plt <- function(x){
  stopifnot(is.numeric(x))
}

validate_nonhiv_cancer <- function(x){
  stopifnot(is.logical(x))
}

validate_race <- function(x){
  stopifnot(is.character(x),
            x%in%c("other","white","black","hispano-american","asian","unknown")|is.na(x))
}

validate_sex <- function(x){
  stopifnot(is.character(x),
            x%in%c("male","female")|is.na(x))
}

validate_scr <- function(x){
  stopifnot(is.numeric(x))
}

validate_vl_log <- function(x){
  stopifnot(is.numeric(x))
}

validate_wbc <- function(x){
  stopifnot(is.numeric(x))
}















