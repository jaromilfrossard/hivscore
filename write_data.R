vacs2<-readr::read_delim("data_vacs2_sas_score.txt",delim="\t",
                  escape_double = FALSE,
                  trim_ws = TRUE,
                  col_types = readr::cols(.default = readr::col_double()), lazy = FALSE)
vacs2<- vacs2|>as.data.frame()
#usethis::use_data(vacs2,vacs2,overwrite = T)


# data("vacs2")
# vacs2$score_vacs2 <- score_vacsindex2(
#   age = vacs2$AGE,cd4_count = vacs2$CD4,vl_log = vacs2$VL_LOG,
#   hgb = vacs2$HGB,fib4 = vacs2$FIB4,egfr = vacs2$EGFR,
#   hepc = vacs2$HEPC==1,alb = vacs2$ALB,bmi = vacs2$BMI,wbc = vacs2$WBC
#   )

dataids<-readr::read_delim("data_dataids.csv",delim=";",
                         escape_double = FALSE,
                         trim_ws = TRUE,
                         col_types = readr::cols(.default = readr::col_double(),
                                                 nonhiv_cancer = readr::col_logical(),
                                                 cardio_disease = readr::col_logical(),
                                                 cirrhosis = readr::col_logical(),
                                                 low_bmi = readr::col_logical(),
                                                 anemia = readr::col_logical()), lazy = FALSE)
dataids <- dataids|>
  dplyr::select(age:anemia,SCORE_DATAIDS = scoretotal)|>
  ##correction ANEMIA
  dplyr::mutate(SCORE_DATAIDS = ifelse(anemia==TRUE,SCORE_DATAIDS+6,SCORE_DATAIDS-6))|>
  as.data.frame()
#usethis::use_data(dataids,dataids,overwrite = T)


# dataids$dataids <- score_dataids(age= dataids$age,cd4_count = dataids$cd4_count,
#                                  nonhiv_cancer = dataids$nonhiv_cancer,
#                                  cardio_disease = dataids$cardio_disease,egfr = dataids$egfr,
#                                  cirrhosis = dataids$cirrhosis,
#                                  low_bmi = dataids$low_bmi,anemia = dataids$anemia)


