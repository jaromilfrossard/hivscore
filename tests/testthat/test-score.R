testthat::expect_equal(
  {data("vacs2")
    vacs2$score_vacs2 <- score_vacsindex2(
      age = vacs2$AGE,cd4_count = vacs2$CD4,vl_log = vacs2$VL_LOG,
      hgb = vacs2$HGB,fib4 = vacs2$FIB4,egfr = vacs2$EGFR,
      hepc = vacs2$HEPC==1,alb = vacs2$ALB,bmi = vacs2$BMI,wbc = vacs2$WBC)
    sum(abs(vacs2$score_vacs2-vacs2$SCORE_V2)<9e-3)},
  nrow(vacs2)
)



testthat::expect_equal(
  {data(dataids)
    dataids$dataids <- score_dataids(
      age = dataids$age, cd4_count = dataids$cd4_count,
      nonhiv_cancer = dataids$nonhiv_cancer, cardio_disease = dataids$cardio_disease,
      egfr = dataids$egfr, cirrhosis = dataids$cirrhosis, low_bmi = dataids$low_bmi,
      anemia = dataids$anemia)
    sum(abs(dataids$dataids- dataids$SCORE_DATAIDS)==0)},
  nrow(dataids)
)


# create dataset
# v0 <- tibble(age=52,cd4=435,log_hiv1_rna=1.7,hgb=14, fib4=1.34,egfr=90,hepc=T,
#             alb=4,wbc=5.5,bmi=25.3)
# vacs1 <- rbind(
#   v0,
#   expand_grid(select(v0,-age),age=c(30,35,40,45,50,55,60,65,70,75)),
#   expand_grid(select(v0,-cd4),cd4=c(10,100,200,300,400,500,600,700,800,900)),
#   expand_grid(select(v0,-log_hiv1_rna),log_hiv1_rna=c(1.3,1.5,1.8,2,2.5,3,3.5,3.5,4,4.5,5)),
#   expand_grid(select(v0,-hgb),hgb=c(9,9.5,10,10.5,11,12,13,14,15,16)),
#   expand_grid(select(v0,-fib4),fib4=c(.5,1,1.45,2,3.25,4,5,6,7,7.5)),
#   expand_grid(select(v0,-egfr),egfr=c(0,20,40,60,80,100,120,140,160,180)),
#   expand_grid(select(v0,-hepc),hepc=c(F,T)),
#   expand_grid(select(v0,-alb),alb=c(2,2.25,2.5,2.75,3,3.25,3.5,4,4.5,5)),
#   expand_grid(select(v0,-wbc),wbc=c(2.5,3,4,5,6,7,8,9,10,11)),
#   expand_grid(select(v0,-bmi),bmi = c(15,17,18,20,22,24,26,28,30,35)))%>%
#   mutate(hiv1_rna= 10^log_hiv1_rna)%>%
#   mutate(across(c(age,cd4,hgb,egfr,alb,bmi,hiv1_rna),as.integer))%>%
#   mutate(vacs1 = score_vacsindex1(age=age,cd4_count = cd4,
#                                   hiv1_rna=hiv1_rna,hgb=hgb,fib4 = fib4,
#                                   egfr=egfr,hepc=hepc),
#          vacs2= score_vacsindex2(
#            age=age,cd4_count = cd4,vl_log = log_hiv1_rna,
#            hgb=hgb,fib4=fib4,egfr=egfr,hepc=hepc,alb = alb,
#            bmi=bmi,wbc=wbc))%>%
#   as.data.frame()
# save(vacs1=vacs1,file="data/vacs1.rda",version = 2)


testthat::expect_equal(
  {data("vacs1")
    vacs1$score_vacs1 <- score_vacsindex1(
      age=vacs1$age,
      cd4_count = vacs1$cd4,
      hiv1_rna=vacs1$hiv1_rna,
      hgb=vacs1$hgb,
      fib4 = vacs1$fib4,
      egfr=vacs1$egfr,
      hepc=vacs1$hepc)
    sum(abs(vacs1$score_vacs1-vacs1$vacs1)<9e-10)},
  nrow(vacs1)
)




