#Expected survival at 2 years for a score of 10 at dataids
testthat::expect_equal(
  surv_dataids(2,score = 10),
  0.96330254366503764
)

#Expected survival at 5 years for a score of 10 at dataids
testthat::expect_equal(
  surv_dataids(5,score = 10),
  0.88771842781478726
)







# tribble(
#   ~Param,~Set,~AGE,~CD4,~VL,~HGB,~FIB4,~EGFR,~Hepc,~ALB,~WBC,~BMI,
#        0,  50,  52,	435,1.7,	14,	1.34,	  90,	   F,	  4, 5.5,	25.3)%>%
#   mutate(vacs1 = pmap_dbl(list(AGE,CD4,VL,HGB,FIB4,EGFR,Hepc),score_vacsindex1))
#
#
# score_vacsindex1(52,	435,1.7,	14,	1.34,	  90,	   F)
