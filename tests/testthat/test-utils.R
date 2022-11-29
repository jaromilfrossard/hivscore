testthat::expect_equal(
  egfr(scr = 100,sex = "female",age = 20,race = "black"),
  80.927218)

testthat::expect_equal(
  egfr_norace(scr = 100,sex = "female",age = 20),
  70.201924)

testthat::expect_equal(
  bmi(79, 1.8),
  79/1.8^2)

testthat::expect_equal(
  fib4(40,10,20,200),
  1.26
)

testthat::expect_equal(
  anemia(c(12.5,13.5,11.5,13.5),c("male","male","female","female")),
  c(T,F,T,F)
)

testthat::expect_equal(
  low_bmi(c(18,19)),
  c(T,F)
)

testthat::expect_equal(
  vl_log(10),
  log10(10)
  )
