test_that("Satterthwaite inference reports the required dependency version", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("lmerTest", "3.2.1")
  m <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  expected <- summary(lmerTest::as_lmerModLmerTest(m))$coefficients
  expect_equal(get_all_sat(m), expected)
})
