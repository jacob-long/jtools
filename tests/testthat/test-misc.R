library(jtools)

states <- as.data.frame(state.x77)
states$HSGrad <- states$`HS Grad`
fit <- lm(Income ~ HSGrad*Murder*Illiteracy,data = states)


library(survey)
data(api)
dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
regmodel <- svyglm(api00~ell*meals*both,design=dstrat)

test_that("interact_plot works for lm", {
  expect_silent(interact_plot(model = fit,
                              pred = Murder,
                              modx = Illiteracy,
                              mod2 = HSGrad,
                              centered = "all"))
  expect_silent(interact_plot(model = fit,
                              pred = Murder,
                              modx = Illiteracy,
                              mod2 = HSGrad,
                              centered = "HSGrad"))
})

test_that("interact_plot works for svyglm", {
  expect_silent(interact_plot(regmodel, pred = ell, modx = meals, mod2 = both,
                              centered = "all"))
  expect_silent(interact_plot(regmodel, pred = ell, modx = meals, mod2 = both,
                              centered = "ell"))
})

