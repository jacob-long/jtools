library(jtools)

context("summ")

# GLM test
set.seed(1)
output <- rpois(100, 5)
input <- log(output) + runif(100,0,1)
clusters <- sample(1:5, size = 100, replace = TRUE)
dat <- as.data.frame(cbind(output, input, clusters))
fitgf <- glm(output ~ input, data = dat, family = poisson)

# Offset test
set.seed(100)
exposures <- rpois(50, 50)
counts <- exposures - rpois(50, 25)
money <- (counts/exposures) + rnorm(50, sd = 1)
talent <- counts*.5 + rnorm(50, sd = 3)
wt <- runif(50, 0, 3)
poisdat <- as.data.frame(cbind(exposures, counts, talent, money, wt))
pmod <- glm(counts ~ talent*money, offset = log(exposures), data = poisdat,
            family = poisson)
pmod2 <- glm(counts ~ talent*money + offset(log(exposures)), data = poisdat,
            family = poisson)
pmodw <- glm(counts ~ talent + money, data = poisdat, weights = wt)

if (requireNamespace("survey")) {
  # survey test
  suppressMessages(library(survey, quietly = TRUE))
  data(api)
  dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
  dstrat$variables$mealsdec <- dstrat$variables$meals/100
  regmodel <- svyglm(mealsdec ~ ell + api00, design = dstrat,
                     family = quasibinomial)
  regmodell <- svyglm(mealsdec ~ ell + api00, design = dstrat)
}

# lm tests (OLS and WLS)
states <- as.data.frame(state.x77)
states$HSGrad <- states$`HS Grad`
set.seed(3)
states$wts <- runif(50, 0, 3)
fit <- lm(Income ~ HSGrad*Murder*Illiteracy, data = states)
fitw <- lm(Income ~ HSGrad*Murder*Illiteracy, data = states, weights = wts)

if (requireNamespace("lme4")) {
  # merMod test
  library(lme4, quietly = TRUE)
  data(sleepstudy)
  mv <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
}

options("summ-stars" = TRUE)

# test_that("standardize gives deprecated warning", {
#   expect_warning(summ(fit, standardize = TRUE))
#   expect_warning(summ(fitgf, standardize = TRUE))
#   if (requireNamespace("lme4")) {
#     expect_warning(summ(mv, standardize = TRUE))
#   }
#   if (requireNamespace("survey")) {
#     expect_warning(summ(regmodel, standardize = TRUE))
#   }
# })

test_that("jsumm: GLMs work", {
  expect_is(summ(fitgf), "summ.glm")
  expect_is(summ(fitgf, scale = TRUE), "summ.glm")
  expect_is(summ(fitgf, center = TRUE), "summ.glm")
})

test_that("jsumm: GLMs w/ offsets work (arg)", {
  expect_is(summ(pmod), "summ.glm")
  expect_is(summ(pmod, scale = TRUE), "summ.glm")
  expect_is(summ(pmod, center = TRUE), "summ.glm")
})

test_that("jsumm: GLMs w/ offsets work (formula)", {
  expect_is(summ(pmod2), "summ.glm")
  expect_is(summ(pmod2, scale = TRUE), "summ.glm")
  expect_is(summ(pmod2, center = TRUE), "summ.glm")
})

test_that("jsumm: GLMs w/ weights work", {
  expect_is(summ(pmodw), "summ.glm")
  expect_is(summ(pmodw, scale = TRUE), "summ.glm")
  expect_is(summ(pmodw, center = TRUE), "summ.glm")
})

test_that("jsumm: partial correlations work", {
  expect_is(summ(fit, part.corr = TRUE), "summ.lm")
  expect_output(print(summ(fit, part.corr = TRUE)))
  expect_warning(summ(fit, part.corr = TRUE, robust = TRUE))
})

test_that("summ: knit_print works", {
  expect_is(jtools:::knit_print.summ.lm(summ(fit)), "knit_asis")
  expect_is(jtools:::knit_print.summ.glm(summ(fitgf)), "knit_asis")
  if (requireNamespace("lme4")) {
    expect_is(jtools:::knit_print.summ.merMod(summ(mv)), "knit_asis")
  }
  if (requireNamespace("survey")) {
    expect_is(jtools:::knit_print.summ.svyglm(summ(regmodel)), "knit_asis")
  }
})

options("summ-stars" = FALSE)

# Test handling of singular models

x1 <- rnorm(100)
x2 <- 2 * x1
y <- rnorm(100)
sing_dat <- as.data.frame(cbind(x1, x2, y))
sing_fit <- lm(y ~ x1 + x2, data = sing_dat)
sing_fitg <- glm(y ~ x1 + x2, data = sing_dat)
int_fit <- lm(y ~ 1, data = sing_dat)
int_fitg <- glm(y ~ 1, data = sing_dat)

test_that("summ handles singular and intercept-only models", {
  expect_is(summ(sing_fit), "summ.lm")
  expect_is(summ(sing_fitg), "summ.glm")
  expect_is(summ(int_fit), "summ.lm")
  expect_is(summ(int_fitg), "summ.glm")
})

#### survey tests ###########################################################

if (requireNamespace("survey")) {
  test_that("jsumm: non-linear svyglm models work", {
    expect_is(summ(regmodel), "summ.svyglm")
  })

  test_that("jsumm: svyglm vifs work", {
    expect_is(summ(regmodel, vifs = TRUE), "summ.svyglm")
  })

  test_that("jsumm: svyglm linear model check works", {
    expect_warning(summ(regmodel, model.check = TRUE))
  })

  test_that("jsumm: svyglm CIs work", {
    expect_is(summ(regmodel, confint = TRUE), "summ.svyglm")
    expect_output(print(summ(regmodel, confint = TRUE)))
  })

  test_that("jsumm: svyglm dropping pvals works", {
    expect_is(summ(regmodel, pvals = FALSE), "summ.svyglm")
    expect_output(print(summ(regmodel, pvals = FALSE)))
  })

  test_that("jsumm: svyglm odds ratios", {
    expect_warning(summ(regmodel, odds.ratio = T))
    expect_is(summ(regmodel, exp = T), "summ.svyglm")
    expect_output(print(summ(regmodel, exp = T)))
  })
}

#### lme4 tests #############################################################

if (requireNamespace("lme4")) {

  gm <- glmer(incidence ~ period + (1 | herd), family = poisson, data = cbpp,
              offset = log(size))

  test_that("jsumm: merMod CIs work", {
    expect_is(s <- summ(mv, confint = TRUE), "summ.merMod")
    expect_output(print(s))
    expect_is(s <- summ(gm, confint = TRUE), "summ.merMod")
    expect_output(print(s))
  })

  test_that("jsumm: merMod dropping pvals works", {
    expect_is(s <- summ(mv, pvals = FALSE), "summ.merMod")
    expect_output(print(s))
    expect_is(s <- summ(gm, pvals = FALSE), "summ.merMod")
    expect_output(print(s))
  })

  test_that("summ: all merMod p-value calculation options work", {
    expect_is(s <- summ(mv, t.df = "s"), "summ.merMod")
    expect_output(print(s))
    expect_is(s <- summ(mv, t.df = "k-r"), "summ.merMod")
    expect_output(print(s))
    expect_is(s <- summ(mv, t.df = "resid"), "summ.merMod")
    expect_output(print(s))
    expect_is(s <- summ(mv, t.df = 1), "summ.merMod")
    expect_output(print(s))
  })

  test_that("jsumm and merMod objects: everything works", {
    expect_is(suppressWarnings(summ(mv, center = TRUE, n.sd = 2,
                                    pvals = FALSE)), "summ.merMod")
    expect_is(summ(mv, scale = TRUE, n.sd = 2, pvals = FALSE), "summ.merMod")
    expect_warning(summ(mv, robust = TRUE))
  })

}

test_that("jsumm: lm CIs work", {
  expect_is(summ(fit, confint = TRUE), "summ.lm")
  expect_output(print(summ(fit, confint = TRUE)))
})

test_that("jsumm: glm CIs work", {
  expect_is(summ(fitgf, confint = TRUE), "summ.glm")
  expect_output(print(summ(fitgf, confint = TRUE)))
})

test_that("jsumm: lm dropping pvals works", {
  expect_is(summ(fit, pvals = FALSE), "summ.lm")
  expect_output(print(summ(fit, pvals = FALSE)))
})

test_that("jsumm: glm dropping pvals works", {
  expect_is(summ(fitgf, pvals = FALSE), "summ.glm")
  expect_output(print(summ(fitgf, pvals = FALSE)))
})

test_that("jsumm and scale_lm: scaling works", {
  expect_is(summ(fitgf, scale = TRUE, n.sd = 2), "summ.glm")
  expect_is(summ(fit, scale = TRUE, n.sd = 2), "summ.lm")
})

test_that("jsumm and center_lm: centering works", {
  expect_is(summ(fitgf, center = TRUE, n.sd = 2), "summ.glm")
  expect_is(summ(fit, center = TRUE, n.sd = 2), "summ.lm")
})

test_that("jsumm can scale weighted lms", {
  expect_is(summ(fitw, scale = T, n.sd = 2, robust = "HC3"), "summ.lm")
  expect_is(summ(fitw, center = T, robust = "HC3"), "summ.lm")
})

test_that("jsumm: lm robust SEs work", {
  expect_is(summ(fit, robust = T), "summ.lm")
  expect_is(summ(fit, robust = "HC4m"), "summ.lm")
  expect_output(print(summ(fit, robust = "HC4m")))
})

test_that("jsumm: lm partial corrs works", {
  expect_is(summ(fit, part.corr = T), "summ.lm")
  expect_output(print(summ(fit, part.corr = T)))
})

test_that("jsumm: warn with partial corrs and robust SEs", {
  expect_warning(summ(fit, robust = "HC3", part.corr = T))
})

test_that("jsumm: glm robust SEs work", {
  expect_is(summ(fitgf, robust = "HC3"), "summ.glm")
  expect_output(print(summ(fitgf, robust = "HC4m")))
})

test_that("jsumm: lm cluster-robust SEs work", {
  expect_is(summ(fit, robust = "HC3", cluster = "Population"), "summ.lm")
  expect_output(print(summ(fit, robust = "HC3", cluster = "Population")))
  expect_error(summ(fit, robust = "HC4m", cluster = "Population"))
  expect_warning(summ(fit, cluster = "Population"))
})

test_that("jsumm: glm cluster-robust SEs work", {
  expect_is(summ(fitgf, robust = "HC3", cluster = clusters), "summ.glm")
  expect_output(print(summ(fitgf, robust = T, cluster = clusters)))
  expect_error(summ(fitgf, robust = "HC4m", cluster = clusters))
})

test_that("jsumm: Printing isn't borked", {
  expect_error(print(summ(fitgf, vifs = TRUE, robust = TRUE)))
  expect_output(print(summ(fitgf, scale = TRUE)))
  if (requireNamespace("survey")) {
    expect_output(print(summ(regmodel, scale = TRUE, n.sd = 2)))
    expect_output(print(summ(regmodel, vifs = TRUE)))
    expect_output(print(summ(regmodell, scale = TRUE, n.sd = 2)))
    expect_output(print(summ(regmodell, vifs = TRUE)))
  }
  expect_output(print(summ(fit, scale = TRUE, n.sd = 2)))
  expect_output(print(summ(fit, vifs = TRUE)))
  if (requireNamespace("lme4")) {
    expect_output(print(summ(mv, scale = TRUE, n.sd = 2, pvals = FALSE)))
  }

})

#### defaults test ##########################################################

set_summ_defaults(digits = 4, model.info = FALSE,
                 model.fit = FALSE, pvals = FALSE, robust = TRUE,
                 confint = TRUE, ci.width = .90, vifs = TRUE,
                 table.format = "grid")

test_that("set_summ_defaults changes options", {

  expect_equal(getOption("jtools-digits"), 4)
  expect_equal(getOption("summ-model.info"), FALSE)
  expect_equal(getOption("summ-model.fit"), FALSE)
  expect_equal(getOption("summ-pvals"), FALSE)
  expect_equal(getOption("summ-robust"), TRUE)
  expect_equal(getOption("summ-confint"), TRUE)
  expect_equal(getOption("summ-ci.width"), .90)
  expect_equal(getOption("summ-vifs"), TRUE)
  expect_equal(getOption("summ.table.format"), "grid")

})

# Set all back to NULL
set_summ_defaults(digits = NULL, model.info = NULL,
                 model.fit = NULL, pvals = NULL, robust = NULL,
                 confint = NULL, ci.width = NULL, vifs = NULL)

