library(jtools)
library(vdiffr)

context("export_summs")

states <- as.data.frame(state.x77)
states$HSGrad <- states$`HS Grad`
states$o70 <- 0
states$o70[states$`Life Exp` > 70] <- 1
set.seed(3)
states$wts <- runif(50, 0, 3)
fit <- lm(Income ~ HSGrad*Murder*Illiteracy, data = states)
fit2 <- lm(Income ~ HSGrad*o70, data = states)
fitw <- lm(Income ~ HSGrad*Murder*Illiteracy, data = states, weights = wts)

if (requireNamespace("survey")) {
  suppressMessages(library(survey, quietly = TRUE))
  data(api)
  dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
  regmodel <- svyglm(api00~ell*meals*both,design=dstrat)
}

set.seed(100)
exposures <- rpois(50, 50)
counts <- exposures - rpois(50, 25)
money <- (counts/exposures) + rnorm(50, sd = 1)
talent <- counts*.5 + rnorm(50, sd = 3)
poisdat <- as.data.frame(cbind(exposures, counts, talent, money))
pmod <- glm(counts ~ talent*money, offset = log(exposures), data = poisdat,
            family = poisson)
pmod_a <- glm(counts ~ talent*money, offset = log(exposures), data = poisdat,
            family = poisson)

if (requireNamespace("lme4")) {
  library(lme4, quietly = TRUE)
  data(VerbAgg)
  mv <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
}

if (requireNamespace("huxtable") & requireNamespace("broom")) {

test_that("Export doesn't fail with lm", {
  expect_is(export_summs(fit, fit2, fitw), "huxtable")
})

test_that("Export accepts named list input", {
  expect_is(export_summs(list("Model 1" = fit, "Model 2" = fit2,
                              "Weighted" = fitw)), "huxtable")
})

test_that("Export uses statistics = 'all' argument", {
  expect_is(export_summs(fit, fit2, fitw, statistics = "all"), "huxtable")
})

test_that("Export works with no summ/huxtable args", {
  expect_is(export_summs(fit, fit2, fitw, statistics = "all",
                         confint = TRUE), "huxtable")
})

test_that("Export accepts summ args with lm", {
  expect_is(export_summs(fit, fit2, fitw, scale = T, robust = T),
            "huxtable")
})

test_that("Export accepts huxreg args with lm", {
  expect_is(export_summs(fit, fit2, fitw, align = ","),
            "huxtable")
})

test_that("Export accepts huxreg and summ args with lm", {
  expect_is(export_summs(fit, fit2, fitw, align = ",",
                         robust = T, n.sd = 2, digits = 1,
                         scale = T), "huxtable")
})

test_that("Export doesn't fail with glm", {
  expect_is(export_summs(pmod), "huxtable")
})

test_that("Export accepts summ args with glm", {
  expect_is(export_summs(pmod, scale = T, robust = T),
            "huxtable")
})

test_that("Export accepts huxreg args with glm", {
  expect_is(export_summs(pmod, align = ","),
            "huxtable")
})

test_that("Export accepts huxreg and summ args with glm", {
  expect_is(export_summs(pmod, align = ",",
                         robust = T), "huxtable")
})

if (requireNamespace("survey")) {
  test_that("Export doesn't fail with svyglm", {
    expect_is(export_summs(regmodel, statistics = c(N = "nobs")), "huxtable")
  })

  test_that("Export accepts summ args with svyglm", {
    expect_is(export_summs(regmodel, scale = T),
              "huxtable")
  })
  test_that("Export accepts huxreg args with svyglm", {
    expect_is(export_summs(regmodel, align = ",",
                           statistics = c(N = "nobs")),
              "huxtable")
  })
  test_that("Export accepts huxreg and summ args with svyglm", {
    expect_is(export_summs(regmodel, align = ",",
                           scale = T), "huxtable")
  })
}

if (requireNamespace("lme4")) {
  test_that("Export doesn't fail with merMod", {
    expect_is(export_summs(mv), "huxtable")
  })

  test_that("Export accepts summ args with merMod", {
    expect_is(export_summs(mv, scale = T),
              "huxtable")
  })

  test_that("Export accepts huxreg args with merMod", {
    expect_is(export_summs(mv, align = ","),
              "huxtable")
  })

  test_that("Export accepts huxreg and summ args with merMod", {
    expect_is(export_summs(mv, align = ",",
                           scale = T), "huxtable")
  })

  test_that("Export can do confidence intervals (merMod)", {
    expect_is(export_summs(mv,
              error_format = "95% CI [{conf.low}, {conf.high}]"), "huxtable")
  })
}

test_that("Export can do confidence intervals (lm)", {
  expect_is(export_summs(fit, fitw,
                         error_format = "CI [{conf.low}, {conf.high}]"),
            "huxtable")
})

test_that("Export can do confidence intervals (glm)", {
  expect_is(export_summs(pmod,
                         error_format = "CI [{conf.low}, {conf.high}]"),
            "huxtable")
})

if (requireNamespace("survey")) {
  test_that("Export can do confidence intervals (svyglm)", {
    expect_is(export_summs(regmodel,
                           error_format = "CI [{conf.low}, {conf.high}]"),
              "huxtable")
  })
}

test_that("Export can take manual coefficient names", {
  expect_is(export_summs(fit, fit2, fitw,
                         coefs = c("HS Grad %" = "HSGrad",
                          "Murder Rate" = "Murder")), "huxtable")
})

}

#### plot_summs ############################################################

context("plot_summs")

if (requireNamespace("broom")) {

test_that("plot_summs doesn't fail with lm", {
  p <- plot_summs(fit, fit2, fitw)
  expect_doppelganger("lm3", p)
})

test_that("plot_summs accepts summ args with lm", {
  p <- plot_summs(fit, fit2, fitw, scale = T, robust = T, n.sd = 2, digits = 2)
  expect_doppelganger("lm3-summ-args", p)
})

test_that("plot_summs works with glm", {
  p <- plot_summs(pmod, pmod_a)
  expect_doppelganger("glm2", p)
})

test_that("plot_summs accepts summ args with glm", {
  p <- plot_summs(pmod, pmod_a, scale = T, robust = T)
  expect_doppelganger("glm2-summ-args", p)
})

test_that("plot_summs accepts odds ratios with glm", {
  p <- plot_summs(pmod, pmod_a, scale = T, robust = T, exp = TRUE,
    model.names = c("Mod1", "Mod2"))
  expect_doppelganger("glm2-odds-modnames", p)
})

if (requireNamespace("survey")) {
  test_that("plot_summs works with svyglm", {
    p <- plot_summs(regmodel)
    expect_doppelganger("svyglm", p)
  })

  test_that("plot_summs accepts summ args with svyglm", {
    p <- plot_summs(regmodel, scale = T)
    expect_doppelganger("svyglm-summ-arg", p)
  })
}

if (requireNamespace("lme4")) {
  test_that("plot_summs works with lmer", {
    p <- plot_summs(mv)
    expect_doppelganger("lmer", p)
  })

  test_that("plot_summs accepts summ args with lmer", {
    p <- plot_summs(mv, scale = T)
    expect_doppelganger("lmer-summ-arg", p)
  })
}

test_that("plot_summs can take manual coefficient names", {
  p <- plot_summs(fit, fit2, fitw, 
    coefs = c("HS Grad %" = "HSGrad", "Murder Rate" = "Murder"))
  expect_doppelganger("lm3-coef-names", p)
})

test_that("plot_summs can omit coefficients", {
  p <- plot_summs(fit, fit2, fitw, coefs = c("HSGrad", "Murder"))
  expect_doppelganger("lm3-coef-omit", p)
})

test_that("plot_summs can facet", {
  p <- plot_summs(fit, fit2, fitw, coefs = c("HSGrad", "Murder"),
    groups = list(c("HSGrad"), c("Murder")))
  expect_doppelganger("lm3-coef-omit-facet", p)
})

context("plot_coefs")

test_that("plot_coefs works", {
  p <- plot_coefs(fit, pmod, model.names = c("Mod1", "Mod2"))
  expect_doppelganger("pc-lm-modnames", p)
})

test_that("plot_coefs can take manual coefficient names", {
  p <- plot_coefs(fit, fit2, fitw, coefs = c("HS Grad %" = "HSGrad",
                                             "Murder Rate" = "Murder"))
  expect_doppelganger("pc-lm3-coefnames", p)
})

test_that("plot_coefs can omit coefficients", {
  p <- plot_coefs(fit, fit2, fitw, coefs = c("HSGrad", "Murder"))
  expect_doppelganger("pc-lm3-coef-omit", p)
})

test_that("inner_ci_level works", {
  p <- plot_coefs(fit, fitw, inner_ci_level = 0.9)
  expect_doppelganger("pc-lm2-inner-ci", p)
})

test_that("plot_summs accepts aesthetic arguments", {
  p <- plot_summs(fit, fit2, fitw, line.size = 2)
  expect_doppelganger("lm3-linesize2", p)
  p <- plot_summs(fit, fit2, fitw, inner_ci_level = 0.8,
         line.size = c(2, 4))
  expect_doppelganger("lm3-linesize2-inner-ci", p)
  expect_message(p <- plot_summs(fit, fit2, fitw, inner_ci_level = 0.8,
         line.size = c(2)))
  expect_doppelganger("lm3-linesize2-inner-ci-msg", p)
})

test_that("plot.distributions works", {
  p <- plot_coefs(fit, plot.distributions = TRUE)
  expect_doppelganger("pc-lm1-dists", p)
  p <- plot_summs(fit, plot.distributions = TRUE, scale = TRUE)
  expect_doppelganger("pc-lm1-dists-scale", p)
  p <- plot_coefs(fit, fitw, plot.distributions = TRUE)
  expect_doppelganger("pc-lm2-dists", p)
  p <- plot_coefs(pmod, plot.distributions = TRUE)
  expect_doppelganger("pc-glm1-dists", p)
  p <- expect_warning(plot_coefs(pmod, plot.distributions = TRUE, exp = TRUE))
  expect_doppelganger("pc-glm1-dists-exp", p)
  p <- plot_coefs(fit, fitw, plot.distributions = TRUE, inner_ci_level = .9)
  expect_doppelganger("pc-lm2-dists-inner-ci", p)
  p <- plot_summs(fit, plot.distributions = TRUE, scale = TRUE, 
    inner_ci_level = .9)
  expect_doppelganger("pc-lm2-dists-inner-ci-scale", p)
})

if (requireNamespace("brms") & requireNamespace("broom.mixed")) {
  bfit1 <- readRDS("brmfit.rds")
  mvfit <- readRDS("mvfit.rds")
  test_that("plot_coefs works with brms", {
    p <- plot_coefs(bfit1) + ggtitle("basic brms fit")
    expect_doppelganger("pc-brm1", p)
    suppressWarnings({ 
    # some parameter names contain underscores: term naming may be unreliable!
    p <- plot_coefs(mvfit) + ggtitle("default mv brms fit")
    })
    expect_doppelganger("pc-brmmv", p)
    suppressWarnings({
    p <- plot_coefs(mvfit, dpar = "sigma") + 
      ggtitle("default dv, dpar sigma mv brms fit")
    })
    expect_doppelganger("pc-brmmv-sigma", p)
    suppressWarnings({
    p <- plot_coefs(mvfit, resp = "wt", dpar = "sigma") +
      ggtitle("select wt dv, dpar sigma mv brms fit")
    })
    expect_doppelganger("pc-brmmv-sigma-select-dv", p)
    suppressWarnings({
    p <- plot_coefs(`MPG DV` = mvfit, `MPG Sigma` = mvfit, 
                    dpar = c(NA, "sigma"), resp = "mpg") +
      ggtitle("select mpg dv, dpar sigma separate models mv brms fit")
    })
    expect_doppelganger("pc-brmmv2-multidist-select-dv", p)
  })
}
}
