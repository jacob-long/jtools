## ----echo=FALSE----------------------------------------------------------
required <- c("survey", "huxtable", "broom", "lme4", "quantreg")
if (!all(sapply(required, requireNamespace, quietly = TRUE)))
  knitr::opts_chunk$set(eval = FALSE)
knitr::opts_chunk$set(message = F, warning = F, fig.width = 6, fig.height = 5,
                      render = knitr::normal_print)
library(jtools)

## ------------------------------------------------------------------------
# Fit model
states <- as.data.frame(state.x77)
fit <- lm(Income ~ Frost + Illiteracy + Murder, data = states)
summ(fit)

## ----render = 'knit_print'-----------------------------------------------
summ(fit)

## ------------------------------------------------------------------------
summ(fit, robust = "HC1")

## ------------------------------------------------------------------------
summ(fit, scale = TRUE)

## ------------------------------------------------------------------------
summ(fit, scale = TRUE, n.sd = 2)

## ------------------------------------------------------------------------
summ(fit, center = TRUE)

## ------------------------------------------------------------------------
summ(fit, confint = TRUE, digits = 3)

## ------------------------------------------------------------------------
summ(fit, confint = TRUE, ci.width = .5, digits = 3)

## ------------------------------------------------------------------------
summ(fit, confint = TRUE, pvals = FALSE, digits = 3)

## ------------------------------------------------------------------------
fitg <- glm(vs ~ drat + mpg, data = mtcars, family = binomial)

summ(fitg)

## ------------------------------------------------------------------------
summ(fitg, exp = TRUE)

## ----message = FALSE, warning = FALSE------------------------------------
library(lme4)
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

summ(fm1)

## ------------------------------------------------------------------------
plot_summs(fit)

## ------------------------------------------------------------------------
plot_summs(fit, scale = TRUE)

## ------------------------------------------------------------------------
plot_summs(fit, scale = TRUE, inner_ci_level = .9)

## ------------------------------------------------------------------------
plot_summs(fit, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)

## ------------------------------------------------------------------------
fit2 <- lm(Income ~ Frost + Illiteracy + Murder + `HS Grad`,
           data = states)
plot_summs(fit, fit2, scale = TRUE)

## ------------------------------------------------------------------------
plot_summs(fit, fit2, scale = TRUE, plot.distributions = TRUE)

## ------------------------------------------------------------------------
plot_summs(fit, fit, fit, scale = TRUE, robust = list(FALSE, "HC0", "HC3"),
           model.names = c("OLS", "HC0", "HC3"))

## ------------------------------------------------------------------------
effect_plot(fit, pred = Illiteracy)

## ------------------------------------------------------------------------
effect_plot(fit, pred = Illiteracy, interval = TRUE)

## ------------------------------------------------------------------------
effect_plot(fit, pred = Illiteracy, interval = TRUE, plot.points = TRUE)

## ------------------------------------------------------------------------
effect_plot(fit, pred = Illiteracy, interval = TRUE, plot.points = TRUE,
            robust = "HC3")

## ------------------------------------------------------------------------
effect_plot(fitg, pred = mpg, interval = TRUE)

## ------------------------------------------------------------------------
effect_plot(fitg, pred = mpg, plot.points = TRUE)

## ----eval = FALSE--------------------------------------------------------
#  export_summs(fit, fit2, scale = TRUE)

## ----echo = FALSE, results = 'asis'--------------------------------------
huxtable::print_html(export_summs(fit, fit2, scale = TRUE))

## ----eval = FALSE--------------------------------------------------------
#  export_summs(fit, fit2, scale = TRUE,
#               error_format = "[{conf.low}, {conf.high}]")

## ----echo = FALSE, results = 'asis'--------------------------------------
huxtable::print_html(export_summs(fit, fit2, scale = TRUE,
                     error_format = "[{conf.low}, {conf.high}]"))

## ----eval = FALSE--------------------------------------------------------
#  export_summs(fit, fit2, scale = TRUE, to.file = "docx", file.name = "test.docx")

## ------------------------------------------------------------------------
summ(fit, model.info = FALSE, model.fit = FALSE)

## ------------------------------------------------------------------------
summ(fit, model.info = FALSE, digits = 5)

## ------------------------------------------------------------------------
summ(fit, model.info = FALSE, digits = 1)

## ------------------------------------------------------------------------
options("jtools-digits" = 2)
summ(fit, model.info = FALSE)

## ----echo = F------------------------------------------------------------
options("jtools-digits" = NULL)

## ------------------------------------------------------------------------
j <- summ(fit, digits = 3)

j$coeftable

## ----eval = F------------------------------------------------------------
#  set_summ_defaults(digits = 2, pvals = FALSE, robust = "HC3")

## ------------------------------------------------------------------------
summ(fit, vifs = TRUE)

