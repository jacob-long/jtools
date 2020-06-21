## ----echo=FALSE---------------------------------------------------------------
required <- c("MASS")
if (!all(sapply(required, requireNamespace, quietly = TRUE)))
  knitr::opts_chunk$set(eval = FALSE)
knitr::opts_chunk$set(message = F, warning = F, fig.width = 6, fig.height = 5,
                      dpi = 150, render = knitr::normal_print)
library(jtools)

## -----------------------------------------------------------------------------
library(ggplot2)
data(mpg)
fit <- lm(cty ~ displ + year + cyl + class + fl, data = mpg[mpg$fl != "c",])
summ(fit)

## -----------------------------------------------------------------------------
effect_plot(fit, pred = displ)

## -----------------------------------------------------------------------------
effect_plot(fit, pred = displ, interval = TRUE)

## -----------------------------------------------------------------------------
effect_plot(fit, pred = displ, interval = TRUE, rug = TRUE)

## -----------------------------------------------------------------------------
effect_plot(fit, pred = displ, interval = TRUE, plot.points = TRUE)

## -----------------------------------------------------------------------------
fit_poly <- lm(cty ~ poly(displ, 2) + year + cyl + class + fl, data = mpg)
effect_plot(fit_poly, pred = displ, interval = TRUE, plot.points = TRUE)

## -----------------------------------------------------------------------------
effect_plot(fit_poly, pred = displ, interval = TRUE, partial.residuals = TRUE)

## -----------------------------------------------------------------------------
library(MASS)
data(bacteria)
l_mod <- glm(y ~ trt + week, data = bacteria, family = binomial)
summ(l_mod)

## -----------------------------------------------------------------------------
effect_plot(l_mod, pred = week, interval = TRUE, y.label = "% testing positive")

## -----------------------------------------------------------------------------
pr_mod <- update(l_mod, family = binomial(link = "probit"))
effect_plot(pr_mod, pred = week, interval = TRUE, y.label = "% testing positive")

## -----------------------------------------------------------------------------
library(MASS)
data(Insurance)
Insurance$age_n <- as.numeric(Insurance$Age)
p_mod <- glm(Claims ~ District + Group + age_n, data = Insurance,
             offset = log(Holders), family = poisson)
summ(p_mod)

## -----------------------------------------------------------------------------
effect_plot(p_mod, pred = age_n, interval = TRUE)

## -----------------------------------------------------------------------------
effect_plot(p_mod, pred = age_n, interval = TRUE, plot.points = TRUE)

## -----------------------------------------------------------------------------
effect_plot(p_mod, pred = age_n, interval = TRUE, partial.residuals = TRUE)

## -----------------------------------------------------------------------------
effect_plot(p_mod, pred = age_n, interval = TRUE, partial.residuals = TRUE,
            jitter = c(0.1,0))

## -----------------------------------------------------------------------------
effect_plot(fit, pred = fl, interval = TRUE)

## -----------------------------------------------------------------------------
effect_plot(fit, pred = fl, interval = TRUE, plot.points = TRUE,
            jitter = .2)

## -----------------------------------------------------------------------------
effect_plot(fit, pred = fl, interval = TRUE, partial.residuals = TRUE,
            jitter = .2)

## -----------------------------------------------------------------------------
effect_plot(l_mod, pred = trt, interval = TRUE, y.label = "% testing positive")

## -----------------------------------------------------------------------------
effect_plot(l_mod, pred = trt, interval = TRUE, y.label = "% testing positive",
            cat.geom = "line")

