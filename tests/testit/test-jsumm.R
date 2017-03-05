library(jtools)

set.seed(1)
output <- rpois(100, 5)
input <- log(output) + runif(100,0,1)
df <- as.data.frame(cbind(input, output))
fitgf <- glm(output ~ input, data = df, family = poisson)

library(survey)
api <- data(api)
dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
regmodel <- svyglm(meals/100 ~ ell + api00, design = dstrat,
                   family = quasibinomial)
regmodell <- svyglm(meals/100 ~ ell + api00, design = dstrat)

myfit <- lm(Income ~ Frost + Illiteracy + Murder, data = as.data.frame(state.x77))

assert("jsumm: non-linear models work",
  any(class(j_summ(fitgf)) == "j_summ.glm"),
  any(class(j_summ(fitgf, standardize = TRUE)) == "j_summ.glm"),
  any(class(j_summ(fitgf, center = TRUE)) == "j_summ.glm")
)


assert("jsumm: non-linear svyglm models work",
  any(class(j_summ(regmodel)) == "j_summ.svyglm")
)

assert("jsumm: svyglm vifs work",
  any(class(j_summ(regmodel, vifs = TRUE)) == "j_summ.svyglm")
)

assert("jsumm: svyglm linear model check works",
  any(class(j_summ(regmodel, model.check = TRUE)) == "j_summ.svyglm")
)

assert("jsumm and scale_lm: scaling works",
  any(class(j_summ(fitgf, standardize = TRUE, n.sd = 2)) == "j_summ.glm"),
  any(class(j_summ(myfit, standardize = TRUE, n.sd = 2)) == "j_summ.lm")
)

assert("jsumm and center_lm: centering works",
  any(class(j_summ(fitgf, center = TRUE, n.sd = 2)) == "j_summ.glm"),
  any(class(j_summ(myfit, center = TRUE, n.sd = 2)) == "j_summ.lm")
)

assert("jsumm: Printing isn't borked",
  !is.null(capture.output(print(j_summ(fitgf, vifs = TRUE)))),
  !is.null(capture.output(print(j_summ(fitgf, standardize = TRUE)))),
  !is.null(capture.output(print(j_summ(regmodel, standardize = TRUE, n.sd = 2)))),
  !is.null(capture.output(print(j_summ(regmodel, model.check = TRUE, vifs = TRUE)))),
  !is.null(capture.output(print(j_summ(regmodell, standardize = TRUE, n.sd = 2)))),
  !is.null(capture.output(print(j_summ(regmodell, model.check = TRUE, vifs = TRUE)))),
  !is.null(capture.output(print(j_summ(myfit, standardize = TRUE, n.sd = 2)))),
  !is.null(capture_output(print(j_summ(myfit, model.check = TRUE, vifs = TRUE))))
)
