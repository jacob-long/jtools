refit_data <- function() {
  set.seed(22)
  d <- data.frame(x = rnorm(100), w = sample(1:4, 100, TRUE),
                   o = runif(100, -1, 1))
  d$count <- rpois(100, exp(.2 + .5 * d$x + d$o))
  d$trials <- sample(5:15, 100, TRUE)
  d$success <- rbinom(100, d$trials, plogis(-.3 + .7 * d$x + d$o))
  d$failure <- d$trials - d$success
  d
}

test_that("update helpers preserve omitted weights and offsets", {
  d <- refit_data()
  m <- lm(count ~ x, d, weights = w, offset = o)
  kept <- j_update(m)
  expect_equal(coef(kept), coef(m))
  expect_equal(model.weights(model.frame(kept)), d$w)
  expect_equal(model.offset(model.frame(kept)), d$o)
  removed <- j_update(m, weights = NULL, offset = NULL)
  expect_null(model.weights(model.frame(removed)))
  expect_null(model.offset(model.frame(removed)))
  replacement <- j_update(m, weights = rep(1, nrow(d)), offset = rep(0, nrow(d)))
  expect_equal(coef(replacement), coef(lm(count ~ x, d)))
})

expect_quasi_fit_statistics <- function(quasi_fit, reference, null_reference) {
  before <- quasi_fit
  actual <- suppressMessages(pR2(quasi_fit))
  full_ll <- as.numeric(logLik(reference))
  null_ll <- as.numeric(logLik(null_reference))
  n <- nrow(model.frame(quasi_fit))
  G2 <- 2 * (full_ll - null_ll)
  expected_mc <- 1 - full_ll / null_ll
  expected_cu <- (1 - exp(-G2 / n)) / (1 - exp(2 * null_ll / n))
  expect_equal(as.numeric(getLL(quasi_fit)), full_ll, tolerance = 1e-8)
  expect_equal(as.numeric(actual$llhNull), null_ll, tolerance = 1e-8)
  expect_equal(actual$G2, G2, tolerance = 1e-8)
  expect_equal(actual$McFadden, expected_mc, tolerance = 1e-8)
  expect_equal(actual$r2CU, expected_cu, tolerance = 1e-8)
  output <- suppressMessages(summ(quasi_fit))
  expect_equal(attr(output, "rsqmc"), expected_mc, tolerance = 1e-8)
  expect_equal(attr(output, "rsq"), expected_cu, tolerance = 1e-8)
  expect_equal(unname(output$coeftable[, "Est."]), unname(coef(quasi_fit)))
  expect_identical(quasi_fit, before)
}

test_that("weighted quasi-family fit statistics match explicit likelihood refits", {
  d <- refit_data()
  for (family in c("poisson", "binomial")) {
    form <- if (family == "poisson") count ~ x else cbind(success, failure) ~ x
    qfamily <- if (family == "poisson") quasipoisson() else quasibinomial()
    lfamily <- if (family == "poisson") poisson() else binomial()
    q <- glm(form, d, family = qfamily, weights = w, offset = o)
    m <- glm(form, d, family = lfamily, weights = w, offset = o)
    m0 <- glm(update(form, . ~ 1), d, family = lfamily, weights = w, offset = o)
    expect_quasi_fit_statistics(q, m, m0)
  }
})

test_that("subsetted null models retain the full-model estimation sample", {
  d <- refit_data()
  d$x[c(30, 50)] <- NA
  d$w[40] <- NA
  q <- glm(count ~ x + offset(o), d, family = quasipoisson, weights = w,
             subset = 21:100, na.action = na.exclude)
  used <- d[as.integer(rownames(model.frame(q))), ]
  m <- glm(count ~ x + offset(o), used, family = poisson, weights = w)
  m0 <- glm(count ~ 1 + offset(o), used, family = poisson, weights = w)
  expect_quasi_fit_statistics(q, m, m0)
  expect_equal(pR2(m)$McFadden, 1 - as.numeric(logLik(m)) / as.numeric(logLik(m0)))
  ordinary <- glm(count ~ x, d, family = poisson, weights = w, offset = o,
                   subset = x > 0, na.action = na.exclude)
  used <- d[as.integer(rownames(model.frame(ordinary))), ]
  null <- glm(count ~ 1, used, family = poisson, weights = w, offset = o)
  expect_equal(pR2(ordinary)$McFadden, 1 - as.numeric(logLik(ordinary)) / as.numeric(logLik(null)))
})

test_that("likelihood refits use saved design rows and the total offset", {
  d <- refit_data()
  q <- glm(count ~ poly(x, 2) + offset(o), d, family = quasipoisson,
             weights = w, offset = x / 10, subset = 21:100)
  m <- glm(count ~ poly(x, 2) + offset(o), d, family = poisson,
             weights = w, offset = x / 10, subset = 21:100)
  expected <- logLik(m)
  d$x <- d$x + 100
  d$w <- 1
  d$o <- 0
  expect_equal(as.numeric(getLL(q)), as.numeric(expected), tolerance = 1e-8)
})

test_that("centered and scaled summaries preserve subsetted fits", {
  d <- refit_data()
  d$x[30] <- NA
  m <- glm(count ~ x, d, family = quasipoisson, weights = w, offset = o,
             subset = 21:100, na.action = na.exclude)
  base <- suppressMessages(summ(m))
  for (changed in list(center_mod(m), scale_mod(m),
                        center_mod(m, data = d[100:1, ]), scale_mod(m, data = d))) {
    expect_equal(nobs(changed), nobs(m))
    expect_equal(model.weights(model.frame(changed)), model.weights(model.frame(m)))
    expect_equal(model.offset(model.frame(changed)), model.offset(model.frame(m)))
    expect_equal(as.numeric(getLL(changed)), as.numeric(getLL(m)), tolerance = 1e-8)
    result <- suppressMessages(summ(changed))
    expect_equal(attr(result, "rsqmc"), attr(base, "rsqmc"), tolerance = 1e-8)
  }
  unweighted <- glm(count ~ x, d, family = quasipoisson, subset = 21:100)
  expect_equal(nobs(scale_mod(unweighted, data = d)), nobs(unweighted))
  expect_error(scale_mod(m, data = d[-40, ]), "every fitted observation")
  d$x[40] <- d$x[40] + 2
  expect_error(scale_mod(m, data = d), "reproduce the fitted model frame")
})

test_that("zero weights, non-log links, and model-free objects retain likelihoods", {
  d <- refit_data()
  d$w[1:5] <- 0
  q <- glm(count ~ x, d, family = quasipoisson, weights = w, offset = o)
  m <- glm(count ~ x, d, family = poisson, weights = w, offset = o)
  expect_equal(as.numeric(getLL(q)), as.numeric(logLik(m)))
  q <- glm(cbind(success, failure) ~ x, d, family = quasibinomial("probit"),
             weights = w, offset = o)
  m <- glm(cbind(success, failure) ~ x, d, family = binomial("probit"),
             weights = w, offset = o)
  expect_equal(as.numeric(getLL(q)), as.numeric(logLik(m)))
  q <- glm(count ~ x, d, family = quasipoisson, weights = w,
             offset = o, model = FALSE, subset = 21:100)
  used <- d[21:100, ]
  m <- glm(count ~ x, used, family = poisson, weights = w, offset = o)
  m0 <- glm(count ~ 1, used, family = poisson, weights = w, offset = o)
  expect_quasi_fit_statistics(q, m, m0)
})

test_that("default-method mixed-model scaling retains subsetted observations", {
  skip_if_not_installed("lme4")
  d <- refit_data()
  d$id <- factor(rep(1:20, each = 5))
  m <- suppressMessages(lme4::lmer(count ~ x + (1 | id), d, weights = w,
                                   subset = 21:100))
  scaled <- suppressMessages(scale_mod(m, data = d))
  expect_equal(nobs(scaled), nobs(m))
  expect_equal(model.weights(model.frame(scaled)), model.weights(model.frame(m)))
})
