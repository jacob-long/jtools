% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export_summ.R
\name{export_summs}
\alias{export_summs}
\title{Export regression summaries to tables}
\usage{
export_summs(
  ...,
  error_format = "({std.error})",
  error_pos = c("below", "right", "same"),
  ci_level = 0.95,
  statistics = NULL,
  model.names = NULL,
  coefs = NULL,
  to.file = NULL,
  file.name = NULL
)
}
\arguments{
\item{...}{At minimum, a regression object(s). See details for more
arguments.}

\item{error_format}{Which of standard error, confidence intervals, test
statistics, or p values should be used to express uncertainty of estimates
for regression coefficients? See details for more info.
Default: \code{"({std.error})"}}

\item{error_pos}{Where should the error statistic defined in
\code{error_style} be placed relative to the coefficient estimate?
Default: "below"}

\item{ci_level}{If reporting confidence intervals, what should the
confidence level be? By default, it is .95 if
confidence intervals are requested in \code{error_format}.}

\item{statistics}{Which model summary statistics should be included?
See \code{\link[huxtable]{huxreg}} for more on usage. The default
for this function depends on the model type. See details for more on
the defaults by model type.}

\item{model.names}{If you want to give your model(s) names at the top
of each column, provide them here as a character vector.
Otherwise, they will just be labeled by number. Default: NULL}

\item{coefs}{If you want to include only a subset of the coefficients in
the table, specify them here in a character vector. If you want the
table to show different names for the coefficients, give a named vector
where the names are the preferred coefficient names. See details for more.}

\item{to.file}{Export the table to a Microsoft Word, PDF, or HTML document?
This functionality relies on \code{huxtable}'s \code{quick_} functions
(\code{\link[huxtable:quick-output]{huxtable::quick_docx()}}, \code{\link[huxtable:quick-output]{huxtable::quick_pdf()}},
\code{\link[huxtable:quick-output]{huxtable::quick_html()}}, \code{\link[huxtable:quick-output]{huxtable::quick_xlsx()}}). Acceptable arguments
are "Word" or "docx" (equivalent), "pdf", "html", or "xlsx". All are
case insensitive. Default is NULL, meaning the table is not saved.}

\item{file.name}{File name with (optionally) file path to save the
file. Ignored if \code{to.file} is FALSE. Default: NULL}
}
\value{
A \code{huxtable}.
}
\description{
This function allows users to use the features of
\code{\link[=summ]{summ()}} (e.g., standardization, robust standard errors)
in the context of shareable HTML, LaTeX, and
Microsoft Word tables. It relies heavily on \code{\link[huxtable:huxreg]{huxtable::huxreg()}}
to do the table formatting. This is particularly useful for putting
the results of multiple models into a single table.
}
\details{
There are many optional parameters not documented above. Any
argument that you would want to pass to \code{\link[=summ]{summ()}}, for instance,
will be used. Of particular interest may be the robust and scale
arguments. Note that some \code{summ} arguments may not have any bearing
on the table output.

The default model summary statistics reporting follows this logic:

\itemize{
\item summ.lm = \code{c(N = "nobs", R2 = "r.squared")},
\item summ.glm = \code{c(N = "nobs", AIC = "AIC", BIC = "BIC",
                       `Pseudo R2` = "pseudo.r.squared")},
\item summ.svyglm = \code{c(N = "nobs", R2 = "r.squared")},
\item summ.merMod = \code{c(N = "nobs", AIC = "AIC", BIC = "BIC",
                          `R2 (fixed)` = "r.squared.fixed",
                          `R2 (total)` = "r.squared")}
\item summ.rq = \code{c(N = "nobs", tau = "tau", R1 = "r.1", AIC = "AIC", BIC = "BIC")}
}

Be sure to look at the \code{\link[=summ]{summ()}} documentation for more on the calculation
of these and other statistics, especially for mixed models.

If you set \code{statistics = "all"}, then the statistics argument
passed to \code{huxreg} will be \code{NULL}, which reports whichever
model statistics are available via \code{glance}. If you want no
model summary statistics, set the argument to \code{character(0)}.

You have a few options for the \code{error_format} argument.
You can include anything returned by \code{\link[broom:reexports]{broom::tidy()}}
(see also \code{\link[=tidy.summ]{tidy.summ()}}). For the most part, you will
be interested in \code{std.error} (standard error), \code{statistic}
(test statistic, e.g. t-value or z-value), \code{p.value}, or
\code{conf.high} and \code{conf.low}, which correspond to the
upper and lower bounds of the confidence interval for the estimate.
Note that the default \code{ci_level} argument is .95, but you
can alter that as desired.

To format the error statistics, simply put the statistics desired in
curly braces wherever you want them in a character string. For example,
if you want the standard error in parentheses, the argument would be
\code{"({std.error})"}, which is the default. Some other ideas:
\itemize{
\item \code{"({statistic})"}, which gives you the test statistic in
parentheses.
\item \code{"({statistic}, p = {p.value})"}, which gives the test
statistic followed by a "p =" p value all in parentheses. Note that
you'll have to pay special attention to rounding if you do this to keep
cells sufficiently narrow.
\item \code{"[{conf.low}, {conf.high}]"}, which gives the confidence
interval in the standard bracket notation. You could also explicitly
write the confidence level, e.g.,
\code{"CI [{conf.low}, {conf.high}]"}.
}

For \code{coefs}, the argument is slightly different than what is default
in \code{huxreg}. If you provide a named vector of coefficients, then
the table will refer to the selected coefficients by the names of the
vector rather than the coefficient names. For instance, if I want to
include only the coefficients for the \code{hp} and \code{mpg} but have
the table refer to them as "Horsepower" and "Miles/gallon", I'd provide
the argument like this:
\code{c("Horsepower" = "hp", "Miles/gallon" = "mpg")}

You can also pass any argument accepted by the
\code{\link[huxtable:huxreg]{huxtable::huxreg()}} function. A few that are likely to be
oft-used are documented above, but visit \code{huxreg}'s documentation
for more info.

For info on converting the \code{\link[huxtable:huxtable]{huxtable::huxtable()}} object to
HTML or LaTeX, see \code{huxtable}'s documentation.
}
\examples{
states <- as.data.frame(state.x77)
fit1 <- lm(Income ~ Frost, data = states)
fit2 <- lm(Income ~ Frost + Illiteracy, data = states)
fit3 <- lm(Income ~ Frost + Illiteracy + Murder, data = states)

if (requireNamespace("huxtable")) {
  # Export all 3 regressions with "Model #" labels,
  # standardized coefficients, and robust standard errors
  export_summs(fit1, fit2, fit3,
               model.names = c("Model 1","Model 2","Model 3"),
               coefs = c("Frost Days" = "Frost",
                         "\% Illiterate" = "Illiteracy",
                         "Murder Rate" = "Murder"),
               scale = TRUE, robust = TRUE)
}

}
\seealso{
\code{\link{summ}}

\code{\link[huxtable]{huxreg}}
}
