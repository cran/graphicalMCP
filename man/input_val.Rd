% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_power_input_val.R
\name{test_input_val}
\alias{test_input_val}
\alias{power_input_val}
\title{Validate inputs for testing and power simulations}
\usage{
test_input_val(
  graph,
  p,
  alpha,
  test_groups = list(seq_along(graph$hypotheses)),
  test_types = c("bonferroni"),
  test_corr,
  verbose,
  test_values
)

power_input_val(graph, sim_n, power_marginal, test_corr, success)
}
\arguments{
\item{graph}{An initial graph as returned by \code{\link[=graph_create]{graph_create()}}.}

\item{p}{A numeric vector of p-values (unadjusted, raw), whose values should
be between 0 & 1. The length should match the number of hypotheses in
\code{graph}.}

\item{alpha}{A numeric value of the overall significance level, which should
be between 0 & 1. The default is 0.025 for one-sided hypothesis testing
problems; another common choice is 0.05 for two-sided hypothesis testing
problems. Note when parametric tests are used, only one-sided tests are
supported.}

\item{test_groups}{A list of numeric vectors specifying hypotheses to test
together. Grouping is needed to correctly perform Simes and parametric
tests.}

\item{test_types}{A character vector of test types to apply to each test
group. This is needed to correctly perform Simes and parametric
tests. The length should match the number of elements in \code{test_groups}.}

\item{test_corr}{(Optional) A list of numeric correlation matrices. Each
entry in the list should correspond to each test group. For a test group
using Bonferroni or Simes tests, its corresponding entry in \code{test_corr}
should be \code{NA}. For a test group using parametric tests, its
corresponding entry in \code{test_corr} should be a numeric correlation matrix
specifying the correlation between test statistics for hypotheses in this
test group. The length should match the number of elements in
\code{test_groups}.}

\item{verbose}{A logical scalar specifying whether the details of the
adjusted p-value calculations should be included in results. When
\code{verbose = TRUE}, adjusted p-values are provided for each intersection
hypothesis. The default is \code{verbose = FALSE}.}

\item{test_values}{A logical scalar specifying whether adjusted significance
levels should be provided for each hypothesis. When \code{test_values = TRUE},
it provides an equivalent way of performing graphical multiple comparison
procedures by comparing each p-value with its significance level. If the
p-value of a hypothesis is less than or equal to its significance level,
the hypothesis is rejected. The default is \code{test_values = FALSE}.}

\item{sim_n}{An integer scalar specifying the number of simulations. The
default is 1e5.}

\item{power_marginal}{A numeric vector of marginal power values to use when
simulating p-values. See Details for more on the simulation process.}

\item{success}{A list of user-defined functions to specify the success
criteria. Functions must take one simulation's logical vector of results as
an input, and return a length-one logical vector. For instance, if
"success" means rejecting hypotheses 1 and 2, use \code{sim_success = list("1 and 2" = function(x) x[1] && x[2])}. If the list is not named, the function
body will be used as the name. Lambda functions also work starting with R
4.1, e.g. \verb{sim_success = list(\\(x) x[3] || x[4])}.}
}
\value{
Returns \code{graph} invisibly
}
\description{
Validate inputs for testing and power simulations
}
\keyword{internal}
