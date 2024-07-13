## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE, warning = FALSE----------------------------------
library(graphicalMCP)
library(lrstat)
library(gMCP)

## ----generate-weights---------------------------------------------------------
set.seed(1234)
identical <- NULL
for (i in 1:1000) {
  graph <- random_graph(5)
  graphicalmcp_weights <- graphicalMCP::graph_generate_weights(graph)
  dimnames(graphicalmcp_weights) <- list(NULL, NULL)
  gmcp_weights <-
    gMCP::generateWeights(graph$transitions, graph$hypotheses)
  gmcp_weights <- gmcp_weights[nrow(gmcp_weights):1, ] # Reorder rows
  identical <- c(
    identical,
    all.equal(gmcp_weights, graphicalmcp_weights, tolerance = 1e-7)
  )
}
all(identical)

## ----shortcut-----------------------------------------------------------------
set.seed(1234)
alpha <- 0.025
identical <- NULL
for (i in 1:10000) {
  graph <- random_graph(5)
  p <- runif(5, 0, alpha)
  graphicalmcp_test_shortcut <-
    graph_test_shortcut(graph, p, alpha = alpha)$outputs$adjusted_p
  gmcp_test_shortcut <-
    gMCP(as_graphMCP(graph), p, alpha = alpha)@adjPValues
  identical <- c(
    identical,
    all.equal(graphicalmcp_test_shortcut, gmcp_test_shortcut, tolerance = 1e-7)
  )
}
all(identical)

## ----power-shortcut, eval = FALSE---------------------------------------------
#  set.seed(1234)
#  alpha <- 0.025
#  sim_corr <- matrix(.5, 5, 5)
#  diag(sim_corr) <- 1
#  graphicalmcp_power <- NULL
#  gmcp_power <- NULL
#  for (i in 1:1000) {
#    graph <- random_graph(5)
#    marginal_power <- runif(5, 0.5, 0.9)
#    noncentrality_parameter <-
#      qnorm(1 - 0.025, lower.tail = TRUE) -
#      qnorm(1 - marginal_power, lower.tail = TRUE)
#  
#    set.seed(1234 + i - 1)
#    graphicalmcp_power <- rbind(
#      graphicalmcp_power,
#      graph_calculate_power(
#        graph,
#        alpha = alpha,
#        power_marginal = marginal_power,
#        sim_corr = sim_corr,
#        sim_n = 2^17
#      )$power$power_local
#    )
#  
#    set.seed(1234 + i - 1)
#    gmcp_power <- rbind(
#      gmcp_power,
#      calcPower(
#        graph$hypotheses,
#        alpha = alpha,
#        graph$transitions,
#        mean = noncentrality_parameter,
#        corr.sim = sim_corr,
#        n.sim = 2^17
#      )$LocalPower
#    )
#  }
#  
#  diff <- data.frame(
#    rbind(graphicalmcp_power, gmcp_power),
#    procedure = rep(c("graphicalMCP", "gMCP"), each = nrow(graphicalmcp_power))
#  )
#  
#  write.csv(
#    diff,
#    here::here("vignettes/cache/comparisons_power_shortcut.csv"),
#    row.names = FALSE
#  )
#  
#  diff <- read.csv(here::here("vignettes/cache/comparisons_power_shortcut.csv"))
#  graphicalmcp_power <- subset(diff, procedure == "graphicalMCP")
#  gmcp_power <- subset(diff, procedure == "gMCP")
#  round(
#    max(
#      abs(
#        graphicalmcp_power[, -ncol(graphicalmcp_power)] -
#          gmcp_power[, -ncol(gmcp_power)]
#      )
#    ),
#    4
#  ) # Maximum difference in local power among 1000 cases

## ----parametric---------------------------------------------------------------
hypotheses <- c(0.5, 0.5, 0, 0)
transitions <- rbind(
  c(0, 0.5, 0.5, 0),
  c(0.5, 0, 0, 0.5),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
graph <- graph_create(hypotheses, transitions)

set.seed(1234)
alpha <- 0.025
identical <- NULL
test_corr <- rbind(
  c(1, 0.5, NA, NA),
  c(0.5, 1, NA, NA),
  c(NA, NA, 1, NA),
  c(NA, NA, NA, 1)
)
for (i in 1:10000) {
  p <- runif(4, 0, alpha)
  graphicalmcp_test_parametric <- graph_test_closure(
    graph,
    p,
    alpha = alpha,
    test_groups = list(1:2, 3:4),
    test_types = c("parametric", "bonferroni"),
    test_corr = list(test_corr[1:2, 1:2], NA)
  )$outputs$adjusted_p
  gmcp_test_parametric <- gMCP(
    as_graphMCP(graph),
    p,
    alpha = 0.025,
    correlation = test_corr
  )@adjPValues
  identical <- c(
    identical,
    all.equal(graphicalmcp_test_parametric, gmcp_test_parametric, tolerance = 1e-7)
  )
}
all(identical)

## ----power-parametric, eval = FALSE-------------------------------------------
#  hypotheses <- c(0.5, 0.5, 0, 0)
#  transitions <- rbind(
#    c(0, 0.5, 0.5, 0),
#    c(0.5, 0, 0, 0.5),
#    c(0, 1, 0, 0),
#    c(1, 0, 0, 0)
#  )
#  graph <- graph_create(hypotheses, transitions)
#  test_corr <- rbind(
#    c(1, 0.5, NA, NA),
#    c(0.5, 1, NA, NA),
#    c(NA, NA, 1, NA),
#    c(NA, NA, NA, 1)
#  )
#  sim_corr <- matrix(0.5, 4, 4)
#  diag(sim_corr) <- 1
#  set.seed(1234)
#  alpha <- 0.025
#  graphicalmcp_power_parametric <- NULL
#  gmcp_power_parametric <- NULL
#  for (i in 1:100) {
#    marginal_power <- runif(4, 0.5, 0.9)
#    noncentrality_parameter <-
#      qnorm(1 - 0.025, lower.tail = TRUE) -
#      qnorm(1 - marginal_power, lower.tail = TRUE)
#  
#    set.seed(1234 + i - 1)
#    graphicalmcp_power_parametric <- rbind(
#      graphicalmcp_power_parametric,
#      graph_calculate_power(
#        graph,
#        alpha = alpha,
#        test_groups = list(1:2, 3:4),
#        test_types = c("parametric", "bonferroni"),
#        test_corr = list(test_corr[1:2, 1:2], NA),
#        power_marginal = marginal_power,
#        sim_corr = sim_corr,
#        sim_n = 2^14
#      )$power$power_local
#    )
#  
#    set.seed(1234 + i - 1)
#    gmcp_power_parametric <- rbind(
#      gmcp_power_parametric,
#      calcPower(
#        graph$hypotheses,
#        alpha = alpha,
#        graph$transitions,
#        corr.test = test_corr,
#        mean = noncentrality_parameter,
#        corr.sim = sim_corr,
#        n.sim = 2^14
#      )$LocalPower
#    )
#  }
#  
#  diff <- data.frame(
#    rbind(graphicalmcp_power_parametric, gmcp_power_parametric),
#    procedure = rep(c("graphicalMCP", "gMCP"), each = nrow(gmcp_power_parametric))
#  )
#  
#  write.csv(
#    diff,
#    here::here("vignettes/cache/comparisons_power_parametric.csv"),
#    row.names = FALSE
#  )
#  
#  diff <- read.csv(here::here("vignettes/cache/comparisons_power_parametric.csv"))
#  graphicalmcp_power <- subset(diff, procedure == "graphicalMCP")
#  gmcp_power <- subset(diff, procedure == "gMCP")
#  round(
#    max(
#      abs(
#        graphicalmcp_power_parametric[, -ncol(graphicalmcp_power_parametric)] -
#          gmcp_power_parametric[, -ncol(gmcp_power)]
#      )
#    ),
#    4
#  ) # Maximum difference in local power among 100 cases

## ----simes--------------------------------------------------------------------
hypotheses <- c(0.5, 0.5, 0, 0)
eps <- 0.0001
transitions <- rbind(
  c(0, 1 - eps, eps, 0),
  c(1 - eps, 0, 0, eps),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
graph <- graph_create(hypotheses, transitions)

set.seed(1234)
alpha <- 0.025
identical <- NULL
family <- rbind(
  c(1, 1, 0, 0),
  c(0, 0, 1, 0),
  c(0, 0, 0, 1)
)
for (i in 1:10000) {
  p <- runif(4, 0, alpha)
  graphicalmcp_test_simes <- graph_test_closure(
    graph,
    p,
    alpha = alpha,
    test_groups = list(1:2, 3:4),
    test_types = c("simes", "bonferroni")
  )$outputs$adjusted_p
  names(graphicalmcp_test_simes) <- NULL
  lrstat_test_simes <-
    fadjpsim(
      fwgtmat(graph$hypotheses, graph$transitions),
      p,
      family
    )

  identical <- c(
    identical,
    all.equal(graphicalmcp_test_simes, lrstat_test_simes, tolerance = 1e-7)
  )
}
all(identical)

