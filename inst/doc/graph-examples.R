## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  fig.align = "center",
  collapse = TRUE,
  comment = "#>",
  fig.dim = c(3, 3)
)

## ----setup, warning=FALSE, message=FALSE--------------------------------------
library(graphicalMCP)

## ----bonferroni---------------------------------------------------------------
set.seed(1234)
alpha <- 0.025
m <- 3
bonferroni_graph <- bonferroni(rep(1 / m, m))
# transitions <- matrix(0, m, m)
# bonferroni_graph <- graph_create(rep(1 / m, m), transitions)

plot(
  bonferroni_graph,
  layout = igraph::layout_in_circle(
    as_igraph(bonferroni_graph),
    order = c(2, 1, 3)
  ),
  vertex.size = 70
)

p_values <- runif(m, 0, alpha)

test_results <-
  graph_test_shortcut(
    bonferroni_graph,
    p = p_values,
    alpha = alpha
  )

test_results$outputs$rejected

## ----bonferroni-holm----------------------------------------------------------
set.seed(1234)
alpha <- 0.025
m <- 3
holm_graph <- bonferroni_holm(rep(1 / m, m))
# transitions <- matrix(1 / (m - 1), m, m)
# diag(transitions) <- 0
# holm_graph <- graph_create(rep(1 / m, m), transitions)

plot(holm_graph,
  layout = igraph::layout_in_circle(
    as_igraph(holm_graph),
    order = c(2, 1, 3)
  ),
  vertex.size = 70
)

p_values <- runif(m, 0, alpha)

test_results <- graph_test_shortcut(holm_graph, p = p_values, alpha = alpha)

test_results$outputs$rejected

## ----fixed-sequence-1, fig.dim=c(4.5, 0.9)------------------------------------
set.seed(1234)
alpha <- 0.025
m <- 3
fixed_sequence_graph <- fixed_sequence(m)
# transitions <- rbind(
#   c(0, 1, 0),
#   c(0, 0, 1),
#   c(0, 0, 0)
# )
# fixed_sequence_graph <- graph_create(c(1, 0, 0), transitions)

plot(fixed_sequence_graph, nrow = 1, asp = 0.05, vertex.size = 40)

p_values <- runif(m, 0, alpha)

test_results <-
  graph_test_shortcut(
    fixed_sequence_graph,
    p = p_values,
    alpha = alpha
  )

test_results$outputs$rejected

## ----fallback, fig.dim=c(4.5, 0.9)--------------------------------------------
set.seed(1234)
alpha <- 0.025
m <- 3
fallback_graph <- fallback(rep(1 / 3, 3))
# transitions <- rbind(
#   c(0, 1, 0),
#   c(0, 0, 1),
#   c(0, 0, 0)
# )
# fallback_graph <- graph_create(rep(1 / 3, 3), transitions)

plot(fallback_graph, nrow = 1, asp = 0.05, vertex.size = 40)

p_values <- runif(m, 0, alpha)

test_results <-
  graph_test_shortcut(
    fallback_graph,
    p = p_values,
    alpha = alpha
  )

test_results$outputs$rejected

## ----fallback-improved, fig.dim=c(4.5, 0.9)-----------------------------------
set.seed(1234)
alpha <- 0.025
m <- 3
fallback_improved_1_graph <- fallback_improved_1(rep(1 / 3, 3))
# hypotheses <- rep(1 / 3, 3)
# transitions <- rbind(
#   c(0, 1, 0),
#   c(0, 0, 1),
#   c(hypotheses[seq_len(m - 1)] / sum(hypotheses[seq_len(m - 1)]), 0)
# )
# fallback_improved_1_graph <- graph_create(rep(1 / 3, 3), transitions)

plot(
  fallback_improved_1_graph,
  nrow = 1,
  asp = 0.05,
  vertex.size = 40,
  edge_curves = c("pairs" = 7, "H3|H1" = -10)
)

epsilon <- 0.0001
fallback_improved_2_graph <- fallback_improved_2(rep(1 / 3, 3), epsilon)
# hypotheses <- rep(1 / 3, 3)
# transitions <- rbind(
#   c(0, 1, 0),
#   c(1 - epsilon, 0, epsilon),
#   c(1, 0, 0)
# )
# fallback_improved_2_graph <- graph_create(rep(1 / 3, 3), transitions)

plot(
  fallback_improved_2_graph,
  nrow = 1,
  asp = 0.05,
  eps = 0.0001,
  edge_curves = c("pairs" = 7, "H3|H1" = -10),
  vertex.size = 40
)

p_values <- runif(m, 0, alpha)

test_results <-
  graph_test_shortcut(
    fallback_improved_2_graph,
    p = p_values,
    alpha = alpha
  )

test_results$outputs$rejected

## ----serial-gatekeeping, fig.dim=c(4.5, 0.9)----------------------------------
set.seed(1234)
alpha <- 0.025
m <- 3
epsilon <- 0.0001

transitions <- rbind(
  c(0, 1, 0),
  c(1 - epsilon, 0, epsilon),
  c(0, 0, 0)
)

serial_gatekeeping_graph <- graph_create(c(0.5, 0.5, 0), transitions)

plot(
  serial_gatekeeping_graph,
  nrow = 1,
  asp = 0.05,
  eps = 0.0001,
  edge_curves = c("pairs" = 7, "H3|H1" = -10),
  vertex.size = 40
)

p_values <- runif(m, 0, alpha)

test_results <-
  graph_test_shortcut(
    serial_gatekeeping_graph,
    p = p_values,
    alpha = alpha
  )

test_results$outputs$rejected

## ----parallel-gatekeeping-----------------------------------------------------
set.seed(1234)
alpha <- 0.025
m <- 4

transitions <- rbind(
  c(0, 0, 0.5, 0.5),
  c(0, 0, 0.5, 0.5),
  c(0, 0, 0, 1),
  c(0, 0, 1, 0)
)

parallel_gatekeeping_graph <- graph_create(c(0.5, 0.5, 0, 0), transitions)

plot(parallel_gatekeeping_graph, vertex.size = 70)

p_values <- runif(m, 0, alpha)

test_results <-
  graph_test_shortcut(
    parallel_gatekeeping_graph,
    p = p_values,
    alpha = alpha
  )

test_results$outputs$rejected

## ----parallel-gatekeeping-improved--------------------------------------------
set.seed(1234)
alpha <- 0.025
m <- 4
epsilon <- 0.0001

transitions <- rbind(
  c(0, 0, 0.5, 0.5),
  c(0, 0, 0.5, 0.5),
  c(epsilon, 0, 0, 1 - epsilon),
  c(0, epsilon, 1 - epsilon, 0)
)

parallel_gatekeeping_improved_graph <-
  graph_create(c(0.5, 0.5, 0, 0), transitions)

plot(parallel_gatekeeping_improved_graph, eps = 0.0001, vertex.size = 70)

p_values <- runif(m, 0, alpha)

test_results <-
  graph_test_shortcut(
    parallel_gatekeeping_improved_graph,
    p = p_values,
    alpha = alpha
  )

test_results$outputs$rejected

## ----simple-successive-1------------------------------------------------------
set.seed(1234)
alpha <- 0.025
m <- 4
simple_successive_graph <- simple_successive_1()
# transitions <- rbind(
#   c(0, 0, 1, 0),
#   c(0, 0, 0, 1),
#   c(0, 1, 0, 0),
#   c(1, 0, 0, 0)
# )
# simple_successive_graph <- graph_create(c(0.5, 0.5, 0, 0), transitions)

plot(simple_successive_graph, layout = "grid", nrow = 2, vertex.size = 70)

p_values <- runif(m, 0, alpha)

test_results <-
  graph_test_shortcut(
    simple_successive_graph,
    p = p_values,
    alpha = alpha
  )

test_results$outputs$rejected

## ----simple-successive-var----------------------------------------------------
set.seed(1234)
alpha <- 0.025
m <- 4

successive_var <- simple_successive_var <- function(gamma) {
  graph_create(
    c(0.5, 0.5, 0, 0),
    rbind(
      c(0, gamma, 1 - gamma, 0),
      c(gamma, 0, 0, 1 - gamma),
      c(0, 1, 0, 0),
      c(1, 0, 0, 0)
    )
  )
}

successive_var_graph <- successive_var(0.5)
plot(successive_var_graph, layout = "grid", nrow = 2, vertex.size = 70)
p_values <- runif(m, 0, alpha)

test_results <-
  graph_test_shortcut(
    successive_var_graph,
    p = p_values,
    alpha = alpha
  )

test_results$outputs$rejected

## ----hommel-------------------------------------------------------------------
set.seed(1234)
alpha <- 0.025
m <- 3
hommel_graph <- bonferroni_holm(rep(1 / m, m))

plot(
  hommel_graph,
  layout = igraph::layout_in_circle(
    as_igraph(hommel_graph),
    order = c(2, 1, 3)
  ),
  vertex.size = 70
)

p_values <- runif(m, 0, alpha)

test_results <- graph_test_closure(
  hommel_graph,
  p = p_values,
  alpha = alpha,
  test_types = "simes"
)

test_results$outputs$rejected

## ----dunnett------------------------------------------------------------------
set.seed(1234)
alpha <- 0.025
m <- 3
dunnett_graph <- bonferroni_holm(rep(1 / m, m))

plot(
  dunnett_graph,
  layout = igraph::layout_in_circle(
    as_igraph(dunnett_graph),
    order = c(2, 1, 3)
  ),
  vertex.size = 70
)

p_values <- runif(m, 0, alpha)
corr <- matrix(0.5, m, m)
diag(corr) <- 1

test_results <- graph_test_closure(
  dunnett_graph,
  p = p_values, alpha = alpha,
  test_types = "parametric",
  test_corr = list(corr)
)

test_results$outputs$rejected

