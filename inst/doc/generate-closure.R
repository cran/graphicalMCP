## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  fig.align = "center",
  echo = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----setup, results="hide", message=FALSE, warning=FALSE----------------------
library(graphicalMCP)
library(lrstat)
library(gMCP)

library(ggplot2)
library(bench)
library(gt)

## ----base-graph-1, fig.dim=c(3, 3)--------------------------------------------
ss_graph <- simple_successive_2()

plot(ss_graph, layout = "grid", vertex.size = 60)

## ----closure-intersections----------------------------------------------------
weighting_strategy <- graph_generate_weights(ss_graph)
matrix_intersections <- weighting_strategy[, seq_along(ss_graph$hypotheses)]
matrix_intersections

## ----closure-weights----------------------------------------------------------
matrix_weights <- weighting_strategy[, -seq_along(ss_graph$hypotheses)]
matrix_weights

## ----plot-gw-benchmarks, fig.dim=c(8, 5), eval=TRUE---------------------------
benchmarks <- read.csv("gw_benchmarks.csv")
benchmarks <- subset(benchmarks, char_expression != "graphicalMCP recursive")

benchmarks$char_expression <- factor(benchmarks$char_expression,
  levels = c(
    "gMCP",
    "graphicalMCP simple",
    "graphicalMCP parent-child",
    "lrstat"
  )
)

benchmarks_plot_standard <-
  ggplot(benchmarks, aes(num_hyps, median, color = char_expression)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1) +
  # scale_y_continuous(labels = scales::label_comma(suffix = "ms")) +
  scale_color_discrete() +
  labs(
    title = "Computing time to generate weighting strategies",
    subtitle = "Median runtime in milliseconds",
    x = "Number of hypotheses",
    y = NULL,
    color = "Approach"
  ) +
  scale_y_log10(
    breaks = c(0, 0.1, 1, 10, 100, 1000, 100000),
    labels = c("0", "0.1", "1", "10", "100", "1,000", "100,000")
  ) +
  labs(subtitle = "Log10(median runtime) in milliseconds")

benchmarks_plot_standard

## ----plot-power-benchmarks, fig.dim=c(8, 5), eval=TRUE------------------------
benchmarks_holm <- read.csv("power_benchmarks_holm.csv")
benchmarks_fixed_sequence <- read.csv("power_benchmarks_fixed_sequence.csv")
benchmarks <- data.frame(
  rbind(benchmarks_holm, benchmarks_fixed_sequence),
  Procedure = rep(c("Holm", "Fixed sequence"), each = nrow(benchmarks_holm))
)
benchmarks$char_expression <- factor(benchmarks$char_expression,
  levels = c(
    "gMCP (C)",
    "graphicalMCP conventional (R)",
    "graphicalMCP parent-child (R)"
  )
)
benchmarks$Procedure <- factor(benchmarks$Procedure,
  levels = c(
    "Holm",
    "Fixed sequence"
  )
)
benchmarks_plot_standard <-
  ggplot(benchmarks, aes(num_hyps, median, color = char_expression)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1) +
  # scale_y_continuous(labels = scales::label_comma(suffix = "ms")) +
  scale_color_discrete() +
  facet_wrap(~Procedure, labeller = label_both) +
  labs(
    title = "Computing time of power simulations",
    subtitle = "Median runtime in seconds",
    x = "Number of hypotheses",
    y = NULL,
    color = "Approach"
  ) +
  scale_y_log10(
    breaks = c(0, 0.1, 0.3, 1, 3, 9, 27, 81, 243),
    labels = c("0", "0.1", "0.3", "1", "3", "9", "27", "81", "243")
  ) +
  labs(subtitle = "Log10(median runtime) in seconds 2^14=16,384 simulations")
benchmarks_plot_standard

## ----gw-benchmarks-functions, eval=FALSE--------------------------------------
#  ggw_simple <- function(graph) {
#    num_hyps <- length(graph$hypotheses)
#  
#    matrix_intersections <-
#      as.matrix(rev(expand.grid(rep(list(1:0), num_hyps))[-2^num_hyps, ]))
#    colnames(matrix_intersections) <- names(graph$hypotheses)
#  
#    matrix_weights <- apply(
#      matrix_intersections,
#      1,
#      function(h) graph_update(graph, !h)$updated_graph$hypotheses,
#      simplify = FALSE
#    )
#  
#    cbind(matrix_intersections, do.call(rbind, matrix_weights))
#  }
#  
#  vec_num_hyps <- 2:8 * 2
#  
#  benchmark_list <- lapply(
#    vec_num_hyps,
#    function(num_hyps) {
#      cat(num_hyps, "\n")
#      # A graph for the Holm procedure
#      transitions <- matrix(1 / (num_hyps - 1), num_hyps, num_hyps)
#      diag(transitions) <- 0
#  
#      graph <- graph_create(rep(1 / num_hyps, num_hyps), transitions)
#  
#      # lrstat sometimes errors, even when hypotheses seem to sum to 1. This
#      # fixes some of those cases
#      graph$hypotheses <- c(
#        graph$hypotheses[seq_len(num_hyps - 1)],
#        1 - sum(graph$hypotheses[seq_len(num_hyps - 1)])
#      )
#  
#      benchmark <- mark(
#        gMCP = generateWeights(graph$transitions, graph$hypotheses),
#        `graphicalMCP simple` = ggw_simple(graph),
#        `graphicalMCP parent-child` = graph_generate_weights(graph),
#        # lrstat still errors with graphs occasionally for unknown reasons
#        lrstat = if (!num_hyps %in% c(10, 14)) {
#          fwgtmat(graph$hypotheses, graph$transitions)
#        },
#        check = FALSE,
#        memory = FALSE,
#        time_unit = "ms",
#        min_iterations = 5
#      )[, 1:5]
#  
#      # Remove rows for lrstat that weren't actually run
#      benchmark <- benchmark[benchmark$median > 0.005, ]
#      benchmark$char_expression <- as.character(benchmark$expression)
#      benchmark <- benchmark[, c("char_expression", "median")]
#  
#      cbind(data.frame(num_hyps = num_hyps), benchmark)
#    }
#  )
#  
#  benchmarks <- do.call(rbind, benchmark_list)
#  
#  benchmarks$char_expression <- ordered(
#    benchmarks$char_expression,
#    c(
#      "gMCP",
#      "graphicalMCP simple",
#      "graphicalMCP recursive",
#      "graphicalMCP parent-child",
#      "lrstat"
#    )
#  )
#  
#  write.csv(
#    benchmarks,
#    here::here("vignettes/cache/gw_benchmarks.csv"),
#    row.names = FALSE
#  )

## ----power-conventional, eval = FALSE-----------------------------------------
#  gcp_conventional <- function(
#      graph,
#      alpha = 0.025,
#      power_marginal = rep(alpha, length(graph$hypotheses)),
#      sim_n = 100,
#      sim_corr = diag(length(graph$hypotheses)),
#      sim_success = NULL,
#      verbose = FALSE) {
#    hyp_names <- names(graph$hypotheses)
#    num_hyps <- length(graph$hypotheses)
#  
#    graphicalMCP:::power_input_val(
#      graph,
#      sim_n,
#      power_marginal,
#      sim_corr,
#      sim_success
#    )
#  
#    # Simulated p-values are generated by sampling from the multivariate normal
#    # distribution. The means are set with `power_marginal`, and the correlations
#    # are set with `sim_corr`. Random samples are converted to p-values with a
#    # one-sided test.
#    noncentrality_parameter <-
#      stats::qnorm(1 - alpha, lower.tail = TRUE) -
#      stats::qnorm(1 - power_marginal, lower.tail = TRUE)
#  
#    p_sim <- stats::pnorm(
#      mvtnorm::rmvnorm(
#        sim_n,
#        noncentrality_parameter,
#        sigma = sim_corr
#      ),
#      lower.tail = FALSE
#    )
#  
#    simulation_test_results <- matrix(
#      NA,
#      nrow = sim_n,
#      ncol = length(power_marginal),
#      dimnames = list(seq_len(sim_n), hyp_names)
#    )
#  
#    for (row in seq_len(sim_n)) {
#      simulation_test_results[row, ] <-
#        graph_test_shortcut(graph, p_sim[row, ], alpha)$outputs$rejected
#    }
#  
#    # Summarize power results ----------------------------------------------------
#    # Each user-defined function provided as a "success" measure should take a
#    # logical vector (a single simulation's test results) as input, and return a
#    # logical scalar. Applying such a function to each simulation, results in a
#    # success indicator vector with one entry per simulation. The average of this
#    # vector is the probability of "success".
#    power_success <- vapply(
#      sim_success,
#      function(fn_success) mean(apply(simulation_test_results, 1, fn_success)),
#      numeric(1)
#    )
#  
#    # If the success functions are not named, set names according to each
#    # function's body
#    if (is.null(names(power_success))) {
#      success_fun_bodies <- vapply(
#        sim_success,
#        function(fn_success) deparse(fn_success)[[2]],
#        character(1)
#      )
#  
#      names(power_success) <- success_fun_bodies
#    }
#  
#    # Power summaries:
#    # * Local power is the probability of rejecting each individual hypothesis:
#    # Mean of results for each hypothesis individually.
#    # * Expected rejections is the total number of rejections divided by the total
#    # possible rejections.
#    # * Power to reject at least one hypothesis is the probability that any result
#    # in a row is TRUE. This one is just like if a success function was defined as
#    # rejecting any hypothesis in the graph
#    # * Power to reject all hypotheses is the mean of a success vector where
#    # success is only triggered when the whole results vector is TRUE
#    power <- list(
#      power_local = colMeans(simulation_test_results),
#      rejection_expected = sum(simulation_test_results) / sim_n,
#      power_at_least_1 = mean(rowSums(simulation_test_results) > 0),
#      power_all =
#        mean(rowSums(simulation_test_results) == length(power_marginal)),
#      power_success = power_success
#    )
#  
#    # The core output of a power report is the 5 power summaries. It also includes
#    # the main testing and simulation input parameters (similar to test results).
#    # For completion, the full matrix of simulations and corresponding matrix of
#    # test results are included. They are truncated in the print method so as to
#    # not blow up output space. It may be preferred for these to be an optional
#    # output with e.g. `verbose = TRUE/FALSE`.
#    structure(
#      list(
#        inputs = list(
#          graph = graph,
#          alpha = alpha,
#          test_groups = NULL,
#          test_types = NULL,
#          test_corr = NULL,
#          sim_n = sim_n,
#          power_marginal = power_marginal,
#          sim_corr = sim_corr,
#          sim_success = sim_success
#        ),
#        power = power,
#        details = if (verbose) {
#          list(
#            p_sim = p_sim,
#            test_results = simulation_test_results
#          )
#        }
#      ),
#      class = "power_report"
#    )
#  }

## ----power-benchmarks-holm, eval = FALSE--------------------------------------
#  vec_num_hyps <- 2:8 * 2
#  
#  benchmark_list <- lapply(
#    vec_num_hyps,
#    function(num_hyps) {
#      # A graph for the Holm procedure
#      transitions <- matrix(1 / (num_hyps - 1), num_hyps, num_hyps)
#      diag(transitions) <- 0
#  
#      graph <- graph_create(rep(1 / num_hyps, num_hyps), transitions)
#  
#      corr <- diag(num_hyps)
#  
#      benchmark <- mark(
#        `gMCP (C)` = gMCP::calcPower(
#          graph$hypotheses,
#          0.025,
#          graph$transitions,
#          n.sim = 2^14,
#          corr.sim = corr
#        ),
#        `graphicalMCP conventional (R)` =
#          gcp_conventional(
#            graph,
#            0.025,
#            power_marginal = rep(.9, num_hyps),
#            sim_n = 2^14
#          ),
#        `graphicalMCP parent-child (R)` =
#          graph_calculate_power(
#            graph,
#            0.025,
#            power_marginal = rep(.9, num_hyps),
#            sim_n = 2^14
#          ),
#        check = FALSE,
#        memory = FALSE,
#        time_unit = "s",
#        min_iterations = 5
#      )[, 1:5]
#  
#      benchmark <- benchmark[benchmark$median > 0.00001, ]
#      benchmark$char_expression <- as.character(benchmark$expression)
#      benchmark <- benchmark[, c("char_expression", "median")]
#  
#      cbind(data.frame(num_hyps = num_hyps), benchmark)
#    }
#  )
#  
#  benchmarks <- do.call(rbind, benchmark_list)
#  
#  benchmarks$char_expression <- ordered(
#    benchmarks$char_expression,
#    c(
#      "gMCP (C)",
#      "graphicalMCP conventional (R)",
#      "graphicalMCP parent-child (R)"
#    )
#  )
#  
#  write.csv(
#    benchmarks,
#    here::here("vignettes/cache/power_benchmarks_holm.csv"),
#    row.names = FALSE
#  )

## ----power-benchmarks-fixed-sequence, eval = FALSE----------------------------
#  vec_num_hyps <- 2:8 * 2
#  
#  benchmark_list <- lapply(
#    vec_num_hyps,
#    function(num_hyps) {
#      # A graph for the fixed sequence procedure
#      graph <- fixed_sequence(num_hyps)
#  
#      corr <- diag(num_hyps)
#  
#      benchmark <- mark(
#        `gMCP (C)` = gMCP::calcPower(
#          graph$hypotheses,
#          0.025,
#          graph$transitions,
#          n.sim = 2^14,
#          corr.sim = corr
#        ),
#        `graphicalMCP conventional (R)` =
#          gcp_conventional(
#            graph,
#            0.025,
#            power_marginal = rep(.9, num_hyps),
#            sim_n = 2^14
#          ),
#        `graphicalMCP parent-child (R)` =
#          graph_calculate_power(
#            graph,
#            0.025,
#            power_marginal = rep(.9, num_hyps),
#            sim_n = 2^14
#          ),
#        check = FALSE,
#        memory = FALSE,
#        time_unit = "s",
#        min_iterations = 5
#      )[, 1:5]
#  
#      benchmark <- benchmark[benchmark$median > 0.00001, ]
#      benchmark$char_expression <- as.character(benchmark$expression)
#      benchmark <- benchmark[, c("char_expression", "median")]
#  
#      cbind(data.frame(num_hyps = num_hyps), benchmark)
#    }
#  )
#  
#  benchmarks <- do.call(rbind, benchmark_list)
#  
#  benchmarks$char_expression <- ordered(
#    benchmarks$char_expression,
#    c(
#      "gMCP (C)",
#      "graphicalMCP conventional (R)",
#      "graphicalMCP parent-child (R)"
#    )
#  )
#  
#  write.csv(
#    benchmarks,
#    here::here("vignettes/cache/power_benchmarks_fixed_sequence.csv"),
#    row.names = FALSE
#  )

