test_that("printing Bonferroni/Simes closure test", {
  par_gate <- simple_successive_1()

  expect_snapshot(graph_test_closure(par_gate, rep(.01, 4), test_types = "s"))

  expect_snapshot(graph_test_closure(par_gate, rep(.01, 4), verbose = TRUE))

  expect_snapshot(graph_test_closure(par_gate, rep(.01, 4), test_values = TRUE))
})

test_that("printing parametric closure test", {
  par_gate <- simple_successive_1()

  expect_snapshot(
    graph_test_closure(
      par_gate,
      rep(.01, 4),
      test_types = "p",
      test_corr = list(diag(4))
    )
  )

  expect_snapshot(
    graph_test_closure(
      par_gate,
      rep(.01, 4),
      test_groups = list(1:2, 3:4),
      test_types = c("p", "s"),
      test_corr = list(diag(2), NA),
      test_values = TRUE,
      verbose = TRUE
    )
  )

  expect_snapshot(
    graph_test_closure(
      par_gate,
      rep(.01, 4),
      test_groups = list(1:2, 3:4),
      test_types = c("p", "p"),
      test_corr = list(diag(2), diag(2)),
      test_values = TRUE,
      verbose = TRUE
    )
  )
})

test_that("printing Bonferroni sequential results", {
  expect_snapshot(graph_test_shortcut(simple_successive_1(), rep(.01, 4)))

  expect_snapshot(
    graph_test_shortcut(simple_successive_1(), rep(.01, 4), verbose = TRUE)
  )
})

test_that("add alternate orderings", {
  test_res <-
    graph_test_shortcut(simple_successive_1(), rep(.01, 4), verbose = TRUE)

  test_res_alt <- graph_rejection_orderings(test_res)

  expect_snapshot(test_res_alt)
})

test_that("additional printing options for graph report", {
  par_gate <- simple_successive_1()

  expect_snapshot(
    print(
      graph_test_closure(
        par_gate,
        rep(.01, 4),
        verbose = TRUE,
        test_values = TRUE
      ),
      precison = 4,
      indent = 4
    )
  )

  expect_snapshot(
    print(
      graph_test_shortcut(
        simple_successive_1(),
        rep(.01, 4),
        verbose = TRUE,
        test_values = TRUE
      ),
      precision = 7,
      indent = 9
    )
  )

  expect_snapshot(
    print(
      graph_test_shortcut(
        two_doses_two_primary_two_secondary(),
        5:0 / 200,
        verbose = TRUE,
        test_values = TRUE
      )
    )
  )

  expect_snapshot(
    print(
      graph_rejection_orderings(
        graph_test_shortcut(
          two_doses_two_primary_two_secondary(),
          6:1 / 400,
          verbose = TRUE,
          test_values = TRUE
        )
      )
    )
  )
})
