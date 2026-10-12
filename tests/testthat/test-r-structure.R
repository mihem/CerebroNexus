test_that("R contracts tolerate formatting without accepting changed behavior", {
  compact <- "observe({ if (is.null(x)) return(NULL); input[['sidebar']] }, priority = 100)"
  formatted <- paste(
    "observe(",
    "  {",
    "    # A formatted early return.",
    "    if (is.null(x)) { return(NULL) }",
    '    input[["sidebar"]]',
    "  },",
    "  priority = 100",
    ")",
    sep = "\n"
  )
  for (source in c(compact, formatted)) {
    observer <- r_test_observer(source, "observe", quote(input[["sidebar"]]))
    expect_identical(observer$priority, 100)
    expect_true(r_test_has_call(observer, quote(if (is.null(x)) return(NULL))))
    expect_false(r_test_has_call(
      observer,
      quote(if (!is.null(x)) return(NULL))
    ))
    expect_false(r_test_has_call(observer, quote(input[["other_page"]])))
  }
  changed_priority <- r_test_calls(sub("100", "0", formatted), "observe")[[1L]]
  expect_false(identical(changed_priority$priority, 100))
  expect_false(r_test_has_call(
    "if (is.null(x)) { force_data(); return(NULL) }",
    quote(if (is.null(x)) return(NULL))
  ))
})

test_that("R contract traversal preserves NULL and omitted arguments", {
  source <- "f <- function(x, y = NULL) { g(x[, 1], y = NULL) }"
  expect_true(r_test_has_call(source, quote(g(x[, 1], y = NULL))))
  expect_false(r_test_has_call(source, quote(g(x[, 2], y = NULL))))
  expect_false(r_test_has_call(source, quote(g(x[, 1]))))
})
