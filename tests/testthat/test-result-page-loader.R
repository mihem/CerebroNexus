test_that("result page completion follows local draws and preserves paint gates", {
  node <- skip_if_no_node()
  result <- system2(
    node,
    shQuote(c(
      test_path("fixtures", "result-page-loader.js"),
      viewer_test_path("www", "viewer-shell.js"),
      viewer_test_path("groups", "UI.R")
    )),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
  expect_match(
    paste(result, collapse = "\n"),
    "result page loader assertions passed",
    fixed = TRUE
  )
})

test_that("stored table controls include the first subgroup without another output", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), scope)
  results <- data.frame(
    cluster = factor(c("b", "a"), levels = c("a", "b", "unused")),
    gene = c("X", "Y")
  )
  for (prefix in c("marker_genes", "enriched_pathways")) {
    html <- as.character(scope$cerebroResultTableControls(
      prefix,
      results,
      TRUE
    ))
    expect_match(
      html,
      paste0('id="', prefix, '_table_select_group_level"'),
      fixed = TRUE
    )
    expect_match(html, 'value="a" selected', fixed = TRUE)
    expect_match(html, 'value="unused"', fixed = TRUE)
    expect_match(
      html,
      paste0('!input.', prefix, '_table_filter_switch'),
      fixed = TRUE
    )
    expect_no_match(html, paste0(prefix, '_filter_subgroups_UI'), fixed = TRUE)
    expect_identical(
      scope$cerebroFilterResultRows(results, FALSE, TRUE, "a"),
      results[2L, , drop = FALSE]
    )
    expect_identical(
      scope$cerebroFilterResultRows(results, TRUE, TRUE, "a"),
      results
    )
    ungrouped <- as.character(scope$cerebroResultTableControls(
      prefix,
      results,
      FALSE
    ))
    expect_no_match(
      ungrouped,
      paste0('id="', prefix, '_table_select_group_level"'),
      fixed = TRUE
    )
  }
})
