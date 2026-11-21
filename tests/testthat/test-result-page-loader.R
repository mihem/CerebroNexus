test_that("result page completion follows local draws and preserves paint gates", {
  expect_node_fixture(
    "result-page-loader.js",
    viewer_test_path("www", "viewer-shell.js"),
    viewer_test_path("groups", "UI.R"),
    pass = "result page loader assertions passed"
  )
})

test_that("stored table controls include the first subgroup without another output", {
  scope <- viewer_runtime_env()
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

test_that("cards, spinners and dimming wait the same time before they show", {
  shell <- paste(
    readLines(viewer_test_path("www", "viewer-shell.js")),
    collapse = "\n"
  )
  css <- paste(
    readLines(viewer_test_path("www", "custom.css")),
    collapse = "\n"
  )
  js_ms <- as.numeric(sub(".*WAIT_SHOW_MS = ([0-9]+);.*", "\\1", shell))
  css_ms <- as.numeric(sub(".*--wait-delay: ([0-9]+)ms;.*", "\\1", css))
  expect_identical(js_ms, css_ms)
  ## progress card, plot spinner, empty-slot spinner and empty-slot text
  uses <- gregexpr("var(--wait-delay)", css, fixed = TRUE)[[1]]
  expect_gte(length(uses), 4L)
})
