## Slow Builder integration tests (app sourcing): CI only.
skip_on_cran()

## The colour picker and the alignment legend are written as one HTML string
## for speed. These tests compare them with the htmltools tag versions they
## replaced, kept below as the reference.

builder_ui_html_app <- function(scope = parent.frame()) {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_options(
    cerebro.builder.precompile = FALSE,
    .local_envir = scope
  )
  withr::local_dir(builder_profile_inst_path("builder"), .local_envir = scope)
  sys.source("app.R", envir = app_env)
  app_env
}

builder_ui_html_text <- function(tag) {
  html <- as.character(htmltools::renderTags(tag)$html)
  gsub(">\\s+<", "><", trimws(html))
}

builder_ui_html_reference_rows <- function(app_env, id, model) {
  ns <- shiny::NS(id)
  largest <- max(c(
    vapply(model$items, function(item) as.numeric(item$count), numeric(1)),
    1
  ))
  lapply(model$items, function(item) {
    input_id <- ns(paste0("group_color_", item$index))
    width <- max(3, 100 * item$count / largest)
    htmltools::div(
      class = "group-color-item viewer-group-distribution-row",
      `data-search` = tolower(item$label),
      htmltools::tags$input(
        id = input_id,
        type = "color",
        class = "group-color-input",
        value = item$color,
        `data-input-id` = ns("group_color"),
        `data-group` = model$group,
        `data-level` = item$key,
        `aria-label` = paste("Color for", model$group, item$label)
      ),
      htmltools::tags$label(
        class = "group-color-name",
        `for` = input_id,
        title = item$label,
        item$label
      ),
      htmltools::span(
        class = "viewer-group-bar-track",
        htmltools::span(
          class = "viewer-group-bar",
          style = paste0("width:", round(width, 2L), "%")
        )
      ),
      htmltools::span(
        class = "viewer-group-count",
        format(item$count, big.mark = ",", scientific = FALSE)
      )
    )
  })
}

test_that("the colour picker rows equal the tag version, escaping included", {
  app_env <- builder_ui_html_app()
  levels <- c("B cells", "<b>bold</b>", "a & b", "quote \" and 'single'", "T")
  model <- app_env$builder_group_colors_model(
    group = "cell type <x>",
    levels = levels,
    palette = "cerebro",
    overrides = list(),
    distribution = lapply(
      seq_along(levels),
      function(i) {
        list(value = levels[[i]], count = c(1234567, 100000, 3, 0, 42)[[i]])
      }
    )
  )
  ui <- app_env$builder_group_colors_ui("datasets", model)
  grid <- htmltools::tagQuery(ui)$find(".group-color-grid")$selectedTags()[[1L]]
  expect_identical(
    builder_ui_html_text(grid$children),
    builder_ui_html_text(htmltools::tagList(
      builder_ui_html_reference_rows(app_env, "datasets", model)
    ))
  )
  html <- builder_ui_html_text(grid$children)
  expect_match(html, "&lt;b&gt;bold&lt;/b&gt;", fixed = TRUE)
  expect_false(grepl("<b>bold</b>", html, fixed = TRUE))
  ## Whole counts are written out, never in scientific notation.
  expect_match(html, ">100,000<", fixed = TRUE)
  expect_match(html, ">1,234,567<", fixed = TRUE)

  empty <- app_env$builder_group_colors_model("g", character())
  expect_match(
    builder_ui_html_text(app_env$builder_group_colors_ui("datasets", empty)),
    "is-empty",
    fixed = TRUE
  )
})

test_that("the alignment legend equals the tag version", {
  app_env <- builder_ui_html_app()
  frame <- data.frame(
    group = c("<i>x</i>", "a & b", "a & b", "plain", "plain", "plain"),
    stringsAsFactors = FALSE
  )
  colors <- c(plain = "#123456")
  counts <- sort(table(as.character(frame$group)), decreasing = TRUE)
  palette <- app_env$builder_level_colors(names(counts))
  palette["plain"] <- colors[["plain"]]
  reference <- htmltools::tags$div(
    class = "spatial-alignment-legend",
    role = "list",
    lapply(names(counts), function(level) {
      htmltools::tags$span(
        class = "spatial-alignment-legend-item",
        role = "listitem",
        htmltools::tags$i(
          class = "spatial-alignment-legend-swatch",
          style = paste0("background:", palette[[level]])
        ),
        htmltools::tags$span(level),
        htmltools::tags$small(format(unname(counts[[level]]), big.mark = ","))
      )
    })
  )
  expect_identical(
    builder_ui_html_text(app_env$builder_alignment_legend_ui(frame, colors)),
    builder_ui_html_text(reference)
  )
  expect_null(app_env$builder_alignment_legend_ui(frame[0L, , drop = FALSE]))
})
