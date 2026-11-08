## A renderPlotly body runs with no graphics device open. Code that touches
## graphics while it builds a plot (ggdendro::dendro_data() in the SizeDist tab)
## then opens R's default device, which writes Rplots.pdf into the working
## directory. That fails with "cannot open file 'Rplots.pdf'" when the App
## folder is read-only, as under Shiny Server in a container.

ir_null_device_scope <- function() {
  scope <- ir_scope(
    c("ir_plot_cache_state", "ir_plot_failed", "safeRenderPlot"),
    "server.R"
  )
  scope <- ir_scope(
    c("ir_render_ggplotly", "ir_empty_plotly"),
    "visualizations.R",
    scope = scope
  )
  scope <- ir_scope("ir_size_profile_plot", "data.R", scope = scope)
  ## The shared toolbar is plotting_functions.R's business, not this test's.
  scope$cerebro_plotly_toolbar <- function(fig) fig
  scope
}

ir_size_profiles <- function() {
  profile <- matrix(
    c(
      0.50, 0.30, 0.15, 0.05,
      0.40, 0.35, 0.15, 0.10,
      0.10, 0.20, 0.30, 0.40,
      0.05, 0.25, 0.30, 0.40
    ),
    nrow = 4L,
    byrow = TRUE,
    dimnames = list(paste0("sample_", 1:4), NULL)
  )
  list(profile = profile, threshold = 1)
}

test_that("a plotly renderer builds its plot without writing a graphics file", {
  skip_on_os("windows")
  skip_if_not_installed("ggdendro")
  skip_if_not_installed("plotly")
  skip_if(
    !is.null(grDevices::dev.list()),
    "a graphics device is already open"
  )

  ## A read-only working directory, like the App folder in the container.
  work <- withr::local_tempdir()
  Sys.chmod(work, "0555")
  withr::defer(Sys.chmod(work, "0755"))
  skip_if(
    file.access(work, 2L) == 0L,
    "the working directory stays writable (running as root)"
  )
  withr::local_dir(work)

  scope <- ir_null_device_scope()
  ## safeRenderPlot turns a failure into an empty-state plot, so a returned
  ## figure alone proves nothing: no failure message may have been written.
  messages <- testthat::capture_messages(
    figure <- scope$ir_render_ggplotly(
      scope$ir_size_profile_plot(ir_size_profiles()),
      "clonalSizeDistribution"
    )
  )
  expect_false(any(grepl("IR ERROR", messages, fixed = TRUE)), info = messages)
  expect_s3_class(figure, "plotly")
  expect_gt(length(figure$x$data), 0L)

  ## No file was attempted, and the renderer left no device behind.
  expect_length(list.files(work, all.files = TRUE, no.. = TRUE), 0L)
  expect_null(grDevices::dev.list())
})

test_that("the renderer closes only the device it opened", {
  skip_if_not_installed("ggdendro")
  skip_if_not_installed("plotly")

  scope <- ir_null_device_scope()
  grDevices::pdf(NULL)
  outer <- grDevices::dev.cur()
  withr::defer(
    if (outer %in% grDevices::dev.list()) grDevices::dev.off(outer)
  )

  suppressMessages(
    scope$ir_render_ggplotly(
      scope$ir_size_profile_plot(ir_size_profiles()),
      "clonalSizeDistribution"
    )
  )
  expect_identical(unname(grDevices::dev.list()), unname(outer))
  expect_identical(grDevices::dev.cur(), outer)

  ## It also cleans up when building the plot fails.
  suppressMessages(
    scope$ir_render_ggplotly(stop("disk unavailable"), "clonalSizeDistribution")
  )
  expect_identical(unname(grDevices::dev.list()), unname(outer))
})
