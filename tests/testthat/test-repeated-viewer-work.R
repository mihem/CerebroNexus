repeat_definitions <- function(file, env) {
  for (x in parse(file)) {
    if (
      is.call(x) &&
        as.character(x[[1]])[1] %in% c("<-", "=") &&
        is.call(x[[3]]) &&
        identical(x[[3]][[1]], as.name("function"))
    ) {
      eval(x, env)
    }
  }
}

test_that("IR encoding and compact profiles follow semantic data changes only", {
  shiny::testServer(
    function(input, output, session) {
      data_set <- shiny::reactiveVal(1)
      getImmuneRepertoire <- function() {
        data_set()
        list(
          A = data.frame(
            barcode = c("a", "b", "c"),
            CTstrict = c("x", "x", "y")
          ),
          B = data.frame(barcode = "d", CTstrict = "z")
        )
      }
      getMetaData <- function() {
        data.frame(
          cell_barcode = letters[1:4],
          severity = rep(c("low", "high"), 2),
          patient = c("p", "p", "q", "q")
        )
      }
      viewerPackCurrent <- function() NULL
      sys.source(viewer_test_path("clone_contract.R"), environment())
      sys.source(
        viewer_test_path("immune_repertoire", "encode.R"),
        environment()
      )
      sys.source(viewer_test_path("immune_repertoire", "data.R"), environment())
      counters <- new.env()
      counters$encodings <- 0L
      counters$preparations <- 0L
      encode_original <- ir_encode_repertoire
      profile_original <- ir_clone_size_profiles
      ir_encode_repertoire <- function(...) {
        counters$encodings <- counters$encodings + 1L
        encode_original(...)
      }
      ir_clone_size_profiles <- function(...) {
        counters$preparations <- counters$preparations + 1L
        profile_original(...)
      }
    },
    {
      session$setInputs(
        ir_tabs = "Diversity",
        ir_groupBy = "severity",
        ir_chain = "both",
        ir_p_sd_threshold = 1
      )
      profile <- ir_size_distribution_profile()
      expect_equal(counters$encodings, 1L)
      expect_equal(counters$preparations, 1L)
      expect_null(profile$clone_counts)
      expect_identical(rownames(profile$profile), c("high", "low"))
      for (tab in c("Homeostasis", "Abundance", "Diversity")) {
        session$setInputs(
          ir_tabs = tab,
          ir_cloneCall = "aa",
          ir_p_sd_method = "average"
        )
        expect_identical(ir_size_distribution_profile(), profile)
      }
      expect_equal(counters$encodings, 1L)
      expect_equal(counters$preparations, 1L)
      ## Another grouping reads its column at the rows' cells; the encoding
      ## stays.
      session$setInputs(ir_groupBy = "patient")
      expect_identical(
        rownames(ir_size_distribution_profile()$profile),
        c("p", "q")
      )
      expect_equal(counters$encodings, 1L)
      expect_equal(counters$preparations, 2L)
      session$setInputs(ir_p_sd_threshold = 2)
      expect_equal(ir_size_distribution_profile()$threshold, 2L)
      expect_equal(counters$preparations, 3L)
      data_set(2)
      session$flushReact()
      ir_size_distribution_profile()
      expect_equal(counters$encodings, 2L)
      expect_equal(counters$preparations, 4L)
    }
  )
})

test_that("IR non-select controls preserve valid values including false and empty", {
  env <- new.env(parent = asNamespace("shiny"))
  repeat_definitions(viewer_test_path("immune_repertoire", "settings.R"), env)
  source(viewer_test_path("immune_repertoire", "param_spec.R"), local = env)
  env$input <- list(ir_tabs = "Homeostasis", ir_p_clone_size = "custom")
  expect_match(
    as.character(env$ir_analysis_panel()),
    'value="custom"',
    fixed = TRUE
  )
  env$input$ir_p_clone_size <- ""
  expect_match(as.character(env$ir_analysis_panel()), 'value=""', fixed = TRUE)
  env$input <- list(ir_tabs = "Diversity", ir_p_n_boots = 100)
  expect_match(
    as.character(env$ir_analysis_panel(TRUE)),
    'value="100"',
    fixed = TRUE
  )
  env$input$ir_p_n_boots <- Inf
  expect_match(
    as.character(env$ir_analysis_panel(TRUE)),
    'value="20"',
    fixed = TRUE
  )
  env$input <- list(ir_tabs = "Abundance", ir_p_scale = TRUE)
  expect_match(as.character(env$ir_analysis_panel()), 'checked', fixed = TRUE)
  env$input$ir_p_scale <- FALSE
  expect_no_match(
    as.character(env$ir_analysis_panel()),
    'checked',
    fixed = TRUE
  )
})

test_that("Groups shares counts across display forms but refreshes data", {
  shiny::testServer(
    function(input, output, session) {
      counter <- new.env()
      counter$calls <- 0L
      generation <- shiny::reactiveVal(1)
      getGroups <- function() c("a", "b")
      repeat_definitions(
        viewer_test_path("groups", "composition.R"),
        environment()
      )
      for (name in c("calculateTableAB", "formatTableAB")) {
        eval(
          viewer_r_assignment(viewer_test_path("utility_functions.R"), name),
          environment()
        )
      }
      groupsMetadataColumns <- function(...) {
        generation()
        counter$calls <- counter$calls + 1L
        data.frame(a = factor(c("x", "x", "y")), b = factor(c("p", "q", "p")))
      }
      expressions <- parse(viewer_test_path("groups", "composition.R"))
      for (x in expressions) {
        if (
          is.call(x) &&
            as.character(x[[1]])[1] %in% c("<-", "=") &&
            identical(x[[2]], as.name("groups_composition_counts"))
        ) {
          eval(x, environment())
        }
      }
    },
    {
      session$setInputs(
        groups_selected_group = "a",
        groups_by_other_group_second_group = "b"
      )
      counts <- groupsCompositionTable("long", FALSE)
      percent <- groupsCompositionTable("long", TRUE)
      wide <- groupsCompositionTable("wide", FALSE)
      expect_equal(counter$calls, 1L)
      expect_equal(counts$count, c(1L, 1L, 1L))
      expect_equal(percent$count, c(.5, .5, 1))
      expect_equal(wide$p, c(1L, 1L))
      generation(2)
      session$flushReact()
      groupsCompositionTable("long", FALSE)
      expect_equal(counter$calls, 2L)
    }
  )
})

test_that("compact SizeDist renders the identical geometry", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggdendro")
  env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("immune_repertoire", "encode.R"), env)
  repeat_definitions(viewer_test_path("immune_repertoire", "data.R"), env)
  raw <- list(
    A = data.frame(CTstrict = c("x", "x", "y")),
    B = data.frame(CTstrict = c("q", "r"))
  )
  full_profiles <- env$ir_clone_size_profiles(env$ir_encode_repertoire(raw))
  profiles <- full_profiles
  profiles$clone_counts <- NULL
  for (method in c("ward.D2", "complete", "average")) {
    expected <- ggplot2::ggplot_build(env$ir_size_profile_plot(
      full_profiles,
      method
    ))$data
    actual <- ggplot2::ggplot_build(env$ir_size_profile_plot(
      profiles,
      method
    ))$data
    expect_equal(actual, expected)
  }
})

test_that("table classification is skipped only when neither feature uses it", {
  env <- new.env(parent = globalenv())
  repeat_definitions(viewer_test_path("utility_functions.R"), env)
  env$`%>%` <- dplyr::`%>%`
  for (name in c(
    "findColumnsInteger",
    "findColumnsMatching"
  )) {
    env[[name]] <- function(...) stop("classification requested")
  }
  frame <- data.frame(value = c(1.2, 2.3), p_val = c(.01, .02))
  expect_s3_class(
    env$prettifyTable(
      frame,
      filter = "none",
      dom = "t",
      number_formatting = FALSE,
      color_highlighting = FALSE
    ),
    "datatables"
  )
  expect_error(
    env$prettifyTable(
      frame,
      number_formatting = TRUE,
      color_highlighting = FALSE
    ),
    "classification requested"
  )
  expect_error(
    env$prettifyTable(
      frame,
      number_formatting = FALSE,
      color_highlighting = TRUE
    ),
    "classification requested"
  )
})

test_that("the faceted paired scatter compares levels of the grouping column", {
  env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("immune_repertoire", "encode.R"), env)
  repeat_definitions(viewer_test_path("immune_repertoire", "data.R"), env)
  repeat_definitions(
    viewer_test_path("immune_repertoire", "visualizations.R"),
    env
  )
  env$IR_PALETTE <- "inferno"
  encoded <- env$ir_encode_repertoire(list(
    s1 = data.frame(
      barcode = c("a", "b", "c"),
      CTgene = c("x", "x", "y"),
      cell_type = c("T", "T", "B")
    ),
    s2 = data.frame(
      barcode = c("d", "e"),
      CTgene = c("x", "z"),
      cell_type = c("B", "T")
    )
  ))
  ## X and Y are cell types, not samples.
  table <- env$ir_scatter_table(
    encoded,
    "CTgene",
    "both",
    env$ir_row_values(encoded, "cell_type", NULL),
    "T",
    "B"
  )
  expect_identical(table$x_n, c(2, 1, 0))
  expect_identical(table$y_n, c(1, 0, 1))
  plot <- env$ir_scatter_ggplot(table, "T", "B", "proportion", "T vs B")
  points <- ggplot2::ggplot_build(plot)$data[[1]]
  expect_identical(nrow(points), 3L)
  expect_identical(plot$labels$title, "T vs B")
})
