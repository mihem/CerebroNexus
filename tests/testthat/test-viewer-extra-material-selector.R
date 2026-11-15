test_that("changing workbooks refreshes sheet choices and retains their display names", {
  source_path <- viewer_test_path("extra_material", "select_content.R")
  server <- function(input, output, session) {
    getExtraMaterialCategories <- function() "tables"
    checkForExtraTables <- function() TRUE
    extra_material_table_groups <- function() {
      list(
        book_a = list(
          sheets = list(
            list(key = "a", label = "QA CAM renamed"),
            list(key = "a2", label = "A second")
          )
        ),
        book_b = list(
          sheets = list(
            list(key = "b", label = "CAM original"),
            list(key = "b2", label = "B second")
          )
        )
      )
    }
    extra_material_table_choices <- function(groups) {
      c("QA workbook" = "book_a", "Original workbook" = "book_b")
    }
    extra_material_table_selection <- function(groups, file_key, load) {
      list(group = groups[[file_key]])
    }
    sys.source(source_path, envir = environment())
  }
  shiny::testServer(server, {
    session$setInputs(
      extra_material_selected_category = "tables",
      extra_material_selected_file = "book_a"
    )
    first <- paste(
      unlist(output$extra_material_selected_content_UI),
      collapse = ""
    )
    expect_match(first, "QA CAM renamed", fixed = TRUE)
    session$setInputs(extra_material_selected_file = "book_b")
    second <- paste(
      unlist(output$extra_material_selected_content_UI),
      collapse = ""
    )
    expect_match(second, "CAM original", fixed = TRUE)
    expect_false(grepl("QA CAM renamed", second, fixed = TRUE))
    session$setInputs(extra_material_selected_file = "book_a")
    expect_match(
      paste(unlist(output$extra_material_selected_content_UI), collapse = ""),
      "QA CAM renamed",
      fixed = TRUE
    )
  })
})

test_that("embedded table display labels preserve identity and dataset isolation", {
  runtime <- viewer_runtime_env()
  tables <- list(
    "old-one" = data.frame(value = 11L),
    "old-two" = data.frame(value = 22L),
    legacy = data.frame(value = 33L)
  )
  labels <- list(
    "old-one" = list(
      workbook_name = "Clinical results",
      display_name = "Patients"
    ),
    "old-two" = list(
      workbook_name = "Validation results",
      display_name = "Patients"
    ),
    absent = list(workbook_name = "Unused", display_name = "Unused")
  )
  groups <- runtime$extra_material_table_groups(NULL, tables, labels)
  expect_identical(
    unname(vapply(groups, `[[`, character(1), "label")),
    c("Clinical results", "Validation results", "Embedded tables")
  )
  expect_identical(groups[[1L]]$sheets[[1L]]$label, "Patients")
  expect_identical(groups[[2L]]$sheets[[1L]]$label, "Patients")
  expect_length(unique(vapply(groups, `[[`, character(1), "key")), 3L)
  selected <- runtime$extra_material_table_selection(
    groups,
    file_key = groups[[2L]]$key,
    sheet_key = "embedded:2"
  )
  expect_identical(selected$sheet$table$value, 22L)
  unnamed <- runtime$extra_material_table_groups(NULL, unname(tables))
  expect_identical(
    vapply(unnamed[[1L]]$sheets, `[[`, character(1), "label"),
    paste("Table", 1:3)
  )
  crb <- Cerebro$new()
  for (name in names(tables)) {
    crb$addExtraTable(name, tables[[name]])
  }
  runtime$Cerebro.options <- list(
    viewer_content = list(a = list(extra_material_table_index = labels))
  )
  dataset <- "a"
  runtime$viewer_current_dataset_key <- function() dataset
  runtime$data_set <- function() crb
  expect_length(runtime$extra_material_table_groups(), 3L)
  dataset <- "b"
  legacy <- runtime$extra_material_table_groups()
  expect_identical(legacy[[1L]]$key, "embedded")
  expect_identical(
    vapply(legacy[[1L]]$sheets, `[[`, character(1), "label"),
    names(tables)
  )
  crb$addTechnicalInfo("extra_material_table_index", labels)
  expect_identical(
    runtime$extra_material_table_groups()[[1L]]$label,
    "Clinical results"
  )
  runtime$Cerebro.options$viewer_content$b <- list(
    extra_material_table_index = list(
      "old-one" = list(workbook_name = "App override")
    )
  )
  overridden <- runtime$extra_material_table_groups()
  expect_identical(overridden[[1L]]$label, "App override")
  expect_identical(overridden[[1L]]$sheets[[1L]]$label, "Patients")
  expect_identical(overridden[[2L]]$label, "Validation results")
})

test_that("single-sheet workbooks still show their table display names", {
  source_path <- viewer_test_path("extra_material", "select_content.R")
  server <- function(input, output, session) {
    getExtraMaterialCategories <- function() "tables"
    checkForExtraTables <- function() TRUE
    extra_material_table_groups <- function() {
      list(
        book_a = list(sheets = list(list(key = "a", label = "QA CAM renamed"))),
        book_b = list(sheets = list(list(key = "b", label = "CAM original")))
      )
    }
    extra_material_table_choices <- function(groups) {
      c("QA workbook" = "book_a", "Original workbook" = "book_b")
    }
    extra_material_table_selection <- function(groups, file_key, load) {
      list(group = groups[[file_key]])
    }
    sys.source(source_path, envir = environment())
  }
  shiny::testServer(server, {
    session$setInputs(
      extra_material_selected_category = "tables",
      extra_material_selected_file = "book_a"
    )
    first <- paste(
      unlist(output$extra_material_selected_content_UI),
      collapse = ""
    )
    expect_match(first, "QA CAM renamed", fixed = TRUE)
    session$setInputs(extra_material_selected_file = "book_b")
    second <- paste(
      unlist(output$extra_material_selected_content_UI),
      collapse = ""
    )
    expect_match(second, "CAM original", fixed = TRUE)
    expect_false(grepl("QA CAM renamed", second, fixed = TRUE))
    session$setInputs(extra_material_selected_file = "book_a")
    expect_match(
      paste(unlist(output$extra_material_selected_content_UI), collapse = ""),
      "QA CAM renamed",
      fixed = TRUE
    )
  })
})

test_that("the first Extra material table needs no bound control", {
  utilities <- viewer_test_path("utility_functions.R")
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      renders <- 0L
      shown <- NULL
      getExtraMaterialCategories <- function() "tables"
      checkForExtraTables <- function() TRUE
      checkForExtraPlots <- function() FALSE
      extra_material_table_groups <- function() {
        sheet <- function(key) {
          list(key = key, label = key, table = data.frame(value = key))
        }
        list(
          list(
            key = "book_a",
            label = "A",
            sheets = list(sheet("a1"), sheet("a2"))
          ),
          list(key = "book_b", label = "B", sheets = list(sheet("b1")))
        )
      }
      for (name in c(
        "extra_material_table_choices",
        "extra_material_table_group",
        "extra_material_table_sheet",
        "extra_material_table_selection"
      )) {
        eval(viewer_r_assignment(utilities, name), scope)
      }
      extra_material_table_filter <- function(...) "none"
      cerebroRegisterInfo <- function(...) invisible(NULL)
      cerebroResultTable <- function(results_df, ...) {
        scope$renders <- scope$renders + 1L
        scope$shown <- list(data = results_df, ...)
        DT::datatable(results_df)
      }
      viewer_control_helpers_load(scope)
      sys.source(viewer_test_path("extra_material", "select_content.R"), scope)
      sys.source(viewer_test_path("extra_material", "content.R"), scope)
    },
    {
      invisible(output$extra_material_table)
      expect_identical(scope$renders, 1L)
      expect_identical(scope$shown$data$value, "a1")
      expect_true(scope$shown$number_formatting)
      expect_true(scope$shown$color_highlighting)
      ## The controls bind with the values the table was drawn with.
      session$setInputs(
        extra_material_selected_category = "tables",
        extra_material_selected_file = "book_a",
        extra_material_selected_content = "a1",
        extra_material_table_number_formatting = TRUE,
        extra_material_table_color_highlighting = TRUE
      )
      invisible(output$extra_material_table)
      expect_identical(scope$renders, 1L)
      session$setInputs(extra_material_selected_content = "a2")
      invisible(output$extra_material_table)
      expect_identical(scope$renders, 2L)
      expect_identical(scope$shown$data$value, "a2")
    }
  )
})
