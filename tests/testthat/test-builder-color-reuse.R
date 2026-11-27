## Slow Builder integration tests (app sourcing, worker processes): CI only.
skip_on_cran()

builder_plan_contract_source_runtime(environment())
builder_repo_source("extras.R", local = environment())

builder_color_reuse_item <- function(has_trekker = FALSE, color = "#000000") {
  item <- list(
    id = "a",
    source_snapshot_identity = list(
      available = TRUE,
      object_md5 = strrep("a", 32L)
    ),
    colors = list(cluster = c(A = color)),
    group_color_overrides = if (identical(color, "#000000")) {
      list()
    } else {
      list(cluster = c(A = color))
    },
    color_custom_count = if (identical(color, "#000000")) 0L else 1L
  )
  item$has_trekker <- has_trekker
  item
}

test_that("colours leave the CRB reuse key only for Trekker-free items", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    key <- function(has_trekker, color) {
      builder_plan_reuse_key(builder_color_reuse_item(has_trekker, color))
    }
    expect_identical(key(FALSE, "#000000"), key(FALSE, "#ffffff"))
    # Trekker writes the default group's colours into the CRB.
    expect_false(identical(key(TRUE, "#000000"), key(TRUE, "#ffffff")))
    # An item frozen before the flag existed keeps its colours in the key.
    expect_false(identical(key(NULL, "#000000"), key(NULL, "#ffffff")))

    aligned <- function(color) {
      item <- builder_color_reuse_item(FALSE, color)
      item$trekker_alignment <- list(section_id = "trekker")
      builder_plan_reuse_key(item)
    }
    expect_false(identical(aligned("#000000"), aligned("#ffffff")))
  })
})

test_that("a frozen item records whether its source has Trekker data", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    plan <- builder_freeze_plan(list(entry), tempdir(), make_app = TRUE)
    expect_null(plan$error)
    expect_false(plan$items[[1L]]$has_trekker)

    entry$profile$extras <- c(
      entry$profile$extras %||% list(),
      list(list(key = "trekker", found = TRUE))
    )
    plan <- builder_freeze_plan(list(entry), tempdir(), make_app = TRUE)
    expect_null(plan$error)
    expect_true(plan$items[[1L]]$has_trekker)
  })
})

test_that("selected colour overrides keep valid colours of included levels", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    settings <- list(
      group_color_overrides = list(
        cluster = c(A = "#112233", B = "not-a-colour", Z = "#445566"),
        sample = c(S1 = "#778899")
      )
    )
    levels <- list(cluster = c("A", "B"), sample = c("S1"))
    selected <- .builder_plan_selected_color_overrides(
      settings,
      c("cluster", "condition"),
      levels
    )
    expect_identical(names(selected), "cluster")
    expect_identical(names(selected$cluster), "A")
    expect_identical(
      .builder_plan_selected_color_overrides(settings, character(), levels),
      stats::setNames(list(), character())
    )
  })
})

test_that("a reused Project artifact refreshes every colour field", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")
    builder_repo_source("project.R")

    entry <- builder_task6_entry()
    first <- builder_freeze_plan(list(entry), tempdir(), make_app = TRUE)
    expect_null(first$error)
    item <- first$items[[1L]]
    group <- item$included_groups[[1L]]
    level <- entry$levels[[group]][[1L]]
    expect_true(is.character(level) && nzchar(level))

    entry$settings$group_color_overrides <- stats::setNames(
      list(stats::setNames("#123456", level)),
      group
    )
    fresh <- builder_freeze_plan(list(entry), tempdir(), make_app = TRUE)
    expect_null(fresh$error)
    expect_identical(fresh$items[[1L]]$color_custom_count, 1L)

    crb <- withr::local_tempfile(fileext = ".crb")
    writeLines("crb", crb)
    reused <- entry
    reused$load_state <- "artifact_ready"
    reused$snapshot <- NULL
    reused$project_artifact <- list(plan_item = item, resolved_path = crb)
    again <- builder_freeze_plan(list(reused), tempdir(), make_app = TRUE)
    expect_null(again$error)
    for (field in c("colors", "group_color_overrides", "color_custom_count")) {
      expect_identical(
        again$items[[1L]][[field]],
        fresh$items[[1L]][[field]],
        info = field
      )
    }
  })
})
