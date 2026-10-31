builder_extra_label_roundtrip <- function(root) {
  generated_app_fixture_source_runtime()
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  snapshot_root <- file.path(root, "snapshots")
  dir.create(snapshot_root, recursive = TRUE)
  prepared <- lapply(c("a", "b"), function(id) {
    fixture <- .generated_app_fixture_basic()
    fixture$expected$dataset_name <- paste("Labels", id)
    fixture$builder_settings$name <- fixture$expected$dataset_name
    fixture$object@misc$extra_material <- list(
      tables = list(source_only = data.frame(value = paste("source", id))),
      table_index = list(
        source_only = list(
          workbook_name = paste("Source", id),
          display_name = "Original sheet"
        )
      )
    )
    entry <- .generated_app_e2e_entry(
      paste0("labels-", id),
      fixture,
      snapshot_root
    )
    table <- function(key, book, sheet, value) {
      list(
        name = key,
        file_name = paste0(book, ".xlsx"),
        sheet_name = key,
        workbook_name = paste("Renamed", book, id),
        display_name = sheet,
        table = data.frame(value = value)
      )
    }
    entry$entry$settings$tables <- list(
      stable_one = table(
        "stable_one",
        "markers",
        "Shared sheet name",
        paste(id, "one")
      ),
      stable_two = table(
        "stable_two",
        "markers",
        "Second renamed sheet",
        paste(id, "two")
      ),
      stable_three = table(
        "stable_three",
        "DE",
        "Shared sheet name",
        paste(id, "three")
      )
    )
    entry
  })
  entries <- lapply(prepared, `[[`, "entry")
  snapshots <- lapply(prepared, `[[`, "snapshot")
  names(snapshots) <- vapply(entries, `[[`, character(1), "id")
  plan <- builder_freeze_plan(
    unname(entries),
    file.path(root, "release"),
    make_app = TRUE,
    app_options = list(
      initial_dataset = entries[[1L]]$id,
      initial_page = "data_info",
      launch_browser = FALSE
    ),
    app_auth = list(enabled = FALSE, account_count = 0L, timeout_minutes = 15L)
  )
  if (!is.null(plan$error)) {
    stop(plan$error)
  }
  coordinator <- builder_coordinator_prepare(plan, "extra-label-roundtrip")
  result <- builder_execute_plan(plan, coordinator$stage, snapshots)
  result$build_id <- coordinator$build_id
  if (!identical(result$state, "success") || !isTRUE(result$publishable)) {
    stop(result$error %||% paste(result$failures, collapse = "; "))
  }
  published <- builder_coordinator_publish(coordinator, result)
  stopifnot(isTRUE(published$published))
  list(
    app_dir = published$app_dir,
    config = readRDS(file.path(published$app_dir, "cerebro_config.rds")),
    crbs = lapply(published$built, readCerebro),
    plan = plan
  )
}
