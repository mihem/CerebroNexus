#' Prepare external tables for a generated Viewer
#'
#' @keywords internal
#' @noRd
.bundleExtraTables <- function(
  extra_tables = NULL,
  extra_tables_sheets = NULL
) {
  if (is.null(extra_tables)) {
    if (!is.null(extra_tables_sheets)) {
      stop("`extra_tables_sheets` requires `extra_tables`.", call. = FALSE)
    }
    return(NULL)
  }

  paths <- if (is.list(extra_tables)) {
    vapply(
      extra_tables,
      function(path) {
        if (
          !is.character(path) ||
            length(path) != 1L ||
            is.na(path) ||
            !nzchar(path)
        ) {
          stop(
            "Every `extra_tables` entry must be one non-empty file path.",
            call. = FALSE
          )
        }
        path
      },
      character(1)
    )
  } else {
    extra_tables
  }
  labels <- names(paths)
  if (
    !is.character(paths) ||
      !length(paths) ||
      is.null(labels) ||
      anyNA(labels) ||
      any(!nzchar(labels)) ||
      anyDuplicated(labels) ||
      anyNA(paths) ||
      any(!nzchar(paths))
  ) {
    stop(
      "`extra_tables` must be a named collection of file paths.",
      call. = FALSE
    )
  }

  extensions <- tolower(tools::file_ext(paths))
  if (any(!extensions %in% c("csv", "tsv", "txt", "xls", "xlsx", "xlsm"))) {
    stop(
      "`extra_tables` contains an unsupported file extension.",
      call. = FALSE
    )
  }
  if (any(!file.exists(paths)) || any(!utils::file_test("-f", paths))) {
    stop(
      "Every `extra_tables` entry must be an existing regular file.",
      call. = FALSE
    )
  }
  canonical_paths <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  if (.Platform$OS.type == "windows") {
    canonical_paths <- tolower(canonical_paths)
  }
  if (anyDuplicated(canonical_paths)) {
    stop("Each `extra_tables` file may only be included once.", call. = FALSE)
  }

  sheet_maps <- .extraTableSheetMaps(extra_tables_sheets, labels, extensions)
  files <- lapply(seq_along(paths), function(index) {
    list(
      path = paths[[index]],
      extension = extensions[[index]],
      sheet_map = sheet_maps[[labels[[index]]]]
    )
  })
  names(files) <- labels
  list(files = files)
}

.extraTableSheetMaps <- function(maps, labels, extensions) {
  result <- stats::setNames(vector("list", length(labels)), labels)
  if (is.null(maps)) {
    return(result)
  }
  if (
    !is.list(maps) ||
      is.null(names(maps)) ||
      anyNA(names(maps)) ||
      any(!nzchar(names(maps))) ||
      anyDuplicated(names(maps))
  ) {
    stop("`extra_tables_sheets` must be a named list.", call. = FALSE)
  }
  unknown <- setdiff(names(maps), labels)
  if (length(unknown)) {
    stop(
      "Unknown `extra_tables_sheets` file label: ",
      unknown[[1L]],
      call. = FALSE
    )
  }

  for (file_label in names(maps)) {
    if (
      !extensions[[match(file_label, labels)]] %in% c("xls", "xlsx", "xlsm")
    ) {
      stop("`extra_tables_sheets` can only map Excel files.", call. = FALSE)
    }
    mapping <- maps[[file_label]]
    if (
      !is.list(mapping) ||
        is.null(names(mapping)) ||
        anyNA(names(mapping)) ||
        any(!nzchar(names(mapping))) ||
        anyDuplicated(names(mapping))
    ) {
      stop("Each sheet mapping must be a uniquely named list.", call. = FALSE)
    }
    sources <- vapply(
      mapping,
      function(source) {
        if (
          !is.character(source) ||
            length(source) != 1L ||
            is.na(source) ||
            !nzchar(source)
        ) {
          stop(
            "Each mapped source sheet must be one non-empty name.",
            call. = FALSE
          )
        }
        source
      },
      character(1)
    )
    if (anyDuplicated(sources)) {
      stop("A source sheet may only be mapped once.", call. = FALSE)
    }
    result[[file_label]] <- stats::setNames(sources, names(mapping))
  }
  result
}

.saveExtraTableRDS <- function(object, file, open_gz = gzfile) {
  connection <- open_gz(file, open = "wb", compression = 1L)
  on.exit(close(connection), add = TRUE)
  saveRDS(object, connection)
}

.materializeExtraTables <- function(
  plan,
  stage_result_dir,
  save_rds = .saveExtraTableRDS
) {
  if (is.null(plan)) {
    return(NULL)
  }

  files <- lapply(seq_along(plan$files), function(file_index) {
    file <- plan$files[[file_index]]
    file_label <- names(plan$files)[[file_index]]
    source_labels <- character()
    sheets <- list()

    add_sheet <- function(table, source_label, source_index) {
      source_labels <<- c(source_labels, source_label)
      label <- source_label
      if (!is.null(file$sheet_map)) {
        mapped <- match(source_label, unname(file$sheet_map))
        if (!is.na(mapped)) {
          label <- names(file$sheet_map)[[mapped]]
        }
      }
      manifest_index <- length(sheets) + 1L
      target <- paste0(
        "private-data/extra-tables/",
        file_index,
        "-",
        manifest_index,
        ".rds"
      )
      target_path <- file.path(stage_result_dir, target)
      dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
      save_rds(table, target_path)
      if (!file.exists(target_path)) {
        stop("Failed to write extra table `", label, "`.", call. = FALSE)
      }
      sheets[[manifest_index]] <<- list(
        key = paste("external", file_index, source_index, sep = ":"),
        label = label,
        path = target
      )
    }

    if (file$extension %in% c("csv", "tsv", "txt")) {
      add_sheet(.readExtraDelimited(file$path, file$extension), file_label, 1L)
    } else {
      .readExtraWorkbook(file$path, file_label, add_sheet)
    }

    if (!is.null(file$sheet_map)) {
      missing <- setdiff(unname(file$sheet_map), source_labels)
      if (length(missing)) {
        stop(
          "Mapped source sheet `",
          missing[[1L]],
          "` was not found or is empty.",
          call. = FALSE
        )
      }
      collisions <- intersect(
        names(file$sheet_map),
        setdiff(source_labels, unname(file$sheet_map))
      )
      if (length(collisions)) {
        stop(
          "Mapped sheet label `",
          collisions[[1L]],
          "` is not unique.",
          call. = FALSE
        )
      }
    }
    list(key = paste0("external-file:", file_index), sheets = sheets)
  })
  names(files) <- names(plan$files)
  list(files = files)
}

.readExtraDelimited <- function(path, extension) {
  utils::read.table(
    path,
    header = TRUE,
    sep = if (extension == "csv") "," else "\t",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    comment.char = ""
  )
}

.readExtraWorkbook <- function(path, file_label, visit) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(
      "Reading Excel extra tables requires the `readxl` package.",
      call. = FALSE
    )
  }
  sheets <- tryCatch(
    readxl::excel_sheets(path),
    error = function(error) {
      stop("Unable to read Excel file `", file_label, "`.", call. = FALSE)
    }
  )
  for (index in seq_along(sheets)) {
    table <- tryCatch(
      as.data.frame(readxl::read_excel(path, sheet = sheets[[index]])),
      error = function(error) {
        stop("Unable to read sheet `", sheets[[index]], "`.", call. = FALSE)
      }
    )
    if (nrow(table)) {
      visit(table, sheets[[index]], index)
    }
  }
  invisible(NULL)
}
