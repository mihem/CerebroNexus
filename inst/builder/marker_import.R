## Pure contracts for precomputed Marker gene imports.

BUILDER_MARKER_IMPORT_MAX_BYTES <- 100 * 1024^2
BUILDER_MARKER_IMPORT_MAX_ROWS <- 1000000L

builder_marker_import_error <- function(filename, sheet = NULL, error) {
  list(
    id = NULL,
    source_name = if (is.null(sheet)) {
      basename(filename)
    } else {
      as.character(sheet)
    },
    file_name = basename(filename),
    sheet = sheet,
    rows = 0L,
    columns = character(),
    raw_table = NULL,
    table = NULL,
    mapping = NULL,
    cluster_column = NULL,
    cluster = NULL,
    levels = character(),
    confirmed = FALSE,
    status = "unresolved",
    error = error
  )
}

builder_marker_import_source <- function(filename, sheet = NULL, table) {
  if (!is.data.frame(table) || nrow(table) < 1L || ncol(table) < 1L) {
    return(builder_marker_import_error(filename, sheet, "empty_table"))
  }
  if (nrow(table) > BUILDER_MARKER_IMPORT_MAX_ROWS) {
    return(builder_marker_import_error(filename, sheet, "too_many_rows"))
  }
  rownames(table) <- NULL
  list(
    id = NULL,
    source_name = if (is.null(sheet)) {
      basename(filename)
    } else {
      as.character(sheet)
    },
    file_name = basename(filename),
    sheet = sheet,
    rows = as.integer(nrow(table)),
    columns = names(table),
    raw_table = table,
    table = NULL,
    mapping = NULL,
    cluster_column = NULL,
    cluster = NULL,
    levels = character(),
    confirmed = FALSE,
    status = "mapping_required",
    error = NULL
  )
}

builder_marker_import_read_delimited <- function(path, extension) {
  separator <- switch(extension, csv = ",", tsv = "\t", NULL)
  if (is.null(separator)) {
    return(NULL)
  }
  result <- suppressWarnings(try(
    utils::read.delim(
      path,
      sep = separator,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    silent = TRUE
  ))
  if (inherits(result, "try-error")) NULL else result
}

builder_marker_import_file_inventory <- function(
  path,
  filename,
  size = NA_real_
) {
  filename <- basename(as.character(filename))
  extension <- tolower(tools::file_ext(filename))
  if (!file.exists(path)) {
    return(list(builder_marker_import_error(
      filename,
      error = "file_not_found"
    )))
  }
  size <- suppressWarnings(as.numeric(size))
  if (
    length(size) == 1L &&
      is.finite(size) &&
      size > BUILDER_MARKER_IMPORT_MAX_BYTES
  ) {
    return(list(builder_marker_import_error(
      filename,
      error = "file_too_large"
    )))
  }
  if (extension %in% c("csv", "tsv")) {
    table <- builder_marker_import_read_delimited(path, extension)
    if (is.null(table)) {
      return(list(builder_marker_import_error(
        filename,
        error = "unreadable_table"
      )))
    }
    return(list(builder_marker_import_source(filename, NULL, table)))
  }
  if (identical(extension, "xlsx")) {
    sheets <- try(readxl::excel_sheets(path), silent = TRUE)
    if (inherits(sheets, "try-error") || !length(sheets)) {
      return(list(builder_marker_import_error(
        filename,
        error = "unreadable_workbook"
      )))
    }
    return(lapply(sheets, function(sheet) {
      table <- try(
        as.data.frame(readxl::read_excel(path, sheet = sheet)),
        silent = TRUE
      )
      if (inherits(table, "try-error")) {
        return(builder_marker_import_error(filename, sheet, "unreadable_sheet"))
      }
      builder_marker_import_source(filename, sheet, table)
    }))
  }
  list(builder_marker_import_error(filename, error = "unsupported_format"))
}

builder_marker_import_inventory <- function(
  paths,
  filenames = basename(paths),
  sizes = file.info(paths)$size
) {
  paths <- as.character(paths)
  filenames <- as.character(filenames)
  sizes <- as.numeric(sizes)
  if (length(paths) != length(filenames) || length(paths) != length(sizes)) {
    stop("Marker import file metadata has inconsistent lengths.", call. = FALSE)
  }
  sources <- unlist(
    Map(builder_marker_import_file_inventory, paths, filenames, sizes),
    recursive = FALSE,
    use.names = FALSE
  )
  for (index in seq_along(sources)) {
    sources[[index]]$id <- sprintf("source-%03d", index)
  }
  sources
}

builder_marker_import_level_key <- function(value) {
  tolower(gsub("[^[:alnum:]]+", "", as.character(value)))
}

builder_marker_import_infer_level <- function(filename, sheet, known_levels) {
  known_levels <- as.character(known_levels)
  candidates <- c(
    if (!is.null(sheet)) as.character(sheet) else character(),
    tools::file_path_sans_ext(basename(filename))
  )
  candidate_keys <- builder_marker_import_level_key(candidates)
  level_keys <- builder_marker_import_level_key(known_levels)
  exact <- known_levels[level_keys %in% candidate_keys]
  if (length(unique(exact)) == 1L) {
    return(unique(exact))
  }
  contained <- known_levels[vapply(
    level_keys,
    function(key) {
      nzchar(key) && any(grepl(key, candidate_keys, fixed = TRUE))
    },
    logical(1)
  )]
  if (length(unique(contained)) == 1L) unique(contained) else NULL
}

builder_marker_import_normalize <- function(table, group, values) {
  table[[group]] <- as.character(values)
  table <- table[c(group, setdiff(names(table), group))]
  rownames(table) <- NULL
  table
}

builder_marker_import_mapping_error <- function(source, error) {
  source$table <- NULL
  source$levels <- character()
  source$confirmed <- FALSE
  source$status <- "unresolved"
  source$error <- error
  source
}

builder_marker_import_map_single <- function(
  source,
  group,
  level,
  known_levels,
  confirmed = FALSE
) {
  if (!is.data.frame(source$raw_table)) {
    return(builder_marker_import_mapping_error(
      source,
      source$error %||% "empty_table"
    ))
  }
  level <- trimws(as.character(level))
  if (length(level) != 1L || is.na(level) || !nzchar(level)) {
    return(builder_marker_import_mapping_error(source, "missing_cluster"))
  }
  if (!level %in% known_levels) {
    return(builder_marker_import_mapping_error(source, "unknown_cluster"))
  }
  source$table <- builder_marker_import_normalize(
    source$raw_table,
    group,
    rep(level, nrow(source$raw_table))
  )
  source$mapping <- "single"
  source$cluster_column <- NULL
  source$cluster <- level
  source$levels <- level
  source$confirmed <- isTRUE(confirmed)
  source$status <- if (source$confirmed) "ready" else "confirmation_required"
  source$error <- NULL
  source
}

builder_marker_import_map_multiple <- function(
  source,
  group,
  column,
  known_levels
) {
  if (!is.data.frame(source$raw_table)) {
    return(builder_marker_import_mapping_error(
      source,
      source$error %||% "empty_table"
    ))
  }
  column <- as.character(column)
  if (
    length(column) != 1L ||
      is.na(column) ||
      !column %in% names(source$raw_table)
  ) {
    return(builder_marker_import_mapping_error(
      source,
      "missing_cluster_column"
    ))
  }
  values <- trimws(as.character(source$raw_table[[column]]))
  if (anyNA(values) || any(!nzchar(values))) {
    return(builder_marker_import_mapping_error(source, "missing_cluster"))
  }
  levels <- unique(values)
  if (any(!levels %in% known_levels)) {
    return(builder_marker_import_mapping_error(source, "unknown_cluster"))
  }
  table <- source$raw_table
  table[[column]] <- NULL
  source$table <- builder_marker_import_normalize(table, group, values)
  source$mapping <- "multiple"
  source$cluster_column <- column
  source$cluster <- NULL
  source$levels <- levels
  source$confirmed <- TRUE
  source$status <- "ready"
  source$error <- NULL
  source
}

builder_marker_import_source_ready <- function(source) {
  is.list(source) &&
    identical(source$status, "ready") &&
    isTRUE(source$confirmed) &&
    is.null(source$error) &&
    is.data.frame(source$table) &&
    nrow(source$table) > 0L
}

builder_marker_import_validate <- function(
  method,
  group,
  sources,
  known_levels,
  existing_methods = character()
) {
  method <- trimws(as.character(method))
  group <- trimws(as.character(group))
  errors <- character()
  if (length(method) != 1L || is.na(method) || !nzchar(method)) {
    errors <- c(errors, "missing_method")
  } else if (method %in% existing_methods) {
    errors <- c(errors, "duplicate_method")
  }
  if (length(group) != 1L || is.na(group) || !nzchar(group)) {
    errors <- c(errors, "missing_group")
  }
  if (!length(sources)) {
    errors <- c(errors, "missing_sources")
  } else if (
    any(!vapply(sources, builder_marker_import_source_ready, logical(1)))
  ) {
    errors <- c(errors, "unresolved_sources")
  }
  ready_sources <- Filter(builder_marker_import_source_ready, sources)
  single <- Filter(
    function(source) identical(source$mapping, "single"),
    ready_sources
  )
  single_levels <- vapply(single, function(source) source$cluster, character(1))
  if (anyDuplicated(single_levels)) {
    errors <- c(errors, "duplicate_cluster_assignment")
  }
  covered <- unique(unlist(
    lapply(ready_sources, `[[`, "levels"),
    use.names = FALSE
  ))
  covered <- intersect(as.character(known_levels), covered)
  missing <- setdiff(as.character(known_levels), covered)
  warnings <- if (length(missing)) {
    paste("No imported rows for:", paste(missing, collapse = ", "))
  } else {
    character()
  }
  list(
    ready = !length(errors) && length(ready_sources) > 0L,
    errors = unique(errors),
    coverage = list(covered = covered, missing = missing),
    warnings = warnings
  )
}

builder_marker_import_prepare_source <- function(source, group, known_levels) {
  if (!is.null(source$error) || !is.data.frame(source$raw_table)) {
    return(source)
  }
  if (group %in% source$columns) {
    source$mapping <- "multiple"
    source$cluster_column <- group
    source$cluster <- NULL
  } else {
    source$mapping <- "single"
    source$cluster_column <- NULL
    source$cluster <- builder_marker_import_infer_level(
      source$file_name,
      source$sheet,
      known_levels
    )
  }
  source$table <- NULL
  source$levels <- character()
  source$confirmed <- FALSE
  source$status <- if (
    is.null(source$cluster) && identical(source$mapping, "single")
  ) {
    "mapping_required"
  } else {
    "confirmation_required"
  }
  source
}

builder_marker_import_refresh_draft <- function(draft) {
  draft$validation <- builder_marker_import_validate(
    draft$method,
    draft$group,
    draft$sources,
    draft$known_levels,
    draft$existing_methods %||% character()
  )
  draft
}

builder_marker_import_new_draft <- function(
  id,
  method,
  group,
  sources,
  known_levels,
  existing_methods = character()
) {
  sources <- lapply(
    sources,
    builder_marker_import_prepare_source,
    group = group,
    known_levels = known_levels
  )
  builder_marker_import_refresh_draft(list(
    id = id,
    method = trimws(as.character(method)),
    group = as.character(group),
    known_levels = as.character(known_levels),
    existing_methods = as.character(existing_methods),
    sources = sources
  ))
}

builder_marker_import_confirm_source <- function(
  draft,
  source_id,
  mode,
  value
) {
  index <- which(vapply(
    draft$sources,
    function(source) identical(source$id, source_id),
    logical(1)
  ))
  if (length(index) != 1L) {
    stop("Marker import source does not exist.", call. = FALSE)
  }
  source <- draft$sources[[index]]
  mapped <- if (identical(mode, "multiple")) {
    builder_marker_import_map_multiple(
      source,
      draft$group,
      value,
      draft$known_levels
    )
  } else {
    builder_marker_import_map_single(
      source,
      draft$group,
      value,
      draft$known_levels,
      confirmed = TRUE
    )
  }
  draft$sources[[index]] <- mapped
  builder_marker_import_refresh_draft(draft)
}

builder_marker_import_safe_source <- function(source) {
  if (!builder_marker_import_source_ready(source)) {
    stop("Only resolved Marker import sources can be frozen.", call. = FALSE)
  }
  list(
    id = as.character(source$id),
    source_name = as.character(source$source_name),
    file_name = basename(as.character(source$file_name)),
    sheet = if (is.null(source$sheet)) NULL else as.character(source$sheet),
    rows = as.integer(source$rows),
    columns = as.character(source$columns),
    mapping = as.character(source$mapping),
    cluster_column = if (is.null(source$cluster_column)) {
      NULL
    } else {
      as.character(source$cluster_column)
    },
    cluster = if (is.null(source$cluster)) {
      NULL
    } else {
      as.character(source$cluster)
    },
    levels = as.character(source$levels),
    table = as.data.frame(source$table, stringsAsFactors = FALSE)
  )
}

builder_freeze_marker_imports <- function(imports) {
  imports <- imports %||% list()
  if (!is.list(imports)) {
    stop("Marker imports must be a list.", call. = FALSE)
  }
  frozen <- lapply(imports, function(record) {
    validation <- record$validation %||% list(ready = record$ready)
    if (!isTRUE(validation$ready) || !length(record$sources %||% list())) {
      stop("Only ready Marker import methods can be frozen.", call. = FALSE)
    }
    list(
      id = as.character(record$id),
      method = trimws(as.character(record$method)),
      group = as.character(record$group),
      sources = lapply(record$sources, builder_marker_import_safe_source),
      coverage = validation$coverage %||% record$coverage %||% list(),
      warnings = as.character(
        validation$warnings %||% record$warnings %||% character()
      ),
      ready = TRUE
    )
  })
  names(frozen) <- names(imports)
  frozen
}

builder_attach_marker_imports <- function(object, imports) {
  imports <- imports %||% list()
  if (!length(imports)) {
    return(object)
  }
  if (!methods::is(object, "Seurat")) {
    stop("Marker imports require a Seurat object.", call. = FALSE)
  }
  existing <- object@misc$marker_genes %||% list()
  for (record in imports) {
    method <- trimws(as.character(record$method %||% ""))
    group <- as.character(record$group %||% "")
    if (!nzchar(method) || !nzchar(group) || !isTRUE(record$ready)) {
      stop("A frozen Marker import method is invalid.", call. = FALSE)
    }
    if (method %in% names(existing)) {
      stop("Marker gene method already exists: ", method, call. = FALSE)
    }
    tables <- lapply(record$sources %||% list(), function(source) source$table)
    if (!length(tables) || any(!vapply(tables, is.data.frame, logical(1)))) {
      stop("Marker gene method has no ready tables: ", method, call. = FALSE)
    }
    if (
      any(
        !vapply(
          tables,
          function(table) {
            identical(names(table)[[1L]], group)
          },
          logical(1)
        )
      )
    ) {
      stop("Marker gene grouping column is invalid: ", method, call. = FALSE)
    }
    merged <- do.call(rbind, tables)
    rownames(merged) <- NULL
    existing[[method]] <- stats::setNames(list(merged), group)
  }
  object@misc$marker_genes <- existing
  object
}
