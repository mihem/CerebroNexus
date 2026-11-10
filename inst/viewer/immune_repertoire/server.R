##----------------------------------------------------------------------------##
## Tab: Immune Repertoire server — entry point
##----------------------------------------------------------------------------##

## Availability check WITHOUT loading the namespace. Loading scRepertoire's
## namespace (its .onLoad, plus Imports: SingleCellExperiment,
## SummarizedExperiment, S4Vectors, Seurat, iNEXT, ...) drags in ~90 packages
## and several seconds of lazyLoadDBfetch — note this is loadNamespace, not
## attaching it to the search path. Because the IR settings outputs are
## suspendWhenHidden = FALSE, they render on the very first flush and used to
## pay that cost at startup even when the user never opened the tab. system.file()
## only resolves the package directory on disk — no .onLoad, no dependency
## cascade — so startup stays cheap. The real load happens lazily on the first
## scRepertoire::fn() call, i.e. the first scRepertoire-backed plot; self-made
## IR plots (Clone Sharing) and the default Clonal UMAP never
## trigger it. ggplot2 and shinycssloaders are suggested packages the page also
## draws with, so they are part of the same check.
IR_PACKAGES <- c("scRepertoire", "ggplot2", "shinycssloaders")
ir_missing_packages <- function() {
  installed <- vapply(
    IR_PACKAGES,
    function(package) nzchar(system.file(package = package)),
    logical(1)
  )
  IR_PACKAGES[!installed]
}
has_scRepertoire <- function() {
  length(ir_missing_packages()) == 0L
}

## ---- Lazy-load boundary ------------------------------------------------ ##
## scRepertoire is loaded lazily: the first scRepertoire::fn() call inside a
## plot renderer triggers loadNamespace() (~8s, ~90 packages). We deliberately
## do NOT prewarm it in the background — later::later() is cooperative
## scheduling on Shiny's single R thread, so a "background" loadNamespace()
## still blocks the event loop and freezes every session sharing the process.
## Startup therefore never loads scRepertoire; a repertoire user instead pays a
## predictable one-time load on their first plot.
##
## req_scRepertoire() gates every repertoire renderer. It cheaply confirms the
## package is installed (system.file, no namespace load), then forces the
## idempotent namespace load HERE so a broken or partial install surfaces as an
## explicit, useful message inside the plot box — rather than an opaque error
## from deep inside a scRepertoire call, or a full UI that only fails when a
## renderer runs (system.file alone cannot detect a missing transitive
## dependency, incompatible binary, failed .onLoad, or corrupt lazy-load DB).
## The settings and tab gates keep using the cheap has_scRepertoire() probe so
## the first flush stays free of any namespace load.
req_scRepertoire <- function() {
  req(has_scRepertoire())
  loaded <- tryCatch(
    {
      loadNamespace("scRepertoire")
      TRUE
    },
    error = function(e) conditionMessage(e)
  )
  validate(need(
    isTRUE(loaded),
    paste0(
      "scRepertoire is installed but could not be loaded",
      if (is.character(loaded)) paste0(": ", loaded) else "",
      ". Please reinstall it and its dependencies."
    )
  ))
}

## ---- Missing-dependency notice ---------------------------------------- ##
## scRepertoire is an optional, lazily loaded dependency. Rather than
## a silent blank panel we show an explicit prompt telling the user how to
## reinstall it. Shown in both the settings and visualizations boxes.
ir_scRepertoire_missing_ui <- function() {
  missing <- ir_missing_packages()
  bioc <- intersect(missing, "scRepertoire")
  cran <- setdiff(missing, bioc)
  div(
    class = "alert alert-warning",
    tags$b(
      paste(missing, collapse = ", "),
      if (length(missing) == 1L) "is" else "are",
      "required for immune repertoire analysis."
    ),
    tags$br(),
    "Please install to use the TCR/BCR features:",
    tags$pre(paste(
      c(
        if (length(bioc)) {
          paste0("BiocManager::install(", deparse(bioc), ")")
        },
        if (length(cran)) {
          paste0("install.packages(", paste(deparse(cran), collapse = ""), ")")
        }
      ),
      collapse = "\n"
    ))
  )
}

## ---- Container size guard: prevent "figure margins too large" during
## ---- tab switches when the output container has zero/tiny dimensions. req()
## ---- silently halts rendering; Shiny re-triggers once the container has real
## ---- space. Both width AND height must clear the floor: base-graphics and
## ---- grid prints call plot.new(), which throws "figure margins too large" when either dimension leaves no room
## ---- for the fixed margins (~1 inch). 72px ≈ 1 inch at the default device
## ---- resolution, so require a comfortable margin above that.
req_plot_space <- function(output_id, min_px = 80L) {
  cd <- shiny::getDefaultReactiveDomain()$clientData
  w <- cd[[paste0("output_", output_id, "_width")]]
  h <- cd[[paste0("output_", output_id, "_height")]]
  shiny::req(isTRUE(w >= min_px), isTRUE(h >= min_px))
}

## Set while a plot is drawn when it shows an error instead of a result; see
## ir_bindCache().
ir_plot_cache_state <- new.env(parent = emptyenv())
ir_plot_failed <- function() {
  ir_plot_cache_state$failed <- TRUE
  invisible(NULL)
}

safeRenderPlot <- function(expr, plot_name = "unknown") {
  tryCatch(
    {
      force(expr)
    },
    error = function(e) {
      # validate()/need()/req() raise a "shiny.silent.error"; re-raise it so
      # Shiny renders the usual grey placeholder instead of our error plot.
      # (Using a single handler avoids the re-raised condition being caught by
      # a sibling error handler in the same tryCatch.)
      if (inherits(e, "shiny.silent.error")) {
        stop(e)
      }
      # "figure margins too large" / "plot region too large" come from base
      # graphics plot.new() when Shiny opens a near-zero PNG device — which
      # happens for a plotOutput on a hidden/not-yet-laid-out tab (the browser
      # reports a CSS height to clientData that does not match the tiny device
      # Shiny actually opens, so the upstream width/height guard can't catch
      # it). This is a transient layout state, not a real failure: swallow it
      # silently (Shiny shows its grey placeholder and re-renders once the
      # container has real space) instead of dumping a scary stack trace to the
      # console on every tab switch.
      if (grepl("figure margins too large|plot region too large", e$message)) {
        shiny::req(FALSE)
      }
      message("[IR ERROR] Plot '", plot_name, "' failed: ", e$message)
      ir_plot_failed()

      # scRepertoire raises cryptic internal errors (e.g. "get1index",
      # "subscript out of bounds", "less than one element") when the current
      # selection leaves a group/sample empty or single-valued. These are not
      # actionable to the user, so translate them into a plain empty-state
      # message instead of dumping the raw R error onto the plot.
      msg <- conditionMessage(e)
      is_empty_selection <- grepl(
        "get1index|subscript out of bounds|less than one element|undefined columns|replacement has|non-conformable|missing value",
        msg,
        ignore.case = TRUE
      )
      # clonalSizeDistribution MLE fitting fails on gene/nt/aa clone calls
      # (the distribution is too noisy for log-normal MLE); the error reads
      # "initial parameter values are invalid" or "NA/NaN ... sigmau".
      is_mle_failure <- grepl(
        "initial parameter|NA/NaN.*sigmau|non-finite|optimization failed",
        msg,
        ignore.case = TRUE
      )
      label <- if (is_empty_selection) {
        paste0(
          "No data to display for the current selection.\n",
          "Try a different Chain, Clone call, or Group by."
        )
      } else if (is_mle_failure) {
        paste0(
          "Clone size distribution fitting failed.\n",
          "The strict clone definition is required for this plot.\n",
          "No action needed — strict is already enforced."
        )
      } else {
        paste0("Could not render this plot:\n", msg)
      }
      # Render an empty-state message robustly even on small devices. ggplot
      # avoids base-graphics margin issues ("figure margins too large").
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 0,
          y = 0,
          label = label,
          size = 4.5,
          colour = "#666666",
          hjust = 0.5,
          vjust = 0.5
        ) +
        ggplot2::theme_void()
    }
  )
}

## Cells per (group, isotype), groups in the order they first appear: the
## counts the isotype bars show. `groups` is the group of every repertoire
## row. NULL when no cell has both.
bcr_isotype_counts <- function(encoded, groups, group_col = "sample") {
  isotype <- encoded$isotype
  groups <- as.character(groups)
  keep <- !is.na(isotype) & !is.na(groups)
  if (!length(groups) || !any(keep)) {
    return(NULL)
  }
  pairs <- data.frame(
    group = groups[keep],
    isotype = encoded$isotype_levels[isotype[keep]]
  )
  groups <- unique(pairs$group)
  isotypes <- unique(pairs$isotype)
  key <- (match(pairs$group, groups) - 1L) *
    length(isotypes) +
    match(pairs$isotype, isotypes)
  first <- !duplicated(key)
  counts <- data.frame(
    group = pairs$group[first],
    isotype = pairs$isotype[first],
    n = tabulate(key, nbins = length(groups) * length(isotypes))[key[first]]
  )
  names(counts)[[1L]] <- group_col
  counts
}


## ---- BCR-specific: isotype distribution plot ---------------------------- ##
bcr_isotype_plot <- function(encoded, groups, group_col = "sample") {
  iso <- bcr_isotype_counts(encoded, groups, group_col)
  if (is.null(iso)) {
    return(NULL)
  }

  iso$isotype <- factor(
    iso$isotype,
    levels = c(
      "IGHM",
      "IGHD",
      "IGHG1",
      "IGHG2",
      "IGHG3",
      "IGHG4",
      "IGHA1",
      "IGHA2",
      "IGHE"
    )
  )
  iso[[group_col]] <- factor(
    iso[[group_col]],
    levels = unique(iso[[group_col]])
  )

  ## Low-saturation isotype palette (was high-saturation Set1). Same low-sat
  ## family used app-wide via cerebro_group_colors(); named per isotype so the
  ## mapping is stable across plots.
  pal <- setNames(
    cerebro_group_colors(9),
    c(
      "IGHM",
      "IGHD",
      "IGHG1",
      "IGHG2",
      "IGHG3",
      "IGHG4",
      "IGHA1",
      "IGHA2",
      "IGHE"
    )
  )

  ggplot2::ggplot(
    iso,
    ggplot2::aes(x = .data[[group_col]], y = n, fill = isotype)
  ) +
    ggplot2::geom_col(position = "fill", colour = "white", linewidth = 0.1) +
    ggplot2::scale_fill_manual(
      values = pal,
      na.value = "grey80",
      name = "Isotype",
      drop = FALSE
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::labs(x = group_col, y = "Proportion") +
    ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
    )
}


## ---- Helper: per-sample metadata (columns constant within each sample) -- ##
## The repertoire tables' own columns and the grouping column `group_by` that
## hold one value within every sample, one row per sample.
sample_level_meta <- function(encoded, metadata = NULL, group_by = NULL) {
  shared <- Filter(function(v) is.null(attr(v, "samples")), encoded$columns)
  candidates <- union(names(shared), group_by)
  n_samples <- length(encoded$samples)
  first <- match(seq_len(n_samples), encoded$sample)
  values <- list()
  for (col in candidates) {
    v <- ir_row_values(encoded, col, metadata)
    if (is.null(v)) {
      next
    }
    code <- match(v, unique(v))
    distinct <- !duplicated(encoded$sample + (code - 1) * n_samples)
    if (all(tabulate(encoded$sample[distinct], n_samples) == 1L)) {
      values[[col]] <- as.character(v[first])
    }
  }
  if (length(values) == 0L) {
    return(NULL)
  }
  data.frame(
    .sample_name = encoded$samples,
    values,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}


## The plots depend on the data set and their controls only (no session state:
## palettes and the plot theme are constants), so sessions of the process that
## open the same data set files share them: a new session, a reload or a
## return to the data set used to redraw every plot (Clone Sharing 14 s on the
## 187-sample T-cell atlas). The key carries the files' identity (ir_dataset_process_key());
## a data set without a file on disk keeps a key private to its session.
ir_dataset_cache_key <- reactive({
  ds <- tryCatch(data_set(), error = function(e) NULL)
  ir_dataset_process_key(ds) %||% list(session = session$token)
})

## The cache the plots are written to. An error, and a figure that shows one
## (ir_plot_failed()), is this session's result and not the data set's: it is
## not stored, so a transient failure or a container measured while hidden is
## not replayed to every session. bindCache() reads a key, draws on a miss and
## then writes it, all in one go.
ir_plot_cache <- function(cache) {
  resolve <- function() {
    if (identical(cache, "app")) {
      shiny::getShinyOption("cache", default = NULL)
    } else if (identical(cache, "session")) {
      session$cache
    } else {
      cache
    }
  }
  list(
    get = function(key, ...) {
      ir_plot_cache_state$failed <- FALSE
      resolve()$get(key, ...)
    },
    set = function(key, value) {
      if (!isTRUE(value$error) && !isTRUE(ir_plot_cache_state$failed)) {
        resolve()$set(key, value)
      }
      invisible(TRUE)
    }
  )
}

ir_bindCache <- function(x, ..., cache = "app") {
  # Keep only truly global plot state here. Plot-specific parameters belong
  # in each renderer's own bindCache call.
  shiny::bindCache(
    x,
    ...,
    available_crb_files$selected,
    ir_dataset_cache_key(),
    cache = ir_plot_cache(cache)
  )
}

for (module_file in c(
  "param_spec.R",
  "encode.R",
  "data.R",
  "settings.R",
  "tabs.R",
  "help_demo_helpers.R",
  "help.R",
  "help_guide.R",
  "paired_scatter_helpers.R",
  "compare_helpers.R",
  "visualizations.R"
)) {
  source(
    cerebro_root_path("viewer", "immune_repertoire", module_file),
    local = TRUE
  )
}
