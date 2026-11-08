##----------------------------------------------------------------------------##
## Analyses that can be run on the object before it is exported.
##
## Each one writes into `@misc` and lights up a page that would otherwise not
## appear. They differ enormously in cost -- seconds, minutes, or a few hundred
## HTTP requests -- so each carries an honest label rather than being offered as
## an undifferentiated list of checkboxes.
##
## Pure: no Shiny. Each step is a function of (object, settings) -> object.
##----------------------------------------------------------------------------##

#' The steps on offer, in the order they have to run.
#'
#' `needs` names another step that has to have produced its result first;
#' `cost` is what the interface shows.
builder_analysis_steps <- function() {
  list(
    list(
      id = "percent_mt_ribo",
      label = "Mitochondrial / ribosomal percentages",
      cost = "seconds",
      network = FALSE,
      note = "Adds continuous QC variables to meta.data.",
      needs = NULL,
      needs_layer = "counts"
    ),
    list(
      id = "most_expressed",
      label = "Most expressed genes",
      cost = "seconds",
      network = FALSE,
      note = "Finds the genes contributing the most expression in each group.",
      needs = NULL,
      needs_layer = "counts"
    ),
    list(
      id = "marker_genes",
      label = "Marker genes",
      cost = "minutes",
      network = FALSE,
      note = "Runs differential expression for every group.",
      needs = NULL
    ),
    list(
      id = "enriched_pathways",
      label = "Enriched pathways (Enrichr)",
      cost = "network",
      network = TRUE,
      note = "Sends marker genes to nine Enrichr databases for every group; requires marker genes and network access.",
      needs = "marker_genes"
    )
  )
}

#' Freeze the selected analysis dependency graph.
builder_analysis_graph <- function(selected) {
  selected <- unique(as.character(selected))
  steps <- builder_analysis_steps()
  known <- vapply(steps, `[[`, character(1), "id")
  unknown <- setdiff(selected, known)
  if (length(unknown)) {
    stop("Unknown Builder analysis: ", unknown[[1L]], call. = FALSE)
  }
  graph <- lapply(selected, function(id) {
    step <- steps[[match(id, known)]]
    needs <- if (is.null(step$needs)) character() else step$needs
    dependencies <- intersect(needs, selected)
    list(id = id, dependencies = dependencies)
  })
  names(graph) <- selected
  graph
}

#' Analyses that must be recomputed after one selected step fails.
builder_retry_closure <- function(graph, failed) {
  if (!is.list(graph) || !is.character(failed) || length(failed) != 1L) {
    stop(
      "Retry closure requires a dependency graph and failed id.",
      call. = FALSE
    )
  }
  ids <- names(graph)
  if (is.null(ids) || !failed %in% ids) {
    stop(
      "The failed analysis is absent from the dependency graph.",
      call. = FALSE
    )
  }
  closure <- failed
  repeat {
    dependencies <- unique(unlist(
      lapply(graph[closure], function(node) {
        node$dependencies
      }),
      use.names = FALSE
    ))
    dependants <- ids[vapply(
      graph,
      function(node) {
        any(node$dependencies %in% closure)
      },
      logical(1)
    )]
    expanded <- unique(c(closure, dependencies, dependants))
    if (identical(expanded, closure)) {
      break
    }
    closure <- expanded
  }
  ids[ids %in% closure]
}

#' Is a step currently selectable, and if not, why.
builder_step_blocked <- function(step, profile, selected) {
  if (identical(step$id, "percent_mt_ribo")) {
    ## The gene list ships per organism; "other" has none.
    if (!profile$organism_guess %in% c("hg", "mm")) {
      return("Human and mouse only")
    }
  }
  ## Raw-count steps cannot run on an assay that only carries normalized data;
  ## say so here instead of failing during Build.
  if (!is.null(step$needs_layer) && !is.null(profile$analysis_layers)) {
    if (!step$needs_layer %in% profile$analysis_layers) {
      return(paste0(
        "Needs a ",
        step$needs_layer,
        " layer in the ",
        profile$analysis_assay %||% "selected",
        " assay"
      ))
    }
  }
  if (!is.null(step$needs)) {
    has_already <- any(vapply(
      profile$extras,
      function(x) identical(x$key, "marker_genes") && isTRUE(x$found),
      logical(1)
    ))
    if (!(step$needs %in% selected) && !has_already) {
      return("Select Marker genes first")
    }
  }
  NULL
}

#' Run the selected steps, in order, reporting progress through `on_progress`.
#'
#' @return A list with `object` and `log` (one line per step).
builder_run_analyses <- function(
  object,
  selected,
  settings,
  on_progress = function(...) NULL
) {
  log <- character()
  completed <- character()
  if (!length(selected)) {
    return(list(
      object = object,
      log = log,
      completed = completed,
      failed = character()
    ))
  }

  for (step in builder_analysis_steps()) {
    if (!step$id %in% selected) {
      next
    }
    on_progress(step$label)
    started <- Sys.time()

    res <- try(
      switch(
        step$id,
        percent_mt_ribo = addPercentMtRibo(
          object,
          assay = settings$assay,
          organism = settings$organism,
          gene_nomenclature = "name"
        ),
        most_expressed = getMostExpressedGenes(
          object,
          assay = settings$assay,
          groups = settings$groups
        ),
        marker_genes = getMarkerGenes(
          object,
          assay = settings$assay,
          organism = settings$organism,
          groups = settings$groups,
          verbose = FALSE
        ),
        enriched_pathways = getEnrichedPathways(object),
        object
      ),
      silent = TRUE
    )

    took <- round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1)
    if (inherits(res, "try-error")) {
      log <- c(
        log,
        paste0(
          "✗ ",
          step$label,
          ": ",
          conditionMessage(attr(res, "condition"))
        )
      )
      return(list(
        object = object,
        log = log,
        completed = completed,
        failed = step$id
      ))
    } else {
      object <- res
      completed <- c(completed, step$id)
      log <- c(log, paste0("✓ ", step$label, " (", took, " s)"))
    }
  }
  list(
    object = object,
    log = log,
    completed = completed,
    failed = character()
  )
}
