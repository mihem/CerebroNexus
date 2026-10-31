builder_e2e_source_runtime <- function(local = parent.frame()) {
  builder_profile_source_runtime(local)
  builder_dir <- normalizePath(builder_profile_inst_path("builder"))
  for (file in c(
    file.path("core", "bundle_path_contract.R"),
    "io.R",
    "recommend.R",
    "inspect.R",
    "adapters.R",
    "preview.R",
    "extras.R",
    "analysis.R",
    "marker_import.R",
    "app_bundle.R",
    "build.R",
    "prerequisite.R",
    "state.R",
    "plan.R",
    "report.R",
    "publish.R",
    "coordinator.R"
  )) {
    source_file <- file.path(builder_dir, file)
    withr::with_dir(dirname(dirname(builder_dir)), {
      sys.source(source_file, envir = local)
    })
  }
  invisible(local)
}

builder_e2e_without_source <- function(value) {
  if (!is.list(value)) {
    return(value)
  }
  value[c("source", "fingerprint")] <- NULL
  lapply(value, builder_e2e_without_source)
}

builder_e2e_review_metadata_policy <- function(recommendation, groups) {
  policy <- unserialize(serialize(recommendation, NULL, version = 3L))
  for (id in names(policy$columns)) {
    record <- policy$columns[[id]]
    if (identical(record$disposition, "blocking")) {
      next
    }
    include <- isTRUE(record$retain_in_crb) ||
      isTRUE(record$required) ||
      id %in% groups
    disposition <- if (include) {
      "included"
    } else {
      "excluded"
    }
    record$value <- disposition
    record$disposition <- disposition
    record$effective_included <- identical(disposition, "included")
    record$retain_in_crb <- include
    record$group_enabled <- id %in% groups
    record$forced <- isTRUE(record$forced %||% record$required)
    record$requires_confirmation <- FALSE
    record$group_eligible <- id %in% groups
    record$preview_allowed <- id %in% groups
    policy$columns[[id]] <- record
  }
  dispositions <- vapply(policy$columns, `[[`, character(1), "disposition")
  retained <- vapply(
    policy$columns,
    `[[`,
    logical(1),
    "retain_in_crb"
  )
  ids <- names(policy$columns)
  policy$retained <- ids[retained]
  policy$groups <- intersect(ids, groups)
  policy$forced <- ids[vapply(policy$columns, `[[`, logical(1), "forced")]
  policy$included <- ids[retained]
  policy$attention <- ids[dispositions == "attention"]
  policy$excluded <- ids[!retained]
  policy$blocking <- ids[dispositions == "blocking"]
  policy$value <- policy$retained
  policy$requires_confirmation <- length(policy$attention) > 0L ||
    length(policy$blocking) > 0L
  policy
}

builder_e2e_entry <- function(record, caller = parent.frame()) {
  inspected <- get("builder_adapter_inspect", caller, inherits = TRUE)(
    get("builder_example_adapter", caller, inherits = TRUE)(
      record$id,
      record$make()$object
    )
  )
  metadata <- get(
    "builder_recommend_metadata",
    caller,
    inherits = TRUE
  )(
    inspected$profile,
    required = c(
      inspected$legacy_profile$nUMI,
      inspected$legacy_profile$nGene
    ),
    dependency_ids = stats::setNames(
      list("core.qc.nUMI", "core.qc.nGene"),
      c(
        inspected$legacy_profile$nUMI,
        inspected$legacy_profile$nGene
      )
    )
  )
  groups <- as.character(inspected$legacy_profile$group_preselect)
  projections <- as.character(inspected$legacy_profile$reduction_preselect)
  recommendations <- list(
    metadata = metadata,
    groups = list(value = groups[[1L]], included = groups),
    projections = list(
      value = projections[[1L]],
      included = projections
    ),
    organism = list(value = inspected$legacy_profile$organism_guess),
    nomenclature = list(value = "name"),
    backend = list(value = "embedded")
  )
  settings <- get("builder_default_settings", caller, inherits = TRUE)(
    inspected$legacy_profile,
    record$label,
    recommendations = recommendations
  )
  settings$organism <- inspected$legacy_profile$organism_guess
  review_groups <- as.character(inspected$legacy_profile$group_candidates)
  settings$metadata_policy <- builder_e2e_review_metadata_policy(
    recommendations$metadata,
    review_groups
  )
  settings$groups <- review_groups
  settings$included_groups <- review_groups
  settings$default_group <- inspected$legacy_profile$group_preselect[[1L]]
  if (identical(record$id, "complete_viewer_data")) {
    settings$cell_cycle_columns <- "Phase"
    settings$included_trajectories <- list(monocle2 = "lineage")
    settings$default_trajectory <- list(
      method = "monocle2",
      name = "lineage"
    )
  }
  list(
    id = record$id,
    profile = inspected$legacy_profile,
    dataset_profile = inspected$profile,
    levels = inspected$levels,
    revision = 0L,
    settings = settings
  )
}

builder_e2e_configure_complete_viewer_data <- function(
  entry,
  record,
  object,
  caller = parent.frame()
) {
  get(
    "builder_example_configure_entry",
    caller,
    inherits = TRUE
  )(entry, record, object)
}

builder_e2e_invalid_content_entry <- function() {
  caller <- parent.frame()
  object <- get(
    ".builder_fixture_immune",
    caller,
    inherits = TRUE
  )("tcr_hla")
  legacy <- object@misc$immune_repertoire
  legacy[[1L]]$CTaa[[1L]] <- "CASSDIVERGENTF"
  legacy[[1L]]$CTstrict[[1L]] <- "TRB_divergent_clone"
  object@misc$tcr_data <- legacy
  record <- list(
    id = "invalid-content",
    label = "Invalid content",
    make = function() list(object = object, format = "Built-in example")
  )
  inspected <- get("builder_adapter_inspect", caller, inherits = TRUE)(
    get("builder_example_adapter", caller, inherits = TRUE)(
      record$id,
      object
    )
  )
  list(
    object = object,
    inspected = inspected,
    entry = builder_e2e_entry(record, caller = caller)
  )
}

builder_e2e_variant <- function(object, mutate) {
  copy <- unserialize(serialize(object, NULL, version = 3L))
  result <- mutate(copy)
  if (is.null(result)) copy else result
}

builder_e2e_variant_entry <- function(object, id) {
  record <- list(
    id = id,
    label = id,
    make = local({
      value <- object
      function() list(object = value, format = "Derived fixture")
    })
  )
  builder_e2e_entry(record, caller = parent.frame())
}

builder_e2e_validate_complete_viewer_data <- function(
  record,
  source,
  settings,
  crb,
  caller = parent.frame()
) {
  check <- function(value, label) {
    if (!isTRUE(value)) {
      stop("complete_viewer_data readback mismatch: ", label, call. = FALSE)
    }
  }
  same <- function(left, right) {
    isTRUE(all.equal(left, right, check.attributes = TRUE))
  }
  field_reader <- get(".builder_build_field", caller, inherits = TRUE)
  field <- function(name) field_reader(crb, name)

  promised <- names(record$expected_dispositions)[
    record$expected_dispositions == "preserved"
  ]
  check(
    identical(promised, names(record$expected_dispositions)),
    "catalog preserved families"
  )
  for (name in c(
    "gene_lists",
    "most_expressed_genes",
    "mean_expression",
    "enriched_pathways",
    "immune_repertoire"
  )) {
    check(same(field(name), source@misc[[name]]), paste(name, "round trip"))
  }
  check(
    identical(crb$getCellCycle(), "Phase"),
    "cell cycle"
  )
  check(
    same(field("trajectories"), source@misc$trajectories),
    "trajectories round trip"
  )
  marker_genes <- field("marker_genes")
  source_methods <- names(source@misc$marker_genes)
  check(
    same(marker_genes[source_methods], source@misc$marker_genes),
    "embedded marker genes round trip"
  )
  check(
    "Fixture sidecars" %in% names(marker_genes),
    "marker sidecars attached"
  )
  extra_material <- field("extra_material")
  check(
    same(
      extra_material$plots,
      source@misc$extra_material$plots
    ),
    "embedded extra plots round trip"
  )
  check(
    "cohort_summary" %in% names(extra_material$tables),
    "embedded extra table round trip"
  )
  check(
    all(
      c(
        "supplementary_clinical",
        "supplementary_tables · Clinical",
        "supplementary_tables · QC"
      ) %in%
        names(extra_material$tables)
    ),
    "supplementary sidecars attached"
  )
  expected_hla <- hla_normalize_typing(
    source@misc$hla_typing,
    source_type = source@misc$hla_typing_source_type
  )
  attr(expected_hla, "qc") <- NULL
  observed_hla <- crb$getHLATyping()
  attr(observed_hla, "qc") <- NULL
  check(
    same(observed_hla, expected_hla),
    "HLA typing canonical round trip"
  )

  source_trekker <- source@misc$trekker
  output_trekker <- field("trekker")
  builder_fields <- c(
    "builder_group",
    "builder_colors",
    "builder_group_values"
  )
  check(
    identical(names(output_trekker), c(names(source_trekker), builder_fields)),
    "trekker source and Builder field order"
  )
  check(
    same(source_trekker, output_trekker[names(source_trekker)]),
    "trekker source fields round trip"
  )
  check(
    identical(output_trekker$builder_group, settings$default_group),
    "trekker Builder group"
  )
  expected_group_values <- as.character(
    source@meta.data[
      source_trekker$barcodes,
      settings$default_group,
      drop = TRUE
    ]
  )
  check(
    identical(output_trekker$builder_group_values, expected_group_values),
    "trekker Builder group values"
  )

  spatial <- field("spatial")
  sections <- SeuratObject::Images(source)
  check(identical(names(spatial), sections), "spatial section order")
  expected_images <- names(settings$images) %||% character()
  for (section in sections) {
    source_coordinates <- SeuratObject::GetTissueCoordinates(source[[section]])
    source_coordinates <- source_coordinates[, c("x", "y"), drop = FALSE]
    output <- spatial[[section]]
    check(
      same(
        unname(as.matrix(output$coordinates)),
        unname(as.matrix(source_coordinates))
      ),
      paste(section, "coordinates")
    )
    check(
      identical(
        rownames(output$coordinates),
        SeuratObject::Cells(source[[section]])
      ),
      paste(section, "coordinate barcodes")
    )
    check(
      identical(output$coordinate_source, "object.GetTissueCoordinates"),
      paste(section, "coordinate source")
    )
    check(
      !length(output$histology_images %||% list()) &&
        is.null(output[["histology_image", exact = TRUE]]) &&
        is.null(output[["histology_image_bounds", exact = TRUE]]),
      paste(section, "contains no embedded histology bytes")
    )
    if (section %in% expected_images) {
      configured <- builder_image_collection_normalize(settings$images)[[
        section
      ]]
      active_label <- utils::tail(names(configured), 1L)
      expected_alignment <- builder_alignment_payload(
        configured[[active_label]]
      )
      expected_alignment$source <- active_label
      check(
        identical(output$histology_alignment, expected_alignment),
        paste(section, "external histology alignment")
      )
    }
  }
  invisible(TRUE)
}
builder_e2e_browser_available <- function(
  info = tryCatch(chromote::chromote_info(), error = function(error) NULL)
) {
  is.list(info) &&
    is.character(info$path) &&
    length(info$path) == 1L &&
    nzchar(info$path) &&
    identical(info$error %||% "", "") &&
    is.list(info$.check) &&
    identical(info$.check$status, 0L)
}

builder_e2e_run_generated_app <- function(
  app_dir,
  hermetic_library,
  root,
  backend,
  content,
  expected_images,
  label,
  gene,
  browser_available = builder_e2e_browser_available()
) {
  started_at <- proc.time()[["elapsed"]]
  driver <- NULL
  runtime_root <- file.path(
    root,
    paste0("runtime-", gsub("[^a-z0-9]+", "-", tolower(label)))
  )
  dir.create(runtime_root, recursive = TRUE, showWarnings = FALSE)
  check <- function(value, detail) {
    if (!isTRUE(value)) {
      stop("generated app runtime [", label, "]: ", detail, call. = FALSE)
    }
  }

  tryCatch(
    {
      port <- httpuv::randomPort(host = "127.0.0.1")
      app <- privacy_start_app(
        app_dir,
        port,
        runtime_root,
        libpath = hermetic_library,
        exclude_package = TRUE,
        test_mode = TRUE
      )
      on.exit(privacy_stop_app(app), add = TRUE)
      privacy_wait_for_app(app)

      if (!isTRUE(browser_available)) {
        return(list(
          started = TRUE,
          browser_checked = FALSE,
          backend = backend,
          content = content,
          elapsed = unname(proc.time()[["elapsed"]] - started_at)
        ))
      }

      driver <- shinytest2::AppDriver$new(
        app$base_url,
        name = paste0("builder_matrix_", gsub("[^a-z0-9]+", "_", label)),
        load_timeout = 60000
      )
      on.exit(try(driver$stop(), silent = TRUE), add = TRUE)
      driver$wait_for_idle(timeout = 60000)
      driver$wait_for_js(
        paste0(
          "document.querySelector('a[href=\"#shiny-tab-geneExpression\"]') ",
          "!== null && window.Shiny && Shiny.shinyapp.$socket.readyState === 1"
        ),
        timeout = 60000
      )

      driver$click(
        selector = "a[href='#shiny-tab-geneExpression']"
      )
      driver$wait_for_js(
        paste0(
          "document.getElementById('expression_genes_input') !== null && ",
          "document.getElementById('expression_projection_to_display') !== null && ",
          "document.getElementById('expression_feedback') && ",
          "document.getElementById('expression_feedback').hidden && ",
          "window.cerebroCellViews?.summary('expression_projection').ready"
        ),
        timeout = 60000
      )
      # Server-side Selectize initially has only search results. Install the
      # known fixture option before setting it; assigning an absent option
      # silently clears the control and exercises the empty-gene frame.
      driver$run_js(paste0(
        "(function(){var el=document.getElementById('expression_genes_input');",
        "var gene=",
        jsonlite::toJSON(gene, auto_unbox = TRUE),
        ";",
        "el.selectize.addOption({value:gene,label:gene});",
        "el.selectize.setValue([gene]);})()"
      ))
      driver$wait_for_js(
        paste0(
          "(function() {",
          "var host = document.getElementById('expression_projection_cell_view_host');",
          "var canvas = host && host.querySelector('canvas:not(.cv-mini)');",
          "var feedback = document.getElementById('expression_feedback');",
          "return !!(canvas && canvas.width > 0 && canvas.height > 0 && ",
          "Number(canvas.dataset.pointCount) > 0 && feedback && ",
          "feedback.hidden && host.innerText.includes(",
          jsonlite::toJSON(gene, auto_unbox = TRUE),
          ") && ",
          "!feedback.classList.contains('is-pending'));",
          "})()"
        ),
        timeout = 60000
      )

      # The matrix test verifies the on-disk backend and expression values.
      # Successful requested-gene painted feedback verifies the generated runtime read.
      # Attachment can happen in master's prefetch worker, whose console output
      # is not part of the Shiny process log.

      has_tab <- function(id) {
        isTRUE(driver$get_js(sprintf(
          "(function(){var link=document.querySelector('a[href=\"#shiny-tab-%s\"]');return !!(link&&link.offsetParent!==null);})()",
          id
        )))
      }
      check(has_tab("coordinated_views"), "Linked views was not exposed")
      check(
        identical(has_tab("spatial"), identical(content, "histology")),
        "Spatial page visibility differs from content"
      )
      check(
        identical(has_tab("trekker"), identical(content, "trekker")),
        "Trekker page visibility differs from content"
      )
      driver$click(selector = "a[href='#shiny-tab-coordinated_views']")
      driver$wait_for_js(
        paste0(
          "document.querySelector(",
          "'a[href=\"#shiny-tab-coordinated_views\"]'",
          ").parentElement.classList.contains('active')"
        ),
        timeout = 30000
      )

      if (identical(content, "plain")) {
        driver$wait_for_js(
          paste0(
            "(function() {",
            "var meta = document.getElementById('cv-meta');",
            "return !!(meta && meta.textContent.indexOf('expression') >= 0);",
            "})()"
          ),
          timeout = 60000
        )
      } else if (identical(content, "histology")) {
        expected <- expected_images[[1L]]
        source <- expected$source %||%
          list(name = "Embedded tissue image")
        expected_label <- basename(source$name)
        expected_json <- jsonlite::toJSON(expected_label, auto_unbox = TRUE)
        driver$wait_for_js(
          paste0(
            "(function() {",
            "var titles = Array.from(document.querySelectorAll('.cv-ptitle'))",
            ".map(function(x) { return x.textContent; });",
            "var picker = document.getElementById('cv-bg-image-select');",
            "var labels = picker ? Array.from(picker.options)",
            ".map(function(x) { return x.textContent; }) : [];",
            "var canvas = document.querySelector(",
            "'.cv-pane:not(.cv-hidden) canvas[id^=\"cv-cv-\"]');",
            "return titles.some(function(x) { return x.indexOf('(spatial)') >= 0; }) && ",
            "labels.indexOf(",
            expected_json,
            ") >= 0 && ",
            "canvas && canvas.width > 0 && canvas.height > 0;",
            "})()"
          ),
          timeout = 60000
        )
      } else if (identical(content, "trekker")) {
        driver$wait_for_js(
          paste0(
            "(function() {",
            "var titles = Array.from(document.querySelectorAll('.cv-ptitle'))",
            ".map(function(x) { return x.textContent; });",
            "var controls = document.getElementById('cv-trekker-ctl');",
            "var canvas = document.querySelector(",
            "'.cv-pane:not(.cv-hidden) canvas[id^=\"cv-cv-\"]');",
            "return titles.some(function(x) { return x.indexOf('Trekker') >= 0; }) && ",
            "controls && controls.style.display !== 'none' && ",
            "canvas && canvas.width > 0 && canvas.height > 0;",
            "})()"
          ),
          timeout = 60000
        )
      }

      list(
        started = TRUE,
        browser_checked = TRUE,
        backend = backend,
        content = content,
        elapsed = unname(proc.time()[["elapsed"]] - started_at)
      )
    },
    error = function(error) {
      if (!is.null(driver)) {
        builder_capture_browser_diagnostics(driver, label)
      }
      stop(
        "generated app runtime [",
        label,
        "]: ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )
}
