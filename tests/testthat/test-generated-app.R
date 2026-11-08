## One Builder bundle, one App server and one browser session serve every
## generated-App test below. Each browser test selects its dataset first.
generated_app_e2e_reset_runtime()

generated_app_expected_content_pages <- function(expected) {
  unique(c(
    intersect(
      expected$visible_pages,
      generated_app_fixture_pages()$hidden
    ),
    intersect(expected$optional_payloads, c("spatial", "trekker"))
  ))
}

.generated_app_e2e_click_inner_tab <- function(id, value, timeout = 60000) {
  selector <- sprintf("#%s a[data-value='%s']", id, value)
  selector_js <- .generated_app_e2e_js_value(selector)
  id_js <- .generated_app_e2e_js_value(id)
  value_js <- .generated_app_e2e_js_value(value)
  driver <- generated_app_e2e_driver()
  script <- function(click) {
    paste0(
      "(function(){var link=document.querySelector(",
      selector_js,
      ");if(!link)return false;var active=!!(link.parentElement&&",
      "link.parentElement.classList.contains('active'));var app=window.Shiny&&",
      "Shiny.shinyapp;var current=app&&app.$inputValues?",
      "app.$inputValues[",
      id_js,
      "]:null;",
      if (click) "if(!active)link.click();" else "",
      "return active&&current===",
      value_js,
      ";})()"
    )
  }
  ## A data switch rebuilds some tab strips on the server, and the rebuilt
  ## strip selects the tab remembered from before. A click that lands before
  ## the rebuild is then undone, so click once the App is idle and check that
  ## the tab is still selected after the next idle period.
  for (attempt in 1:3) {
    driver$wait_for_idle(duration = 500, timeout = timeout)
    driver$wait_for_js(script(click = TRUE), timeout = timeout)
    driver$wait_for_idle(duration = 500, timeout = timeout)
    if (isTRUE(driver$get_js(script(click = FALSE)))) {
      return(invisible(value))
    }
  }
  stop("Tab ", value, " of ", id, " did not stay selected.", call. = FALSE)
}

.generated_app_e2e_select_text <- function(id, timeout = 60000) {
  id_js <- .generated_app_e2e_js_value(id)
  generated_app_e2e_driver()$wait_for_js(
    paste0(
      "(function(){var node=document.getElementById(",
      id_js,
      ");return !!(node && node.options && node.options.length);})()"
    ),
    timeout = timeout
  )
  as.character(generated_app_e2e_driver()$get_js(paste0(
    "(function(){var node=document.getElementById(",
    id_js,
    ");return Array.from(node.options).map(function(option){",
    "return option.textContent.trim();}).join(' | ');})()"
  )))
}

.generated_app_e2e_dom_text <- function(id, contains, timeout = 60000) {
  id_js <- .generated_app_e2e_js_value(id)
  contains_js <- .generated_app_e2e_js_value(as.list(as.character(contains)))
  driver <- generated_app_e2e_driver()
  driver$wait_for_js(
    paste0(
      "(function(){var node=document.getElementById(",
      id_js,
      ");if(!node)return false;var text=node.textContent||'';var expected=",
      contains_js,
      ";return expected.every(function(value){return text.indexOf(value)!==-1;});})()"
    ),
    timeout = timeout
  )
  as.character(driver$get_js(sprintf(
    "document.getElementById(%s).textContent",
    id_js
  )))
}


test_that("canonical fixtures freeze into one explicit multi-dataset plan", {
  ## Slow (full Builder plan).
  skip_on_cran()
  bundle <- generated_app_e2e_bundle()

  expect_s3_class(bundle$plan, "builder_build_plan")
  expected_ids <- paste0(
    "e2e-",
    c("basic", "analysis", "spatial", "immune-tcr-hla", "immune-bcr", "trekker")
  )
  expect_identical(bundle$plan$dataset_order, expected_ids)
  expect_identical(
    vapply(bundle$plan$items, `[[`, character(1), "id"),
    expected_ids
  )
  expect_true(bundle$plan$make_app)
  expect_identical(bundle$plan$app_options$initial_dataset, "e2e-basic")
  expect_identical(bundle$plan$app_options$initial_dataset_mode, "explicit")
  expect_false(bundle$plan$app_options$show_upload_ui)
  expect_identical(
    bundle$plan$app_options$welcome_message,
    "Generated App E2E"
  )
  expect_identical(
    bundle$plan$app_options$point_size,
    list(
      overview_projection_point_size = 6,
      projection_point_opacity = 1
    )
  )
  expect_false(bundle$plan$app_options$variable_to_compare)
})

test_that("Builder settings survive profile and BuildPlan normalization", {
  bundle <- generated_app_e2e_bundle()

  for (name in names(bundle$fixtures)) {
    fixture <- bundle$fixtures[[name]]
    expected <- fixture$expected
    entry <- bundle$entries[[name]]
    item <- bundle$plan$items[[match(entry$id, bundle$plan$dataset_order)]]

    expect_identical(entry$settings$name, expected$dataset_name, info = name)
    expect_identical(entry$settings$organism, expected$organism, info = name)
    expect_identical(
      entry$settings$groups,
      fixture$builder_settings$groups,
      info = name
    )
    expect_identical(
      entry$settings$default_group,
      expected$default_group,
      info = name
    )
    expect_identical(
      entry$settings$default_projection,
      expected$default_projection,
      info = name
    )
    expect_true(
      all(expected$groups %in% entry$settings$metadata_policy$included),
      info = name
    )

    expect_identical(item$name, expected$dataset_name, info = name)
    expect_identical(item$organism, expected$organism, info = name)
    expect_identical(item$included_groups, expected$groups, info = name)
    expect_identical(
      item$included_projections,
      expected$projections,
      info = name
    )
    expect_identical(item$default_group, expected$default_group, info = name)
    expect_identical(
      item$default_projection,
      expected$default_projection,
      info = name
    )
    expect_identical(
      item$viewer_page_expectations$visible_conditional,
      generated_app_expected_content_pages(expected),
      info = name
    )
    for (group in names(expected$palettes)) {
      configured <- expected$palettes[[group]]
      expect_identical(
        unname(item$colors[[group]][names(configured)]),
        unname(configured),
        info = paste(name, group)
      )
    }
  }
})

test_that("generated CRBs preserve source identities, values, and page causes", {
  bundle <- generated_app_e2e_bundle()
  expect_identical(names(bundle$crbs), names(bundle$fixtures))

  for (name in names(bundle$fixtures)) {
    fixture <- bundle$fixtures[[name]]
    expected <- fixture$expected
    crb <- bundle$crbs[[name]]
    metadata <- crb$getMetaData()
    expression <- crb$getExpressionMatrix()

    expect_identical(nrow(metadata), expected$n_cells, info = name)
    expect_identical(crb$getCellNames(), expected$cell_ids, info = name)
    expect_identical(nrow(expression), expected$n_genes, info = name)
    expect_identical(colnames(expression), expected$cell_ids, info = name)
    expect_identical(rownames(expression), expected$gene_ids, info = name)
    expect_identical(
      crb$getExperiment()$organism,
      expected$organism,
      info = name
    )
    expect_identical(crb$getGroups(), expected$groups, info = name)
    expect_identical(
      crb$availableProjections(),
      expected$projections,
      info = name
    )
    expect_setequal(
      .builder_crb_visible_pages(crb),
      generated_app_expected_content_pages(expected)
    )
  }

  analysis <- bundle$crbs$analysis
  expect_identical(
    analysis$getEnrichedPathways("offline", "seurat_clusters")$Term,
    c("Pathway A", "Pathway B", "Pathway C")
  )
  expect_identical(
    analysis$getExtraTable("fixture_summary")$value,
    c("28", "52", "offline fixture")
  )
  expect_identical(
    analysis$getNamesOfTrajectories("monocle2"),
    "analysis_lineage"
  )

  spatial <- bundle$crbs$spatial
  expect_identical(spatial$availableSpatial(), c("section_a", "section_b"))
  source_images <- builder_image_collection_normalize(
    bundle$entries$spatial$settings$images
  )
  configured_images <- bundle$config$spatial_images[[
    .generated_app_e2e_id("spatial")
  ]]
  for (section in spatial$availableSpatial()) {
    value <- spatial$getSpatialData(section)
    contract <- bundle$fixtures$spatial$expected$image_alignment[[section]]
    image_label <- names(source_images[[section]])[[1L]]
    expected_alignment <- builder_alignment_payload(
      source_images[[section]][[image_label]]
    )
    expected_alignment$source <- image_label

    expect_identical(value[["histology_images", exact = TRUE]], list())
    expect_null(value[["histology_image", exact = TRUE]])
    expect_null(value[["histology_image_bounds", exact = TRUE]])
    expect_identical(value$histology_alignment, expected_alignment)

    descriptors <- configured_images[[section]]
    expect_identical(names(descriptors), image_label)
    descriptor <- descriptors[[image_label]]
    expect_identical(descriptor$label, image_label)
    expect_identical(
      descriptor$bounds,
      unlist(contract$bounds, use.names = TRUE)
    )
    expect_true(.builder_app_safe_relative(descriptor$path))
    expect_false(startsWith(descriptor$path, "data:"))
    expect_true(file.exists(file.path(bundle$app_dir, descriptor$path)))
  }

  expect_false(is.null(bundle$crbs$immune_tcr_hla$getImmuneRepertoire()))
  expect_false(is.null(bundle$crbs$immune_tcr_hla$getHLATyping()))
  expect_false(is.null(bundle$crbs$immune_bcr$getImmuneRepertoire()))
  expect_s3_class(bundle$crbs$immune_bcr$getHLATyping(), "data.frame")
  expect_identical(nrow(bundle$crbs$immune_bcr$getHLATyping()), 0L)
  expect_false(is.null(bundle$crbs$trekker$getTrekker()))
})

test_that("private App config and all outputs remain inside the temporary release", {
  bundle <- generated_app_e2e_bundle()
  config <- bundle$config

  expect_true(dir.exists(bundle$app_dir))
  expect_true(file.exists(file.path(bundle$app_dir, "app.R")))
  expect_true(file.exists(file.path(bundle$app_dir, "cerebro_config.rds")))
  expect_identical(
    names(config$crb_file_to_load),
    unname(vapply(names(bundle$fixtures), .generated_app_e2e_id, character(1)))
  )
  expect_identical(
    config$dataset_labels,
    stats::setNames(
      vapply(
        bundle$fixtures,
        function(fixture) fixture$expected$dataset_name,
        character(1)
      ),
      names(config$crb_file_to_load)
    )
  )
  expect_identical(config$initial_dataset, "e2e-basic")
  expect_false(config$show_upload_ui)
  expect_identical(config$welcome_message, "Generated App E2E")
  expect_identical(
    config$point_size,
    stats::setNames(
      rep(6, length(config$crb_file_to_load)),
      names(config$crb_file_to_load)
    )
  )
  expect_identical(
    config$point_opacity,
    stats::setNames(
      rep(1, length(config$crb_file_to_load)),
      names(config$crb_file_to_load)
    )
  )
  expect_false(config$variable_to_compare)

  root <- paste0(normalizePath(bundle$root, winslash = "/"), "/")
  outputs <- c(bundle$published$built, bundle$app_dir)
  expect_true(all(startsWith(
    normalizePath(outputs, winslash = "/"),
    root
  )))
  expect_true(all(file.exists(bundle$published$built)))
  expect_identical(
    basename(unname(bundle$published$built)),
    unname(vapply(
      bundle$fixtures,
      function(fixture) fixture$expected$output_files$crb,
      character(1)
    ))
  )

  mouse_conversion <- utils::read.delim(
    gzfile(file.path(bundle$app_dir, "extdata", "mm10_gene_ID_name.tsv.xz")),
    nrows = 1L,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  expect_identical(
    names(mouse_conversion),
    c("gencode", "ensembl", "havana", "symbol", "type")
  )
  expect_identical(
    unname(unlist(mouse_conversion[1L, ], use.names = FALSE)),
    c(
      "ENSMUSG00000102693.1",
      "ENSMUSG00000102693",
      "OTTMUSG00000049935.1",
      "4933401J01Rik",
      "TEC"
    )
  )
})

test_that("generated private App reaches semantic HTTP readiness", {
  ## Starts an app server.
  skip_on_cran()
  bundle <- generated_app_e2e_bundle()
  server <- generated_app_e2e_server()

  expect_true(server$process$is_alive())
  expect_match(server$base_url, "^http://127[.]0[.]0[.]1:[0-9]+$")
  expect_true(dir.exists(server$library))
  expect_false(dir.exists(file.path(server$library, "CerebroNexus")))

  root <- generated_app_e2e_request("/")
  expect_identical(httr::status_code(root), 200L)
  expect_match(
    httr::content(root, as = "text", encoding = "UTF-8"),
    "CerebroNexus",
    fixed = TRUE
  )

  expect_true(dir.exists(bundle$app_dir))
  expect_true(file.exists(file.path(bundle$app_dir, "app.R")))
})

test_that("generated App serves every versionless Viewer asset locally", {
  root <- generated_app_e2e_request("/")
  html <- httr::content(root, as = "text", encoding = "UTF-8")
  assets <- generated_app_e2e_asset_urls(html)

  expect_true(all(c("custom.css", "fill_height.js") %in% names(assets)))
  expect_true(all(grepl("[?]v=[[:xdigit:]]+", assets)))
  for (asset in assets) {
    response <- generated_app_e2e_request(asset)
    expect_identical(httr::status_code(response), 200L, info = asset)
    expect_gt(length(httr::content(response, as = "raw")), 0L)
  }

  bundle <- generated_app_e2e_bundle()
  relative <- list.files(bundle$app_dir, recursive = TRUE, all.files = TRUE)
  segments <- unlist(strsplit(relative, "[/\\\\]"))
  expect_false(any(grepl("^v[0-9]+(?:[.][0-9]+)+$", segments)))
})


test_that("generated App keeps every data payload outside HTTP resources", {
  bundle <- generated_app_e2e_bundle()
  config <- bundle$config
  private_paths <- unname(config$crb_file_to_load)

  expect_true(all(startsWith(private_paths, "private-data/")))
  expect_true(all(file.exists(file.path(bundle$app_dir, private_paths))))

  private_files <- list.files(
    file.path(bundle$app_dir, "private-data"),
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  spatial_files <- list.files(
    file.path(bundle$app_dir, "spatial-assets"),
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  forbidden <- unique(c(
    "/cerebro_config.rds",
    "/private-data",
    paste0("/", private_paths),
    paste0("/private-data/", private_files),
    paste0("/data/", private_files),
    paste0("/data/", basename(private_paths)),
    paste0("/spatial-assets/", spatial_files)
  ))
  forbidden <- forbidden[nzchar(forbidden)]
  for (path in forbidden) {
    response <- generated_app_e2e_request(path)
    expect_identical(httr::status_code(response), 404L, info = path)
  }
})

test_that("generated App source is package-free and contains no remote assets", {
  bundle <- generated_app_e2e_bundle()
  source <- paste(
    readLines(file.path(bundle$app_dir, "app.R"), warn = FALSE),
    collapse = "\n"
  )

  forbidden <- c(
    "CerebroNexus::",
    "library(CerebroNexus",
    "requireNamespace(\"CerebroNexus\"",
    "requireNamespace('CerebroNexus'",
    "packageVersion(\"CerebroNexus\"",
    "packageVersion('CerebroNexus'"
  )
  expect_false(any(vapply(
    forbidden,
    grepl,
    logical(1),
    x = source,
    fixed = TRUE
  )))

  root <- generated_app_e2e_request("/")
  html <- httr::content(root, as = "text", encoding = "UTF-8")
  urls <- generated_app_e2e_document_urls(html)
  expect_true(length(urls) > 0L)
  expect_false(any(grepl("^(?:https?:)?//", urls)))
  expect_identical(generated_app_e2e_external_resource_urls(), character())
  generated_app_e2e_expect_clean_browser()
})


test_that("dataset selector preserves Builder order and switches exact page contracts", {
  bundle <- generated_app_e2e_bundle()
  options <- generated_app_e2e_selector_options()
  expect_identical(options$labels, unname(bundle$config$dataset_labels))
  expect_identical(options$values, names(bundle$config$crb_file_to_load))

  for (name in names(bundle$fixtures)) {
    fixture <- generated_app_e2e_select_dataset(name)
    expected_value <- .generated_app_e2e_id(name)
    expect_identical(
      generated_app_e2e_value("input", "crb_file_selector"),
      expected_value,
      info = name
    )
    summary <- generated_app_e2e_dataset_summary()
    expect_match(
      summary$cells,
      as.character(fixture$expected$n_cells),
      fixed = TRUE,
      info = name
    )
    expect_match(
      summary$organism,
      generated_app_e2e_organism_label(fixture$expected$organism),
      fixed = TRUE,
      info = name
    )
    expect_identical(
      generated_app_e2e_visible_pages(),
      fixture$expected$visible_pages,
      info = name
    )
    expect_identical(
      intersect(
        generated_app_e2e_visible_pages(),
        fixture$expected$hidden_pages
      ),
      character(),
      info = paste(
        name,
        "must not retain a conditional page from the prior dataset"
      )
    )
  }
  generated_app_e2e_expect_clean_browser()
})

test_that("dataset switching resumes Data info outputs from a conditional page", {
  generated_app_e2e_select_dataset("analysis")
  generated_app_e2e_activate_tab("trajectory")

  fixture <- generated_app_e2e_select_dataset("basic")

  expect_identical(
    generated_app_e2e_value("input", "sidebar"),
    unname(generated_app_e2e_tab_catalog()[["data_info"]])
  )
  summary <- generated_app_e2e_dataset_summary()
  expect_match(
    summary$cells,
    as.character(fixture$expected$n_cells),
    fixed = TRUE
  )
  expect_match(
    summary$organism,
    generated_app_e2e_organism_label(fixture$expected$organism),
    fixed = TRUE
  )
})


test_that("Linked views uses Builder defaults, palette, and complete cell set", {
  fixture <- generated_app_e2e_select_dataset("basic")
  generated_app_e2e_open_linked_views(fixture)
  driver <- generated_app_e2e_driver()
  driver$wait_for_js(
    paste0(
      "document.getElementById('cv-pick-proj').selectize && ",
      "document.getElementById('cv-pick-proj').selectize.getValue().length"
    ),
    timeout = 60000
  )

  expect_equal(
    as.numeric(driver$get_js(
      "$('#cv-ps').data('ionRangeSlider').options.min"
    )),
    1
  )

  expect_identical(
    driver$get_js(
      "document.getElementById('cv-pick-proj').selectize.getValue()[0]"
    ),
    fixture$expected$default_projection
  )
  expect_identical(
    driver$get_js("document.getElementById('cv-pick-color').value"),
    fixture$expected$default_group
  )
  expect_equal(
    as.numeric(driver$get_js("document.getElementById('cv-ps').value")),
    fixture$expected$app_settings$point_size$overview_projection_point_size
  )

  expect_true(any(grepl(
    paste0("^", fixture$expected$default_projection, " \\(expression"),
    generated_app_e2e_linked_titles()
  )))
  expect_match(
    generated_app_e2e_output_text("cv-meta"),
    paste(fixture$expected$n_cells, "cells"),
    fixed = TRUE
  )

  legend <- generated_app_e2e_linked_legend()
  expect_identical(
    vapply(legend, `[[`, character(1), "color"),
    generated_app_e2e_rgb(fixture$expected$palettes$seurat_clusters)
  )
  expect_true(all(vapply(
    names(fixture$expected$palettes$seurat_clusters),
    function(level) {
      any(grepl(
        level,
        vapply(legend, `[[`, character(1), "text"),
        fixed = TRUE
      ))
    },
    logical(1)
  )))
})

test_that("Gene expression renders the exact selected source-gene values", {
  fixture <- generated_app_e2e_select_dataset("basic")
  generated_app_e2e_activate_tab("gene_expression")
  generated_app_e2e_set_input("expression_analysis_mode", "Gene(s)")
  generated_app_e2e_wait_input("expression_genes_input")
  generated_app_e2e_driver()$wait_for_js(
    paste0(
      "(function(){var input=document.getElementById('expression_genes_input');",
      "return !!(input&&input.selectize&&",
      "Object.keys(input.selectize.options).length);})()"
    ),
    timeout = 60000
  )
  Sys.sleep(1.1)
  generated_app_e2e_set_input("expression_genes_input", "GENE001")

  generated_app_e2e_driver()$wait_for_idle(timeout = 60000)
  displayed <- generated_app_e2e_driver()$get_js(
    "(function(){var node=document.getElementById('expression_genes_displayed');return node?(node.textContent||'').trim():'';})()"
  )
  expect_identical(as.character(displayed %||% ""), "")
  generated_app_e2e_wait_plotly("expression_projection")

  source <- SeuratObject::LayerData(
    fixture$object,
    assay = "RNA",
    layer = "data"
  )
  expected <- as.numeric(source["GENE001", ])
  observed <- generated_app_e2e_value(
    "export",
    "expression_levels",
    validate = function(value) {
      length(value) == fixture$expected$n_cells &&
        isTRUE(all.equal(
          sort(as.numeric(value)),
          sort(expected),
          tolerance = 1e-12
        ))
    }
  )
  expect_equal(sort(observed), sort(expected), tolerance = 1e-12)
})

test_that("Groups, gene IDs, colors, and About render source-backed content", {
  fixture <- generated_app_e2e_select_dataset("basic")

  generated_app_e2e_activate_tab("groups")
  generated_app_e2e_wait_input("groups_selected_group")
  generated_app_e2e_set_input("groups_selected_group", "seurat_clusters")
  generated_app_e2e_wait_input("groups_by_other_group_second_group")
  generated_app_e2e_set_input("groups_by_other_group_second_group", "sample")
  generated_app_e2e_set_input("groups_by_other_group_show_table", TRUE)
  groups <- generated_app_e2e_table_text(
    "groups_by_other_group_table",
    contains = c("Alpha", "Beta", "Gamma", "sample_a", "sample_b")
  )
  expect_match(groups, "6", fixed = TRUE)

  generated_app_e2e_activate_tab("about")
  expect_match(
    generated_app_e2e_output_text("about"),
    "Cerebro",
    fixed = TRUE
  )

  ## Opening Gene ID conversion eagerly materializes the complete bundled
  ## dictionary and dominates the whole browser suite. Its exact sidebar
  ## contract is verified here, while the pipeline test reads the generated
  ## dictionary and checks its headers and first source row directly.
  expect_true("gene_id_conversion" %in% generated_app_e2e_visible_pages())
  generated_app_e2e_driver()$wait_for_js(
    "document.querySelector(\"a[href='#shiny-tab-geneIdConversion']\") !== null",
    timeout = 60000
  )

  generated_app_e2e_expect_clean_browser()
})


test_that("offline analysis pages render the retained source tables", {
  fixture <- generated_app_e2e_select_dataset("analysis")

  generated_app_e2e_activate_tab("marker_genes")
  generated_app_e2e_wait_input("marker_genes_selected_method")
  expect_identical(
    generated_app_e2e_value("input", "marker_genes_selected_method"),
    "cerebro_seurat"
  )
  expect_identical(
    generated_app_e2e_value("input", "marker_genes_selected_table"),
    "seurat_clusters"
  )
  generated_app_e2e_set_input("marker_genes_table_filter_switch", TRUE)
  markers <- generated_app_e2e_table_text(
    "marker_genes_table",
    contains = c("GENE001", "GENE002", "GENE003", "Delta", "Epsilon")
  )
  expect_match(markers, "2.4", fixed = TRUE)

  generated_app_e2e_activate_tab("most_expressed_genes")
  generated_app_e2e_wait_input("most_expressed_genes_selected_group")
  expect_identical(
    generated_app_e2e_value("input", "most_expressed_genes_selected_group"),
    fixture$expected$default_group
  )
  generated_app_e2e_set_input(
    "most_expressed_genes_table_filter_switch",
    TRUE
  )
  expressed <- generated_app_e2e_table_text(
    "most_expressed_genes_table",
    contains = c(
      "GENE004",
      "GENE005",
      "GENE006",
      "95",
      "90",
      "82",
      "Stored score"
    )
  )
  # The legacy pct field has no unit provenance; retain master's neutral label.
  expect_match(expressed, "Stored score", fixed = TRUE)

  generated_app_e2e_set_input("most_expressed_genes_metric_type", "mean_expr")
  means <- generated_app_e2e_table_text(
    "most_expressed_genes_table",
    contains = c("GENE004", "GENE005", "GENE006", "3.5", "2.75", "1.5")
  )
  expect_match(means, "Mean", fixed = TRUE)

  generated_app_e2e_activate_tab("enriched_pathways")
  generated_app_e2e_wait_input("enriched_pathways_selected_method")
  expect_identical(
    generated_app_e2e_value("input", "enriched_pathways_selected_method"),
    "offline"
  )
  expect_identical(
    generated_app_e2e_value("input", "enriched_pathways_selected_table"),
    "seurat_clusters"
  )
  generated_app_e2e_set_input("enriched_pathways_table_filter_switch", TRUE)
  pathways <- generated_app_e2e_table_text(
    "enriched_pathways_table",
    contains = c("Pathway A", "Pathway B", "Pathway C", "9.5", "8.25")
  )
  expect_match(pathways, "GENE003", fixed = TRUE)

  generated_app_e2e_activate_tab("extra_material")
  generated_app_e2e_wait_input("extra_material_selected_category")
  expect_identical(
    generated_app_e2e_value("input", "extra_material_selected_category"),
    "tables"
  )
  extra <- generated_app_e2e_table_text(
    "extra_material_table",
    contains = c("cells", "28", "genes", "52", "offline fixture")
  )
  expect_match(extra, "source", fixed = TRUE)
})

test_that("offline trajectory selection renders its complete source lineage", {
  generated_app_e2e_select_dataset("analysis")
  generated_app_e2e_activate_tab("trajectory")
  generated_app_e2e_wait_input("trajectory_selected_method")

  expect_identical(
    generated_app_e2e_value("input", "trajectory_selected_method"),
    "monocle2"
  )
  expect_identical(
    generated_app_e2e_value("input", "trajectory_selected_name"),
    "analysis_lineage"
  )

  generated_app_e2e_wait_plotly("trajectory_projection")
  expect_true(
    isTRUE(generated_app_e2e_driver()$get_js(
      paste0(
        "(function(){var host=document.getElementById('trajectory_projection_cell_view_host');",
        "var canvas=host&&host.querySelector('.cv-canvas-wrap > canvas:not(.cv-mini)');",
        "return !!(canvas&&canvas.width>0&&canvas.height>0);})()"
      )
    ))
  )
  generated_app_e2e_expect_clean_browser()
})


test_that("TCR and HLA fixture renders repertoire data and stored typing", {
  fixture <- generated_app_e2e_select_dataset("immune_tcr_hla")

  generated_app_e2e_activate_tab("immune_repertoire")
  expect_identical(
    generated_app_e2e_value("input", "ir_tabs"),
    "Clonal UMAP"
  )
  receptor <- generated_app_e2e_value(
    "input",
    "ir_p_umap_receptor",
    validate = function(value) identical(value, "TCR")
  )
  expect_identical(receptor, "TCR")
  generated_app_e2e_set_input("ir_p_umap_projection", "tsne")
  generated_app_e2e_wait_plotly("ir_clonalUMAP_projection")
  expect_identical(
    generated_app_e2e_plotly_point_count("ir_clonalUMAP_projection"),
    fixture$expected$n_cells
  )
  .generated_app_e2e_click_inner_tab("ir_tabs", "Abundance")
  expect_match(.generated_app_e2e_select_text("ir_chain"), "TRB", fixed = TRUE)
  generated_app_e2e_set_input("ir_chain", "TRB")
  generated_app_e2e_wait_plotly("ir_plot_clonalAbundance")

  generated_app_e2e_activate_tab("hla_tcr_motifs")
  .generated_app_e2e_click_inner_tab("hla_tabs", "Data & QC")
  coverage <- generated_app_e2e_table_text(
    "hla_coverage_table",
    contains = c("donor1", "donor2", "HLA-A")
  )
  expect_match(coverage, "6", fixed = TRUE)
  typing <- generated_app_e2e_table_text(
    "hla_normalized_preview",
    contains = c("HLA-A*01:01", "HLA-A*02:01", "synthetic")
  )
  expect_match(typing, "2-field", fixed = TRUE)
})

test_that("BCR-only fixture renders IGH repertoire without an HLA page", {
  fixture <- generated_app_e2e_select_dataset("immune_bcr")
  expect_false("hla_tcr_motifs" %in% generated_app_e2e_visible_pages())

  generated_app_e2e_activate_tab("immune_repertoire")
  .generated_app_e2e_click_inner_tab("ir_tabs", "Clonal UMAP")
  receptor <- generated_app_e2e_value(
    "input",
    "ir_p_umap_receptor",
    validate = function(value) identical(value, "BCR")
  )
  expect_identical(receptor, "BCR")
  expect_identical(
    generated_app_e2e_value("input", "ir_p_umap_projection"),
    "umap"
  )
  generated_app_e2e_wait_plotly("ir_clonalUMAP_projection")
  expect_identical(
    generated_app_e2e_plotly_point_count("ir_clonalUMAP_projection"),
    fixture$expected$n_cells
  )
  .generated_app_e2e_click_inner_tab("ir_tabs", "Abundance")
  expect_match(.generated_app_e2e_select_text("ir_chain"), "IGH", fixed = TRUE)
  expect_identical(generated_app_e2e_value("input", "ir_chain"), "both")
  generated_app_e2e_wait_plotly("ir_plot_clonalAbundance")
  generated_app_e2e_expect_clean_browser()
})


test_that("Linked views renders both retained spatial sections and images", {
  fixture <- generated_app_e2e_select_dataset("spatial")
  generated_app_e2e_open_linked_views(fixture)
  driver <- generated_app_e2e_driver()

  driver$wait_for_js(
    "document.getElementById('cv-pick-spatial').selectize != null",
    timeout = 60000
  )
  expect_identical(
    unname(unlist(
      driver$get_js(paste0(
        "Object.keys(document.getElementById('cv-pick-spatial')",
        ".selectize.options)"
      )),
      use.names = FALSE
    )),
    fixture$expected$spatial_sections
  )

  expect_setequal(
    unlist(driver$get_js(
      "document.getElementById('cv-pick-spatial').selectize.getValue()"
    )),
    fixture$expected$spatial_sections
  )
  driver$wait_for_js(
    paste0(
      "document.querySelectorAll(",
      "'.cv-pane:not(.cv-hidden) .cv-ptitle').length>=3"
    ),
    timeout = 60000
  )
  titles <- generated_app_e2e_linked_titles()
  for (section in fixture$expected$spatial_sections) {
    expect_true(any(grepl(section, titles, fixed = TRUE)), info = section)
  }

  ## Linked views uses the first eligible image for each FOV; image selection
  ## belongs to Spatial. Verify both scopes survive in the portable view state.
  driver$wait_for_js(
    "window.cerebroLinkedViewsState.capture().view.spatial_backgrounds.length === 2",
    timeout = 60000
  )
  backgrounds <- driver$get_js(
    "window.cerebroLinkedViewsState.capture().view.spatial_backgrounds"
  )
  expect_setequal(
    vapply(backgrounds, `[[`, character(1), "section"),
    fixture$expected$spatial_sections
  )
  expect_true(all(vapply(
    backgrounds,
    function(x) {
      identical(x$mode, "image") && nzchar(x$image_id)
    },
    logical(1)
  )))

  expect_identical(
    driver$get_js(
      "document.querySelector(\"a[href='#shiny-tab-spatial']\").offsetParent !== null"
    ),
    TRUE
  )
  generated_app_e2e_expect_clean_browser()
})


test_that("Linked views renders Trekker canvas and source-backed QC content", {
  fixture <- generated_app_e2e_select_dataset("trekker")
  generated_app_e2e_open_linked_views(fixture)
  driver <- generated_app_e2e_driver()

  driver$wait_for_js(
    paste0(
      "(function(){var pane=Array.from(document.querySelectorAll(",
      "'.cv-pane:not(.cv-hidden)')).find(function(node){return ",
      "(node.querySelector('.cv-ptitle').textContent||'')",
      ".indexOf('Trekker')!==-1;});var canvas=pane&&pane.querySelector('canvas');",
      "return !!(canvas&&canvas.width>0&&canvas.height>0);})()"
    ),
    timeout = 60000
  )
  expect_true(any(grepl(
    "Trekker",
    generated_app_e2e_linked_titles(),
    fixed = TRUE
  )))

  stats <- .generated_app_e2e_dom_text("cv-tk-stats", c("180", "100"))
  expect_match(stats, "nuclei", ignore.case = TRUE)
  position_classes <- .generated_app_e2e_dom_text(
    "cv-tk-postbl",
    c("90", "50.00%", "Imported")
  )
  expect_match(position_classes, "salvaged", ignore.case = TRUE)
  moran <- .generated_app_e2e_dom_text(
    "cv-tk-morantbl",
    c("EPCAM", "0.7600")
  )
  expect_match(moran, "LUM", fixed = TRUE)

  expect_false(identical(
    driver$get_js(
      "getComputedStyle(document.getElementById('cv-tk-insights')).display"
    ),
    "none"
  ))
  expect_identical(
    driver$get_js(
      "document.querySelector(\"a[href='#shiny-tab-trekker']\").offsetParent !== null"
    ),
    TRUE
  )
  generated_app_e2e_expect_clean_browser()
})
