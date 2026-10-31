generated_app_e2e_tab_catalog <- function() {
  c(
    data_info = "loadData",
    coordinated_views = "coordinated_views",
    groups = "groups",
    gene_expression = "geneExpression",
    gene_id_conversion = "geneIdConversion",
    color_management = "color_management",
    about = "about",
    marker_genes = "markerGenes",
    most_expressed_genes = "mostExpressedGenes",
    enriched_pathways = "enrichedPathways",
    extra_material = "extra_material",
    immune_repertoire = "immune_repertoire",
    trajectory = "trajectory",
    hla_tcr_motifs = "hla_tcr_motifs"
  )
}

generated_app_e2e_open_linked_views <- function(
  fixture,
  timeout = 60000
) {
  generated_app_e2e_activate_tab("coordinated_views", timeout = timeout)
  cells <- as.character(fixture$expected$n_cells)
  generated_app_e2e_driver()$wait_for_js(
    paste0(
      "(function(){var meta=document.getElementById('cv-meta');",
      "var canvases=Array.from(document.querySelectorAll(",
      "'.cv-pane:not(.cv-hidden) canvas[id^=\"cv-cv-\"]'));",
      "return !!(meta && meta.textContent.indexOf(",
      .generated_app_e2e_js_value(cells),
      ")!==-1 && canvases.length && canvases.every(function(canvas){",
      "return canvas.width>0 && canvas.height>0;}));})()"
    ),
    timeout = timeout
  )
  invisible(fixture)
}

generated_app_e2e_linked_titles <- function() {
  value <- generated_app_e2e_driver()$get_js(
    paste0(
      "Array.from(document.querySelectorAll(",
      "'.cv-pane:not(.cv-hidden) .cv-ptitle'))",
      ".map(function(node){return node.textContent.trim();})"
    )
  )
  unname(unlist(value, use.names = FALSE))
}

generated_app_e2e_linked_legend <- function() {
  value <- generated_app_e2e_driver()$get_js(
    paste0(
      "Array.from(document.querySelectorAll('#cv-legend .cv-lg')).map(",
      "function(row){var dot=row.querySelector('.cv-dot');return {",
      "text:(row.textContent||'').trim(),",
      "color:dot?getComputedStyle(dot).backgroundColor:''};})"
    )
  )
  lapply(value, function(row) {
    list(
      text = as.character(row$text),
      color = as.character(row$color)
    )
  })
}

generated_app_e2e_rgb <- function(colors) {
  rgb <- grDevices::col2rgb(colors)
  unname(apply(rgb, 2L, function(value) {
    paste0("rgb(", paste(value, collapse = ", "), ")")
  }))
}

.generated_app_e2e_js_value <- function(value) {
  as.character(jsonlite::toJSON(value, auto_unbox = TRUE))
}

.generated_app_e2e_js_array <- function(value) {
  as.character(jsonlite::toJSON(unname(as.list(value)), auto_unbox = TRUE))
}

generated_app_e2e_value <- function(
  kind,
  id,
  timeout = 60000,
  interval = 100,
  validate = NULL
) {
  kind <- match.arg(kind, c("input", "output", "export"))
  driver <- generated_app_e2e_driver()
  deadline <- Sys.time() + timeout / 1000
  last_error <- NULL
  repeat {
    value <- tryCatch(
      switch(
        kind,
        input = driver$get_value(input = id),
        output = driver$get_value(output = id),
        export = driver$get_value(export = id)
      ),
      error = function(error) {
        last_error <<- error
        NULL
      }
    )
    ready <- !is.null(value)
    if (ready && !is.null(validate)) {
      ready <- tryCatch(
        isTRUE(validate(value)),
        error = function(error) {
          last_error <<- error
          FALSE
        }
      )
    }
    if (ready) {
      return(value)
    }
    if (Sys.time() >= deadline) {
      if (!is.null(last_error)) {
        stop(last_error)
      }
      stop(
        "Timed out waiting for generated App ",
        kind,
        " '",
        id,
        "'.",
        call. = FALSE
      )
    }
    Sys.sleep(interval / 1000)
  }
}

generated_app_e2e_wait_input <- function(id, timeout = 60000) {
  id_js <- .generated_app_e2e_js_value(id)
  driver <- generated_app_e2e_driver()
  tryCatch(
    driver$wait_for_js(
      sprintf("document.getElementById(%s) !== null", id_js),
      timeout = timeout
    ),
    error = function(error) {
      details <- driver$get_js(paste0(
        "Array.from(document.querySelectorAll('.shiny-output-error'))",
        ".map(function(node){return node.textContent.trim();})"
      ))
      stop(
        conditionMessage(error),
        if (length(details)) {
          paste0("\nShiny errors: ", paste(details, collapse = " | "))
        } else {
          ""
        },
        call. = FALSE
      )
    }
  )
  invisible(id)
}

generated_app_e2e_set_input <- function(id, value, timeout = 60000) {
  generated_app_e2e_wait_input(id, timeout = timeout)
  args <- list(value)
  names(args) <- id
  args$wait_ <- FALSE
  do.call(generated_app_e2e_driver()$set_inputs, args)
  generated_app_e2e_value(
    "input",
    id,
    timeout = timeout,
    validate = function(current) {
      isTRUE(all.equal(current, value, check.attributes = FALSE))
    }
  )
  invisible(value)
}

generated_app_e2e_activate_tab <- function(page, timeout = 60000) {
  catalog <- generated_app_e2e_tab_catalog()
  if (!page %in% names(catalog)) {
    stop("Unknown generated App page: ", page, call. = FALSE)
  }
  selector <- sprintf("a[href='#shiny-tab-%s']", catalog[[page]])
  selector_js <- .generated_app_e2e_js_value(selector)
  driver <- generated_app_e2e_driver()
  driver$wait_for_js(
    sprintf("document.querySelector(%s) !== null", selector_js),
    timeout = timeout
  )
  driver$run_js(sprintf("document.querySelector(%s).click()", selector_js))
  driver$wait_for_js(
    paste0(
      "(function(){var link=document.querySelector(",
      selector_js,
      ");return !!(link && link.parentElement && ",
      "link.parentElement.classList.contains('active'));})()"
    ),
    timeout = timeout
  )
  generated_app_e2e_value(
    "input",
    "sidebar",
    timeout = timeout,
    validate = function(current) identical(current, unname(catalog[[page]]))
  )
  invisible(page)
}

generated_app_e2e_visible_pages <- function() {
  catalog <- generated_app_e2e_tab_catalog()
  mapping <- .generated_app_e2e_js_value(as.list(catalog))
  pages <- generated_app_e2e_driver()$get_js(paste0(
    "(function(){var tabs=",
    mapping,
    ";return Object.keys(tabs).filter(function(page){return ",
    "(function(){var link=document.querySelector('a[href=\"#shiny-tab-' + ",
    "tabs[page] + '\"]');return !!(link&&link.offsetParent!==null);})()",
    ";});})()"
  ))
  unname(unlist(pages, use.names = FALSE))
}

.generated_app_e2e_wait_pages <- function(expected, timeout = 60000) {
  catalog <- generated_app_e2e_tab_catalog()
  visible <- .generated_app_e2e_js_array(unname(catalog[expected]))
  hidden <- .generated_app_e2e_js_array(unname(catalog[setdiff(
    names(catalog),
    expected
  )]))
  generated_app_e2e_driver()$wait_for_js(
    paste0(
      "(function(){var visible=",
      visible,
      ",hidden=",
      hidden,
      ";var has=function(tab){var link=document.querySelector(",
      "'a[href=\"#shiny-tab-' + tab + '\"]');",
      "return !!(link&&link.offsetParent!==null);};",
      "return visible.every(has) && hidden.every(function(tab){return !has(tab);});",
      "})()"
    ),
    timeout = timeout
  )
  invisible(expected)
}

generated_app_e2e_output_text <- function(id, timeout = 60000) {
  id_js <- .generated_app_e2e_js_value(id)
  driver <- generated_app_e2e_driver()
  driver$wait_for_js(
    paste0(
      "(function(){var node=document.getElementById(",
      id_js,
      ");return !!(node && (node.textContent||'').trim().length);})()"
    ),
    timeout = timeout
  )
  as.character(driver$get_js(paste0(
    "(function(){var node=document.getElementById(",
    id_js,
    ");return node?(node.textContent||'').trim():'';})()"
  )))
}

generated_app_e2e_dataset_summary <- function() {
  list(
    cells = generated_app_e2e_output_text("load_data_number_of_cells"),
    organism = generated_app_e2e_output_text("load_data_organism")
  )
}

generated_app_e2e_select_dataset <- function(name, timeout = 60000) {
  bundle <- generated_app_e2e_bundle()
  if (!name %in% names(bundle$fixtures)) {
    stop("Unknown generated App fixture: ", name, call. = FALSE)
  }
  fixture <- bundle$fixtures[[name]]
  value <- .generated_app_e2e_id(name)
  driver <- generated_app_e2e_driver()
  generated_app_e2e_wait_input("crb_file_selector", timeout = timeout)
  current <- tryCatch(
    driver$get_value(input = "crb_file_selector"),
    error = function(error) NULL
  )
  if (!identical(current, value)) {
    driver$set_inputs(crb_file_selector = value, wait_ = FALSE)
  }
  generated_app_e2e_value(
    "input",
    "crb_file_selector",
    timeout = timeout,
    validate = function(current) identical(current, value)
  )
  ## A data switch can remove the currently active conditional page. Return to
  ## the always-available Data info page before reading its outputs; otherwise
  ## Shiny correctly leaves those hidden outputs suspended at the prior data
  ## set's values.
  generated_app_e2e_activate_tab("data_info", timeout = timeout)
  generated_app_e2e_value(
    "output",
    "load_data_overview",
    timeout = timeout,
    validate = function(current) {
      html <- if (is.list(current)) current$html else as.character(current)
      any(grepl(as.character(fixture$expected$n_cells), html, fixed = TRUE))
    }
  )
  generated_app_e2e_value(
    "output",
    "load_data_overview",
    timeout = timeout,
    validate = function(current) {
      html <- if (is.list(current)) current$html else as.character(current)
      any(grepl(
        generated_app_e2e_organism_label(fixture$expected$organism),
        html,
        fixed = TRUE
      ))
    }
  )
  .generated_app_e2e_wait_pages(
    fixture$expected$visible_pages,
    timeout = timeout
  )
  fixture
}

generated_app_e2e_selector_options <- function(timeout = 60000) {
  expected <- generated_app_e2e_bundle()$config$crb_file_to_load
  driver <- generated_app_e2e_driver()
  driver$wait_for_js(
    paste0(
      "(function(){var selector=document.getElementById('crb_file_selector');",
      "if(!selector || !selector.selectize)return false;return Object.keys(",
      "selector.selectize.options).length===",
      length(expected),
      ";})()"
    ),
    timeout = timeout
  )
  result <- driver$get_js(paste0(
    "(function(){var selector=document.getElementById('crb_file_selector');",
    "var instance=selector.selectize;var keys=Object.keys(instance.options);",
    "return {labels:keys.map(function(key){return String(",
    "instance.options[key].label);}),values:keys};",
    "})()"
  ))
  list(
    labels = unname(unlist(result$labels, use.names = FALSE)),
    values = unname(unlist(result$values, use.names = FALSE))
  )
}

generated_app_e2e_wait_plotly <- function(id, timeout = 60000) {
  id_js <- .generated_app_e2e_js_value(id)
  driver <- generated_app_e2e_driver()
  tryCatch(
    driver$wait_for_js(
      paste0(
        "(function(){var plot=document.getElementById(",
        id_js,
        ")||document.getElementById(",
        id_js,
        "+'_cell_view_host');var canvas=plot&&plot.querySelector('canvas:not(.cv-mini)');",
        "return !!(plot && ((plot.data && plot.data.some(function(trace){",
        "return trace.x && trace.x.length>0;})) || ",
        "Number(plot.dataset.pointCount)>0 || ",
        "(canvas && canvas.width>0 && canvas.height>0)));})()"
      ),
      timeout = timeout
    ),
    error = function(error) {
      details <- driver$get_js(paste0(
        "Array.from(document.querySelectorAll('.shiny-output-error'))",
        ".map(function(node){return node.textContent.trim();})"
      ))
      stop(
        conditionMessage(error),
        if (length(details)) {
          paste0("\nShiny errors: ", paste(details, collapse = " | "))
        } else {
          ""
        },
        call. = FALSE
      )
    }
  )
  invisible(id)
}

generated_app_e2e_plotly_point_count <- function(id) {
  id_js <- .generated_app_e2e_js_value(id)
  value <- generated_app_e2e_driver()$get_js(paste0(
    "(function(){var plot=document.getElementById(",
    id_js,
    ")||document.getElementById(",
    id_js,
    "+'_cell_view_host');if(!plot)return 0;if(Number(plot.dataset.pointCount)>0)",
    "return Number(plot.dataset.pointCount);if(!plot.data)return 0;",
    "return plot.data.reduce(",
    "function(total,trace){var mode=String(trace.mode||'');return total+",
    "(mode.indexOf('markers')!==-1&&trace.x?trace.x.length:0);},0);})()"
  ))
  as.integer(value)
}

generated_app_e2e_plotly_coordinates <- function(id) {
  id_js <- .generated_app_e2e_js_value(id)
  value <- generated_app_e2e_driver()$get_js(paste0(
    "(function(){var plot=document.getElementById(",
    id_js,
    ");if(!plot || !plot.data)return [];return plot.data.reduce(",
    "function(points,trace){var mode=String(trace.mode||'');if(",
    "mode.indexOf('markers')===-1)return points;var xs=Array.from(trace.x||[]);",
    "var ys=Array.from(trace.y||[]);return points.concat(xs.map(",
    "function(x,index){return [Number(x),Number(ys[index])];}));},[]);})()"
  ))
  coordinates <- do.call(
    rbind,
    lapply(value, function(point) {
      as.numeric(unlist(point, use.names = FALSE))
    })
  )
  if (is.null(coordinates)) {
    return(matrix(numeric(), ncol = 2L))
  }
  unname(coordinates)
}

generated_app_e2e_external_resource_urls <- function() {
  urls <- generated_app_e2e_driver()$get_js(
    paste0(
      "performance.getEntriesByType('resource').map(function(entry){",
      "return entry.name;}).filter(function(url){return !(",
      "url.indexOf(window.location.origin)===0||url.indexOf('data:')===0||",
      "url.indexOf('blob:')===0);})"
    )
  )
  unique(as.character(unlist(urls, use.names = FALSE)))
}

generated_app_e2e_table_text <- function(
  id,
  contains = character(),
  timeout = 60000
) {
  id_js <- .generated_app_e2e_js_value(id)
  contains_js <- .generated_app_e2e_js_array(as.character(contains))
  table_expression <- paste0(
    "var host=document.getElementById(",
    id_js,
    ");if(!host)return null;var tables=host.matches('table')?[host]:",
    "Array.from(host.querySelectorAll('table'));var table=tables.find(",
    "function(candidate){return candidate.querySelector('tbody tr');});"
  )
  # DT may replace its table between the condition and the subsequent read.
  # Recheck the captured contents; never accept an empty transient redraw.
  for (attempt in seq_len(3L)) {
    generated_app_e2e_driver()$wait_for_js(
      paste0(
        "(function(){",
        table_expression,
        "if(!table || !table.querySelector('tbody tr'))return false;",
        "var text=table.textContent||'';var expected=",
        contains_js,
        ";return expected.every(function(value){return text.indexOf(value)!==-1;});",
        "})()"
      ),
      timeout = timeout
    )
    text <- as.character(generated_app_e2e_driver()$get_js(paste0(
      "(function(){",
      table_expression,
      "return table?table.textContent:'';})()"
    )))
    if (
      length(text) == 1L &&
        all(vapply(
          contains,
          function(value) {
            grepl(value, text, fixed = TRUE)
          },
          logical(1)
        ))
    ) {
      return(text)
    }
  }
  stop("The rendered table changed before its contents could be read: ", id)
}

generated_app_e2e_browser_failures <- function(
  logs = generated_app_e2e_driver()$get_logs()
) {
  required <- c("location", "level", "message")
  if (!is.data.frame(logs) || !all(required %in% names(logs))) {
    stop(
      "Browser logs must contain location, level, and message columns.",
      call. = FALSE
    )
  }
  browser_failure <-
    as.character(logs$location) == "chromote" &
    tolower(as.character(logs$level)) %in%
      c(
        "error",
        "warning",
        "assert",
        "throw"
      )
  logs[browser_failure, , drop = FALSE]
}

generated_app_e2e_expect_clean_browser <- function() {
  failures <- generated_app_e2e_browser_failures()
  testthat::expect_identical(
    nrow(failures),
    0L,
    info = paste(as.character(failures$message), collapse = "\n")
  )
  testthat::expect_identical(
    generated_app_e2e_external_resource_urls(),
    character()
  )
  invisible(failures)
}

generated_app_e2e_organism_label <- function(value) {
  switch(value, hg = "Human", mm = "Mouse", value)
}
