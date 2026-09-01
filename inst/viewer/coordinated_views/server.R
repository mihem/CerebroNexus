##----------------------------------------------------------------------------##
## Tab: Linked views — server.
##
## Sourced into the main server scope (source(..., local = TRUE)), so
## `input`, `output`, `session` and `data_set` are in scope.
##
## Responsibility: build ONE per-dataset bundle describing the cells, their
## categorical groupings, and every available "space" (a named 2-D layout of the
## SAME cells: umap / spatial / clone), plus per-cell clone identity, and push it
## to www/cell_views.js. All interaction (linked brushing, highlight, readout) is
## then client-side. The engine keys selection on cell index, so a brush in any
## panel highlights the same cells in every other panel — across modalities.
##----------------------------------------------------------------------------##

## Per-dataset bundle builders (pure functions; see bundle.R). Sourced first so
## cv_build_bundle() and every cv_build_* / cv_* helper is in scope for the
## reactive below. Kept in a separate file so the builders can be unit-tested
## without a running session (tests/testthat/test-coordinated-views.R).
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/coordinated_views/bundle.R"
  ),
  local = TRUE
)

## Always resolves to something sendable: the bundle, or a list(error = <text>)
## describing why this data set has no linked views. Never NULL — see the observe
## below for why silence is the one outcome we cannot afford.
## How many times the bundle has actually been built this session. A plain
## environment rather than a reactiveVal: it is written from inside the reactive
## that it counts, and a reactive value would make that a dependency on itself.
## Read back through exportTestValues -- "was any work done for a tab nobody
## opened" is otherwise invisible from the outside, which is how it went
## unnoticed in the first place.
coordviews_build_log <- new.env(parent = emptyenv())
coordviews_build_log$n <- 0L
coordviews_build_log$sent_n <- 0L

coordviews_bundle <- reactive({
  req(!is.null(data_set()))
  coordviews_build_log$n <- coordviews_build_log$n + 1L
  tryCatch(
    {
      b <- cv_build_bundle(data_set())
      if (is.null(b)) {
        list(
          error = paste(
            "This data set carries no dimensional reduction, so there is",
            "nothing to link its modalities on."
          )
        )
      } else {
        b
      }
    },
    error = function(e) {
      ## The message goes to the console for debugging; the client gets a
      ## generic one (an internal error string is neither useful nor safe to
      ## render in the browser).
      warning(
        "Linked views bundle failed: ",
        conditionMessage(e),
        call. = FALSE
      )
      list(error = "Linked views could not be built for this data set.")
    }
  )
})

## The bundle when it actually built; NULL otherwise. Server-side consumers
## (gene vectors, histology controls) need real cells, not an error payload.
cv_ok <- function(b) {
  if (is.null(b) || !is.null(b$error)) NULL else b
}

## Palette edits do not change cells, coordinates, or available spaces. Keep the
## expensive bundle reactive independent of Color management and send only the
## categorical colours that changed.
coordviews_color_patch <- reactive({
  b <- cv_ok(coordviews_bundle())
  req(!is.null(b))
  colors <- tryCatch(reactive_colors(), error = function(e) NULL)
  cv_color_patch(b, colors)
})
## Nothing is built or sent until the user actually opens the tab.
##
## `coordviews_bundle()` walks every cell of the loaded object -- reductions,
## spatial coordinates, the immune repertoire -- and the result is sizeable.
## Doing that on connect made every session pay for a tab most of them never
## open; colour edits now stay in the small patch reactive above.
##
## The client reports whether the workspace is on screen (`coordviews_visible`)
## -- see www/cell_views.js for why that signal rather than the sidebar's
## active-tab input. The gate is CURRENT visibility, not "was opened once":
## a sticky flag stopped the first build but left every later one, so opening
## the tab, going to Color management and changing a colour rebuilt and re-sent
## the whole bundle to a hidden page.
coordviews_visible <- reactiveVal(FALSE)
observeEvent(input[["coordviews_visible"]], {
  coordviews_visible(isTRUE(input[["coordviews_visible"]]))
})

## Push the full bundle while visible and when the data set changes. The error
## payload is pushed too, and that is the point: staying silent would leave the
## PREVIOUS data set's panels on screen, presenting one data set's cells as
## another's. Colour changes use the patch observer below.
##
## The req() has to come FIRST. It is what keeps this observer from taking a
## dependency on the bundle while hidden -- nothing is built until the user
## returns to the workspace.
observe(
  {
    req(coordviews_visible())
    bundle <- coordviews_bundle()
    if (identical(coordviews_build_log$sent_n, coordviews_build_log$n)) {
      return()
    }
    if (is.null(bundle$error)) {
      bundle <- cv_apply_color_patch(bundle, isolate(coordviews_color_patch()))
    }
    session$sendCustomMessage("coordviews_data", bundle)
    coordviews_build_log$sent_n <- coordviews_build_log$n
  },
  priority = 1
)

observeEvent(
  reactive_colors(),
  {
    if (coordviews_build_log$sent_n == 0L) {
      return()
    }
    session$sendCustomMessage("coordviews_colors", coordviews_color_patch())
  },
  ignoreInit = TRUE
)

##----------------------------------------------------------------------------##
## Selected-cell detail views — mirror the Overview/Projection tab. When cells
## are selected in ANY linked panel, the client reports their barcodes in
## `coordviews_selection`; we render a "Plot of selected cells" (bar chart for a
## categorical variable / violin for a numeric one, selected vs. rest) and a
## "Table of selected cells" (their meta data), exactly like the Projection tab.
## The client-side composition + top-clonotype readout is unaffected and stays.
##----------------------------------------------------------------------------##
coordviews_selected_barcodes <- reactive({
  sel <- input[["coordviews_selection"]]
  if (is.null(sel) || !length(sel)) {
    return(NULL)
  }
  as.character(sel)
})

## Boxes appear only while a selection exists (same gating as the Overview tab).
## Shiny re-runs this renderUI on every selection change (same as Overview), which
## would reset the "Variable to compare" dropdown each lasso — so seed it from the
## current input via isolate(): the user's choice survives the rebuild, and reading
## it isolated adds no extra dependency (no rebuild when only the variable changes).
## The `cv-rise` class + its delay continue the stagger the client-side readout
## starts (composition 0ms, clonotypes 60ms), so everything a selection produces
## arrives as one motion instead of three boxes popping in independently. It is a
## CSS animation rather than a transition because Shiny rebuilds these two from
## scratch on every selection change — a new element has nothing to transition
## from. Definition: www/coordviews.css, @keyframes cvRise.
output[["coordviews_selected_cells_UI"]] <- renderUI({
  req(coordviews_selected_barcodes())
  meta_cols <- colnames(getMetaData())
  tagList(
    fluidRow(
      class = "cv-rise",
      style = "--cv-rise-delay:120ms",
      cerebroBox(
        title = tagList(
          boxTitle("Plot of selected cells"),
          cerebroInfoButton("coordviews_selected_cells_plot_info")
        ),
        tagList(
          selectInput(
            "coordviews_selected_cells_plot_variable",
            label = "Variable to compare:",
            choices = meta_cols[!meta_cols %in% c("cell_barcode")],
            selected = isolate(input[[
              "coordviews_selected_cells_plot_variable"
            ]])
          ),
          plotly::plotlyOutput("coordviews_selected_cells_plot")
        )
      )
    ),
    fluidRow(
      class = "cv-rise",
      style = "--cv-rise-delay:180ms",
      cerebroBox(
        title = tagList(
          boxTitle("Table of selected cells"),
          cerebroInfoButton("coordviews_selected_cells_table_info")
        ),
        tagList(
          shinyWidgets::materialSwitch(
            inputId = "coordviews_selected_cells_table_number_formatting",
            label = "Automatically format numbers:",
            value = TRUE,
            status = "primary",
            inline = TRUE
          ),
          shinyWidgets::materialSwitch(
            inputId = "coordviews_selected_cells_table_color_highlighting",
            label = "Highlight values with colors:",
            value = TRUE,
            status = "primary",
            inline = TRUE
          ),
          DT::dataTableOutput("coordviews_selected_cells_table")
        )
      )
    )
  )
})

## Plot: categorical variable -> bar of selected-cell counts per group (coloured
## by the SAME assignColorsToGroups() the Projection tab uses); numeric variable
## -> violin/box of selected vs. not-selected. Barcode-keyed, so it needs no
## projection coordinates.
output[["coordviews_selected_cells_plot"]] <- plotly::renderPlotly({
  sel <- coordviews_selected_barcodes()
  req(sel, input[["coordviews_selected_cells_plot_variable"]])
  cells_df <- cv_canonical_metadata(getMetaData())
  var <- input[["coordviews_selected_cells_plot_variable"]]
  req(var %in% colnames(cells_df))
  is_selected <- cells_df[["cell_barcode"]] %in% sel
  ## categorical -> bar chart of counts within the selection
  if (is.factor(cells_df[[var]]) || is.character(cells_df[[var]])) {
    sub <- cells_df[is_selected, , drop = FALSE]
    if (nrow(sub) > 0) {
      counts <- sub %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(var))) %>%
        dplyr::tally() %>%
        dplyr::ungroup()
    } else {
      lv <- if (var %in% getGroups()) {
        getGroupLevels(var)
      } else {
        unique(cells_df[[var]])
      }
      counts <- data.frame(x = lv, n = 0L)
      colnames(counts)[1] <- var
    }
    colors_for_groups <- assignColorsToGroups(counts, var)
    x_vals <- as.character(counts[[1]])
    plot <- plotly::plot_ly(
      x = x_vals,
      y = counts[[2]],
      type = "bar",
      color = x_vals,
      colors = colors_for_groups,
      showlegend = FALSE,
      hoverinfo = "y"
    )
    y_axis_title <- "Number of cells"
    ## numeric -> violin/box of selected vs. not selected
  } else if (is.numeric(cells_df[[var]])) {
    grp <- factor(
      ifelse(is_selected, "selected", "not selected"),
      levels = c("selected", "not selected")
    )
    plot <- plotly::plot_ly(
      x = grp,
      y = cells_df[[var]],
      type = "violin",
      box = list(visible = TRUE),
      meanline = list(visible = TRUE),
      color = grp,
      colors = setNames(
        c("#e74c3c", "#7f8c8d"),
        c("selected", "not selected")
      ),
      showlegend = FALSE,
      hoverinfo = "y",
      marker = list(size = 5)
    )
    y_axis_title <- var
  } else {
    return(NULL)
  }
  plot %>%
    plotly::layout(
      title = "",
      xaxis = list(title = "", mirror = TRUE, showline = TRUE),
      yaxis = list(
        title = y_axis_title,
        tickformat = ",.0f",
        hoverformat = ",.0f",
        mirror = TRUE,
        showline = TRUE
      ),
      hovermode = "compare"
    )
})

## Table: meta data of the selected cells (same prettifyTable options as the
## Projection tab's table). Filtered by barcode; empty skeleton when nothing hit.
output[["coordviews_selected_cells_table"]] <- DT::renderDataTable({
  sel <- coordviews_selected_barcodes()
  if (is.null(sel)) {
    return(
      cv_canonical_metadata(getMetaData()) %>%
        dplyr::slice(0) %>%
        prepareEmptyTable()
    )
  }
  cells_df <- cv_selected_metadata(getMetaData(), sel) %>%
    dplyr::select(cell_barcode, dplyr::everything())
  if (nrow(cells_df) == 0) {
    cv_canonical_metadata(getMetaData()) %>%
      dplyr::slice(0) %>%
      prepareEmptyTable()
  } else {
    prettifyTable(
      cells_df,
      filter = list(position = "top", clear = TRUE),
      dom = "Brtlip",
      show_buttons = TRUE,
      number_formatting = input[[
        "coordviews_selected_cells_table_number_formatting"
      ]],
      color_highlighting = input[[
        "coordviews_selected_cells_table_color_highlighting"
      ]],
      hide_long_columns = TRUE,
      download_file_name = "linked_views_selected_cells"
    )
  }
})

## Info modals for the two panels.
observeEvent(input[["coordviews_selected_cells_plot_info"]], {
  showModal(modalDialog(
    title = "Plot of selected cells",
    easyClose = TRUE,
    footer = NULL,
    size = "l",
    p(
      "Depending on the variable chosen, this plot summarises the cells you ",
      "selected across the linked panels. A categorical variable (e.g. ",
      "'cluster' or 'sample') gives a bar chart of how many selected cells fall ",
      "in each group, coloured exactly as in the panels. A continuous variable ",
      "(e.g. number of transcripts) gives a violin/box plot comparing its ",
      "distribution in the selected vs. non-selected cells."
    )
  ))
})
observeEvent(input[["coordviews_selected_cells_table_info"]], {
  showModal(modalDialog(
    title = "Table of selected cells",
    easyClose = TRUE,
    footer = NULL,
    size = "l",
    p(
      "Meta data for the cells selected across the linked panels (some columns ",
      "may be hidden — check the 'Column visibility' button). The table can be ",
      "downloaded as CSV or Excel for further analysis."
    )
  ))
})

##----------------------------------------------------------------------------##
## Gene-expression colouring (the richer "Colour by"). The gene pickers are
## whole-transcriptome server-side searches (same helper the Spatial/Gene tabs
## use); on change the server returns a 0-255 vector aligned to the bundle's
## cell order, and the client colours the points (viridis, or RGB blend).
##----------------------------------------------------------------------------##
cv_has_expression <- function() {
  isTRUE(tryCatch(nrow(data_set()$expression) > 0, error = function(e) FALSE))
}

## Pull one gene aligned to `cells`. Returns NULL if unavailable.
cv_gene_values <- function(gene, cells) {
  if (is.null(gene) || !nzchar(gene)) {
    return(NULL)
  }
  cells <- as.character(cells)
  m <- tryCatch(
    data_set()$getExpressionMatrix(cells = cells, genes = gene),
    error = function(e) NULL
  )
  if (is.null(m)) {
    return(NULL)
  }
  if (is.null(dim(m))) {
    v <- as.numeric(m)
  } else {
    cn <- colnames(m)
    v <- if (!is.null(cn)) {
      as.numeric(m[1, match(cells, cn)])
    } else {
      as.numeric(m[1, ])
    }
  }
  v[is.na(v)] <- 0
  v
}

cv_scale_gene_values <- function(v) {
  mx <- suppressWarnings(max(v, na.rm = TRUE))
  q <- if (is.finite(mx) && mx > 0) {
    as.integer(round(v / mx * 255))
  } else {
    rep(0L, length(v))
  }
  q[is.na(q)] <- 0L
  list(v = q, max = round(mx, 3))
}

cv_gene_vector <- function(gene, cells) {
  v <- cv_gene_values(gene, cells)
  if (is.null(v)) {
    return(NULL)
  }
  cv_scale_gene_values(v)
}

serverSideGeneSelector(
  session,
  "coordviews_gene",
  active = function() cv_has_expression()
)
lapply(
  c("coordviews_gene_r", "coordviews_gene_g", "coordviews_gene_b"),
  function(channel_id) {
    serverSideGeneSelector(session, channel_id, active = cv_has_expression)
  }
)

observeEvent(
  list(
    input[["coordviews_gene"]],
    input[["coordviews_expression_mode"]]
  ),
  {
    req(coordviews_visible())
    b <- cv_ok(coordviews_bundle())
    genes <- unique(input[["coordviews_gene"]])
    genes <- genes[!is.na(genes) & nzchar(genes)]
    if (is.null(b) || length(genes) == 0) {
      session$sendCustomMessage(
        "coordviews_geneval",
        list(gene = "", ok = FALSE)
      )
      session$sendCustomMessage("coordviews_genepanels", list(ok = FALSE))
      return()
    }
    values <- lapply(genes, cv_gene_values, cells = b$cells)
    keep <- !vapply(values, is.null, logical(1))
    genes <- genes[keep]
    values <- values[keep]
    if (length(values) == 0) {
      session$sendCustomMessage(
        "coordviews_geneval",
        list(gene = "", ok = FALSE)
      )
      return()
    }
    mode <- input[["coordviews_expression_mode"]]
    if (identical(mode, "panels")) {
      global_max <- max(unlist(values), na.rm = TRUE)
      scaled <- lapply(values, function(v) {
        if (is.finite(global_max) && global_max > 0) {
          as.integer(round(v / global_max * 255))
        } else {
          rep(0L, length(v))
        }
      })
      session$sendCustomMessage(
        "coordviews_genepanels",
        list(
          ok = TRUE,
          genes = genes,
          values = scaled,
          max = round(global_max, 3)
        )
      )
    } else {
      mean_values <- Reduce(`+`, values) / length(values)
      gv <- cv_scale_gene_values(mean_values)
      session$sendCustomMessage(
        "coordviews_geneval",
        list(
          gene = if (length(genes) == 1) {
            genes[[1]]
          } else {
            paste0("Mean expression (", length(genes), " genes)")
          },
          ok = TRUE,
          v = gv$v,
          max = gv$max
        )
      )
    }
  }
)

## RGB co-expression: one gene per channel, each scaled independently; an empty
## channel is all-zero. Recompute whenever any of the three genes changes.
## ignoreInit: the event value is a LIST, and a list of three NULLs is not NULL,
## so ignoreNULL does not suppress the initial run the way it does for the
## single-gene selector above. Without it this observer built the whole bundle
## on connect -- the one place the laziness leaked, and invisible from outside
## because the bundle was built but never sent.
observeEvent(
  list(
    input[["coordviews_gene_r"]],
    input[["coordviews_gene_g"]],
    input[["coordviews_gene_b"]]
  ),
  {
    req(coordviews_visible())
    b <- cv_ok(coordviews_bundle())
    if (is.null(b)) {
      return()
    }
    zero <- rep(0L, b$n)
    chan <- function(id) {
      g <- input[[id]]
      if (is.null(g) || !nzchar(g)) {
        return(list(v = zero, gene = ""))
      }
      gv <- cv_gene_vector(g, b$cells)
      if (is.null(gv)) list(v = zero, gene = "") else list(v = gv$v, gene = g)
    }
    r <- chan("coordviews_gene_r")
    g <- chan("coordviews_gene_g")
    bl <- chan("coordviews_gene_b")
    if (!nzchar(r$gene) && !nzchar(g$gene) && !nzchar(bl$gene)) {
      session$sendCustomMessage("coordviews_rgbval", list(ok = FALSE))
      return()
    }
    session$sendCustomMessage(
      "coordviews_rgbval",
      list(
        ok = TRUE,
        r = r$v,
        g = g$v,
        b = bl$v,
        genes = c(r$gene, g$gene, bl$gene)
      )
    )
  },
  ignoreInit = TRUE
)

##----------------------------------------------------------------------------##
## Spatial histology-image controls — shown only when the current data set's
## spatial entry carries an embedded image. The controls are client-owned
## (cv-img- ids), wired by cell_views.js; adjusting them re-styles the image on
## the canvas instantly and never round-trips to the server.
##----------------------------------------------------------------------------##
output[["coordviews_image_ui"]] <- renderUI({
  ## suspendWhenHidden = FALSE below keeps these controls in the DOM for
  ## cell_views.js to wire, which also means this output runs while the tab is
  ## hidden -- and it reads the bundle. Without the same gate as the push, it
  ## would build the bundle on connect on its own and the laziness would be
  ## worth nothing.
  req(coordviews_visible())
  b <- cv_ok(coordviews_bundle())
  ## Two separate questions, and conflating them is what went wrong before.
  ##
  ## DOES a bar exist? Any section carrying an image is enough. A space's own
  ## `image` is its FIRST section's, so a data set whose first section has no
  ## histology used to render no bar at all, and switching to one that does
  ## revealed an empty box.
  ##
  ## What does it open SHOWING? The section that is on screen, which is the
  ## first one. Scanning for "any image" and keeping the last one found seeded
  ## the controls -- values, and the slider ranges built from the coordinate
  ## span -- from a section the user is not looking at.
  img <- NULL
  seed <- NULL
  if (!is.null(b)) {
    for (s in b$spaces) {
      if (!is.null(s$image)) {
        img <- s$image
        if (is.null(seed)) {
          seed <- s$image
        }
      }
      for (smp in (s$samples %||% list())) {
        if (!is.null(smp$image)) {
          img <- smp$image
        }
      }
    }
  }
  if (is.null(img)) {
    return(NULL)
  }
  ## The displayed section's image when it has one; otherwise any, purely so the
  ## controls exist for the client to re-seed on the first switch.
  img <- seed %||% img
  ## Seed the controls from the alignment preset so an external image (Visium
  ## H&E) opens PRE-ALIGNED, exactly as the Spatial tab does. Move sliders are in
  ## DATA units, ranged to the coordinate span so the nudge is meaningful.
  pr <- img$preset
  span <- img$coord_span
  if (is.null(span) || length(span) < 2) {
    span <- c(400, 400)
  }
  rng <- function(id, label, mn, mx, val, step) {
    div(
      class = "cv-img-range",
      tags$input(
        type = "range",
        id = id,
        min = mn,
        max = mx,
        value = val,
        step = step
      ),
      tags$input(
        type = "number",
        id = paste0(id, "-number"),
        class = "cv-img-number",
        min = mn,
        max = mx,
        value = val,
        step = step,
        `aria-label` = paste(label, "value")
      )
    )
  }
  chk <- function(id, label, on) {
    tags$label(
      class = "cv-chk",
      if (isTRUE(on)) {
        tags$input(type = "checkbox", id = id, checked = "checked")
      } else {
        tags$input(type = "checkbox", id = id)
      },
      label
    )
  }
  ## The range has to CONTAIN the value it is being asked to show. A preset is a
  ## calibration someone measured; a slider ranged on the coordinate span alone
  ## clamps anything outside it, and because the whole bar is read back together
  ## the clamped number is then written into the state by an unrelated nudge --
  ## the alignment silently becoming one nobody chose.
  sx <- signif(max(span[1] * 1.2, abs(pr$offsetX %||% 0) * 1.1), 3)
  sy <- signif(max(span[2] * 1.2, abs(pr$offsetY %||% 0) * 1.1), 3)
  scale_lo <- min(
    0.3,
    (pr$scaleX %||% 1) * 0.9,
    (pr$scaleY %||% pr$scaleX %||% 1) * 0.9
  )
  scale_hi <- max(
    3,
    (pr$scaleX %||% 1) * 1.1,
    (pr$scaleY %||% pr$scaleX %||% 1) * 1.1
  )
  div(
    class = "cv-imgbar",
    div(
      class = "cv-imgbar-heading",
      tags$span(class = "cv-imgbar-title", "Alignment")
    ),
    div(
      class = "cv-img-ctl",
      tags$label(`for` = "cv-img-opacity-number", "Opacity"),
      rng("cv-img-opacity", "Opacity", 0, 1, pr$opacity %||% 0.6, "any")
    ),
    div(
      class = "cv-img-ctl",
      tags$label(`for` = "cv-img-offx-number", "Move X"),
      rng("cv-img-offx", "Move X", -sx, sx, pr$offsetX %||% 0, "any")
    ),
    div(
      class = "cv-img-ctl",
      tags$label(`for` = "cv-img-offy-number", "Move Y"),
      rng("cv-img-offy", "Move Y", -sy, sy, pr$offsetY %||% 0, "any")
    ),
    ## Two scales, not one. A preset can carry scaleX != scaleY -- a calibration
    ## that is genuinely non-uniform -- and a single slider had to pick a number
    ## for both, so touching ANY control in this bar silently squared the image
    ## up and threw that calibration away. Locked together by default, since a
    ## uniform scale is the common case and two sliders to drag is a worse one.
    div(
      class = "cv-img-ctl",
      tags$label(`for` = "cv-img-scalex-number", "Scale X"),
      rng(
        "cv-img-scalex",
        "Scale X",
        scale_lo,
        scale_hi,
        pr$scaleX %||% 1,
        "any"
      )
    ),
    div(
      class = "cv-img-ctl",
      tags$label(`for` = "cv-img-scaley-number", "Scale Y"),
      rng(
        "cv-img-scaley",
        "Scale Y",
        scale_lo,
        scale_hi,
        pr$scaleY %||% pr$scaleX %||% 1,
        "any"
      )
    ),
    div(
      class = "cv-img-ctl",
      tags$label(`for` = "cv-img-rotate-number", "Rotate"),
      rng(
        "cv-img-rotate",
        "Rotate",
        -180,
        180,
        pr$rotation %||% 0,
        "any"
      )
    ),
    div(
      class = "cv-img-checks",
      chk("cv-img-show", "Show", TRUE),
      chk(
        "cv-img-lock",
        "Lock aspect",
        isTRUE(is.null(pr$scaleY) || identical(pr$scaleY, pr$scaleX))
      ),
      chk("cv-img-flipx", "Flip X", isTRUE(pr$flipX)),
      chk("cv-img-flipy", "Flip Y", isTRUE(pr$flipY))
    ),
    ## Alignment is fiddly and easy to lose; the preset is the state the data set
    ## shipped with, so there has to be a way back to it that is not "reload".
    tags$button(
      type = "button",
      id = "cv-img-reset",
      class = "cv-imgbar-reset",
      "Reset to preset"
    )
  )
})
outputOptions(output, "coordviews_image_ui", suspendWhenHidden = FALSE)

##----------------------------------------------------------------------------##
## Single-cell detail card — the complete meta row for one clicked cell.
##
## The bundle deliberately does not carry this: it holds categorical LEVELS and
## numerics quantised for colouring, which is right for drawing and wrong for
## reading. A card that shows a cell's meta data should show the values the data
## set actually holds, so the client asks for the row when a card opens (one
## small round-trip per click, and the card is already on screen meanwhile).
##----------------------------------------------------------------------------##
cv_fmt_value <- function(v) {
  if (length(v) != 1 || is.na(v)) {
    return("NA")
  }
  if (is.numeric(v)) {
    ## integers plain, otherwise enough decimals to stay meaningful — the same
    ## reading the "Table of selected cells" gives, without its column-wide
    ## type inference (a single value carries no column to infer from).
    if (abs(v - round(v)) < 1e-9) {
      return(format(round(v), big.mark = ",", scientific = FALSE, trim = TRUE))
    }
    return(format(
      signif(v, 5),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    ))
  }
  as.character(v)
}

observeEvent(input[["coordviews_cell_detail"]], {
  bc <- input[["coordviews_cell_detail"]]
  if (is.null(bc) || !nzchar(bc)) {
    return()
  }
  md <- tryCatch(
    cv_cell_metadata(getMetaData(), bc),
    error = function(e) NULL
  )
  if (is.null(md)) {
    return()
  }
  cols <- setdiff(colnames(md), "cell_barcode")
  rows <- lapply(cols, function(cn) {
    list(k = cn, v = cv_fmt_value(md[[cn]][1L]))
  })
  session$sendCustomMessage(
    "coordviews_cell_meta",
    list(cell = as.character(bc), rows = rows)
  )
})

##----------------------------------------------------------------------------##
## Info modal
##----------------------------------------------------------------------------##
observeEvent(input[["coordinated_views_info"]], {
  showModal(modalDialog(
    title = "Linked views",
    easyClose = TRUE,
    footer = NULL,
    size = "l",
    tagList(
      tags$p(
        "Every modality is a layout of the ",
        tags$b("same cells"),
        ": the ",
        tags$b("selected projections"),
        " (UMAP, t-SNE, PCA or any other embedding), the ",
        tags$b("Spatial"),
        " map (physical positions, when the data set carries them), and ",
        tags$b("Clonal expansion"),
        " (each receptor-bearing cell placed by its clone's rank and size)."
      ),
      tags$p(
        tags$b("Coordinated selection"),
        " — lasso-drag in any ",
        tags$i("2-D"),
        " panel and the same cells highlight in ",
        tags$i("every"),
        " panel, because the selection is keyed on the cell, not on a panel's ",
        "coordinates. Select a cluster in any projection to see where those cells sit in ",
        "tissue and which clonotypes they carry; select an expanded clone to see ",
        "where its cells fall across every selected embedding. A 3-D panel is for navigating: with ",
        "depth on screen, what a lasso encloses depends on the viewing angle, so ",
        "those panels display a selection rather than make one."
      ),
      tags$p(
        tags$b("Readout"),
        " — the selection's cell-type composition and its top clonotypes ",
        "(CDR3, clone size, share of the selection) update live. Click a ",
        "clonotype row to select all of its cells across every panel."
      ),
      tags$p(
        tags$b("Colouring"),
        " — every meta data column is available, exactly as on the Projection ",
        "tab: the grouping variables, any other categorical column, and every ",
        "numeric one (number of transcripts, percent mitochondrial, scores) on a ",
        "continuous scale. Single genes and three-gene co-expression are there too."
      ),
      tags$p(
        tags$b("Navigating"),
        " — zoom with each panel's toolbar buttons, and drag with the hand tool ",
        "(or shift-drag / middle-drag from any tool) to pan. The toolbar also ",
        "has reset and PNG download. A 3-D embedding is ",
        "marked as such in the multi-projection picker and gains a rotate tool: drag ",
        "to turn it, and nearer cells are drawn larger so the depth reads."
      ),
      tags$p(
        style = "color:#6b6b70;",
        tags$b("Why this is different. "),
        "General coordinated viewers link expression and space, but have no ",
        "concept of a clonotype, so the immune repertoire can never enter their ",
        "linked loop. Here it is a first-class space, and the composition and ",
        "clonotype readouts are computed, not just recoloured."
      )
    )
  ))
})
