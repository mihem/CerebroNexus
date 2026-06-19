## ---- Visualizations UI ------------------------------------------------ ##
output$ir_visualizations_UI <- renderUI({
  req(has_scRepertoire())
  data <- ir_data()
  if (is.null(data)) {
    return(div(
      class = "alert alert-warning",
      "No immune repertoire data available."
    ))
  }

  ## Priority tabs (per performance-notes order)
  priority_tabs <- list()
  if (n_samples() >= 2) {
    priority_tabs <- c(
      priority_tabs,
      list(
        tabPanel(
          "Scatter",
          shinycssloaders::withSpinner(plotOutput(
            "ir_plot_clonalScatter",
            height = 450
          ))
        )
      )
    )
  }
  priority_tabs <- c(
    priority_tabs,
    list(
      tabPanel(
        "Paired Scatter",
        shinycssloaders::withSpinner(uiOutput("ir_ui_pairedScatter"))
      ),
      tabPanel(
        "Isotype",
        shinycssloaders::withSpinner(plotOutput(
          "ir_plot_isotype",
          height = 450
        ))
      ),
      tabPanel(
        "Diversity",
        uiOutput("ir_ui_clonalDiversity")
      ),
      tabPanel(
        "Homeostasis",
        shinycssloaders::withSpinner(plotOutput(
          "ir_plot_clonalHomeostasis",
          height = 450
        ))
      ),
      tabPanel(
        "SHM Proxy",
        shinycssloaders::withSpinner(plotOutput(
          "ir_plot_shmProxy",
          height = 450
        ))
      )
    )
  )

  ## Remaining tabs
  other_tabs <- list(
    tabPanel(
      "Abundance",
      shinycssloaders::withSpinner(plotOutput(
        "ir_plot_clonalAbundance",
        height = 450
      ))
    ),
    tabPanel(
      "Length",
      shinycssloaders::withSpinner(plotOutput(
        "ir_plot_clonalLength",
        height = 450
      ))
    ),
    tabPanel(
      "Proportion",
      shinycssloaders::withSpinner(plotOutput(
        "ir_plot_clonalProportion",
        height = 450
      ))
    ),
    tabPanel(
      "Quant",
      shinycssloaders::withSpinner(plotOutput(
        "ir_plot_clonalQuant",
        height = 450
      ))
    ),
    tabPanel("Rarefaction", uiOutput("ir_ui_clonalRarefaction")),
    tabPanel("Gene usage", uiOutput("ir_ui_percentGeneUsage")),
    tabPanel("vizGenes", uiOutput("ir_ui_vizGenes")),
    tabPanel("percentGenes", uiOutput("ir_ui_percentGenes")),
    tabPanel("percentVJ", uiOutput("ir_ui_percentVJ")),
    tabPanel("AA %", uiOutput("ir_ui_percentAA")),
    tabPanel(
      "Entropy",
      shinycssloaders::withSpinner(plotOutput(
        "ir_plot_positionalEntropy",
        height = 450
      ))
    ),
    tabPanel("Property", uiOutput("ir_ui_positionalProperty")),
    tabPanel(
      "K-mer",
      sliderInput(
        "ir_kmer_top_motifs",
        "Top motifs:",
        min = 10,
        max = 100,
        value = 30,
        step = 5
      ),
      uiOutput("ir_ui_percentKmer")
    )
  )

  ## Tabs requiring >= 2 samples
  if (n_samples() >= 2) {
    other_tabs <- c(
      other_tabs,
      list(
        tabPanel(
          "Compare",
          shinycssloaders::withSpinner(plotOutput(
            "ir_plot_clonalCompare",
            height = 450
          ))
        ),
        tabPanel(
          "Overlap",
          shinycssloaders::withSpinner(plotOutput(
            "ir_plot_clonalOverlap",
            height = 450
          ))
        ),
        tabPanel(
          "SizeDist",
          shinycssloaders::withSpinner(plotOutput(
            "ir_plot_clonalSizeDistribution",
            height = 450
          ))
        )
      )
    )
  }

  tabs <- c(priority_tabs, other_tabs)

  do.call(tabsetPanel, c(list(id = "ir_tabs"), tabs))
})

##------------------------------------------------------------------------##
## Plot renderers
##------------------------------------------------------------------------##

## ---- BCR-specific renderers --------------------------------------------- ##
output$ir_plot_isotype <- renderPlot({
  data <- ir_data()
  req(!is.null(data))
  gb <- ir_params()$groupBy
  group_col <- if (is.null(gb)) "sample" else gb
  safeRenderPlot(
    {
      p <- bcr_isotype_plot(data, group_col = group_col)
      if (is.null(p)) {
        plot.new()
        text(
          0.5,
          0.5,
          "No BCR isotype data available.\nRequires BCR data with CTgene column.",
          cex = 0.9
        )
      } else {
        print(p)
      }
    },
    "isotype"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol
  )

output$ir_plot_shmProxy <- renderPlot({
  data <- ir_data()
  req(!is.null(data))
  gb <- ir_params()$groupBy
  group_col <- if (is.null(gb)) "sample" else gb
  safeRenderPlot(
    {
      p <- bcr_shm_proxy_plot(data, group_col = group_col)
      if (is.null(p)) {
        plot.new()
        text(
          0.5,
          0.5,
          "No SHM proxy data available.\nRequires CTnt and CTstrict columns with BCR data.",
          cex = 0.9
        )
      } else {
        print(p)
      }
    },
    "shmProxy"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol
  )

## ---- Paired Scatter (generic) ------------------------------------------- ##
ir_sample_meta <- reactive({
  data <- ir_data()
  if (is.null(data) || length(data) < 2) {
    return(NULL)
  }
  sample_level_meta(data)
})

output$ir_ui_pairedScatter <- renderUI({
  meta <- ir_sample_meta()
  if (is.null(meta) || nrow(meta) < 2) {
    return(div(
      class = "alert alert-info",
      "Paired scatter requires >= 2 samples with shared metadata columns."
    ))
  }
  meta_cols <- setdiff(colnames(meta), ".sample_name")
  compare_candidates <- meta_cols[vapply(
    meta_cols,
    function(col) {
      length(unique(meta[[col]])) == 2L
    },
    logical(1)
  )]
  facet_candidates <- meta_cols[vapply(
    meta_cols,
    function(col) {
      length(unique(meta[[col]])) >= 2L
    },
    logical(1)
  )]
  if (length(compare_candidates) == 0L) {
    return(div(
      class = "alert alert-info",
      "No metadata column with exactly 2 levels found for paired comparison.",
      tags$br(),
      "Available sample-level columns: ",
      paste(meta_cols, collapse = ", ")
    ))
  }
  tagList(
    fluidRow(
      column(
        6,
        selectInput(
          "ir_pair_compare",
          "Compare (2-level column):",
          choices = compare_candidates,
          selected = compare_candidates[1]
        )
      ),
      column(6, {
        default_facet <- ""
        cmp1 <- compare_candidates[1]
        for (fc in facet_candidates[facet_candidates != cmp1]) {
          ok <- all(vapply(
            unique(meta[[fc]]),
            function(lv) {
              subs <- meta[meta[[fc]] == lv, , drop = FALSE]
              length(unique(subs[[cmp1]])) >= 2L
            },
            logical(1)
          ))
          if (ok) {
            default_facet <- fc
            break
          }
        }
        selectInput(
          "ir_pair_facet",
          "Facet by:",
          choices = c("(none)" = "", facet_candidates),
          selected = default_facet
        )
      })
    ),
    shinycssloaders::withSpinner(
      uiOutput("ir_ui_pairedScatter_plot")
    )
  )
})

output$ir_ui_pairedScatter_plot <- renderUI({
  meta <- ir_sample_meta()
  req(!is.null(meta))
  compare_col <- input$ir_pair_compare
  req(!is.null(compare_col))
  facet_col <- input$ir_pair_facet
  if (is.null(facet_col) || facet_col == "") {
    h <- 500
  } else {
    n_facets <- length(unique(meta[[facet_col]]))
    ncol_p <- min(4L, n_facets)
    nrow_p <- ceiling(n_facets / ncol_p)
    h <- max(450, nrow_p * 420)
  }
  plotOutput("ir_plot_pairedScatter", height = paste0(h, "px"))
})

output$ir_plot_pairedScatter <- renderPlot({
  req(has_scRepertoire())
  data <- ir_data()
  req(!is.null(data))
  meta <- ir_sample_meta()
  req(!is.null(meta))
  pars <- ir_params()
  compare_col <- input$ir_pair_compare
  req(!is.null(compare_col))
  facet_col <- input$ir_pair_facet

  lvls <- sort(unique(meta[[compare_col]]))
  req(length(lvls) == 2L)

  safeRenderPlot(
    {
      if (is.null(facet_col) || facet_col == "") {
        s_a <- meta$.sample_name[meta[[compare_col]] == lvls[1]][1]
        s_b <- meta$.sample_name[meta[[compare_col]] == lvls[2]][1]
        p <- scRepertoire::clonalScatter(
          data,
          cloneCall = pars$cloneCall,
          chain = pars$chain,
          x.axis = s_a,
          y.axis = s_b,
          dot.size = "total",
          graph = "proportion",
          exportTable = FALSE,
          palette = "inferno"
        )
        p <- p + ggplot2::ggtitle(paste(lvls[1], "vs", lvls[2]))
        print(p)
      } else {
        facet_lvls <- unique(meta[[facet_col]])
        panels <- list()
        for (fl in facet_lvls) {
          rows <- meta[meta[[facet_col]] == fl, , drop = FALSE]
          s_a <- rows$.sample_name[rows[[compare_col]] == lvls[1]]
          s_b <- rows$.sample_name[rows[[compare_col]] == lvls[2]]
          if (length(s_a) == 0L || length(s_b) == 0L) {
            next
          }
          tryCatch(
            {
              p <- scRepertoire::clonalScatter(
                data,
                cloneCall = pars$cloneCall,
                chain = pars$chain,
                x.axis = s_a[1],
                y.axis = s_b[1],
                dot.size = "total",
                graph = "proportion",
                exportTable = FALSE,
                palette = "inferno"
              )
              p <- p +
                ggplot2::ggtitle(paste0(fl, ": ", lvls[1], " vs ", lvls[2]))
              panels[[length(panels) + 1L]] <- p
            },
            error = function(e) {
              message("[IR] Paired scatter for ", fl, " failed: ", e$message)
            }
          )
        }
        if (length(panels) == 0L) {
          plot.new()
          text(
            0.5,
            0.5,
            "No valid pairs found for the selected columns.",
            cex = 0.9
          )
        } else {
          ncol_p <- min(4L, length(panels))
          print(patchwork::wrap_plots(panels, ncol = ncol_p))
        }
      }
    },
    "pairedScatter"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol,
    input$ir_pair_compare,
    input$ir_pair_facet
  )

output$ir_plot_clonalAbundance <- renderPlot({
  req(has_scRepertoire())
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  safeRenderPlot(
    scRepertoire::clonalAbundance(
      data,
      cloneCall = pars$cloneCall,
      group.by = pars$groupBy
    ),
    "clonalAbundance"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol
  )

output$ir_plot_clonalCompare <- renderPlot({
  req(has_scRepertoire())
  req(
    !is.null(input$ir_compare_samples) && length(input$ir_compare_samples) >= 2
  )
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  safeRenderPlot(
    scRepertoire::clonalCompare(
      data,
      cloneCall = pars$cloneCall,
      chain = pars$chain,
      group.by = pars$groupBy,
      samples = input$ir_compare_samples,
      top.clones = 5,
      graph = "alluvial",
      proportion = TRUE,
      exportTable = FALSE,
      palette = "inferno"
    ),
    "clonalCompare"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol,
    input$ir_compare_samples
  )

output$ir_ui_clonalDiversity <- renderUI({
  tagList(
    sliderInput(
      "ir_diversity_boots",
      "Bootstrap iterations:",
      min = 3,
      max = 100,
      value = 20,
      step = 5
    ),
    shinycssloaders::withSpinner(plotOutput(
      "ir_plot_clonalDiversity",
      height = 450
    ))
  )
})

output$ir_plot_clonalDiversity <- renderPlot({
  req(has_scRepertoire())
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  n_boots <- ir_diversity_boots_d()
  if (is.null(n_boots)) {
    n_boots <- 20
  }
  safeRenderPlot(
    scRepertoire::clonalDiversity(
      data,
      cloneCall = pars$cloneCall,
      chain = pars$chain,
      group.by = pars$groupBy,
      metric = "shannon",
      n.boots = n_boots,
      exportTable = FALSE,
      palette = "inferno"
    ),
    "clonalDiversity"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol,
    ir_diversity_boots_d()
  )

output$ir_plot_clonalHomeostasis <- renderPlot({
  req(has_scRepertoire())
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  safeRenderPlot(
    scRepertoire::clonalHomeostasis(
      data,
      cloneCall = pars$cloneCall,
      chain = pars$chain,
      group.by = pars$groupBy,
      exportTable = FALSE,
      palette = "inferno"
    ),
    "clonalHomeostasis"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol
  )

output$ir_plot_clonalLength <- renderPlot({
  req(has_scRepertoire())
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  safeRenderPlot(
    scRepertoire::clonalLength(
      data,
      cloneCall = pars$cloneCall,
      chain = pars$chain,
      group.by = pars$groupBy,
      exportTable = FALSE,
      palette = "inferno"
    ),
    "clonalLength"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol
  )

output$ir_plot_clonalOverlap <- renderPlot({
  req(has_scRepertoire())
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  safeRenderPlot(
    scRepertoire::clonalOverlap(
      data,
      cloneCall = pars$cloneCall,
      chain = pars$chain,
      group.by = pars$groupBy,
      method = "overlap",
      exportTable = FALSE,
      palette = "inferno"
    ),
    "clonalOverlap"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol
  )

output$ir_plot_clonalProportion <- renderPlot({
  req(has_scRepertoire())
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  safeRenderPlot(
    scRepertoire::clonalProportion(
      data,
      cloneCall = pars$cloneCall,
      chain = pars$chain,
      group.by = pars$groupBy,
      clonalSplit = c(10, 100, 1000, 10000, 30000, 1e+05),
      exportTable = FALSE,
      palette = "inferno"
    ),
    "clonalProportion"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol
  )

output$ir_plot_clonalQuant <- renderPlot({
  req(has_scRepertoire())
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  safeRenderPlot(
    scRepertoire::clonalQuant(
      data,
      cloneCall = pars$cloneCall,
      chain = pars$chain,
      group.by = pars$groupBy,
      scale = FALSE,
      exportTable = FALSE,
      palette = "inferno"
    ),
    "clonalQuant"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol
  )

output$ir_ui_clonalRarefaction <- renderUI({
  tagList(
    sliderInput(
      "ir_rarefaction_boots",
      "Bootstrap iterations:",
      min = 3,
      max = 50,
      value = 5,
      step = 1
    ),
    plotOutput("ir_plot_clonalRarefaction", height = "450px")
  )
})

output$ir_plot_clonalRarefaction <- renderPlot({
  req(has_scRepertoire())
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  n_boots <- ir_rarefaction_boots_d()
  if (is.null(n_boots)) {
    n_boots <- 5
  }
  safeRenderPlot(
    scRepertoire::clonalRarefaction(
      data,
      cloneCall = pars$cloneCall,
      chain = pars$chain,
      group.by = pars$groupBy,
      plot.type = 1,
      hill.numbers = 0,
      n.boots = n_boots,
      exportTable = FALSE,
      palette = "inferno"
    ),
    "clonalRarefaction"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol,
    ir_rarefaction_boots_d()
  )

output$ir_plot_clonalScatter <- renderPlot({
  req(has_scRepertoire())
  req(!is.null(input$ir_scatter_x) && !is.null(input$ir_scatter_y))
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  safeRenderPlot(
    scRepertoire::clonalScatter(
      data,
      cloneCall = pars$cloneCall,
      chain = pars$chain,
      group.by = pars$groupBy,
      x.axis = input$ir_scatter_x,
      y.axis = input$ir_scatter_y,
      dot.size = "total",
      graph = "proportion",
      exportTable = FALSE,
      palette = "inferno"
    ),
    "clonalScatter"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol,
    input$ir_scatter_x,
    input$ir_scatter_y
  )

output$ir_plot_clonalSizeDistribution <- renderPlot({
  req(has_scRepertoire())
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  safeRenderPlot(
    scRepertoire::clonalSizeDistribution(
      data,
      cloneCall = pars$cloneCall,
      group.by = pars$groupBy,
      method = "ward.D2",
      exportTable = FALSE
    ),
    "clonalSizeDistribution"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol
  )

output$ir_ui_percentGeneUsage <- renderUI({
  h <- ir_plot_height("none")
  shinycssloaders::withSpinner(plotOutput(
    "ir_plot_percentGeneUsage",
    height = paste0(h, "px")
  ))
})

output$ir_plot_percentGeneUsage <- renderPlot({
  req(has_scRepertoire())
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  safeRenderPlot(
    scRepertoire::percentGeneUsage(
      data,
      chain = pars$chain,
      gene = default_gene_family(),
      group.by = pars$groupBy,
      summary.fun = "percent",
      plot.type = "heatmap",
      exportTable = FALSE,
      palette = "inferno"
    ),
    "percentGeneUsage"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol
  )

output$ir_ui_vizGenes <- renderUI({
  h <- ir_plot_height("none")
  shinycssloaders::withSpinner(plotOutput(
    "ir_plot_vizGenes",
    height = paste0(h, "px")
  ))
})

output$ir_plot_vizGenes <- renderPlot({
  req(has_scRepertoire())
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  safeRenderPlot(
    scRepertoire::vizGenes(
      data,
      x.axis = default_gene_family(),
      y.axis = NULL,
      group.by = pars$groupBy,
      plot = "heatmap",
      summary.fun = "count",
      exportTable = FALSE,
      palette = "inferno"
    ),
    "vizGenes"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol
  )

output$ir_ui_percentGenes <- renderUI({
  h <- ir_plot_height("none")
  shinycssloaders::withSpinner(plotOutput(
    "ir_plot_percentGenes",
    height = paste0(h, "px")
  ))
})

output$ir_plot_percentGenes <- renderPlot({
  req(has_scRepertoire())
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  safeRenderPlot(
    scRepertoire::percentGenes(
      data,
      chain = specific_chain(),
      gene = "Vgene",
      group.by = pars$groupBy,
      summary.fun = "percent",
      exportTable = FALSE,
      palette = "inferno"
    ),
    "percentGenes"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol
  )

output$ir_ui_percentVJ <- renderUI({
  h <- ir_plot_height("wrap")
  shinycssloaders::withSpinner(plotOutput(
    "ir_plot_percentVJ",
    height = paste0(h, "px")
  ))
})

output$ir_plot_percentVJ <- renderPlot({
  req(has_scRepertoire())
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  safeRenderPlot(
    scRepertoire::percentVJ(
      data,
      chain = specific_chain(),
      group.by = pars$groupBy,
      summary.fun = "percent",
      exportTable = FALSE,
      palette = "inferno"
    ),
    "percentVJ"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol
  )

output$ir_ui_percentAA <- renderUI({
  ng <- n_groups()
  # facet_grid(group ~ .): ~200px per group, minimum 400
  h <- max(400, ng * 200)
  shinycssloaders::withSpinner(plotOutput(
    "ir_plot_percentAA",
    height = paste0(h, "px")
  ))
})

output$ir_plot_percentAA <- renderPlot({
  req(has_scRepertoire())
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  safeRenderPlot(
    scRepertoire::percentAA(
      data,
      chain = pars$chain,
      group.by = pars$groupBy,
      aa.length = 20,
      exportTable = FALSE,
      palette = "inferno"
    ),
    "percentAA"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol
  )

output$ir_plot_positionalEntropy <- renderPlot({
  req(has_scRepertoire())
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  safeRenderPlot(
    scRepertoire::positionalEntropy(
      data,
      chain = pars$chain,
      group.by = pars$groupBy,
      aa.length = 20,
      method = "norm.entropy",
      exportTable = FALSE,
      palette = "inferno"
    ),
    "positionalEntropy"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol
  )

## ---- Positional Property: facet count per method ---------------------- ##
## Requires immApex; most methods also need the Peptides package.
all_property_facets <- c(
  atchleyFactors = 5,
  crucianiProperties = 3,
  FASGAI = 6,
  kideraFactors = 10,
  MSWHIM = 3,
  ProtFP = 8,
  stScales = 8,
  tScales = 5,
  VHSE = 8,
  zScales = 5
)

available_property_methods <- reactive({
  resolver <- tryCatch(
    getFromNamespace(".aa.property.matrix", "immApex"),
    error = function(e) NULL
  )
  if (is.null(resolver)) {
    return(all_property_facets["atchleyFactors"])
  }
  ok <- vapply(
    names(all_property_facets),
    function(m) {
      tryCatch(
        {
          resolver(m)
          TRUE
        },
        error = function(e) FALSE
      )
    },
    logical(1)
  )
  all_property_facets[ok]
})

output$ir_ui_positionalProperty <- renderUI({
  avail <- available_property_methods()
  method <- input$ir_property_method
  if (is.null(method) || !method %in% names(avail)) {
    method <- names(avail)[1]
  }
  n_facets <- avail[[method]]
  if (is.null(n_facets)) {
    n_facets <- 5
  }
  # ~120px per facet row, minimum 450
  h <- max(450, n_facets * 120)
  tagList(
    selectInput(
      "ir_property_method",
      "Property method:",
      choices = names(avail),
      selected = method
    ),
    shinycssloaders::withSpinner(plotOutput(
      "ir_plot_positionalProperty",
      height = paste0(h, "px")
    ))
  )
})

output$ir_plot_positionalProperty <- renderPlot({
  req(has_scRepertoire())
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  method <- input$ir_property_method
  if (is.null(method)) {
    method <- names(available_property_methods())[1]
  }
  safeRenderPlot(
    scRepertoire::positionalProperty(
      data,
      chain = pars$chain,
      group.by = pars$groupBy,
      method = method,
      exportTable = FALSE,
      palette = "inferno"
    ),
    "positionalProperty"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol,
    input$ir_property_method
  )

output$ir_ui_percentKmer <- renderUI({
  top_m <- ir_kmer_top_motifs_d()
  if (is.null(top_m)) {
    top_m <- 30
  }
  h <- max(450, top_m * 20)
  shinycssloaders::withSpinner(plotOutput(
    "ir_plot_percentKmer",
    height = paste0(h, "px")
  ))
})

output$ir_plot_percentKmer <- renderPlot({
  req(has_scRepertoire())
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  top_m <- ir_kmer_top_motifs_d()
  if (is.null(top_m)) {
    top_m <- 30
  }
  safeRenderPlot(
    scRepertoire::percentKmer(
      data,
      chain = pars$chain,
      cloneCall = pars$cloneCall,
      group.by = pars$groupBy,
      motif.length = 3,
      min.depth = 3,
      top.motifs = top_m,
      exportTable = FALSE,
      palette = "inferno"
    ),
    "percentKmer"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sampleCol,
    ir_kmer_top_motifs_d()
  )
