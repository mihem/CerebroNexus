##----------------------------------------------------------------------------##
## Tab: Immune Repertoire server (BCR/TCR)
##----------------------------------------------------------------------------##

createImmuneRepertoireServer <- function(repertoire_type = c("bcr", "tcr")) {
  repertoire_type <- match.arg(repertoire_type)

  prefix <- paste0(repertoire_type, "_")

  getDataFunc <- if (repertoire_type == "bcr") getBCR else getTCR

  chain_choices <- if (repertoire_type == "bcr") {
    c("both", "IGH", "IGK", "IGL")
  } else {
    c("both", "TRA", "TRB", "TRG", "TRD")
  }

  has_scRepertoire <- function() {
    requireNamespace("scRepertoire", quietly = TRUE)
  }

  safeRenderPlot <- function(expr, plot_name = "unknown") {
    result <- tryCatch({
      message("[", toupper(repertoire_type), " DEBUG] Rendering plot: ", plot_name)
      p <- expr
      message("[", toupper(repertoire_type), " DEBUG] Plot '", plot_name, "' rendered successfully")
      p
    }, error = function(e) {
      message("[", toupper(repertoire_type), " ERROR] Plot '", plot_name, "' failed: ", e$message)
      plot.new()
      text(0.5, 0.5, paste("Error in", plot_name, ":\n", e$message), cex = 0.8)
    })
    return(result)
  }

  observeEvent(input[[paste0(prefix, "cloneCall")]], ignoreInit = TRUE, { })

  observeEvent(input[[paste0(prefix, "tabs")]], {
    req(has_scRepertoire())

    tabs_input <- input[[paste0(prefix, "tabs")]]
    cloneCall_input <- paste0(prefix, "cloneCall")

    if (tabs_input %in% c("Length", "K-mer")) {
      updateSelectInput(
        session,
        cloneCall_input,
        choices = c("nt", "aa"),
        selected = ifelse(input[[cloneCall_input]] %in% c("nt", "aa"), input[[cloneCall_input]], "aa")
      )
    } else if (tabs_input %in% c("Gene usage", "vizGenes", "percentGenes", "percentVJ", "AA %", "Entropy")) {
      updateSelectInput(
        session,
        cloneCall_input,
        choices = NULL,
        selected = NULL
      )
    } else {
      updateSelectInput(
        session,
        cloneCall_input,
        choices = c("gene", "nt", "aa", "strict"),
        selected = input[[cloneCall_input]]
      )
    }

    shinyjs::toggleElement(
      id = paste0(prefix, "scatter_x"),
      anim = TRUE,
      condition = tabs_input %in% c("Scatter")
    )
    shinyjs::toggleElement(
      id = paste0(prefix, "scatter_y"),
      anim = TRUE,
      condition = tabs_input %in% c("Scatter")
    )
    shinyjs::toggleElement(
      id = paste0(prefix, "compare_samples"),
      anim = TRUE,
      condition = tabs_input %in% c("Compare")
    )
  })

  output[[paste0(prefix, "settings_UI")]] <- renderUI({
    req(has_scRepertoire())

    data <- getDataFunc()
    available_groups <- c(NULL)
    available_samples <- c()

    if (!is.null(data) && is.list(data) && length(data) > 0) {
      all_groups <- getGroups()
      data_cols <- names(data[[1]])
      available_groups <- c(NULL, intersect(all_groups, data_cols))
      available_samples <- names(data)
    }

    if (length(available_samples) == 0) {
      return(
        div(
          class = "alert alert-warning",
          paste0(
            "No ", toupper(repertoire_type), " data available. ",
            "Please import ", toupper(repertoire_type), " data first."
          )
        )
      )
    }

    tagList(
      fluidRow(
        column(
          width = 6,
          selectInput(
            paste0(prefix, "cloneCall"),
            label = "Clone call:",
            choices = c("gene", "nt", "aa", "strict"),
            selected = "gene"
          )
        ),
        column(
          width = 6,
          selectInput(
            paste0(prefix, "groupBy"),
            label = "Group by:",
            choices = available_groups,
            selected = "none"
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          selectInput(
            paste0(prefix, "chain"),
            label = "Chain:",
            choices = chain_choices,
            selected = "both"
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          selectInput(
            paste0(prefix, "scatter_x"),
            label = "Sample 1 (for Scatter):",
            choices = available_samples,
            selected = if (length(available_samples) >= 1) available_samples[1] else NULL
          )
        ),
        column(
          width = 6,
          selectInput(
            paste0(prefix, "scatter_y"),
            label = "Sample 2 (for Scatter):",
            choices = available_samples,
            selected = if (length(available_samples) >= 2) available_samples[2] else if (length(available_samples) >= 1) available_samples[1] else NULL
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          selectInput(
            paste0(prefix, "compare_samples"),
            label = "Samples for Compare (select at least 2):",
            choices = available_samples,
            selected = if (length(available_samples) >= 2) available_samples[1:2] else available_samples,
            multiple = TRUE
          )
        )
      )
    )
  })

  repertoire_data <- reactive({
    message("[", toupper(repertoire_type), " DEBUG] ", repertoire_type, "_data() called")
    data <- getDataFunc()
    if (is.null(data)) {
      message("[", toupper(repertoire_type), " DEBUG] getDataFunc() returned NULL")
      return(NULL)
    }
    message("[", toupper(repertoire_type), " DEBUG] getDataFunc() returned data of class: ", paste(class(data), collapse = ", "))
    message("[", toupper(repertoire_type), " DEBUG] ", toupper(repertoire_type), " data length: ", length(data))
    if (is.list(data) && length(data) > 0) {
      message("[", toupper(repertoire_type), " DEBUG] First element class: ", paste(class(data[[1]]), collapse = ", "))
      if (is.data.frame(data[[1]])) {
        message("[", toupper(repertoire_type), " DEBUG] First element has ", nrow(data[[1]]), " rows and ", ncol(data[[1]]), " columns")
        message("[", toupper(repertoire_type), " DEBUG] Column names: ", paste(head(names(data[[1]]), 10), collapse = ", "))
      }
    }
    if (is.null(data) || !is.list(data) || length(data) == 0) {
      message("[", toupper(repertoire_type), " DEBUG] Data is NULL, not a list, or empty")
      return(NULL)
    }
    return(data)
  })

  repertoire_params <- reactive({
    list(
      cloneCall = input[[paste0(prefix, "cloneCall")]],
      groupBy = input[[paste0(prefix, "groupBy")]]
    )
  })

  needs_cloneCall <- reactive({
    !input[[paste0(prefix, "tabs")]] %in% c("Gene usage", "vizGenes", "percentGenes", "percentVJ", "AA %", "Entropy")
  })

  output[[paste0(prefix, "visualizations_UI")]] <- renderUI({
    req(has_scRepertoire())

    data <- getDataFunc()
    if (is.null(data) || !is.list(data) || length(data) == 0) {
      return(
        div(
          class = "alert alert-warning",
          paste0(
            "No ", toupper(repertoire_type), " data available. ",
            "Please import ", toupper(repertoire_type), " data first."
          )
        )
      )
    }

    tagList(
      tabsetPanel(
        id = paste0(prefix, "tabs"),
        tabPanel("Abundance", plotOutput(paste0(prefix, "plot_clonalAbundance"), height = 450)),
        tabPanel("Compare", plotOutput(paste0(prefix, "plot_clonalCompare"), height = 450)),
        tabPanel("Diversity", plotOutput(paste0(prefix, "plot_clonalDiversity"), height = 450)),
        tabPanel("Homeostasis", plotOutput(paste0(prefix, "plot_clonalHomeostasis"), height = 450)),
        tabPanel("Length", plotOutput(paste0(prefix, "plot_clonalLength"), height = 450)),
        tabPanel("Overlap", plotOutput(paste0(prefix, "plot_clonalOverlap"), height = 450)),
        tabPanel("Proportion", plotOutput(paste0(prefix, "plot_clonalProportion"), height = 450)),
        tabPanel("Quant", plotOutput(paste0(prefix, "plot_clonalQuant"), height = 450)),
        tabPanel("Rarefaction", plotOutput(paste0(prefix, "plot_clonalRarefaction"), height = 450)),
        tabPanel("Scatter", plotOutput(paste0(prefix, "plot_clonalScatter"), height = 450)),
        tabPanel("SizeDist", plotOutput(paste0(prefix, "plot_clonalSizeDistribution"), height = 450)),
        tabPanel("Gene usage", plotOutput(paste0(prefix, "plot_percentGeneUsage"), height = 450)),
        tabPanel("vizGenes", plotOutput(paste0(prefix, "plot_vizGenes"), height = 450)),
        tabPanel("percentGenes", plotOutput(paste0(prefix, "plot_percentGenes"), height = 450)),
        tabPanel("percentVJ", plotOutput(paste0(prefix, "plot_percentVJ"), height = 450)),
        tabPanel("AA %", plotOutput(paste0(prefix, "plot_percentAA"), height = 450)),
        tabPanel("Entropy", plotOutput(paste0(prefix, "plot_positionalEntropy"), height = 450)),
        tabPanel("Property", plotOutput(paste0(prefix, "plot_positionalProperty"), height = 450)),
        tabPanel("K-mer", plotOutput(paste0(prefix, "plot_percentKmer"), height = 450))
      )
    )
  })

  output[[paste0(prefix, "plot_clonalAbundance")]] <- renderPlot({
    req(has_scRepertoire())
    message("[", toupper(repertoire_type), " DEBUG] Entering clonalAbundance renderer")
    pars <- repertoire_params()
    data <- repertoire_data()
    if (is.null(data)) {
      message("[", toupper(repertoire_type), " DEBUG] Data is NULL, skipping plot")
      plot.new()
      text(0.5, 0.5, paste0("No ", toupper(repertoire_type), " data available"), cex = 1.2)
      return()
    }
    message("[", toupper(repertoire_type), " DEBUG] Data for clonalAbundance - class: ", paste(class(data), collapse = ", "))
    message("[", toupper(repertoire_type), " DEBUG] Params - cloneCall: ", pars$cloneCall, ", groupBy: ", pars$groupBy)
    safeRenderPlot({
      scRepertoire::clonalAbundance(data, cloneCall = pars$cloneCall)
    }, plot_name = "clonalAbundance")
  })

  output[[paste0(prefix, "plot_clonalCompare")]] <- renderPlot({
    req(has_scRepertoire())
    req(!is.null(input[[paste0(prefix, "compare_samples")]]) && length(input[[paste0(prefix, "compare_samples")]]) >= 2)
    pars <- repertoire_params()
    data <- repertoire_data()
    if (is.null(data)) {
      message("[", toupper(repertoire_type), " DEBUG] Data is NULL, skipping plot")
      plot.new()
      text(0.5, 0.5, paste0("No ", toupper(repertoire_type), " data available"), cex = 1.2)
      return()
    }
    safeRenderPlot({
      scRepertoire::clonalCompare(
        repertoire_data(),
        cloneCall = pars$cloneCall,
        chain = "both",
        samples = input[[paste0(prefix, "compare_samples")]],
        clones = NULL,
        top.clones = 5,
        highlight.clones = NULL,
        relabel.clones = FALSE,
        group.by = NULL,
        order.by = NULL,
        graph = "alluvial",
        proportion = TRUE,
        exportTable = FALSE,
        palette = "inferno"
      )
    }, plot_name = "clonalCompare")
  })

  output[[paste0(prefix, "plot_clonalDiversity")]] <- renderPlot({
    req(has_scRepertoire())
    pars <- repertoire_params()
    data <- repertoire_data()
    if (is.null(data)) {
      message("[", toupper(repertoire_type), " DEBUG] Data is NULL, skipping plot")
      plot.new()
      text(0.5, 0.5, paste0("No ", toupper(repertoire_type), " data available"), cex = 1.2)
      return()
    }
    safeRenderPlot({
      scRepertoire::clonalDiversity(
        repertoire_data(),
        cloneCall = pars$cloneCall,
        metric = "shannon",
        chain = "both",
        group.by = NULL,
        order.by = NULL,
        x.axis = NULL,
        exportTable = FALSE,
        palette = "inferno",
        n.boots = 100,
        return.boots = FALSE,
        skip.boots = FALSE)
    }, plot_name = "clonalDiversity")
  })

  output[[paste0(prefix, "plot_clonalHomeostasis")]] <- renderPlot({
    req(has_scRepertoire())
    pars <- repertoire_params()
    data <- repertoire_data()
    if (is.null(data)) {
      message("[", toupper(repertoire_type), " DEBUG] Data is NULL, skipping plot")
      plot.new()
      text(0.5, 0.5, paste0("No ", toupper(repertoire_type), " data available"), cex = 1.2)
      return()
    }
    safeRenderPlot({
      scRepertoire::clonalHomeostasis(
        repertoire_data(),
        chain = input[[paste0(prefix, "chain")]],
        cloneCall = pars$cloneCall,
        group.by = NULL,
        order.by = NULL,
        exportTable = FALSE,
        palette = "inferno")
    }, plot_name = "clonalHomeostasis")
  })

  output[[paste0(prefix, "plot_clonalLength")]] <- renderPlot({
    req(has_scRepertoire())
    pars <- repertoire_params()
    data <- repertoire_data()
    if (is.null(data)) {
      message("[", toupper(repertoire_type), " DEBUG] Data is NULL, skipping plot")
      plot.new()
      text(0.5, 0.5, paste0("No ", toupper(repertoire_type), " data available"), cex = 1.2)
      return()
    }
    safeRenderPlot({
      scRepertoire::clonalLength(
        repertoire_data(),
        chain = input[[paste0(prefix, "chain")]],
        cloneCall = pars$cloneCall,
        group.by = NULL,
        order.by = NULL,
        scale = FALSE,
        exportTable = FALSE,
        palette = "inferno")
    }, plot_name = "clonalLength")
  })

  output[[paste0(prefix, "plot_clonalOverlap")]] <- renderPlot({
    req(has_scRepertoire())
    pars <- repertoire_params()
    data <- repertoire_data()
    if (is.null(data)) {
      message("[", toupper(repertoire_type), " DEBUG] Data is NULL, skipping plot")
      plot.new()
      text(0.5, 0.5, paste0("No ", toupper(repertoire_type), " data available"), cex = 1.2)
      return()
    }
    safeRenderPlot({
      scRepertoire::clonalOverlap(
        repertoire_data(),
        cloneCall = pars$cloneCall,
        method = "overlap",
        chain = "both",
        group.by = NULL,
        order.by = NULL,
        exportTable = FALSE,
        palette = "inferno")
    }, plot_name = "clonalOverlap")
  })

  output[[paste0(prefix, "plot_clonalProportion")]] <- renderPlot({
    req(has_scRepertoire())
    pars <- repertoire_params()
    data <- repertoire_data()
    if (is.null(data)) {
      message("[", toupper(repertoire_type), " DEBUG] Data is NULL, skipping plot")
      plot.new()
      text(0.5, 0.5, paste0("No ", toupper(repertoire_type), " data available"), cex = 1.2)
      return()
    }
    safeRenderPlot({
      scRepertoire::clonalProportion(
        repertoire_data(),
        clonalSplit = c(10, 100, 1000, 10000, 30000, 1e+05),
        chain = input[[paste0(prefix, "chain")]],
        cloneCall = pars$cloneCall,
        group.by = pars$groupBy,
        order.by = NULL,
        exportTable = FALSE,
        palette = "inferno")
    }, plot_name = "clonalProportion")
  })

  output[[paste0(prefix, "plot_clonalQuant")]] <- renderPlot({
    req(has_scRepertoire())
    pars <- repertoire_params()
    data <- repertoire_data()
    if (is.null(data)) {
      message("[", toupper(repertoire_type), " DEBUG] Data is NULL, skipping plot")
      plot.new()
      text(0.5, 0.5, paste0("No ", toupper(repertoire_type), " data available"), cex = 1.2)
      return()
    }
    safeRenderPlot({
      scRepertoire::clonalQuant(
        repertoire_data(),
        cloneCall = pars$cloneCall,
        chain = input[[paste0(prefix, "chain")]],
        scale = FALSE,
        group.by = pars$groupBy,
        order.by = NULL,
        exportTable = FALSE,
        palette = "inferno")
    }, plot_name = "clonalQuant")
  })

  output[[paste0(prefix, "plot_clonalRarefaction")]] <- renderPlot({
    req(has_scRepertoire())
    pars <- repertoire_params()
    data <- repertoire_data()
    if (is.null(data)) {
      message("[", toupper(repertoire_type), " DEBUG] Data is NULL, skipping plot")
      plot.new()
      text(0.5, 0.5, paste0("No ", toupper(repertoire_type), " data available"), cex = 1.2)
      return()
    }
    safeRenderPlot({
      scRepertoire::clonalRarefaction(
        repertoire_data(),
        cloneCall = pars$cloneCall,
        chain = input[[paste0(prefix, "chain")]],
        group.by = pars$groupBy,
        plot.type = 1,
        hill.numbers = 0,
        n.boots = 20,
        exportTable = FALSE,
        palette = "inferno")
    }, plot_name = "clonalRarefaction")
  })

  output[[paste0(prefix, "plot_clonalScatter")]] <- renderPlot({
    req(has_scRepertoire())
    req(!is.null(input[[paste0(prefix, "scatter_x")]]) && !is.null(input[[paste0(prefix, "scatter_y")]]))
    pars <- repertoire_params()
    data <- repertoire_data()
    if (is.null(data)) {
      message("[", toupper(repertoire_type), " DEBUG] Data is NULL, skipping plot")
      plot.new()
      text(0.5, 0.5, paste0("No ", toupper(repertoire_type), " data available"), cex = 1.2)
      return()
    }
    safeRenderPlot({
      scRepertoire::clonalScatter(
        repertoire_data(),
        cloneCall = pars$cloneCall,
        x.axis = input[[paste0(prefix, "scatter_x")]],
        y.axis = input[[paste0(prefix, "scatter_y")]],
        chain = input[[paste0(prefix, "chain")]],
        dot.size = "total",
        group.by = pars$groupBy,
        graph = "proportion",
        exportTable = FALSE,
        palette = "inferno")
    }, plot_name = "clonalScatter")
  })

  output[[paste0(prefix, "plot_clonalSizeDistribution")]] <- renderPlot({
    req(has_scRepertoire())
    pars <- repertoire_params()
    data <- repertoire_data()
    if (is.null(data)) {
      message("[", toupper(repertoire_type), " DEBUG] Data is NULL, skipping plot")
      plot.new()
      text(0.5, 0.5, paste0("No ", toupper(repertoire_type), " data available"), cex = 1.2)
      return()
    }
    safeRenderPlot({
      scRepertoire::clonalSizeDistribution(
        repertoire_data(),
        cloneCall = pars$cloneCall,
        method = "ward.D2",
        exportTable = FALSE)
    }, plot_name = "clonalSizeDistribution")
  })

  output[[paste0(prefix, "plot_percentGeneUsage")]] <- renderPlot({
    req(has_scRepertoire())
    pars <- repertoire_params()
    data <- repertoire_data()
    if (is.null(data)) {
      message("[", toupper(repertoire_type), " DEBUG] Data is NULL, skipping plot")
      plot.new()
      text(0.5, 0.5, paste0("No ", toupper(repertoire_type), " data available"), cex = 1.2)
      return()
    }
    safeRenderPlot({
      scRepertoire::percentGeneUsage(
        repertoire_data(),
        chain = input[[paste0(prefix, "chain")]],
        genes = if (repertoire_type == "bcr") "TRBV" else "TRBV",
        group.by = pars$groupBy,
        order.by = NULL,
        summary.fun = "percent",
        plot.type = "heatmap",
        exportTable = FALSE,
        palette = "inferno")
    }, plot_name = "percentGeneUsage")
  })

  output[[paste0(prefix, "plot_vizGenes")]] <- renderPlot({
    req(has_scRepertoire())
    pars <- repertoire_params()
    data <- repertoire_data()
    if (is.null(data)) {
      message("[", toupper(repertoire_type), " DEBUG] Data is NULL, skipping plot")
      plot.new()
      text(0.5, 0.5, paste0("No ", toupper(repertoire_type), " data available"), cex = 1.2)
      return()
    }
    safeRenderPlot({
      scRepertoire::vizGenes(
        repertoire_data(),
        x.axis = if (repertoire_type == "bcr") "TRBV" else "TRBV",
        y.axis = NULL,
        group.by = pars$groupBy,
        plot = "heatmap",
        order.by = NULL,
        summary.fun = "count",
        exportTable = FALSE,
        palette = "inferno")
    }, plot_name = "vizGenes")
  })

  output[[paste0(prefix, "plot_percentGenes")]] <- renderPlot({
    req(has_scRepertoire())
    pars <- repertoire_params()
    data <- repertoire_data()
    if (is.null(data)) {
      message("[", toupper(repertoire_type), " DEBUG] Data is NULL, skipping plot")
      plot.new()
      text(0.5, 0.5, paste0("No ", toupper(repertoire_type), " data available"), cex = 1.2)
      return()
    }
    safeRenderPlot({
      scRepertoire::percentGenes(
        repertoire_data(),
        chain = input[[paste0(prefix, "chain")]],
        gene = "Vgene",
        group.by = pars$groupBy,
        order.by = NULL,
        exportTable = FALSE,
        summary.fun = "percent",
        palette = "inferno")
    }, plot_name = "percentGenes")
  })

  output[[paste0(prefix, "plot_percentVJ")]] <- renderPlot({
    req(has_scRepertoire())
    pars <- repertoire_params()
    data <- repertoire_data()
    if (is.null(data)) {
      message("[", toupper(repertoire_type), " DEBUG] Data is NULL, skipping plot")
      plot.new()
      text(0.5, 0.5, paste0("No ", toupper(repertoire_type), " data available"), cex = 1.2)
      return()
    }
    safeRenderPlot({
      scRepertoire::percentVJ(
        repertoire_data(),
        chain = input[[paste0(prefix, "chain")]],
        group.by = pars$groupBy,
        order.by = NULL,
        exportTable = FALSE,
        summary.fun = "percent",
        palette = "inferno")
    }, plot_name = "percentVJ")
  })

  output[[paste0(prefix, "plot_percentAA")]] <- renderPlot({
    req(has_scRepertoire())
    pars <- repertoire_params()
    data <- repertoire_data()
    if (is.null(data)) {
      message("[", toupper(repertoire_type), " DEBUG] Data is NULL, skipping plot")
      plot.new()
      text(0.5, 0.5, paste0("No ", toupper(repertoire_type), " data available"), cex = 1.2)
      return()
    }
    safeRenderPlot({
      scRepertoire::percentAA(
        repertoire_data(),
        chain = input[[paste0(prefix, "chain")]],
        group.by = pars$groupBy,
        order.by = NULL,
        aa.length = 20,
        exportTable = FALSE,
        palette = "inferno")
    }, plot_name = "percentAA")
  })

  output[[paste0(prefix, "plot_positionalEntropy")]] <- renderPlot({
    req(has_scRepertoire())
    pars <- repertoire_params()
    data <- repertoire_data()
    if (is.null(data)) {
      message("[", toupper(repertoire_type), " DEBUG] Data is NULL, skipping plot")
      plot.new()
      text(0.5, 0.5, paste0("No ", toupper(repertoire_type), " data available"), cex = 1.2)
      return()
    }
    safeRenderPlot({
      scRepertoire::positionalEntropy(
        repertoire_data(),
        chain = input[[paste0(prefix, "chain")]],
        group.by = pars$groupBy,
        order.by = NULL,
        aa.length = 20,
        method = "norm.entropy",
        exportTable = FALSE,
        palette = "inferno")
    }, plot_name = "positionalEntropy")
  })

  output[[paste0(prefix, "plot_positionalProperty")]] <- renderPlot({
    req(has_scRepertoire())
    pars <- repertoire_params()
    data <- repertoire_data()
    if (is.null(data)) {
      message("[", toupper(repertoire_type), " DEBUG] Data is NULL, skipping plot")
      plot.new()
      text(0.5, 0.5, paste0("No ", toupper(repertoire_type), " data available"), cex = 1.2)
      return()
    }
    safeRenderPlot({
      scRepertoire::positionalProperty(
        repertoire_data(),
        chain = input[[paste0(prefix, "chain")]],
        group.by = pars$groupBy,
        order.by = NULL,
        exportTable = FALSE,
        palette = "inferno")
    }, plot_name = "positionalProperty")
  })

  output[[paste0(prefix, "plot_percentKmer")]] <- renderPlot({
    req(has_scRepertoire())
    pars <- repertoire_params()
    data <- repertoire_data()
    if (is.null(data)) {
      message("[", toupper(repertoire_type), " DEBUG] Data is NULL, skipping plot")
      plot.new()
      text(0.5, 0.5, paste0("No ", toupper(repertoire_type), " data available"), cex = 1.2)
      return()
    }
    safeRenderPlot({
      scRepertoire::percentKmer(
        repertoire_data(),
        chain = input[[paste0(prefix, "chain")]],
        cloneCall = pars$cloneCall,
        group.by = pars$groupBy,
        motif.length = 3,
        min.depth = 3,
        top.motifs = 30,
        exportTable = FALSE,
        palette = "inferno")
    }, plot_name = "percentKmer")
  })
}

createImmuneRepertoireServer("bcr")
createImmuneRepertoireServer("tcr")
