##----------------------------------------------------------------------------##
## Expression by gene.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for plot.
##----------------------------------------------------------------------------##
output[["expression_by_gene_UI"]] <- renderUI({
  req(expression_summary_show_genes())
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Expression levels by gene"),
        cerebroInfoButton("expression_by_gene_info")
      ),
      plotly::plotlyOutput("expression_by_gene")
    )
  )
})

##----------------------------------------------------------------------------##
## Bar plot.
##----------------------------------------------------------------------------##
output[["expression_by_gene"]] <- plotly::renderPlotly({
  if (!isTRUE(isolate(input[["expression_summary_viewport_request"]]))) {
    req(input[["expression_summary_viewport_request"]], cancelOutput = TRUE)
  }
  req(
    expression_projection_summary_ready(),
    expression_projection_parameters_color(),
    expression_summary_data()
  )
  expression_levels <- expressionProjectionGeneMeans(
    data_set(),
    expression_summary_data()$genes
  ) %>%
    dplyr::slice_max(expression, n = 50)
  color_settings <- expression_projection_parameters_color()
  color_scale <- expressionColorScale(color_settings[["color_scale"]])
  ## prepare plot
  plotly::plot_ly(
    expression_levels,
    x = ~gene,
    y = ~expression,
    text = ~ paste0(
      expression_levels$gene,
      ': ',
      format(expression_levels$expression, digits = 3)
    ),
    type = "bar",
    marker = list(
      color = ~expression,
      colorscale = color_scale,
      reversescale = expressionReverseColorScale(
        color_settings[["color_scale"]]
      ),
      line = list(
        color = "rgb(196,196,196)",
        width = 1
      )
    ),
    hoverinfo = "text",
    showlegend = FALSE
  ) %>%
    plotly::layout(
      title = "",
      xaxis = list(
        title = "",
        type = "category",
        categoryorder = "array",
        categoryarray = expression_levels$gene,
        mirror = TRUE,
        showline = TRUE
      ),
      yaxis = list(
        title = "Expression level",
        mirror = TRUE,
        showline = TRUE
      ),
      dragmode = "lasso",
      hovermode = "compare"
    ) %>%
    cerebro_plotly_toolbar()
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
cerebroRegisterInfo(input, "expression_by_gene_info", expression_by_gene_info)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
expression_by_gene_info <- list(
  title = "Expression levels by gene",
  text = p(
    "Mean log-normalised expression across all cells for up to 50 selected genes. This comparison is shown only when at least two genes are selected."
  )
)
