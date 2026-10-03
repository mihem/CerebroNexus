library(shiny)

addResourcePath(
  "renderer",
  normalizePath(Sys.getenv("CEREBRO_RENDERER_WWW"), mustWork = TRUE)
)

shinyApp(
  fluidPage(
    tags$head(
      tags$script(src = "renderer/cell_points_gpu.js"),
      tags$script(src = "benchmark.js")
    ),
    tags$canvas(
      id = "benchmark-canvas",
      style = "display:block;width:1280px;height:720px"
    )
  ),
  function(input, output, session) {}
)
