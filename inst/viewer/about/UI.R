##----------------------------------------------------------------------------##
## Tab: About.
##----------------------------------------------------------------------------##

tab_about <- tabItem(
  tabName = "about",
  tagList(
    fluidRow(
      column(
        12,
        ## Floated right so the text keeps its width and wraps beside the logo.
        if (!is.null(cerebro_www_prefix)) {
          tags$img(
            class = "cerebro-about-logo",
            src = cerebro_asset_url("cerebronexus-logo.png"),
            alt = "CerebroNexus logo"
          )
        },
        titlePanel("About CerebroNexus"),
        htmlOutput("about"),
        #        uiOutput("preferences"),
        actionButton("browser", "browser"),
        tags$script("$('#browser').hide();")
      )
    ),
    fluidRow(
      htmlOutput("about_footer")
    )
  )
)
