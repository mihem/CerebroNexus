##----------------------------------------------------------------------------##
## UI elements to choose whether gene(s) or gene sets should be analyzed.
## Keep gene controls mounted: rebuilding three whole-transcriptome selectize
## inputs when RGB mode opens is prohibitively expensive for large gene lists.
##----------------------------------------------------------------------------##
output[["expression_projection_input_type_UI"]] <- renderUI({
  channel_input <- function(channel, label) {
    selectizeInput(
      paste0("expression_rgb_gene_", channel),
      label = label,
      choices = NULL,
      selected = "",
      multiple = FALSE,
      options = list(
        maxOptions = 1000,
        create = FALSE,
        allowEmptyOption = TRUE,
        placeholder = paste(tolower(label), "gene..."),
        loadThrottle = 300
      )
    )
  }

  tagList(
    conditionalPanel(
      paste0(
        "input.expression_analysis_mode == 'Gene(s)' && ",
        "input.expression_projection_genes_in_separate_panels != 'rgb'"
      ),
      selectizeInput(
        "expression_genes_input",
        label = "Gene(s)",
        choices = NULL,
        multiple = TRUE,
        options = list(
          maxOptions = 1000,
          create = TRUE,
          plugins = list("remove_button"),
          loadThrottle = 300
        )
      )
    ),
    conditionalPanel(
      paste0(
        "input.expression_analysis_mode == 'Gene(s)' && ",
        "input.expression_projection_genes_in_separate_panels == 'rgb'"
      ),
      div(
        class = "cerebro-gene-rgb-row cerebro-control-enter",
        div(
          class = "cerebro-gene-rgb-channel",
          channel_input("r", "Red channel")
        ),
        div(
          class = "cerebro-gene-rgb-channel",
          channel_input("g", "Green channel")
        ),
        div(
          class = "cerebro-gene-rgb-channel",
          channel_input("b", "Blue channel")
        )
      ),
      tags$small(
        "RGB starts with the first three available genes; your full gene list is kept. ",
        "Each channel uses its own maximum, so brightness is not comparable across genes."
      )
    ),
    conditionalPanel(
      "input.expression_analysis_mode == 'Gene set'",
      uiOutput("expression_select_gene_set_UI")
    ),
    tags$script(HTML("
    (function () {
      // Leaving RGB appends the channel genes to the gene list.
      Shiny.addCustomMessageHandler('cerebro_expression_gene_list', function (message) {
        var attempts = 0;
        function apply() {
          if (!window.CerebroDatasetContext?.accepts(message)) return;
          var source = document.getElementById('expression_genes_input')?.selectize;
          if (!source || typeof source.settings.load !== 'function' || source.loading) {
            if (++attempts < 400) setTimeout(apply, 25);
            return;
          }
          message.genes.forEach(function (gene) {
            var option = {};
            option[source.settings.valueField] = gene;
            option[source.settings.labelField] = gene;
            source.addOption(option);
          });
          source.setValue(message.genes);
        }
        setTimeout(apply, 0);
      });
      var revision = 0;
      Shiny.addCustomMessageHandler('cerebro_expression_rgb_seed', function (message) {
        var token = ++revision;
        function apply() {
          if (token !== revision || !window.CerebroDatasetContext?.accepts(message)) return;
          var source = document.getElementById('expression_genes_input')?.selectize;
          if (!source || JSON.stringify(source.getValue() || []) !== JSON.stringify(message.source)) return;
          var controls = ['r', 'g', 'b'].map(function (channel) {
            return document.getElementById('expression_rgb_gene_' + channel)?.selectize;
          });
          // A late initial search response must not clear the seeded channels.
          if (controls.some(c => !c || typeof c.settings.load !== 'function' || c.loading)) {
            setTimeout(apply, 25);
            return;
          }
          controls.forEach(function (control, i) {
            var gene = message.genes[i] || '';
            if (gene) control.addOption({value: gene, label: gene, text: gene});
            control.setValue(gene);
          });
        }
        setTimeout(apply, 0);
      });
    })();
    "))
  )
})

output[["expression_select_gene_set_UI"]] <- renderUI({
  req(identical(input[["expression_analysis_mode"]], "Gene set"))
  selectizeInput(
    "expression_select_gene_set",
    label = "Gene set",
    choices = list("Gene sets" = as.list(c("-", getGeneSetNames()))),
    multiple = FALSE
  )
})

## Wait for the dynamic input-type UI's first bound input, then initialise all
## four stable gene controls. Later display-mode switches only show/hide them.
observeEvent(
  input[["expression_analysis_mode"]],
  {
    for (input_id in c(
      "expression_genes_input",
      "expression_rgb_gene_r",
      "expression_rgb_gene_g",
      "expression_rgb_gene_b"
    )) {
      local({
        id <- input_id
        serverSideGeneSelector(
          session,
          id,
          canonical_selection = TRUE,
          refresh_on_activation = FALSE,
          ## These controls are already mounted. Do not schedule repeated URL
          ## registrations while a large dataset is still being prepared.
          retry = FALSE,
          active = function() identical(input[["sidebar"]], "geneExpression")
        )
      })
    }
  },
  once = TRUE
)
