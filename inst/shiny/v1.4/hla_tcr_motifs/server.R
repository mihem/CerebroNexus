##----------------------------------------------------------------------------##
## HLA & TCR Motifs — module server entry
##
## Sources the module's server pieces into the app server scope (this file is
## itself sourced with local = TRUE inside the server function, so bare source()
## calls here land in the same scope and `output$...` reaches the app output).
## Core algorithms live in the installed package (hla_motif_core.R /
## hla_typing.R); these files only wire reactives, renderers and UI.
##----------------------------------------------------------------------------##

source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/hla_tcr_motifs/core_shim.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/hla_tcr_motifs/data.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/hla_tcr_motifs/settings.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/hla_tcr_motifs/visualizations.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/hla_tcr_motifs/network_table.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/hla_tcr_motifs/data_qc.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/hla_tcr_motifs/associations.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/hla_tcr_motifs/help.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/hla_tcr_motifs/help_guide.R"
  ),
  local = TRUE
)
