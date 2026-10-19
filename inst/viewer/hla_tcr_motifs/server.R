##----------------------------------------------------------------------------##
## HLA & TCR Motifs — module server entry
##
## Sources the module's server pieces into the app server scope (this file is
## itself sourced with local = TRUE inside the server function, so bare source()
## calls here land in the same scope and `output$...` reaches the app output).
## Core algorithms live in the installed package (hla_motif_core.R /
## hla_typing.R); these files only wire reactives, renderers and UI.
##----------------------------------------------------------------------------##

if (!exists("hla_detect_chains", inherits = TRUE)) {
  source(cerebro_root_path("viewer/hla_tcr_motifs/core_shim.R"), local = TRUE)
}
for (module_file in c(
  "data.R",
  "settings.R",
  "visualizations.R",
  "network_table.R",
  "data_qc.R",
  "associations.R",
  "help.R",
  "help_guide.R"
)) {
  source(
    cerebro_root_path("viewer", "hla_tcr_motifs", module_file),
    local = TRUE
  )
}
