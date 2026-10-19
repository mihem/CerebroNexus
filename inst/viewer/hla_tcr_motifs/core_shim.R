##----------------------------------------------------------------------------##
## HLA & TCR Motifs — core function shim
##
## The pure motif / HLA-typing core is authored once, in the package R/hla_*.R
## files, so it is exported (only hla_normalize_typing), roxygen-documented and
## unit-tested. The Shiny app, however, must run in three modes, and each finds
## the core differently:
##
##   1. repository launch  — runApp("inst")    (package maybe absent/old)
##      sources the checkout's R/hla_*.R next to cerebro_root.
##   2. installed launch   — launchCerebro()   (package present)
##      launchCerebro() hands the namespace's core objects over in
##      Cerebro.options$.hla_core.
##   3. standalone bundle  — createShinyApp()  (package NEVER loaded)
##      createShinyApp() writes the core into viewer/hla_tcr_motifs/core/.
##
## Mode 3 is the strict one: the generated bundle is self-contained and must not
## reach CerebroNexus at runtime (see R/createShinyApp.R and
## tests/testthat/test-smoke-production.R). So the shim never touches the
## namespace — not getFromNamespace(), not requireNamespace().
##
## This file is sourced with local = TRUE into the module server scope (which is
## itself the app server scope), so the definitions below land there and every
## other module file — and the getHLATyping() wrapper in utility_functions.R —
## resolves the core functions by bare name. test-hla-app-contract.R covers all
## three modes.
##----------------------------------------------------------------------------##

if (is.list(Cerebro.options[[".hla_core"]])) {
  list2env(Cerebro.options[[".hla_core"]], envir = environment())
} else {
  .hla_core_files <- list.files(
    file.path(
      Cerebro.options[["cerebro_root"]],
      "viewer/hla_tcr_motifs/core"
    ),
    pattern = "[.]R$",
    full.names = TRUE
  )
  if (length(.hla_core_files) == 0L) {
    .hla_core_files <- list.files(
      file.path(Cerebro.options[["cerebro_root"]], "..", "R"),
      pattern = "^hla_.*[.]R$",
      full.names = TRUE
    )
  }
  if (length(.hla_core_files) == 0L) {
    stop(
      "HLA core not found: expected viewer/hla_tcr_motifs/core/ (exported ",
      "app) or ../R/hla_*.R (source checkout) under cerebro_root.",
      call. = FALSE
    )
  }
  for (.hla_core_file in .hla_core_files) {
    sys.source(.hla_core_file, envir = environment())
  }
  rm(.hla_core_files, .hla_core_file)
}
