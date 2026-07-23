##----------------------------------------------------------------------------##
## HLA & TCR Motifs — core function shim
##
## The pure motif / HLA-typing core is authored in the package R/ directory so it
## is exported (only hla_normalize_typing), roxygen-documented and unit-tested.
## The Shiny app, however, must run in three modes:
##
##   1. repository launch  — runApp("inst")           (package maybe absent/old)
##   2. installed launch   — package attached normally (package present)
##   3. standalone bundle  — createShinyApp() output   (package NEVER loaded)
##
## Mode 3 is the strict one: the generated bundle is self-contained and must not
## name CerebroNexus anywhere in its source (see R/createShinyApp.R and
## tests/testthat/test-smoke-production.R). So the shim cannot reach into the
## namespace — not getFromNamespace(), not requireNamespace().
##
## Instead the core files are shipped as byte-identical copies under this
## module's core/ directory. That directory lives inside inst/, so it survives
## package installation AND is copied verbatim into every createShinyApp bundle.
## It is therefore present in all three modes, and sourcing it needs no package
## on the search path. A drift guard in tests/testthat/test-hla-app-contract.R
## keeps core/ byte-identical to R/.
##
## This file is sourced with local = TRUE into the module server scope (which is
## itself the app server scope), so the definitions below land there and every
## other module file — and the getHLATyping() wrapper in utility_functions.R —
## resolves the core functions by bare name.
##----------------------------------------------------------------------------##

## Every core file the module (and the getHLATyping wrapper) calls into, in
## dependency order. A file present in R/ but missing here is never sourced, so
## its functions surface as "could not find function" in a running app while unit
## tests (which reach R/ directly) stay green. test-hla-app-contract.R pins this
## list against R/ and pins the copies byte-for-byte.
.hla_source_files <- c(
  "hla_typing.R",
  "hla_motif_core.R",
  "hla_association_core.R",
  "hla_visual_helpers.R",
  "hla_export.R"
)

.hla_core_dir <- paste0(
  Cerebro.options[["cerebro_root"]],
  "/shiny/v1.4/hla_tcr_motifs/core"
)

for (.hla_core_file in .hla_source_files) {
  sys.source(
    file.path(.hla_core_dir, .hla_core_file),
    envir = environment()
  )
}

rm(.hla_source_files, .hla_core_dir, .hla_core_file)
