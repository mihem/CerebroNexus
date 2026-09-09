##----------------------------------------------------------------------------##
## HLA & TCR Motifs — core function shim
##
## The Shiny app must run in three modes:
##
##   1. repository launch  — runApp("inst")           (package maybe absent/old)
##   2. installed launch   — package attached normally (package present)
##   3. standalone bundle  — createShinyApp() output   (package NEVER loaded)
##
## The HLA core has one authored source in R/. createShinyApp() materializes it
## as core/hla_package_core.R so a standalone bundle remains self-contained.
##
## This file is sourced with local = TRUE into the module server scope (which is
## itself the app server scope), so the definitions below land there and every
## other module file — and the getHLATyping() wrapper in utility_functions.R —
## resolves the core functions by bare name.
##----------------------------------------------------------------------------##

## Resolve the core from the generated standalone payload, the repository source
## tree, or the installed namespace already on the parent chain.
.hla_env <- environment()
.hla_root <- Cerebro.options[["cerebro_root"]]
.hla_generated_core <- file.path(
  .hla_root,
  "viewer/hla_tcr_motifs/core/hla_package_core.R"
)
.hla_package_files <- file.path(
  .hla_root,
  "..",
  "R",
  c(
    "hla_typing.R",
    "hla_motif_core.R",
    "hla_association_core.R",
    "hla_visual_helpers.R",
    "hla_export.R"
  )
)
.hla_namespace <- NULL
.hla_symbols <- character()

if (file.exists(.hla_generated_core)) {
  sys.source(.hla_generated_core, envir = .hla_env)
} else if (all(file.exists(.hla_package_files))) {
  invisible(lapply(.hla_package_files, sys.source, envir = .hla_env))
} else if (requireNamespace("CerebroNexus", quietly = TRUE)) {
  .hla_namespace <- asNamespace("CerebroNexus")
  .hla_symbols <- ls(
    .hla_namespace,
    pattern = "^(\\.hla_|hla_|HLA_)",
    all.names = TRUE
  )
  list2env(
    mget(.hla_symbols, envir = .hla_namespace, inherits = FALSE),
    envir = .hla_env
  )
}

if (
  !exists(
    "hla_build_manifest",
    envir = .hla_env,
    inherits = FALSE
  )
) {
  stop("HLA package core is unavailable.", call. = FALSE)
}

rm(
  .hla_env,
  .hla_root,
  .hla_generated_core,
  .hla_package_files,
  .hla_namespace,
  .hla_symbols
)
