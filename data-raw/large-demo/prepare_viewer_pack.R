# Prepare derived assets before starting a large-data demo, not on first page
# entry. Validation prevents a stale pack from serving another CRB's results.
prepareDemoViewerPack <- function(file) {
  checked <- CerebroNexus::validateViewerPack(file, full = TRUE)
  if (isTRUE(checked$valid)) {
    chains <- as.character(checked$manifest$hla_chains)
    ## First frames of version 3 (format revision 2): an older Pack is rebuilt.
    first_frames <- file.path("hla_tcr", paste0(chains, ".first.v3.qs2"))
    ## A pack from before the integer encoding of the repertoire (or of another
    ## encoding version) makes the Immune repertoire page encode it at every
    ## start: rebuild it.
    if (
      (!length(chains) ||
        all(first_frames %in% checked$manifest$assets$path)) &&
        !CerebroNexus:::.viewerPackImmuneOutdated(checked$manifest)
    ) {
      return(invisible(checked$path))
    }
  }
  message(
    "Preparing Viewer Pack for ",
    basename(file),
    " (one-time preprocessing)"
  )
  CerebroNexus::buildViewerPack(
    file,
    viewer_binary = "always",
    overwrite = TRUE
  )
}
