# Prepare derived assets before starting a large-data demo, not on first page
# entry. Validation prevents a stale pack from serving another CRB's results.
prepareDemoViewerPack <- function(file) {
  checked <- CerebroNexus::validateViewerPack(file, full = TRUE)
  if (isTRUE(checked$valid)) {
    chains <- as.character(checked$manifest$hla_chains)
    first_frames <- file.path("hla_tcr", paste0(chains, ".first.qs2"))
    if (!length(chains) || all(first_frames %in% checked$manifest$assets$path)) {
      return(invisible(checked$path))
    }
  }
  message("Preparing Viewer Pack for ", basename(file), " (one-time preprocessing)")
  CerebroNexus::buildViewerPack(file, viewer_binary = "always", overwrite = TRUE)
}
