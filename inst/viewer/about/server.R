##----------------------------------------------------------------------------##
## Tab: About.
##----------------------------------------------------------------------------##

##
output[["about"]] <- renderText({
  ## The launcher or exporter records the version in app configuration while
  ## CerebroNexus is available. Standalone bundles therefore render this page
  ## without querying (or requiring) the package at runtime.
  version <- Cerebro.options[["cerebro_version"]]
  if (
    is.null(version) ||
      !length(version) ||
      is.na(version[[1]]) ||
      !nzchar(as.character(version[[1]]))
  ) {
    version <- "unknown"
  } else {
    version <- as.character(version[[1]])
  }
  paste0(
    '<b>Version of CerebroNexus</b><br>
    v',
    version,
    '<br>
    <br>
    <b>Current developers and maintainers</b><br>
    Michael Heming<br>
    Xuesong Wang<br>
    <br>
    CerebroNexus (CellReportBrowser Nexus) is a Shiny platform for exploring and sharing single-cell and spatial transcriptomics data, with gene-expression, immune-repertoire, trajectory, and HLA-TCR analyses.<br>
    <br>
    CerebroNexus originated as a fork of
    <a href="https://github.com/romanhaa/cerebroApp" target="_blank" rel="noopener noreferrer">cerebroApp</a>,
    originally developed by Roman Hillje.<br>
    <br>
    <b>Links</b><br>
    <ul>
      <li><a href=https://github.com/mihem/CerebroNexus title="CerebroNexus on GitHub" target="_blank"><b>CerebroNexus on GitHub</b></a></li>
    </ul>
    <br>
    <b>Citation</b><br>
    If you used CerebroNexus for your research, please cite:<br>
    Michael Heming, Xuesong Wang.
    <i>CerebroNexus</i> [software]. Version ',
    htmltools::htmlEscape(version),
    '.
    <a href="https://github.com/mihem/CerebroNexus" target="_blank" rel="noopener noreferrer">https://github.com/mihem/CerebroNexus</a>.
    <br>
    <b>License</b><br>
    CerebroNexus is distributed under the terms of the <a href=https://github.com/mihem/CerebroNexus/blob/master/LICENSE.md title="MIT license" target="_blank">MIT license.</a><br>
    <br>
    <b>Credit where credit is due</b><br>
    <ul>
      <li>The default plot palettes are a custom low-saturation set; several colours draw on <a href="https://flatuicolors.com/" title="Flat UI Colors 2" target="_blank">Flat UI Colors 2</a>.</li>
    </ul>
    <br>'
  )
})
