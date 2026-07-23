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
    <b>Authors</b><br>
    Roman Hillje<br>
    Michael Heming<br>
    Xuesong Wang<br>
    <br>
    <b>Links</b><br>
    <ul>
      <li><a href=https://github.com/mihem/CerebroNexus title="CerebroNexus on GitHub" target="_blank"><b>CerebroNexus on GitHub</b></a></li>
      <li><a href=https://github.com/romanhaa/Cerebro title="Discontinued Cerebro repository on GitHub (Roman Hillje)" target="_blank"><b>Discontinued Cerebro repository on GitHub (Roman Hillje)</b></a></li>
    </ul>
    <br>
    <b>Citation</b><br>
    If you used CerebroNexus for your research, please cite the original Cerebro publication:
    <br>
    Roman Hillje, Pier Giuseppe Pelicci, Lucilla Luzi, Cerebro: Interactive visualization of scRNA-seq data, Bioinformatics, btz877, <a href=https://doi.org/10.1093/bioinformatics/btz877 title="DOI" target="_blank">https://doi.org/10.1093/bioinformatics/btz877</a><br>
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
