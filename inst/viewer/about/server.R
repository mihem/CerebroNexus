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
  ## Font Awesome is already loaded by the dashboard, so the icons need no extra
  ## files. `kind` picks the colour (see .cerebro-about-ico in custom.css).
  ico <- function(icon, kind = "blue") {
    sprintf(
      '<i class="%s cerebro-about-ico cerebro-about-ico-%s" aria-hidden="true"></i>',
      icon,
      kind
    )
  }
  link <- function(href, label, icon, kind = "blue") {
    sprintf(
      '<li>%s<a href="%s" target="_blank" rel="noopener noreferrer">%s</a></li>',
      ico(icon, kind),
      href,
      label
    )
  }
  paste0(
    '<b>',
    ico("fas fa-tag", "amber"),
    'Version of CerebroNexus</b><br>
    v',
    version,
    '<br>
    <br>
    <b>',
    ico("fas fa-users", "blue"),
    'Current developers and maintainers</b><br>
    ',
    ico("fas fa-user", "muted"),
    'Michael Heming<br>
    ',
    ico("fas fa-user", "muted"),
    'Xuesong Wang<br>
    <br>
    ',
    ico("fas fa-brain", "pink"),
    'CerebroNexus (CellReportBrowser Nexus) is a Shiny platform for exploring and sharing single-cell and spatial transcriptomics data, with gene-expression, immune-repertoire, trajectory, and HLA-TCR analyses.<br>
    <br>
    ',
    ico("fas fa-code-branch", "teal"),
    'CerebroNexus originated as a fork of
    <a href="https://github.com/romanhaa/cerebroApp" target="_blank" rel="noopener noreferrer">cerebroApp</a>,
    originally developed by Roman Hillje.<br>
    <br>
    <b>',
    ico("fas fa-link", "green"),
    'Links</b><br>
    <ul class="cerebro-about-links">',
    link(
      "https://github.com/mihem/CerebroNexus",
      "<b>CerebroNexus on GitHub</b>",
      "fab fa-github",
      "dark"
    ),
    link(
      "https://www.mheming.com/CerebroNexus/",
      "Documentation",
      "fas fa-book",
      "blue"
    ),
    link(
      "https://osmzhlab.uni-muenster.de/shiny/demo/",
      "Live demo",
      "fas fa-circle-play",
      "amber"
    ),
    '</ul>
    <br>
    <b>',
    ico("fas fa-quote-left", "purple"),
    'Citation</b><br>
    If you used CerebroNexus for your research, please cite:<br>
    Michael Heming, Xuesong Wang.
    <i>CerebroNexus</i> [software]. Version ',
    htmltools::htmlEscape(version),
    '.
    <a href="https://github.com/mihem/CerebroNexus" target="_blank" rel="noopener noreferrer">https://github.com/mihem/CerebroNexus</a>.<br>
    <br>
    <b>',
    ico("fas fa-scale-balanced", "green"),
    'License</b><br>
    CerebroNexus is distributed under the terms of the <a href=https://github.com/mihem/CerebroNexus/blob/master/LICENSE.md title="MIT license" target="_blank">MIT license.</a><br>
    <br>
    <b>',
    ico("fas fa-palette", "pink"),
    'Credit where credit is due</b><br>
    <ul class="cerebro-about-links">
      <li>',
    ico("fas fa-droplet", "blue"),
    'The default plot palettes are a custom low-saturation set; several colours draw on <a href="https://flatuicolors.com/" title="Flat UI Colors 2" target="_blank">Flat UI Colors 2</a>.</li>
    </ul>
    <br>'
  )
})
