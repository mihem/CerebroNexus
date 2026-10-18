##----------------------------------------------------------------------------##
## Custom functions.
##----------------------------------------------------------------------------##
cerebroIcon <- function(name) {
  result <- icon(name)
  result$attribs[["aria-label"]] <- NULL
  result$attribs[["aria-hidden"]] <- "true"
  result
}

cerebroBox <- function(
  title,
  content,
  collapsible = TRUE,
  collapsed = FALSE
) {
  box(
    title = title,
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    collapsible = collapsible,
    collapsed = collapsed,
    content
  )
}

cerebroInfoButton <- function(
  id,
  ...,
  label = "info",
  icon = NULL,
  class = "btn-xs cerebro-info-btn"
) {
  actionButton(
    inputId = id,
    label = label,
    icon = icon,
    class = class,
    title = "Show additional information for this panel.",
    ...
  )
}

boxTitle <- function(title) {
  p(title, style = "padding-right: 5px; display: inline")
}

## Stored-result pages share a stable frame; only their demanded content is
## dynamic. This does not gate rendering or read any dataset contents.
cerebroResultPage <- function(title, controls_id, result_id, info_id) {
  tagList(
    div(class = "cerebro-viz-page-heading", h1(title)),
    uiOutput(controls_id, class = "cerebro-result-controls"),
    fluidRow(cerebroBox(
      title = tagList(boxTitle(title), cerebroInfoButton(info_id)),
      div(
        class = "cerebro-page-loading-region",
        uiOutput(result_id, class = "cerebro-result-slot")
      )
    ))
  )
}

cerebroSettingsButton <- function(id, target) {
  tags$button(
    type = "button",
    id = id,
    class = "cerebro-more-btn",
    `aria-expanded` = "false",
    `aria-controls` = target,
    `data-cerebro-drawer-target` = target,
    cerebroIcon("sliders"),
    tags$span("Settings"),
    tags$span(class = "cerebro-more-caret")
  )
}

conditionalSidebarItem <- function(label, tab_name, icon_name) {
  shiny::tagAppendAttributes(
    menuItem(label, tabName = tab_name, icon = cerebroIcon(icon_name)),
    id = paste0("sidebar_item_", tab_name),
    style = "display: none;"
  )
}

cerebroVizPageHeader <- function(title, info_id, subtitle, meta_id = NULL) {
  div(
    class = "cerebro-viz-page-heading",
    tags$h1(title),
    tags$span(id = meta_id, class = "cerebro-viz-page-meta", subtitle),
    cerebroInfoButton(
      info_id,
      label = "Info",
      icon = cerebroIcon("circle-info"),
      class = "cerebro-page-info-btn"
    )
  )
}

cerebroSettingsSection <- function(title, content, info = NULL) {
  div(
    class = "cerebro-settings-section",
    div(
      class = "cerebro-settings-heading",
      tags$span(title),
      info
    ),
    div(class = "cerebro-settings-content", content)
  )
}

cerebroSettingsDrawer <- function(id, ...) {
  div(
    id = id,
    class = "cerebro-settings-drawer",
    role = "dialog",
    `aria-modal` = "false",
    `aria-hidden` = "true",
    `aria-labelledby` = paste0(id, "_title"),
    div(
      class = "cerebro-settings-titlebar",
      tags$span(id = paste0(id, "_title"), "Settings"),
      tags$button(
        type = "button",
        class = "cerebro-settings-close",
        `data-cerebro-drawer-close` = "",
        `aria-label` = "Close Settings",
        HTML("&times;")
      )
    ),
    div(class = "cerebro-settings-body", ...)
  )
}

cerebroCellViewOutput <- function(id) {
  div(
    id = paste0(id, "_cell_view_host"),
    class = "coordviews-page cerebro-cell-view-host cerebro-page-loading-region",
    `data-cell-view-id` = id,
    div(class = "cerebro-cell-view-surface", `aria-live` = "polite"),
    shiny::uiOutput(
      paste0(id, "_composition"),
      class = "cerebro-selection-composition-slot"
    )
  )
}

cerebroShareButton <- function(
  plot_id,
  ...,
  class = "cerebro-config-open cerebro-toolbar-share"
) {
  tags$button(
    type = "button",
    class = class,
    ...,
    `data-view-id` = plot_id,
    disabled = "disabled",
    `aria-disabled` = "true",
    `aria-haspopup` = "dialog",
    `aria-controls` = "cv-config-dialog",
    cerebroIcon("share-alt"),
    tags$span("Share")
  )
}

cerebroToolbarActions <- function(settings_button, share_button = NULL) {
  div(
    class = "cerebro-toolbar-actions",
    share_button,
    settings_button
  )
}

cerebroSelectionStatus <- function(
  plot_id,
  count_output_id,
  client_actions = TRUE,
  portable = TRUE,
  extra_actions = NULL
) {
  focus_button <- function() {
    if (!client_actions) {
      return(NULL)
    }
    tags$button(
      id = paste0(plot_id, "_focus_selection"),
      type = "button",
      class = paste(
        "btn btn-xs btn-default btn-breathing",
        "cerebro-selection-action-focus"
      ),
      `data-cell-view-id` = plot_id,
      `data-cell-view-action` = "focus",
      `aria-pressed` = "false",
      cerebroIcon("crop-simple"),
      tags$span("Focus")
    )
  }
  clear_button <- function() {
    input_id <- paste0(plot_id, "_clear_selection")
    contents <- tagList(cerebroIcon("eraser"), tags$span("Clear"))
    class <- paste(
      "btn btn-xs btn-default btn-breathing",
      "cerebro-selection-action-clear"
    )
    if (!client_actions) {
      return(actionButton(input_id, contents, class = class))
    }
    tags$button(
      id = input_id,
      type = "button",
      class = class,
      `data-cell-view-id` = plot_id,
      `data-cell-view-action` = "clear",
      contents
    )
  }
  tagList(
    if (portable) cerebroShareButton(plot_id),
    div(
      class = "cerebro-selection-status-slot",
      div(
        id = paste0(plot_id, "_selection_guide"),
        class = "cerebro-selection-status-guide",
        tags$span(
          class = "cerebro-selection-status-kicker",
          cerebroIcon("arrow-pointer"),
          "Selection workspace"
        ),
        tags$span(
          class = "cerebro-selection-status-text",
          "Drag on the plot to create an active cohort."
        )
      ),
      tags$div(
        id = paste0(plot_id, "_selection_active"),
        class = paste(
          "cerebro-selection-status-active",
          "cerebro-selection-status-hidden"
        ),
        `aria-live` = "polite",
        tags$span(class = "cerebro-selection-status-kicker", "Active cohort"),
        shiny::tagAppendAttributes(
          htmlOutput(count_output_id, inline = TRUE),
          class = "cerebro-selection-status-count"
        ),
        tags$div(
          class = "cerebro-selection-actions",
          focus_button(),
          extra_actions,
          clear_button()
        )
      )
    )
  )
}

## Read an entire file into a single string. Used to inline .js/.svg/.html
## assets into the UI. readChar reads `size` bytes (the file's byte count) and
## stops at EOF, which faithfully covers ASCII/UTF-8 assets. Defined here,
## before the per-tab UI.R files are sourced with local = TRUE, so it is in
## scope for every one of them.
cerebro_read_file <- function(path) {
  readChar(path, file.info(path)$size)
}

## Serve the www/ directory as cacheable static assets, so the app's own CSS/JS
## are delivered as <link>/<script src> (browser-cached, downloaded in parallel,
## deferred) instead of being inlined into every page's HTML on every connection.
## Runs once when this file is sourced — by inst/app.R and by exported apps alike
## (both source shiny_UI.R with Cerebro.options already set).
cerebro_www_dir <- normalizePath(
  file.path(Cerebro.options[["cerebro_root"]], "viewer/www"),
  mustWork = FALSE
)
## Register under a prefix UNIQUE to this directory. addResourcePath's namespace
## is process-global, so a fixed "cerebro_www" would let a second exported app in
## the same process silently replace the first app's mapping and serve assets
## from the wrong bundle. Reuse a prefix already pointing here (idempotent);
## otherwise allocate a fresh, unused one. NULL = registration unavailable, so
## callers inline the asset instead.
cerebro_www_prefix <- local({
  if (!dir.exists(cerebro_www_dir)) {
    return(NULL)
  }
  existing <- shiny::resourcePaths()
  same <- names(existing)[vapply(
    existing,
    function(p) normalizePath(p, mustWork = FALSE) == cerebro_www_dir,
    logical(1)
  )]
  if (length(same)) {
    return(same[[1]])
  }
  prefix <- "cerebro_www"
  i <- 1L
  while (prefix %in% names(existing)) {
    prefix <- paste0("cerebro_www_", i)
    i <- i + 1L
  }
  tryCatch(
    {
      shiny::addResourcePath(prefix, cerebro_www_dir)
      prefix
    },
    error = function(e) NULL
  )
})

## Content-hashed URL for a registered asset: append ?v=<md5> so a redeployed
## app at the same address cannot keep serving a stale cached file. The token
## changes exactly when the file's bytes change — which is when the browser must
## refetch. (The per-directory prefix only prevents same-process path
## collisions; the path is identical across versions, so it does nothing for
## cross-version caching.)
cerebro_asset_url <- function(file) {
  url <- paste0(cerebro_www_prefix, "/", file)
  ver <- tryCatch(
    unname(tools::md5sum(file.path(cerebro_www_dir, file))),
    error = function(e) NA_character_
  )
  if (is.na(ver)) url else paste0(url, "?v=", substr(ver, 1, 10))
}

## Emit a <link>/<script> for a www asset, or — when registration was
## unavailable — inline the file contents so the page still works instead of
## 404-ing. This is the fallback the previous cerebro_asset() described in a
## comment but never actually provided.
cerebro_css <- function(file) {
  if (!is.null(cerebro_www_prefix)) {
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = cerebro_asset_url(file)
    )
  } else {
    tags$style(HTML(cerebro_read_file(file.path(cerebro_www_dir, file))))
  }
}
cerebro_js <- function(file, defer = FALSE) {
  if (!is.null(cerebro_www_prefix)) {
    src <- cerebro_asset_url(file)
    if (defer) tags$script(defer = NA, src = src) else tags$script(src = src)
  } else {
    tags$script(HTML(cerebro_read_file(file.path(cerebro_www_dir, file))))
  }
}

##----------------------------------------------------------------------------##
## timeout function
##----------------------------------------------------------------------------##

timeoutSeconds <- 600
timeoutWarningSeconds <- 60

inactivity <- sprintf(
  "function idleTimer() {
var warningTimer;
var logoutTimer;
var countdownTimer;
var deadline;
var previousFocus;
var warning = document.getElementById('session-timeout-warning');
var countdown = document.getElementById('session-timeout-countdown');
var continueButton = document.getElementById('session-timeout-continue');

function clearTimers() {
clearTimeout(warningTimer);
clearTimeout(logoutTimer);
clearInterval(countdownTimer);
}

function closeWarning(restoreFocus) {
clearInterval(countdownTimer);
if (warning && warning.open) warning.close();
if (restoreFocus && previousFocus && previousFocus.isConnected) previousFocus.focus();
previousFocus = null;
}

function logout() {
clearTimers();
closeWarning(false);
Shiny.setInputValue('timeOut', '%ss')
}

function updateCountdown() {
if (!countdown) return;
countdown.textContent = Math.max(0, Math.ceil((deadline - Date.now()) / 1000));
}

function showWarning() {
if (!warning) return;
document.dispatchEvent(new CustomEvent('cerebro:overlay-opening', {
detail: { owner: 'session-timeout' }
}));
previousFocus = document.activeElement;
deadline = Date.now() + %s;
updateCountdown();
countdownTimer = setInterval(updateCountdown, 1000);
if (!warning.open) warning.showModal();
if (continueButton) continueButton.focus();
}

function resetTimer() {
clearTimers();
closeWarning(true);
warningTimer = setTimeout(showWarning, %s);
logoutTimer = setTimeout(logout, %s);
}

if (continueButton) continueButton.addEventListener('click', resetTimer);
if (warning) warning.addEventListener('cancel', function(event) {
event.preventDefault();
resetTimer();
});
['mousemove', 'mousedown', 'click', 'scroll', 'keydown'].forEach(function(name) {
window.addEventListener(name, resetTimer, { passive: name !== 'keydown' });
});
resetTimer();
}
idleTimer();",
  timeoutSeconds,
  timeoutWarningSeconds * 1000,
  (timeoutSeconds - timeoutWarningSeconds) * 1000,
  timeoutSeconds * 1000
)

##----------------------------------------------------------------------------##
## Load UI content for each tab.
##----------------------------------------------------------------------------##
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/load_data/UI.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/overview/UI.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/groups/UI.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/marker_genes/UI.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/gene_expression/UI.R"),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/gene_id_conversion/UI.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/color_management/UI.R"
  ),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/about/UI.R"),
  local = TRUE
)

## Enhanced module UIs.
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/most_expressed_genes/UI.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/enriched_pathways/UI.R"
  ),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/extra_material/UI.R"),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/immune_repertoire/UI.R"
  ),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/trajectory/UI.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/spatial/UI.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/trekker/UI.R"),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/coordinated_views/UI.R"
  ),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/hla_tcr_motifs/UI.R"),
  local = TRUE
)

##----------------------------------------------------------------------------##
## Create dashboard with different tabs.
##----------------------------------------------------------------------------##
## Use the recorded version so standalone bundles need no installed package.
brand_version <- Cerebro.options[["cerebro_version"]]
brand_version_label <- NULL
if (
  length(brand_version) &&
    !is.na(brand_version[[1L]]) &&
    nzchar(as.character(brand_version[[1L]]))
) {
  brand_version_label <- tags$span(
    class = "cerebro-brand-version",
    paste0("v", as.character(brand_version[[1L]]))
  )
}
ui <- dashboardPage(
  title = "CerebroNexus",
  ## Header is collapsed to zero height by the theme (see www/custom.css); the
  ## brand now lives at the top of the sidebar. We keep an empty
  ## dashboardHeader() because shinydashboard requires one for layout.
  dashboardHeader(title = NULL),
  dashboardSidebar(
    tags$head(tags$style(HTML(".content-wrapper {overflow-x: scroll;}"))),
    div(
      class = "cerebro-brand",
      ## Rounded-geometric wordmark: the letters are vector outlines (Fredoka,
      ## SIL OFL), so it renders identically without depending on any installed
      ## font. Cerebro in near-black, Nexus in the amber accent. Kept in its own
      ## file (www/cerebronexus.svg) rather than inlined as ~10KB of path data.
      HTML(
        paste(
          readLines(
            paste0(
              Cerebro.options[["cerebro_root"]],
              "/viewer/www/cerebronexus.svg"
            ),
            warn = FALSE
          ),
          collapse = ""
        )
      ),
      brand_version_label
    ),
    tags$button(
      type = "button",
      id = "cerebro-nav-close",
      class = "cerebro-nav-close",
      `aria-label` = "Close navigation",
      HTML("&times;")
    ),
    uiOutput("crb_file_selector_UI", class = "cerebro-dataset-switcher"),
    sidebarMenu(
      id = "sidebar",
      menuItem(
        "Data info",
        tabName = "loadData",
        icon = cerebroIcon("info"),
        selected = TRUE
      ),
      menuItem(
        "Projection",
        tabName = "overview",
        icon = cerebroIcon("home")
      ),
      conditionalSidebarItem(
        "Linked views",
        "coordinated_views",
        "link"
      ),
      menuItem(
        "Groups",
        tabName = "groups",
        icon = cerebroIcon("layer-group")
      ),
      ## Conditional items are present when shinydashboard binds the sidebar,
      ## then shown only for data sets that support them. Keeping their anchors
      ## in the initial DOM lets updateTabItems() select them reliably.
      conditionalSidebarItem("Marker genes", "markerGenes", "list-alt"),
      conditionalSidebarItem(
        "Most expressed genes",
        "mostExpressedGenes",
        "bullhorn"
      ),
      conditionalSidebarItem(
        "Enriched pathways",
        "enrichedPathways",
        "sitemap"
      ),
      conditionalSidebarItem("Extra material", "extra_material", "gift"),
      conditionalSidebarItem(
        "Immune repertoire",
        "immune_repertoire",
        "dna"
      ),
      conditionalSidebarItem("Trajectory", "trajectory", "route"),
      conditionalSidebarItem("Spatial", "spatial", "map-pin"),
      conditionalSidebarItem("Trekker", "trekker", "map-marked-alt"),
      conditionalSidebarItem(
        "HLA & TCR Motifs",
        "hla_tcr_motifs",
        "fingerprint"
      ),
      menuItem(
        "Gene expression",
        tabName = "geneExpression",
        icon = cerebroIcon("signal")
      ),
      menuItem(
        "Gene ID conversion",
        tabName = "geneIdConversion",
        icon = cerebroIcon("barcode")
      ),
      menuItem(
        "Colour management",
        tabName = "color_management",
        icon = cerebroIcon("palette")
      ),
      menuItem("About", tabName = "about", icon = cerebroIcon("at"))
    ),
    uiOutput("preferences_options", class = "cerebro-sidebar-preferences")
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$button(
      type = "button",
      id = "cerebro-nav-scrim",
      `aria-label` = "Close navigation",
      `aria-hidden` = "true",
      tabindex = "-1"
    ),
    tags$div(
      id = "cerebro-update-status",
      class = "sr-only",
      role = "status",
      `aria-live` = "polite",
      `aria-atomic` = "true"
    ),
    tags$dialog(
      id = "session-timeout-warning",
      role = "dialog",
      `aria-modal` = "true",
      `aria-labelledby` = "session-timeout-warning-title",
      `aria-describedby` = "session-timeout-warning-message",
      tags$h2(id = "session-timeout-warning-title", "Session expiring"),
      tags$p(
        id = "session-timeout-warning-message",
        "Your session will end in ",
        tags$span(id = "session-timeout-countdown", "60"),
        " seconds unless you continue."
      ),
      tags$button(
        id = "session-timeout-continue",
        type = "button",
        class = "btn btn-primary",
        "Continue session"
      )
    ),
    ## App CSS/JS as cacheable static resources (served from the cerebro_www
    ## resource path registered above) instead of inlined into every page. The
    ## browser caches them across connections and downloads them in parallel;
    ## scripts are deferred so they run after the document parses (each is a
    ## self-contained IIFE with its own Shiny-readiness retry, so order-safe).
    ##  - custom.css      : Console design language; overrides AdminLTE 2 chrome.
    ##  - fill_height.js  : sizes any .cerebro-fill element to the live viewport.
    ##  - trekker.css     : Trekker insight/QC presentation shared with Linked views.
    ##  - hla_motifs.*    : modebar over the visNetwork motif network.
    tags$head(
      tags$style(HTML(
        "html, body { background: #f7f7f9; } .cerebro-starting .wrapper { visibility: hidden; } #cerebro-startup { position: fixed; inset: 0; z-index: 2147483000; display: grid; place-items: center; background: #f7f7f9; color: #51545c; font: 16px Arial, sans-serif; transition: opacity .12s ease-out; } #cerebro-startup > div { text-align: center; } #cerebro-startup button { margin-top: 16px; } @media (prefers-reduced-motion: reduce) { #cerebro-startup { transition: none; } }"
      )),
      tags$script(HTML(
        "document.documentElement.classList.add('cerebro-starting');"
      )),
      cerebro_js("page-lifecycle.js", defer = TRUE),
      cerebro_js("startup.js", defer = TRUE),
      cerebro_css("custom.css"),
      cerebro_css("trekker.css"),
      cerebro_css("hla_motifs.css"),
      cerebro_css("coordviews.css"),
      cerebro_js("fill_height.js", defer = TRUE),
      cerebro_js("cv-geom.js", defer = TRUE),
      cerebro_js("cell_views_state.js", defer = TRUE),
      cerebro_js("dataset-context.js"),
      cerebro_js("cell_points_gpu.js"),
      cerebro_js("cell_views_wire.js"),
      cerebro_js("hla_motifs.js", defer = TRUE),
      cerebro_js("coordviews-config.js", defer = TRUE),
      cerebro_js("viewer-shell.js", defer = TRUE),
      cerebro_js("gene-feedback.js", defer = TRUE),
      cerebro_js("view-feedback.js", defer = TRUE),
      cerebro_js("multiselect.js", defer = TRUE),
      cerebro_js("gene-selector.js", defer = TRUE),
      cerebro_js("background-model.js"),
      cerebro_js("cell_views.js"),
      cerebro_js("settings_drawer.js", defer = TRUE),
      cerebro_js("specialist-view-state.js", defer = TRUE)
    ),
    tags$main(
      id = "main-content",
      tabindex = "-1",
      tags$div(
        id = "cerebro-page-loader-announcement",
        class = "sr-only",
        role = "status",
        `aria-live` = "polite",
        `aria-atomic` = "true"
      ),
      tags$div(
        id = "cerebro-page-loader",
        class = "cerebro-page-loader",
        `aria-hidden` = "true",
        hidden = "hidden",
        tags$span(
          class = "cerebro-page-loader-spinner",
          `aria-hidden` = "true"
        ),
        tags$span(
          class = "cerebro-page-loader-copy",
          tags$span(
            id = "cerebro-page-loader-detail",
            "Preparing data and visualisation…"
          )
        )
      ),
      tabItems(
        tab_load_data,
        tab_overview,
        tab_coordinated_views,
        tab_groups,
        tab_marker_genes,
        tab_most_expressed_genes,
        tab_enriched_pathways,
        tab_extra_material,
        tab_immune_repertoire,
        tab_trajectory,
        tab_spatial,
        tab_trekker,
        tab_hla_tcr_motifs,
        tab_gene_expression,
        tab_gene_id_conversion,
        tab_color_management,
        tab_about
      )
    ),
    tags$script(HTML(inactivity))
  )
)

ui$children <- tagList(
  tags$div(
    id = "cerebro-startup",
    tags$div(
      tags$div(
        style = "width:200px;margin:0 auto 24px",
        `aria-hidden` = "true",
        HTML(paste(
          readLines(
            file.path(
              Cerebro.options[["cerebro_root"]],
              "viewer/www/cerebronexus.svg"
            ),
            warn = FALSE
          ),
          collapse = "\n"
        ))
      ),
      tags$div(
        role = "status",
        `aria-live` = "polite",
        `data-startup-message` = "",
        "Opening workspace\u2026"
      ),
      tags$button(
        type = "button",
        class = "btn btn-default",
        `data-startup-retry` = "",
        hidden = "hidden",
        "Reload"
      )
    )
  ),
  tags$a(
    class = "cerebro-skip-link",
    href = "#main-content",
    "Skip to main content"
  ),
  ui$children
)

source(
  file.path(Cerebro.options[["cerebro_root"]], "viewer/static_ui.R"),
  local = TRUE
)
ui <- viewerStaticUI(ui)
