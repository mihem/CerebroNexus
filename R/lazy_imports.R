## Imported functions that are loaded when they are first used.
##
## Loading CerebroNexus loaded every package named in its NAMESPACE: about 2 s
## before launchCerebro() could run a line, 1.3 s of it for Matrix, plotly,
## dplyr and their own imports, which the Viewer does not need to start. The
## functions below are therefore not imported in the NAMESPACE. .onLoad() binds
## each name in the package namespace as a promise that fetches the function
## from its package the first time the name is used; code in R/ and the Viewer
## files that launchCerebro() sources call them as before. A function this
## package defines itself, or one that another import already provides, keeps
## its binding.

## Matrix's generics for an S4 object, the base function for everything else:
## what importFrom(Matrix, colMeans, colSums, rowSums, t) dispatched to, without
## loading Matrix (0.65 s) until a sparse matrix needs it.
t <- function(x) {
  if (isS4(x)) Matrix::t(x) else base::t(x)
}
rowSums <- function(x, na.rm = FALSE, dims = 1L, ...) {
  if (isS4(x)) {
    Matrix::rowSums(x, na.rm = na.rm, dims = dims, ...)
  } else {
    base::rowSums(x, na.rm = na.rm, dims = dims, ...)
  }
}
colSums <- function(x, na.rm = FALSE, dims = 1L, ...) {
  if (isS4(x)) {
    Matrix::colSums(x, na.rm = na.rm, dims = dims, ...)
  } else {
    base::colSums(x, na.rm = na.rm, dims = dims, ...)
  }
}
colMeans <- function(x, na.rm = FALSE, dims = 1L, ...) {
  if (isS4(x)) {
    Matrix::colMeans(x, na.rm = na.rm, dims = dims, ...)
  } else {
    base::colMeans(x, na.rm = na.rm, dims = dims, ...)
  }
}

## Binds one name as a promise for `value`, which is only evaluated (loading
## its package) when the name is first used. It is bound where import(dplyr)
## put the names, in the imports environment between the namespace and the base
## namespace: code in the package finds it as before, and R CMD check, which
## evaluates every object of the namespace and reads the functions it gets as
## the package's own code, does not see dplyr's and plotly's internals as ours
## (cli, vctrs, tidyselect, ...). A name the package defines itself, or that one
## of its NAMESPACE imports provides, is left alone, as it took precedence over
## import(dplyr) too.
.lazyBind <- function(namespace, name, value) {
  if (
    exists(name, envir = namespace, inherits = FALSE) ||
      exists(name, envir = parent.env(namespace), inherits = FALSE)
  ) {
    return(invisible(FALSE))
  }
  delayedAssign(name, value, assign.env = parent.env(namespace))
  invisible(TRUE)
}

## Called by .onLoad() with the package namespace.
.bindLazyImports <- function(namespace) {
  lazy <- function(name, value) .lazyBind(namespace, name, value)
  ## dplyr: every export that R/ or the Viewer files refer to (a test compares
  ## this list with the code). import(dplyr) bound all 284.
  lazy("%>%", dplyr::`%>%`)
  lazy("across", dplyr::across)
  lazy("all_of", dplyr::all_of)
  lazy("any_of", dplyr::any_of)
  lazy("arrange", dplyr::arrange)
  lazy("as_data_frame", dplyr::as_data_frame)
  lazy("as_tibble", dplyr::as_tibble)
  lazy("bind_cols", dplyr::bind_cols)
  lazy("bind_rows", dplyr::bind_rows)
  lazy("compute", dplyr::compute)
  lazy("count", dplyr::count)
  lazy("desc", dplyr::desc)
  lazy("distinct", dplyr::distinct)
  lazy("enquos", dplyr::enquos)
  lazy("everything", dplyr::everything)
  lazy("expr", dplyr::expr)
  lazy("filter", dplyr::filter)
  lazy("first", dplyr::first)
  lazy("group_by", dplyr::group_by)
  lazy("groups", dplyr::groups)
  lazy("intersect", dplyr::intersect)
  lazy("last", dplyr::last)
  lazy("left_join", dplyr::left_join)
  lazy("matches", dplyr::matches)
  lazy("mutate", dplyr::mutate)
  lazy("n", dplyr::n)
  lazy("n_distinct", dplyr::n_distinct)
  lazy("n_groups", dplyr::n_groups)
  lazy("order_by", dplyr::order_by)
  lazy("pick", dplyr::pick)
  lazy("rename", dplyr::rename)
  lazy("select", dplyr::select)
  lazy("setdiff", dplyr::setdiff)
  lazy("setequal", dplyr::setequal)
  lazy("slice", dplyr::slice)
  lazy("slice_max", dplyr::slice_max)
  lazy("slice_min", dplyr::slice_min)
  lazy("src", dplyr::src)
  lazy("summarise", dplyr::summarise)
  lazy("summarize", dplyr::summarize)
  lazy("tally", dplyr::tally)
  lazy("tibble", dplyr::tibble)
  lazy("ungroup", dplyr::ungroup)
  lazy("union", dplyr::union)
  lazy("where", dplyr::where)

  lazy("datatable", DT::datatable)
  lazy("formatPercentage", DT::formatPercentage)
  lazy("formatRound", DT::formatRound)
  lazy("formatSignif", DT::formatSignif)
  lazy("formatStyle", DT::formatStyle)
  lazy("styleColorBar", DT::styleColorBar)
  lazy("styleEqual", DT::styleEqual)
  lazy("styleInterval", DT::styleInterval)

  lazy("add_lines", plotly::add_lines)
  lazy("add_trace", plotly::add_trace)
  lazy("event_data", plotly::event_data)
  lazy("layout", plotly::layout)
  lazy("plot_ly", plotly::plot_ly)
  lazy("plotlyOutput", plotly::plotlyOutput)
  lazy("renderPlotly", plotly::renderPlotly)
  lazy("toWebGL", plotly::toWebGL)

  lazy("awesomeCheckbox", shinyWidgets::awesomeCheckbox)
  lazy("dropdownButton", shinyWidgets::dropdownButton)
  lazy("materialSwitch", shinyWidgets::materialSwitch)
  lazy("radioGroupButtons", shinyWidgets::radioGroupButtons)
  lazy("sendSweetAlert", shinyWidgets::sendSweetAlert)

  lazy("colourInput", colourpicker::colourInput)

  lazy("withSpinner", shinycssloaders::withSpinner)

  lazy("mirai", mirai::mirai)
  invisible(namespace)
}

## Packages every Viewer session uses from its first reactive flush on.
## launchCerebro() loads them before it reads the data sets: with a 1.3 million
## cell CRB in memory the same loading runs into full garbage collections, and
## left to the first session it delayed the first view by 0.25 s (10,000
## cells) to 1.4 s.
.lazy_imports_session <- c("dplyr", "mirai")

## Packages only some pages need (plotly: the charts; Matrix: sparse
## expression). The Viewer loads them one at a time once a session has been
## quiet for a few seconds (`.idle_packages` in inst/viewer/shiny_server.R), so
## that the first of those pages does not wait for them: 0.5-0.9 s for plotly
## and 0.5-1.8 s for Matrix, the more the larger the data set in memory.
.lazy_imports_idle <- c("plotly", "Matrix", "colourpicker", "shinycssloaders")
