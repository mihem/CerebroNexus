# test-coordinated-views.R — Tests for the Linked views bundle builders.
#
# cv_build_bundle() and its cv_build_* / cv_* helpers (coordinated_views/bundle.R)
# turn a loaded Cerebro object into ONE client bundle. They are pure — no
# input/output/reactive scope — so we source the file into an isolated env and
# drive it directly. Two past regressions are guarded here explicitly:
#   * a single-level group / single clonotype serialising as a JSON scalar
#     instead of an array (the I()/AsIs contract in cv_group/cv_space/cv_clone),
#     which made the client throw mid-update and keep the previous data set; and
#   * a standard `spatial` slot and a `trekker` slot collapsing into one space
#     instead of two — each builder keeps its own space identity.

local_inst <- viewer_app_test_path()
bundle_file <- viewer_test_path("coordinated_views", "bundle.R")
tcr_crb <- file.path(local_inst, "extdata/examples/demo_full_tcr_bcr.crb")
trekker_crb <- file.path(local_inst, "extdata/examples/demo_trekker.crb")

## Source the pure builders into an isolated env (no app scope). The app-only
## helpers cerebro_group_colors() / Cerebro.options are absent here; bundle.R
## guards each, so cv_build_bundle() uses its fallback palette.
##
## clone_contract.R is NOT optional and is deliberately not guarded: it defines
## what a clone is, and the app sources it before any module for the same reason
## this env has to. A bundle built without it would silently answer that question
## on its own again, which is the divergence the file exists to prevent.
contract_file <- file.path(dirname(bundle_file), "..", "clone_contract.R")
utility_file <- file.path(dirname(bundle_file), "..", "utility_functions.R")
cv_env <- new.env()
have_bundle <- nzchar(bundle_file) &&
  file.exists(bundle_file) &&
  file.exists(contract_file) &&
  file.exists(utility_file)
if (have_bundle) {
  sys.source(utility_file, envir = cv_env)
  sys.source(
    file.path(dirname(bundle_file), "..", "spatial", "func_spatial_helpers.R"),
    envir = cv_env
  )
  sys.source(contract_file, envir = cv_env)
  sys.source(bundle_file, envir = cv_env)
}

new_cv_image_session <- function() {
  callbacks <- new.env(parent = emptyenv())
  session <- new.env(parent = emptyenv())
  session$userData <- new.env(parent = emptyenv())
  session$callbacks <- callbacks
  session$registerDataObj <- function(name, data, handler) {
    assign(name, function() handler(data, NULL), envir = callbacks)
    paste0("session/", name)
  }
  session
}

test_that("linked spatial images report decode failures and reject stale completions", {
  js <- paste(
    readLines(file.path(dirname(bundle_file), "..", "www", "cell_views.js")),
    collapse = "\n"
  )
  server <- paste(
    readLines(file.path(dirname(bundle_file), "server.R")),
    collapse = "\n"
  )

  expect_match(js, "im.onerror = function", fixed = TRUE)
  expect_match(js, "if (!stillCurrent()) return;", fixed = TRUE)
  expect_match(
    js,
    "coordviews_spatial_image_load_error",
    fixed = TRUE
  )
  expect_match(
    server,
    'input[["coordviews_spatial_image_load_error"]]',
    fixed = TRUE
  )
  expect_match(server, "The browser could not decode", fixed = TRUE)
})

test_that("Viewer Colour by honours registered groups and main group", {
  skip_if_not(have_bundle)
  helper <- get0("viewerColourGroupChoices", envir = cv_env)
  expect_true(is.function(helper))
  if (!is.function(helper)) {
    return()
  }

  metadata <- data.frame(
    cell_barcode = c("c1", "c2", "c3"),
    sample = c("s1", "s1", "s2"),
    seurat_clusters = c("0", "1", "0"),
    nUMI = c(100, 200, 300),
    nGene = c(50, 75, 100),
    orig.ident = c("old-a", "old-a", "old-b"),
    stringsAsFactors = FALSE
  )
  result <- helper(
    metadata,
    c("sample", "seurat_clusters"),
    "seurat_clusters"
  )

  expect_identical(result$choices, c("sample", "seurat_clusters"))
  expect_identical(result$selected, "seurat_clusters")
  expect_false(any(c("nUMI", "nGene", "orig.ident") %in% result$choices))
  expect_identical(
    helper(metadata, c("missing", "sample"), "missing")$selected,
    "sample"
  )
  expect_identical(helper(metadata, character(), "sample")$choices, character())
  expect_null(helper(metadata, character(), "sample")$selected)
})

test_that("Linked views excludes unregistered metadata colourings", {
  skip_if_not(have_bundle)
  cells <- c("c1", "c2", "c3")
  metadata <- data.frame(
    cell_barcode = cells,
    sample = c("s1", "s1", "s2"),
    seurat_clusters = c("0", "1", "0"),
    nUMI = c(100, 200, 300),
    nGene = c(50, 75, 100),
    orig.ident = c("old-a", "old-a", "old-b"),
    row.names = cells,
    stringsAsFactors = FALSE
  )
  crb <- list(
    getMetaData = function() metadata,
    getGroups = function() c("sample", "seurat_clusters"),
    getParameters = function() list(main_group = "seurat_clusters"),
    availableProjections = function() "umap",
    getProjection = function(name) {
      matrix(1:6, nrow = 3, dimnames = list(cells, c("x", "y")))
    },
    availableSpatial = function() character(),
    getTrekker = function() NULL,
    getImmuneRepertoire = function() NULL,
    getGeneNames = function() c("ACTB", "GAPDH")
  )

  bundle <- cv_env$cv_build_bundle(crb)

  expect_named(bundle$groups, c("sample", "seurat_clusters"))
  expect_length(bundle$cat_extra, 0L)
  expect_length(grep("^meta:", names(bundle$fields)), 0L)
  expect_identical(bundle$default_group, "seurat_clusters")
  expect_null(bundle$genes) # PR5 populates the gene selector on demand.
})

test_that("trajectory coordinates and graph enter Linked views", {
  skip_if_not(have_bundle, "coordinated_views/bundle.R not found")

  trajectory <- list(
    meta = data.frame(
      DR_1 = c(10, 20),
      DR_2 = c(30, 40),
      pseudotime = c(0, 1),
      state = factor(c("1", "2")),
      row.names = c("cell_b", "cell_c")
    ),
    edges = data.frame(
      source_dim_1 = 10,
      source_dim_2 = 30,
      target_dim_1 = 20,
      target_dim_2 = 40
    )
  )
  crb <- list(
    getMethodsForTrajectories = function() "monocle2",
    getNamesOfTrajectories = function(method) {
      expect_identical(method, "monocle2")
      "B_cell_maturation"
    },
    getTrajectory = function(method, name) {
      expect_identical(method, "monocle2")
      expect_identical(name, "B_cell_maturation")
      trajectory
    }
  )

  spaces <- cv_env$cv_build_trajectories(
    crb,
    c("cell_a", "cell_b", "cell_c")
  )

  expect_length(spaces, 1)
  expect_identical(spaces[[1]]$id, "trajectory::monocle2::B_cell_maturation")
  expect_identical(spaces[[1]]$label, "B cell maturation (trajectory)")
  expect_equal(unclass(spaces[[1]]$x), c(NA, 10, 20))
  expect_equal(unclass(spaces[[1]]$y), c(NA, 30, 40))
  expect_true(isTRUE(spaces[[1]]$trajectory))
  expect_length(spaces[[1]]$edges, 1)
})

run_config_transport_node <- function(body) {
  run_node_script(c(
    "const fs = require('fs');",
    "const timers = []; const sent = []; const handlers = {}; let applied = 0;",
    "const events = {}; let ready = true, selectedCells = 1, captures = 0;",
    "function classes(){return {toggle:()=>{},add:()=>{},remove:()=>{}};}",
    "function element(id){return {id:id,value:'',disabled:false,open:false,attrs:{},",
    "  classList:classes(),listeners:{},addEventListener:function(n,f){this.listeners[n]=f;},",
    "  setAttribute:function(n,v){this.attrs[n]=v;},getAttribute:function(n){return this.attrs[n]||null;},",
    "  removeAttribute:function(n){delete this.attrs[n];},focus:()=>{},click:function(){",
    "    if(this.listeners.click)this.listeners.click.call(this,{currentTarget:this});}};}",
    "const ids = {}; ['cv-config-dialog','cv-config-open','cv-config-close','cv-config-png',",
    "  'cv-config-download','coordviews_config_upload','cv-config-upload-button',",
    "  'cv-config-status'].forEach(id=>{ids[id]=element(id);});",
    "const host={classList:classes()}; const label={textContent:'Download PNG'};",
    "ids.coordviews_config_upload.closest=()=>host;",
    "ids['cv-config-png'].querySelector=()=>label;",
    "ids['cv-config-dialog'].showModal=function(){this.open=true;};",
    "ids['cv-config-dialog'].close=function(){this.open=false;};",
    "global.document={readyState:'complete',activeElement:null,body:{appendChild:()=>{}},",
    "  getElementById:id=>ids[id]||null,querySelectorAll:()=>[],addEventListener:()=>{},",
    "  contains:()=>true,createElement:()=>element('created')};",
    "global.window=global; window.devicePixelRatio=1; window.addEventListener=(name,fn)=>{events[name]=fn;};",
    "window.setTimeout=(fn,delay)=>{const timer={fn:fn,delay:delay};timers.push(timer);return timer;};",
    "window.clearTimeout=timer=>{const at=timers.indexOf(timer);if(at>=0)timers.splice(at,1);};",
    "window.FileReader=function(){this.readAsText=file=>{this.result=file.text;this.onload();};};",
    "const datasetContext={epoch:'session-1',dataset_key:'dataset-1',generation:1};",
    "window.CerebroDatasetContext={phase:()=> 'ready',current:()=>datasetContext,",
    "  key:value=>value?JSON.stringify([value.epoch,value.dataset_key,value.generation]):'',",
    "  accepts:message=>!!(message&&message.dataset_context&&",
    "    message.dataset_context.epoch===datasetContext.epoch&&",
    "    message.dataset_context.dataset_key===datasetContext.dataset_key&&",
    "    message.dataset_context.generation===datasetContext.generation)};",
    "window.cerebroLinkedViewsState={ready:()=>ready,summary:()=>({selectedCells}),",
    "  capture:()=>{captures++;return {selection:{cells:[]}};},apply:()=>{applied+=1;return {selectedCells:1};},downloadPNG:()=>true};",
    "global.Shiny={setInputValue:(id,value)=>sent.push({id:id,value:value}),",
    "  addCustomMessageHandler:(id,fn)=>{handlers[id]=fn;}};",
    js_eval_file(file.path(
      dirname(bundle_file),
      "..",
      "www",
      "coordviews-config.js"
    )),
    body
  ))
}

test_that("sharing waits for readiness but accepts an empty selection", {
  output <- run_config_transport_node(c(
    "ready=false; selectedCells=0;",
    "events['cerebro:linkedviews-ready']({detail:{ready,selectedCells}});",
    "const blocked=ids['cv-config-download'].disabled;",
    "ids['cv-config-download'].click();",
    "const blockedCaptures=captures, blockedRequests=sent.length;",
    "ready=true;",
    "events['cerebro:linkedviews-ready']({detail:{ready,selectedCells}});",
    "const allowed=!ids['cv-config-download'].disabled;",
    "ids['cv-config-download'].click();",
    "console.log(JSON.stringify({blocked,allowed,blockedCaptures,blockedRequests,captures,sent}));"
  ))
  expect_equal(attr(output, "status"), NULL)
  result <- jsonlite::fromJSON(output, simplifyVector = FALSE)
  expect_true(result$blocked)
  expect_true(result$allowed)
  expect_identical(result$blockedCaptures, 0L)
  expect_identical(result$blockedRequests, 0L)
  expect_identical(result$captures, 1L)
  expect_length(result$sent, 1L)
  expect_identical(result$sent[[1L]]$id, "coordviews_config_request")
  expect_identical(result$sent[[1L]]$value$action, "prepare")
  expect_length(result$sent[[1L]]$value$config$selection$cells, 0L)
})

test_that("JSON upload transport is atomic and ignores timed-out responses", {
  output <- run_config_transport_node(c(
    "const upload=ids.coordviews_config_upload;",
    "upload.files=[{name:'first.json',size:2,text:'{}'}];",
    "upload.listeners.change({currentTarget:upload});",
    "const first=sent[0].value; timers.find(timer=>timer.delay===10000).fn();",
    "upload.files=[{name:'second.json',size:2,text:'{}'}];",
    "upload.listeners.change({currentTarget:upload}); const second=sent[1].value;",
    "handlers.coordviews_config_result({nonce:first.nonce,action:'apply',ok:true,",
    "  dataset_context:first.dataset_context,",
    "  config:{schema:'cerebronexus-linked-view'},selected_cells:1});",
    "handlers.coordviews_config_result({nonce:second.nonce,action:'apply',ok:true,",
    "  dataset_context:second.dataset_context,",
    "  config:{schema:'cerebronexus-linked-view'},selected_cells:1});",
    "ids['cv-config-png'].listeners.click.call(ids['cv-config-png']);",
    "ids['cv-config-png'].listeners.click.call(ids['cv-config-png']);",
    "timers.filter(timer=>timer.delay===1600).forEach(timer=>timer.fn());",
    "console.log(JSON.stringify({ids:sent.map(item=>item.id),first:first,second:second,",
    "  applied:applied,label:label.textContent}));"
  ))
  result <- jsonlite::fromJSON(output, simplifyVector = FALSE)

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    result$ids,
    list("coordviews_config_upload_request", "coordviews_config_upload_request")
  )
  expect_identical(result$first$action, "apply")
  expect_identical(result$first$name, "first.json")
  expect_identical(result$first$text, "{}")
  expect_false(identical(result$first$nonce, result$second$nonce))
  expect_identical(result$applied, 1L)
  expect_identical(result$label, "Download PNG")
})

test_that("Trekker depth views form one collapsed insights region", {
  ui <- htmltools::tagQuery(viewer_ui_test_pages()$coordinated_views)
  region <- ui$find("#cv-tk-insights")
  expect_equal(region$length(), 1L)
  for (id in c(
    "cv-tk-insights-toggle",
    "cv-tk-tab-cell",
    "cv-tk-tab-qc",
    "cv-tk-tab-moran",
    "cv-tk-panel-stage",
    "cv-tk-cell-body"
  )) {
    expect_equal(region$find(paste0("#", id))$length(), 1L, info = id)
  }
  expect_identical(
    region$find("#cv-tk-insights-toggle")$selectedTags()[[1]]$attribs[[
      "aria-expanded"
    ]],
    "false"
  )
})

test_that("cv_group/cv_space/cv_clone force JSON arrays even at length 1", {
  skip_if_not(have_bundle)
  skip_if_not_installed("jsonlite")
  arr <- function(x) {
    as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
  }

  ## A single-level group: values/levels/colors must all still be JSON arrays.
  g <- cv_env$cv_group(0L, "OnlyLevel", "#123456")
  expect_s3_class(g$levels, "AsIs")
  expect_s3_class(g$colors, "AsIs")
  expect_match(arr(g$levels), "^\\[")
  expect_match(arr(g$colors), "^\\[")
  expect_match(arr(g$values), "^\\[")

  ## A single-cell space: x/y are arrays, id/label stay scalar.
  s <- cv_env$cv_space("umap", "x (expression)", 1, 2)
  expect_match(arr(s$x), "^\\[")
  expect_match(arr(s$y), "^\\[")

  ## A single-clonotype bundle: id/label/size are arrays; the true scalars
  ## (n_clones / n_receptor) stay bare so the client reads them as numbers.
  cl <- cv_env$cv_clone(0L, "CASSLGX", 1L, 1L, 1L)
  expect_match(arr(cl$size), "^\\[")
  expect_match(arr(cl$label), "^\\[")
  expect_false(startsWith(arr(cl$n_clones), "["))
})

test_that("one-cell dynamic expression messages retain JSON arrays", {
  skip_if_not_installed("jsonlite")
  skip_if_not(have_bundle, "coordinated_views/bundle.R not found")

  decode <- function(value) {
    jsonlite::fromJSON(
      jsonlite::toJSON(value, auto_unbox = TRUE),
      simplifyVector = FALSE
    )
  }

  gene <- decode(cv_env$cv_gene_message("CD3D", 128L, 1.5))
  panels <- decode(cv_env$cv_gene_panels_message(
    c("CD3D", "MS4A1"),
    list(64L, 192L),
    2
  ))
  rgb <- decode(cv_env$cv_rgb_message(
    list(v = 10L, gene = "CD3D"),
    list(v = 20L, gene = "MS4A1"),
    list(v = 30L, gene = "NKG7")
  ))

  expect_type(gene$v, "list")
  expect_true(all(vapply(panels$values, is.list, logical(1))))
  expect_type(rgb$r, "list")
  expect_type(rgb$g, "list")
  expect_type(rgb$b, "list")
  expect_type(rgb$genes, "list")
})

test_that("large linked-view vectors use a lossless compact wire format", {
  skip_if_not_installed("base64enc")
  skip_if_not_installed("jsonlite")
  skip_if_not(have_bundle, "coordinated_views/bundle.R not found")

  bundle <- list(
    cells = I(c("cell-1", "cell-2")),
    groups = list(cluster = cv_env$cv_group(c(0L, NA_integer_), "A", "#fff")),
    cat_extra = list(),
    fields = list(score = cv_env$cv_field("Score", c(0L, 1000L), 0, 1)),
    projections = list(
      umap = list(x = I(c(1.25, NA_real_)), y = I(c(-2.5, 3.75)), ndim = 2L)
    ),
    spaces = list(cv_env$cv_space("spatial", "Spatial", c(4, 5), c(6, 7))),
    clone = cv_env$cv_clone(
      c(0L, 1L),
      c("clone-a", "clone-b"),
      c(3L, 2L),
      2L,
      2L
    )
  )
  packed <- cv_env$cv_wire_pack_bundle(bundle, min_length = 1L)

  header_length <- readBin(
    packed[seq_len(4L)],
    integer(),
    size = 4L,
    endian = "little"
  )
  header <- jsonlite::fromJSON(
    rawToChar(packed[4L + seq_len(header_length)]),
    simplifyVector = FALSE
  )
  data_start <- 4L + header_length + ((4L - header_length %% 4L) %% 4L)
  decode <- function(value) {
    type <- value[["__cv_wire__"]]
    first <- data_start + value$offset + 1L
    bytes <- packed[seq.int(first, length.out = value$bytes)]
    if (identical(type, "json")) {
      return(jsonlite::fromJSON(rawToChar(bytes)))
    }
    if (identical(type, "f32")) {
      readBin(bytes, numeric(), n = value$length, size = 4L, endian = "little")
    } else {
      readBin(
        bytes,
        integer(),
        n = value$length,
        size = switch(type, i8 = 1L, i16 = 2L, 4L),
        signed = TRUE,
        endian = "little"
      )
    }
  }

  expect_identical(header$wire_format, "binary-v1")
  expect_identical(decode(header$cells), c("cell-1", "cell-2"))
  expect_identical(decode(header$groups$cluster$values), c(0L, NA_integer_))
  expect_identical(decode(header$fields$score$v), c(0L, 1000L))
  expect_equal(decode(header$projections$umap$x), c(1.25, NA_real_))
  expect_equal(decode(header$projections$umap$y), c(-2.5, 3.75))
  expect_equal(decode(header$spaces[[1L]]$x), c(4, 5))
  expect_equal(decode(header$spaces[[1L]]$y), c(6, 7))
  expect_identical(decode(header$clone$id), c(0L, 1L))
  expect_identical(decode(header$clone$label), c("clone-a", "clone-b"))
  expect_identical(decode(header$clone$size), c(3L, 2L))
})

test_that("saved per-gene panels use the dynamic payload contract", {
  skip_if_not(have_bundle, "coordinated_views/bundle.R not found")

  payload <- cv_env$cv_gene_panels_payload(
    c("CD3D", "MS4A1"),
    list(c(0, 2), c(1, 4))
  )

  expect_identical(payload$mode, "__gene_panels__")
  expect_identical(payload$max, 4)
  expect_identical(unclass(payload$genes), c("CD3D", "MS4A1"))
  expect_identical(unclass(payload$values[[1]]), c(0L, 128L))
  expect_identical(unclass(payload$values[[2]]), c(64L, 255L))
})

test_that("single-cell projections and bundle cell IDs stay JSON arrays", {
  skip_if_not(have_bundle)
  skip_if_not_installed("jsonlite")
  cells <- "c1"
  md <- data.frame(
    cell_barcode = cells,
    cluster = "A",
    row.names = cells,
    stringsAsFactors = FALSE
  )
  crb <- list(
    getMetaData = function() md,
    getGroups = function() "cluster",
    getParameters = function() list(main_group = "cluster"),
    availableProjections = function() "umap",
    getProjection = function(name) {
      matrix(c(1, 2), nrow = 1, dimnames = list(cells, c("x", "y")))
    },
    availableSpatial = function() NULL,
    getTrekker = function() NULL,
    getImmuneRepertoire = function() NULL
  )

  bundle <- cv_env$cv_build_bundle(crb)
  projection <- bundle$projections$umap
  as_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE))

  expect_s3_class(bundle$cells, "AsIs")
  expect_s3_class(projection$x, "AsIs")
  expect_s3_class(projection$y, "AsIs")
  expect_match(as_json(bundle$cells), "^\\[")
  expect_match(as_json(projection$x), "^\\[")
  expect_match(as_json(projection$y), "^\\[")
})

test_that("Linked views does not duplicate immutable bundle data", {
  skip_if_not(have_bundle)
  cells <- c("c1", "c2")
  metadata <- data.frame(
    cell_barcode = cells,
    cluster = c("A", "B"),
    row.names = cells,
    stringsAsFactors = FALSE
  )
  crb <- list(
    getMetaData = function() metadata,
    getGroups = function() "cluster",
    getParameters = function() list(main_group = "cluster"),
    availableProjections = function() "umap",
    getProjection = function(name) {
      matrix(1:4, nrow = 2, dimnames = list(cells, c("x", "y")))
    },
    availableSpatial = function() NULL,
    getTrekker = function() NULL,
    getImmuneRepertoire = function() NULL,
    getGeneNames = function() character()
  )

  bundle <- cv_env$cv_build_bundle(crb)
  expression_space <- bundle$spaces[[which(
    vapply(bundle$spaces, `[[`, character(1), "id") == "expression"
  )]]

  expect_true(all(c("x", "y") %in% names(bundle$projections$umap)))
  expect_false(any(c("x", "y", "z") %in% names(expression_space)))
  expect_null(bundle$cell_fingerprint)
})

test_that("primary bundle materializes only the first visible projection and colour", {
  skip_if_not(have_bundle)
  cells <- paste0("c", 1:4)
  metadata <- data.frame(
    cell_barcode = cells,
    cell_type = c("B", "T", "B", "T"),
    sample = c("s1", "s1", "s2", "s2"),
    donor = c("d1", "d2", "d1", "d2"),
    score = c(1, 2, 3, 4),
    identifier = cells,
    row.names = cells,
    stringsAsFactors = FALSE
  )
  calls <- new.env(parent = emptyenv())
  calls$projections <- character()
  calls$genes <- 0L
  calls$immune <- 0L
  crb <- list(
    getMetaData = function() metadata,
    getGroups = function() c("cell_type", "sample"),
    getParameters = function() list(main_group = "cell_type"),
    availableProjections = function() c("umap", "tsne"),
    getProjection = function(name) {
      calls$projections <- c(calls$projections, name)
      matrix(seq_len(8), nrow = 4, dimnames = list(cells, c("x", "y")))
    },
    availableSpatial = function() NULL,
    getTrekker = function() NULL,
    getImmuneRepertoire = function() {
      calls$immune <- calls$immune + 1L
      NULL
    },
    getGeneNames = function() {
      calls$genes <- calls$genes + 1L
      c("CD3D", "MS4A1")
    }
  )

  primary <- cv_env$cv_build_bundle(crb, primary_only = TRUE)

  expect_named(primary$groups, "cell_type")
  expect_length(primary$cat_extra, 0L)
  expect_length(primary$fields, 0L)
  expect_length(primary$genes, 0L)
  expect_named(primary$projections, "umap")
  expect_identical(calls$projections, "umap")
  expect_identical(calls$genes, 0L)
  expect_identical(calls$immune, 0L)

  primary$dataset_fingerprint <- "md5-cell-set-v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  primary$progressive_token <- 4L
  supplement <- cv_env$cv_build_progressive_supplement(crb, primary)

  expect_identical(calls$immune, 1L)
  expect_identical(calls$genes, 0L)
  expect_identical(supplement$dataset_id, primary$dataset_id)
  expect_identical(
    supplement$dataset_fingerprint,
    primary$dataset_fingerprint
  )
  expect_identical(supplement$progressive_token, primary$progressive_token)
  expect_named(supplement$groups, "sample")
  expect_length(supplement$cat_extra, 0L)
  expect_length(supplement$fields, 0L)
  expect_named(supplement$projections, "tsne")
  expect_true(isTRUE(supplement$groups$sample$deferred))
  expect_null(supplement$groups$sample$values)
  expect_null(supplement$cat_extra$donor)
  expect_null(supplement$fields[["meta:score"]])
})

test_that("primary bundle reuses thin CRB first-frame fields", {
  skip_if_not(have_bundle)
  metadata <- data.frame(
    cell_type = factor(c("B", "T", "B", "T")),
    sample = factor(c("s1", "s1", "s2", "s2"))
  )
  projection <- data.frame(
    x = c(1, 2, 3, 4),
    y = c(4, 3, 2, 1)
  )
  crb <- list(
    getMetaData = function() stop("metadata hydration was forced"),
    getGroups = function() c("cell_type", "sample"),
    getParameters = function() list(main_group = "cell_type"),
    availableProjections = function() "umap",
    getProjection = function(name) stop("projection hydration was forced"),
    availableSpatial = function() NULL,
    getTrekker = function() NULL,
    getImmuneRepertoire = function() NULL,
    getGeneNames = function() character()
  )

  primary <- cv_env$cv_build_bundle(
    crb,
    primary_only = TRUE,
    first_frame = list(
      meta_data = metadata,
      projections = list(umap = projection),
      canonical_projections = "umap"
    )
  )

  expect_named(primary$groups, "cell_type")
  expect_named(primary$projections, "umap")
  expect_identical(unclass(primary$projections$umap$x), c(1, 2, 3, 4))
  expect_identical(unclass(primary$cells), seq_len(4L))
})

test_that("thin preloaded primary and supplement align unverified projections once", {
  skip_if_not(have_bundle)
  skip_if_not_installed("BPCells")
  root <- withr::local_tempdir()
  cells <- c("3", "1", "2") # Numeric-looking barcodes are not row indices.
  counts <- Matrix::Matrix(
    matrix(1:6, 2, dimnames = list(c("g1", "g2"), cells)),
    sparse = TRUE
  )
  object <- Cerebro$new()
  object$setExpression(BPCells::write_matrix_dir(
    counts,
    file.path(root, "matrix.bpcells")
  ))
  object$setExpressionBackend("bpcells", "matrix.bpcells")
  object$setMetaData(data.frame(cell_barcode = cells, sample = "s"))
  object$addGroup("sample", "s")
  for (name in c("umap", "tsne")) {
    object$addProjection(
      name,
      data.frame(x = c(30, 20, 10), y = 3:1, row.names = rev(cells))
    )
  }
  path <- file.path(root, "test.crb")
  saveCerebro(object, path, viewer_binary = "never")
  scope <- new.env(parent = globalenv())
  sys.source(file.path(dirname(utility_file), "crb_cache.R"), scope)
  sys.source(utility_file, scope)
  ## The bundle resolves author display defaults; installed checks cannot
  ## reach the package's internal copy of these helpers.
  sys.source(viewer_test_path("core", "viewer_content_contract.R"), scope)
  sys.source(contract_file, scope)
  sys.source(bundle_file, scope)
  loaded <- scope$get_or_load_crb(path)
  frame <- scope$viewerProjectionFirstFrameCache(loaded)
  expect_length(frame$canonical_projections, 0L)
  primary <- scope$cv_build_bundle(
    loaded,
    primary_only = TRUE,
    first_frame = frame
  )
  expect_equal(as.numeric(primary$projections$umap$x), c(10, 20, 30))
  expect_identical(rownames(frame$aligned_projections$umap), cells)
  attr(loaded, "cerebro_viewer_pack") <- list(
    manifest = list(
      n_cells = 3L,
      cell_order_fingerprint = "test-order"
    )
  )
  primary$canonical_order_id <- "test-order"
  supplement <- scope$cv_build_compact_supplement(
    loaded,
    primary,
    frame,
    include_clone = FALSE
  )
  expect_equal(as.numeric(supplement$projections$tsne$x), c(10, 20, 30))
  expect_named(
    as.list(frame$aligned_projections),
    c("umap", "tsne"),
    ignore.order = TRUE
  )
  # A repeat must use the dataset-owned aligned copy, without another hydration.
  repeat_projection <- scope$cv_build_projections(
    list(getMetaData = function() stop("repeated metadata read")),
    seq_len(3),
    preloaded = frame$projections,
    aligned_cache = frame$aligned_projections
  )
  expect_equal(as.numeric(repeat_projection$umap$x), c(10, 20, 30))
  invalid <- frame$projections
  rownames(invalid$umap) <- c("missing", "1", "3")
  expect_error(
    scope$cv_build_projections(
      loaded,
      seq_len(3),
      only = "umap",
      preloaded = invalid
    ),
    "unique, matching cell identities"
  )
})

test_that("primary bundle can keep canonical projection coordinates external", {
  skip_if_not(have_bundle)
  metadata <- data.frame(
    cell_type = factor(c("B", "T", "B", "T"))
  )
  crb <- list(
    getMetaData = function() stop("metadata hydration was forced"),
    getGroups = function() "cell_type",
    getParameters = function() list(main_group = "cell_type"),
    availableProjections = function() "umap",
    getProjection = function(name) stop("projection coordinates were copied"),
    availableSpatial = function() NULL,
    getTrekker = function() NULL,
    getImmuneRepertoire = function() NULL,
    getGeneNames = function() character()
  )
  descriptor <- list(
    url = "projection/001.bin",
    cells = 4L,
    dimensions = 2L,
    bytes = 32,
    checksum = "0123456789abcdef0123456789abcdef"
  )

  primary <- cv_env$cv_build_bundle(
    crb,
    primary_only = TRUE,
    first_frame = list(
      meta_data = metadata,
      projections = list(umap = stop)
    ),
    primary_projection_resource = list(
      name = "umap",
      descriptor = descriptor
    )
  )

  expect_named(primary$projections, "umap")
  expect_identical(primary$projections$umap$projection_resource, descriptor)
  expect_identical(primary$projections$umap$ndim, 2L)
  expect_null(primary$projections$umap$x)
  expect_null(primary$projections$umap$y)
})

test_that("primary bundle can build canonical group codes externally", {
  crb <- list(
    getGroups = function() "cell_type",
    getParameters = function() list(main_group = "cell_type")
  )
  metadata <- data.frame(
    cell_barcode = c("one", "two", "three"),
    cell_type = c("B", "A", "B"),
    stringsAsFactors = FALSE
  )
  descriptor <- list(
    url = "metadata/004.codes.bin",
    cells = 3L,
    dtype = "uint8",
    bytes = 3,
    checksum = "abc",
    levels = I(c("A", "B")),
    code_map = I(c(-1L, 1L, 0L))
  )
  resource <- cv_env$cv_primary_group_resource(
    crb,
    metadata,
    function(name, factor_levels) {
      expect_identical(name, "cell_type")
      expect_null(factor_levels)
      descriptor
    }
  )
  colours <- cv_env$cv_build_primary_colours(
    crb,
    metadata,
    "cell_type",
    function(name, levels) c("red", "blue"),
    resource
  )

  expect_null(colours$groups$cell_type[["values"]])
  expect_identical(
    colours$groups$cell_type$values_resource$url,
    descriptor$url
  )
  expect_identical(colours$groups$cell_type$levels, I(c("A", "B")))
  expect_identical(colours$groups$cell_type$colors, I(c("red", "blue")))
  expect_identical(colours$default_group, "cell_type")

  fallback <- cv_env$cv_build_primary_colours(
    crb,
    metadata,
    "cell_type",
    function(name, levels) c("red", "blue")
  )
  expect_identical(as.integer(fallback$groups$cell_type$values), c(1L, 0L, 1L))
})

test_that("saved-view startup identity does not materialize cell names", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      sys.source(viewer_test_path("utility_functions.R"), scope)
      data_set <- reactive(list(cell_fingerprint = input$fingerprint))
      getNumberOfCells <- function() 1000000L
      cv_saved_view_cells <- function() stop("must not read cell names")
      viewer_server_load(
        viewer_server_statements("shiny_server.R", server = TRUE),
        scope,
        names = "viewerDatasetIdentity"
      )
    },
    {
      for (letter in c("a", "b")) {
        fingerprint <- paste0("md5-cell-set-v1:", strrep(letter, 32))
        session$setInputs(fingerprint = fingerprint)
        expect_identical(viewerDatasetIdentity()$fingerprint, fingerprint)
        expect_equal(viewerDatasetIdentity()$cell_count, 1000000L)
      }
    }
  )
})

test_that("primary colour selection does not scan unused categorical columns", {
  skip_if_not(have_bundle)
  old <- get0("as.character.unscannable", envir = .GlobalEnv, inherits = FALSE)
  assign(
    "as.character.unscannable",
    function(x, ...) stop("unused categorical column was scanned"),
    envir = .GlobalEnv
  )
  on.exit(
    {
      if (is.null(old)) {
        rm("as.character.unscannable", envir = .GlobalEnv)
      } else {
        assign("as.character.unscannable", old, envir = .GlobalEnv)
      }
    },
    add = TRUE
  )

  metadata <- data.frame(
    cell_type = factor(c("B", "T", "B")),
    unused = structure(c("x", "y", "z"), class = "unscannable")
  )
  colours <- cv_env$cv_build_primary_colours(
    list(
      getGroups = function() c("cell_type", "unused"),
      getParameters = function() list(main_group = "cell_type")
    ),
    metadata,
    c("cell_type", "unused"),
    function(name, levels) rep("#000000", length(levels))
  )

  expect_identical(colours$default_group, "cell_type")
  expect_named(colours$groups, "cell_type")
})

test_that("deferred metadata builds only the requested attribute", {
  skip_if_not(have_bundle)
  metadata <- data.frame(
    cell_type = factor(c("B", "T", "B")),
    donor = c("d1", "d1", "d2"),
    score = c(1, 2, 4),
    ignored = c(9, 8, 7),
    stringsAsFactors = FALSE
  )
  crb <- list(getGroups = function() "cell_type")
  colors <- function(name, levels) rep("#000000", length(levels))

  group <- cv_env$cv_build_attribute(
    crb,
    metadata,
    "groups",
    "cell_type",
    colors
  )
  extra <- cv_env$cv_build_attribute(
    crb,
    metadata,
    "cat_extra",
    "donor",
    colors
  )
  field <- cv_env$cv_build_attribute(
    crb,
    metadata,
    "fields",
    "meta:score",
    colors
  )

  expect_identical(as.integer(group$values), c(0L, 1L, 0L))
  expect_identical(as.integer(extra$values), c(0L, 0L, 1L))
  expect_identical(as.integer(field$v), c(0L, 333L, 1000L))
  expect_null(cv_env$cv_build_attribute(
    crb,
    metadata,
    "fields",
    "meta:ignored_missing",
    colors
  ))
})

test_that("deferred assets leave only descriptors in the supplement", {
  skip_if_not(have_bundle)
  value <- list(
    dataset_id = "xenium",
    dataset_fingerprint = "md5-cell-set-v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    spaces = list(list(
      id = "spatial",
      samples = list(
        list(
          name = "fov",
          images = list(list(
            id = "dapi",
            label = "DAPI",
            uri = "data:image/png;base64,AAAA"
          ))
        ),
        list(
          name = "fov_colour",
          images = list(list(
            id = "pink",
            label = "Pink",
            uri = "data:image/png;base64,BBBB"
          ))
        )
      )
    )),
    trekker = list(
      evidence = I(c(1L, 0L, 1L)),
      evidence_img = I(list(
        "data:image/png;base64,CCCC",
        NULL,
        "data:image/png;base64,DDDD"
      ))
    )
  )

  deferred <- cv_env$cv_defer_assets(value)
  samples <- deferred$value$spaces[[1L]]$samples

  expect_length(deferred$assets, 4L)
  expect_null(samples[[1L]]$images[[1L]]$uri)
  expect_true(isTRUE(samples[[1L]]$images[[1L]]$deferred))
  expect_true(nzchar(samples[[1L]]$images[[1L]]$asset_key))
  expect_null(samples[[2L]]$images[[1L]]$uri)
  expect_null(deferred$value$trekker$evidence_img)
  expect_true(isTRUE(deferred$value$trekker$evidence_deferred))
  expect_identical(
    sort(names(deferred$assets)),
    sort(c(
      samples[[1L]]$images[[1L]]$asset_key,
      samples[[2L]]$images[[1L]]$asset_key,
      "trekker-evidence:0",
      "trekker-evidence:2"
    ))
  )
})

test_that("external image files stay server-side until requested", {
  skip_if_not(have_bundle)
  skip_if_not_installed("base64enc")
  image_path <- tempfile(fileext = ".png")
  on.exit(unlink(image_path), add = TRUE)
  writeBin(
    as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)),
    image_path
  )
  value <- list(
    spaces = list(list(
      id = "spatial",
      images = list(list(
        id = "external",
        label = "External",
        asset_path = image_path,
        asset_mime = "image/png"
      ))
    ))
  )

  deferred <- cv_env$cv_defer_assets(value)
  image <- deferred$value$spaces[[1L]]$images[[1L]]
  asset <- deferred$assets[[image$asset_key]]

  expect_null(image$uri)
  expect_null(image$asset_path)
  expect_null(image$asset_mime)
  expect_true(isTRUE(image$deferred))
  expect_identical(asset, list(path = image_path, mime = "image/png"))
  expect_match(cv_env$cv_asset_data_uri(asset), "^data:image/png;base64,")
})

test_that("bundle cell identity falls back to metadata row names", {
  skip_if_not(have_bundle)
  cells <- c("c1", "c2")
  md <- data.frame(
    cluster = c("A", "B"),
    row.names = cells,
    stringsAsFactors = FALSE
  )
  crb <- list(
    getMetaData = function() md,
    getGroups = function() "cluster",
    getParameters = function() list(main_group = "cluster"),
    availableProjections = function() "umap",
    getProjection = function(name) {
      matrix(1:4, nrow = 2, dimnames = list(cells, c("x", "y")))
    },
    availableSpatial = function() NULL,
    getTrekker = function() NULL,
    getImmuneRepertoire = function() NULL,
    getGeneNames = function() character()
  )

  bundle <- cv_env$cv_build_bundle(crb)

  expect_identical(as.character(bundle$cells), cells)
})

test_that("canonical zero-based indices resolve only requested cell IDs", {
  skip_if_not(have_bundle)
  cells <- c("c1", "c2", "c3", "c4")

  expect_identical(
    cv_env$cv_cells_at_indices(cells, c(3L, 0L, 3L, 99L, -1L, NA)),
    c("c4", "c1", "c4")
  )
  expect_null(cv_env$cv_cells_at_indices(cells, NULL))
})

test_that("rownames-only metadata supports selection and cell details", {
  skip_if_not(have_bundle)
  metadata <- data.frame(
    cluster = c("A", "B", "A"),
    score = c(1, 2, 3),
    row.names = c("c1", "c2", "c3"),
    stringsAsFactors = FALSE
  )

  canonical <- cv_env$cv_canonical_metadata(metadata)
  selected <- cv_env$cv_selected_metadata(metadata, c("c3", "c1"))
  detail <- cv_env$cv_cell_metadata(metadata, "c2")

  expect_identical(canonical$cell_barcode, c("c1", "c2", "c3"))
  expect_identical(selected$cell_barcode, c("c1", "c3"))
  expect_identical(detail$cell_barcode, "c2")
  expect_identical(detail$score, 2)
})

test_that("explicit cell barcodes remain authoritative", {
  skip_if_not(have_bundle)
  metadata <- data.frame(
    cell_barcode = c("explicit-2", "explicit-1"),
    row.names = c("row-1", "row-2"),
    stringsAsFactors = FALSE
  )

  canonical <- cv_env$cv_canonical_metadata(metadata)

  expect_identical(canonical$cell_barcode, metadata$cell_barcode)
  metadata$cell_barcode <- c("duplicate", "duplicate")
  expect_error(
    cv_env$cv_canonical_metadata(metadata),
    "duplicate cell IDs"
  )
})

test_that("cv_color_patch carries only palette updates for the current bundle", {
  skip_if_not(have_bundle)
  bundle <- list(
    dataset_id = "example.crb",
    groups = list(
      cluster = cv_env$cv_group(c(0L, 1L), c("A", "B"), c("#aaaaaa", "#bbbbbb"))
    ),
    cat_extra = list(
      donor = cv_env$cv_group(c(0L, 1L), c("d1", "d2"), c("#cccccc", "#dddddd"))
    )
  )

  patch <- cv_env$cv_color_patch(
    bundle,
    list(cluster = c(A = "#111111", B = "#222222"))
  )

  expect_named(patch, c("dataset_id", "groups", "cat_extra"))
  expect_identical(patch$dataset_id, "example.crb")
  expect_equal(as.character(patch$groups$cluster), c("#111111", "#222222"))
  expect_equal(as.character(patch$cat_extra$donor), c("#cccccc", "#dddddd"))
  expect_s3_class(patch$groups$cluster, "AsIs")
})

test_that("palette patches can refresh an existing bundle", {
  skip_if_not(have_bundle)
  bundle <- list(
    groups = list(
      cluster = cv_env$cv_group(0L, "A", "#aaaaaa")
    ),
    cat_extra = list()
  )
  patch <- list(
    groups = list(cluster = I("#123456")),
    cat_extra = list()
  )

  refreshed <- cv_env$cv_apply_color_patch(bundle, patch)

  expect_identical(as.character(refreshed$groups$cluster$colors), "#123456")
})

## A minimal but realistically-shaped IR table: the CT* columns spell out chain
## names, because that is what the receptor scoping reads. c1/c2 share a CTgene
## clone that CTstrict splits in two -- the exact case where reading the wrong
## column changes the answer rather than just the formatting.
cv_ir_fixture <- function() {
  list(data.frame(
    barcode = c("c1", "c2", "c3", "b1"),
    CTgene = c(
      "TRAV1-2.TRAJ33.TRAC_TRBV6-4.TRBJ2-1.TRBC2",
      "TRAV1-2.TRAJ33.TRAC_TRBV6-4.TRBJ2-1.TRBC2",
      "TRAV12-1.TRAJ20.TRAC_TRBV20-1.TRBJ1-2.TRBC1",
      "IGHV3-23.IGHJ4.IGHM_IGKV1-5.IGKJ1.IGKC"
    ),
    CTstrict = c(
      "TRAV1-2.TRAJ33.TRAC_CAVMDSNYQLIW_TRBV6-4.TRBJ2-1.TRBC2_CASSAAA",
      "TRAV1-2.TRAJ33.TRAC_CAVMDSNYQLIW_TRBV6-4.TRBJ2-1.TRBC2_CASSBBB",
      "TRAV12-1.TRAJ20.TRAC_CAVXX_TRBV20-1.TRBJ1-2.TRBC1_CASSCCC",
      "IGHV3-23.IGHJ4.IGHM_CARDXX_IGKV1-5.IGKJ1.IGKC_CQQYX"
    ),
    CTaa = c("CAVM_CASSAAA", "CAVM_CASSBBB", "CAVX_CASSCCC", "CARD_CQQY"),
    stringsAsFactors = FALSE
  ))
}

test_that("cv_clone_per_cell aligns clone identity to cells, NA when unmatched", {
  skip_if_not(have_bundle)
  ir <- cv_ir_fixture()
  cells <- c("c3", "c1", "cX") # cX carries no receptor
  out <- cv_env$cv_clone_per_cell(ir, cells)
  expect_equal(
    out$clone,
    c(
      "TRAV12-1.TRAJ20.TRAC_TRBV20-1.TRBJ1-2.TRBC1",
      "TRAV1-2.TRAJ33.TRAC_TRBV6-4.TRBJ2-1.TRBC2",
      NA
    )
  )
  expect_equal(out$ctaa, c("CAVX_CASSCCC", "CAVM_CASSAAA", NA))
  ## No IR at all -> NULL (the immune axis is simply skipped).
  expect_null(cv_env$cv_clone_per_cell(NULL, cells))
})

test_that("receptor detection falls back from empty CTstrict values", {
  skip_if_not(have_bundle)
  ir <- list(data.frame(
    CTgene = c(
      "TRAV1-2.TRAJ33.TRAC_TRBV6-4.TRBJ2-1.TRBC2",
      "IGHV3-23.IGHJ4.IGHM_IGKV1-5.IGKJ1.IGKC"
    ),
    CTstrict = c(NA_character_, ""),
    stringsAsFactors = FALSE
  ))

  expect_setequal(cv_env$cerebro_receptors_present(ir), c("TCR", "BCR"))
  expect_identical(
    cv_env$cerebro_rows_in_receptor(ir[[1]], "TCR", "CTgene"),
    c(TRUE, FALSE)
  )
})

test_that("cv_clone_per_cell calls clones the way the Clonal UMAP does", {
  skip_if_not(have_bundle)
  ir <- cv_ir_fixture()
  cells <- c("c1", "c2", "c3")
  out <- cv_env$cv_clone_per_cell(ir, cells)
  ## c1 and c2 are ONE clone by CTgene and TWO by CTstrict. Reading the stricter
  ## column here reported more, smaller clones than the Clonal UMAP did for the
  ## same cells -- a different scientific answer under the same word.
  expect_equal(out$clone[1], out$clone[2])
  expect_equal(length(unique(out$clone)), 2)
})

test_that("cv_clone_per_cell keeps to one receptor class", {
  skip_if_not(have_bundle)
  ir <- cv_ir_fixture()
  ## b1 is a B cell. Mixing it in would put BCR and TCR clonotypes in one ranking
  ## and one expansion legend, which no page in the app does.
  out <- cv_env$cv_clone_per_cell(ir, c("c1", "b1"))
  expect_equal(out$receptor, "TCR")
  expect_false(is.na(out$clone[1]))
  expect_true(is.na(out$clone[2]))
  ## ... and asking for the other class gives its cells instead.
  bcr <- cv_env$cv_clone_per_cell(ir, c("c1", "b1"), receptor = "BCR")
  expect_true(is.na(bcr$clone[1]))
  expect_false(is.na(bcr$clone[2]))
})

test_that("both clone pages read one definition of a clone", {
  skip_if_not(have_bundle)
  ## The contract itself.
  expect_equal(cv_env$CEREBRO_CLONE_BINS, c(0, 1, 5, 20, 100, Inf))
  expect_equal(length(cv_env$CEREBRO_CLONE_LABELS), 5)
  expect_match(cv_env$CEREBRO_CLONE_LABELS[5], "Hyperexpanded")
  expect_equal(cv_env$cerebro_clonecall_col(), "CTgene")
})

test_that("cv_build_fields turns every numeric meta column into a colouring", {
  skip_if_not(have_bundle)
  md <- data.frame(
    cell_barcode = c("c1", "c2", "c3"),
    cluster = c("a", "b", "a"),
    nUMI = c(100, 200, 300),
    percent.mt = c(1.5, NA, 3.5),
    constant = c(7, 7, 7),
    stringsAsFactors = FALSE
  )
  f <- cv_env$cv_build_fields(md)
  ## QC-looking columns are exactly what users colour by — they must be offered.
  expect_true("meta:nUMI" %in% names(f))
  expect_true("meta:percent.mt" %in% names(f))
  ## no colouring can be built from a constant column or a non-numeric one
  expect_false("meta:constant" %in% names(f))
  expect_false("meta:cluster" %in% names(f))
  expect_false("meta:cell_barcode" %in% names(f))
  ## the quantised vector is aligned to the rows, spans the full scale, and
  ## carries the true range so the client can show real values, not 0-255.
  fu <- f[["meta:nUMI"]]
  expect_length(fu$v, nrow(md))
  expect_equal(as.integer(fu$v), c(0L, fu$scale %/% 2L, fu$scale))
  expect_equal(fu$min, 100)
  expect_equal(fu$max, 300)
  ## NA stays NA so the client can draw it as "no value" rather than as zero.
  expect_true(is.na(f[["meta:percent.mt"]]$v[2]))
})

test_that("cv_build_extra_groups covers categorical columns that are not groups", {
  skip_if_not(have_bundle)
  md <- data.frame(
    cell_barcode = c("c1", "c2", "c3"),
    cluster = c("a", "b", "a"),
    dextramer_allele = c("A*02:01", "A*02:01", "B*07:02"),
    stringsAsFactors = FALSE
  )
  eg <- cv_env$cv_build_extra_groups(md, "cluster", function(g, lev) {
    cv_env$cv_colors_for(lev)
  })
  ## registered groups stay out (they are already offered, and they own the
  ## group filters); unregistered categorical columns come in.
  expect_false("cluster" %in% names(eg$groups))
  expect_true("dextramer_allele" %in% names(eg$groups))
  expect_equal(
    as.character(eg$groups$dextramer_allele$levels),
    c("A*02:01", "B*07:02")
  )
  expect_equal(as.integer(eg$groups$dextramer_allele$values), c(0L, 0L, 1L))
  expect_length(eg$skipped, 0)

  ## A column with as many levels as cells is an identifier, not a grouping —
  ## not colourable, but REPORTED rather than dropped, so the picker can say why
  ## a column the Projection tab offers is missing here.
  md$barcode_copy <- md$cell_barcode
  eg2 <- cv_env$cv_build_extra_groups(md, "cluster", function(g, lev) {
    cv_env$cv_colors_for(lev)
  })
  expect_false("barcode_copy" %in% names(eg2$groups))
  expect_true("barcode_copy" %in% names(eg2$skipped))
  expect_equal(eg2$skipped$barcode_copy, 3L)
})

test_that("cv_build_projections records dimensionality instead of dropping it", {
  skip_if_not(have_bundle)
  cells <- c("c1", "c2")
  crb <- list(
    availableProjections = function() c("umap", "umap_3D"),
    getProjection = function(n) {
      m <- if (n == "umap_3D") {
        matrix(1:6, nrow = 2, dimnames = list(cells, c("x", "y", "z")))
      } else {
        matrix(1:4, nrow = 2, dimnames = list(cells, c("x", "y")))
      }
      m
    }
  )
  pj <- cv_env$cv_build_projections(crb, cells)
  expect_equal(pj$umap$ndim, 2L)
  expect_equal(pj$umap_3D$ndim, 3L)
  ## The third dimension travels so the client can rotate the embedding rather
  ## than show a flattened shadow of it. A 2-D projection must NOT carry a z —
  ## the client uses its presence to decide which panels can be turned.
  expect_null(pj$umap$z)
  expect_length(pj$umap_3D$z, length(cells))
  expect_equal(as.numeric(pj$umap_3D$z), c(5, 6))
  ## and it is array-wrapped, like every other per-cell vector in the bundle
  expect_s3_class(pj$umap_3D$z, "AsIs")
})

test_that("cv_build_bundle safely handles no registered grouping variable", {
  skip_if_not(have_bundle)
  cells <- c("c1", "c2", "c3")
  md <- data.frame(
    cell_barcode = cells,
    nUMI = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  crb <- list(
    getMetaData = function() md,
    getGroups = function() character(0),
    availableProjections = function() "umap",
    getProjection = function(n) {
      matrix(1:6, nrow = 3, dimnames = list(cells, c("x", "y")))
    },
    availableSpatial = function() NULL,
    getTrekker = function() NULL,
    getImmuneRepertoire = function() NULL
  )
  b <- cv_env$cv_build_bundle(crb)
  expect_type(b, "list")
  expect_equal(b$n, 3L)
  expect_length(b$groups, 0L)
  expect_length(b$cat_extra, 0L)
  expect_length(b$fields, 0L)
  expect_null(b$default_group)
})

test_that("Linked views consumes the selected dataset point appearance", {
  skip_if_not(have_bundle)
  cells <- c("c1", "c2", "c3")
  md <- data.frame(
    cell_barcode = cells,
    cell_type = c("A", "B", "A"),
    region = c("R1", "R1", "R2"),
    row.names = cells,
    stringsAsFactors = FALSE
  )
  crb <- list(
    getMetaData = function() md,
    getGroups = function() c("cell_type", "region"),
    getParameters = function() list(main_group = "region"),
    availableProjections = function() c("umap", "tsne"),
    getProjection = function(name) {
      offset <- if (identical(name, "tsne")) 10 else 0
      matrix(
        seq_len(6) + offset,
        nrow = 3,
        dimnames = list(cells, c("x", "y"))
      )
    },
    availableSpatial = function() character(),
    getTrekker = function() NULL,
    getImmuneRepertoire = function() NULL
  )
  cv_env$Cerebro.options <- list(
    point_size = c(ds = 5),
    point_opacity = c(ds = 0.7),
    percentage_cells_to_show = c(ds = 60),
    viewer_content = list(
      ds = list(
        default_projection = "tsne",
        initial_projections = c("tsne", "umap"),
        default_trajectory = NULL
      )
    )
  )
  cv_env$available_crb_files <- list(
    selected = "f.crb",
    files = c(ds = "f.crb")
  )
  on.exit(
    {
      rm("Cerebro.options", envir = cv_env)
      rm("available_crb_files", envir = cv_env)
    },
    add = TRUE
  )

  bundle <- cv_env$cv_build_bundle(crb)
  expect_identical(bundle$default_projection, "tsne")
  expression_space <- bundle$spaces[[which(
    vapply(bundle$spaces, `[[`, character(1), "id") == "expression"
  )]]
  expect_identical(expression_space$label, "tsne (expression)")
  expect_identical(bundle$default_group, "region")
  expect_identical(bundle$default_point_size, 5)
  expect_identical(bundle$default_point_opacity, 0.7)
  expect_identical(bundle$default_percentage_cells_to_show, 60)
})

test_that("spatial and Trekker spaces do not require an expression projection", {
  skip_if_not(have_bundle)
  cells <- c("c1", "c2")
  md <- data.frame(cell_barcode = cells, row.names = cells)
  base <- list(
    getMetaData = function() md,
    getGroups = function() character(),
    availableProjections = function() character(),
    getProjection = function(name) NULL,
    getImmuneRepertoire = function() NULL
  )

  spatial_crb <- base
  spatial_crb$availableSpatial <- function() "slice-a"
  spatial_crb$getSpatialData <- function(name) {
    list(
      coordinates = data.frame(
        x = c(1, 2),
        y = c(3, 4),
        row.names = cells
      )
    )
  }
  spatial_crb$getTrekker <- function() NULL

  spatial_bundle <- cv_env$cv_build_bundle(spatial_crb)
  expect_type(spatial_bundle, "list")
  expect_length(spatial_bundle$projections, 0L)
  expect_null(spatial_bundle$default_projection)
  expect_identical(
    vapply(spatial_bundle$spaces, `[[`, character(1), "id"),
    "spatial"
  )

  trekker_crb <- base
  trekker_crb$availableSpatial <- function() character()
  trekker_crb$getSpatialData <- function(name) NULL
  trekker_crb$getTrekker <- function() {
    list(
      x = c(10, 20),
      y = c(30, 40),
      barcodes = cells,
      fields = list(),
      evidence = list(),
      qc = NULL,
      moran = NULL
    )
  }

  trekker_bundle <- cv_env$cv_build_bundle(trekker_crb)
  expect_type(trekker_bundle, "list")
  expect_length(trekker_bundle$projections, 0L)
  expect_null(trekker_bundle$default_projection)
  expect_identical(
    vapply(trekker_bundle$spaces, `[[`, character(1), "id"),
    "trekker"
  )
})

test_that("Builder Trekker backgrounds and appearance reach Linked views", {
  skip_if_not(have_bundle)
  cells <- c("c1", "c2")
  md <- data.frame(cell_barcode = cells, row.names = cells)
  crb <- list(
    getMetaData = function() md,
    getGroups = function() character(),
    getParameters = function() list(),
    availableProjections = function() character(),
    availableSpatial = function() character(),
    getImmuneRepertoire = function() NULL,
    getTrekker = function() {
      list(
        x = c(10, 20),
        y = c(30, 40),
        barcodes = cells,
        fields = list(),
        evidence = list(),
        qc = NULL,
        moran = NULL,
        histology_image = "data:image/png;base64,AA==",
        histology_image_bounds = c(
          xmin = 0,
          xmax = 30,
          ymin = 20,
          ymax = 50
        ),
        histology_alignment = list(
          source = "trekker.png",
          image_opacity = 0.7,
          point_opacity = 0.65,
          point_size = 9
        )
      )
    }
  )

  cv_env$Cerebro.options <- list(
    point_size = c(ds = 5),
    point_opacity = c(ds = 0.65),
    viewer_content = list(
      ds = list()
    )
  )
  cv_env$available_crb_files <- list(
    selected = "f.crb",
    files = c(ds = "f.crb")
  )
  on.exit(
    {
      rm("Cerebro.options", envir = cv_env)
      rm("available_crb_files", envir = cv_env)
    },
    add = TRUE
  )

  bundle <- cv_env$cv_build_bundle(crb)
  trekker <- bundle$spaces[[which(
    vapply(
      bundle$spaces,
      `[[`,
      character(1),
      "id"
    ) ==
      "trekker"
  )]]
  expect_length(trekker$images, 1L)
  expect_identical(trekker$images[[1L]]$label, "trekker.png")
  expect_identical(
    trekker$images[[1L]]$uri,
    "data:image/png;base64,AA=="
  )
  expect_identical(trekker$images[[1L]]$preset$opacity, 0.7)
  expect_identical(trekker$background_scope, "Trekker")
  ## Cell appearance belongs to the dataset, not its background image.
  expect_identical(bundle$default_point_size, 5)
  expect_identical(bundle$default_point_opacity, 0.65)
  expect_null(trekker$builder_point_opacity)
  expect_null(trekker$builder_point_size)
})

test_that("a clone's label names its dominant CDR3 and says how many it hides", {
  skip_if_not(have_bundle)
  ## A clone is called on CTgene, and one CTgene clone routinely covers several
  ## CDR3s in realistic receptor data can do this. Naming the row after
  ## whichever CDR3 came first presented one sequence while the row selected
  ## cells carrying the others, under a column header that read "CDR3".
  ir <- list(data.frame(
    barcode = c("c1", "c2", "c3", "c4"),
    CTgene = rep("TRAV1-2.TRAJ33.TRAC_TRBV6-4.TRBJ2-1.TRBC2", 4),
    CTstrict = paste0(
      "TRAV1-2.TRAJ33.TRAC_CAV",
      c("A", "B", "B", "B"),
      "_TRBV6-4.TRBJ2-1.TRBC2"
    ),
    ## "CASSB" is carried by three of the four cells; "CASSA" by one, and it
    ## comes first, which is exactly what the old label picked.
    CTaa = c("CASSA", "CASSB", "CASSB", "CASSB"),
    stringsAsFactors = FALSE
  ))
  crb <- list(getImmuneRepertoire = function() ir)
  cells <- c("c1", "c2", "c3", "c4")
  out <- cv_env$cv_build_clone(crb, cells, length(cells))

  expect_false(is.null(out))
  expect_null(out$space$x)
  expect_null(out$space$y)
  expect_equal(out$bundle$n_clones, 1)
  expect_equal(as.character(out$bundle$label[1]), "CASSB")
  expect_equal(as.integer(out$bundle$n_cdr3[1]), 2L)
})

test_that("a single-CDR3 clone is not annotated", {
  skip_if_not(have_bundle)
  ir <- list(data.frame(
    barcode = c("c1", "c2"),
    CTgene = rep("TRAV1-2.TRAJ33.TRAC_TRBV6-4.TRBJ2-1.TRBC2", 2),
    CTstrict = rep("TRAV1-2.TRAJ33.TRAC_CAVX_TRBV6-4.TRBJ2-1.TRBC2", 2),
    CTaa = c("CASSX", "CASSX"),
    stringsAsFactors = FALSE
  ))
  crb <- list(getImmuneRepertoire = function() ir)
  out <- cv_env$cv_build_clone(crb, c("c1", "c2"), 2)
  expect_equal(as.character(out$bundle$label[1]), "CASSX")
  expect_equal(as.integer(out$bundle$n_cdr3[1]), 1L)
})

test_that("sparse clone transport expands to the legacy cell-aligned values", {
  skip_if_not(have_bundle)
  ir <- list(data.frame(
    barcode = c("c2", "c4", "c5"),
    CTgene = c(
      "TRAV1_TRAJ1_TRBV1_TRBJ1",
      "TRAV1_TRAJ1_TRBV1_TRBJ1",
      "TRAV2_TRAJ2_TRBV2_TRBJ2"
    ),
    CTaa = c("CASSA", "CASSA", "CASSB"),
    stringsAsFactors = FALSE
  ))
  crb <- list(getImmuneRepertoire = function() ir)
  cells <- paste0("c", 1:6)

  dense <- cv_env$cv_build_clone(crb, cells, length(cells))
  sparse <- cv_env$cv_build_clone(crb, cells, length(cells), sparse = TRUE)
  clone_ids <- rep(-1L, length(cells))
  clone_ids[as.integer(sparse$bundle$index) + 1L] <-
    as.integer(sparse$bundle$code)
  expansion <- rep(0L, length(cells))
  expansion[as.integer(sparse$bundle$index) + 1L] <-
    as.integer(sparse$group$values)

  expect_null(sparse$bundle$id)
  expect_null(sparse$group$index)
  expect_true(isTRUE(sparse$group$clone_index))
  expect_identical(clone_ids, as.integer(dense$bundle$id))
  expect_identical(expansion, as.integer(dense$group$values))
  expect_identical(sparse$bundle$label, dense$bundle$label)
  expect_identical(sparse$bundle$size, dense$bundle$size)
  expect_identical(sparse$bundle$n_cdr3, dense$bundle$n_cdr3)
})

test_that("Viewer Pack builds sparse clones from canonical indices", {
  skip_if_not(have_bundle)
  old_reader <- get0("viewerPackImmuneIndex", envir = cv_env, inherits = FALSE)
  on.exit(
    {
      if (is.null(old_reader)) {
        rm("viewerPackImmuneIndex", envir = cv_env)
      } else {
        assign("viewerPackImmuneIndex", old_reader, envir = cv_env)
      }
    },
    add = TRUE
  )
  assign(
    "viewerPackImmuneIndex",
    function(pack, receptor) {
      list(
        cell_index = c(2L, 4L, 5L),
        clone = c("clone-a", "clone-a", "clone-b"),
        ctaa = c("CASSA", "CASSA", "CASSB"),
        receptor = "TCR"
      )
    },
    envir = cv_env
  )
  crb <- list(getImmuneRepertoire = function() stop("CRB fallback used"))
  attr(crb, "cerebro_viewer_pack") <- list(
    cells = paste0("c", 1:6),
    manifest = list(immune_receptors = "TCR")
  )

  sparse <- cv_env$cv_build_clone(crb, seq_len(6L), 6L, sparse = TRUE)

  expect_identical(as.integer(sparse$bundle$index), c(1L, 3L, 4L))
  expect_identical(as.integer(sparse$bundle$code), c(0L, 0L, 1L))
  expect_identical(as.integer(sparse$group$values), c(2L, 2L, 1L))
})

test_that("compact supplement avoids full CRB hydration", {
  skip_if_not(have_bundle)
  immune_index_calls <- 0L
  old_reader <- get0("viewerPackImmuneIndex", envir = cv_env, inherits = FALSE)
  on.exit(
    {
      if (is.null(old_reader)) {
        rm("viewerPackImmuneIndex", envir = cv_env)
      } else {
        assign("viewerPackImmuneIndex", old_reader, envir = cv_env)
      }
    },
    add = TRUE
  )
  assign(
    "viewerPackImmuneIndex",
    function(pack, receptor) {
      immune_index_calls <<- immune_index_calls + 1L
      list(
        cell_index = c(2L, 4L),
        clone = c("clone-a", "clone-a"),
        ctaa = c("CASSA", "CASSA"),
        receptor = "TCR"
      )
    },
    envir = cv_env
  )
  metadata <- data.frame(
    cell_type = factor(c("B", "T", "B", "T")),
    sample = factor(c("s1", "s1", "s2", "s2")),
    score = c(1, 2, 3, 4)
  )
  projection <- data.frame(x = 1:4, y = 4:1)
  projection_catalog_calls <- 0L
  crb <- list(
    getMetaData = function() stop("metadata hydration was forced"),
    getGeneNames = function() stop("gene hydration was forced"),
    getGroups = function() c("cell_type", "sample"),
    availableProjections = function() {
      projection_catalog_calls <<- projection_catalog_calls + 1L
      stop("projection hydration was forced")
    },
    getMethodsForTrajectories = function() character(),
    availableSpatial = function() character(),
    getTrekker = function() NULL,
    getImmuneRepertoire = function() stop("immune fallback was forced")
  )
  attr(crb, "cerebro_viewer_pack") <- list(
    cells = paste0("c", 1:4),
    manifest = list(
      n_cells = 4L,
      cell_order_fingerprint = "md5-cell-order-v1:order",
      immune_receptors = "TCR"
    )
  )
  primary <- list(
    dataset_id = "ren.crb",
    dataset_fingerprint = "md5-cell-set-v1:cells",
    canonical_order_id = "md5-cell-order-v1:order",
    progressive_token = 7L,
    n = 4L,
    groups = list(cell_type = list()),
    cat_extra = list(),
    fields = list(),
    projections = list(tsne = list()),
    spaces = list(list(id = "umap"))
  )

  supplement <- cv_env$cv_build_compact_supplement(
    crb,
    primary,
    list(meta_data = metadata, projections = list(tsne = projection))
  )

  expect_true(isTRUE(supplement$groups$sample$deferred))
  expect_null(supplement$groups$sample$values)
  expect_null(supplement$fields[["meta:score"]])
  expect_identical(as.integer(supplement$clone$index), c(1L, 3L))
  expect_named(supplement$groups, c("sample", "clone_expansion"))
  expect_identical(projection_catalog_calls, 0L)

  metadata_only <- cv_env$cv_build_compact_supplement(
    crb,
    primary,
    list(meta_data = metadata, projections = list(tsne = projection)),
    include_clone = FALSE
  )
  expect_identical(immune_index_calls, 1L)
  expect_null(metadata_only$clone)
  expect_false("clone_expansion" %in% names(metadata_only$groups))
  expect_length(metadata_only$spaces, 0L)
})

test_that("clone core omits duplicate indices and deferred labels", {
  clone <- cv_env$cv_build_clone_rows(
    clone = c("a", "a", "b"),
    ctaa = c("AA", "AA", "BB"),
    receptor = "TCR",
    receptor_index = c(1L, 3L, 5L),
    n = 6L,
    sparse = TRUE
  )

  expect_null(clone$group$index)
  expect_true(isTRUE(clone$group$clone_index))

  deferred <- cv_env$cv_defer_clone_details(clone)
  expect_length(deferred$value$bundle$label, 0L)
  expect_length(deferred$value$bundle$n_cdr3, 0L)
  expect_true(isTRUE(deferred$value$bundle$details_deferred))
  expect_identical(deferred$details$label, c("AA", "BB"))
  expect_identical(as.integer(deferred$details$n_cdr3), c(1L, 1L))
})

## The divergences that clone_contract.R exists to prevent were never visible to
## a test of either page on its own: each was self-consistent. This one computes
## a clone's size the way each page does, on the same object, and compares -- the
## only shape of test that can fail when the two drift apart again.
test_that("both pages give a clone the same size", {
  skip_if_not(have_bundle)
  skip_if_not(nzchar(tcr_crb) && file.exists(tcr_crb))

  crb <- readCerebro(tcr_crb)
  md <- crb$getMetaData()
  cells <- as.character(md$cell_barcode)
  ir <- crb$getImmuneRepertoire()

  ## Linked views: align to the object's cells, then count.
  cp <- cv_env$cv_clone_per_cell(ir, cells)
  cv_sizes <- table(cp$clone[!is.na(cp$clone)])

  ## Immune repertoire: the same contract, applied independently -- one row per
  ## barcode, receptor-scoped, restricted to cells the object has, then counted.
  clone_col <- cv_env$cerebro_clonecall_col()
  receptor <- cv_env$cerebro_receptors_present(ir)[1]
  rows <- do.call(
    rbind,
    lapply(ir, function(df) {
      keep <- cv_env$cerebro_rows_in_receptor(df, receptor, clone_col)
      df <- df[keep, , drop = FALSE]
      if (!nrow(df)) {
        return(NULL)
      }
      data.frame(
        barcode = as.character(df$barcode),
        clone = as.character(df[[clone_col]]),
        stringsAsFactors = FALSE
      )
    })
  )
  rows <- rows[!is.na(rows$clone) & nzchar(rows$clone), , drop = FALSE]
  rows <- rows[!duplicated(rows$barcode), , drop = FALSE]
  rows <- rows[rows$barcode %in% cells, , drop = FALSE]
  ir_sizes <- table(rows$clone)

  expect_equal(length(cv_sizes), length(ir_sizes))
  expect_setequal(names(cv_sizes), names(ir_sizes))
  expect_equal(
    as.integer(cv_sizes[names(ir_sizes)]),
    as.integer(ir_sizes)
  )

  ## ... and therefore the same expansion level for every clone.
  expect_equal(
    as.character(cv_env$cerebro_clone_expansion(as.integer(cv_sizes))),
    as.character(cv_env$cerebro_clone_expansion(
      as.integer(ir_sizes[names(cv_sizes)])
    ))
  )
})

test_that("receptor detection does not stop at the third sample", {
  skip_if_not(have_bundle)
  ## detect_chains() samples the first three list entries, which is fine for
  ## guessing a default gene family and wrong for deciding which receptor a page
  ## defaults to: a data set whose FOURTH sample is the only one carrying TCR
  ## would have shown BCR on one page and TCR on the other.
  bcr <- function(i) {
    data.frame(
      barcode = paste0("b", i),
      CTgene = "IGHV3-23.IGHJ4.IGHM_IGKV1-5.IGKJ1.IGKC",
      CTstrict = "IGHV3-23.IGHJ4.IGHM_CARDX_IGKV1-5.IGKJ1.IGKC_CQQY",
      CTaa = "CARD_CQQY",
      stringsAsFactors = FALSE
    )
  }
  ir <- list(
    bcr(1),
    bcr(2),
    bcr(3),
    data.frame(
      barcode = "t1",
      CTgene = "TRAV1-2.TRAJ33.TRAC_TRBV6-4.TRBJ2-1.TRBC2",
      CTstrict = "TRAV1-2.TRAJ33.TRAC_CAVX_TRBV6-4.TRBJ2-1.TRBC2_CASSX",
      CTaa = "CAVX_CASSX",
      stringsAsFactors = FALSE
    )
  )
  expect_setequal(cv_env$cerebro_receptors_present(ir), c("TCR", "BCR"))
})

## The card's Positioning section is only as good as what the builder actually
## ships. It used to be tested against a hand-written payload, which is how it
## came to read a `fields.bead_noise` that no builder has ever produced while
## printing position confidence twice -- once from `conf`, once from the field of
## the same name. These assertions are on the real output for the real object.
test_that("the trekker bundle carries what a placement is judged on", {
  skip_if_not(have_bundle)
  skip_if_not(file.exists(trekker_crb))

  b <- cv_env$cv_build_bundle(readCerebro(trekker_crb))
  expect_false(is.null(b$trekker))

  ## position_confidence is a FIELD -- a colouring -- so the card reads it from
  ## there. `conf` remains the bare vector the dissolve slider indexes.
  expect_true("position_confidence" %in% names(b$fields))
  expect_false(is.null(b$trekker$conf))
  purity <- b$fields$spatial_purity
  expect_identical(purity$source, "trekker")
  expect_true(nzchar(purity$desc))
  expect_true(length(purity$by_type) > 0)
  expect_true(all(vapply(
    purity$by_type,
    function(x) !is.null(x$type) && !is.null(x$median),
    logical(1)
  )))

  ## The two numbers the dedicated page prints beside confidence, which say
  ## whether that confidence is worth anything, now travel with it.
  expect_false(is.null(b$trekker$conf_noise))
  expect_false(is.null(b$trekker$conf_sb))
  expect_equal(length(b$trekker$conf_noise), b$n)
  expect_equal(length(b$trekker$conf_sb), b$n)

  ## Evidence is not just a ring: the detail card must be able to explain why a
  ## nucleus was placed there without sending the user back to the old page.
  expect_equal(length(b$trekker$evidence_img), b$n)
  evidence <- Filter(Negate(is.null), unclass(b$trekker$evidence_img))
  expect_true(length(evidence) > 0)
  expect_true(all(startsWith(unlist(evidence), "data:image/")))

  ## No field is invented: everything the card lists as Trekker's own is a key
  ## the builder produced.
  tk_fields <- Filter(function(k) !startsWith(k, "meta:"), names(b$fields))
  expect_true(length(tk_fields) > 0)
  expect_false("bead_noise" %in% tk_fields)
})

test_that("each section offers only its own configured backgrounds", {
  skip_if_not(have_bundle)
  ## Embedded and external used to be exclusive -- an object carrying its own
  ## histology silently dropped whatever the deployment had configured -- and
  ## only the FIRST configured file was read. Two files of the same basename
  ## also have to stay apart, so the id cannot be the basename alone.
  tmp <- file.path(tempdir(), "cv_imgs")
  dir.create(
    file.path(tmp, "spatial-assets", "a"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create(
    file.path(tmp, "spatial-assets", "b"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  png <- file.path(tmp, "spatial-assets", "a", "he.png")
  png2 <- file.path(tmp, "spatial-assets", "b", "he.png")
  ## A real 1x1 PNG written byte-wise: no graphics device needed, so this does
  ## not depend on one being available in the check environment.
  px <- as.raw(c(
    0x89,
    0x50,
    0x4e,
    0x47,
    0x0d,
    0x0a,
    0x1a,
    0x0a,
    0x00,
    0x00,
    0x00,
    0x0d,
    0x49,
    0x48,
    0x44,
    0x52,
    0x00,
    0x00,
    0x00,
    0x01,
    0x00,
    0x00,
    0x00,
    0x01,
    0x08,
    0x06,
    0x00,
    0x00,
    0x00,
    0x1f,
    0x15,
    0xc4,
    0x89,
    0x00,
    0x00,
    0x00,
    0x0d,
    0x49,
    0x44,
    0x41,
    0x54,
    0x78,
    0x9c,
    0x63,
    0xf8,
    0xcf,
    0xc0,
    0x50,
    0x0f,
    0x00,
    0x04,
    0x85,
    0x01,
    0x80,
    0x84,
    0xa9,
    0x8c,
    0x21,
    0x00,
    0x00,
    0x00,
    0x00,
    0x49,
    0x45,
    0x4e,
    0x44,
    0xae,
    0x42,
    0x60,
    0x82
  ))
  writeBin(px, png)
  writeBin(px, png2)
  skip_if_not(file.exists(png) && file.exists(png2))

  ## The builders read these two app-scope objects when they exist; this is the
  ## same shape the running app provides.
  cv_env$session <- new_cv_image_session()
  cv_env$Cerebro.options <- list(
    cerebro_root = tmp,
    spatial_images = list(
      ds = list(
        `section-a` = list(
          `H&E` = list(
            path = "spatial-assets/a/he.png",
            bounds = c(xmin = 1, xmax = 11, ymin = 2, ymax = 12),
            viewport_bounds = c(xmin = -5, xmax = 15, ymin = -2, ymax = 14)
          )
        ),
        `section-b` = c(`H&E` = "spatial-assets/b/he.png")
      )
    ),
    spatial_image_settings = list(
      ds = list(
        `section-a` = list(
          `H&E` = list(
            offset_x = 4,
            offset_y = -3,
            scale_x = 1.5,
            scale_y = 0.75,
            flip_x = TRUE,
            flip_y = FALSE,
            rotation = 90,
            image_opacity = 0.6,
            point_opacity = 0.55,
            point_size = 8
          )
        )
      )
    )
  )
  cv_env$available_crb_files <- list(
    selected = "f.crb",
    files = c(ds = "f.crb")
  )
  on.exit(
    {
      rm("session", envir = cv_env)
      rm("Cerebro.options", envir = cv_env)
      rm("available_crb_files", envir = cv_env)
    },
    add = TRUE
  )

  first <- cv_env$cv_external_images("section-a")
  second <- cv_env$cv_external_images("section-b")
  deferred <- cv_env$cv_external_images("section-a", defer = TRUE)
  expect_length(first, 1L)
  expect_length(second, 1L)
  expect_identical(first[[1L]]$label, "H&E")
  expect_identical(second[[1L]]$label, "H&E")
  expect_false(identical(first[[1L]]$id, second[[1L]]$id))
  expect_null(deferred[[1L]]$uri)
  expect_identical(
    deferred[[1L]]$asset_path,
    normalizePath(png, winslash = "/", mustWork = TRUE)
  )
  expect_identical(deferred[[1L]]$asset_mime, "image/png")
  expect_equal(
    unlist(first[[1L]]$bounds, use.names = TRUE),
    c(xmin = 1, xmax = 11, ymin = 2, ymax = 12)
  )
  expect_equal(
    unlist(first[[1L]]$viewport, use.names = TRUE),
    c(xmin = -5, xmax = 15, ymin = -2, ymax = 14)
  )
  expect_identical(
    first[[1L]]$preset,
    list(
      offsetX = 4,
      offsetY = -3,
      scaleX = 1.5,
      scaleY = 0.75,
      flipX = TRUE,
      flipY = FALSE,
      rotation = 90,
      opacity = 0.6
    )
  )
})

test_that("per-image settings also apply to embedded backgrounds", {
  skip_if_not(have_bundle)
  cells <- c("c1", "c2")
  crb <- list(getSpatialData = function(name) {
    list(
      coordinates = data.frame(
        x = c(1.123456789, 2.987654321),
        y = c(3.246813579, 4.135792468),
        row.names = cells
      ),
      histology_images = list(
        Embedded = list(
          histology_image = "data:image/png;base64,AA==",
          histology_image_bounds = c(
            xmin = 0,
            xmax = 3,
            ymin = 0,
            ymax = 5
          ),
          histology_alignment = list(
            source = "Embedded",
            dx = -4,
            dy = 6,
            scale = 1.25,
            rotation = -32,
            flip_x = TRUE,
            flip_y = FALSE,
            image_opacity = 0.7,
            point_opacity = 0.35,
            point_size = 9,
            viewport_bounds = list(
              xmin = -2,
              xmax = 4,
              ymin = -1,
              ymax = 6
            )
          )
        ),
        `ROI only` = list(
          histology_image = "data:image/png;base64,AA==",
          histology_image_bounds = c(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
          roi_field = "sample_roi",
          roi_value = "lesion"
        )
      ),
      histology_alignment = list(
        source = "Embedded",
        image_opacity = 0.7,
        point_opacity = 0.8,
        point_size = 5
      )
    )
  })
  cv_env$Cerebro.options <- list(
    viewer_content = list(
      ds = list(
        spatial_point_appearance = list(
          fov = list(point_opacity = 0.42, point_size = 8)
        )
      )
    ),
    spatial_image_settings = list(
      ds = list(
        fov = list(
          Embedded = list(offset_x = 2, flip_y = TRUE, rotation = 45)
        )
      )
    )
  )
  cv_env$available_crb_files <- list(
    selected = "f.crb",
    files = c(ds = "f.crb")
  )
  on.exit(
    {
      rm("Cerebro.options", envir = cv_env)
      rm("available_crb_files", envir = cv_env)
    },
    add = TRUE
  )

  built <- cv_env$cv_spatial_one(crb, cells, "fov", allow_external = TRUE)
  expect_length(built$images, 1L)
  expect_identical(built$x, c(1.123456789, 2.987654321))
  expect_identical(built$y, c(3.246813579, 4.135792468))
  expect_identical(built$x_range, c(-2, 4))
  expect_identical(built$y_range, c(-1, 6))
  expect_identical(built$builder_point_opacity, 0.42)
  expect_identical(built$builder_point_size, 8)
  expect_identical(
    built$images[[1L]]$preset,
    list(
      offsetX = -4,
      offsetY = 6,
      scaleX = 1.25,
      scaleY = 1.25,
      flipX = TRUE,
      flipY = FALSE,
      rotation = -32,
      opacity = 0.7,
      geometryBaked = TRUE
    )
  )
})

test_that("legacy spatial images share identity and point appearance", {
  skip_if_not(have_bundle)
  cells <- c("c1", "c2")
  crb <- list(getSpatialData = function(name) {
    list(
      coordinates = data.frame(
        x = c(1, 2),
        y = c(3, 4),
        row.names = cells
      ),
      histology_image = "data:image/png;base64,AA==",
      histology_image_bounds = list(xmin = 0, xmax = 3, ymin = 0, ymax = 5)
    )
  })
  cv_env$Cerebro.options <- list(
    spatial_plot_rotation = list(ds = c(fov = 90)),
    point_size = c(ds = 5),
    point_opacity = c(ds = 0.9),
    spatial_image_settings = list(
      ds = list(
        fov = list(
          `Tissue background` = list(offset_y = -10, flip_y = TRUE)
        )
      )
    )
  )
  cv_env$available_crb_files <- list(
    selected = "f.crb",
    files = c(ds = "f.crb"),
    names = "ds"
  )
  on.exit(
    {
      rm("Cerebro.options", envir = cv_env)
      rm("available_crb_files", envir = cv_env)
    },
    add = TRUE
  )

  built <- cv_env$cv_spatial_one(crb, cells, "fov", allow_external = TRUE)

  expect_identical(built$images[[1L]]$label, "Tissue background")
  expect_identical(built$images[[1L]]$preset$offsetY, -10)
  expect_true(built$images[[1L]]$preset$flipY)
  expect_identical(built$images[[1L]]$preset$rotation, 0)
  expect_null(built$point_size)
  expect_null(built$point_opacity)
  expect_identical(
    cv_env$viewerScatterDefaults(cv_env$Cerebro.options, "ds"),
    list(
      point_size = 5,
      point_opacity = 0.9,
      percentage_cells_to_show = 100
    )
  )
})

test_that("Linked views keeps the all-ROI coordinate layout unchanged", {
  skip_if_not(have_bundle)
  cells <- c("c1", "c2")
  crb <- list(getSpatialData = function(name) {
    list(
      coordinates = data.frame(
        x = c(0, 2),
        y = c(0, 0),
        row.names = cells
      )
    )
  })
  cv_env$Cerebro.options <- list(
    viewer_content = list(
      ds = list(
        spatial_roi_settings = list(
          fov = list(
            roi = list(
              rotation_degrees = 90,
              point_opacity = 0.8,
              point_size = 5
            )
          )
        )
      )
    )
  )
  cv_env$available_crb_files <- list(
    selected = "f.crb",
    files = c(ds = "f.crb")
  )
  on.exit(
    {
      rm("Cerebro.options", envir = cv_env)
      rm("available_crb_files", envir = cv_env)
    },
    add = TRUE
  )

  built <- cv_env$cv_spatial_one(
    crb,
    cells,
    "fov",
    allow_external = FALSE
  )

  expect_identical(built$x, c(0, 2))
  expect_identical(built$y, c(0, 0))
})

test_that("bundling two images of the same basename keeps both", {
  ## The ids kept them apart in the bundle, but createShinyApp() copied both to
  ## data/<basename> and let the second overwrite the first, so two entries in
  ## the picker resolved to one file. The earlier test called the builder on the
  ## original absolute paths and never went through the packaging that breaks it.
  tmp <- file.path(tempdir(), "cv_pack")
  unlink(tmp, recursive = TRUE)
  dir.create(file.path(tmp, "a"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(tmp, "b"), recursive = TRUE, showWarnings = FALSE)
  px <- as.raw(c(
    0x89,
    0x50,
    0x4e,
    0x47,
    0x0d,
    0x0a,
    0x1a,
    0x0a,
    0x00,
    0x00,
    0x00,
    0x0d,
    0x49,
    0x48,
    0x44,
    0x52,
    0x00,
    0x00,
    0x00,
    0x01,
    0x00,
    0x00,
    0x00,
    0x01,
    0x08,
    0x06,
    0x00,
    0x00,
    0x00,
    0x1f,
    0x15,
    0xc4,
    0x89,
    0x00,
    0x00,
    0x00,
    0x0d,
    0x49,
    0x44,
    0x41,
    0x54,
    0x78,
    0x9c,
    0x63,
    0xf8,
    0xcf,
    0xc0,
    0x50,
    0x0f,
    0x00,
    0x04,
    0x85,
    0x01,
    0x80,
    0x84,
    0xa9,
    0x8c,
    0x21,
    0x00,
    0x00,
    0x00,
    0x00,
    0x49,
    0x45,
    0x4e,
    0x44,
    0xae,
    0x42,
    0x60,
    0x82
  ))
  p1 <- file.path(tmp, "a", "he.png")
  p2 <- file.path(tmp, "b", "he.png")
  writeBin(px, p1)
  writeBin(px, p2)

  example <- system.file(
    "extdata/examples/demo_spatial_visium.crb",
    package = "CerebroNexus"
  )
  skip_if_not(nzchar(example))
  crb_one <- file.path(tmp, "demo-one.crb")
  crb_two <- file.path(tmp, "demo-two.crb")
  file.copy(example, crb_one, overwrite = TRUE)
  file.copy(example, crb_two, overwrite = TRUE)
  spatial_name <- readCerebro(crb_one)$availableSpatial()[[1L]]
  app_dir <- file.path(tmp, "app")
  createShinyApp(
    cerebro_data = c("ds one" = crb_one, "ds two" = crb_two),
    result_dir = app_dir,
    spatial_images = list(
      `ds one` = stats::setNames(list(c(`H&E` = p1)), spatial_name),
      `ds two` = stats::setNames(list(c(`H&E` = p2)), spatial_name)
    ),
    launch_browser = FALSE,
    verbose = FALSE
  )

  copied <- list.files(
    file.path(app_dir, "spatial-assets"),
    pattern = "[.]png$",
    recursive = TRUE
  )
  expect_equal(length(copied), 2)
  ## ... and the bundled configuration points at the two distinct files rather
  ## than twice at one. (The paths live in cerebro_config.rds, not app.R.)
  cfg <- readRDS(file.path(app_dir, "cerebro_config.rds"))
  paths <- unname(unlist(cfg$spatial_images))
  expect_equal(length(unique(paths)), 2)
  expect_true(all(file.exists(file.path(app_dir, paths))))
})

test_that("Linked labels remain reachable in Settings", {
  ui <- htmltools::tagQuery(viewer_ui_test_pages()$coordinated_views)
  expect_equal(ui$find(".cerebro-settings-drawer #cv-labels")$length(), 1L)
})

test_that("external backgrounds require matching PNG or JPEG magic bytes", {
  skip_if_not(have_bundle)
  tmp <- file.path(tempdir(), "cv_external_magic")
  unlink(tmp, recursive = TRUE)
  assets <- file.path(tmp, "spatial-assets")
  dir.create(assets, recursive = TRUE)
  png_magic <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
  jpeg_magic <- as.raw(c(0xff, 0xd8, 0xff, 0xe0))
  writeBin(png_magic, file.path(assets, "valid.png"))
  writeBin(jpeg_magic, file.path(assets, "valid.jpg"))
  writeLines("not an image", file.path(assets, "text.png"))
  writeBin(png_magic, file.path(assets, "mismatch.jpg"))
  cv_env$session <- new_cv_image_session()
  cv_env$Cerebro.options <- list(
    cerebro_root = tmp,
    spatial_images = list(
      ds = list(
        fov = c(
          png = "spatial-assets/valid.png",
          jpeg = "spatial-assets/valid.jpg",
          text = "spatial-assets/text.png",
          mismatch = "spatial-assets/mismatch.jpg"
        )
      )
    )
  )
  cv_env$available_crb_files <- list(
    selected = "f.crb",
    files = c(ds = "f.crb")
  )
  on.exit(
    {
      rm("session", envir = cv_env)
      rm("Cerebro.options", envir = cv_env)
      rm("available_crb_files", envir = cv_env)
    },
    add = TRUE
  )

  images <- cv_env$cv_external_images("fov")
  expect_identical(
    vapply(images, `[[`, character(1), "label"),
    c("png", "jpeg")
  )
  urls <- vapply(images, `[[`, character(1), "uri")
  expect_true(all(grepl("^session/cerebro-image-", urls)))
  expect_false(any(startsWith(urls, "data:")))
  responses <- lapply(urls, function(url) {
    get(sub("^session/", "", url), envir = cv_env$session$callbacks)()
  })
  expect_identical(
    responses[[1L]]$content,
    list(
      file = normalizePath(
        file.path(assets, "valid.png"),
        winslash = "/",
        mustWork = TRUE
      ),
      owned = FALSE
    )
  )
  expect_identical(
    responses[[2L]]$content,
    list(
      file = normalizePath(
        file.path(assets, "valid.jpg"),
        winslash = "/",
        mustWork = TRUE
      ),
      owned = FALSE
    )
  )
})

test_that("Linked spatial scenes use Spatial ROI geometry and image ownership", {
  skip_if_not(have_bundle)
  root <- withr::local_tempdir()
  dir.create(file.path(root, "spatial-assets"))
  # The resolver reads only the PNG signature; rendering is tested in-browser.
  writeBin(
    as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)),
    file.path(root, "spatial-assets", "roi.png")
  )
  cells <- paste0("c", 1:5)
  metadata <- data.frame(
    cell_barcode = cells,
    sample = "S",
    sample_roi = c("A", "A", "B", "B", NA)
  )
  coordinates <- data.frame(
    x = c(0, 2, 10, 12),
    y = c(0, 0, 0, 0),
    row.names = cells[1:4]
  )
  sd <- list(coordinates = coordinates)
  crb <- list(
    getMetaData = function() metadata,
    availableSpatial = function() c("fov", "fov.2"),
    getSpatialData = function(name) {
      if (name == "fov") {
        sd
      } else {
        list(coordinates = coordinates[1:2, , drop = FALSE])
      }
    }
  )
  bounds <- c(xmin = 0, xmax = 2, ymin = -1, ymax = 1)
  image <- list(
    path = "spatial-assets/roi.png",
    bounds = bounds,
    label = "Histology A",
    roi_field = "sample_roi",
    roi_value = "A"
  )
  settings <- list(
    A = list(rotation_degrees = 90, point_opacity = .8, point_size = 5),
    B = list(rotation_degrees = 180, point_opacity = .9, point_size = 7)
  )
  options <- list(
    cerebro_root = root,
    spatial_images = list(
      ds = list(fov = list(HE = image), fov.2 = list(HE = image))
    ),
    spatial_image_settings = list(
      ds = list(
        fov = list(HE = list(rotation = 12)),
        fov.2 = list(HE = list(rotation = 12))
      )
    ),
    viewer_content = list(
      ds = list(spatial_roi_settings = list(fov = settings, fov.2 = settings))
    )
  )
  cv_env$Cerebro.options <- options
  cv_env$available_crb_files <- list(
    selected = "ds",
    files = c(ds = "file.crb")
  )
  on.exit(
    rm(list = c("Cerebro.options", "available_crb_files"), envir = cv_env),
    add = TRUE
  )
  overview <- cv_env$cv_spatial_one(crb, cells, "fov", TRUE)
  expect_length(overview$images, 0)
  expect_null(overview$image)
  expect_equal(overview$x, c(0, 2, 10, 12, NA))
  roi <- cv_env$cv_spatial_one(crb, cells, "fov", TRUE, "A")
  context <- cv_env$spatial_roi_transform_context(coordinates, metadata, "S")
  expected <- cv_env$rotateSpatialCoordinatesByRoi(
    coordinates[1:2, , drop = FALSE],
    c("A", "A"),
    settings,
    pivots = context$pivots
  )
  expect_equal(roi$x, c(expected$x, NA, NA, NA))
  expect_equal(roi$y, c(expected$y, NA, NA, NA))
  expect_equal(roi$builder_point_size, 5)
  expect_equal(roi$builder_point_opacity, .8)
  external <- cv_env$configured_spatial_images(options, "ds", "fov", "A")
  descriptor <- cv_env$resolve_spatial_background(
    "external::HE",
    list(),
    external
  )
  expect_equal(roi$images[[1]]$bounds, as.list(descriptor$bounds))
  expect_equal(roi$images[[1]]$label, descriptor$label)
  expect_equal(
    roi$images[[1]]$preset,
    cv_env$spatial_background_preset(options, "ds", "fov", descriptor)
  )
  expect_length(cv_env$cv_spatial_one(crb, cells, "fov", TRUE, "B")$images, 0)
  single <- cv_env$cv_spatial_one(crb, cells, "fov.2", TRUE)
  expect_equal(single$x, roi$x)
  expect_equal(single$y, roi$y)
  expect_length(single$images, 1)
  scenes <- cv_env$cv_build_spatial(crb, cells)$samples
  expect_identical(
    vapply(scenes, `[[`, character(1), "name"),
    c("fov", "fov::roi::A", "fov::roi::B", "fov.2")
  )
  expect_identical(
    vapply(scenes, `[[`, character(1), "label"),
    c(
      "fov · All ROIs (spatial)",
      "fov · A (spatial)",
      "fov · B (spatial)",
      "fov.2 · A (spatial)"
    )
  )
  expect_identical(
    vapply(scenes, `[[`, logical(1), "is_roi_overview"),
    c(TRUE, FALSE, FALSE, FALSE)
  )
  expect_length(scenes[[1]]$images, 0)
  expect_equal(scenes[[2]]$x, structure(roi$x, class = "AsIs"))
  expect_length(scenes[[3]]$images, 0)
  one <- crb
  one$availableSpatial <- function() "fov.2"
  single_space <- cv_env$cv_build_spatial(one, cells)
  expect_identical(single_space$name, "fov.2")
  expect_false(single_space$is_roi_overview)
  expect_identical(single_space$label, "fov.2 · A (spatial)")
  # Multiple matching images keep declaration order, independent of labels.
  cv_env$Cerebro.options$spatial_images$ds$fov.2 <- list(
    Zebra = image,
    Alpha = image
  )
  multi_image <- cv_env$cv_spatial_one(crb, cells, "fov.2", TRUE)
  expect_match(multi_image$image$id, ":Zebra$", perl = TRUE)
  expect_match(multi_image$images[[2]]$id, ":Alpha$", perl = TRUE)

  # Embedded ROI declarations follow the same scope precedence as externals.
  embedded <- list(
    coordinates = coordinates,
    histology_images = list(
      A = list(
        histology_image = "data:image/png;base64,AA==",
        histology_image_bounds = bounds,
        roi_field = "sample_roi",
        roi_value = "A"
      )
    )
  )
  crb$getSpatialData <- function(name) embedded
  expect_length(cv_env$cv_spatial_one(crb, cells, "fov", FALSE)$images, 0)
  expect_length(cv_env$cv_spatial_one(crb, cells, "fov", FALSE, "A")$images, 1)
  expect_length(cv_env$cv_spatial_one(crb, cells, "fov", FALSE, "B")$images, 0)
})

test_that("supplement projections served by the Viewer Pack are sent as descriptors", {
  built_names <- NULL
  build <- function(names) {
    built_names <<- names
    stats::setNames(
      lapply(names, function(name) {
        list(ndim = 2L, x = I(c(1, 2)), y = I(c(3, 4)))
      }),
      names
    )
  }
  descriptor <- list(url = "tsne.bin", cells = 2L, dimensions = 2L)
  projections <- cv_env$cv_supplement_projections(
    c("pca", "tsne", "umap3d"),
    list(
      tsne = descriptor,
      umap3d = list(url = "bad", cells = 5L, dimensions = 2L)
    ),
    2L,
    build
  )
  ## Valid resources are not built; invalid ones fall back to arrays.
  expect_identical(built_names, c("pca", "umap3d"))
  expect_identical(names(projections), c("pca", "tsne", "umap3d"))
  expect_identical(projections$tsne$projection_resource, descriptor)
  expect_null(projections$tsne$x)
  expect_identical(projections$pca$x, I(c(1, 2)))
})
