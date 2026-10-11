## Public MSigDB queries and bounded compact results, shared across sessions.
## No dataset/session access: the caller supplies species and gene-set identity.
.msigdb_process_cache <- new.env(parent = emptyenv())

.emptyMsigdbCatalogue <- function() {
  data.frame(
    gs_name = character(),
    collection = character(),
    stringsAsFactors = FALSE
  )
}

.msigdbCache <- function() {
  get(".msigdb_process_cache", inherits = TRUE)
}

.msigdbGeneCacheKey <- function(species, gene_set) {
  paste("genes", species, gene_set, sep = "\r")
}

.msigdbFunction <- function() {
  msigdbr::msigdbr
}

.msigdbCollectionsFunction <- function() {
  if ("msigdbr_collections" %in% getNamespaceExports("msigdbr")) {
    msigdbr::msigdbr_collections()
  } else {
    NULL
  }
}

.msigdbCatalogueRows <- function() {
  query <- .msigdbFunction()
  collections <- .msigdbCollectionsFunction()
  column <- intersect(c("gs_collection", "gs_cat"), names(collections))
  argument <- intersect(c("collection", "category"), names(formals(query)))
  if (!length(column) || !length(argument)) {
    return(query(species = "Homo sapiens")) # compatibility with older msigdbr
  }
  # Discard each wide table before requesting the next collection. The public
  # API does not expose gene-set names alone; never retain the full database.
  pieces <- lapply(unique(collections[[column[[1L]]]]), function(collection) {
    args <- list(species = "Homo sapiens")
    args[[argument[[1L]]]] <- collection
    table <- do.call(query, args)
    column <- intersect(c("gs_collection", "gs_cat"), names(table))
    if (!length(column) || !"gs_name" %in% names(table)) {
      stop("MSigDB query returned unexpected columns.")
    }
    unique(data.frame(
      gs_name = table$gs_name,
      gs_collection = table[[column[[1L]]]]
    ))
  })
  if (!length(pieces)) {
    return(NULL)
  }
  do.call(rbind, pieces)
}

.cacheMsigdbGenes <- function(cache, key, genes) {
  # Resolved single sets have a separate small FIFO budget; large sets can be
  # returned without retaining them. Size accounting here is ordinary vectors.
  present <- grep("^genes\\r", ls(cache, all.names = TRUE), value = TRUE)
  keys <- unique(c(intersect(cache[["gene_order"]], present), present))
  if (as.numeric(object.size(genes)) <= 4 * 1024^2) {
    cache[[key]] <- genes
    keys <- unique(c(setdiff(keys, key), key))
  }
  while (
    length(keys) > 128L ||
      sum(vapply(
        keys,
        function(k) as.numeric(object.size(cache[[k]])),
        numeric(1)
      )) >
        4 * 1024^2
  ) {
    rm(list = keys[[1L]], envir = cache)
    keys <- keys[-1L]
  }
  cache[["gene_order"]] <- keys
}

getMsigdbCatalogue <- function() {
  cache <- .msigdbCache()
  if (exists("catalogue", envir = cache, inherits = FALSE)) {
    return(cache[["catalogue"]])
  }
  if (!requireNamespace("msigdbr", quietly = TRUE)) {
    warning("The 'msigdbr' package is required to resolve gene sets.")
    return(.emptyMsigdbCatalogue())
  }
  table <- tryCatch(
    .msigdbCatalogueRows(),
    error = function(error_condition) {
      warning("MSigDB query failed: ", conditionMessage(error_condition))
      NULL
    }
  )
  collection_column <- if (
    !is.null(table) && "gs_collection" %in% colnames(table)
  ) {
    "gs_collection"
  } else if (!is.null(table) && "gs_cat" %in% colnames(table)) {
    "gs_cat"
  } else {
    NULL
  }
  if (
    is.null(collection_column) ||
      !is.data.frame(table) ||
      !"gs_name" %in% colnames(table)
  ) {
    if (!is.null(table)) {
      warning("MSigDB query returned unexpected columns.")
    }
    return(.emptyMsigdbCatalogue())
  }
  catalogue <- unique(data.frame(
    gs_name = trimws(as.character(table[["gs_name"]])),
    collection = trimws(as.character(table[[collection_column]])),
    stringsAsFactors = FALSE
  ))
  catalogue <- catalogue[
    !is.na(catalogue$gs_name) &
      nzchar(catalogue$gs_name) &
      !is.na(catalogue$collection) &
      nzchar(catalogue$collection),
    ,
    drop = FALSE
  ]
  ambiguous <- duplicated(catalogue$gs_name) |
    duplicated(catalogue$gs_name, fromLast = TRUE)
  catalogue <- catalogue[!ambiguous, , drop = FALSE]
  row.names(catalogue) <- NULL
  if (nrow(catalogue) == 0L) {
    warning("MSigDB query returned an empty catalogue.")
    return(.emptyMsigdbCatalogue())
  }
  cache[["catalogue"]] <- catalogue
  rm(table)
  catalogue
}

getGeneSetNames <- function() {
  sort(unique(getMsigdbCatalogue()$gs_name))
}

getMsigdbGenes <- function(species, gene_set) {
  cache <- .msigdbCache()
  key <- .msigdbGeneCacheKey(species, gene_set)
  if (exists(key, envir = cache, inherits = FALSE)) {
    return(cache[[key]])
  }
  catalogue <- getMsigdbCatalogue()
  collection <- catalogue$collection[match(gene_set, catalogue$gs_name)]
  if (length(collection) != 1L || is.na(collection) || !nzchar(collection)) {
    return(character())
  }
  compact <- cache[["compact_collection"]]
  if (
    !is.null(compact) &&
      identical(compact$species, species) &&
      identical(compact$collection, collection)
  ) {
    genes <- compact$genes[[gene_set]]
    if (length(genes)) {
      .cacheMsigdbGenes(cache, key, genes)
      return(genes)
    }
  }
  arguments <- list(species = species)
  query <- .msigdbFunction()
  if ("collection" %in% names(formals(query))) {
    arguments[["collection"]] <- collection
  } else {
    arguments[["category"]] <- collection
  }
  table <- tryCatch(
    do.call(query, arguments),
    error = function(error_condition) {
      warning("MSigDB query failed: ", conditionMessage(error_condition))
      NULL
    }
  )
  if (is.null(table)) {
    return(character())
  }
  if (
    !is.data.frame(table) ||
      !all(c("gs_name", "gene_symbol") %in% colnames(table))
  ) {
    warning("MSigDB gene query returned unexpected columns.")
    return(character())
  }
  returned_names <- trimws(as.character(table[["gs_name"]]))
  matches <- !is.na(returned_names) & returned_names == gene_set
  genes <- trimws(as.character(table[["gene_symbol"]][matches]))
  genes <- sort(unique(genes[!is.na(genes) & nzchar(genes)]))
  if (length(genes) > 0L) {
    .cacheMsigdbGenes(cache, key, genes)
  }
  # Reuse only the two relevant fields for the most recent small collection.
  # Large collections are not retained, and the wide query table is never cached.
  symbols <- trimws(as.character(table[["gene_symbol"]]))
  valid <- !is.na(returned_names) &
    nzchar(returned_names) &
    !is.na(symbols) &
    nzchar(symbols)
  if (
    as.numeric(object.size(returned_names)) +
      as.numeric(object.size(symbols)) <=
      8 * 1024^2
  ) {
    mapping <- lapply(
      split(symbols[valid], returned_names[valid]),
      function(x) sort(unique(x))
    )
    compact <- list(species = species, collection = collection, genes = mapping)
    cache[["compact_collection"]] <- if (
      as.numeric(object.size(compact)) <= 8 * 1024^2
    ) {
      compact
    } else {
      NULL
    }
  }
  rm(table)
  genes
}
