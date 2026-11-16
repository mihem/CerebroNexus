##----------------------------------------------------------------------------##
## The encodings of repertoires kept between sessions (user cache). Sourced by
## the page (server.R) and by the job that encodes a large repertoire in another
## R process (data.R, ir_encoded_background()): functions only, which need
## encode.R (IR_ENCODING_VERSION, ir_clone_ranks()) and nothing of a session.
##----------------------------------------------------------------------------##

## Where an encoding made in memory is kept between sessions: the user cache
## directory (CEREBRO_IR_CACHE overrides it). The Pack's encoding is the real
## store; this one spares a data set without it (or with a Pack built before
## the encoding) the minute of encoding at every start.
ir_disk_cache_file <- function(name, key) {
  dir <- Sys.getenv("CEREBRO_IR_CACHE", unset = "")
  if (!nzchar(dir)) {
    dir <- file.path(tools::R_user_dir("CerebroNexus", "cache"), "immune")
  }
  file.path(
    dir,
    paste0(
      name,
      "-v",
      IR_ENCODING_VERSION,
      "-",
      rlang::hash(as.character(key$path %||% "")),
      "-",
      rlang::hash(key),
      ".qs2"
    )
  )
}

## An encoding is kept as two files: the integer columns with the clone ranks
## (12 MB for the 1.46 million cells of Ren, read in 0.04 s), and the clone
## names beside it (3.3 million strings: 400 MB in memory, seconds to read),
## which only the clone tables and Compare need: they are read then, once per
## process (ir_clone_names()).
ir_disk_cache_names_file <- function(file) {
  sub("[.]qs2$", ".names.qs2", file)
}

ir_disk_cache_read <- function(file) {
  if (!file.exists(file)) {
    return(NULL)
  }
  value <- tryCatch(qs2::qs_read(file), error = function(e) NULL)
  if (!is.list(value) || !identical(value$version, IR_ENCODING_VERSION)) {
    return(NULL)
  }
  if (isTRUE(value$names_apart)) {
    names_file <- ir_disk_cache_names_file(file)
    ## Without its names an encoding cannot name its clones: encode again.
    if (!file.exists(names_file)) {
      return(NULL)
    }
    value$names_file <- names_file
  } else if (!is.null(value$names)) {
    ## kept by an earlier release in one file: kept again in two
    ir_disk_cache_write(file, value)
  }
  value
}

## Written through a temporary name, so a session that stops half way or reads
## at the same time never sees a partial file. A failure (a read-only cache) is
## not an error: the next start encodes again.
ir_disk_cache_write <- function(file, value) {
  tryCatch(
    {
      dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
      save <- function(object, target) {
        partial <- paste0(target, ".", Sys.getpid(), ".tmp")
        on.exit(unlink(partial), add = TRUE)
        qs2::qs_save(object, partial)
        file.rename(partial, target)
      }
      names_file <- ir_disk_cache_names_file(file)
      if (!is.null(value$names)) {
        ## the names apart, written first: an encoding found has its names
        save(value$names, names_file)
        if (is.null(value$rank)) {
          value$rank <- ir_clone_ranks(value$names)
        }
        value$names <- NULL
        value$names_apart <- TRUE
      }
      save(value, file)
      ## The data set's earlier entries (another file version, another encoding
      ## version) are out of date for good.
      same_path <- sub("-[0-9a-f]+[.]qs2$", "", basename(file))
      stale <- list.files(
        dirname(file),
        pattern = paste0("^", gsub("-v[0-9]+-", "-v[0-9]+-", same_path), "-")
      )
      unlink(file.path(
        dirname(file),
        setdiff(stale, basename(c(file, names_file)))
      ))
    },
    error = function(e) NULL
  )
  invisible(NULL)
}
