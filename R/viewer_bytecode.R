## Byte code for the Viewer's helper functions, written when the package is
## installed. viewerSource() (inst/viewer/source_cache.R) evaluates the Viewer
## files in every browser session, and the JIT compiles each `name <- function`
## helper the first time it runs in a process: 0.1-0.2 s before the first view
## of a page. Compiling all helpers takes about 5 s, too long for start-up, so
## it is done once here. A file's byte code is used only while the file still
## has the md5 recorded with it and R is the version that compiled it.

## Positions of the top-level `name <- function(...)` expressions.
.viewerBytecodeIndex <- function(expressions) {
  which(vapply(
    expressions,
    function(expression) {
      is.call(expression) &&
        length(expression) == 3L &&
        identical(expression[[1L]], as.name("<-")) &&
        is.symbol(expression[[2L]]) &&
        is.call(expression[[3L]]) &&
        identical(expression[[3L]][[1L]], as.name("function"))
    },
    logical(1)
  ))
}

## Write viewer-bytecode/<file>.rds for every Viewer R file with helpers.
.writeViewerBytecode <- function(package_dir) {
  viewer <- file.path(package_dir, "viewer")
  out <- file.path(package_dir, "viewer-bytecode")
  for (file in list.files(viewer, "[.]R$", recursive = TRUE)) {
    path <- file.path(viewer, file)
    expressions <- parse(path, keep.source = FALSE)
    index <- .viewerBytecodeIndex(expressions)
    if (!length(index)) {
      next
    }
    code <- lapply(
      expressions[index],
      compiler::compile,
      options = list(suppressAll = TRUE)
    )
    target <- file.path(out, paste0(file, ".rds"))
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    saveRDS(
      list(
        md5 = unname(tools::md5sum(path)),
        r_version = R.version.string,
        index = index,
        ## the helpers' names, for a file that takes one function's byte code
        ## (`server` of shiny_server.R) instead of evaluating all of them
        names = vapply(
          expressions[index],
          function(expression) as.character(expression[[2L]]),
          character(1)
        ),
        code = code
      ),
      target
    )
  }
  invisible(out)
}

## R CMD INSTALL evaluates this file after copying inst/ into R_PACKAGE_DIR.
## Without the byte code the Viewer runs as before, so a failure only warns.
local({
  package_dir <- Sys.getenv("R_PACKAGE_DIR")
  if (
    nzchar(package_dir) &&
      identical(Sys.getenv("R_PACKAGE_NAME"), "CerebroNexus") &&
      dir.exists(file.path(package_dir, "viewer"))
  ) {
    tryCatch(
      .writeViewerBytecode(package_dir),
      error = function(error) {
        warning(
          "Viewer byte code was not written: ",
          conditionMessage(error),
          call. = FALSE
        )
      }
    )
  }
})
