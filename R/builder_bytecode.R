## Byte code for the Builder's server programs, written when the package is
## installed (mirroring R/viewer_bytecode.R). `inst/builder/app.R` otherwise
## parses and compiles all 8 `server/*.R` files on every launch. A cached file
## is used only while its source file still matches the recorded MD5 and R is
## the exact version that compiled it.

.writeBuilderBytecode <- function(package_dir) {
  builder_dir <- file.path(package_dir, "builder")
  server_dir <- file.path(builder_dir, "server")
  if (!dir.exists(server_dir)) {
    return(invisible(NULL))
  }
  out <- file.path(package_dir, "builder-bytecode")
  files <- list.files(server_dir, "[.]R$", recursive = FALSE)
  for (file in files) {
    rel <- file.path("server", file)
    path <- file.path(builder_dir, rel)
    lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
    if (length(lines)) {
      lines[[1L]] <- sub("^\ufeff", "", lines[[1L]])
    }
    parsed <- parse(
      text = lines,
      encoding = "UTF-8",
      keep.source = FALSE
    )
    program <- as.call(c(list(as.name("{")), as.list(parsed)))
    code <- compiler::compile(program, options = list(suppressAll = TRUE))
    target <- file.path(out, paste0(rel, ".rds"))
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    saveRDS(
      list(
        md5 = unname(tools::md5sum(path)),
        r_version = R.version.string,
        code = code
      ),
      target
    )
  }
  invisible(out)
}

## R CMD INSTALL evaluates this file after copying inst/ into R_PACKAGE_DIR.
## Without the byte code the Builder runs as before, so a failure only warns.
local({
  package_dir <- Sys.getenv("R_PACKAGE_DIR")
  if (
    nzchar(package_dir) &&
      identical(Sys.getenv("R_PACKAGE_NAME"), "CerebroNexus") &&
      dir.exists(file.path(package_dir, "builder"))
  ) {
    tryCatch(
      .writeBuilderBytecode(package_dir),
      error = function(error) {
        warning(
          "Builder byte code was not written: ",
          conditionMessage(error),
          call. = FALSE
        )
      }
    )
  }
})
