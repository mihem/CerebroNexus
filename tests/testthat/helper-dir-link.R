## Links `link` to the existing directory `target`; TRUE when it worked. On
## Windows a junction, which R can remove again (a directory symbolic link
## there cannot be deleted by unlink() and is left in the temporary folder).
test_dir_link <- function(target, link) {
  ok <- if (identical(.Platform$OS.type, "windows")) {
    tryCatch(
      Sys.junction(normalizePath(target, mustWork = TRUE), link),
      error = function(error) FALSE
    )
  } else {
    file.symlink(target, link)
  }
  isTRUE(suppressWarnings(ok))
}
