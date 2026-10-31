##----------------------------------------------------------------------------##
## Reading a serialised Seurat object, whichever way it was written.
##
## `.rds` is base R. `qs` and `qs2` are what people reach for once objects get
## big enough that `saveRDS()` becomes the slow part of the day, so a tool that
## only reads `.rds` asks them to convert first.
##
## Pure: no Shiny. Each reader is optional -- the package is only needed by
## whoever actually has files in that format, and its absence is reported as a
## sentence rather than a missing-function error.
##----------------------------------------------------------------------------##

`%||%` <- function(a, b) if (is.null(a)) b else a

## An incremental MD5 over the bytes a copy loop already holds, or NULL when
## openssl's digest primitives are unavailable (callers then hash the file).
.builder_md5_stream <- function() {
  if (!requireNamespace("openssl", quietly = TRUE)) {
    return(NULL)
  }
  namespace <- asNamespace("openssl")
  init <- get0("md_init", envir = namespace, mode = "function")
  feed <- get0("md_feed", envir = namespace, mode = "function")
  final <- get0("md_final", envir = namespace, mode = "function")
  if (is.null(init) || is.null(feed) || is.null(final)) {
    return(NULL)
  }
  state <- tryCatch(init("md5"), error = function(error) NULL)
  if (is.null(state)) {
    return(NULL)
  }
  list(
    feed = function(bytes) invisible(feed(state, bytes)),
    final = function() {
      digest <- final(state)
      if (!is.raw(digest) || length(digest) != 16L) {
        return(NA_character_)
      }
      paste(as.character(digest), collapse = "")
    }
  )
}

## `on_progress(copied_bytes, total_bytes)` is called after each buffer.
builder_copy_file_verified <- function(
  source,
  destination,
  expected_md5 = NULL,
  buffer_bytes = 16L * 1024L^2,
  on_progress = NULL
) {
  if (
    !is.character(source) ||
      length(source) != 1L ||
      is.na(source) ||
      !file.exists(source) ||
      dir.exists(source) ||
      !is.character(destination) ||
      length(destination) != 1L ||
      is.na(destination) ||
      file.exists(destination)
  ) {
    stop(
      "A verified copy requires one file and a new destination.",
      call. = FALSE
    )
  }
  expected_md5 <- if (is.null(expected_md5)) NULL else tolower(expected_md5)
  if (
    !is.null(expected_md5) &&
      (length(expected_md5) != 1L ||
        !grepl("^[[:xdigit:]]{32}$", expected_md5))
  ) {
    stop("The expected file MD5 is invalid.", call. = FALSE)
  }
  input <- output <- NULL
  complete <- FALSE
  on.exit(
    {
      if (!is.null(input)) {
        try(close(input), silent = TRUE)
      }
      if (!is.null(output)) {
        try(close(output), silent = TRUE)
      }
      if (!complete) unlink(destination, force = TRUE)
    },
    add = TRUE
  )
  expected_bytes <- suppressWarnings(as.double(file.info(source)$size[[1L]]))
  if (
    identical(unname(Sys.info()[["sysname"]]), "Darwin") &&
      file.exists("/bin/cp")
  ) {
    status <- suppressWarnings(system2(
      "/bin/cp",
      c("-c", "--", shQuote(source), shQuote(destination)),
      stdout = FALSE,
      stderr = FALSE
    ))
    if (isTRUE(status == 0L) && file.exists(destination)) {
      copied <- suppressWarnings(as.double(file.info(destination)$size[[1L]]))
      actual_md5 <- unname(tolower(as.character(tools::md5sum(destination))))
      if (
        !is.finite(expected_bytes) ||
          !identical(copied, expected_bytes) ||
          length(actual_md5) != 1L ||
          is.na(actual_md5) ||
          !grepl("^[[:xdigit:]]{32}$", actual_md5) ||
          (!is.null(expected_md5) && !identical(actual_md5, expected_md5))
      ) {
        stop("The cloned file could not be verified.", call. = FALSE)
      }
      complete <- TRUE
      return(list(path = destination, bytes = copied, md5 = actual_md5))
    }
    unlink(destination, force = TRUE)
  }
  input <- file(source, open = "rb")
  output <- file(destination, open = "wb")
  # Hash each buffer while it is in memory instead of rereading the copy.
  digest <- .builder_md5_stream()
  copied <- 0
  repeat {
    bytes <- readBin(input, what = "raw", n = as.integer(buffer_bytes))
    if (!length(bytes)) {
      break
    }
    writeBin(bytes, output)
    if (!is.null(digest)) {
      digest$feed(bytes)
    }
    copied <- copied + length(bytes)
    if (is.function(on_progress)) {
      try(on_progress(copied, expected_bytes), silent = TRUE)
    }
  }
  close(input)
  close(output)
  input <- output <- NULL
  actual_md5 <- if (is.null(digest)) NA_character_ else digest$final()
  if (is.na(actual_md5)) {
    actual_md5 <- unname(tools::md5sum(destination))
  }
  if (
    !is.finite(expected_bytes) || !identical(as.double(copied), expected_bytes)
  ) {
    stop(
      "The source file ended before the verified copy completed.",
      call. = FALSE
    )
  }
  if (!is.null(expected_md5) && !identical(actual_md5, expected_md5)) {
    stop("The copied file does not match its saved MD5.", call. = FALSE)
  }
  complete <- TRUE
  list(path = destination, bytes = as.double(copied), md5 = actual_md5)
}

## Shiny writes browser uploads below this R session's tempdir(). Only such a
## file may be moved into Builder's private session sources; anything else
## (a picked local file, a project file) is the user's and is copied.
builder_upload_transport_owned <- function(path, temp_root = tempdir()) {
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(path) ||
      !file.exists(path) ||
      dir.exists(path)
  ) {
    return(FALSE)
  }
  canonical <- function(value) {
    tryCatch(
      normalizePath(value, winslash = "/", mustWork = TRUE),
      error = function(error) NA_character_
    )
  }
  candidate <- canonical(path)
  root <- canonical(temp_root)
  if (is.na(candidate) || is.na(root)) {
    return(FALSE)
  }
  if (.Platform$OS.type == "windows") {
    candidate <- tolower(candidate)
    root <- tolower(root)
  }
  startsWith(candidate, paste0(sub("/+$", "", root), "/"))
}

builder_server_path_roots <- function(
  roots = getOption(
    "CerebroNexus.builder.server_path_roots",
    path.expand("~")
  )
) {
  if (!is.character(roots) || !length(roots)) {
    stop("No allowed server folders are configured.", call. = FALSE)
  }
  roots <- unique(path.expand(roots[!is.na(roots) & nzchar(roots)]))
  roots <- roots[vapply(roots, fs::is_absolute_path, logical(1))]
  roots <- roots[dir.exists(roots)]
  resolved <- vapply(
    roots,
    function(path) {
      tryCatch(
        as.character(fs::path_real(path)),
        error = function(error) ""
      )
    },
    character(1)
  )
  resolved <- unique(resolved[nzchar(resolved)])
  if (!length(resolved)) {
    stop("No allowed server folders are available.", call. = FALSE)
  }
  resolved
}

builder_server_path_resolve <- function(
  value,
  type = c("directory", "file"),
  roots = builder_server_path_roots(),
  .wsl = builder_is_wsl(),
  .translate = builder_wsl_picker_paths
) {
  type <- match.arg(type)
  if (
    isTRUE(.wsl) &&
      is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      grepl("^[A-Za-z]:[/\\\\]", trimws(value))
  ) {
    value <- .translate(trimws(value))
  }
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(trimws(value)) ||
      !isTRUE(fs::is_absolute_path(trimws(value)))
  ) {
    stop("Enter an absolute server path.", call. = FALSE)
  }
  path <- tryCatch(
    as.character(fs::path_real(path.expand(trimws(value)))),
    error = function(error) NULL
  )
  valid_path <- is.character(path) &&
    length(path) == 1L &&
    !is.na(path) &&
    nzchar(path)
  valid_type <- valid_path &&
    if (identical(type, "directory")) {
      dir.exists(path)
    } else {
      file.exists(path) && !dir.exists(path)
    }
  if (!isTRUE(valid_type)) {
    stop(
      if (identical(type, "directory")) {
        "The server folder does not exist."
      } else {
        "The server file does not exist."
      },
      call. = FALSE
    )
  }
  allowed_roots <- builder_server_path_roots(roots)
  allowed <- any(vapply(
    allowed_roots,
    function(root) .pathWithin(path, root),
    logical(1)
  ))
  if (!allowed) {
    stop(
      paste0(
        "The path is outside the allowed server folders. Allowed: ",
        builder_server_path_roots_label(allowed_roots),
        "."
      ),
      call. = FALSE
    )
  }
  path
}

builder_server_path_roots_label <- function(roots) {
  if (.Platform$OS.type == "windows") {
    roots <- gsub("/", "\\", roots, fixed = TRUE)
  }
  paste(roots, collapse = "; ")
}

builder_native_picker_kinds <- function() {
  c(
    "output_directory",
    "project_directory",
    "project_manifest",
    "dataset_files"
  )
}

builder_native_picker_cancel_marker <- function() {
  "__CEREBRO_BUILDER_PICKER_CANCELLED__"
}

builder_native_picker_contract <- function(
  kind = builder_native_picker_kinds()
) {
  kind <- match.arg(kind)
  switch(
    kind,
    output_directory = list(
      kind = kind,
      type = "directory",
      prompt = "Choose where to save the build output.",
      unavailable = "The selected folder is not available."
    ),
    project_directory = list(
      kind = kind,
      type = "directory",
      prompt = "Choose a folder for the Builder project.",
      unavailable = "The selected folder is not available."
    ),
    project_manifest = list(
      kind = kind,
      type = "file",
      prompt = "Open a Builder project.",
      windows_filter = "Builder project (*.json)|*.json|All files (*.*)|*.*",
      macos_types = "public.json",
      zenity_filter = "Builder project | *.json",
      kdialog_filter = "Builder project (*.json)",
      unavailable = "The selected project file is not available."
    ),
    dataset_files = {
      formats <- builder_available_formats()
      extensions <- unique(tolower(unlist(lapply(
        formats,
        `[[`,
        "extensions"
      ))))
      extension_list <- paste0("*.", extensions, collapse = ";")
      extension_words <- paste0("*.", extensions, collapse = " ")
      list(
        kind = kind,
        type = "file",
        multiple = TRUE,
        prompt = "Choose dataset files.",
        windows_filter = paste0(
          "Dataset files (",
          extension_list,
          ")|",
          extension_list,
          "|All files (*.*)|*.*"
        ),
        macos_types = extensions,
        zenity_filter = paste("Dataset files |", extension_words),
        kdialog_filter = paste0("Dataset files (", extension_words, ")"),
        unavailable = "One or more selected dataset files are not available."
      )
    }
  )
}

builder_macos_picker_script <- function(kind) {
  contract <- builder_native_picker_contract(kind)
  quote_applescript <- function(value) {
    value <- gsub("\\", "\\\\", value, fixed = TRUE)
    paste0('"', gsub('"', '\\"', value, fixed = TRUE), '"')
  }
  command <- if (identical(contract$type, "directory")) {
    paste0(
      "POSIX path of (choose folder with prompt ",
      quote_applescript(contract$prompt),
      ")"
    )
  } else {
    types <- paste0(
      '{',
      paste(
        vapply(
          contract$macos_types,
          quote_applescript,
          character(1)
        ),
        collapse = ", "
      ),
      '}'
    )
    choose <- paste0(
      "POSIX path of (choose file with prompt ",
      quote_applescript(contract$prompt),
      " of type ",
      types,
      ")"
    )
    if (isTRUE(contract$multiple)) {
      paste(
        paste0(
          "set selectedFiles to choose file with prompt ",
          quote_applescript(contract$prompt),
          " of type ",
          types,
          " with multiple selections allowed"
        ),
        "set selectedPaths to \"\"",
        "repeat with selectedFile in selectedFiles",
        paste0(
          "set selectedPaths to selectedPaths & ",
          "POSIX path of selectedFile & linefeed"
        ),
        "end repeat",
        "return selectedPaths",
        sep = "\n"
      )
    } else {
      choose
    }
  }
  paste(
    "try",
    command,
    "on error number -128",
    paste0('return "', builder_native_picker_cancel_marker(), '"'),
    "end try",
    sep = "\n"
  )
}

builder_windows_folder_picker_source <- function() {
  paste(
    "using System;",
    "using System.Runtime.InteropServices;",
    "",
    "[ComImport]",
    "[Guid(\"DC1C5A9C-E88A-4DDE-A5A1-60F82A20AEF7\")]",
    "internal class FileOpenDialogClass {}",
    "",
    "[Flags]",
    "internal enum FileOpenOptions : uint",
    "{",
    "    PickFolders = 0x00000020,",
    "    ForceFileSystem = 0x00000040,",
    "    NoChangeDirectory = 0x00000008,",
    "    PathMustExist = 0x00000800",
    "}",
    "",
    "internal enum ShellDisplayName : uint",
    "{",
    "    FileSystemPath = 0x80058000",
    "}",
    "",
    "[ComImport]",
    "[Guid(\"43826D1E-E718-42EE-BC55-A1E261C37BFE\")]",
    "[InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]",
    "internal interface IShellItem",
    "{",
    "    void BindToHandler(IntPtr bindingContext, ref Guid handler,",
    "        ref Guid interfaceId, out IntPtr result);",
    "    void GetParent(out IShellItem parent);",
    "    void GetDisplayName(ShellDisplayName name, out IntPtr value);",
    "    void GetAttributes(uint mask, out uint attributes);",
    "    void Compare(IShellItem other, uint hint, out int order);",
    "}",
    "",
    "[ComImport]",
    "[Guid(\"D57C7288-D4AD-4768-BE02-9D969532D960\")]",
    "[InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]",
    "internal interface IFileOpenDialog",
    "{",
    "    [PreserveSig] int Show(IntPtr owner);",
    "    void SetFileTypes(uint count, IntPtr filters);",
    "    void SetFileTypeIndex(uint index);",
    "    void GetFileTypeIndex(out uint index);",
    "    void Advise(IntPtr events, out uint cookie);",
    "    void Unadvise(uint cookie);",
    "    void SetOptions(FileOpenOptions options);",
    "    void GetOptions(out FileOpenOptions options);",
    "    void SetDefaultFolder(IShellItem item);",
    "    void SetFolder(IShellItem item);",
    "    void GetFolder(out IShellItem item);",
    "    void GetCurrentSelection(out IShellItem item);",
    "    void SetFileName([MarshalAs(UnmanagedType.LPWStr)] string name);",
    "    void GetFileName([MarshalAs(UnmanagedType.LPWStr)] out string name);",
    "    void SetTitle([MarshalAs(UnmanagedType.LPWStr)] string title);",
    "    void SetOkButtonLabel([MarshalAs(UnmanagedType.LPWStr)] string text);",
    "    void SetFileNameLabel([MarshalAs(UnmanagedType.LPWStr)] string label);",
    "    void GetResult(out IShellItem item);",
    "    void AddPlace(IShellItem item, int alignment);",
    "    void SetDefaultExtension([MarshalAs(UnmanagedType.LPWStr)] string extension);",
    "    void Close(int result);",
    "    void SetClientGuid(ref Guid guid);",
    "    void ClearClientData();",
    "    void SetFilter(IntPtr filter);",
    "    void GetResults(out IntPtr items);",
    "    void GetSelectedItems(out IntPtr items);",
    "}",
    "",
    "public static class BuilderModernFolderPicker",
    "{",
    "    private const int Cancelled = unchecked((int)0x800704C7);",
    "",
    "    public static string Pick(string title, IntPtr owner)",
    "    {",
    "        IFileOpenDialog dialog =",
    "            (IFileOpenDialog)new FileOpenDialogClass();",
    "        IShellItem item = null;",
    "        IntPtr value = IntPtr.Zero;",
    "        try",
    "        {",
    "            FileOpenOptions options;",
    "            dialog.GetOptions(out options);",
    "            dialog.SetOptions(options | FileOpenOptions.PickFolders |",
    "                FileOpenOptions.ForceFileSystem |",
    "                FileOpenOptions.NoChangeDirectory |",
    "                FileOpenOptions.PathMustExist);",
    "            dialog.SetTitle(title);",
    "            int result = dialog.Show(owner);",
    "            if (result == Cancelled) return null;",
    "            Marshal.ThrowExceptionForHR(result);",
    "            dialog.GetResult(out item);",
    "            item.GetDisplayName(ShellDisplayName.FileSystemPath, out value);",
    "            return Marshal.PtrToStringUni(value);",
    "        }",
    "        finally",
    "        {",
    "            if (value != IntPtr.Zero) Marshal.FreeCoTaskMem(value);",
    "            if (item != null) Marshal.FinalReleaseComObject(item);",
    "            Marshal.FinalReleaseComObject(dialog);",
    "        }",
    "    }",
    "}",
    sep = "\n"
  )
}

builder_windows_picker_script <- function(kind) {
  contract <- builder_native_picker_contract(kind)
  quote_powershell <- function(value) {
    paste0("'", gsub("'", "''", value, fixed = TRUE), "'")
  }
  directory <- identical(contract$type, "directory")
  setup <- if (directory) {
    paste(
      "$folderPickerSource = @'",
      builder_windows_folder_picker_source(),
      "'@",
      "Add-Type -TypeDefinition $folderPickerSource",
      sep = "\n"
    )
  } else {
    character()
  }
  dialog <- if (directory) {
    paste(
      paste0(
        "$selected = [BuilderModernFolderPicker]::Pick(",
        quote_powershell(contract$prompt),
        ", $owner.Handle)"
      ),
      "if ($null -ne $selected) {",
      "  [Console]::WriteLine($selected)",
      "} else {",
      paste0(
        "  [Console]::WriteLine(",
        quote_powershell(builder_native_picker_cancel_marker()),
        ")"
      ),
      "}",
      sep = "\n"
    )
  } else {
    multiple <- isTRUE(contract$multiple)
    paste(
      "$dialog = New-Object System.Windows.Forms.OpenFileDialog",
      paste0("$dialog.Title = ", quote_powershell(contract$prompt)),
      paste0("$dialog.Filter = ", quote_powershell(contract$windows_filter)),
      "$dialog.AutoUpgradeEnabled = $true",
      paste0("$dialog.Multiselect = $", tolower(as.character(multiple))),
      "$result = $dialog.ShowDialog($owner)",
      "if ($result -eq [System.Windows.Forms.DialogResult]::OK) {",
      if (multiple) {
        paste0(
          "  $dialog.FileNames | ForEach-Object { ",
          "[Console]::WriteLine($_) }"
        )
      } else {
        "  [Console]::WriteLine($dialog.FileName)"
      },
      "} else {",
      paste0(
        "  [Console]::WriteLine(",
        quote_powershell(builder_native_picker_cancel_marker()),
        ")"
      ),
      "}",
      sep = "\n"
    )
  }
  paste(
    "$ErrorActionPreference = 'Stop'",
    "Add-Type -AssemblyName System.Windows.Forms",
    "[Console]::OutputEncoding = [System.Text.UTF8Encoding]::new($false)",
    setup,
    "$owner = New-Object System.Windows.Forms.Form",
    "$owner.ShowInTaskbar = $false",
    "$owner.StartPosition = 'CenterScreen'",
    "$owner.Size = New-Object System.Drawing.Size(1, 1)",
    "$owner.Opacity = 0",
    "$owner.TopMost = $true",
    "$owner.Show()",
    "$owner.Activate()",
    "$dialog = $null",
    "try {",
    paste0("  ", gsub("\n", "\n  ", dialog, fixed = TRUE)),
    "} finally {",
    "  if ($null -ne $dialog) { $dialog.Dispose() }",
    "  $owner.Close()",
    "  $owner.Dispose()",
    "}",
    sep = "\n"
  )
}

builder_dataset_extensions <- function(available_only = FALSE) {
  formats <- if (isTRUE(available_only)) {
    builder_available_formats()
  } else {
    builder_formats
  }
  unique(tolower(unlist(lapply(formats, `[[`, "extensions"))))
}

builder_table_extensions <- function() {
  c("csv", "tsv", "txt", "xls", "xlsx", "xlsm")
}

builder_image_extensions <- function() {
  c("png", "jpg", "jpeg")
}

builder_file_accept <- function(extensions) {
  extensions <- unique(tolower(as.character(extensions)))
  extensions <- extensions[!is.na(extensions) & nzchar(extensions)]
  paste0(".", extensions, collapse = ",")
}

builder_is_wsl <- function() {
  if (!identical(Sys.info()[["sysname"]], "Linux")) {
    return(FALSE)
  }
  nzchar(Sys.getenv("WSL_DISTRO_NAME")) || nzchar(Sys.getenv("WSL_INTEROP"))
}

## wslpath converts one path per invocation; a single shell converts every
## selected path so a multi-file pick starts one process instead of one each.
builder_wsl_picker_paths <- function(paths, .run = processx::run) {
  paths <- as.character(paths)
  if (!length(paths)) {
    return(character())
  }
  result <- .run(
    "sh",
    c(
      "-c",
      'for path in "$@"; do wslpath -u "$path" || exit 1; done',
      "sh",
      paths
    ),
    error_on_status = FALSE
  )
  converted <- strsplit(result$stdout %||% "", "\n", fixed = TRUE)[[1L]]
  converted <- trimws(converted)
  if (
    !identical(result$status, 0L) ||
      length(converted) != length(paths) ||
      any(!nzchar(converted))
  ) {
    stop(
      "Cannot access the selected Windows path. Choose another location.",
      call. = FALSE
    )
  }
  converted
}

builder_native_picker_spec <- function(
  kind = c(
    "output_directory",
    "project_directory",
    "project_manifest",
    "dataset_files"
  ),
  .system = Sys.info()[["sysname"]] %||% "",
  .powershell = NULL,
  .which = Sys.which,
  .wsl = builder_is_wsl()
) {
  kind <- match.arg(kind)
  contract <- builder_native_picker_contract(kind)
  directory <- identical(contract$type, "directory")
  prompt <- contract$prompt
  if (identical(.system, "Linux") && isTRUE(.wsl)) {
    powershell <- .which("powershell.exe")
    if (nzchar(powershell) && nzchar(.which("wslpath"))) {
      script <- builder_windows_picker_script(kind)
      # EncodedCommand avoids WSL's extra Windows command-line quoting layer.
      encoded <- base64enc::base64encode(iconv(
        script,
        from = "UTF-8",
        to = "UTF-16LE",
        toRaw = TRUE
      )[[1L]])
      return(list(
        command = unname(powershell),
        args = c(
          "-NoLogo",
          "-NoProfile",
          "-NonInteractive",
          "-STA",
          "-EncodedCommand",
          encoded
        ),
        cancel_status = integer(),
        encoding = "UTF-8",
        path_style = "windows"
      ))
    }
  }
  if (identical(.system, "Windows")) {
    powershell <- .powershell %||% .which("powershell.exe")
    if (!nzchar(powershell)) {
      powershell <- .which("powershell")
    }
    if (!nzchar(powershell)) {
      powershell <- .which("pwsh.exe")
    }
    if (!nzchar(powershell)) {
      powershell <- .which("pwsh")
    }
    if (!nzchar(powershell)) {
      stop(
        "Windows PowerShell is required for the system picker.",
        call. = FALSE
      )
    }
    return(list(
      command = unname(powershell),
      args = c(
        "-NoLogo",
        "-NoProfile",
        "-NonInteractive",
        "-STA",
        "-Command",
        builder_windows_picker_script(kind)
      ),
      cancel_status = integer(),
      encoding = "UTF-8"
    ))
  }
  if (identical(.system, "Darwin")) {
    return(list(
      command = "osascript",
      args = c("-e", builder_macos_picker_script(kind)),
      cancel_status = integer(),
      encoding = "UTF-8"
    ))
  }
  zenity <- .which("zenity")
  if (nzchar(zenity)) {
    args <- if (directory) {
      c(
        "--file-selection",
        "--directory",
        paste0("--title=", prompt)
      )
    } else {
      file_args <- c(
        "--file-selection",
        paste0("--title=", prompt),
        paste0("--file-filter=", contract$zenity_filter)
      )
      if (isTRUE(contract$multiple)) {
        file_args <- c(file_args, "--multiple", "--separator=\n")
      }
      file_args
    }
    return(list(
      command = unname(zenity),
      args = args,
      cancel_status = 1L,
      encoding = "UTF-8"
    ))
  }
  kdialog <- .which("kdialog")
  if (nzchar(kdialog)) {
    args <- if (directory) {
      c(
        "--getexistingdirectory",
        path.expand("~"),
        "--title",
        prompt
      )
    } else {
      file_args <- c(
        "--getopenfilename",
        path.expand("~"),
        contract$kdialog_filter,
        "--title",
        prompt
      )
      if (isTRUE(contract$multiple)) {
        file_args <- c(file_args, "--multiple", "--separate-output")
      }
      file_args
    }
    return(list(
      command = unname(kdialog),
      args = args,
      cancel_status = 1L,
      encoding = "UTF-8"
    ))
  }
  stop("No system picker is available.", call. = FALSE)
}

builder_native_picker_lines <- function(output) {
  output <- paste(as.character(output), collapse = "\n")
  if (!nzchar(output)) {
    return(character())
  }
  lines <- strsplit(gsub("\r", "", output, fixed = TRUE), "\n", fixed = TRUE)[[
    1L
  ]]
  lines[nzchar(lines)]
}

builder_native_picker_output <- function(
  status,
  output,
  errors = character(),
  cancel_status = integer()
) {
  output <- builder_native_picker_lines(output)
  errors <- builder_native_picker_lines(errors)
  if (is.null(status)) {
    status <- 0L
  }
  status <- if (length(status) == 1L && !is.na(status)) {
    as.integer(status)
  } else {
    NA_integer_
  }
  if (identical(status, 0L)) {
    marker <- builder_native_picker_cancel_marker()
    if (identical(output, marker) && !length(errors)) {
      return(character())
    }
    if (!length(output)) {
      stop(
        if (length(errors)) {
          paste(errors, collapse = "\n")
        } else {
          "The system picker exited without returning a selection."
        },
        call. = FALSE
      )
    }
    if (marker %in% output) {
      stop(
        "The system picker returned an invalid cancellation response.",
        call. = FALSE
      )
    }
    return(output)
  }
  if (
    !is.na(status) &&
      status %in% as.integer(cancel_status) &&
      !length(output) &&
      !length(errors)
  ) {
    return(character())
  }
  detail <- if (length(errors)) {
    paste(errors, collapse = "\n")
  } else {
    label <- if (is.na(status)) "unknown" else status
    paste0("The system picker exited with status ", label, ".")
  }
  stop(detail, call. = FALSE)
}

builder_native_picker_result <- function(kind, select) {
  contract <- builder_native_picker_contract(kind)
  result <- function(
    status,
    error = NULL,
    fallback = FALSE
  ) {
    value <- list(status = status, path = NULL)
    if (!is.null(error)) {
      value$error <- error
    }
    if (isTRUE(fallback)) {
      value$fallback <- TRUE
    }
    value
  }
  chosen <- tryCatch(select(), error = identity)
  if (inherits(chosen, "condition")) {
    return(result(
      "error",
      error = conditionMessage(chosen),
      fallback = TRUE
    ))
  }
  chosen <- trimws(as.character(chosen))
  chosen <- chosen[!is.na(chosen) & nzchar(chosen)]
  if (!isTRUE(contract$multiple) && length(chosen)) {
    chosen <- chosen[[1L]]
  }
  if (!length(chosen)) {
    return(result("cancelled"))
  }
  paths <- tryCatch(
    unique(normalizePath(
      path.expand(chosen),
      winslash = "/",
      mustWork = TRUE
    )),
    error = identity
  )
  valid <- !inherits(paths, "condition") &&
    if (identical(contract$type, "directory")) {
      all(dir.exists(paths))
    } else {
      all(file.exists(paths) & !dir.exists(paths))
    }
  if (!isTRUE(valid)) {
    return(result("error", error = contract$unavailable))
  }
  selected <- list(status = "selected", path = paths[[1L]])
  if (isTRUE(contract$multiple)) {
    selected$paths <- paths
  }
  selected
}

# Run the operating-system dialog outside Shiny's main R event loop. A dialog
# may stay open for minutes; only its small path result returns to the session.
builder_start_native_picker <- function(
  kind = c(
    "output_directory",
    "project_directory",
    "project_manifest",
    "dataset_files"
  ),
  .spec = NULL,
  .process_new = NULL
) {
  kind <- match.arg(kind)
  spec <- .spec %||% builder_native_picker_spec(kind)
  if (is.null(.process_new) && !requireNamespace("processx", quietly = TRUE)) {
    return(list(
      kind = kind,
      process = NULL,
      result = builder_native_picker_result(
        kind,
        function() {
          stop(
            "The processx package is required for a non-blocking system picker.",
            call. = FALSE
          )
        }
      )
    ))
  }
  process_new <- .process_new %||% processx::process$new
  process <- tryCatch(
    process_new(
      command = spec$command,
      args = spec$args,
      stdout = "|",
      stderr = "|",
      cleanup = TRUE,
      cleanup_tree = TRUE,
      windows_hide_window = TRUE,
      encoding = spec$encoding %||% ""
    ),
    error = identity
  )
  if (inherits(process, "condition")) {
    return(list(
      kind = kind,
      process = NULL,
      result = builder_native_picker_result(
        kind,
        function() stop(conditionMessage(process), call. = FALSE)
      )
    ))
  }
  list(
    kind = kind,
    process = process,
    result = NULL,
    cancel_status = spec$cancel_status %||% integer(),
    path_style = spec$path_style %||% "native"
  )
}

builder_native_picker_is_alive <- function(picker) {
  !is.null(picker$process) && isTRUE(picker$process$is_alive())
}

builder_collect_native_picker <- function(picker) {
  if (!is.null(picker$result)) {
    return(picker$result)
  }
  if (builder_native_picker_is_alive(picker)) {
    stop("The native picker is still open.", call. = FALSE)
  }
  output <- picker$process$read_all_output_lines()
  errors <- picker$process$read_all_error_lines()
  status <- picker$process$get_exit_status()
  builder_native_picker_result(picker$kind, function() {
    paths <- builder_native_picker_output(
      status,
      output,
      errors,
      picker$cancel_status %||% integer()
    )
    if (length(paths) && identical(picker$path_style, "windows")) {
      paths <- builder_wsl_picker_paths(paths)
    }
    paths
  })
}

builder_kill_native_picker <- function(picker) {
  if (builder_native_picker_is_alive(picker)) {
    tryCatch(picker$process$kill(), error = function(error) NULL)
  }
  invisible(TRUE)
}

## Resource lookup must stay in the same immutable inst/ tree as this file.
## `system.file()` is deliberately only a fallback for runtimes where io.R was
## not sourced from a verifiable <inst>/builder/io.R path (for example, a
## future namespace-loaded copy). A source checkout may coexist with an older
## installed CerebroNexus, and mixing those two trees makes fixtures depend on
## whichever package library happens to win lookup.
.builder_io_source_path <- local({
  frames <- sys.frames()
  paths <- vapply(
    rev(frames),
    function(frame) frame$ofile %||% "",
    character(1)
  )
  paths <- paths[nzchar(paths)]
  paths <- paths[
    basename(paths) == "io.R" & basename(dirname(paths)) == "builder"
  ]
  paths <- paths[file.exists(paths)]
  if (!length(paths)) {
    paths <- vapply(
      rev(frames),
      function(frame) {
        path <- get0("file", envir = frame, inherits = FALSE)
        if (is.character(path) && length(path) == 1L) path else ""
      },
      character(1)
    )
    paths <- paths[
      nzchar(paths) &
        basename(paths) == "io.R" &
        basename(dirname(paths)) == "builder" &
        file.exists(paths)
    ]
  }
  if (!length(paths)) "" else normalizePath(paths[[1L]], mustWork = TRUE)
})

.builder_example_inst_root <- local({
  if (!nzchar(.builder_io_source_path)) {
    return("")
  }
  root <- normalizePath(
    dirname(dirname(.builder_io_source_path)),
    mustWork = TRUE
  )
  expected <- normalizePath(
    file.path(root, "builder", "io.R"),
    mustWork = TRUE
  )
  if (!identical(expected, .builder_io_source_path)) "" else root
})

## The readers below are package functions; a Builder worker loads the
## package runtime only on request, so request it before calling them.
.builder_io_require_package <- function() {
  if (exists("builder_worker_require_capability", mode = "function")) {
    builder_worker_require_capability("package")
  }
  invisible(TRUE)
}

## One row per format: the extensions it claims, the package it needs, and how
## to read with it. Adding a format means adding a row.
##
## There is deliberately no `.qd` row. qs2's qdata format does not serialise S4
## at all: `qd_save()` on a Seurat object warns "Objects of type S4 are not
## supported in qdata format", writes a 38-byte container, and reading it back
## yields NULL. A `.qd` file cannot hold a Seurat object, so listing it among
## the readable formats told users something untrue. `.qs2`, from the same
## package, does work.
builder_formats <- list(
  list(
    id = "rds",
    label = "RDS",
    extensions = "rds",
    package = NULL,
    read = function(path) readRDS(path)
  ),
  list(
    id = "qs2",
    label = "qs2",
    extensions = "qs2",
    package = "qs2",
    read = function(path) qs2::qs_read(path)
  ),
  ## Still read, but no longer advertised: the qs package is archived, so the
  ## Add files hints list RDS and QS2 and warn that QS support will end.
  list(
    id = "qs",
    label = "qs",
    extensions = "qs",
    package = "qs",
    deprecated = TRUE,
    read = function(path) {
      cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
      if (length(cores) != 1L || !is.finite(cores)) {
        cores <- 1L
      }
      qs::qread(
        path,
        strict = TRUE,
        nthreads = min(16L, as.integer(cores))
      )
    }
  ),
  ## AnnData / MuData files are opened natively and arrive as a
  ## SingleCellExperiment, converted to a Seurat object by builder_read_object().
  list(
    id = "h5ad",
    label = "h5ad",
    extensions = "h5ad",
    package = "rhdf5",
    read = function(path) {
      .builder_io_require_package()
      readH5adAsSCE(path)
    }
  ),
  list(
    id = "h5mu",
    label = "h5mu",
    extensions = "h5mu",
    package = "rhdf5",
    ## protein (CITE-seq) and other modalities come along as extra features
    read = function(path) {
      .builder_io_require_package()
      readH5adAsSCE(path, extra_modalities = "all")
    }
  ),
  ## A SpatialData store is a folder, so it arrives as a .zip of it (how the
  ## SpatialData sample data sets are distributed).
  list(
    id = "spatialdata",
    label = "SpatialData (zip)",
    extensions = "zip",
    package = "arrow",
    read = function(path) {
      .builder_io_require_package()
      readSpatialDataAsSCE(path)
    }
  )
)

#' Alternative experiments that hold antibody counts (CITE-seq), whose features
#' are added to a SingleCellExperiment's own when it goes into the Builder.
#' Spike-ins and repeat elements are not.
builder_extra_feature_names <- function(object) {
  names <- tryCatch(
    SingleCellExperiment::altExpNames(object),
    error = function(e) character()
  )
  names[grepl(
    "^(adt|prot|protein|antibody|antibodies)$",
    names,
    ignore.case = TRUE
  )]
}

builder_format_capability <- function(
  format,
  .require_namespace = function(package) {
    requireNamespace(package, quietly = TRUE)
  }
) {
  if (!is.list(format)) {
    stop("A Builder format record is required.", call. = FALSE)
  }
  package <- format$package %||% NULL
  available <- is.null(package) ||
    isTRUE(tryCatch(
      .require_namespace(package),
      error = function(error) FALSE
    ))
  label <- as.character(format$label %||% format$id %||% "dataset")[[1L]]
  format$label <- label
  reason <- if (available) {
    NULL
  } else {
    paste0(
      toupper(label),
      " unavailable. Use RDS."
    )
  }
  c(
    format,
    list(
      available = available,
      unavailable_reason = reason,
      unavailable_detail = if (available) {
        NULL
      } else {
        paste0(
          "The ",
          package,
          " reader cannot load in this R environment. ",
          "Install a compatible reader, or convert the files in an R environment that can open them."
        )
      }
    )
  )
}

builder_format_capabilities <- function(
  formats = builder_formats,
  .require_namespace = function(package) {
    requireNamespace(package, quietly = TRUE)
  }
) {
  capabilities <- lapply(formats, builder_format_capability, .require_namespace)
  alternatives <- vapply(
    Filter(function(format) isTRUE(format$available), capabilities),
    function(format) toupper(format$label),
    character(1)
  )
  for (i in seq_along(capabilities)) {
    if (!isTRUE(capabilities[[i]]$available) && length(alternatives)) {
      capabilities[[i]]$unavailable_reason <- paste0(
        toupper(capabilities[[i]]$label),
        " unavailable. Use ",
        paste(alternatives, collapse = " or "),
        "."
      )
    }
  }
  capabilities
}

## Reader availability cannot change without reinstalling packages, so the
## per-file checks share one probe per process.
.builder_format_capability_cache <- new.env(parent = emptyenv())

## The probe is keyed by the format table so a changed table re-probes.
builder_cached_format_capabilities <- function(
  formats = builder_formats,
  .probe = builder_format_capabilities
) {
  cache <- .builder_format_capability_cache
  if (is.null(cache$value) || !identical(cache$formats, formats)) {
    cache$value <- .probe(formats)
    cache$formats <- formats
  }
  cache$value
}

builder_available_formats <- function(
  capabilities = builder_cached_format_capabilities()
) {
  Filter(function(format) isTRUE(format$available), capabilities)
}

## Deprecated formats are flagged in builder_formats; records built from bare
## ids (tests, older callers) inherit the flag through their id.
.builder_format_deprecated <- function(format) {
  if (isTRUE(format$deprecated)) {
    return(TRUE)
  }
  id <- tolower(as.character(format$id %||% "")[1L])
  known <- get0("builder_formats", inherits = TRUE)
  any(vapply(
    known %||% list(),
    function(entry) {
      isTRUE(entry$deprecated) && identical(tolower(entry$id), id)
    },
    logical(1)
  ))
}

## Labels shown under "Add files": readable, not deprecated, RDS first.
builder_advertised_format_labels <- function(
  formats = builder_available_formats()
) {
  formats <- Filter(
    function(format) {
      is.list(format) &&
        !identical(format$available, FALSE) &&
        !.builder_format_deprecated(format)
    },
    formats %||% list()
  )
  labels <- unique(toupper(vapply(
    formats,
    function(format) {
      as.character(format$label %||% format$id %||% "")[1L]
    },
    character(1)
  )))
  labels <- labels[!is.na(labels) & nzchar(labels)]
  c(labels[labels == "RDS"], sort(labels[labels != "RDS"]))
}

## A short notice for readable formats that are on their way out.
builder_deprecated_format_note_ui <- function(
  formats = builder_available_formats()
) {
  deprecated <- Filter(
    function(format) {
      is.list(format) &&
        !identical(format$available, FALSE) &&
        .builder_format_deprecated(format)
    },
    formats %||% list()
  )
  if (!length(deprecated)) {
    return(NULL)
  }
  labels <- unique(toupper(vapply(
    deprecated,
    function(format) as.character(format$label %||% format$id)[1L],
    character(1)
  )))
  replacements <- builder_advertised_format_labels(formats)
  shiny::tags$small(
    class = "builder-format-deprecated",
    paste0(
      paste(labels, collapse = " and "),
      " files are deprecated and support will end soon.",
      if (length(replacements)) {
        paste0(" Save as ", paste(replacements, collapse = " or "), ".")
      } else {
        ""
      }
    )
  )
}

builder_path_format_capability <- function(
  path,
  capabilities = builder_cached_format_capabilities()
) {
  extension <- tolower(tools::file_ext(path %||% ""))
  matches <- Filter(
    function(format) extension %in% tolower(format$extensions %||% character()),
    capabilities
  )
  if (length(matches)) matches[[1L]] else NULL
}

#' Which formats can be read right now.
#'
#' @return A data.frame with one row per format: id, label, extensions,
#'   package, available.
#' Pick the format for a path, by extension.
#'
#' @return The format entry, or `NULL` when the extension is not one we claim.
builder_match_format <- function(path) {
  ext <- tolower(tools::file_ext(path))
  for (f in builder_formats) {
    if (ext %in% f$extensions) {
      return(f)
    }
  }
  NULL
}

#' Read a serialised object, choosing the reader by file extension.
#'
#' @param path Path to the file.
#'
#' @return A list with `object` on success, or `error` with a sentence saying
#'   what went wrong. Never throws: the caller is a page, not a script.
builder_read_object <- function(path) {
  if (!nzchar(path) || !file.exists(path)) {
    return(list(error = "File not found."))
  }
  if (dir.exists(path)) {
    return(list(error = "This path is a directory, not a file."))
  }

  fmt <- builder_match_format(path)
  if (is.null(fmt)) {
    known <- paste(
      unique(unlist(lapply(builder_formats, `[[`, "extensions"))),
      collapse = ", "
    )
    return(list(
      error = paste0(
        "Unsupported .",
        tools::file_ext(path),
        " extension. Supported formats: ",
        known,
        "."
      )
    ))
  }

  if (!is.null(fmt$package) && !requireNamespace(fmt$package, quietly = TRUE)) {
    return(list(
      error = paste0(
        "Reading .",
        tools::file_ext(path),
        " requires the ",
        fmt$package,
        " package. Run install.packages(\"",
        fmt$package,
        "\") or save the object as .rds first."
      )
    ))
  }

  obj <- try(fmt$read(path), silent = TRUE)
  if (inherits(obj, "try-error")) {
    return(list(
      error = paste0(
        "Could not read with ",
        fmt$label,
        ": ",
        conditionMessage(attr(obj, "condition"))
      )
    ))
  }
  if (methods::is(obj, "SingleCellExperiment")) {
    ## Everything downstream is Seurat-shaped; a SingleCellExperiment enters
    ## through this one conversion and keeps its results in @misc.
    .builder_io_require_package()
    obj <- try(
      convertSCEToSeurat(
        obj,
        extra_features = builder_extra_feature_names(obj)
      ),
      silent = TRUE
    )
    if (inherits(obj, "try-error")) {
      return(list(
        error = paste0(
          "Could not convert the SingleCellExperiment: ",
          conditionMessage(attr(obj, "condition"))
        )
      ))
    }
  }
  if (!inherits(obj, "Seurat")) {
    return(list(
      error = paste0(
        "The file contains a ",
        class(obj)[1],
        " object, not a Seurat object."
      )
    ))
  }
  list(object = obj, format = fmt$label)
}

## ---------------------------------------------------------------------------
## Example data, so the tool can be tried before anyone has a file to hand
## ---------------------------------------------------------------------------

.builder_example_path <- function(relative) {
  root <- .builder_example_inst_root
  source_root <- Sys.getenv("CEREBRO_PACKAGE_SOURCE", unset = "")
  if (!nzchar(root) && nzchar(source_root)) {
    candidate <- file.path(source_root, "inst")
    if (dir.exists(candidate)) {
      root <- candidate
    }
  }
  if (!nzchar(root)) {
    root <- system.file(package = "CerebroNexus")
  }
  if (!nzchar(root)) {
    return("")
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  path <- file.path(root, relative)
  if (!file.exists(path) && !dir.exists(path)) {
    return("")
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  inside <- identical(path, root) ||
    startsWith(path, paste0(root, "/"))
  if (!inside) "" else path
}


#' Construct one immutable Builder example catalog record.
builder_example_record <- function(
  id,
  label,
  detail,
  provenance,
  serialized_path,
  make = NULL,
  expected_manifest = character(),
  expected_dispositions = character(),
  expected_pages = character(),
  expected_supporting_content = character(),
  histology_images = list(),
  gallery_visible = TRUE
) {
  require_string <- function(value, name, nonempty = TRUE) {
    valid <- is.character(value) && length(value) == 1L && !is.na(value)
    if (isTRUE(nonempty)) {
      valid <- valid && nzchar(value)
    }
    if (!valid) {
      qualifier <- if (isTRUE(nonempty)) "a non-empty" else "a single"
      stop("`", name, "` must be ", qualifier, " string.", call. = FALSE)
    }
  }
  for (name in c("id", "label", "detail", "provenance")) {
    require_string(get(name), name)
  }
  allowed_provenance <- c("real", "synthetic")
  if (!(provenance %in% allowed_provenance)) {
    stop(
      "`provenance` must be one of: ",
      paste(allowed_provenance, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  require_string(serialized_path, "serialized_path", nonempty = FALSE)
  if (
    !is.character(expected_manifest) ||
      anyNA(expected_manifest) ||
      any(!nzchar(expected_manifest)) ||
      anyDuplicated(expected_manifest) > 0L ||
      !is.null(names(expected_manifest))
  ) {
    stop(
      "`expected_manifest` must contain unique non-empty strings without names.",
      call. = FALSE
    )
  }
  disposition_names <- names(expected_dispositions)
  if (
    length(expected_dispositions) > 0L &&
      (is.null(disposition_names) ||
        anyNA(disposition_names) ||
        any(!nzchar(disposition_names)) ||
        anyDuplicated(disposition_names) > 0L)
  ) {
    stop(
      "Use named `expected_dispositions` with unique non-empty names.",
      call. = FALSE
    )
  }
  allowed_dispositions <- c("preserved", "converted")
  if (
    !is.character(expected_dispositions) ||
      anyNA(expected_dispositions) ||
      any(!nzchar(expected_dispositions)) ||
      any(!(expected_dispositions %in% allowed_dispositions))
  ) {
    stop(
      "`expected_dispositions` must use only: ",
      paste(allowed_dispositions, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (!identical(expected_manifest, disposition_names %||% character())) {
    stop(
      "`expected_manifest` must exactly match disposition names.",
      call. = FALSE
    )
  }
  for (name in c("expected_pages", "expected_supporting_content")) {
    value <- get(name)
    if (
      !is.character(value) ||
        anyNA(value) ||
        any(!nzchar(value)) ||
        anyDuplicated(value) > 0L ||
        !is.null(names(value))
    ) {
      stop(
        "`",
        name,
        "` must contain unique non-empty strings without names.",
        call. = FALSE
      )
    }
  }
  if (
    !is.list(histology_images) ||
      (length(histology_images) &&
        (is.null(names(histology_images)) ||
          anyNA(names(histology_images)) ||
          any(!nzchar(names(histology_images))) ||
          anyDuplicated(names(histology_images)) > 0L))
  ) {
    stop(
      "`histology_images` must be a named list with unique non-empty IDs.",
      call. = FALSE
    )
  }
  for (image_id in names(histology_images)) {
    image <- histology_images[[image_id]]
    required <- c("id", "label", "stain", "path", "section_id", "fov_ids")
    if (!is.list(image) || !all(required %in% names(image))) {
      stop(
        "`histology_images` entries must declare id, label, stain, path, ",
        "section_id, and fov_ids.",
        call. = FALSE
      )
    }
    if (!identical(image$id, image_id)) {
      stop(
        "`histology_images` entry IDs must match their list names.",
        call. = FALSE
      )
    }
    for (name in c("id", "label", "stain", "path", "section_id")) {
      require_string(image[[name]], paste0("histology_images$", name))
    }
    if (
      !is.character(image$fov_ids) ||
        !length(image$fov_ids) ||
        anyNA(image$fov_ids) ||
        any(!nzchar(image$fov_ids)) ||
        anyDuplicated(image$fov_ids) > 0L
    ) {
      stop(
        "`histology_images` entries must declare unique non-empty `fov_ids`.",
        call. = FALSE
      )
    }
    if (!image$path %in% expected_supporting_content) {
      stop(
        "Each `histology_images` path must appear in ",
        "`expected_supporting_content`.",
        call. = FALSE
      )
    }
  }
  if (
    !is.logical(gallery_visible) ||
      length(gallery_visible) != 1L ||
      is.na(gallery_visible)
  ) {
    stop("`gallery_visible` must be TRUE or FALSE.", call. = FALSE)
  }
  if (is.null(make)) {
    make <- local({
      path <- serialized_path
      function() {
        if (!nzchar(path) || !file.exists(path)) {
          return(list(error = "The package example object was not found."))
        }
        # A saved Project keeps a copy of this file, so its content hash is
        # the source identity a dataset cache built from the example records.
        source_md5 <- unname(tools::md5sum(path))
        loaded <- builder_read_object(path)
        if (!is.null(loaded$error)) {
          return(loaded)
        }
        loaded$format <- "Built-in example"
        loaded$source_file <- basename(path)
        loaded$source_md5 <- source_md5
        loaded
      }
    })
  }
  if (!is.function(make)) {
    stop("`make` must be a function.", call. = FALSE)
  }
  list(
    id = id,
    label = label,
    detail = detail,
    provenance = provenance,
    make = make,
    serialized_path = serialized_path,
    expected_manifest = expected_manifest,
    expected_dispositions = expected_dispositions,
    expected_pages = expected_pages,
    expected_supporting_content = expected_supporting_content,
    histology_images = histology_images,
    gallery_visible = gallery_visible
  )
}

#' Stable, offline examples covering every Builder content family.
builder_example_catalog <- function() {
  fixture <- function(...) {
    .builder_example_path(
      file.path("builder", "fixtures", ...)
    )
  }
  core <- function(reductions = "umap", groups = "preserved") {
    values <- stats::setNames(
      rep("preserved", 5L + length(reductions)),
      c(
        "dataset_identity",
        "expression",
        "metadata",
        "groups",
        paste0("reduction:", reductions),
        "projection"
      )
    )
    values[["groups"]] <- groups
    values
  }
  with_content <- function(core_dispositions, ...) {
    c(core_dispositions, unlist(list(...), use.names = TRUE))
  }
  record <- function(..., expected_dispositions = character()) {
    expected_dispositions <- c(
      expected_dispositions,
      metadata_policy = "preserved"
    )
    builder_example_record(
      ...,
      expected_manifest = names(expected_dispositions),
      expected_dispositions = expected_dispositions
    )
  }
  histology_images <- list(
    section_a_1_he = list(
      id = "section_a_1_he",
      label = "H&E",
      stain = "H&E",
      path = "section_a_1_he.png",
      section_id = "section_a_1",
      fov_ids = "section_a_1_fov_1"
    ),
    section_a_1_dapi = list(
      id = "section_a_1_dapi",
      label = "DAPI",
      stain = "DAPI",
      path = "section_a_1_dapi.png",
      section_id = "section_a_1",
      fov_ids = "section_a_1_fov_1"
    ),
    section_a_2_he = list(
      id = "section_a_2_he",
      label = "H&E",
      stain = "H&E",
      path = "section_a_2_he.png",
      section_id = "section_a_2",
      fov_ids = "section_a_2_fov_1"
    ),
    section_a_2_dapi = list(
      id = "section_a_2_dapi",
      label = "DAPI",
      stain = "DAPI",
      path = "section_a_2_dapi.png",
      section_id = "section_a_2",
      fov_ids = "section_a_2_fov_1"
    ),
    section_b_1_he = list(
      id = "section_b_1_he",
      label = "H&E",
      stain = "H&E",
      path = "section_b_1_he.png",
      section_id = "section_b_1",
      fov_ids = "section_b_1_fov_1"
    ),
    section_b_1_if = list(
      id = "section_b_1_if",
      label = "IF",
      stain = "IF",
      path = "section_b_1_if.jpg",
      section_id = "section_b_1",
      fov_ids = "section_b_1_fov_1"
    ),
    section_b_1_pas = list(
      id = "section_b_1_pas",
      label = "PAS",
      stain = "PAS",
      path = "section_b_1_pas.png",
      section_id = "section_b_1",
      fov_ids = "section_b_1_fov_1"
    )
  )
  trekker_histology_images <- lapply(seq_len(4L), function(index) {
    id <- sprintf("tissue_%02d_he", index)
    list(
      id = id,
      label = paste("Tissue", index, "H&E"),
      stain = "H&E",
      path = paste0(id, ".jpg"),
      section_id = "slice1",
      fov_ids = "slice1"
    )
  })
  names(trekker_histology_images) <- vapply(
    trekker_histology_images,
    `[[`,
    character(1),
    "id"
  )
  alignment_histology_images <- list(
    arrow_background = list(
      id = "arrow_background",
      label = "Arrow background",
      stain = "Synthetic alignment guide",
      path = "arrow_background.png",
      section_id = "arrow_fov",
      fov_ids = "arrow_fov"
    )
  )
  complete_viewer_data <- record(
    "complete_viewer_data",
    "Complete Viewer demo",
    paste(
      "A synthetic Seurat demo for exploring Builder, with projections,",
      "Spatial sections, Trekker, and supported attachments"
    ),
    "synthetic",
    fixture("complete_viewer", "complete_viewer_data.rds"),
    expected_dispositions = with_content(
      core(c("pca", "umap", "tsne")),
      marker_genes = "preserved",
      most_expressed_genes = "preserved",
      mean_expression = "preserved",
      enriched_pathways = "preserved",
      trajectory = "preserved",
      gene_lists = "preserved",
      extra_material = "preserved",
      immune_repertoire = "preserved",
      hla_tcr_motifs = "preserved",
      hla = "preserved",
      spatial = "preserved",
      trekker = "preserved"
    ),
    expected_pages = c(
      "marker_genes",
      "most_expressed_genes",
      "enriched_pathways",
      "extra_material",
      "immune_repertoire",
      "trajectory",
      "spatial",
      "trekker",
      "hla_tcr_motifs"
    ),
    expected_supporting_content = c(
      "marker_t_cells.csv",
      "marker_b_cells.tsv",
      "marker_panels.xlsx",
      "supplementary_clinical.csv",
      "supplementary_tables.xlsx",
      unname(vapply(
        histology_images,
        `[[`,
        character(1),
        "path"
      ))
    ),
    histology_images = histology_images
  )
  trekker_spatial <- record(
    "trekker_spatial",
    "Trekker spatial demo",
    paste(
      "A synthetic four-tissue Trekker-style Seurat object with a SlideSeq",
      "spatial section and four H&E images"
    ),
    "synthetic",
    fixture("trekker_spatial", "trekker_4_tissues.qs2"),
    expected_dispositions = with_content(
      core(c("pca", "umap", "SPATIAL")),
      spatial = "preserved"
    ),
    expected_pages = "spatial",
    expected_supporting_content = unname(vapply(
      trekker_histology_images,
      `[[`,
      character(1),
      "path"
    )),
    histology_images = trekker_histology_images
  )
  spatial_alignment <- record(
    "spatial_alignment",
    "Spatial alignment demo",
    paste(
      "A synthetic arrow-shaped Spatial section with an automatically",
      "aligned directional background image"
    ),
    "synthetic",
    fixture("spatial_alignment", "arrow_spatial.rds"),
    expected_dispositions = with_content(
      core("umap"),
      spatial = "preserved"
    ),
    expected_pages = "spatial",
    expected_supporting_content = "arrow_background.png",
    histology_images = alignment_histology_images
  )
  list(
    complete_viewer_data = complete_viewer_data,
    trekker_spatial = trekker_spatial,
    spatial_alignment = spatial_alignment
  )
}

#' Apply non-image attachment presets for a gallery example.
builder_example_configure_entry <- function(entry, record, object) {
  example_id <- record$id %||% NULL
  if (identical(example_id, "spatial_alignment")) {
    if (
      !is.list(entry) ||
        !is.list(entry$settings) ||
        !methods::is(object, "Seurat") ||
        !"arrow_fov" %in% SeuratObject::Images(object)
    ) {
      stop(
        "The Spatial alignment example could not be configured.",
        call. = FALSE
      )
    }
    image_path <- file.path(
      dirname(record$serialized_path),
      record$histology_images$arrow_background$path
    )
    image <- builder_read_image(image_path, basename(image_path))
    if (!is.null(image$error)) {
      stop(image$error, call. = FALSE)
    }
    coordinates <- SeuratObject::GetTissueCoordinates(object[["arrow_fov"]])
    bounds <- list(
      xmin = min(coordinates$x),
      xmax = max(coordinates$x),
      ymin = min(coordinates$y),
      ymax = max(coordinates$y)
    )
    parameters <- utils::modifyList(
      builder_alignment_defaults(),
      ## The cell coordinates are the background's pixel positions
      ## (x = column, y = row) and image rows point down, so flipping y
      ## registers the picture exactly; no rotation, offset or scale.
      list(
        flip_y = TRUE,
        image_opacity = 0.65,
        point_opacity = 0.6,
        point_size = 7
      )
    )
    alignment <- builder_alignment_record(
      source = list(
        name = basename(image_path),
        type = image$mime,
        size = image$bytes
      ),
      base_bounds = builder_alignment_fit_bounds(
        bounds,
        image$source_dimensions
      ),
      parameters = parameters,
      section = list(id = "arrow_fov", kind = "spatial"),
      source_path = image$source_path
    )
    alignment$viewport_bounds <- bounds
    alignment$image_label <- "Arrow background"
    facts <- intersect(
      names(image),
      c(
        "bytes",
        "width",
        "height",
        "source_width",
        "source_height",
        "extent_width",
        "extent_height",
        "display_width",
        "display_height",
        "source_content_md5"
      )
    )
    alignment[facts] <- image[facts]
    entry$settings$images <- list(
      arrow_fov = list(`Arrow background` = alignment)
    )
    entry$settings$spatial_point_appearance <- list(
      arrow_fov = list(point_opacity = 0.6, point_size = 7)
    )
    return(entry)
  }
  if (!identical(example_id, "complete_viewer_data")) {
    return(entry)
  }
  if (
    !is.list(entry) ||
      !is.list(entry$settings) ||
      !methods::is(object, "Seurat")
  ) {
    stop("The complete Viewer example could not be configured.", call. = FALSE)
  }
  fixture_dir <- dirname(record$serialized_path)
  sidecars <- record$expected_supporting_content
  sidecars <- sidecars[grepl("^(marker_|supplementary_)", sidecars)]
  sidecars <- file.path(fixture_dir, sidecars)
  missing <- sidecars[!file.exists(sidecars)]
  if (length(missing)) {
    stop(
      "The complete Viewer example is missing bundled attachments: ",
      paste(basename(missing), collapse = ", "),
      call. = FALSE
    )
  }

  marker_paths <- sidecars[grepl("^marker_", basename(sidecars))]
  sources <- builder_marker_import_inventory(
    marker_paths,
    basename(marker_paths)
  )
  known_levels <- entry$levels$cell_type %||% levels(object$cell_type)
  draft <- builder_marker_import_new_draft(
    id = "fixture-marker-import",
    method = "Fixture sidecars",
    group = "cell_type",
    sources = sources,
    known_levels = known_levels,
    existing_methods = names(object@misc$marker_genes)
  )
  for (source in draft$sources) {
    value <- if (identical(source$mapping, "multiple")) {
      "cell_type"
    } else {
      source$cluster
    }
    draft <- builder_marker_import_confirm_source(
      draft,
      source$id,
      source$mapping,
      value
    )
  }
  if (!isTRUE(draft$validation$ready)) {
    stop("The bundled Marker gene tables could not be mapped.", call. = FALSE)
  }
  entry$settings$marker_imports <- list(fixture_sidecars = draft)
  entry$settings$analyses <- setdiff(
    entry$settings$analyses %||% character(),
    "marker_genes"
  )

  table_paths <- sidecars[grepl("^supplementary_", basename(sidecars))]
  table_records <- unname(unlist(
    lapply(table_paths, function(path) {
      filename <- basename(path)
      records <- builder_table_inventory(path, filename)
      if (!is.null(records$error)) {
        stop(records$error, call. = FALSE)
      }
      lapply(records, function(record) {
        record$file_name <- filename
        record$file_type <- toupper(tools::file_ext(filename))
        record$file_size <- as.numeric(file.info(path)$size[[1L]])
        record$source_path <- path
        record
      })
    }),
    recursive = FALSE
  ))
  tables <- builder_materialize_tables(table_records)
  names(tables) <- make.unique(vapply(tables, `[[`, character(1), "name"))
  entry$settings$tables <- tables
  entry$settings$cell_cycle_columns <- "Phase"
  entry$settings$included_trajectories <- list(monocle2 = "lineage")
  entry$settings$default_trajectory <- list(
    method = "monocle2",
    name = "lineage"
  )
  entry
}

builder_apply_example_import_preset <- function(
  entry,
  example_id,
  catalog = builder_example_catalog()
) {
  if (
    !is.character(example_id) ||
      length(example_id) != 1L ||
      is.na(example_id) ||
      !nzchar(example_id)
  ) {
    return(entry)
  }
  record <- catalog[[example_id]] %||% NULL
  if (!is.list(record) || !is.function(record$make)) {
    stop("The selected example is unavailable.", call. = FALSE)
  }
  loaded <- record$make()
  if (!is.list(loaded) || !methods::is(loaded$object, "Seurat")) {
    stop("The selected example could not be loaded.", call. = FALSE)
  }
  builder_example_configure_entry(entry, record, loaded$object)
}

#' Static first-paint directory for the example picker.
builder_example_directory <- local({
  directory <- list(
    list(
      id = "complete_viewer_data",
      label = "Load example datasets",
      detail = paste(
        "Try three datasets with expression, trajectories and spatial images."
      ),
      source = "builder/fixtures/complete_viewer/complete_viewer_data.rds",
      examples = list(
        list(id = "complete_viewer_data", label = "Complete Viewer demo"),
        list(id = "trekker_spatial", label = "Trekker spatial demo"),
        list(id = "spatial_alignment", label = "Spatial alignment demo")
      )
    )
  )
  names(directory) <- vapply(directory, `[[`, character(1), "id")
  function() directory
})

#' Gallery projection of the stable example catalog.
builder_examples <- function() {
  lapply(
    Filter(
      function(record) isTRUE(record$gallery_visible),
      builder_example_catalog()
    ),
    function(record) record[c("id", "label", "detail", "make")]
  )
}
