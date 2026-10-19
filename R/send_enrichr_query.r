#' @title
#' Gene enrichment using Enrichr.
#'
#' @description
#' Gene enrichment using Enrichr.
#'
#' @param genes Gene names or dataframe of gene names in first column and a
#' score between 0 and 1 in the other.
#' @param databases Databases to search.
#' @param URL_API URL to send requests to (Enrichr API).
#' See https://maayanlab.cloud/Enrichr/#stats for available databases.
#'
#' @return
#' Returns a data frame of enrichment terms, p-values, ...
#'
#' @author Wajid Jawaid, modified by Roman Hillje
#'
.send_enrichr_query <- function(
  genes,
  databases = NULL,
  URL_API = NULL
) {
  ## check that httr is installed before contacting the Enrichr API
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop(
      "Package 'httr' is required for this function. Install it with install.packages(\"httr\").",
      call. = FALSE
    )
  }

  ## A bounded request budget applies independently to submission, sharing and
  ## every database export. HTTP errors must not reach the TSV parser.
  request_timeout <- httr::timeout(60)

  ## check input format
  ## ... input is a vector with length > 0 and not all empty genes
  if (
    is.vector(genes) &&
      length(genes) > 0 &&
      !all(genes == '')
  ) {
    ## send request with gene names
    temp <- httr::POST(
      url = paste0(URL_API, "/enrich"),
      request_timeout,
      body = list(
        list = paste(genes, collapse = '\n')
      )
    )

    ## ... input is a data frame
  } else if (is.data.frame(genes)) {
    ## send request with gene names and scores
    temp <- httr::POST(
      url = paste0(URL_API, "/enrich"),
      request_timeout,
      body = list(
        list = paste(paste(genes[, 1], genes[, 2], sep = ','), collapse = '\n')
      )
    )

    ## ... none of the above
  } else {
    warning(
      paste0(
        'genes must be a non-empty vector of gene names or a dataframe ',
        'with genes and score.'
      )
    )
    return(invisible(NULL))
  }
  httr::stop_for_status(temp, "submit genes to Enrichr")

  ##
  shared <- httr::GET(url = paste0(URL_API, "/share"), request_timeout)
  httr::stop_for_status(shared, "share the Enrichr query")

  ##
  enrichr_export_fun <- function(x) {
    ##
    r <- httr::GET(
      url = paste0(URL_API, "/export"),
      request_timeout,
      query = list(
        file = 'API',
        backgroundType = x
      )
    )
    httr::stop_for_status(r, paste("export Enrichr database", x))

    ##
    r <- gsub('&#39;', "'", intToUtf8(r$content))

    ##
    tc <- textConnection(r)
    on.exit(close(tc), add = TRUE)

    ##
    r <- utils::read.table(
      tc,
      sep = '\t',
      header = TRUE,
      quote = '',
      comment.char = '',
      stringsAsFactors = FALSE
    )

    ##
    ##
    return(r)
  }
  future.apply::future_sapply(
    databases,
    enrichr_export_fun,
    USE.NAMES = TRUE,
    simplify = FALSE
  )
}
