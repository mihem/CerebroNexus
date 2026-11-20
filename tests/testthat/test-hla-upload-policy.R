hla_upload_policy_server <- function(options) {
  force(options)
  function(input, output, session) {
    scope <- environment()
    Cerebro.options <- options
    stored_typing <- hla_normalize_typing(
      list(sample_1 = "HLA-A*01:01"),
      source_type = "genotyped"
    )
    getHLATyping <- function() stored_typing
    read_count <- 0L
    read_typing <- hla_read_typing_file
    hla_read_typing_file <- function(...) {
      read_count <<- read_count + 1L
      read_typing(...)
    }
    viewer_server_load(
      viewer_server_statements("utility_functions.R"),
      scope,
      names = "viewerUploadsEnabled"
    )
    viewer_server_load(
      viewer_server_statements("hla_tcr_motifs", "data.R"),
      scope,
      names = c(
        "hla_stored_typing",
        "hla_session_typing",
        "hla_active_typing",
        "hla_has_typing"
      )
    )
    viewer_server_load(
      viewer_server_statements("hla_tcr_motifs", "data_qc.R"),
      scope,
      names = c(
        "output$hla_data_qc_ui",
        "output$hla_active_source_ui",
        "output$hla_download_normalized"
      ),
      events = c("hla_upload", "hla_clear_session")
    )
  }
}

test_that("closed and unspecified modes hide HLA override controls, not stored data", {
  for (options in list(
    list(mode = "closed"),
    list(),
    NULL,
    list(mode = TRUE),
    list(mode = "unexpected")
  )) {
    shiny::testServer(hla_upload_policy_server(options), {
      html <- output$hla_data_qc_ui$html
      for (id in c(
        "hla_upload",
        "hla_upload_source_type",
        "hla_clear_session",
        "hla_download_template"
      )) {
        expect_no_match(html, paste0('id="', id, '"'), fixed = TRUE)
      }
      expect_no_match(html, "Stored HLA typing", fixed = TRUE)
      expect_no_match(html, "HLA uploads are disabled", fixed = TRUE)
      coverage <- xml2::xml_find_first(
        xml2::read_html(html),
        '//*[@id="hla_coverage_table"]'
      )
      coverage_column <- xml2::xml_parent(coverage)
      expect_identical(xml2::xml_attr(coverage_column, "class"), "col-sm-12")
      expect_length(xml2::xml_children(xml2::xml_parent(coverage_column)), 1L)
      for (id in c(
        "hla_coverage_table",
        "hla_normalized_preview",
        "hla_donor_mapping_preview",
        "hla_qc_ui",
        "hla_download_normalized"
      )) {
        expect_match(html, paste0('id="', id, '"'), fixed = TRUE)
      }
      expect_true(hla_has_typing())
      expect_identical(hla_active_typing(), stored_typing)
      expect_match(output$hla_active_source_ui$html, "stored .crb", fixed = TRUE)
      downloaded <- utils::read.csv(
        output$hla_download_normalized,
        na.strings = ""
      )
      expect_identical(downloaded$allele, stored_typing$allele)
      expect_identical(downloaded$source_type, stored_typing$source_type)
    })
  }
})

test_that("closed and unspecified modes reject forged HLA uploads before reading", {
  path <- withr::local_tempfile()
  writeLines(c("sample,allele", "sample_1,HLA-A*02:01"), path)
  for (options in list(
    list(mode = "closed"),
    list(),
    NULL,
    list(mode = TRUE),
    list(mode = "unexpected")
  )) {
    shiny::testServer(hla_upload_policy_server(options), {
      session$setInputs(
        hla_upload_source_type = "genotyped",
        hla_upload = data.frame(name = "typing.csv", datapath = path)
      )
      expect_identical(scope$read_count, 0L)
      expect_null(hla_session_typing())
      expect_identical(hla_active_typing(), stored_typing)
    })
  }
})

test_that("closed mode also ignores forged clear-override events", {
  shiny::testServer(hla_upload_policy_server(list(mode = "closed")), {
    hla_session_typing(stored_typing)
    session$setInputs(hla_clear_session = 1)
    expect_identical(hla_session_typing(), stored_typing)
  })
})

test_that("open mode keeps CSV and TSV HLA overrides session-only and clearable", {
  for (extension in c("csv", "tsv")) {
    path <- withr::local_tempfile()
    utils::write.table(
      data.frame(sample = "sample_1", allele = "A*02:01"),
      path,
      sep = if (extension == "tsv") "\t" else ",",
      row.names = FALSE,
      quote = FALSE
    )
    shiny::testServer(hla_upload_policy_server(list(mode = "open")), {
      html <- output$hla_data_qc_ui$html
      coverage <- xml2::xml_find_first(
        xml2::read_html(html),
        '//*[@id="hla_coverage_table"]'
      )
      coverage_column <- xml2::xml_parent(coverage)
      expect_identical(xml2::xml_attr(coverage_column, "class"), "col-sm-6")
      expect_length(xml2::xml_children(xml2::xml_parent(coverage_column)), 2L)
      for (id in c(
        "hla_upload",
        "hla_upload_source_type",
        "hla_clear_session",
        "hla_download_template"
      )) {
        expect_match(html, paste0('id="', id, '"'), fixed = TRUE)
      }
      session$setInputs(
        hla_upload_source_type = "imputed",
        hla_upload = data.frame(
          name = paste0("typing.", extension),
          datapath = path
        )
      )
      expect_identical(scope$read_count, 1L)
      expect_identical(hla_session_typing()$allele, "HLA-A*02:01")
      expect_identical(hla_active_typing()$source_type, "imputed")
      expect_identical(hla_active_typing()$typing_method, "session upload")
      expect_identical(hla_stored_typing(), stored_typing)
      expect_match(
        output$hla_active_source_ui$html,
        "session override",
        fixed = TRUE
      )
      session$setInputs(hla_clear_session = 1)
      expect_null(hla_session_typing())
      expect_identical(hla_active_typing(), stored_typing)
    })
  }
})

test_that("HLA empty states suggest uploads only in explicit open mode", {
  for (options in list(
    list(mode = "closed"),
    list(),
    NULL,
    list(mode = TRUE),
    list(mode = "unexpected"),
    list(mode = "open")
  )) {
    shiny::testServer(
      function(input, output, session) {
        scope <- environment()
        Cerebro.options <- options
        hla_active_typing <- hla_session_typing <- function() NULL
        hla_has_typing <- function() FALSE
        hla_ir_samples <- function() character()
        hla_celltype_col <- function() NA_character_
        hla_association_page_requested <- function() TRUE
        viewer_server_load(
          viewer_server_statements("utility_functions.R"),
          scope,
          names = "viewerUploadsEnabled"
        )
        viewer_server_load(
          viewer_server_statements("hla_tcr_motifs", "settings.R"),
          scope,
          names = "output$hla_status_ui"
        )
        viewer_server_load(
          viewer_server_statements("hla_tcr_motifs", "associations.R"),
          scope,
          names = "output$hla_associations_ui"
        )
      },
      {
        text <- vapply(
          c("hla_status_ui", "hla_associations_ui"),
          function(id) xml2::xml_text(xml2::read_html(output[[id]]$html)),
          character(1)
        )
        expect_match(
          text[["hla_status_ui"]],
          "Motif network uses cell-type / sample colouring only.",
          fixed = TRUE
        )
        for (message in text) {
          if (identical(Cerebro.options[["mode"]], "open")) {
            expect_match(message, "Upload HLA typing in Data & QC", fixed = TRUE)
          } else {
            expect_no_match(message, "upload|Data & QC|open mode", ignore.case = TRUE)
            expect_match(
              message,
              "No HLA typing is stored in this data set.",
              fixed = TRUE
            )
          }
        }
        if (!identical(Cerebro.options[["mode"]], "open")) {
          expect_match(
            text[["hla_associations_ui"]],
            "HLA association analysis is unavailable.",
            fixed = TRUE
          )
        }
      }
    )
  }
})
