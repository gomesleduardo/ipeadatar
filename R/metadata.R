#' @title Metadata for Requested Series
#'
#' @description Returns metadata information for requested Ipeadata series.
#'
#' @param code A character vector of Ipeadata series codes.
#'
#' @param language A character string specifying the language. Available
#'   options are English (`"en"`, default) and Brazilian Portuguese (`"br"`).
#'
#' @param label Logical. If `TRUE` (default), variable labels are added to
#'   the output data frame using `sjlabelled::set_label()`.
#'
#' @param quiet Logical. If `FALSE` (default), a progress bar is displayed.
#'
#' @return A data frame containing metadata for the requested Ipeadata series,
#'   including code, name, comments, last update, theme, source, frequency,
#'   unit, multiplier, status, subject code, and country code.
#'
#' @examples
#' \donttest{
#' meta <- metadata(code = "PRECOS12_IPCA12")
#' }
#'
#' @note The original language of series names and comments is preserved.
#'   Ipeadata codes may be obtained using [available_series()].
#'
#' @seealso [available_series()], [available_subjects()],
#'   [available_territories()]
#'
#' @references This package uses the Ipeadata API:
#'   \url{https://www.ipeadata.gov.br/}
#'
#' @export
metadata <- function(code, language = c("en", "br"), label = TRUE, quiet = FALSE) {
  
  # Check language arg
  language <- match.arg(language)
  
  # Output
  metadata <- tibble::tibble()
  
  # Progress Bar settings
  n <- length(code)
  pb <- NULL
  
  if (!quiet && n >= 2) {
    
    cli::cli_alert_info(
      "Requesting Ipeadata API {.url https://www.ipeadata.gov.br/api/}"
    )
    
    pb <- cli::cli_progress_bar("Processing", total = n)

  }
  
  update.step <- max(2L, floor(n / 100))
  
  # Test internet connection
  if (curl::has_internet()) {
    
    Sys.sleep(.01)
    
    tryCatch({
      
      # Retrieve metadata for each series
      for (i in seq_len(n)) {
        
        # Check
        code0 <- gsub(" ", "_", toupper(code[i]))
        
        # URL for metadata
        url <- paste0(
          "https://www.ipeadata.gov.br/api/odata4/Metadados('",
          code0,
          "')"
        )
        
        # Extract from JSON
        Sys.sleep(.01)
        metadata.aux <- jsonlite::fromJSON(url, flatten = TRUE)[[2]]
        
        if (nrow(metadata.aux) > 0) {
          
          ## Starting: Transform to tbl >
          ##           Select variables
          metadata.aux <- metadata.aux |>
            dplyr::as_tibble() |>
            dplyr::select(-"SERNUMERICA")
          
          # Concatenate rows
          metadata <- dplyr::bind_rows(metadata, metadata.aux)
          
        } else {
          
          rlang::warn(
            paste0("Series code not found: '", code[i], "'."),
            class = "ipeadata_series_not_found"
          )
          
        }
        
        # Progress Bar
        if (!quiet && n >= 2 && (i %% update.step == 0L || i == n)) {
          cli::cli_progress_update(id = pb, set = i)
        }
        
      }
      
    }, error = function(e) {
      rlang::abort(
        "Failed to retrieve data from the Ipeadata API.",
        class = "ipeadata_api_error",
        parent = e
      )
    })
    
  } else {
    
    rlang::abort(
      "Internet connection is unavailable.",
      class = "ipeadata_no_internet"
    )
    
  }
  
  # Progress Bar closes
  if (!quiet && n >= 2) {
    cli::cli_progress_done(id = pb)
  }
  
  # Setting labels in selected language
  if (nrow(metadata) != 0) {
    
    ## Starting: Transform in date >
    ##           Transform in factor >
    ##           Transform in chr >
    ##           Replace missing status
    metadata <- metadata |>
      dplyr::mutate(
        SERATUALIZACAO = lubridate::as_date(.data$SERATUALIZACAO),
        FNTSIGLA = as.factor(.data$FNTSIGLA),
        SERSTATUS = as.character(.data$SERSTATUS),
        SERSTATUS = dplyr::if_else(
          is.na(.data$SERSTATUS),
          "",
          .data$SERSTATUS
        )
      )
    
    # Setting labels in selected language
    if (language == "en") {
      
      metadata <- metadata |>
        dplyr::mutate(
          BASNOME = iconv(.data$BASNOME, "UTF-8", "ASCII//TRANSLIT"),
          BASNOME = factor(
            .data$BASNOME,
            levels = c("Macroeconomico", "Regional", "Social"),
            labels = c("Macroeconomic", "Regional", "Social")
          ),
          UNINOME = iconv(.data$UNINOME, "UTF-8", "ASCII//TRANSLIT"),
          UNINOME = factor(
            .data$UNINOME,
            levels = df_uninome$uninome,
            labels = df_uninome$uninome_en
          ),
          PERNOME = iconv(.data$PERNOME, "UTF-8", "ASCII//TRANSLIT"),
          PERNOME = factor(
            .data$PERNOME,
            levels = df_pernome$pernome,
            labels = df_pernome$pernome_en
          ),
          MULNOME = iconv(.data$MULNOME, "UTF-8", "ASCII//TRANSLIT"),
          MULNOME = factor(
            .data$MULNOME,
            levels = df_mulnome$mulnome,
            labels = df_mulnome$mulnome_en
          ),
          SERSTATUS = factor(
            .data$SERSTATUS,
            levels = c("A", "I", ""),
            labels = c("Active", "Inactive", "")
          ),
          TEMCODIGO = as.integer(.data$TEMCODIGO)
        ) |>
        purrr::set_names(c(
          "code", "name", "comment", "lastupdate", "theme", "source",
          "sourcename", "sourceurl", "freq", "unity", "mf", "status",
          "scode", "ccode"
        ))
      
      if (isTRUE(label)) {
        
        metadata <- metadata |>
          sjlabelled::set_label(c(
            "Ipeadata Code", "Series Name (PT-BR)", "Comment (PT-BR)",
            "Last Update", "Theme", "Source", "Source Full Name",
            "Source URL", "Frequency", "Unity", "Multiplier Factor",
            "Status", "Subject Code", "Country Code"
          ))
        
      }
      
    } else {
      
      metadata <- metadata |>
        dplyr::mutate(
          BASNOME = factor(.data$BASNOME),
          UNINOME = factor(.data$UNINOME),
          PERNOME = factor(.data$PERNOME),
          MULNOME = factor(.data$MULNOME),
          SERSTATUS = factor(
            .data$SERSTATUS,
            levels = c("A", "I", ""),
            labels = c("Ativa", "Inativa", "")
          ),
          TEMCODIGO = as.integer(.data$TEMCODIGO)
        ) |>
        purrr::set_names(c(
          "code", "name", "comment", "lastupdate", "theme", "source",
          "sourcename", "sourceurl", "freq", "unity", "mf", "status",
          "scode", "ccode"
        ))
      
      if (isTRUE(label)) {
        
        metadata <- metadata |>
          sjlabelled::set_label(c(
            "Codigo Ipeadata", "Nome da Serie (PT-BR)", "Comentario",
            "Ultima Atualizacao", "Base", "Fonte", "Nome da Fonte",
            "URL da Fonte", "Frequencia", "Unidade", "Fator Multiplicador",
            "Status", "Codigo do Tema", "Codigo de Pais"
          ))
        
      }
      
    }
    
  }
  
  metadata
}
