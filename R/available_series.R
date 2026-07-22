#' @title Available Series
#'
#' @description Returns a list of available series from the Ipeadata API.
#'
#' @param language A character string specifying the language. Available
#'   options are English (`"en"`, default) and Brazilian Portuguese (`"br"`).
#'
#' @param label Logical. If `TRUE` (default), variable labels are added to
#'   the output data frame using `sjlabelled::set_label()`.
#'
#' @return A data frame containing the Ipeadata code, name, theme, source,
#'   frequency, last update, and activity status of available series.
#'
#' @note The original language of the available series names is preserved.
#'
#' @export

available_series <- function(language = c("en", "br"), label = TRUE) {
  
  # Check language arg
  language <- match.arg(language)
  
  # URL for metadata
  url <- "https://www.ipeadata.gov.br/api/odata4/Metadados/"
  urla <- "http://www.ipeadata.gov.br/api/odata4/Metadados/"
  
  # Output NULL
  series <- NULL
  
  # Test internet connection
  if (curl::has_internet()) {
    
    ## Main URL 
    Sys.sleep(.01)
    tryCatch({
      
      res <- curl::curl_fetch_memory(url)
      ## Starting: Extract from JSON >
      ##           Transform to tbl >
      ##           Select variables >
      ##           Sort by source, freq and code >
      ##           Transform in factor >
      ##           Transform in date >
      series <- jsonlite::fromJSON(
        rawToChar(res$content),
        flatten = TRUE
      )[["value"]] |>
        dplyr::as_tibble() |>
        dplyr::select(
          "SERCODIGO", "SERNOME", "BASNOME", "FNTSIGLA", "PERNOME",
          "SERATUALIZACAO", "SERSTATUS"
        ) |>
        dplyr::arrange(
          .data$BASNOME, .data$FNTSIGLA, .data$PERNOME, .data$SERCODIGO
        ) |>
        dplyr::mutate(
          FNTSIGLA = as.factor(.data$FNTSIGLA),
          SERATUALIZACAO = lubridate::as_date(.data$SERATUALIZACAO),
          SERSTATUS = as.character(.data$SERSTATUS),
          SERSTATUS = dplyr::if_else(
            is.na(.data$SERSTATUS), "", .data$SERSTATUS
          )
        )
      
    }, error = function(e) {NULL})
    
    ## Alternative URL 
    if (is.null(series)) {
      
      Sys.sleep(.01)
      tryCatch({
        
        res <- curl::curl_fetch_memory(urla)
        ## Starting: Extract from JSON >
        ##           Transform to tbl >
        ##           Select variables >
        ##           Sort by source, freq and code >
        ##           Transform in factor >
        ##           Transform in date >
        series <- jsonlite::fromJSON(
          rawToChar(res$content),
          flatten = TRUE
        )[["value"]] |>
          dplyr::as_tibble() |>
          dplyr::select(
            "SERCODIGO", "SERNOME", "BASNOME", "FNTSIGLA", "PERNOME",
            "SERATUALIZACAO", "SERSTATUS"
          ) |>
          dplyr::arrange(
            .data$BASNOME, .data$FNTSIGLA, .data$PERNOME, .data$SERCODIGO
          ) |>
          dplyr::mutate(
            FNTSIGLA = as.factor(.data$FNTSIGLA),
            SERATUALIZACAO = lubridate::as_date(.data$SERATUALIZACAO),
            SERSTATUS = as.character(.data$SERSTATUS),
            SERSTATUS = dplyr::if_else(
              is.na(.data$SERSTATUS), "", .data$SERSTATUS
            )
          )
        
      }, error = function(e) {
        rlang::abort(
          "Failed to retrieve data from the Ipeadata API.",
          class = "ipeadata_api_error",
          parent = e
        )
      })
      
    }

    # Setting labels in selected language
    if (!is.null(series)) {
      
      if (language == "en") {
        
        series <- series |> 
          dplyr::mutate(
            SERSTATUS = factor(
              .data$SERSTATUS,
              levels = c("A", "I", ""),
              labels = c("Active", "Inactive", "")
            ),
            PERNOME = iconv(.data$PERNOME, "UTF-8", "ASCII//TRANSLIT"),
            PERNOME = factor(
              .data$PERNOME,
              levels = df_pernome$pernome,
              labels = df_pernome$pernome_en
            ),
            BASNOME = iconv(.data$BASNOME, "UTF-8", "ASCII//TRANSLIT"),
            BASNOME = factor(
              .data$BASNOME,
              levels = c("Macroeconomico", "Regional", "Social"),
              labels = c("Macroeconomic", "Regional", "Social")
            )
          ) |>
          purrr::set_names(c(
            "code", "name", "theme", "source",
            "freq", "lastupdate", "status"
          ))
        
        if (isTRUE(label)) {
          
          series <- series |>
            sjlabelled::set_label(c(
              "Ipeadata Code", "Series Name (PT-BR)", "Theme", 
              "Source", "Frequency", "Last Update", "Status"
            ))
          
        }
        
      } else {
        
        series <- series |> 
          dplyr::mutate(
            SERSTATUS = factor(
              .data$SERSTATUS,
              levels = c("A", "I", ""),
              labels = c("Ativa", "Inativa", "")
            ),
            BASNOME = factor(.data$BASNOME),
            PERNOME = factor(.data$PERNOME)
          ) |>
          purrr::set_names(c(
            "code", "name", "theme", "source",
            "freq", "lastupdate", "status"
          ))
        
        if (isTRUE(label)) {
          
          series <- series |>
            sjlabelled::set_label(c(
              "Codigo Ipeadata", "Nome da Serie", "Base", "Fonte",
              "Frequencia", "Ultima Atualizacao", "Status"
            ))
          
        }

      }
      
    }
    
  } else {
    rlang::abort(
      "Internet connection is unavailable.",
      class = "ipeadata_no_internet"
    )
  }
  
  series
}
