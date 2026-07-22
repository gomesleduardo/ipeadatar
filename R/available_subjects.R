#' @title Available Subjects
#'
#' @description Returns a list of available subjects from the Ipeadata API.
#'
#' @param language A character string specifying the language. Available
#'   options are English (`"en"`, default) and Brazilian Portuguese (`"br"`).
#'   
#' @param label Logical. If `TRUE` (default), variable labels are added to
#'   the output data frame using `sjlabelled::set_label()`.
#'
#' @return A data frame containing the subject code and subject name.
#'
#' @export

available_subjects <- function(language = c("en", "br"), label = TRUE) {
  
  # Check language arg
  language <- match.arg(language)
  
  # URL for subjects
  url <- "https://www.ipeadata.gov.br/api/odata4/Temas/"
  urla <- "http://www.ipeadata.gov.br/api/odata4/Temas/"
  
  # Output NULL
  subjects <- NULL
  
  # Test internet connection
  if (curl::has_internet()) {
    
    ## Main URL 
    Sys.sleep(.01)
    tryCatch({
      
      res <- curl::curl_fetch_memory(url)
      ## Starting: Extract from JSON >
      ##           Transform to tbl >
      ##           Select variables >
      ##           Sort by code >
      ##           Transform in chr
      subjects <- jsonlite::fromJSON(
        rawToChar(res$content),
        flatten = TRUE
      )[["value"]] |>
        dplyr::as_tibble() |>
        dplyr::select("TEMCODIGO", "TEMNOME") |>
        dplyr::arrange(.data$TEMCODIGO) |>
        dplyr::mutate(TEMNOME = as.character(.data$TEMNOME))
      
    }, error = function(e) {NULL})
    
    ## Alternative URL
    if (is.null(subjects)) {
      
      Sys.sleep(.01)
      tryCatch({
        
        res <- curl::curl_fetch_memory(urla)
        ## Starting: Extract from JSON >
        ##           Transform to tbl >
        ##           Select variables >
        ##           Sort by code >
        ##           Transform in chr
        subjects <- jsonlite::fromJSON(
          rawToChar(res$content),
          flatten = TRUE
        )[["value"]] |>
          dplyr::as_tibble() |>
          dplyr::select("TEMCODIGO", "TEMNOME") |>
          dplyr::arrange(.data$TEMCODIGO) |>
          dplyr::mutate(TEMNOME = as.character(.data$TEMNOME))
        
      }, error = function(e) {
        rlang::abort(
          "Failed to retrieve data from the Ipeadata API.",
          class = "ipeadata_api_error",
          parent = e
        )
      })
      
    }

    # Setting labels in selected language
    if (!is.null(subjects)) {
      
      # Setting labels in selected language
      if (language == "en") {
        
        subjects <- subjects |>
          dplyr::mutate(
            TEMNOME = iconv(.data$TEMNOME, "UTF-8", "ASCII//TRANSLIT"),
            TEMNOME = factor(
              .data$TEMNOME,
              levels = df_temnome$temnome,
              labels = df_temnome$temnome_en
            ),
            TEMNOME = as.character(.data$TEMNOME)
          ) |>
          purrr::set_names(c("scode", "sname"))
        
        if (isTRUE(label)) {
          
          subjects <- subjects |>
            sjlabelled::set_label(c("Subject Code", "Subject Name"))
          
        }
        
      } else {
        
        subjects <- subjects |>
          purrr::set_names(c("scode", "sname"))
        
        if (isTRUE(label)) {
          
          subjects <- subjects |>
            sjlabelled::set_label(c("Codigo do Tema", "Nome do Tema"))
          
        }

      }
      
    }
    
  } else {
    rlang::abort(
      "Internet connection is unavailable.",
      class = "ipeadata_no_internet"
    )
  }
  
  subjects
}
