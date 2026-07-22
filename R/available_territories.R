#' @title Available Territorial Divisions
#'
#' @description Returns a list of available Brazilian territorial divisions
#' from the Ipeadata API.
#'
#' @param language A character string specifying the language. Available
#'   options are English (`"en"`, default) and Brazilian Portuguese (`"br"`).
#'   
#' @param label Logical. If `TRUE` (default), variable labels are added to
#'   the output data frame using `sjlabelled::set_label()`.
#'
#' @return A data frame containing the unit type, code, name, and area
#'   (in km²) of Brazilian territorial divisions.
#'
#' @export

available_territories <- function(language = c("en", "br"), label = TRUE) {
  
  # Check language arg
  language <- match.arg(language)
  
  # URL for territories
  url <- "https://www.ipeadata.gov.br/api/odata4/Territorios/"
  urla <- "http://www.ipeadata.gov.br/api/odata4/Territorios/"
  
  # Output NULL
  territories <- NULL
  
  # Test internet connection
  if (curl::has_internet()) {
    
    ## Main URL 
    Sys.sleep(.01)
    tryCatch({
      
      res <- curl::curl_fetch_memory(url)
      ## Starting: Extract from JSON >
      ##           Transform to tbl >
      ##           Select variables >
      ##           Remove NA >
      ##           Sort by uname >
      ##           Rename variables >
      ##           Add subtitles
      territories <- jsonlite::fromJSON(
        rawToChar(res$content),
        flatten = TRUE
      )[["value"]] |>
        dplyr::as_tibble() |>
        dplyr::select("NIVNOME", "TERCODIGO", "TERNOME", "TERAREA") |>
        dplyr::filter(!is.na(.data$TERAREA)) |>
        dplyr::arrange(.data$TERCODIGO)
      
    }, error = function(e) {NULL})
    
    ## Alternative URL 
    if (is.null(territories)) {
      
      Sys.sleep(.01)
      tryCatch({
        
        res <- curl::curl_fetch_memory(urla)
        ## Starting: Extract from JSON >
        ##           Transform to tbl >
        ##           Select variables >
        ##           Remove NA >
        ##           Sort by uname >
        ##           Rename variables >
        ##           Add subtitles
        territories <- jsonlite::fromJSON(
          rawToChar(res$content),
          flatten = TRUE
        )[["value"]] |>
          dplyr::as_tibble() |>
          dplyr::select("NIVNOME", "TERCODIGO", "TERNOME", "TERAREA") |>
          dplyr::filter(!is.na(.data$TERAREA)) |>
          dplyr::arrange(.data$TERCODIGO)
        
      }, error = function(e) {
        rlang::abort(
          "Failed to retrieve data from the Ipeadata API.",
          class = "ipeadata_api_error",
          parent = e
        )
      })
      
    }
    
    # Setting labels in selected language
    if (!is.null(territories)) {
      
      # Setting labels in selected language
      if (language == "en") {
        
        territories <- territories |>
          dplyr::mutate(
            NIVNOME = iconv(.data$NIVNOME, "UTF-8", "ASCII//TRANSLIT"),
            NIVNOME = factor(
              .data$NIVNOME,
              levels = df_nivnome$nivnome,
              labels = df_nivnome$nivnome_en
            )
          ) |>
          dplyr::arrange(.data$NIVNOME) |>
          purrr::set_names(c("uname", "tcode", "tname", "area"))
        
        if (isTRUE(label)) {
          
          territories <- territories |>
            sjlabelled::set_label(c(
              "Territorial Unit Name", 
              "Territorial Code",
              "Territorial Name", "Area (km2)"
            ))
          
        }
        
      } else {
        
        aux_ter <- territories |>
          dplyr::select("NIVNOME", "TERCODIGO") |>
          purrr::set_names(c("NIVNOME0", "TERCODIGO"))
        
        territories <- territories |>
          dplyr::mutate(
            NIVNOME = iconv(.data$NIVNOME, "UTF-8", "ASCII//TRANSLIT"),
            NIVNOME = factor(.data$NIVNOME, levels = df_nivnome$nivnome)
          ) |>
          dplyr::arrange(.data$NIVNOME) |>
          dplyr::left_join(
            aux_ter,
            by = "TERCODIGO",
            relationship = "many-to-many"
          ) |>
          dplyr::select("NIVNOME0", "TERCODIGO", "TERNOME", "TERAREA") |>
          dplyr::mutate(NIVNOME0 = as.factor(.data$NIVNOME0)) |>
          purrr::set_names(c("uname", "tcode", "tname", "area"))
        
        if (isTRUE(label)) {
          
          territories <- territories |>
            sjlabelled::set_label(c(
              "Nome da Unidade Territorial", 
              "Codigo Territorial",
              "Nome do Territorio", "Area (km2)"
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
  
  territories
}
