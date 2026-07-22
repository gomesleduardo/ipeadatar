#' @title Data for Requested Series
#'
#' @description Returns the data associated with requested Ipeadata series.
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
#' @return A data frame containing the Ipeadata code, date, value,
#'   territorial unit name, and territorial code of the requested series.
#'
#' @note Ipeadata codes may be obtained using [available_series()].
#'
#' @seealso [available_series()], [available_territories()]
#'
#' @references This package uses the Ipeadata API:
#'   \url{https://www.ipeadata.gov.br/}
#'
#' @export
#' 
#' @examples
#' \donttest{data <- ipeadata(code = "PRECOS12_IPCA12")}

ipeadata <- function(
    code, language = c("en", "br"), label = TRUE, quiet = FALSE
) {
  
  # Check language arg
  language <- match.arg(language)
  
  # Output
  values <- tibble::tibble()
  
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
    
    ## Main URL 
    Sys.sleep(.01)
    tryCatch({
      
      # Retrieve series data for each code
      for (i in seq_along(code)) {
        
        # Check
        code0 <- gsub(" ", "_", toupper(code[i]))
        
        # URL for series data
        url <- paste0(
          "https://www.ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='", 
          code0, "')"
        )
        
        # Extract from JSON
        Sys.sleep(.01)
        res <- curl::curl_fetch_memory(url)
        values.aux <- jsonlite::fromJSON(
          rawToChar(res$content),
          flatten = TRUE
        )[["value"]] |>
          dplyr::as_tibble()
        
        if (nrow(values.aux) > 0) {
          
          # Sorting by tcode and date
          values.aux <- values.aux |>
            dplyr::mutate(
              TERCODIGO = dplyr::if_else(
                .data$TERCODIGO == "",
                "0",
                .data$TERCODIGO
              ),
              TERCODIGO = as.integer(.data$TERCODIGO),
              NIVNOME = dplyr::if_else(
                .data$NIVNOME == "",
                "Brasil",
                .data$NIVNOME
              ),
              VALDATA = lubridate::as_date(.data$VALDATA)
            ) |>
            dplyr::arrange(.data$TERCODIGO, .data$VALDATA)
          
          # Concatenate rows
          values <- dplyr::bind_rows(values, values.aux)
          
        } else {
          
          cli::cli_alert_warning("Code '{code[i]}' not found.")
          
        }
        
        # Progress Bar
        if (nrow(values) > 0) { 
          if (!quiet && n >= 2 && (i %% update.step == 0L || i == n)) {
            cli::cli_progress_update(id = pb, set = i)
          }
        }

      }
      
    }, error = function(e) {NULL})
    
    ## Alternative URL 
    if (nrow(values) == 0) {
      
      Sys.sleep(.01)
      tryCatch({
        
        # Retrieve series data for each code
        for (i in seq_along(code)) {
          
          # Check
          code0 <- gsub(" ", "_", toupper(code[i]))
          
          # URL for series data
          url <- paste0(
            "http://www.ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='", 
            code0, "')"
          )
          
          # Extract from JSON
          Sys.sleep(.01)
          res <- curl::curl_fetch_memory(url)
          values.aux <- jsonlite::fromJSON(
            rawToChar(res$content),
            flatten = TRUE
          )[["value"]] |>
            dplyr::as_tibble()
          
          if (nrow(values.aux) > 0) {
            
            # Sorting by tcode and date
            values.aux <- values.aux |>
              dplyr::mutate(
                TERCODIGO = dplyr::if_else(
                  .data$TERCODIGO == "",
                  "0",
                  .data$TERCODIGO
                ),
                TERCODIGO = as.integer(.data$TERCODIGO),
                NIVNOME = dplyr::if_else(
                  .data$NIVNOME == "",
                  "Brasil",
                  .data$NIVNOME
                ),
                VALDATA = lubridate::as_date(.data$VALDATA)
              ) |>
              dplyr::arrange(.data$TERCODIGO, .data$VALDATA)
            
            # Concatenate rows
            values <- dplyr::bind_rows(values, values.aux)
            
          } else {
            
            cli::cli_alert_warning("Code '{code[i]}' not found.")
            
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
      
    }

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
  if (nrow(values) > 0) {
    
    ## Starting: Remove NA >
    ##           Rename variables >
    ##           Add subtitles >
    ##           Remove duplicates
    values <- values |>
      dplyr::filter(!is.na(.data$VALVALOR)) |>
      dplyr::distinct()
    
    # Setting labels in selected language
    if (language == "en") {
      
      values <- values |>
        dplyr::mutate(
          NIVNOME = iconv(.data$NIVNOME, "UTF-8", "ASCII//TRANSLIT"),
          NIVNOME = factor(
            .data$NIVNOME,
            levels = df_nivnome$nivnome,
            labels = df_nivnome$nivnome_en
          ),
          TERCODIGO = as.character(.data$TERCODIGO)
        ) |> 
        purrr::set_names(c("code", "date", "value", "uname", "tcode"))
      
      if (isTRUE(label)) {
        
        values <- values |>
          sjlabelled::set_label(c(
            "Ipeadata Code", "Date", "Value", "Territorial Unit Name", 
            "Territorial Code"
          ))
        
      }
      
    } else {
      
      values <- values |>
        dplyr::mutate(
          NIVNOME = iconv(.data$NIVNOME, "UTF-8", "ASCII//TRANSLIT"),
          NIVNOME = factor(.data$NIVNOME, levels = df_nivnome$nivnome),
          TERCODIGO = as.character(.data$TERCODIGO)
        ) |>
        purrr::set_names(c("code", "date", "value", "uname", "tcode"))
      
      if (isTRUE(label)) {
        
        values <- values |>
          sjlabelled::set_label(c(
            "Codigo Ipeadata", "Data", "Valor", "Nome da Unidade Territorial",
            "Codigo Territorial"
          ))
        
      }
      
    }
  }
  
  values
}
