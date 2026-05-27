#' @title Searched Series
#'
#' @description Searches for Ipeadata series matching the provided terms.
#'
#' @param terms A character vector of search terms.
#'
#' @param language A character string specifying the language. Available
#'   options are English (`"en"`, default) and Brazilian Portuguese (`"br"`).
#'
#' @param label Logical. If `TRUE` (default), variable labels are added to
#'   the output data frame using `sjlabelled::set_label()`.
#'
#' @return A data frame containing the Ipeadata code, name, theme, source,
#'   frequency, last update, and activity status of the matched series.
#'
#' @examples
#' \donttest{
#' search_series(terms = "rural")
#' }
#'
#' @note The original language of the series names is preserved.
#'
#' @export

search_series <- function(terms = NULL, language = c("en", "br"), label = TRUE) {
  
  # Check language arg
  language <- match.arg(language)
  
  # Check terms
  if (!is.null(terms)) {
    if (!is.character(terms)) {
      rlang::abort(
        "`terms` must be a character vector or NULL.",
        class = "ipeadata_invalid_terms"
      )
    }
    
    if (anyNA(terms)) {
      rlang::abort(
        "`terms` must not contain NA values.",
        class = "ipeadata_invalid_terms"
      )
    }
    
    if (any(!nzchar(terms))) {
      rlang::abort(
        "`terms` must not contain empty strings.",
        class = "ipeadata_invalid_terms"
      )
    }
  }
  
  # Getting all series
  all_series <- available_series(language = language, label = FALSE)
  
  # Searching
  if (!is.null(terms)) {
    
    user_search <- dplyr::tibble()
    
    for (i in seq_along(terms)) {
      
      user_search <- dplyr::bind_rows(
        user_search,
        all_series |>
          dplyr::filter(
            dplyr::if_any(
              dplyr::everything(),
              ~ grepl(
                terms[i],
                as.character(.x),
                ignore.case = TRUE
              )
            )
          )
      ) |>
        dplyr::distinct()
      
    }
    
  } else {
    
    user_search <- all_series
    
  }
  
  user_search <- user_search |>
    purrr::set_names(c(
      "code", "name", "theme", "source", "freq", "lastupdate", "status"
    ))
  
  # Setting labels in selected language
  if (isTRUE(label)) {
    
    if (language == "en") {
      user_search <- user_search |>
        sjlabelled::set_label(c(
          "Ipeadata Code", "Series Name (PT-BR)", "Theme",
          "Source", "Frequency", "Last Update", "Status"
        ))
    } else {
      user_search <- user_search |>
        sjlabelled::set_label(c(
          "Codigo Ipeadata", "Nome da Serie", "Base", "Fonte",
          "Frequencia", "Ultima Atualizacao", "Status"
        ))
    }
    
  }

  user_search
}
