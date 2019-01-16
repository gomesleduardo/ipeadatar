# ------------------------------------------------------------------ #
# |   Brazilian Institute for Applied Economic Research - Ipea     | #
# ------------------------------------------------------------------ #
# ------------------------------------------------------------------ #
# |   Author: Luiz E. S. Gomes                                     | #
# |   Collaborator: Jessyka A. P. Goltara                          | #
# |   Advisor: Erivelton P. Guedes                                 | #
# ------------------------------------------------------------------ #
# ------------------------------------------------------------------ #
# |   R package for Ipeadata API database                          | #
# |   Version: 0.0.1                                               | #
# |   January 14, 2019                                             | #
# ------------------------------------------------------------------ #

# Available series ------------------------------------------------

#' @title List with available series
#'
#' @description Returns a list with available series from Ipeadata API database.
#'
#' @usage available_series(language = c("en", "br"))
#'
#' @param language string specifying the selected language. Language options are
#' English (\code{"en"}, default) and Brazilian portuguese (\code{"br"}).
#'
#' @return A data frame containing Ipeadata code, name (in Brazilian portuguese), source,
#' periodicity, last update and activity status of available series.
#'
#' @examples
#' # Available series (in English)
#' all_series <- available_series()
#'
#' # Available series (in Brazilian portuguese)
#' all_seriesBR <- available_series(language = "br")
#'
#' @note The names of available series maintain in the original language (Brazilian portuguese).
#'
#' @export
#'
#' @importFrom magrittr %<>%

available_series <- function(language = c("en", "br")) {

  # Check language arg
  language <- match.arg(language)

  # URL for metadata
  url <- 'http://www.ipeadata.gov.br/api/odata4/Metadados/'

  ## Starting: Extract from JSON >
  ##           Transform to tbl >
  ##           Select variables >
  ##           Sort by source, periodicity and code >
  ##           Remove NA >
  ##           Remove accents >
  ##           Transform in factor >
  ##           Transform in date >
  series <-
    jsonlite::fromJSON(url, flatten = TRUE)[[2]] %>%
    dplyr::as_tibble() %>%
    dplyr::select(SERCODIGO, SERNOME, FNTSIGLA, PERNOME, SERATUALIZACAO, SERSTATUS) %>%
    dplyr::arrange(FNTSIGLA, PERNOME, SERCODIGO) %>%
    dplyr::filter(!is.na(PERNOME) & !is.na(SERSTATUS)) %>%
    dplyr::mutate(PERNOME = iconv(PERNOME, 'UTF-8', 'ASCII//TRANSLIT')) %>%
    dplyr::mutate(FNTSIGLA = as.factor(FNTSIGLA)) %>%
    dplyr::mutate(SERATUALIZACAO = lubridate::as_date(SERATUALIZACAO))

    # Setting labels in selected language
    if (language == 'en') {

      series %<>%
        dplyr::mutate(SERSTATUS = factor(SERSTATUS,
                                         levels = c('A', 'I'),
                                         labels =  c('Active', 'Inactive'))) %>%
        dplyr::mutate(PERNOME = factor(PERNOME,
                                       levels = c('Anual', 'Decenal', 'Diaria',
                                                  'Mensal', 'Quadrienal', 'Quinquenal',
                                                  'Semestral', 'Trimestral', 'Nao se aplica'),
                                       labels = c('Yearly', 'Decennial', 'Daily',
                                                  'Monthly', 'Quadrennial', 'Quinquennial',
                                                  'Semiannual', 'Quarterly', 'Not applicable'))) %>%
        purrr::set_names(c('code', 'name', 'source',
                           'periodicity', 'lastupdate', 'status')) %>%
        sjlabelled::set_label(c('Ipeadata Code','Serie Name (PT-BR)','Source',
                                'Periodicity','Last Update','Status'))

    } else {

      series %<>%
        dplyr::mutate(SERSTATUS = factor(SERSTATUS,
                                         levels = c('A', 'I'),
                                         labels =  c('Ativa', 'Inativa'))) %>%
        dplyr::mutate(PERNOME = factor(PERNOME)) %>%
        purrr::set_names(c('codigo', 'nome', 'fonte',
                           'periodicidade', 'ultimaatualizacao', 'status')) %>%
        sjlabelled::set_label(c('Codigo Ipeadata','Nome da Serie','Fonte',
                                'Periodicidade','Ultima Atualizacao','Status'))

    }

  series
}

#' # Available themes ------------------------------------------------
#'
#' #' @title List with available themes
#' #'
#' #' @description Returns a list with all available series from Ipeadata API database. XXXXXXXXXXXXX
#' #'
#' #' @usage available_themes()
#' #'
#' #' @return A data frame containing Ipeadata code, name (in brazilian portuguese), source,
#' #' periodicity, last update date and activity status of available series. XXXXXXXXXXXX
#' #'
#' #' @examples
#' #' # Available themes
#' #' all_themes <- available_themes()
#' #'
#' #' @export
#'
#' available_themes <- function() {
#'
#'   # URL for themes
#'   url <- 'http://www.ipeadata.gov.br/api/odata4/Temas/'
#'
#'   ## Starting: Extract from JSON >
#'   ##           Transform to tbl >
#'   ##           Select variables >
#'   ##           Sort by code >
#'   ##           Transform in chr >
#'   ##           Rename variables >
#'   ##           Add subtitles
#'
#'   themes <-
#'     data.frame(jsonlite::fromJSON(url, flatten = TRUE)[[2]]) %>%
#'     dplyr::as_tibble() %>%
#'     dplyr::select(TEMCODIGO, TEMNOME) %>%
#'     dplyr::arrange(TEMCODIGO) %>%
#'     dplyr::mutate(TEMNOME = as.character(TEMNOME)) %>%
#'     purrr::set_names(c('tcode', 'theme')) %>%
#'     sjlabelled::set_label(c('Theme Code','Theme (PT-BR)'))
#'
#'   themes
#' }
#'
#' # Available countries ------------------------------------------------
#' available_countries <- function() {
#'   # URL for countries
#'   url <- 'http://www.ipeadata.gov.br/api/odata4/Paises/'
#'
#'   ## Starting: Extract from JSON >
#'   ##           Transform to tbl >
#'   ##           Sort by code >
#'   ##           Rename variables >
#'   ##           Add subtitles
#'
#'   countries <-
#'     jsonlite::fromJSON(url, flatten = TRUE)[[2]] %>%
#'     dplyr::as_tibble() %>%
#'     dplyr::arrange(PAICODIGO) %>%
#'     purrr::set_names(c('ccode', 'cname')) %>%
#'     sjlabelled::set_label(c('Country Code (ISO 3)','Country Name (PT-BR)'))
#'
#'   countries
#' }
#'
#' # Example:
#' all_countries <- available_countries()

# # Available territories ------------------------------------------------
# available_territories <- function() {
#
#   # URL for territories
#   url <- 'http://www.ipeadata.gov.br/api/odata4/Territorios/'
#
#   ## Starting: Extract from JSON >
#   ##           Transform to tbl >
#   ##           Select variables >
#   ##           Remove NA >
#   ##           Sort by uname >
#   ##           Rename variables >
#   ##           Add subtitles
#
#   territories <-
#     jsonlite::fromJSON(url, flatten = TRUE)[[2]] %>%
#     dplyr::as_tibble() %>%
#     dplyr::select(NIVNOME, TERCODIGO, TERNOME, TERAREA) %>%
#     dplyr::filter(!is.na(TERAREA)) %>%
#     dplyr::arrange(NIVNOME) %>%
#     purrr::set_names(c('uname', 'ccode', 'cname', 'area')) %>%
#     sjlabelled::set_label(c('Territorial Unit Name (PT-BR)',
#                             'Territorial Code','Territorial Name (PT-BR)','Area (Km2)'))
#
#   territories
# }
#
# # Example:
# all_territories <- available_territories()
#
# # Metadata ------------------------------------------------
# metadata <- function(code, quiet = FALSE) {
#
#   # Output
#   metadata <- dplyr::as_tibble(data.frame(NULL))
#
#   # Progress Bar settings
#   if (!quiet & (length(code) >= 5)) {
#     cat("Requesting Ipeadata API <http://www.ipeadata.gov.br/api/>")
#     cat('\n')
#     pb <- txtProgressBar(min = 0, max = length(code), style = 3)
#   }
#   update.step <- max(5, floor(length(code)/100))
#
#   # Retrieve metadata 1 by 1
#   for (i in 1:length(code)) {
#
#     # Check
#     code0 <- gsub(" ", "_", toupper(code[i]))
#
#     # URL for metadata
#     url <- paste0("http://www.ipeadata.gov.br/api/odata4/Metadados('", code0,"')")
#
#     # Extract from JSON
#     metadata.aux <- jsonlite::fromJSON(url, flatten = TRUE)[[2]]
#
#     if (length(metadata.aux) > 0) {
#
#       ## Starting: Transform to tbl >
#       ##           Select variables >
#       metadata.aux %<>%
#         dplyr::as_tibble() %>%
#         dplyr::select(- SERNUMERICA)
#
#       # Concatenate rows
#       metadata <- rbind(metadata,metadata.aux)
#     } else {
#       warning(paste0("code '", code[i], "' not found"))
#     }
#
#     # Progress Bar
#     if (!quiet & (i %% update.step == 0 | i == length(code)) & (length(code) >= 5)) {
#       setTxtProgressBar(pb, i)
#     }
#   }
#
#   # Progress Bar closes
#   if (!quiet & (length(code) >= 5)) {
#     close(pb)
#   }
#
#   ## Starting: Transform in date >
#   ##           Remove accents >
#   ##           Label bname >
#   ##           Transform in factor >
#   ##           Transform in factor >
#   ##           Remove accents >
#   ##           Label unity >
#   ##           Remove accents >
#   ##           Label periodicity >
#   ##           Remove accents >
#   ##           Label mf >
#   ##           Label status >
#   ##           Rename variables >
#   ##           Add subtitles
#
#   metadata %<>%
#     dplyr::mutate(SERATUALIZACAO = lubridate::as_date(SERATUALIZACAO)) %>%
#     dplyr::mutate(BASNOME = iconv(BASNOME, 'UTF-8', 'ASCII//TRANSLIT')) %>%
#     dplyr::mutate(BASNOME = factor(BASNOME,
#                                    levels = c('Macroeconomico', 'Regional', 'Social'),
#                                    labels = c('Macroeconomic', 'Regional', 'Social'))) %>%
#     dplyr::mutate(FNTSIGLA = as.factor(FNTSIGLA)) %>%
#     dplyr::mutate(FNTNOME = as.factor(FNTNOME)) %>%
#     dplyr::mutate(UNINOME = iconv(UNINOME, 'UTF-8', 'ASCII//TRANSLIT')) %>%
#     dplyr::mutate(UNINOME = factor(UNINOME,
#                                    levels = c("-", "Tonelada", "R$", "GWh", "US$", "Euro", "Unidade", "Metro cubico",
#                                               "Marco alemao", "Franco frances", "Franco frances", "Libra esterlina",
#                                               "Dolar canadense", "Florim holandes", "Franco belga", "Peso argentino",
#                                               "Peso chileno", "Peseta espanhola", "Peso novo mexicano", "Peso uruguaio",
#                                               "Won sul-coreano", "Cabeca", "Litro", "Duzia", "Cacho", "Ouro", "Iene japones",
#                                               "Guarani paraguaio", "Conto de reis", "Km", "Km2", "Mil barris/dia", "NCZ$",
#                                               "Ouro oncas", "Pessoa", "R$/MWh", "R$/US$", "Indice", "Porcentagem",
#                                               "Habitante", "Ano", "R$, a precos do ano 2000", "(% a.m.)", "(% PIB)",
#                                               "(% a.a.)", "(%)", "Metro", "Caixa", "Cr$", "Barril", "Bolivar venezuelano",
#                                               "Peso colombiano", "CV", "R$ de 2000/HA", "R$ de 2000/T/KM", "Reis",
#                                               "US$ de 2008", "Passageiro-quilometro", "TEU", "Assento-quilometro",
#                                               "Tonelada-quilometro", "Hora", "determinado", "R$ Outubro 2009", "Qde.",
#                                               "Coroa dinamarquesa", "Dolar cingapurense", "Rupia cingalesa", "Dolar taiwanes",
#                                               "Nro", "R$ Janeiro 2012", "Sacas de 60 kg", "R$ Outubro 2012", "R$ de 2013",
#                                               "US$ de 2013", "R$ de outubro 2012", "Meses", "R$ de 2010", "R$ Outubro 2013",
#                                               "R$ de 2000", "R$ (do ultimo mes)", "R$ Outubro 2014", "R$ de 2014",
#                                               "R$ Penultimo mes da serie", "Hectare", "Razao", "Domicilios", "R$ de 2001",
#                                               "Yuan", "Grau", "Eleitor", "Voto", "Numero", "(Eleitor)", "(Voto)", "(Unidade)",
#                                               "R$/Hrs", "Pence", "MW", "estabelecimento", "R$ de 1999", "Salario Minimo",
#                                               "(p.p.)", "Dias", "R$ de 1980",  "R$ de 1995", "Conto de reis de 1947",
#                                               "US$ de 1995", "Coroa norueguesa", "Coroa sueca", "Franco suico",
#                                               "Dolar australiano", "Dolar da nova zelândia", "Rand", "Peso boliviano",
#                                               "Peso dominicano", "Sucre", "Sol novo", "Dolar das bahamas", "Franco CFA",
#                                               "Dolar de trindad e tobago", "Rial iraniano", "Dinar iraquiano", "Shekel novo",
#                                               "Rial saudita", "Libra egipcia", "Rial iemenita", "Dolar de hong kong", "Rupia indiana",
#                                               "Rupia", "Ringgit malaio", "Peso filipino", "Dolar de cingapura", "Baht", "Dinar argelino",
#                                               "Kuanza reajustavel", "Dirra marroquino", "Naira", "Rublo", "Tolar", "Leu romeno",
#                                               "Libra esterlina de 1913", "(Sigla)", "Kg", "Pessoas", "Horas", "US$ FOB", "Libra irlandesa",
#                                               "Escudo", "zloty", "US$ de 2005", "MWh", "Tep", "Razao/relacao", "Quantidade", "Real",
#                                               "SM", "°C", "mm/mes"),
#                                    labels = c("-", "Ton", "R$", "GWh", "US$", "Euro", "Unit", "Cubic meter", "Deutsche mark",
#                                               "French franc", "Italian lira", "Pound sterling", "Canadian dollar",
#                                               "Dutch guilder", "Belgian franc", "Argentine peso", "Chilean peso", "Spanish peseta",
#                                               "Mexican new peso", "Uruguayan peso", "South Korean won", "Head", "Liter", "Dozen",
#                                               "Bunch", "Gold", "Japanese yen", "Paraguayan guarani", "Conto de reis", "Km", "Km2",
#                                               "Thousand barrels per day", "Brazilian cruzado novo", "Gold per ounce", "Person",
#                                               "R$/MWh", "R$/US$", "Index", "Percentage", "Inhabitant", "Year",
#                                               "R$, constant prices of 2000", "(% p.m.)", "(% GDP)", "(% p.y.)", "(%)", "Meter",
#                                               "Matchbox", "Cr$", "Barrel", "Venezuelan bolivar soberano", "Colombian peso",
#                                               "hp", "R$, constant prices of 2000 per hectare",
#                                               "R$, constant prices of 2000, R$ T/km", "Reis", "US$, constant prices of 2008",
#                                               "Passenger per km", "TEU", "Seat per kilometer", "Tonne per kilometer", "Hour",
#                                               "Determined", "R$, constant prices of October 2000", "Quantity", "Danish krone",
#                                               "Singapore dollar", "Sri Lankan rupee", "New Taiwan dollar", "Number",
#                                               "R$, constant prices of January 2012", "60 kg Sacks",
#                                               "R$, constant prices of October 2012", "R$, constant prices of 2013",
#                                               "US$, constant prices of 2013", "R$, constant prices of October 2012",
#                                               "Month", "R$, constant prices of 2010", "R$, constant prices of October 2013",
#                                               "R$, constant prices of 2000", "R$ (last month)",
#                                               "R$, constant prices of October 2014", "R$, constant prices of 2014",
#                                               "R$ (penultimate month of serie)", "Hectare", "Ratio", "Residence",
#                                               "R$, constant prices of 2001", "Yuan", "Degree", "Voter", "Vote", "Number",
#                                               "(Eleitor)", "(Voto)", "(Unity)", "R$ per hour", "Pence", "MW", "Establishment",
#                                               "R$, constant prices of 1999", "Basic salary", "(p.p.)", "Days", "R$, constant prices of 1980",
#                                               "R$, constant prices of 1995", "Conto de reis, constant prices of 1947",
#                                               "US$, constant prices of 1995", "Norwegian krone", "Swedish krona", "Swiss franc",
#                                               "Australian dollar", "New Zealand dollar", "South African rand", "Bolivian peso",
#                                               "Dominican peso", "Ecuadorian sucre", "Peruvian new sol", "Bahamian dollar", "CFA franc",
#                                               "Trinidad and Tobago dollar", "Iranian rial", "Iraqi dinar", "Israeli new shekel",
#                                               "Saudi riyal", "Egyptian pound", "Egyptian pound", "Hong Kong dollar", "Indian Rupee",
#                                               "Rupee", "Malaysian ringgit", "Philippine peso", "Singapore dollar", "Thai baht",
#                                               "Algerian dinar", "Angolan readjusted Kwanza", "Moroccan dirham", "Nigerian naira",
#                                               "Russian ruble", "Slovenian tolar", "Romanian leu",
#                                               "Pound sterling, constant prices of 1913", "(acronyms)", "Kg", "People", "Hour",
#                                               "US$ FOB", "Irish pound", "Escudo", "Polish zloty", "US$, constant prices of 2005",
#                                               "MWh", "toe", "Ratio/relation", "Quantity", "Real", "Basic salary", "°C", "mm/month"))) %>%
#     dplyr::mutate(PERNOME = iconv(PERNOME, 'UTF-8', 'ASCII//TRANSLIT')) %>%
#     dplyr::mutate(PERNOME = factor(PERNOME,
#                                    levels = c('Anual', 'Decenal', 'Diaria',
#                                               'Mensal', 'Quadrienal', 'Quinquenal',
#                                               'Semestral', 'Trimestral', 'Nao se aplica'),
#                                    labels = c('Yearly', 'Decennial', 'Daily',
#                                               'Monthly', 'Quadrennial', 'Quinquennial',
#                                               'Semiannual', 'Quarterly', 'Not applicable'))) %>%
#     dplyr::mutate(MULNOME = iconv(MULNOME, 'UTF-8', 'ASCII//TRANSLIT')) %>%
#     dplyr::mutate(MULNOME = factor(MULNOME,
#                                    levels =   c('mil', 'milhoes', 'bilhoes', 'centavos',
#                                                 'milhares', 'trilhoes', 'centenas',
#                                                 'centena de milhao'),
#                                    labels = c( 'Thousand', 'Millions', 'Billions', 'Cents',
#                                                'Thousands', 'Trillions', 'Hundreds',
#                                                'Hundred million'))) %>%
#     dplyr::mutate(SERSTATUS = factor(SERSTATUS,
#                                      levels = c('A', 'I'),
#                                      labels = c('Active', 'Inactive'))) %>%
#     purrr::set_names(c('code', 'name', 'comment', 'lastupdate', 'bname', 'source', 'sname',
#                        'surl', 'periodicity', 'unity', 'mf', 'status',
#                        'tcode', 'ccode')) %>%
#     sjlabelled::set_label(c('Ipeadata Code','Serie Name (PT-BR)', 'Comment (PT-BR)', 'Last Update',
#                             'Base name', 'Source', 'Source Name', 'Source URL',
#                             'Periodicity', 'Unity', 'Multiplier Factor', 'Status',
#                             'Theme Code', 'Country or Territorial Code'))
#
#   metadata
# }
#
# # Example:
# metadado.serieA <- metadata(c('abate_abpeav','AbInEe_VeLeTfC','VALOR366_FEDFUND366',
#                               'ABCD', 'CONSUMOTOT', '13T_BANAGUA'))
# metadado.serieB <- metadata(c('valor366_fedfund366'))
#
# # metadado.serieC <- metadata(all_series$code)
#
# metadado.serieD <- metadata(all_series$code[1:150])
#
# metadado.serieD <- metadata(all_series$code[1:150], TRUE)
#
# # Values ------------------------------------------------
# ipeadata <- function(code, quiet = FALSE) {
#
#   # Output
#   values <- dplyr::as_tibble(data.frame(NULL))
#
#   # Progress Bar settings
#   if (!quiet & (length(code) >= 5)) {
#     cat("Requesting Ipeadata API <http://www.ipeadata.gov.br/api/>")
#     cat('\n')
#     pb <- txtProgressBar(min = 0, max = length(code), style = 3)
#   }
#   update.step <- max(5, floor(length(code)/100))
#
#   # Retrieve metadata 1 by 1
#   for (i in 1:length(code)){
#
#     # Check
#     code0 <- gsub(" ", "_", toupper(code[i]))
#
#     # URL for metadata
#     url <- paste0("http://www.ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='", code0, "')")
#
#     # Extract from JSON
#     values.aux <- dplyr::as_tibble(jsonlite::fromJSON(url, flatten = TRUE)[[2]])
#
#     if (length(values.aux) > 0) {
#
#       # Sorting by ccode and date
#       values.aux %<>%
#         dplyr::mutate(TERCODIGO = as.integer(TERCODIGO)) %>%
#         dplyr::mutate(NIVNOME = as.factor(NIVNOME)) %>%
#         dplyr::mutate(VALDATA = lubridate::as_date(VALDATA)) %>%
#         dplyr::arrange(TERCODIGO, VALDATA)
#
#       # Concatenate rows
#       values <- rbind(values, values.aux)
#     } else {
#       warning(paste0("code '", code[i], "' not found"))
#     }
#
#     # Progress Bar
#     if (!quiet & (i %% update.step == 0 | i == length(code)) & (length(code) >= 5)) {
#       setTxtProgressBar(pb, i)
#     }
#   }
#
#   # Progress Bar closes
#   if (!quiet & (length(code) >= 5)) {
#     close(pb)
#   }
#
#   ## Starting: Remove NA >
#   ##           Rename variables >
#   ##           Add subtitles >
#   ##           Remove duplicates
#   values %<>%
#     dplyr::filter(!is.na(VALVALOR)) %>%
#     purrr::set_names(c('code', 'date', 'value', 'uname', 'ccode')) %>%
#     sjlabelled::set_label(c('Ipeadata Code', 'Date', 'Value',
#                             'Territorial Unit Name (PT-BR)',
#                             'Country or Territorial Code')) %>%
#     dplyr::distinct()
#
#   values
# }
#
# # Example:
# serieA <- ipeadata(c('abate_abpeav','AbInEe_VeLeTfC','VALOR366_FEDFUND366',
#                        'ABCD', 'CONSUMOTOT', '13T_BANAGUA'))
# serieB <- ipeadata('valor366 fedfund366')
#
# serieC <- ipeadata(all_series$code[1:56])
#
# # Change periodicity ------------------------------------------------
