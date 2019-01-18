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
#' @param language String specifying the selected language. Language options are
#' English (\code{"en"}, default) and Brazilian portuguese (\code{"br"}).
#'
#' @return A data frame containing Ipeadata code, name, source,
#' frequency, last update and activity status of available series.
#'
#' @examples
#' # Available series (in English)
#' all_series <- available_series()
#'
#' # Available series (in Brazilian portuguese)
#' all_seriesBR <- available_series(language = "br")
#'
#' @note The original language of the available series’ names were preserved.
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
  ##           Sort by source, freq and code >
  ##           Transform in factor >
  ##           Transform in date >
  series <-
    jsonlite::fromJSON(url, flatten = TRUE)[[2]] %>%
    dplyr::as_tibble() %>%
    dplyr::select_(.dots = c('SERCODIGO', 'SERNOME', 'FNTSIGLA', 'PERNOME', 'SERATUALIZACAO', 'SERSTATUS')) %>%
    dplyr::arrange_(.dots = c('FNTSIGLA', 'PERNOME', 'SERCODIGO')) %>%
    dplyr::mutate_(FNTSIGLA = ~ as.factor(FNTSIGLA)) %>%
    dplyr::mutate_(SERATUALIZACAO = ~ lubridate::as_date(SERATUALIZACAO)) %>%
    dplyr::mutate_(SERSTATUS = ~ as.character(SERSTATUS)) %>%
    dplyr::mutate_(SERSTATUS = ~ dplyr::if_else(is.na(SERSTATUS), '', SERSTATUS))

    # Setting labels in selected language
    if (language == 'en') {

      series %<>%
        dplyr::mutate_(SERSTATUS = ~ factor(SERSTATUS,
                                           levels = c('A', 'I', ''),
                                           labels =  c('Active', 'Inactive', ''))) %>%
        dplyr::mutate_(PERNOME = ~ iconv(PERNOME, 'UTF-8', 'ASCII//TRANSLIT')) %>%
        dplyr::mutate_(PERNOME = ~ factor(PERNOME,
                                         levels = c('Anual', 'Decenal', 'Diaria',
                                                    'Mensal', 'Quadrienal', 'Quinquenal',
                                                    'Semestral', 'Trimestral', 'Nao se aplica'),
                                         labels = c('Yearly', 'Decennial', 'Daily',
                                                    'Monthly', 'Quadrennial', 'Quinquennial',
                                                    'Semiannual', 'Quarterly', 'Not applicable'))) %>%
        purrr::set_names(c('code', 'name', 'source',
                           'freq', 'lastupdate', 'status')) %>%
        sjlabelled::set_label(c('Ipeadata Code','Serie Name (PT-BR)','Source',
                                'Frequency','Last Update','Status'))

    } else {

      series %<>%
        dplyr::mutate_(SERSTATUS = ~ factor(SERSTATUS,
                                           levels = c('A', 'I', ''),
                                           labels =  c('Ativa', 'Inativa', ''))) %>%
        dplyr::mutate_(PERNOME = ~ factor(PERNOME)) %>%
        purrr::set_names(c('codigo', 'nome', 'fonte',
                           'freq', 'ultimaatualizacao', 'status')) %>%
        sjlabelled::set_label(c('Codigo Ipeadata','Nome da Serie','Fonte',
                                'Frequencia','Ultima Atualizacao','Status'))

    }

  series
}

# Available subjects ------------------------------------------------

#' @title List with available subjects
#'
#' @description Returns a list with available subjects from Ipeadata API database.
#'
#' @usage available_subjects(language = c("en", "br"))
#'
#' @param language String specifying the selected language. Language options are
#' English (\code{"en"}, default) and Brazilian portuguese (\code{"br"}).
#'
#' @return A data frame containing code and name of available subjects.
#'
#' @examples
#' # Available subjects (in English)
#' all_subjects <- available_subjects()
#'
#' # Available subjects (in Brazilian portuguese)
#' all_subjectsBR <- available_subjects(language = "br")
#'
#' @export

available_subjects <- function(language = c("en", "br")) {

  # Check language arg
  language <- match.arg(language)

  # URL for themes
  url <- 'http://www.ipeadata.gov.br/api/odata4/Temas/'

  ## Starting: Extract from JSON >
  ##           Transform to tbl >
  ##           Select variables >
  ##           Sort by code >
  ##           Transform in chr

  subjects <-
    data.frame(jsonlite::fromJSON(url, flatten = TRUE)[[2]]) %>%
    dplyr::as_tibble() %>%
    dplyr::select_(.dots = c('TEMCODIGO', 'TEMNOME')) %>%
    dplyr::arrange_(.dots = c('TEMCODIGO')) %>%
    dplyr::mutate_(TEMNOME = ~ as.character(TEMNOME))

  # Setting labels in selected language
  if (language == 'en') {

    subjects %<>%
      dplyr::mutate_(TEMNOME = ~ iconv(TEMNOME, 'UTF-8', 'ASCII//TRANSLIT')) %>%
      dplyr::mutate_(TEMNOME = ~ factor(TEMNOME,
                                       levels = c('Producao', 'Consumo e vendas', 'Moeda e credito', 'Juros', 'Comercio exterior',
                                                  'Financas publicas', 'Cambio', 'Contas nacionais', 'Precos', 'Balanco de pagamentos',
                                                  'Economia internacional', 'Emprego', 'Salario e renda', 'Populacao', 'Indicadores sociais',
                                                  'Projecoes', 'Sinopse macroeconomica', 'Eleicoes', 'Estoque de capital', 'Seguranca Publica',
                                                  'Assistencia social', 'Correcao monetaria', 'Avaliacao do governo', 'Vendas',
                                                  'Percepcao e expectativa', 'Agropecuaria', 'Educacao', 'Renda', 'Habitacao',
                                                  'Geografico', 'Transporte', 'Demografia', 'Desenvolvimento humano', 'Financeiras',
                                                  'Mercado de trabalho', 'Saude', 'Deputado Estadual', 'Deputado Federal', 'Governador',
                                                  'Prefeito', 'Presidente', 'Senador', 'Vereador', 'Eleitorado', 'IDHm2010',
                                                  'IDHm2000', 'IDHm1991', 'Contas Regionais'),
                                       labels = c('Production', 'Consumption and sales', 'Currency and credit', 'Interest',
                                                  'Foreign trade', 'public finances', 'Exchange', 'National Accounts',
                                                  'Prices', 'Balance of payments', 'International economy', 'Employment',
                                                  'Salary and income', 'Population', 'Social indicators', 'Projections',
                                                  'Macroeconomic synopsis', 'Elections', 'Capital stock', 'Public security',
                                                  'Social assistance', 'Monetary correction', 'Government evaluation', 'Sales',
                                                  'Perception and expectation', 'Agriculture and animal husbandry', 'Education',
                                                  'Income', 'Housing', 'Geographical', 'Transport', 'Demography', 'Human development',
                                                  'Financial', 'Labor market', 'Health', 'State deputy', 'Federal deputy',
                                                  'Governor', 'Mayor', 'President', 'Senator', 'Alderman', 'Electorate', 'mHDI2010',
                                                  'mHDI2000', 'mHDI1991', 'Regional Accounts'))) %>%
      purrr::set_names(c('scode', 'sname')) %>%
      sjlabelled::set_label(c('Subject Code','Subject Name'))

  } else {

    subjects %<>%
      dplyr::mutate_(TEMNOME = ~ factor(TEMNOME)) %>%
      purrr::set_names(c('scodigo', 'snome')) %>%
      sjlabelled::set_label(c('Codigo do Tema','Nome do Tema'))

  }

  subjects
}

# Available countries ------------------------------------------------

#' @title List with available countries
#'
#' @description Returns a list with available countries from Ipeadata API database.
#'
#' @usage available_countries(language = c("en", "br"))
#'
#' @param language String specifying the selected language. Language options are
#' English (\code{"en"}, default) and Brazilian portuguese (\code{"br"}).
#'
#' @return A data frame containing ISO 3 code and name of available countries.
#'
#' @examples
#' # Available countries (in English)
#' all_countries <- available_countries()
#'
#' # Available countries (in Brazilian portuguese)
#' all_countriesBR <- available_countries(language = "br")
#'
#' @export

available_countries <- function(language = c("en", "br")) {

  # Check language arg
  language <- match.arg(language)

  # URL for countries
  url <- 'http://www.ipeadata.gov.br/api/odata4/Paises/'

  ## Starting: Extract from JSON >
  ##           Transform to tbl >
  ##           Sort by code >

  countries <-
    jsonlite::fromJSON(url, flatten = TRUE)[[2]] %>%
    dplyr::as_tibble() %>%
    dplyr::arrange_(.dots = c('PAICODIGO'))

  # Setting labels in selected language
  if (language == 'en') {

    countries %<>%
      dplyr::mutate_(PAINOME = ~ iconv(PAINOME, 'UTF-8', 'ASCII//TRANSLIT')) %>%
      dplyr::mutate_(PAINOME = ~ factor(PAINOME,
                                       levels = c('Angola', 'Emirados Arabes Unidos', 'Argentina', 'Sudeste Asiatico', 'Australia',
                                                  'Austria', 'Belgica', 'Bahamas', 'Bolivia', 'Brasil', 'Canada', 'Suica', 'Chile',
                                                  'China', 'Congo', 'Colombia', 'Cabo Verde', 'Republica Tcheca', 'Alemanha',
                                                  'Dinamarca', 'Republica Dominicana', 'Argelia', 'Equador', 'Egito', 'Espanha',
                                                  'Uniao Europeia', 'Finlandia', 'Franca', 'Gra-Bretanha (Reino Unido, UK)',
                                                  'Guine-Bissau', 'Grecia', 'Hong Kong', 'Haiti', 'Hungria', 'Indonesia',
                                                  'India', 'Paises industrializados', 'Irlanda', 'Ira', 'Iraque', 'Islandia',
                                                  'Israel', 'Italia', 'Japao', 'Coreia do Sul', 'America Latina', 'Santa Lucia',
                                                  'Luxemburgo', 'Macau', 'Marrocos', 'Mexico', 'Myanma (Ex-Burma)', 'Mocambique',
                                                  'Malasia', 'Nigeria', 'Holanda', 'Noruega', 'Nova Zelandia', 'Peru', 'Filipinas',
                                                  'Polonia', 'Portugal', 'Paraguai', 'Qatar', 'Leste Europeu e Russia', 'Romenia',
                                                  'Federacao Russa', 'Arabia Saudita', 'Cingapura', 'Sao Tome e Principe', 'Eslovenia',
                                                  'Suecia', 'Tailandia', 'Timor Leste (Ex-East Timor)', 'Trinidad and Tobago', 'Taiwan',
                                                  'Paises em desenvolvimento', 'Uruguai', 'Estados Unidos', 'Venezuela', 'Mundial',
                                                  'Iemen', 'Africa do Sul', 'Zona do Euro'),
                                       labels = c('Angola', 'United Arab Emirates', 'Argentina', 'Southeast Asia', 'Australia', 'Austria',
                                                  'Belgium', 'Bahamas', 'Bolivia', 'Brazil', 'Canada', 'Switzerland', 'Chile',
                                                  'China', 'Congo', 'Colombia', 'Cape Verde', 'Czech republic', 'Germany',
                                                  'Denmark', 'Dominican Republic', 'Algeria', 'Ecuador', 'Egypt', 'Spain',
                                                  'European Union', 'Finland', 'France', 'Great Britain (United Kingdom, UK)', 'Guinea Bissau',
                                                  'Greece', 'Hong Kong', 'Haiti', 'Hungary', 'Indonesia', 'India', 'Developed countries',
                                                  'Ireland', 'Iran', 'Iraq', 'Iceland', 'Israel', 'Italy', 'Japan', 'South Korea',
                                                  'Latin America', 'Saint Lucia', 'Luxembourg', 'Macao', 'Morocco', 'Mexico', 'Myanmar',
                                                  'Mozambique', 'Malaysia', 'Nigeria', 'Netherlands', 'Norway', 'New Zealand', 'Peru',
                                                  'Philippines', 'Poland', 'Portugal', 'Paraguay', 'Qatar', 'Eastern Europe and Russia',
                                                  'Romania', 'Russian Federation', 'Saudi Arabia', 'Singapore', 'Sao Tome and Principe',
                                                  'Slovenia', 'Sweden', 'Thailand', 'East Timor', 'Trinidad and Tobago', 'Taiwan',
                                                  'Developing countries', 'Uruguay','United States of America', 'Venezuela', 'World',
                                                  'Yemen', 'South Africa', 'Euro Area'))) %>%
      dplyr::mutate_(PAINOME = ~ as.character(PAINOME)) %>%
      purrr::set_names(c('tcode', 'tname')) %>%
      sjlabelled::set_label(c('Country Code (ISO 3)','Country Name'))

  } else {

    countries %<>%
      purrr::set_names(c('tcodigo', 'tnome')) %>%
      sjlabelled::set_label(c('Codigo do Pais (ISO 3)','Nome do Pais'))

  }

  countries
}

# Available territories ------------------------------------------------

#' @title List with available territorial divisions
#'
#' @description Returns a list with available Brazilian territorial
#'  divisions from Ipeadata API database.
#'
#' @usage available_territories(language = c("en", "br"))
#'
#' @param language String specifying the selected language. Language options are
#' English (\code{"en"}, default) and Brazilian portuguese (\code{"br"}).
#'
#' @return A data frame containing unit name, code, name and area (in km2)
#'  of Brazilian territorial divisions.
#'
#' @examples
#' # Available territories (in English)
#' all_territories <- available_territories()
#'
#' # Available territories (in Brazilian portuguese)
#' all_territoriesBR <- available_territories(language = "br")
#'
#' @export

available_territories <- function(language = c("en", "br")) {

  # Check language arg
  language <- match.arg(language)

  # URL for territories
  url <- 'http://www.ipeadata.gov.br/api/odata4/Territorios/'

  ## Starting: Extract from JSON >
  ##           Transform to tbl >
  ##           Select variables >
  ##           Remove NA >
  ##           Sort by uname >
  ##           Rename variables >
  ##           Add subtitles

  territories <-
    jsonlite::fromJSON(url, flatten = TRUE)[[2]] %>%
    dplyr::as_tibble() %>%
    dplyr::select_(.dots = c('NIVNOME', 'TERCODIGO', 'TERNOME', 'TERAREA')) %>%
    dplyr::filter_(~ !is.na(TERAREA)) %>%
    dplyr::arrange_(.dots = c('TERCODIGO'))

  # Setting labels in selected language
  if (language == 'en') {

    territories %<>%
      dplyr::mutate_(NIVNOME = ~ iconv(NIVNOME, 'UTF-8', 'ASCII//TRANSLIT')) %>%
      dplyr::mutate_(NIVNOME = ~ factor(NIVNOME,
                                       levels = c('Brasil', 'Regioes', 'Estados', 'Mesorregioes', 'Microrregioes',
                                                   'Estado/RM', 'Area metropolitana', 'Municipios',
                                                  'AMC 91-00', 'AMC 70-00', 'AMC 60-00', 'AMC 40-00',
                                                  'AMC 20-00', 'AMC 1872-00'),
                                       labels = c('Brazil', 'Regions', 'States', 'Mesoregions', 'Microregions',
                                                  'State/Metropolitan region', 'Metropolitan area', 'Municipality',
                                                  'MCA 91-00', 'MCA 70-00', 'MCA 60-00', 'MCA 40-00',
                                                  'MCA 20-00', 'MCA 1872-00'), ordered = TRUE)) %>%
      dplyr::arrange_(.dots = c('NIVNOME')) %>%
      purrr::set_names(c('uname', 'tcode', 'tname', 'area')) %>%
      sjlabelled::set_label(c('Territorial Unit Name',
                              'Territorial Code','Territorial Name','Area (Km2)'))

  } else {

    territories %<>%
      dplyr::mutate_(NIVNOME = ~ factor(NIVNOME,
                                       levels = levels(factor(NIVNOME))[ c(
                                                                           which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == ''),
                                                                           which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'Brasil'),
                                                                           which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'Regioes'),
                                                                           which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'Estados'),
                                                                           which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'Mesorregioes'),
                                                                           which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'Microrregioes'),
                                                                           which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'Estado/RM'),
                                                                           which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'Area metropolitana'),
                                                                           which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'Municipios'),
                                                                           which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'AMC 91-00'),
                                                                           which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'AMC 70-00'),
                                                                           which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'AMC 60-00'),
                                                                           which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'AMC 40-00'),
                                                                           which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'AMC 20-00'),
                                                                           which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'AMC 1872-00')
                                                                      )], ordered = TRUE)) %>%
      dplyr::arrange_(.dots = c('NIVNOME')) %>%
      purrr::set_names(c('unome', 'tcodigo', 'tnome', 'area')) %>%
      sjlabelled::set_label(c('Nome da Unidade Territorial',
                              'Codigo Territorial','Nome do Territorio','Area (Km2)'))

  }

  territories
}

# Metadata ------------------------------------------------

#' @title Returns a metadata about the requested series
#'
#' @description Returns a list with metadata information about the requested series.
#'
#' @usage metadata(code, language = c("en", "br"), quiet = FALSE)
#'
#' @param code Vector with Ipeadata code.
#' @param language String specifying the selected language. Language options are
#' English (\code{"en"}, default) and Brazilian portuguese (\code{"br"}).
#' @param quiet Logical. If \code{FALSE} (default), a progress bar is shown.
#'
#' @return A data frame containing Ipeadata code, name, short comment, last update, theme name,
#'  source's name and full name, source's URL, frequency, unity, multiplier factor, status,
#'  subject code and the country or territorial code of requested series.
#'
#' @examples
#' # Metadata about
#' # "JPM366_EMBI366": J.P. Morgan Emerging Markets Bond Index (EMBI+), Brazil
#' # "SGS366_NASDAQ366": Nasdaq Composite Index - closed
#' meta <- metadata(code = c("JPM366_EMBI366", "SGS366_NASDAQ366"))
#'
#' # Metadata about
#' # "PRECOS12_IPCA12": Extended National Consumer Price Index (IPCA), Brazil
#' # in Brazilian portuguese
#' metaBR <- metadata(code = "PRECOS12_IPCA12", language = "br")
#'
#' # Regional metadata about
#' # "CONSUMOTOT": Electric energy consumption, Brazil
#' dataReg <- ipeadata(code = "CONSUMOTOT")
#'
#' @note The original language of the available series' names and the comments were preserved.
#' The Ipeadata codes may be required by \code{available_series()}.
#'
#' @seealso \code{\link{available_series}}, \code{\link{available_subjects}},
#' \code{\link{available_territories}}
#'
#' @references This R package uses the Ipeadata API.
#' For more information go to \url{http://www.ipeadata.gov.br/api/}.
#'
#' @export
#'
#' @importFrom utils setTxtProgressBar txtProgressBar

metadata <- function(code, language = c("en", "br"), quiet = FALSE) {

  # Check language arg
  language <- match.arg(language)

  # Output
  metadata <- dplyr::as_tibble(data.frame(NULL))

  # Progress Bar settings
  if (!quiet & (length(code) >= 2)) {
    cat("Requesting Ipeadata API <http://www.ipeadata.gov.br/api/>")
    cat('\n')
    pb <- txtProgressBar(min = 0, max = length(code), style = 3)
  }
  update.step <- max(2, floor(length(code)/100))

  # Retrieve metadata 1 by 1
  for (i in 1:length(code)) {

    # Check
    code0 <- gsub(" ", "_", toupper(code[i]))

    # URL for metadata
    url <- paste0("http://www.ipeadata.gov.br/api/odata4/Metadados('", code0,"')")

    # Extract from JSON
    metadata.aux <- jsonlite::fromJSON(url, flatten = TRUE)[[2]]

    if (length(metadata.aux) > 0) {

      ## Starting: Transform to tbl >
      ##           Select variables >
      metadata.aux %<>%
        dplyr::as_tibble() %>%
        dplyr::select_(.dots = c('-SERNUMERICA'))

      # Concatenate rows
      metadata <- rbind(metadata,metadata.aux)
    } else {
      warning(paste0("code '", code[i], "' not found"))
    }

    # Progress Bar
    if (!quiet & (i %% update.step == 0 | i == length(code)) & (length(code) >= 2)) {
      setTxtProgressBar(pb, i)
    }
  }

  # Progress Bar closes
  if (!quiet & (length(code) >= 2)) {
    close(pb)
  }

  ## Starting: Transform in date >
  ##           Transform in factor >
  ##           Transform in factor >
  ##           Transform in chr >
  ##           Replace missing status >

  metadata %<>%
    dplyr::mutate_(SERATUALIZACAO = ~ lubridate::as_date(SERATUALIZACAO)) %>%
    dplyr::mutate_(FNTSIGLA = ~ as.factor(FNTSIGLA)) %>%
    dplyr::mutate_(FNTNOME = ~ as.factor(FNTNOME)) %>%
    dplyr::mutate_(SERSTATUS = ~ as.character(SERSTATUS)) %>%
    dplyr::mutate_(SERSTATUS = ~ dplyr::if_else(is.na(SERSTATUS), '', SERSTATUS))

  # Setting labels in selected language
  if (language == 'en') {

    metadata %<>%
      dplyr::mutate_(BASNOME = ~ iconv(BASNOME, 'UTF-8', 'ASCII//TRANSLIT')) %>%
      dplyr::mutate_(BASNOME = ~ factor(BASNOME,
                                       levels = c('Macroeconomico', 'Regional', 'Social'),
                                       labels = c('Macroeconomic', 'Regional', 'Social'))) %>%
      dplyr::mutate_(UNINOME = ~ iconv(UNINOME, 'UTF-8', 'ASCII//TRANSLIT')) %>%
      dplyr::mutate_(UNINOME = ~ factor(UNINOME,
                                       levels = c("-", "Tonelada", "R$", "GWh", "US$", "Euro", "Unidade", "Metro cubico",
                                                  "Marco alemao", "Franco frances", "Franco frances", "Libra esterlina",
                                                  "Dolar canadense", "Florim holandes", "Franco belga", "Peso argentino",
                                                  "Peso chileno", "Peseta espanhola", "Peso novo mexicano", "Peso uruguaio",
                                                  "Won sul-coreano", "Cabeca", "Litro", "Duzia", "Cacho", "Ouro", "Iene japones",
                                                  "Guarani paraguaio", "Conto de reis", "Km", "Km2", "Mil barris/dia", "NCZ$",
                                                  "Ouro oncas", "Pessoa", "R$/MWh", "R$/US$", "Indice", "Porcentagem",
                                                  "Habitante", "Ano", "R$, a precos do ano 2000", "(% a.m.)", "(% PIB)",
                                                  "(% a.a.)", "(%)", "Metro", "Caixa", "Cr$", "Barril", "Bolivar venezuelano",
                                                  "Peso colombiano", "CV", "R$ de 2000/HA", "R$ de 2000/T/KM", "Reis",
                                                  "US$ de 2008", "Passageiro-quilometro", "TEU", "Assento-quilometro",
                                                  "Tonelada-quilometro", "Hora", "determinado", "R$ Outubro 2009", "Qde.",
                                                  "Coroa dinamarquesa", "Dolar cingapurense", "Rupia cingalesa", "Dolar taiwanes",
                                                  "Nro", "R$ Janeiro 2012", "Sacas de 60 kg", "R$ Outubro 2012", "R$ de 2013",
                                                  "US$ de 2013", "R$ de outubro 2012", "Meses", "R$ de 2010", "R$ Outubro 2013",
                                                  "R$ de 2000", "R$ (do ultimo mes)", "R$ Outubro 2014", "R$ de 2014",
                                                  "R$ Penultimo mes da serie", "Hectare", "Razao", "Domicilios", "R$ de 2001",
                                                  "Yuan", "Grau", "Eleitor", "Voto", "Numero", "(Eleitor)", "(Voto)", "(Unidade)",
                                                  "R$/Hrs", "Pence", "MW", "estabelecimento", "R$ de 1999", "Salario Minimo",
                                                  "(p.p.)", "Dias", "R$ de 1980",  "R$ de 1995", "Conto de reis de 1947",
                                                  "US$ de 1995", "Coroa norueguesa", "Coroa sueca", "Franco suico",
                                                  "Dolar australiano", "Dolar da nova zelandia", "Rand", "Peso boliviano",
                                                  "Peso dominicano", "Sucre", "Sol novo", "Dolar das bahamas", "Franco CFA",
                                                  "Dolar de trindad e tobago", "Rial iraniano", "Dinar iraquiano", "Shekel novo",
                                                  "Rial saudita", "Libra egipcia", "Rial iemenita", "Dolar de hong kong", "Rupia indiana",
                                                  "Rupia", "Ringgit malaio", "Peso filipino", "Dolar de cingapura", "Baht", "Dinar argelino",
                                                  "Kuanza reajustavel", "Dirra marroquino", "Naira", "Rublo", "Tolar", "Leu romeno",
                                                  "Libra esterlina de 1913", "(Sigla)", "Kg", "Pessoas", "Horas", "US$ FOB", "Libra irlandesa",
                                                  "Escudo", "zloty", "US$ de 2005", "MWh", "Tep", "Razao/relacao", "Quantidade", "Real",
                                                  "SM", "?C", "mm/mes"),
                                       labels = c("-", "Ton", "R$", "GWh", "US$", "Euro", "Unit", "Cubic meter", "Deutsche mark",
                                                  "French franc", "Italian lira", "Pound sterling", "Canadian dollar",
                                                  "Dutch guilder", "Belgian franc", "Argentine peso", "Chilean peso", "Spanish peseta",
                                                  "Mexican new peso", "Uruguayan peso", "South Korean won", "Head", "Liter", "Dozen",
                                                  "Bunch", "Gold", "Japanese yen", "Paraguayan guarani", "Conto de reis", "Km", "Km2",
                                                  "Thousand barrels per day", "Brazilian cruzado novo", "Gold per ounce", "Person",
                                                  "R$/MWh", "R$/US$", "Index", "Percentage", "Inhabitant", "Year",
                                                  "R$, constant prices of 2000", "(% p.m.)", "(% GDP)", "(% p.y.)", "(%)", "Meter",
                                                  "Matchbox", "Cr$", "Barrel", "Venezuelan bolivar soberano", "Colombian peso",
                                                  "hp", "R$, constant prices of 2000 per hectare",
                                                  "R$, constant prices of 2000, R$ T/km", "Reis", "US$, constant prices of 2008",
                                                  "Passenger per km", "TEU", "Seat per kilometer", "Tonne per kilometer", "Hour",
                                                  "Determined", "R$, constant prices of October 2000", "Quantity", "Danish krone",
                                                  "Singapore dollar", "Sri Lankan rupee", "New Taiwan dollar", "Number",
                                                  "R$, constant prices of January 2012", "60 kg Sacks",
                                                  "R$, constant prices of October 2012", "R$, constant prices of 2013",
                                                  "US$, constant prices of 2013", "R$, constant prices of October 2012",
                                                  "Month", "R$, constant prices of 2010", "R$, constant prices of October 2013",
                                                  "R$, constant prices of 2000", "R$ (last month)",
                                                  "R$, constant prices of October 2014", "R$, constant prices of 2014",
                                                  "R$ (penultimate month of serie)", "Hectare", "Ratio", "Residence",
                                                  "R$, constant prices of 2001", "Yuan", "Degree", "Voter", "Vote", "Number",
                                                  "(Eleitor)", "(Voto)", "(Unity)", "R$ per hour", "Pence", "MW", "Establishment",
                                                  "R$, constant prices of 1999", "Basic salary", "(p.p.)", "Days", "R$, constant prices of 1980",
                                                  "R$, constant prices of 1995", "Conto de reis, constant prices of 1947",
                                                  "US$, constant prices of 1995", "Norwegian krone", "Swedish krona", "Swiss franc",
                                                  "Australian dollar", "New Zealand dollar", "South African rand", "Bolivian peso",
                                                  "Dominican peso", "Ecuadorian sucre", "Peruvian new sol", "Bahamian dollar", "CFA franc",
                                                  "Trinidad and Tobago dollar", "Iranian rial", "Iraqi dinar", "Israeli new shekel",
                                                  "Saudi riyal", "Egyptian pound", "Egyptian pound", "Hong Kong dollar", "Indian Rupee",
                                                  "Rupee", "Malaysian ringgit", "Philippine peso", "Singapore dollar", "Thai baht",
                                                  "Algerian dinar", "Angolan readjusted Kwanza", "Moroccan dirham", "Nigerian naira",
                                                  "Russian ruble", "Slovenian tolar", "Romanian leu",
                                                  "Pound sterling, constant prices of 1913", "(acronyms)", "Kg", "People", "Hour",
                                                  "US$ FOB", "Irish pound", "Escudo", "Polish zloty", "US$, constant prices of 2005",
                                                  "MWh", "toe", "Ratio/relation", "Quantity", "Real", "Basic salary", "Celsius Degree", "mm/month"))) %>%
      dplyr::mutate_(PERNOME = ~ iconv(PERNOME, 'UTF-8', 'ASCII//TRANSLIT')) %>%
      dplyr::mutate_(PERNOME = ~ factor(PERNOME,
                                       levels = c('Anual', 'Decenal', 'Diaria',
                                                  'Mensal', 'Quadrienal', 'Quinquenal',
                                                  'Semestral', 'Trimestral', 'Nao se aplica'),
                                       labels = c('Yearly', 'Decennial', 'Daily',
                                                  'Monthly', 'Quadrennial', 'Quinquennial',
                                                  'Semiannual', 'Quarterly', 'Not applicable'))) %>%
      dplyr::mutate_(MULNOME = ~ iconv(MULNOME, 'UTF-8', 'ASCII//TRANSLIT')) %>%
      dplyr::mutate_(MULNOME = ~ factor(MULNOME,
                                       levels =   c('mil', 'milhoes', 'bilhoes', 'centavos',
                                                    'milhares', 'trilhoes', 'centenas',
                                                    'centena de milhao'),
                                       labels = c( 'Thousand', 'Millions', 'Billions', 'Cents',
                                                   'Thousands', 'Trillions', 'Hundreds',
                                                   'Hundred million'))) %>%
      dplyr::mutate_(SERSTATUS = ~ factor(SERSTATUS,
                                         levels = c('A', 'I', ''),
                                         labels = c('Active', 'Inactive', ''))) %>%
      purrr::set_names(c('code', 'name', 'comment', 'lastupdate', 'bname', 'source', 'sourcename',
                         'sourceurl', 'freq', 'unity', 'mf', 'status',
                         'scode', 'tcode')) %>%
      sjlabelled::set_label(c('Ipeadata Code','Serie Name (PT-BR)', 'Comment (PT-BR)', 'Last Update',
                              'Theme name', 'Source', 'Source Name', 'Source URL',
                              'Frequency', 'Unity', 'Multiplier Factor', 'Status',
                              'Subject Code', 'Country or Territorial Code'))

  } else {

    metadata %<>%
      dplyr::mutate_(BASNOME = ~ factor(BASNOME)) %>%
      dplyr::mutate_(UNINOME = ~ factor(UNINOME)) %>%
      dplyr::mutate_(PERNOME = ~ factor(PERNOME)) %>%
      dplyr::mutate_(MULNOME = ~ factor(MULNOME)) %>%
      dplyr::mutate_(SERSTATUS = ~ factor(SERSTATUS,
                                         levels = c('A', 'I', ''),
                                         labels = c('Ativa', 'Inativa', ''))) %>%
      purrr::set_names(c('codigo', 'nome', 'coment', 'ultimaatualizacao', 'bnome', 'fonte', 'fontenome',
                         'fonteurl', 'freq', 'unid', 'fm', 'status',
                         'scodigo', 'tcodigo')) %>%
      sjlabelled::set_label(c('Codigo Ipeadata','Nome da Serie (PT-BR)', 'Comentario', 'Ultima Atualizacao',
                              'Nome da Base', 'Fonte', 'Nome da Fonte', 'URL da Fonte',
                              'Frequencia', 'Unidade', 'Fator Multiplicador', 'Status',
                              'Codigo do Tema', 'Codigo de Pais ou Territorial'))

  }

  metadata
}

# Values ------------------------------------------------

#' @title Returns a database about the requested series
#'
#' @description Returns a list with available database about the requested series.
#'
#' @usage ipeadata(code, language = c("en", "br"), quiet = FALSE)
#'
#' @param code Vector with Ipeadata code.
#' @param language String specifying the selected language. Language options are
#' English (\code{"en"}, default) and Brazilian portuguese (\code{"br"}).
#' @param quiet Logical. If \code{FALSE} (default), a progress bar is shown.
#'
#' @return A data frame containing Ipeadata code, date, value, territorial unit name
#' and country or territorial code of requested series.
#'
#' @note The Ipeadata codes may be required by \code{available_series()}.
#'
#' @seealso \code{\link{available_series}}, \code{\link{available_territories}}
#'
#' @examples
#' # Data from
#' # "JPM366_EMBI366": J.P. Morgan Emerging Markets Bond Index (EMBI+), Brazil
#' # "SGS366_NASDAQ366": Nasdaq Composite Index - closed
#' data <- ipeadata(code = c("JPM366_EMBI366", "SGS366_NASDAQ366"))
#'
#' # Data from
#' # "PRECOS12_IPCA12": Extended National Consumer Price Index (IPCA), Brazil
#' # in Brazilian portuguese
#' dataBR <- ipeadata(code = "PRECOS12_IPCA12", language = "br")
#'
#' # Regional data from
#' # "CONSUMOTOT": Electric energy consumption, Brazil
#' dataReg <- ipeadata(code = "CONSUMOTOT")
#'
#' @references This R package uses the Ipeadata API.
#' For more information go to \url{http://www.ipeadata.gov.br/api/}.
#'
#' @export
#'
#' @importFrom utils setTxtProgressBar txtProgressBar

ipeadata <- function(code, language = c("en", "br"), quiet = FALSE) {

  # Check language arg
  language <- match.arg(language)

  # Output
  values <- dplyr::as_tibble(data.frame(NULL))

  # Progress Bar settings
  if (!quiet & (length(code) >= 2)) {
    cat("Requesting Ipeadata API <http://www.ipeadata.gov.br/api/>")
    cat('\n')
    pb <- txtProgressBar(min = 0, max = length(code), style = 3)
  }
  update.step <- max(2, floor(length(code)/100))

  # Retrieve metadata 1 by 1
  for (i in 1:length(code)){

    # Check
    code0 <- gsub(" ", "_", toupper(code[i]))

    # URL for metadata
    url <- paste0("http://www.ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='", code0, "')")

    # Extract from JSON
    values.aux <- dplyr::as_tibble(jsonlite::fromJSON(url, flatten = TRUE)[[2]])

    if (length(values.aux) > 0) {

      # Sorting by ccode and date
      values.aux %<>%
        dplyr::mutate_(TERCODIGO = ~ as.integer(TERCODIGO)) %>%
        dplyr::mutate_(NIVNOME = ~ as.factor(NIVNOME)) %>%
        dplyr::mutate_(VALDATA = ~ lubridate::as_date(VALDATA)) %>%
        dplyr::arrange_(.dots = c('TERCODIGO', 'VALDATA'))

      # Concatenate rows
      values <- rbind(values, values.aux)
    } else {
      warning(paste0("code '", code[i], "' not found"))
    }

    # Progress Bar
    if (!quiet & (i %% update.step == 0 | i == length(code)) & (length(code) >= 2)) {
      setTxtProgressBar(pb, i)
    }
  }

  # Progress Bar closes
  if (!quiet & (length(code) >= 2)) {
    close(pb)
  }

  ## Starting: Remove NA >
  ##           Rename variables >
  ##           Add subtitles >
  ##           Remove duplicates
  values %<>%
    dplyr::filter_(~ !is.na(VALVALOR)) %>%
    dplyr::distinct()

  # Setting labels in selected language
  if (language == 'en') {

    values %<>%
      dplyr::mutate_(NIVNOME = ~ iconv(NIVNOME, 'UTF-8', 'ASCII//TRANSLIT')) %>%
      dplyr::mutate_(NIVNOME = ~ factor(NIVNOME,
                                       levels = c('Brasil', 'Regioes', 'Estados', 'Mesorregioes', 'Microrregioes',
                                                  'Estado/RM', 'Area metropolitana', 'Municipios',
                                                  'AMC 91-00', 'AMC 70-00', 'AMC 60-00', 'AMC 40-00',
                                                  'AMC 20-00', 'AMC 1872-00', ''),
                                       labels = c('Brazil', 'Regions', 'States', 'Mesoregions', 'Microregions',
                                                  'State/Metropolitan region', 'Metropolitan area', 'Municipality',
                                                  'MCA 91-00', 'MCA 70-00', 'MCA 60-00', 'MCA 40-00',
                                                  'MCA 20-00', 'MCA 1872-00', ''), ordered = TRUE)) %>%
      purrr::set_names(c('code', 'date', 'value', 'uname', 'tcode')) %>%
      sjlabelled::set_label(c('Ipeadata Code', 'Date', 'Value',
                              'Territorial Unit Name',
                              'Country or Territorial Code'))

  } else {

    values %<>%
      dplyr::mutate_(NIVNOME = ~ factor(NIVNOME,
                                       levels = levels(factor(NIVNOME))[ c(
                                         which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == ''),
                                         which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'Brasil'),
                                         which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'Regioes'),
                                         which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'Estados'),
                                         which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'Mesorregioes'),
                                         which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'Microrregioes'),
                                         which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'Estado/RM'),
                                         which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'Area metropolitana'),
                                         which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'Municipios'),
                                         which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'AMC 91-00'),
                                         which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'AMC 70-00'),
                                         which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'AMC 60-00'),
                                         which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'AMC 40-00'),
                                         which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'AMC 20-00'),
                                         which(iconv(levels(factor(NIVNOME)), 'UTF-8', 'ASCII//TRANSLIT') == 'AMC 1872-00')
                                       )], ordered = TRUE)) %>%
      purrr::set_names(c('codigo', 'data', 'valor', 'unome', 'tcodigo')) %>%
      sjlabelled::set_label(c('Codigo Ipeadata', 'Data', 'Valor',
                              'Nome da Unidade Territorial',
                              'Codigo de Pais ou Territorial'))

  }

  values
}

# Change frequency ------------------------------------------------
