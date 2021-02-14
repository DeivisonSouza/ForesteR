#' Simple Random Sampling
#'
#' Calcula as estimativas de amostragem aleatória simples para dados de inventário florestal.
#'
#' Um detalhe importante.
#'
#' @param data Conjunto de dados que contém as informações de inventário florestal.
#' @param x Um vetor numérico com a variável de interesse.
#' @param by Um fator para cálculo.
#' @param A Área total da população, em hectare.
#' @param a Área da unidade de amostra, em hectare.
#' @param LE Numérico. Limite de erro admissível. O padrão é 0.1, isto é, 10% da média amostral estimada.
#' @param FP Lógico. Fator de proporcionalidade para transformar para hectare. (default = FALSE)
#' @param DT Lógico. Imprime a saída no painel de visualização. (default = FALSE)
#' @param digits Inteiro. Número de casas decimais para imprimir os resultados. (default = 3)
#' @param ... Parâmetros adicionais
#'
#' @return Uma lista com estatística descritiva, estimativas de amostragem aleatória simples e informações de base usadas pelos estimadores.
#' @export
#'
#' @examples
#' \dontrun{
#' data(pinus)
#' RS(x = pinus$Volume, A = 40, a = 0.06, LE = 0.1, FP = F)
#'}
#'
#' @importFrom rlang .data

RS <- function(data = NULL, x, by = NULL, A, a, LE = 0.1, FP = FALSE, DT = FALSE, digits = 3, ...){
  #if(!is.data.frame(data)) stop("data must be a dataframe")

  if(!is.numeric(x) | is.integer(x) | is.character(x))
    stop("x must be a numeric vector")

  if(is.character(by) | is.numeric(by)) by <- as.factor(by)

  # Summary table per sample unit ===========================================
  if (!is.null(by)) {

    tb <- tibble::tibble(
      by = factor(by, levels = unique(by)),
      x = x) %>%
      dplyr::left_join(
        tibble::tibble(by = unique(by),
                       a = a),
        by = 'by') %>%
      dplyr::mutate(
        x = dplyr::case_when((FP == TRUE ) ~ x*(1/a),
                             (FP == FALSE) ~ x)) %>%
      dplyr::select(dplyr::all_of(c("by", "x")))
  }else{
    tb <- tibble::tibble(
      x = dplyr::case_when((FP == TRUE ) ~ x*(1/a),
                           (FP == FALSE) ~ x))
  }

  # Basic information for calculating estimates ============================
  if (!is.null(by)) {
    #gr <- rlang::syms("by")
    gr <- rlang::quo(by)
    gr1 <- rlang::quo(!by)
  }else{
    #gr <- rlang::syms(NULL)
    gr <- rlang::quo(NULL)
    gr1 <- rlang::quo(tidyselect::everything())
  }

  tbInf <- tb %>%
    dplyr::group_by(!! gr)%>%
    dplyr::summarise(count = dplyr::n(),
                     E = LE*mean(x, na.rm=TRUE),
                     t = stats::qt(1-.05/2, length(x)-1),
                     .groups = 'drop') %>%
    dplyr::mutate(A = A,
                  a = a,
                  N = A/a,
                  f = count/N,
                  fc = 1-f
    ) %>%
    dplyr::select(-count)

  # Descriptive statistics =================================================
  descBy <- tb %>%
    dplyr::group_by(!! gr) %>%
    dplyr::summarise(count = dplyr::n(),
                     Mean = mean(x, na.rm = TRUE),
                     sd = stats::sd(x),
                     var = stats::var(x),
                     cv = (stats::sd(x)/mean(x, na.rm = TRUE))*100,
                     min = min(x),
                     Q1 = stats::quantile(x, 0.25),
                     median = stats::median(x),
                     Q3 = stats::quantile(x, 0.75),
                     IQR = stats::IQR(x),
                     max = max(x),
                     kurt = moments::kurtosis(x),
                     skew = moments::skewness(x),
                     .groups = 'drop')

  tbDescBy <- descBy %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, digits)) %>%
    tidyr::pivot_longer(!! gr1,
                        names_to = "Parameters",
                        values_to = "Value")

  if(!is.null(by)){
    tbDescBy <- tbDescBy %>%
      tidyr::pivot_wider(names_from = !! gr,
                         values_from = Value)
  }

  # Parameters SRS ==========================================================

  if(!is.null(by)){
    tbInf2 <- tbInf %>%
      dplyr::left_join(descBy, by = "by") %>%
      dplyr::select(dplyr::all_of(c("by","fc","t","var","E","count","N","Mean")))
  }else{
    tbInf2 <- dplyr::bind_cols(tbInf, descBy) %>%
      dplyr::select(dplyr::all_of(c("fc","t","var","E","count","N","Mean")))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tbPar <- tbInf2 %>%
    dplyr::mutate(n = dplyr::case_when((fc >= 0.98) ~ ceiling((t^2*var)/E^2),
                                       (fc < 0.98) ~ ceiling((N*t^2*var)/(N*E^2 + t^2*var))),
                  nrec = dplyr::case_when((fc >= 0.98) ~ ceiling(((stats::qt(1-.05/2, round(n)-1))^2*var)/E^2),
                                          (fc < 0.98) ~ ceiling((N*(stats::qt(1-.05/2, round(n)-1))^2*var)/(N*E^2 + (stats::qt(1-.05/2, round(n)-1))^2*var))),
                  var.mean = dplyr::if_else(fc >= 0.98, var/count, (var/count)*fc),
                  std.mean = sqrt(var.mean),
                  ase = t*std.mean,
                  rse = (ase/Mean)*100,
                  lci = Mean - ase,
                  uci = Mean + ase,
                  totPop = dplyr::case_when((FP == TRUE) ~ A*Mean,
                                            (FP == FALSE) ~ N*Mean),
                  lcip = dplyr::case_when((FP == TRUE) ~ totPop - (A*ase),
                                          (FP == FALSE) ~ totPop - (N*ase)),
                  ucip = dplyr::case_when((FP == TRUE) ~ totPop + (A*ase),
                                          (FP == FALSE) ~ totPop + (N*ase))
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, digits))

  tbPar <- tbPar %>%
    tidyr::pivot_longer(!! gr1,
                        names_to = "Parameters",
                        values_to = "Value")

  if(!is.null(by)){
    tbPar <- tbPar %>%
      tidyr::pivot_wider(names_from = !! gr,
                         values_from = Value)
  }

  # Rename parameters =======================================================
  tbPar <- tbPar %>%
    dplyr::mutate(
      Parameters = dplyr::recode(Parameters,
                                 N = "Number of potential sample units",
                                 count = "Count",
                                 n = "Sample sufficiency",
                                 nrec = paste0(ifelse(is.null(by) == TRUE,
                                                      paste0("Sample sufficiency recalculation", " ", '(df = ',tbPar %>%
                                                               dplyr::filter(Parameters == 'n') %>%
                                                               .[[2]] %>%
                                                               round()-1,')'),
                                                      "Sample sufficiency recalculation")),
                                 Mean = "Sample mean",
                                 var.mean = "Variance of the mean",
                                 std.mean = "Standard error of the mean",
                                 ase = "Absolute sampling error",
                                 rse = "Relative sampling error",
                                 lci = "Lower confidence interval for the mean",
                                 uci = "Upper confidence interval for the mean",
                                 totPop = "Total population",
                                 lcip = "Lower confidence interval for the population",
                                 ucip = "Upper confidence interval for the population"))

  tbParOut <- tbPar %>%
    dplyr::filter(!Parameters %in% c("E","t","fc","var"))

  # DataTable in Viewer =====================================================
  if(DT == TRUE) {
    tbParOut %>% DT::datatable(editable = TRUE,
                               rownames = F,
                               extensions=c("Buttons",'ColReorder'),
                               options = list(
                                 colReorder = TRUE,
                                 pageLength = 20,
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'excel', 'pdf', I('colvis')),
                                 scroller = TRUE,
                                 searchHighlight = TRUE)
    ) %>%
      DT::formatRound(
        -1,
        digits = 2,
        mark = ",",
        dec.mark = getOption("OutDec")
      ) %>% print()
  }

  # Output ==================================================================
  result <- structure(list(Descriptive = tbDescBy,
                           Estimated = tbParOut,
                           BaseInfo = tbInf),
                      class = c("forester", "RS"))
  return(result)
}
