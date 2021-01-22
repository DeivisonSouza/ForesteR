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
#' @param FP Lógico. Fator de proporcionalidade para transformar para hectare. (default = FALSE)
#' @param DT Lógico. Imprime a saída no painel de visualização. (default = FALSE)
#' @param ... Parâmetros adicionais
#'
#' @return Uma lista com estatística descritiva, estimativas de amostragem aleatória simples e informações de base para cálculo das estimativas.
#' @export
#'
#' @examples
#' \dontrun{
#' data(pinus)
#' SRS(x = pinus$Volume, A = 40, a = 0.06, FP = T, DT = F)
#'}
#'
#' @importFrom rlang .data

SRS <- function(data = NULL, x, by = NULL, A, a, FP = FALSE, DT = FALSE, ...){
  #if(!is.data.frame(data)) stop("data must be a dataframe")

  if(!is.numeric(x) | is.integer(x) | is.character(x))
    stop("x must be a numeric vector")

  if(is.character(by)) by <- as.factor(by)

  if (!is.null(by)) {

    if (length(A) != length(a) |
        length(unique(by)) != length(a) |
        length(unique(by)) != length(A))
      stop("Lengths of vectors (with unique elements) `by`, `A` and `a` must agree.")

    tb <- tibble::tibble(x = x,
                         by = factor(by, levels = unique(by)))

    tbInf <- tb %>%
      dplyr::group_by(by) %>%
      dplyr::summarise(count = dplyr::n(),
                       E = 0.1*mean(x, na.rm=TRUE),
                       t = stats::qt(1-.05/2, length(x)-1),
                       .groups = 'drop') %>%
      dplyr::mutate(A = A,
                    a = a,
                    N = A/a,
                    f = count/N,
                    fc = 1-f
      ) %>%
      dplyr::select(-count)

    if (FP == TRUE) {
      tbInf <- tbInf %>%
        dplyr::mutate(fp = 1/a) %>%
        dplyr::select(-E)

      tb <- tbInf %>%
        dplyr::left_join(tb, by = 'by') %>%
        dplyr::mutate(x = x*fp) %>%
        dplyr::select(x, by)

      tbInf <- tb %>%
        dplyr::group_by(by) %>%
        dplyr::summarise(E = 0.1*mean(x, na.rm=TRUE),
                         .groups = 'drop') %>%
        dplyr::right_join(tbInf, by = 'by')
    }

    descBy <- tb %>%
      dplyr::group_by(by) %>%
      dplyr::summarise(count = dplyr::n(),
                       mean = mean(x, na.rm = TRUE),
                       sd = stats::sd(x),
                       var = stats::var(x),
                       cv = (sd/mean)*100,
                       min = min(x),
                       Q1 = stats::quantile(x, 0.25),
                       median = stats::median(x),
                       Q3 = stats::quantile(x, 0.75),
                       IQR = stats::IQR(x),
                       max = max(x),
                       kurtosi = moments::kurtosis(x),
                       skew = moments::skewness(x),
                       .groups = 'drop')

    descByOut <- descBy %>%
      tidyr::pivot_longer(!by,
                          names_to = "Parameters",
                          values_to = "Value") %>%
      tidyr::pivot_wider(names_from = by,
                         values_from = Value)

    tb_par <- tbInf %>%
      dplyr::right_join(descBy, by = 'by') %>%
      dplyr::select(by,fc,t,var,E,N,count,mean)

    par <- tb_par %>%
      dplyr::mutate(n = dplyr::if_else(fc >= 0.98,
                                       ceiling((t^2*var)/E^2),
                                       ceiling((N*t^2*var)/(N*E^2 + t^2*var)))) %>%
      dplyr::mutate(VarM = dplyr::if_else(fc >= 0.98,
                                          var/count,
                                          (var/count)*fc)) %>%
      dplyr::mutate(StdM = sqrt(VarM),
                    ASE = t*StdM,
                    RSE = (ASE/mean)*100,
                    LCI = mean - ASE,
                    UCI = mean + ASE) %>%
      dplyr::select(-var,-t)

    if (FP == TRUE) {
      par <- par %>%
        dplyr::mutate(TotPop = A*mean,
                      LCIP = TotPop - A*(ASE),
                      UCIP = TotPop + A*(ASE))
    }else{
      par <- par %>%
        dplyr::mutate(TotPop = N*mean,
                      LCIP = TotPop - N*(ASE),
                      UCIP = TotPop + N*(ASE))
    }

    out <- par %>%
      tidyr::pivot_longer(!by,
                          names_to = "Parameters",
                          values_to = "Value") %>%
      tidyr::pivot_wider(names_from = by,
                         values_from = Value)
  }

  if (is.null(by)) {
    tb <- tibble::tibble(x = x)

    tbInf <- tb %>%
      dplyr::summarise(count = dplyr::n(),
                       E = 0.1*mean(x, na.rm=TRUE),
                       t = stats::qt(1-.05/2, length(x)-1),
                       .groups = 'drop') %>%
      dplyr::mutate(A = A,
                    a = a,
                    N = A/a,
                    f = count/N,
                    fc = 1-f
      ) %>%
      dplyr::select(-count)

    if (FP == TRUE) {
      tb <- tb %>%
        dplyr::mutate(x = x*(1/a))

      tbInf <- tbInf %>%
        dplyr::mutate(fp = 1/a) %>%
        dplyr::mutate(tb %>%
                        dplyr::summarise(E = 0.1*mean(x, na.rm=TRUE),
                                         .groups = 'drop'))
    }

    descBy <- tb %>%
      dplyr::summarise(count = dplyr::n(),
                       mean = mean(x, na.rm = TRUE),
                       sd = stats::sd(x),
                       var = stats::var(x),
                       cv = (sd/mean)*100,
                       min = min(x),
                       Q1 = stats::quantile(x, 0.25),
                       median = stats::median(x),
                       Q3 = stats::quantile(x, 0.75),
                       IQR = stats::IQR(x),
                       max = max(x),
                       kurtosi = moments::kurtosis(x),
                       skew = moments::skewness(x),
                       .groups = 'drop')

    descByOut <- descBy %>%
      tidyr::pivot_longer(tidyselect::everything(),
                          names_to = "Parameters",
                          values_to = "Value")

    tb_par <- dplyr::bind_cols(tbInf,descBy) %>%
      dplyr::select(fc,t,var,E,N,count,mean)

    par <- tb_par %>%
      dplyr::mutate(n = dplyr::if_else(fc >= 0.98,
                                       ceiling((t^2*var)/E^2),
                                       ceiling((N*t^2*var)/(N*E^2 + t^2*var)))) %>%
      dplyr::mutate(VarM = dplyr::if_else(fc >= 0.98,
                                          var/count,
                                          (var/count)*fc)) %>%
      dplyr::mutate(StdM = sqrt(VarM),
                    ASE = t*StdM,
                    RSE = (ASE/mean)*100,
                    LCI = mean - ASE,
                    UCI = mean + ASE) %>%
      dplyr::select(-var,-t)

    if (FP == TRUE) {
      par <- par %>%
        dplyr::mutate(TotPop = A*mean,
                      LCIP = TotPop - A*(ASE),
                      UCIP = TotPop + A*(ASE))
    }else{
      par <- par %>%
        dplyr::mutate(TotPop = N*mean,
                      LCIP = TotPop - N*(ASE),
                      UCIP = TotPop + N*(ASE))
    }

    out <- par %>%
      tidyr::pivot_longer(tidyselect::everything(),
                          names_to = "Parameters",
                          values_to = "Value")
  }

  out <- out %>%
    dplyr::filter(!Parameters %in% c("E", "f", "fc")) %>%
    dplyr::mutate(
      Parameters = dplyr::recode(Parameters,
                                 N = "Number of potential sample units",
                                 count = "Count",
                                 mean = "Sample mean",
                                 n = "Sample sufficiency",
                                 VarM = "Variance of mean",
                                 StdM = "Standard error of mean",
                                 ASE = "Absolute sampling error",
                                 RSE = "Relative sampling error",
                                 LCI = "Lower confidence interval the mean",
                                 UCI = "Upper confidence interval the mean",
                                 TotPop = "Total population",
                                 LCIP = "Lower confidence interval (Population)",
                                 UCIP = "Upper confidence interval (Population)"))

  if(DT == TRUE) {
    # manipulateWidget::manipulateWidget({
    #   manipulateWidget::combineWidgets(
    #     title = "Simple Random Sampling",
    #     ncol = 1, colsize = 1,
    #     DT::datatable(out, editable = TRUE,
    #                   rownames = F,
    #                   extensions=c("Buttons",'ColReorder'),
    #                   options = list(
    #                     colReorder = TRUE,
    #                     pageLength = 20,
    #                     dom = 'Bfrtip',
    #                     buttons = c('copy', 'excel', 'pdf', I('colvis')),
    #                     scroller = TRUE,
    #                     searchHighlight = TRUE)
    #     ),
    #     DT::datatable(descByOut, editable = TRUE,
    #                   rownames = F,
    #                   extensions=c("Buttons",'ColReorder'),
    #                   options = list(
    #                     colReorder = TRUE,
    #                     pageLength = 20,
    #                     dom = 'Bfrtip',
    #                     buttons = c('copy', 'excel', 'pdf', I('colvis')),
    #                     scroller = TRUE,
    #                     searchHighlight = TRUE)
    #     ))
    # }, select = manipulateWidget::mwSelect(1:3))

    out %>% DT::datatable(editable = TRUE,
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
      )
  }else{

    result <- structure(list(Descriptive = descByOut,
                             Estimated = out,
                             'Base information' = tbInf),
                        class = c("forester", "SRS"))
  }
  return(result)
}
