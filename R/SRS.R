#' Stratified Random Sampling
#'
#' @param data Conjunto de dados que contém as informações de inventário florestal para diferentes estratos.
#' @param x Um vetor numérico com a variável de interesse.
#' @param strata Fator. Usado para estratificação da variável de interesse.
#' @param A Numérico. Um vetor númerico com a área total de cada estrato, em hectare.
#' @param a Numérico. Área da unidade de amostra, em hectare.
#' @param LE Numérico. Limite de erro admissível. O padrão é 0.1, isto é, 10% da média amostral estimada.
#' @param SA Método de alocação das unidades de amostras nos estratos. Estão disponíveis os método "PA" (Proportional allocation) e "Neyman".
#' @param FP Lógico. Fator de proporcionalidade para transformar os valores da variável de interesse para hectare. (default = FALSE)
#' @param DT Lógico. Imprime as estimativas dos parâmetros no painel de visualização. (default = FALSE)
#' @param digits Inteiro. Número de casas decimais para imprimir os resultados. (default = 3)
#' @param ... Parâmetros adicionais
#'
#' @return Uma lista com estatística descritiva, anova, estimativas de amostragem aleatória estratificada e informações de base para cálculo das estimativas.
#' @export
#'
#' @examples
#' \dontrun{
#' data(native)
#' SRS(x=native$Volume1, strata=native$Strata, A = c(650, 350), a = 1, LE = 0.1, SA = "PA")
#'}
#'
#' @importFrom rlang .data

SRS <- function(data = NULL, x, strata, A, a, LE = 0.1, SA = "PA", FP = FALSE, DT = FALSE, digits = 3, ...){
  #if(!is.data.frame(data)) stop("data must be a dataframe")

  if(!is.numeric(x) | is.integer(x) | is.character(x))
    stop("x must be a numeric vector")

  if(is.character(strata) | is.numeric(strata)) strata <- as.factor(strata)

  if(!(SA %in% c("PA", "Neyman"))) stop("`SA` should be either 'PA' or 'Neyman'")

  # Summary table per sample unit ===========================================
  if (!is.null(data)) {
    tb <- tibble::tibble(
      strata = factor(strata, levels = unique(strata)),
      x = dplyr::case_when((FP == TRUE ) ~ x*(1/a),
                           (FP == FALSE) ~ x))
  }else{
    tb <- tibble::tibble(
      strata = factor(strata, levels = unique(strata)),
      x = dplyr::case_when((FP == TRUE ) ~ x*(1/a),
                           (FP == FALSE) ~ x))
  }

  # Basic information for calculating estimates ============================
  tbInf <- tb %>%
    dplyr::group_by(strata) %>%
    dplyr::summarise(.groups = 'drop') %>%
    dplyr::mutate(Ah = A,
                  Nh = Ah/a,
                  Wh = Ah/sum(Ah)
    ) %>%
    janitor::adorn_totals("row")
    #dplyr::rowwise(strata) %>%
    #dplyr::mutate(gear_total = sum(dplyr::c_across()))

  # Descriptive statistics by strata ========================================
  descBy <- tb %>%
    dplyr::group_by(strata) %>%
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
                     .groups = 'drop') #%>%
    # dplyr::mutate_at(dplyr::vars(c(count)), ceiling)

  tbDescBy <- descBy %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, digits)) %>%
    tidyr::pivot_longer(!strata,
                        names_to = "Parameters",
                        values_to = "Value") %>%
    tidyr::pivot_wider(names_from = strata,
                       values_from = Value)

  # ANOVA for stratification ================================================
  AOV <- tb %>% stats::aov(x ~ strata, .) %>% summary()

  # Parameters SRS ==========================================================
  tbInf2 <- tbInf %>%
    dplyr::slice(-dplyr::n()) %>%
    dplyr::left_join(descBy, by = 'strata') %>%
    dplyr::select(dplyr::all_of(c("strata", "count","Wh", "Nh", "Ah", "Mean", "var")))

  tbPar <- tbInf2 %>%
    dplyr::summarise(E = LE*sum(Wh*Mean),
                     t = stats::qt(1-.05/2, length(x)-1),
                     f = length(x)/sum(Nh),
                     fc = 1-f,
                     # ne = dplyr::case_when((FP == TRUE ) ~ (sum(((Ah*(Ah-count))/count)*var)^2)/sum((((Ah*(Ah-count))/count)^2)*(var^2)/(count-1)),
                     #                      (FP == FALSE) ~ (sum(((Nh*(Nh-count))/count)*var)^2)/sum((((Nh*(Nh-count))/count)^2)*(var^2)/(count-1))),
                     ne = (sum(((Nh*(Nh-count))/count)*var)^2)/sum((((Nh*(Nh-count))/count)^2)*(var^2)/(count-1)),
                     N = sum(Nh),
                     Count = sum(count),
                     n = dplyr::case_when((fc >= 0.98 & SA == "PA") ~ (t^2*sum(Wh*var))/ E^2, # infinite
                                           (fc < 0.98 & SA == "PA") ~ (t^2*sum(Wh*var))/ ((E^2)+((t^2)*(sum(Wh*var/sum(Nh))))), # finite
                                           (fc >= 0.98 & SA == "Neyman") ~ (t^2*(sum(Wh*sqrt(var))^2))/ E^2, # infinite
                                           (fc < 0.98 & SA == "Neyman") ~ (t^2*(sum(Wh*sqrt(var))^2))/ ((E^2)+((t^2)*(sum(Wh*var/sum(Nh))))) # finite
                                          ),
                     nrec = dplyr::case_when((fc >= 0.98 & SA == "PA") ~ ((stats::qt(1-.05/2, round(n)-1))^2*sum(Wh*var))/ E^2, # infinite
                                          (fc < 0.98 & SA == "PA") ~ ((stats::qt(1-.05/2, round(n)-1))^2*sum(Wh*var))/ ((E^2)+(((stats::qt(1-.05/2, round(n)-1))^2)*(sum(Wh*var/sum(Nh))))), # finite
                                          (fc >= 0.98 & SA == "Neyman") ~ ((stats::qt(1-.05/2, round(n)-1))^2*(sum(Wh*sqrt(var))^2))/ E^2, # infinite
                                          (fc < 0.98 & SA == "Neyman") ~ ((stats::qt(1-.05/2, round(n)-1))^2*(sum(Wh*sqrt(var))^2))/ ((E^2)+(((stats::qt(1-.05/2, round(n)-1))^2)*(sum(Wh*var/sum(Nh))))) # finite
                     ),
                     mean.st = sum(Wh*Mean),
                     var.st = sum(Wh*var),
                     var.mean.st = sum(Wh^2*(var/count)) - sum((Wh*var)/sum(Nh)),
                     std.mean.st = sqrt(var.mean.st),
                     ase = stats::qt(1-.05/2, ne)*std.mean.st,
                     rse = (ase/mean.st)*100,
                     lci = mean.st - ase,
                     uci = mean.st + ase,
                     totPop = ifelse(FP == FALSE, sum(Nh), sum(Ah))*mean.st,
                     lcip = totPop - ifelse(FP == FALSE, sum(Nh), sum(Ah))*(ase),
                     ucip = totPop + ifelse(FP == FALSE, sum(Nh), sum(Ah))*(ase)
                     ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, digits))
    #dplyr::mutate_at(dplyr::vars(-c(N, Count)), round, digits)
    #dplyr::mutate_if("Parameters" %in% c("N", "Count"), ceiling)

    tbPar <- tbPar %>%
    tidyr::pivot_longer(tidyselect::everything(),
                        names_to = 'Parameters',
                        values_to = 'Value')

    # Sample sufficiency per strata ============================================
    tbSA <- tbInf %>%
      dplyr::select(dplyr::contains(c("s", "W"))) %>%
      dplyr::slice(-dplyr::n()) %>%
      dplyr::group_by(strata) %>%
      dplyr::summarise(nh  = Wh*(tbPar %>%
                                  dplyr::filter(Parameters == 'nrec') %>%
                                  .[[2]] %>%
                                  round())) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), round)) %>%
      dplyr::rename_with(function(x) paste0("nh", " ", "(", ifelse(SA == "PA", "Proportional allocation", "Neyman"), ")"), where(is.numeric)) %>%
      janitor::adorn_totals("row")

    # Rename parameters =======================================================
    tbPar <- tbPar %>%
    dplyr::mutate(
      Parameters = dplyr::recode(Parameters,
                                 N = "Number of potential sample units",
                                 count = "Count",
                                 n = paste0("Sample sufficiency", " ", '(df =',length(x)-1,')', " ","(", ifelse(SA == "PA", "Proportional allocation", "Neyman"), ")"),
                                 nrec = paste0("Sample sufficiency recalculation", " ", '(df =',tbPar %>%
                                                 dplyr::filter(Parameters == 'n') %>%
                                                 .[[2]] %>%
                                                 round()-1,')', " ","(", ifelse(SA == "PA", "Proportional allocation", "Neyman"), ")"),
                                 mean.st = "Stratified sample mean",
                                 var.st = "Stratified sample variance",
                                 var.mean.st = "Variance of the mean stratified",
                                 std.mean.st = "Standard error of the mean stratified",
                                 ase = "Absolute sampling error",
                                 rse = "Relative sampling error",
                                 lci = "Lower confidence interval for the mean",
                                 uci = "Upper confidence interval for the mean",
                                 totPop = "Total population",
                                 lcip = "Lower confidence interval for the population",
                                 ucip = "Upper confidence interval for the population"))

  tbParOut <- tbPar %>%
    dplyr::filter(!Parameters %in% c("E", "t", "f", "fc", "ne"))

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
                           Anova = AOV,
                           Estimated = list(parameters = tbParOut,
                                            nst = tbSA),
                           BaseInfo = list(`1` = tbInf,
                                           `2` = tbPar %>%
                                             dplyr::filter(Parameters %in% c("E", "t", "f", "fc", "ne")))),
                      class = c("forester", "SRS"))
  return(result)
}

# Teste FP (dados: http://www.mensuracaoflorestal.com.br/capitulo-5-amostragem-casual-estratificada)
# r <- SRS(x=test$Volume, strata=test$Strata, A = c(14.4, 16.4, 14.2), a = 0.1, LE = 0.05, SA = "PA", FP = TRUE)
# z <- SRS(x=test$Volume, strata=test$Strata, A = c(14.4, 16.4, 14.2), a = 0.1, LE = 0.05, SA = "PA", FP = FALSE)
