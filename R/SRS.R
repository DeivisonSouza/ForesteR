#' Stratified Random Sampling
#'
#' @param data Conjunto de dados que contém as informações de inventário florestal.
#' @param x Um vetor numérico com a variável de interesse.
#' @param strata Fator de estratificação da característica de interesse.
#' @param A Área total da população, em hectare.
#' @param a Área da unidade de amostra, em hectare.
#' @param SA Método de alocação das unidades de amostras nos estratos.
#' @param FP Lógico. Fator de proporcionalidade para transformar para hectare. (default = FALSE)
#' @param DT Lógico. Imprime a saída no painel de visualização. (default = FALSE)
#' @param ... Parâmetros adicionais
#'
#' @return Uma lista com estatística descritiva, anova, estimativas de amostragem aleatória estratificada e informações de base para cálculo das estimativas.
#' @export
#'
#' @examples
#' \dontrun{
#' data(native)
#' SRS(x=native$Volume, strata=native$Strata, A = c(650, 350), a = 1, SA = "PA")
#'}
#'
#' @importFrom rlang .data

SRS <- function(data = NULL, x, strata, A, a, SA = "PA", FP = FALSE, DT = FALSE, ...){
  #if(!is.data.frame(data)) stop("data must be a dataframe")

  if(!is.numeric(x) | is.integer(x) | is.character(x))
    stop("x must be a numeric vector")

  if(is.character(strata) | is.numeric(strata)) strata <- as.factor(strata)

  if(!(SA %in% c("PA", "Neyman"))) stop("`SA` should be either 'PA' or 'Neyman'")

  # Summary table per sample unit ===========================================

  tb <- tibble::tibble(
    strata = factor(strata, levels = unique(strata)),
    x = dplyr::case_when((FP == TRUE ) ~ x*(1/a),
                         (FP == FALSE) ~ x))

  # Table with basic information for calculating estimates ==================
  tbInf <- tb %>%
    dplyr::group_by(strata) %>%
    dplyr::summarise(.groups = 'drop') %>%
    dplyr::mutate(Ah = A,
                  Nh = Ah/a,
                  Wh = Ah/sum(Ah),
                  #f = count/N,
                  #fc = 1-f
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
                     .groups = 'drop')

  tbDescBy <- descBy %>%
    dplyr::mutate_if(is.numeric, format, scientific=FALSE) %>%
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
    dplyr::summarise(E = 0.1*sum(Wh*Mean),
                     t = stats::qt(1-.05/2, length(x)-1),
                     #f = length(x)*a/sum(A),
                     #f = length(x)/sum(Nh),
                     #f = (length(x)*(a*(1/a)))/sum(A),
                     f = dplyr::case_when((FP == TRUE ) ~ (length(x)*(a*(1/a)))/sum(Ah),
                                          (FP == FALSE) ~ length(x)/sum(Nh)),
                     fc = 1-f,
                     ne = dplyr::case_when((FP == TRUE ) ~ (sum(((Ah*(Ah-count))/count)*var)^2)/sum((((Ah*(Ah-count))/count)^2)*(var^2)/(count-1)),
                                          (FP == FALSE) ~ (sum(((Nh*(Nh-count))/count)*var)^2)/sum((((Nh*(Nh-count))/count)^2)*(var^2)/(count-1))),
                     #ne = (sum(((Nh*(Nh-count))/count)*var)^2)/sum((((Nh*(Nh-count))/count)^2)*(var^2)/(count-1)),
                     N = sum(Nh),
                     Count = sum(count),
                     n = dplyr::case_when((fc >= 0.98 & SA == "PA") ~ (t^2*sum(Wh*var))/ E^2, # infinite
                                           (fc < 0.98 & SA == "PA") ~ (t^2*sum(Wh*var))/ ((E^2)+((t^2)*(sum(Wh*var/ifelse(FP == FALSE, sum(Nh), sum(Ah)))))), # finite
                                           (fc >= 0.98 & SA == "Neyman") ~ (t^2*(sum(Wh*sqrt(var))^2))/ E^2, # infinite
                                           (fc < 0.98 & SA == "Neyman") ~ (t^2*(sum(Wh*sqrt(var))^2))/ ((E^2)+((t^2)*(sum(Wh*var/ifelse(FP == FALSE, sum(Nh), sum(Ah)))))) # finite
                                          ),
                     mean.st = sum(Wh*Mean),
                     var.st = sum(Wh*var),
                     var.mean.st = sum(Wh^2*(var/count)) - sum((Wh*var)/ifelse(FP == FALSE, sum(Nh), sum(Ah))),
                     std.mean.st = sqrt(var.mean.st),
                     ase = stats::qt(1-.05/2, ne)*std.mean.st,
                     rse = (ase/mean.st)*100,
                     lci = mean.st - ase,
                     uci = mean.st + ase,
                     totPop = ifelse(FP == FALSE, sum(Nh), sum(Ah))*mean.st,
                     lcip = totPop - ifelse(FP == FALSE, sum(Nh), sum(Ah))*(ase),
                     ucip = totPop + ifelse(FP == FALSE, sum(Nh), sum(Ah))*(ase)
                     ) %>%
    dplyr::mutate_if(is.numeric, format, scientific=FALSE)

  tbPar <- tbPar %>%
    tidyr::pivot_longer(tidyselect::everything(),
                        names_to = 'Parameters',
                        values_to = 'Value')

  tbPar <- tbPar %>%
    dplyr::mutate(
      Parameters = dplyr::recode(Parameters,
                                 N = "Number of potential sample units",
                                 count = "Count",
                                 n = paste0("Sample sufficiency", " ", "(", ifelse(SA == "PA", "Proportional allocation", "Neyman"), ")"),
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

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  result <- structure(list(Descriptive = tbDescBy,
                           Anova = AOV,
                           Estimated = tbParOut,
                           BaseInfo = list(`1` = tbInf,
                                           `2` = tbPar %>%
                                             dplyr::filter(Parameters %in% c("E", "t", "f", "fc", "ne")))),
                      class = c("forester", "SRS"))
  return(result)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
