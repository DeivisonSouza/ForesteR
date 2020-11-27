#' Descriptive statistics
#'
#' Esta função realiza a estatística descritiva para um vetor numérico.
#'
#' Um detalhe importante.
#'
#' @param x Um vetor numérico.
#'
#' @return Uma lista com estatísticas descritivas.
#'
#' @export
#'
#' @examples
desc <-
  function(x){
    n <- length(x)
    Soma <- sum(x)
    Media <- round(sum(x)/n, digits=2)
    Variancia <- stats::var(x)
    DP <- round(stats::sd(x), digits=2)
    CV <- round((DP/Media)*100,digits=2)
    Minimo <- min(x)
    Quartil1 <- stats::quantile(x,0.25)
    Mediana <- stats::median(x)
    Quartil3 <- stats::quantile(x,0.75)
    Interquatil <- Quartil3-Quartil1
    Maximo <- max(x)
    return(list(n=n, Media=Media, DP=DP, CV=CV, Min=Minimo,
                Q1=Quartil1, Md=Mediana, Q3=Quartil3,
                Interquatil=Interquatil, Max=Maximo))
  }
