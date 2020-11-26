#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
desc <-
  function(x){
    n <- length(x)
    Soma <- sum(x)
    Media <- round(sum(x)/n, digits=2)
    Variancia <- var(x)
    DP <- round(sd(x), digits=2)
    CV <- round((DP/Media)*100,digits=2)
    Minimo <- min(x)
    Quartil1 <- quantile(x,0.25)
    Mediana <- median(x)
    Quartil3 <- quantile(x,0.75)
    Interquatil <- Quartil3-Quartil1
    Maximo <- max(x)
    return(list(n=n, Media=Media, DP=DP, CV=CV, Min=Minimo,
                Q1=Quartil1, Md=Mediana, Q3=Quartil3,
                Interquatil=Interquatil, Max=Maximo))
  }
