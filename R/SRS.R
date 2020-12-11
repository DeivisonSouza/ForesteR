#' Simple Random Sampling
#'
#' Calcula as estimativas de amostragem aleatória simples para dados de inventário florestal.
#'
#' Um detalhe importante.
#'
#' @param x Um vetor numérico com a variável de interesse.
#' @param by Um fator para cálculo.
#' @param A Área total da população, em metros quadrados.
#' @param a Área da amostra, em metros quadrados.
#' @param DT Lógico. imprime a saída no painel de visualização. (default=TRUE)
#'
#' @return Uma tibble com as estimativas de amostragem aleatória simples.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(pinus)
#' SRS(x = pinus$Volume, A = 400000, a = 600, DT=F)
#'}
#'
#' @importFrom rlang .data

SRS <- function(x, by=NULL, A, a, DT=TRUE){

  if(!is.numeric(x) | is.integer(x) | is.character(x))
    stop("x must be a numeric vector")

  if (is.character(by)) by <- as.factor(by)

  fx <- function(x, by=NULL, A, a){

    FP <- 10000/a
    x <- x*FP
    Sum <- sum(x)
    Mean <- mean(x, na.rm = TRUE)
    Var <- stats::var(x)
    N <- ceiling(A/a)
    f <- length(x)/N
    FC <- 1-f
    E = 0.1*mean(x)
    t = stats::qt(1-.05/2, df=length(x)-1)

    if(FC >= 0.98){
      # cat("\n-------------------------------------------------------------\n
      #   A população é Infinita.\n")
      n <- ceiling((t^2*stats::var(x))/E^2)
      #cat("Para atender ao erro estabelecido você deve amostrar", n, "parcelas.\n")
      VarM <- stats::var(x)/length(x)
      SdM <- sqrt(VarM)
      Ea <- t*SdM
      Er <- (Ea/mean(x, na.rm = TRUE))*100
      ICI <- mean(x, na.rm = TRUE) - Ea
      ICS <- mean(x, na.rm = TRUE) + Ea
      TotPop <- N*mean(x, na.rm = TRUE)
      ICIP <- ICI*A
      ICSP <- ICS*A

      if(n <= length(x)){
        #cat("Esforço amostral satisfatório. O IF é definitivo!")
      }else{
        #cat("Retorne a campo e meça mais", abs(length(x)-n), "parcelas.")
      }

    }else{
      # cat("\n-------------------------------------------------------------\n")
      # cat("A população é Finita -", "FC =",round(FC,3),"\n")
      n <- ceiling((N*t^2*stats::var(x))/(N*E^2 + t^2*stats::var(x)))
      #cat("Para atender ao erro estabelecido você deve amostrar", n, "parcelas.\n")
      VarM <- stats::var(x)/length(x)*FC
      SdM <- (stats::sd(x)/sqrt(length(x)))*sqrt(FC)
      Ea <- t*SdM
      Er <- (Ea/mean(x, na.rm = TRUE))*100
      ICI <- mean(x, na.rm = TRUE) - Ea
      ICS <- mean(x, na.rm = TRUE) + Ea
      TotPop <- N*mean(x, na.rm = TRUE)
      ICIP <- ICI*A
      ICSP <- ICS*A

      if(n <= length(x)){
        #cat("Esforço amostral satisfatório. O IF é definitivo!")
      }else{
        #cat("ATENÇÃO: Retorne a campo e meça mais", abs(length(x)-n), "parcelas.")
      }

    }
    #cat("\n-------------------------------------------------------------\n")

    out <- list(Sum, Mean, N, f, E, t, n,
                VarM, SdM, Ea, Er, ICI, ICS,
                TotPop, ICIP, ICSP)

    names(out) <- c("Sum",
                    "Sample mean",
                    "Number of potential sample units",
                    "Sample fraction",
                    "Maximum acceptable error",
                    "t-student value",
                    "Sample sufficiency",
                    "Variance of mean",
                    "Standard error of mean",
                    "Absolute sampling error",
                    "Relative sampling error",
                    "Lower confidence interval the mean",
                    "Upper confidence interval the mean",
                    "Total population",
                    "Lower confidence interval (Population)",
                    "Upper confidence interval (Population)")
    out
  }

  if(is.null(by)){
    out <- fx(x = x, A = A, a = a)
    out <- do.call(rbind, out) %>%
      tibble::as_tibble(.name_repair="unique", rownames=NA)
    out <- out %>%
      tibble::rownames_to_column(var = "Parameters") %>%
      dplyr::rename("Estimates" = names(out)) %>%
      dplyr::mutate_if(is.numeric, scales::comma, accuracy = .01)

    if(DT==TRUE){
      DT::datatable(out,
                    editable = TRUE,
                    rownames = F,
                    extensions=c("Buttons",'ColReorder'),
                    options = list(
                      colReorder = TRUE,
                      pageLength = 20,
                      dom = 'Bfrtip',
                      buttons = c('copy', 'excel', 'pdf', I('colvis')),
                      scroller = TRUE,
                      searchHighlight = TRUE)
      )
    }else{
      out
    }

  }else{
    if (length(x) != length(by))
      stop("Lengths of vectors `x` and `y` must agree.")
    outBy <- tapply(x, by, FUN=fx, A = A, a = a, simplify = F)
    out <- dplyr::bind_rows(outBy, .id = NULL) %>%
      t() %>%
      tibble::as_tibble(rownames = "Parameters") %>%
      dplyr::rename_if(is.numeric, ~paste(names(outBy),"Estimates", sep="\n")) %>%
      dplyr::mutate_if(is.numeric, scales::comma, accuracy = .01)

    if(DT==TRUE){
      DT::datatable(out,
                    editable = TRUE,
                    rownames = F,
                    extensions=c("Buttons",'ColReorder'),
                    options = list(
                      colReorder = TRUE,
                      pageLength = 20,
                      dom = 'Bfrtip',
                      buttons = c('copy', 'excel', 'pdf', I('colvis')),
                      scroller = TRUE,
                      searchHighlight = TRUE)
      )
    }else{
      out
    }
  }
}


