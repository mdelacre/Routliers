#' mahalanobis function to detect outliers
#'
#' Detecting multivariate outliers using the Mahalanobis distance
#'
#' @param x matrix of bivariate values from which we want to compute outliers
#' @param alpha nominal type I error probability (by default .01)
#' @param na.rm set whether Missing Values should be excluded (na.rm = TRUE) or not (na.rm = FALSE) - defaults to TRUE
#'
#' @export outliers_mahalanobis
#' @S3method outliers_mahalanobis default
#' @S3method print outliers_mahalanobis
#'
#' @keywords mahalanobis outliers
#' @return Returns Call, Max distance, number of outliers
#' @examples
#' #### Run outliers_mahalanobis
#' data(Attacks)
#' SOC <- rowMeans(Attacks[,c("soc1r","soc2r","soc3r","soc4","soc5","soc6","soc7r",
#' "soc8","soc9","soc10r","soc11","soc12","soc13")])
#' HSC <- rowMeans(Attacks[,22:46])
#' res <- outliers_mahalanobis(x = cbind(SOC,HSC), na.rm = TRUE)
#' # A list of elements can be extracted from the function,
#' # such as the position of outliers in the dataset
#' # and the coordinates of outliers
#' res$outliers_pos
#' res$outliers_val

#' @importFrom stats mahalanobis cov na.omit qchisq

# Create a generic function
outliers_mahalanobis <- function(x,alpha,na.rm)
  UseMethod("outliers_mahalanobis")

outliers_mahalanobisEst <- function(x,
                                    alpha = .01,
                                    na.rm = TRUE){

  if (na.rm == TRUE) {
    data <- na.omit(x)
  } else {data <- x}

  for (i in seq_len(ncol(data))){
    if(inherits(data[,i],c("numeric","integer")) == FALSE)
      stop("Data are neither numeric nor integer")
  }

  #Distances from centroid for each matrix
  dist <- mahalanobis(data,colMeans(data),cov(data))

  #Detecting outliers
  cutoff <- (qchisq(p = 1-alpha, df = ncol(data)))
  names_outliers <- which(dist > cutoff)
  coordinates <- data.frame(
    matrix(NA, nrow = length(names_outliers), ncol = ncol(data))
    )
    for (k in seq_len(ncol(data))){
  coordinates[,k] <- data[,k][dist > cutoff]
    }

  # print results
  meth <- "Mahalanobis distance"

  # Return results in list()
  invisible(
    list(MaxDist = cutoff,
         center = colMeans(data),
         outliers_pos = names_outliers,
         outliers_val = coordinates,
         dist_from_center = dist)
    )

}

# Adding a default method in defining a function called outliers_mcd.default

outliers_mahalanobis.default <- function(x,alpha = .01,na.rm = TRUE){
  out <- outliers_mahalanobisEst(x,alpha,na.rm)
  out$distance <- out$MaxDist
  out$call <- match.call()
  out$nb <- c(total = length(out$outliers_pos))

  class(out) <- "outliers_mahalanobis"
  out
}


print.outliers_mahalanobis <- function(x,...){
  cat("Call:\n")
  print(x$call)

  cat("\nLimit distance of acceptable values from the centroid :\n")
  print(x$distance)

  cat("\nNumber of detected outliers:\n")
  print(x$nb)

}

