#' MCD function to detect outliers
#'
#' Detecting multivariate outliers using the Minimum Covariance Determinant approach
#'
#' @param x matrix of bivariate values from which we want to compute outliers
#' @param h proportion of dataset to use in order to compute sample means and covariances
#' @param alpha nominal type I error probability (by default .01)
#' @param na.rm set whether Missing Values should be excluded (na.rm = TRUE) or not (na.rm = FALSE) - defaults to TRUE
#'
#' @export outliers_mcd
#' @S3method outliers_mcd default
#' @S3method print outliers_mcd
#'
#' @keywords MMCD outliers
#' @return Returns Call, Max distance, number of outliers
#' @examples
#' #### Run outliers_mcd
#' # The default is to use 75% of the datasets in order to compute sample means and covariances
#' # This proportion equals 1-breakdown points (i.e. h = .75 <--> breakdown points = .25)
#' # This breakdown points is encouraged by Leys et al. (2018)
#' data(Attacks)
#' SOC <- rowMeans(Attacks[,c("soc1r","soc2r","soc3r","soc4","soc5","soc6","soc7r",
#' "soc8","soc9","soc10r","soc11","soc12","soc13")])
#' HSC <- rowMeans(Attacks[,22:46])
#' res <- outliers_mcd(x = cbind(SOC,HSC), h = .5,na.rm = TRUE)
#' res
#'
#' # Moreover, a list of elements can be extracted from the function,
#' # such as the position of outliers in the dataset
#' # and the coordinates of outliers
#' res$outliers_pos
#' res$outliers_val
#' @importFrom stats mahalanobis na.omit qchisq
#' @importFrom MASS cov.mcd

# Create a generic function
outliers_mcd <- function(x,h,alpha,na.rm) UseMethod("outliers_mcd")

outliers_mcdEst <- function(x,
                      h = .75, # fraction of data we wanna keep
                               # to compute the MCD (between 0 and 1)
                      alpha = .01,
                      na.rm = TRUE){

  if (na.rm == TRUE ) {
    data <- na.omit(x)
  } else {data <- x}

  for (i in seq_len(ncol(data))){
    if(inherits(data[,i],c("numeric","integer")) == FALSE)
      stop("Data are neither numeric nor integer")
  }

  #Creating covariance matrix for Minimum Covariance Determinant
  # by default, use the "best" method = exhaustive method
  output <- cov.mcd(data,cor = FALSE,quantile.used = nrow(data)*h)
  cutoff <- (qchisq(p = 1-alpha, df = ncol(data)))
  # cor = FALSE to avoid useless output(correlation matrix)

  #Distances from centroid for each matrix
  dist <- mahalanobis(data,output$center,output$cov) # distance

  #Detecting outliers
  names_outliers <- which(dist > cutoff)
  coordinates <- cbind(x_axis = data[,1][dist > cutoff],
                      y_axis = data[,2][dist > cutoff])

  # print results
  meth <- "Minimum Covariance Determinant estimator"

  # Return results in list()
    invisible(
      list(MaxDist = cutoff,
           center = output$center,
           outliers_pos = names_outliers,
           outliers_val=coordinates)
      )

}

# Adding a default method in defining a function called outliers_mcd.default

outliers_mcd.default <- function(x,h = .5,alpha = .01,na.rm = TRUE){
  out <- outliers_mcdEst(x,h,alpha,na.rm)
  out$distance <- out$MaxDist
  out$center <- out$center
  out$call <- match.call()
  out$nb <- c(total = length(out$outliers_pos))

  class(out) <- "outliers_mcd"
  out
}

print.outliers_mcd <- function(x,...){
  cat("Call:\n")
  print(x$call)

  cat("\nLimit distance of acceptable values from the centroid :\n")
  print(x$distance)

  cat("\nNumber of detected outliers:\n")
  print(x$nb)

}

