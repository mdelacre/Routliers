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
#' @keywords MMCD outliers
#' @return Returns Call, Max distance, number of outliers
#' @examples
#' ## Run outliers_mcd
#' SOC <- rowMeans(Attacks[,c("soc1r","soc2r","soc3r","soc4","soc5","soc6","soc7r","soc8","soc9","soc10r","soc11","soc12","soc13")])
#' HSC <- rowMeans(Attacks[,22:46])
#' res <- outliers_mcd(x = cbind(SOC,HSC), h = .5,na.rm = TRUE)
#' res
#' @importFrom stats mahalanobis na.omit qchisq
#' @importFrom MASS cov.mcd

outliers_mcdEst <- function(x,
                      h = .5, # fraction of data we wanna keep to compute the MCD (between 0 and 1)
                      alpha = .01,
                      na.rm = TRUE){

  if (na.rm == TRUE ) {
    data <- na.omit(x)
  } else {data <- x}

  for (i in 1:ncol(data)){
    if(inherits(data[,i],c("numeric","integer")) == FALSE) stop("Data are neither numeric nor integer")
  }

  #Creating covariance matrix for Minimum Covariance Determinant
  output <- cov.mcd(data,cor = FALSE,quantile.used = nrow(data)*h) # by default, use the "best" method = exhaustive method
  cutoff <- (qchisq(p = 1-alpha, df = ncol(data)))
  # cor = FALSE to avoid useless output(correlation matrix)

  #Distances from centroid for each matrix
  dist <- mahalanobis(data,output$center,output$cov) # distance
  #Detecting outliers
  names_outliers <- which(dist > cutoff)
  coordinates <- list(x_axis = data[,1][dist > cutoff],y_axis = data[,2][dist > cutoff])
  outliers <- cbind(x_axis = coordinates$x_axis,y_axis = coordinates$y_axis)
  if (length(names_outliers) != 0){rownames(outliers) <- paste("POS",names_outliers)}

  # print results
    meth <- "Minimum Covariance Determinant estimator"

  # Return results in list()
    invisible(list(MaxDist = cutoff, center = output$center,nbrow = names_outliers))

}

# Create a generic function
outliers_mcd <- function(x,...) UseMethod ("outliers_mcd")

# Adding a default method in defining a function called outliers_mcd.default

outliers_mcd.default <- function(x,h = .5,alpha = .01,na.rm = TRUE){
  out <- outliers_mcdEst(x,h,alpha,na.rm)
  out$distance <- out$MaxDist
  out$center <- out$center
  out$call <- match.call()
  out$nb <- c(total = length(out$nbrow))

  class(out) <- "outliers_mcd"
  out
}

print.outliers_mcd <- function(x){
  cat("Call:\n")
  print(x$call)

  cat("\nLimit distance of acceptable values from the centroïd :\n")
  print(x$distance)

  cat("\nNumber of detected outliers:\n")
  print(x$nb)

}


