#' mahalanobis function to detect outliers
#'
#' Detecting multivariate outliers using the Mahalanobis distance
#'
#' @param data matrix of bivariate values from which we want to compute outliers
#' @param h when using the MMCD method, proportion of dataset to used in order to compute sample means and covariances
#' @param alpha alpha value (by default .01)
#' @param na.rm set whether Missing Values should be excluded (na.rm=TRUE) or not (na.rm=FALSE) - defaults to TRUE
#' @param verbose logical variable indicating whether text output should be generated (verbose = TRUE) or not (verbose = FALSE) - default to TRUE
#'
#' @export outliers_mahalanobis
#'
#' @keywords mahalanobis outliers
#' @return Returns Method, Max distance, number of outliers, Outliers positions
#' @examples
#' ## Run outliers_mahalanobis
#' Sigma=matrix(c(1,.5,.5,1),2,2)
#' data=mvrnorm(100,mu=rep(0,2),Sigma=Sigma)
#' outliers_mahalanobis(data=data, h=.5,na.rm=TRUE,plot=TRUE, verbose=TRUE)
#' @importFrom stats mahalanobis cov na.omit qchisq
#' @importFrom MASS cov.mcd

outliers_mahalanobis=function(data,
                      h=.5, # fraction of data we wanna keep to compute the MCD (between 0 and 1)
                      alpha=.01,
                      na.rm = TRUE,
                      verbose = TRUE){

  if (na.rm==TRUE) {
    dat=na.omit(data)
  } else {dat=data}

  for (i in 1:ncol(dat)){
    if(inherits(dat[,i],c("numeric","integer"))==FALSE) stop("Data are neither numeric nor integer")
  }

  #Creating covariance matrix for Minimum Covariance Determinant
  output<-cov.mcd(dat,cor=FALSE,quantile.used=nrow(dat)*h) # by default, use the "best" method = exhaustive method
  cutoff<-(qchisq(p=1-alpha, df=ncol(dat))) # how to compute df? Read the text and add it (2 si 2 variables, c dc tj ?gal ? k? ? checker)
  # cor = FALSE to avoid useless output(correlation matrix)

  #Distances from centroid for each matrix
  dist<-mahalanobis(dat,colMeans(dat),cov(dat))
  #Detecting outliers
  names_outliers<-which(dist>cutoff)
  coordinates <- list(x_axis=dat[,1][dist>cutoff],y_axis=dat[,2][dist>cutoff])
  outliers<-cbind(x_axis=coordinates$x_axis,y_axis=coordinates$y_axis)
  if (length(names_outliers)!=0){rownames(outliers)=paste("POS",names_outliers)}

  # print results
  meth="Mahalanobis distance"

  if(verbose == TRUE){
    cat("Method:",meth)
    cat("\n\n")
    cat("Results:\n")
    cat("Max distance:",format(cutoff, digits = 1, nsmall = 3, scientific = FALSE))
    cat("\n")
    cat("Number of outliers:", length(names_outliers))
    cat("\n")
    cat("Outliers positions:", paste0(round(names_outliers, digits = 4)))

    # Return results in list()
    invisible(list(MaxDist = cutoff, nbrow=names_outliers))

  }

}

