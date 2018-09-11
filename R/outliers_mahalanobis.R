#' MMCD function to detect outliers
#' @export outliers_MCD
#' @param data matrix of bivariate values from which we want to compute outliers
#' @param method method of outliers detection (MMCD or Mahalanobis distance)
#' @param h when using the MMCD method, proportion of dataset to used in order to compute sample means and covariances
#' @param alpha alpha value (by default .01)
#' @param na.rm set whether Missing Values should be excluded (na.rm=TRUE) or not (na.rm=FALSE) - defaults to TRUE
#' @param plot set whether results should be plotted (plot = TRUE) or not (plot = FALSE) - defaults to TRUE
#' @param verbose logical variable indicating whether text output should be generated (verbose = TRUE) or not (verbose = FALSE) - default to TRUE
#' @keywords mahalanobis MMCD outliers
#' @return Returns Method, Max distance, number of outliers, Outliers positions
#' @examples
#' ## Run outliers_bivar
#' Sigma=matrix(c(1,.5,.5,1),2,2)
#' data=mvrnorm(100,mu=rep(0,2),Sigma=Sigma)
#' outliers_mahalanobis(data=data, h=.5,na.rm=TRUE,plot=TRUE, verbose=TRUE)
#' @importFrom stats mahalanobis cov lm median na.omit qchisq
#' @importFrom MASS cov.mcd
#' @importFrom graphics abline legend par points rect segments text title

outliers_mahalanobis=function(data,
                      h=.5, # fraction of data we wanna keep to compute the MCD (between 0 and 1)
                      alpha=.01,
                      na.rm = TRUE,
                      plot=TRUE,
                      verbose = TRUE){

  if (na.rm==TRUE) {
    dat=na.omit(data)
  } else {dat=data}

  for (i in 1:ncol(dat)){
    if(class(dat[,i])!="numeric") stop("Data are not numeric")
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

  # plotting results
  if (plot==TRUE){
    par(xpd=FALSE)
    plot(dat[,1],dat[,2],xlab="X",ylab="Y",pch=19,cex=.5)
    center=cov.mcd(dat,cor=FALSE,quantile.used=nrow(dat)*h)$center
    abline(h=center[2],col="lightgrey",lty=2)
    abline(v=center[1],col="lightgrey",lty=2)
    abline(lm(dat[,2]~dat[,1]),col="darkviolet") # regression line, based on ALL values (y=dv, x = predictor)
    if (length(names_outliers)>0){           # if there are outliers, compute the regression line excluding it
      dat2<-dat[-names_outliers,]           # matrix without outliers (IF there are outliers)
      mod<-lm(dat2[,2]~dat2[,1])              # regression line computed without outliers
      abline(mod,col="darkgreen")}
    par(xpd=TRUE,mar=c(2,2,4,2))
    if (length(names_outliers)==0){
      legend(x="top",xjust="centered",inset=c(0,-.2),legend = "Regression line",fill = "darkviolet",box.lty=0)
    } else if (length(names_outliers)==1){
      points(dat[names_outliers,][1],dat[names_outliers,][2],col="red",bg="red",pch=19,cex=.5)
      text(dat[names_outliers,][1],dat[names_outliers,][2], as.character(names_outliers),pos=4,cex=.75,col="red")
      legend(x="top",xjust="centered",inset=c(0,-.2),legend = c("Regression line including all data","Regression line without detected outliers"),fill = c("darkviolet","darkgreen"),box.lty=0)
    } else if (length(names_outliers)>1){
      points(dat[names_outliers,][,1],dat[names_outliers,][,2],col="red",bg="red",pch=19,cex=.5)
      text(dat[names_outliers,][,1],dat[names_outliers,][,2], as.character(names_outliers),pos=4,cex=.75,col="red")
      legend(x="top",xjust="centered",inset=c(0,-.2),legend = c("Regression line including all data","Regression line without detected outliers"),fill = c("darkviolet","darkgreen"),box.lty=0)}}


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

Sigma=matrix(c(1,.5,.5,1),2,2)
data=mvrnorm(100,mu=rep(0,2),Sigma=Sigma)

