#' #' Plotting function for the Mahalanobis distance approach
#'
#' plotting data and highlighting multivariate outliers detected with the mahalanobis distance approach
#'
#' #' plotting data and highlighting multivariate outliers detected with the MCD function
#' Additionnally, the plot return two regression lines: the first one including all data and
#' the second one including all observations but the detected outliers. It allows to observe how much the outliers
#' influence of outliers on the regression line.
#'
#' @param x matrix of bivariate values from which we want to compute outliers
#' @param h when using the MMCD method, proportion of dataset to use in order to compute sample means and covariances
#' @param alpha nominal type I error probability (by default .01)
#' @param na.rm set whether Missing Values should be excluded (na.rm = TRUE) or not (na.rm = FALSE) - defaults to TRUE
#'
#' @export plot_outliers_mahalanobis
#' @keywords plot mahalanobis outliers
#' @return Returns Method, Max distance, number of outliers, Outliers positions
#' @return None
#' @examples
#' ## Run plot_outliers_mahalanobis
#' SOC <- rowMeans(Attacks[,c("soc1r","soc2r","soc3r","soc4","soc5","soc6","soc7r","soc8","soc9","soc10r","soc11","soc12","soc13")])
#' HSC <- rowMeans(Attacks[,22:46])
#' plot_outliers_mahalanobis(x = cbind(SOC,HSC), h = .5,na.rm = TRUE)
#' @importFrom stats mahalanobis cov lm na.omit qchisq
#' @importFrom MASS cov.mcd
#' @importFrom graphics abline legend par points

plot_outliers_mahalanobis <- function(x,
                              h = .5, # fraction of data we wanna keep to compute the MCD (between 0 and 1)
                              alpha = .01,
                              na.rm = TRUE){

  if (na.rm == TRUE) {
    data <- na.omit(x)
  } else {data <- x}

  for (i in 1:ncol(data)){
    if(inherits(data[,i],c("numeric","integer")) == FALSE) stop("Data are neither numeric nor integer")
  }

  #Creating covariance matrix for Minimum Covariance Determinant
  output <- cov.mcd(data,cor = FALSE,quantile.used = nrow(data)*h) # by default, use the "best" method = exhaustive method
  cutoff <- (qchisq(p = 1-alpha, df = ncol(data))) # how to compute df? Read the text and add it (2 si 2 variables, c dc tj ?gal ? k? ? checker)
  # cor = FALSE to avoid useless output(correlation matrix)

  #Distances from centroid for each matrix
  dist <- mahalanobis(data,colMeans(data),cov(data))
  #Detecting outliers
  names_outliers <- which(dist > cutoff)
  coordinates <- list(x_axis = data[,1][dist > cutoff],y_axis = data[,2][dist > cutoff])
  outliers <- cbind(x_axis = coordinates$x_axis,y_axis = coordinates$y_axis)
  if (length(names_outliers) != 0){rownames(outliers) = paste("POS",names_outliers)}

  # plotting results
    par(xpd = FALSE)
    plot(data[,1],data[,2],xlab = "X",ylab = "Y",pch = 19,cex = .5)
    center <- cov.mcd(data,cor = FALSE,quantile.used = nrow(data)*h)$center
    abline(h = center[2],col = "lightgrey",lty = 2)
    abline(v = center[1],col = "lightgrey",lty = 2)
    abline(lm(data[,2]~data[,1]),col = "darkviolet") # regression line, based on ALL values (y = dv, x = predictor)
    if (length(names_outliers) > 0){           # if there are outliers, compute the regression line excluding it
      dat2 <- data[-names_outliers,]           # matrix without outliers (IF there are outliers)
      mod <- lm(dat2[,2]~dat2[,1])              # regression line computed without outliers
      abline(mod,col = "darkgreen")}
    par(xpd = TRUE,mar = c(2,2,4,2))
    if (length(names_outliers) == 0){
      legend(x = "top",xjust = "centered",inset = c(0,-.2),legend = "Regression line",fill = "darkviolet",box.lty = 0)
    } else if (length(names_outliers) == 1){
      points(data[names_outliers,][1],data[names_outliers,][2],col = "red",bg = "red",pch = 19,cex = .5)
      text(data[names_outliers,][1],data[names_outliers,][2], as.character(names_outliers),pos = 4,cex = .75,col = "red")
      legend(x = "top",xjust = "centered",inset = c(0,-.2),legend = c("Regression line including all data","Regression line without detected outliers"),fill = c("darkviolet","darkgreen"),box.lty = 0)
    } else if (length(names_outliers) > 1){
      points(data[names_outliers,][,1],data[names_outliers,][,2],col = "red",bg = "red",pch = 19,cex = .5)
      text(data[names_outliers,][,1],data[names_outliers,][,2], as.character(names_outliers),pos = 4,cex = .75,col = "red")
      legend( x = "top",xjust = "centered",inset = c(0,-.2),legend = c("Regression line including all data","Regression line without detected outliers"),fill = c("darkviolet","darkgreen"),box.lty = 0)}

}

