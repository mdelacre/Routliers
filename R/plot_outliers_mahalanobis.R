#' Plotting function for the Mahalanobis distance approach
#'
#' plotting data and highlighting multivariate outliers detected with the mahalanobis distance approach
#'
#' plotting data and highlighting multivariate outliers detected with the MCD function
#' Additionnally, the plot return two regression lines: the first one including all data and
#' the second one including all observations but the detected outliers. It allows to observe how much the outliers
#' influence of outliers on the regression line.
#'
#' @param x matrix of bivariate values from which we want to compute outliers
#' @param alpha nominal type I error probability (by default .01)
#' @param na.rm set whether Missing Values should be excluded (na.rm = TRUE) or not (na.rm = FALSE) - defaults to TRUE
#'
#' @export plot_outliers_mahalanobis
#' @keywords plot mahalanobis outliers
#' @return None
#' @examples
#' ## Run plot_outliers_mahalanobis
#' data(Attacks)
#' SOC <- rowMeans(Attacks[,c("soc1r","soc2r","soc3r","soc4","soc5","soc6","soc7r","soc8","soc9","soc10r","soc11","soc12","soc13")])
#' HSC <- rowMeans(Attacks[,21:45])
#' plot_outliers_mahalanobis(x = cbind(SOC,HSC),na.rm = TRUE)
#'
#' @importFrom stats mahalanobis cov lm na.omit qchisq
#' @importFrom graphics abline legend par points

plot_outliers_mahalanobis <- function(x,
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
  coordinates <- list(x_axis = data[,1][dist > cutoff],y_axis = data[,2][dist > cutoff])

  # plotting results
  par(xpd = FALSE)
  plot(data[,1],data[,2],xlab = "X",ylab = "Y",pch = 19,cex = .5)
  abline(h = colMeans(data)[2],col = "lightgrey",lty = 2)
  abline(v = colMeans(data)[1],col = "lightgrey",lty = 2)
  # regression line, based on ALL values (y = dv, x = predictor)
  abline(lm(data[,2]~data[,1]),col = "darkviolet")
  # if there are outliers, compute the regression line excluding it
  if (length(names_outliers) > 0){
    dat2 <- data[-names_outliers,]           # matrix without outliers (IF there are outliers)
    mod <- lm(dat2[,2]~dat2[,1])              # regression line computed without outliers
    abline(mod,col = "darkgreen")}
  par(xpd = TRUE,mar = c(2,2,4,2))
  if (length(names_outliers) == 0){
    legend(x = "top",
             xjust = "centered",
             inset = c(0,-.2),
             legend = "Regression line",
             fill = "darkviolet",
             box.lty = 0)
    } else if (length(names_outliers) == 1){
      points(data[names_outliers,][1],
             data[names_outliers,][2],
             col = "red",
             bg = "red",
             pch = 19,
             cex = .5)
      text(data[names_outliers,][1],
           data[names_outliers,][2],
           as.character(names_outliers),
           pos = 4,
           cex = .75,
           col = "red")


      legend(x = "top",
             xjust = "centered",
             inset = c(0,-.2),
             legend = c(
               paste0("Regression line including all data: y = ",
                      round(lm(data[,2]~data[,1])$coefficients[1],3),
                      sign,round(lm(data[,2]~data[,1])$coefficients[2],3),"x"),
               paste0("Regression line without detected outliers: y = ",
                      round(mod$coefficients[1],3),sign2,
                      round(mod$coefficients[2],3),"x")),
             fill = c("darkviolet","darkgreen"),
             box.lty = 0)
    } else if (length(names_outliers) > 1){
      points(data[names_outliers,][,1],
             data[names_outliers,][,2],
             col = "red",
             bg = "red",
             pch = 19,
             cex = .5)

      text(data[names_outliers,][,1],
           data[names_outliers,][,2],
           as.character(names_outliers),
           pos = 4,
           cex = .75,
           col = "red")

      legend( x = "top",
              xjust = "centered",
              inset = c(0,-.2),
              legend = c(
                paste0("Regression line including all data: y = ",
                       round(lm(data[,2]~data[,1])$coefficients[1],3),
                       sign,round(lm(data[,2]~data[,1])$coefficients[2],3),"x"),
                paste0("Regression line without detected outliers: y = ",
                       round(mod$coefficients[1],3),sign2,
                       round(mod$coefficients[2],3),"x")),
              fill = c("darkviolet","darkgreen"),
              box.lty = 0)}

}



