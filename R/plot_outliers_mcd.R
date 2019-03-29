#' Plotting function for the MCD
#'
#' plotting data and highlighting multivariate outliers detected with the MCD function
#' Additionnally, the plot return two regression lines: the first one including all data and
#' the second one including all observations but the detected outliers. It allows to observe how much the outliers
#' influence of outliers on the regression line.
#'
#' @param res result of the outliers_mad function from which we want to create a plot
#' @param x matrix of bivariate values from which we want to compute outliers
#' @param pos_display set whether the position of outliers in the dataset should be displayed on the graph (pos_display = TRUE)
#' or not (pos_display = FALSE)
#'
#' @export plot_outliers_mcd
#' @keywords plot MMCD outliers
#' @return None
#' @examples
#' #### Run plot_outliers_mcd
#' data(Attacks)
#' SOC <- rowMeans(Attacks[,c("soc1r","soc2r","soc3r","soc4","soc5","soc6",
#' "soc7r","soc8","soc9","soc10r","soc11","soc12","soc13")])
#' HSC <- rowMeans(Attacks[,22:46])
#' res <- outliers_mcd(x = cbind(SOC,HSC),na.rm=TRUE)
#' plot_outliers_mcd(res,x = cbind(SOC,HSC))
#'
#' # it's also possible to display the position of the multivariate outliers ion the graph
#' # preferably, when the number of multivariate outliers is not too high
#' c1 <- c(1,4,3,6,5,2,1,3,2,4,7,3,6,3,4,6)
#' c2 <- c(1,3,4,6,5,7,1,4,3,7,50,8,8,15,10,6)
#' res2 <- outliers_mcd(x = cbind(c1,c2),na.rm=TRUE)
#' plot_outliers_mcd(res2, x=cbind(c1,c2),pos_display=TRUE)
#'
#' # When no outliers are detected, only one regression line is displayed
#' c3 <- c(1,2,3,1,4,3,6,5)
#' c4 <- c(1,2,3,1,3,4,6,5)
#' res3 <- outliers_mcd(x = cbind(c3,c4),na.rm=TRUE)
#' plot_outliers_mcd(res3,x=cbind(c3,c4),pos_display=TRUE)
#' @importFrom stats mahalanobis lm na.omit qchisq
#' @importFrom MASS cov.mcd
#' @importFrom graphics abline legend par points plot

plot_outliers_mcd <- function(res,
                              x,
                              pos_display=FALSE){

  data <- x
  for (i in seq_len(ncol(data))){
    if(inherits(data[,i],c("numeric","integer")) == FALSE)
      stop("Data are neither numeric nor integer")
  }

  # plotting results
  par(xpd = FALSE)
  plot(data[,1],data[,2],xlab = "X",ylab = "Y",pch = 19,cex = .5)

  abline(h = res$center[2],col = "lightgrey",lty = 2)
  abline(v = res$center[1],col = "lightgrey",lty = 2)
  # regression line, based on ALL values (y = dv, x = predictor)
  abline(lm(data[,2]~data[,1]),col = "darkviolet")
  # if there are outliers, compute the regression line excluding it
  if (length(res$outliers_pos) > 0){
    # matrix without outliers (IF there are outliers):
    dat2 <- data[-res$outliers_pos,]
    # regression line computed without outliers:
    mod <- lm(dat2[,2]~dat2[,1])
    abline(mod,col = "darkgreen")}
  par(xpd = TRUE,mar = c(2,2,4,2))
  if (length(res$outliers_pos) == 0){
    if(lm(data[,2]~data[,1])$coefficients[2] > 0){
      sign <- "+"
    } else {sign <- ""}

    legend(x = "top",
           xjust = "centered",
           inset = c(0,-.2),
           legend = paste0("Regression line: y = ",
                           round(lm(data[,2]~data[,1])$coefficients[1],3),
                           sign,
                           round(lm(data[,2]~data[,1])$coefficients[2],3),
                           "x"
           ),
           fill = "darkviolet",
           box.lty = 0)
  } else if (length(res$outliers_pos) == 1){
    points(data[res$outliers_pos,][1],
           data[res$outliers_pos,][2],
           col = "red",
           bg = "red",
           pch = 15,
           cex = .6)

    if (pos_display==TRUE){
      text(data[res$outliers_pos,][1],
           data[res$outliers_pos,][2],
           as.character(res$outliers_pos),
           pos = 1,
           cex = .75,
           col = "red")}

    if(lm(data[,2]~data[,1])$coefficients[2] > 0){
      sign <- "+"
    } else {sign <- ""}

    if(lm(dat2[,2]~dat2[,1])$coefficients[2] > 0){
      sign2 <- "+"
    } else {sign2 <- ""}

    legend(x = "top",
           xjust = "centered",
           inset = c(0,-.2),
           legend = c(paste0("Regression line including all data: y = ",
                             round(lm(data[,2]~data[,1])$coefficients[1],3),
                             sign,round(lm(data[,2]~data[,1])$coefficients[2],3),"x"),
                      paste0("Regression line without detected outliers:
                             y = ",
                             round(mod$coefficients[1],3),sign2,
                             round(mod$coefficients[2],3),"x")),
           fill = c("darkviolet","darkgreen"),
           box.lty = 0)

  } else if (length(res$outliers_pos) > 1){
    points(data[res$outliers_pos,][,1],
           data[res$outliers_pos,][,2],
           col = "red",
           bg = "red",
           pch = 15,
           cex = .6)

    if (pos_display==TRUE){
      for (i in seq_len(length(res$outliers_pos))){
        text(data[res$outliers_pos,][i,1],
             data[res$outliers_pos,][i,2],
             as.character(res$outliers_pos[i]),
             pos = 1,
             cex = .75,
             col = "red")}}

    if(lm(data[,2]~data[,1])$coefficients[2] > 0){
      sign <- "+"
    } else {sign <- ""}

    if(lm(dat2[,2]~dat2[,1])$coefficients[2] > 0){
      sign2 <- "+"
    } else {sign2 <- ""}

    legend(x = "top",
           xjust = "centered",
           inset = c(0,-.2),
           legend = c(paste0("Regression line including all data: y = ",
                             round(lm(data[,2]~data[,1])$coefficients[1],3),
                             sign,round(lm(data[,2]~data[,1])$coefficients[2],3),"x"),
                      paste0("Regression line without detected outliers:
                                     y = ",
                             round(mod$coefficients[1],3),sign2,
                             round(mod$coefficients[2],3),"x")),
           fill = c("darkviolet","darkgreen"),
           box.lty = 0)}



}


