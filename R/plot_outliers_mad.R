#' Plotting function for the mad
#'
#' plotting data and highlighting univariate outliers detected with the MAD function
#'
#' @param x vector of numeric values from which we want to compute outliers
#' @param b constant depending on the assumed distribution underlying the data, that equals 1/Q(0.75).  When the normal distribution is assumed, the constant 1.4826 is used (and it makes the MAD and SD of normal distributions comparable).
#' @param threshold the number of MAD considered as a threshold to consider a value an outlier
#' @param na.rm set whether Missing Values should be excluded (na.rm = TRUE) or not (na.rm = FALSE) - defaults to TRUE
#'
#' @export plot_outliers_mad
#' @keywords plot MAD outliers
#' @return None
#' @examples
#' ## Run plot_outliers_mad
#' #plot_outliers_mad(x = runif(150,-100,100), b = 1.4826,threshold = 3,na.rm = TRUE)
#' #data(Intention)
#' #plot_outliers_mad(x = Intention$age)
#' #plot_outliers_mad(x = Intention$Total_Amount_Earned)
#' #data(Attacks)
#' #SOC <- rowMeans(Attacks[,c("soc1r","soc2r","soc3r","soc4","soc5","soc6","soc7r","soc8","soc9","soc10r","soc11","soc12","soc13")])
#' #plot_outliers_mad(x = SOC)
#'
#' @importFrom stats na.omit
#' @importFrom graphics par points rect segments text title

plot_outliers_mad <- function(x,
                              b = 1.4826,
                              threshold = 3,
                              na.rm = TRUE){

  if(inherits(x,c("numeric","integer")) == FALSE)
    stop("Data are neither numeric nor integer")

  if (na.rm == TRUE) {
    data <- na.omit(x)
  } else {data <- x}

  # Calculate the MAD
  center <- median(data)
  MAD <- b*median(abs(data-center))
  half_CI <- threshold*MAD

  # Calculate the range of acceptable values
  LL_CI_MAD <- center-half_CI
  UL_CI_MAD <- center+half_CI

  # calculate the outliers
  outliers <- c(data[data < LL_CI_MAD],data[data > UL_CI_MAD])
  outliers_pos <- c(which(data < LL_CI_MAD),which(data > UL_CI_MAD))

  # plotting results
  par(mar = c(5.1,3.1,5.1,1.1))
  plot(NA,
       xlim = c(min(min(data),LL_CI_MAD)-.1*(max(data)-min(data)),max(max(data),UL_CI_MAD)+.1*(max(data)-min(data))),
       ylim = c(0,1),
       bty = "n",
       yaxt = "n",
       ylab = "",
       xlab = ""
       )

  rect(LL_CI_MAD,.25,UL_CI_MAD,.45,col = "lightgrey", border = "lightgrey", lwd = par("lwd"))

  if(LL_CI_MAD != UL_CI_MAD){
    segments(LL_CI_MAD,0.25,LL_CI_MAD,0.45, lwd = 1)
    text(LL_CI_MAD,.45,"lower CI limit",lwd = 1,pos = 3,cex = .75)
    segments(UL_CI_MAD,0.25,UL_CI_MAD,0.45, lwd = 1)
    text(UL_CI_MAD,.45,"upper CI limit",lwd = 1,pos = 3,cex = .75)
    segments(center,0.25,center,0.45, lwd = 3,col = "red")
    text(center,.45,"median",lwd = 1,pos = 3,cex = .75,col = "red")
  } else if (LL_CI_MAD == UL_CI_MAD){
    segments(LL_CI_MAD,0.25,LL_CI_MAD,0.45, lwd = 1)
    text(LL_CI_MAD,.45,paste0("lower = upper","\n","CI limit"),lwd = 1,pos = 3,cex = .75)
  }
  if (length(outliers) != 0){
    points(data[outliers_pos],rep(.35,length(outliers_pos)),col = "red",bg = "red",pch = 19,cex = .5)}

  title(
    main = paste("Detecting values out of the Confidence Interval \n CI = Median",
                 "\u00B1",threshold," MAD")
    )

  if(length(data[outliers_pos]) == 0){comment <- "No outliers are detected"
  } else {comment <- paste(length(outliers),"outliers are detected")}

  legend("top",comment,pch = 1,col = "white",cex = 1,bty = "n")
}


