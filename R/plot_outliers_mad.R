#' Plotting function for the mad
#'
#' plotting data and highlighting univariate outliers detected with the MAD function
#'
#' @param res vector of numeric values from which we want to compute outliers
#' @param x data from which the outliers_mad function was performed
#'
#' @export plot_outliers_mad
#' @keywords plot MAD outliers
#' @return None
#'
#' @importFrom stats na.omit
#' @importFrom graphics par points rect segments text title plot

plot_outliers_mad <- function(res,x){
  # plotting results
  par(mar = c(5.1,3.1,5.1,1.1))

  if(inherits(x,c("numeric","integer")) == FALSE)
    stop("x is neither numeric nor integer")

  data <- na.omit(x)   # incomplete cases are removed

  plot(NA,
       xlim = c(min(min(data),res$LL_CI_MAD)-.1*(max(data)-min(data)),
                max(max(data),res$UL_CI_MAD)+.1*(max(data)-min(data))),
       ylim = c(0,1),
       bty = "n",
       yaxt = "n",
       ylab = "",
       xlab = ""
  )

  rect(res$LL_CI_MAD,.25,
       res$UL_CI_MAD,.45,
       col = "lightgrey",
       border = "lightgrey",
       lwd = par("lwd"))

  if(res$LL_CI_MAD != res$UL_CI_MAD){
    segments(res$LL_CI_MAD,0.25,res$LL_CI_MAD,0.45, lwd = 1)
    text(res$LL_CI_MAD,.45,"lower CI limit",lwd = 1,pos = 3,cex = .75)
    segments(res$UL_CI_MAD,0.25,res$UL_CI_MAD,0.45, lwd = 1)
    text(res$UL_CI_MAD,.45,"upper CI limit",lwd = 1,pos = 3,cex = .75)
    segments(res$median,0.25,res$median,0.45, lwd = 3,col = "red")
    text(res$median,.45,"median",lwd = 1,pos = 3,cex = .75,col = "red")
  } else if (res$LL_CI_MAD == res$UL_CI_MAD){
    segments(res$LL_CI_MAD,0.25,res$LL_CI_MAD,0.45, lwd = 1)
    text(res$LL_CI_MAD,
         .45,
         paste0("lower = upper","\n","CI limit"),
         lwd = 1,
         pos = 3,
         cex = .75)
  }
  if (length(res$outliers) != 0){
    points(data[res$outliers_pos],
           rep(.35,length(res$outliers_pos)),
           col = "red",bg = "red",
           pch = 19,cex = .5)}

  title(
    main = paste(
      "Detecting values out of the Confidence Interval \n CI = Median",
      "\u00B1",res$threshold," MAD"
    )
  )

  if(length(data[res$outliers_pos]) == 0){comment <- "No outliers are detected"
  } else {comment <- paste(length(res$outliers),"outliers are detected")}

  legend("top",comment,pch = 1,col = "white",cex = 1,bty = "n")

}

