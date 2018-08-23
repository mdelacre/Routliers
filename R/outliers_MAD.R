#' MAD function to detect outliers
#' @export outliers_MAD
#' @param data vector of numeric values from which we want to compute outliers
#' @param constant scale factor
#' @param threshold the number of MAD considered as a threshold to consider a value an outlier
#' @param na.rm set whether Missing Values should be excluded (na.rm=TRUE) or not (na.rm=FALSE) - defaults to TRUE
#' @param plot set whether results should be plotted (plot = TRUE) or not (plot = FALSE) - defaults to TRUE
#' @param verbose logical variable indicating whether text output should be generated (verbose = TRUE) or not (verbose = FALSE) - default to TRUE
#' @keywords MAD outliers
#' @return Returns median, MAD, lower MAD limit, upper MAD limit, extremely small values, extremely high values
#' @examples
#' Run outliers_MAD
#' outliers_MAD(data=c(3,3,10,10,8,1,6,50), constant=1.4826,threshold=3,na.rm=TRUE,
#' plot=TRUE, verbose=TRUE)
#' @importFrom stats na.omit qchisq
#' @importFrom graphics abline legend par points rect segments text title

outliers_MAD=function(data,
                      constant = 1.4826,
                      threshold=3,
                      na.rm = TRUE,
                      plot=TRUE,
                      verbose = TRUE){

  if(class(data)!="numeric") stop("Data are not numeric")

  if (na.rm==TRUE) {
    dat=na.omit(data)
  } else {dat=data}

  # Calculate the MAD
  center=median(dat)
  MAD=constant*median(abs(dat-center))
  half_CI=threshold*MAD

  # Calculate the range of acceptable values
  LL_CI_MAD=center-half_CI
  UL_CI_MAD=center+half_CI

  # calculate the outliers
  outliers=c(dat[dat<LL_CI_MAD],dat[dat>UL_CI_MAD])
  outliers_pos=c(which(dat<LL_CI_MAD),which(dat>UL_CI_MAD))
  # plotting results
  if (plot==TRUE){
    plot(NA, ylim=c(min(min(dat),LL_CI_MAD)-1.5*abs(min(min(dat),LL_CI_MAD)),max(max(dat),UL_CI_MAD)+1.5*abs(max(max(dat),UL_CI_MAD))),xlim=c(0,1), bty="n",xaxt="n", ylab="",xlab="")
    rect(.25, LL_CI_MAD, .35, UL_CI_MAD, col = "lightgrey", border = "lightgrey", lwd = par("lwd"))
    segments(0.25,LL_CI_MAD,0.35,LL_CI_MAD, lwd=1)
    text(.35,LL_CI_MAD,"lower MAD limit",lwd=1,pos=4,cex=.75)
    segments(0.25,UL_CI_MAD,0.35,UL_CI_MAD, lwd=1)
    text(.35,UL_CI_MAD,"upper MAD limit",lwd=1,pos=4,cex=.75)
    segments(0.25,center,0.35,center, lwd=3,col="red")
    text(.35,center,"median",lwd=1,pos=4,cex=.75)
    if (length(outliers)!=0){

    points(rep(.3,length(outliers_pos)),dat[outliers_pos],col="red",bg="red",pch=19,cex=.5)
    text(.3, outliers, paste("POS",as.character(outliers_pos)),pos=4,cex=.75,col="red")}

    title(main=paste("Detecting outliers \n Threshold=",threshold,"MAD around the median"))
  }

  if(verbose == TRUE){
    cat("Results:\n")
    cat("Median:",format(center, digits = 1, nsmall = 3, scientific = FALSE))
    cat("\n")
    cat("MAD:",format(MAD, digits = 1, nsmall = 3, scientific = FALSE))
    cat("\n\n")
    cat("Acceptable range of values:")
    cat("\n")
    cat("lower MAD limit:", paste0(round(LL_CI_MAD, digits = 4)),"\nupper MAD limit:",paste0(round(UL_CI_MAD, digits = 4)))
    cat("\n\n")
    cat("Outliers:")
    cat("\n\n")
    cat("extremely small values:", "\npositions:",which(dat<LL_CI_MAD),"\nvalues:",paste0(round(dat[dat<LL_CI_MAD], digits = 4)))
    cat("\n\n")
    cat("extremely high values:", "\npositions:",which(dat>UL_CI_MAD),"\nvalues:",paste0(round(dat[dat>UL_CI_MAD], digits = 4)))
  }

  # Return results in list()
  invisible(list(Median = center, MAD = MAD, LL_CI_MAD = LL_CI_MAD,UL_CI_MAD = UL_CI_MAD,L_outliers=dat[dat<LL_CI_MAD],U_outliers=dat[dat>UL_CI_MAD],outliers=c(dat[dat<LL_CI_MAD],dat[dat>UL_CI_MAD])))

}

# Note: in the mad() function from the package stats
# It is possible to use other center than the median. Should I include it? (for I don't know trimmed median or similar)

data=c(2,3,4,9,60,40)
outliers_MAD(data)


