#' MAD function to detect outliers
#' @export plot_outliers_mad
#' @param data vector of numeric values from which we want to compute outliers
#' @param constant scale factor
#' @param threshold the number of MAD considered as a threshold to consider a value an outlier
#' @param na.rm set whether Missing Values should be excluded (na.rm=TRUE) or not (na.rm=FALSE) - defaults to TRUE
#' @param verbose logical variable indicating whether text output should be generated (verbose = TRUE) or not (verbose = FALSE) - default to TRUE
#' @keywords plot MAD outliers
#' @examples
#' Run outliers_mad
#' plot_outliers_mad(data=rnorm(150), constant=1.4826,threshold=3,na.rm=TRUE,
#' plot=TRUE, verbose=TRUE)
#' @importFrom stats na.omit
#' @importFrom graphics legend par points rect segments text title

plot_outliers_mad=function(data,
                      constant = 1.4826,
                      threshold=3,
                      na.rm = TRUE,
                      verbose = TRUE){

  if(inherits(data,c("numeric","integer"))==FALSE) stop("Data are neither numeric nor integer")

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


