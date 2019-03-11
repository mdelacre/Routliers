#' MAD function to detect outliers
#'
#' Detecting univariate outliers using the robust median absolute deviation
#'
#' @param data vector of values from which we want to compute outliers
#' @param b constant depending on the assumed distribution underlying the data, that equals 1/Q(0.75).  When the normal distribution is assumed, the constant 1.4826 is used (and it makes the MAD and SD of normal distributions comparable).
#' @param threshold the number of MAD considered as a threshold to consider a value an outlier
#' @param na.rm set whether Missing Values should be excluded (na.rm=TRUE) or not (na.rm=FALSE) - defaults to TRUE
#' @param verbose logical variable indicating whether text output should be generated (verbose = TRUE) or not (verbose = FALSE) - default to TRUE
#'
#' @export outliers_mad
#' @keywords MAD outliers
#'
#' @return Returns median, MAD, lower MAD limit, upper MAD limit, extremely small values, extremely high values
#' @examples
#' Run outliers_mad
#' data=
#'
#' ability.cov
#' outliers_mad(data=rnorm(150), b=1.4826,threshold=3,na.rm=TRUE,
#' plot=TRUE, verbose=TRUE)
#' @importFrom stats na.omit

outliers_mad=function(data,
                      b = 1.4826,
                      threshold=3,
                      na.rm = TRUE,
                      verbose = TRUE){

  if(inherits(data,c("numeric","integer"))==FALSE) stop("Data are neither numeric nor integer")

  if (na.rm==TRUE) {
    dat=na.omit(data)
  } else {dat=data}

  # Calculate the MAD
  center=median(dat)
  MAD=b*median(abs(dat-center))
  half_CI=threshold*MAD

  # Calculate the range of acceptable values
  LL_CI_MAD=center-half_CI
  UL_CI_MAD=center+half_CI

  # calculate the outliers
  outliers=c(dat[dat<LL_CI_MAD],dat[dat>UL_CI_MAD])
  outliers_pos=c(which(dat<LL_CI_MAD),which(dat>UL_CI_MAD))
  # plotting results

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

