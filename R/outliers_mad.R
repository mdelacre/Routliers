#' MAD function to detect outliers
#'
#' Detecting univariate outliers using the robust median absolute deviation
#'
#' @param data vector of values from which we want to compute outliers
#' @param b constant depending on the assumed distribution underlying the data, that equals 1/Q(0.75).  When the normal distribution is assumed, the constant 1.4826 is used (and it makes the MAD and SD of normal distributions comparable).
#' @param threshold the number of MAD considered as a threshold to consider a value an outlier
#' @param na.rm set whether Missing Values should be excluded (na.rm=TRUE) or not (na.rm=FALSE) - defaults to TRUE
#'
#' @export outliers_mad
#' @keywords MAD outliers
#'
#' @return Returns median, MAD, lower MAD limit, upper MAD limit, extremely small values, extremely high values
#' @examples
#' ## Run outliers_mad
#' outliers_mad(data=runif(150,-100,100), b=1.4826,threshold=3,na.rm=TRUE)
#' outliers_mad(data=Intention$age)
#' outliers_mad(data=Intention$Total_Amount_Earned)
#' SOC=rowMeans(Attacks[,c("soc1r","soc2r","soc3r","soc4","soc5","soc6","soc7r","soc8","soc9","soc10r","soc11","soc12","soc13")])
#' res=outliers_mad(data=SOC)
#'
#' @importFrom stats na.omit

outliers_mad=function(data,
                      b = 1.4826,
                      threshold=3,
                      na.rm = TRUE){

  # If data are numeric or integer, applying the function. Otherwise, stopping it.
  if(inherits(data,c("numeric","integer"))==FALSE) stop("Data are neither numeric nor integer")

  if (na.rm==TRUE) {
    dat=na.omit(data)   # incomplete cases are removed
  } else {dat=data}

  # Calculate the MAD
  center=median(dat)
  MAD=b*median(abs(dat-center))
  half_CI=threshold*MAD # how many MAD from the median are the limits of the IC?

  # Calculate the range of acceptable values
  LL_CI_MAD=center-half_CI # lower limit of the median CI
  UL_CI_MAD=center+half_CI # upper limit of the median CI

  # calculate the outliers
  outliers=c(dat[dat<LL_CI_MAD],dat[dat>UL_CI_MAD])
  outliers_pos=c(which(dat<LL_CI_MAD),which(dat>UL_CI_MAD))

    print.outliers_MAD <- function(x){
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

outliers_mad(Attacks$age)$L_outliers

