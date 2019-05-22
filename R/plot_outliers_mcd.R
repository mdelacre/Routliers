#' Plotting function for the MCD
#'
#' plotting data and highlighting multivariate outliers detected with the MCD function
#' Additionnally, the plot return two regression lines: the first one including all data and
#' the second one including all observations but the detected outliers. It allows to observe how much the outliers
#' influence of outliers on the regression line.
#'
#' @param res result of the outliers_mad function from which we want to create a plot
#' @param x matrix of multivariate values from which we want to compute outliers. Last column of the matrix is considered as the DV in the regression line.
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
#' res <- outliers_mcd(x = cbind(SOC,HSC),na.rm=TRUE,h=.75)
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
#' c3 <- c(1,2,3,1,4,3,5,5)
#' c4 <- c(1,2,3,1,5,3,5,5)
#' res3 <- outliers_mcd(x = cbind(c3,c4),na.rm=TRUE)
#' plot_outliers_mcd(res3,x=cbind(c3,c4),pos_display=TRUE)
#' @importFrom stats mahalanobis lm na.omit qchisq
#' @importFrom MASS cov.mcd
#' @importFrom graphics abline legend par points plot
#' @import ggplot2

plot_outliers_mcd <- function(res,
                              x,
                              pos_display=FALSE){

  data <- data.frame(x)

  for (i in seq_len(ncol(data))){
    if(inherits(data[,i],c("numeric","integer")) == FALSE)
      stop("Data are neither numeric nor integer")
  }

  # specify for each point if it is a standard point or an outlier
  values <- NULL
  for (i in seq_len(nrow(data))){
    if(res$dist_from_center[i] > res$distance){
      values[i] <- "varoutlier"
    } else {values[i] <- "standard"}
  }
  values <- as.factor(values)

  # basic plot
  p <- ggplot(data,aes(data[,1],data[,2])) +
    geom_point(aes(x=data[,1], y=data[,2], shape=values, color=values)) +
    geom_smooth(aes(x=data[,1], y=data[,2]),
                method=lm,
                se=FALSE,
                colour="darkviolet"
                ) +
    theme(legend.position="none",
          legend.title = element_blank(),
          panel.background = element_rect(fill="white",
                                          colour="white"),
          axis.line = element_line(colour = "black",
                                   size = .5,
                                   linetype = "solid")
    ) +
    geom_hline(yintercept=res$center[2], linetype=2, color="lightgrey") +
    geom_vline(xintercept=res$center[1], linetype=2, color="lightgrey") +
    xlab(colnames(data)[1]) + ylab(colnames(data)[ncol(data)])

  # if pos_display = TRUE, adding annotations in outliers points
  if (pos_display==TRUE){
    display_decision <- annotate(geom="text",
                                 x=res$outliers_val[,1],
                                 y=res$outliers_val[,2],
                                 label=res$outliers_pos,
                                 color="red",
                                 hjust="inward",
                                 vjust="inward"
                                 )
  } else {display_decision <- NULL}

  if (length(res$outliers_pos) == 0){ # if there are no outliers in the plot

    DV <- names(data)[ncol(data)]
    names(data)[ncol(data)]="DV"
    regr <- lm(DV~.,data=data)$coefficients
    # Translate regr into a regression line
    regr_line <- paste("Regression line:", DV," = ",round(regr[1],3))
    for (k in seq_len(length(regr)-1)){
      if (regr[k+1] > 0){regr_line <- paste0(regr_line," + ",abs(round(regr[k+1],3))," ",names(regr)[k+1])
      } else {regr_line <- paste0(regr_line," - ",abs(round(regr[k+1],3))," ",names(regr)[k+1])}
    }

    # plotting results, with one regression line (including all data points)
    p +
      scale_shape_manual(values=c(16,16),
                         labels=c("standard values",
                                  "standard values")
                         ) +
      scale_color_manual(values=c('black','black'),
                         labels=c("standard values",
                                  "standard values")
                         )+
      ggtitle(label=regr_line) +
      theme(plot.title = element_text(hjust = 0.5,color="darkviolet",size=12))

  } else { # if there are detected outliers

    # regression line including all data:
    DV <- names(data)[ncol(data)]
    names(data)[ncol(data)]="DV"
    regr <- lm(DV~.,data=data)$coefficients
    regr_line <- paste("Regression line including all data:",DV," = ",round(regr[1],3))
    for (k in seq_len(length(regr)-1)){
      if (regr[k+1] > 0){regr_line <- paste0(regr_line," + ",abs(round(regr[k+1],3))," ",names(regr)[k+1])
      } else {regr_line <- paste0(regr_line," - ",abs(round(regr[k+1],3))," ",names(regr)[k+1])}
    }
    # regression without outliers:
    dat2 <- data[-res$outliers_pos,]
    names(dat2)[ncol(dat2)]="DV"
    regr_no <- lm(DV~.,data=dat2)$coefficients
    regr_line_no <- paste("Regression line without detected outliers:",DV," = ",round(regr_no[1],3))
    for (k in seq_len(length(regr_no)-1)){
      if (regr_no[k+1] > 0){regr_line_no <- paste0(regr_line_no," + ",abs(round(regr_no[k+1],3))," ",names(regr_no)[k+1])
      } else {regr_line_no <- paste0(regr_line_no," - ",abs(round(regr_no[k+1],3))," ",names(regr_no)[k+1])}
    }

    p + geom_abline(slope=regr_no[2],
                    intercept=regr_no[1],
                    colour="darkgreen",size=.8) +
      scale_shape_manual(values=c(16,15),
                         labels=c("standard values",
                                  "outliers")) +
      scale_color_manual(values=c('black','red'),
                         labels=c("standard values",
                                  "outliers")) +
      ggtitle(label=regr_line,subtitle =regr_line_no) +
      theme(plot.title = element_text(hjust = 0.5,color="darkviolet",size=12),
            plot.subtitle = element_text(hjust = 0.5,color="darkgreen",size=12)
      ) +
      display_decision

  }


}
