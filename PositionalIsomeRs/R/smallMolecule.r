#' smallMolecule
#'
#' Scans in the specified MS3 files are filtered by the ms1 full scan parameters.
#' From each of the matching scans the masses closest to two user specified values
#' (default value 108 and 109) are identified to compare the intensity values.
#' Plots are created to visualize the intensity vs the retention time for both specified masses.
#' Plots are also created to compare the log2 ratio of the intensity vs the mass.
#'
#' @param data.in list structure produced by readMS3
#' @param cutoffs mass cutoff values
#' @param filter filter values
#' @param int.filter intensity upper limit
#' @param debug.test set to TRUE for debugging output
#'
#' @return a data frame of filtered values
#'
#' @export smallMolecule
#' @export smallMolIntPlot
#' @export smallMolRatioPlot
#' @examples
#' smallMolecule(data.out, cutoffs=c(c1,c2,c3,c4), filter=c(f1,f2))
#'
#' @import ggplot2
#' @import reshape
#' @import ggsci
#'
smallMolecule <- function(data.out, cutoffs, filter, int.filter=NA, debug.test=F){
  lst.match <- sapply(data.out, function(x) grepl(filter[1],x$Filter) & grepl(filter[2],x$Filter))

  data.match <- data.out[which(lst.match)]


  MassInt.df <- data.frame("ScanNumber"=NA, "RetentionTime"=NA, "Mass108"=NA, "Intensity108"=NA,"Mass109"=NA, "Intensity109"=NA, "IntensityRatio"=NA, "LogIntensityRatio"=NA)
  df.iv <- 1

  if(length(data.match)>0){

      int.match <- F

      for(i in 1:length(data.match)){
        iv1 <- which(data.match[[i]]$MassInt[,"Mass"]>=cutoffs[1] & data.match[[i]]$MassInt[,"Mass"]<=cutoffs[2])[1]
        if(is.na(iv1)){ next }
        if(length(iv1)==0){ next }
        if(!is.na(int.filter)){
          if(data.match[[i]]$MassInt[iv1,"Intensity"]<int.filter){
            int.match <- T
          }else{
            int.match <- F
          }
        }

        iv2 <- which(data.match[[i]]$MassInt[,"Mass"]>=cutoffs[3] & data.match[[i]]$MassInt[,"Mass"]<=cutoffs[4])[1]
        if(is.na(iv2)){ next }
        if(length(iv2)==0){ next }

        if(!is.na(int.filter)){
          if(data.match[[i]]$MassInt[iv2,"Intensity"]<int.filter){
            if(int.match){ next }
          }else{
            int.match <- F
          }
        }

        MassInt.df[df.iv,"Mass108"] = data.match[[i]]$MassInt[iv1,"Mass"]
        MassInt.df[df.iv,"Mass109"] = data.match[[i]]$MassInt[iv2,"Mass"]
        MassInt.df[df.iv,"Intensity108"] = data.match[[i]]$MassInt[iv1,"Intensity"]
        MassInt.df[df.iv,"Intensity109"] = data.match[[i]]$MassInt[iv2,"Intensity"]
        MassInt.df[df.iv,"ScanNumber"] = data.match[[i]]$ScanNo
        MassInt.df[df.iv,"RetentionTime"] = data.match[[i]]$RetentionTime
        MassInt.df[df.iv,"IntensityRatio"] = MassInt.df[df.iv,"Intensity108"]/MassInt.df[df.iv,"Intensity109"]
        MassInt.df[df.iv,"LogIntensityRatio"] = log2(MassInt.df[df.iv,"IntensityRatio"])
        df.iv <-  df.iv+1

      }

    MassInt.df  <- MassInt.df[!(apply(MassInt.df, 1, function(x) sum(is.na(x))>0)),]

  }

  MassInt.df

}


smallMolIntPlot <- function(massInt.df, debug.test=F){

  massInt.df[,"RetentionTime"] <- as.character(massInt.df[,"RetentionTime"])
  MassInt.melt <- melt(massInt.df[,c(1,2,4,6)])
  MassInt.melt[,"RetentionTime"] <- as.numeric(MassInt.melt[,"RetentionTime"])

  if(debug.test){
    print(MassInt.melt$value)
  }

  ggplot(data=MassInt.melt, aes(x=RetentionTime, y=value, group=variable, color=variable)) +
    geom_line(size=1) +
    scale_x_continuous(breaks=seq(0, round(as.numeric(max(MassInt.melt[,c("RetentionTime")]), 0)), 5)) +
    scale_y_continuous(breaks=seq(0, round(as.numeric(max(MassInt.melt[,c("value")]), -1*(nchar(round(max(MassInt.melt[,c("value")])/10))-1))),
                                  round(as.numeric(max(MassInt.melt[,c("value")]))/12, -1*(nchar(round(max(MassInt.melt[,c("value")])/10))-1)))) +
    xlab("Retention Time") +
    ylab("Intensity") +
    scale_color_d3() +
    theme(legend.title=element_blank()) +
    theme(text=element_text(size=12), axis.text=element_text(size=12), legend.text=element_text(size=12))
}

smallMolRatioPlot <- function(massInt.df, debug.test=F){

  massInt.df[,"RetentionTime"] <- as.numeric(massInt.df[,"RetentionTime"])

  ggplot(massInt.df, aes(x=RetentionTime, y=LogIntensityRatio)) +
    geom_point(color="darkblue", size=3) +
    scale_x_continuous(breaks=seq(0, round(max(as.numeric(massInt.df[,c("RetentionTime")]), 1)), 5)) +
    xlab("Retention Time") +
    ylab("Log2 Ratio") +
    theme(text=element_text(size=12), axis.text=element_text(size=12))
}

