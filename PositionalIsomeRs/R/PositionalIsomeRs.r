#' PositionalIsomeRs
#'
#' Processes multiple ms3 files in the current directory.
#' Scans in the specified MS3 files are filtered by the ms1 full scan parameters.
#' From each of the matching scans the masses closest to two user specified values
#' (default value 108 and 109) are identified to compare the intensity values.
#' Plots are created to visualize the intensity vs the retention time for both specified masses.
#' Plots are also created to compare the log2 ratio of the intensity vs the mass.
#'
#' @param c1 first cutoff - 108 lower limit
#' @param c2 second cutoff - 108 upper limit
#' @param c3 third cutoff - 109 lower limit
#' @param c4 fourth cutoff -109 upper limit
#' @param f1 filter value
#' @param f2 filter value
#' @param intfilt intensity filter upper limit
#'
#' @return None
#'
#' @export PositionalIsomeRs
#' @examples
#' PositionalIsomeRs()
#' PositionalIsomeRs(107.9,108.5,108.9,109.3, "150.078[0-9]", "282.121[0-9]")
#'

PositionalIsomeRs <- function(c1=107.9,c2=108.5,c3=108.9,c4=109.3,f1="150.078[0-9]",f2="282.121[0-9]", intfilt=50000){
  file.in <- list.files(pattern="*.ms3$")

  ##read every file with name ending in .ms3 in the current directory
  for(i in 1:length(file.in)){

    file.prefix <- strsplit(file.in[i], "\\.")[[1]][1]
    data.out <- readMS3(file.in[i])

    ##Filter data by mass range, deafult mass of 108 & 109
    ##Not Filtered by Intensity
    massInt.df <- smallMolecule(data.out, cutoffs=c(c1,c2,c3,c4), filter=c(f1,f2), int.filter=NA, debug.test=F)

    plot.name <- paste(file.prefix, "_Intensity_NotFiltered.png", collapse="")
    file.out <- paste(file.prefix, "_Results_NotFiltered.csv", collapse="")
    write.csv(massInt.df, file=file.out, row.names=F)

    ##only create plot if more than 10 matching rows found
    if(nrow(massInt.df)>10){
      png(file=plot.name, width=800, height=600)
      print(smallMolIntPlot(massInt.df))
      dev.off()
    }


    ##Filtered by Intensity & mass range
    massInt.filt.df <- smallMolecule(data.out, c(c1,c2,c3,c4), c(f1,f2), int.filter=intfilt, debug.test=F)
    plot.name <- paste(file.prefix, "_Ratio_filtered.png", collapse="")
    file.out <- paste(file.prefix, "_Results_Filtered.csv", collapse="")
    write.csv(massInt.filt.df, file=file.out, row.names=F)

    ##only create plot if more than 20 matching rows are found
    if(nrow(massInt.filt.df)>20){
      png(file=plot.name, width=800, height=600)
      print(smallMolRatioPlot(massInt.filt.df))
      dev.off()
    }



  }

}







