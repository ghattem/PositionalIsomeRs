#' readMS3
#'
#' Reads an .ms3 file
#'
#' @param file.in file name of .ms3 file
#'
#' @return a list with scan no, retention time, filter & MassInt
#'
#' @export readMS3
#' @examples
#' readMS3("10mix_1.ms3")
#'

##Read data from ms3 file
readMS3 <- function(file.in){

  con <- file(description = file.in, open="")

  data.in <- readLines(con=con, n=-1)

  data.out <- list()

  scan.num <- ""
  ret.time <- ""
  filter.in <- ""

  scan.i <- 1
  i <- 1

  repeat {
    tmp <- strsplit(data.in[i],split=c("\t",","))
    if(is.na(tmp[[1]][1])){ break }
    i<-i+1
    if(tmp[[1]][1] == "H"){ next }
    if(tmp[[1]][1] == "S"){
      if(scan.i==2){
        data.out[[1]] <- list("ScanNo"=scan.num, "RetentionTime"=ret.time, "Filter"=filter.in, "MassInt"=df)
      }else if(scan.i>2){
        data.out[[scan.i-1]] <- list("ScanNo"=scan.num, "RetentionTime"=ret.time, "Filter"=filter.in, "MassInt"=df)
      }

      scan.num <-  tmp[[1]][3]
      scan.i <- scan.i+1
      df <- data.frame("Mass"=double(), "Intensity"=double())
      j<-1
      next
    }
    if(tmp[[1]][1] == "I"){
      if(tmp[[1]][2] == "RetTime"){ret.time <-tmp[[1]][3]}
      if(tmp[[1]][2] == "Filter"){filter.in <-tmp[[1]][3]}
      next
    }
    if(tmp[[1]][1] == "Z"){ next }

    if(grepl("^[[:digit:]]", tmp[[1]][1])){
      mi <- as.double(strsplit(tmp[[1]][1], "\\s+")[[1]])
      if(!is.na(mi[1])){
        df[j,"Mass"] <- mi[1]
        df[j,"Intensity"] <- mi[2]
        j<- j+1
      }
      next
    }

  }

  data.out[[scan.i-1]] <- list("ScanNo"=scan.num, "RetentionTime"=ret.time, "Filter"=filter.in, "MassInt"=df)

  close(con)

  data.out
}
