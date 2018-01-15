#' Transform a PTDF file
#'
#' Link function to transform a PTDF file for computeFB function (package antaresFlowbased)
#'
#' @param cluster \code{data.table} output of \link{clusteringTypicalDays}
#' @param PTDF \code{character}, path of a PTDF file. 
#' The PTDF file must contains at least these seven columns : Date, Period, BE, DE, FR, NL and RAM - in this order.
#' It must be a .csv file with ";" as column separator and "." as decimal separator.
#' The names of the seven columns (Date, Period, ...) must be included in the header (first line)
#' of the .csv file.
#' @param pathOutput \code{character} path of folder where file will be write, default getwd()
#' 
#' @importFrom utils type.convert
#' 
#' @examples
#'
#' \dontrun{
#' #load clustering results (or build them with clusteringTypicalDays function())
#' clusterTD <- readRDS(system.file("dataset/cluster_example.RDS",package = "flowBasedClustering"))
#' 
#' PTDF = system.file("dataset/ptdf_example.csv",
#'       package = "flowBasedClustering")
#'       
#' createPtdfForCompute(clusterTD, PTDF)
#' #Write on D:/ :
#' createPtdfForCompute(clusterTD, PTDF, pathOutput = "D:/")
#' 
#' }
#' 
#' @export
createPtdfForCompute <- function(cluster, 
                                 PTDF = system.file("dataset/ptdf_example.csv",
                                                    package = "flowBasedClustering"),
                                 pathOutput = getwd()){
  
  cluster <- copy(cluster)
  # Load PTDF
  if(!file.exists(PTDF)){
    stop(paste0(PTDF, " file not found"))
  }
  PTDF <- fread(PTDF)
  
  if("RAM_0" %in% names(PTDF)){
    setnames(PTDF, "RAM_0", "RAM")
  }
  
  # Control PTFD format
  if(any(names(PTDF)[1:7] != c("Date", "Period", "BE", "DE", "FR", "NL", "RAM"))){
    stop(paste0("Names of ptdf file must be : Date, Period, BE, DE, FR, NL, RAM currently : ",
                paste0(names(PTDF)[1:7] , collapse = ", ")))
  }
  
  PTDF <- .ctrlVertices(PTDF)
  
  
  
  dta <- PTDF[Date%in%cluster$TypicalDay]
  setnames(cluster, "TypicalDay", "Date")
  out <- merge(cluster[, .SD, .SDcols = c("Date", "idDayType")], dta, by = "Date")
  out[,"Date" := NULL]
  out <- out[order(out$idDayType, out$Period)]
  setnames(out, "idDayType" , "Id_day")
  fil <- paste0(pathOutput, "/PTDF.csv")
  if(file.exists(fil))
  {
    
    cat(paste0("Do you want to overwite ", fil," \n"))
    for (i in 1:2) {
      cat(sprintf("   %s - %s\n", i, ifelse(i == 1, "Yes", "No")))
    }
    choice <- utils::type.convert(scan(what = character(), nmax = 1), as.is = TRUE)
    if(choice == 1){
      fwrite(out, fil)
      cat(paste0("Write ",fil," done"))
    }
    
  }else{
    fwrite(out, fil)
    cat(paste0("Write ",fil," done"))
  }
}