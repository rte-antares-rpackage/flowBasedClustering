# library(data.table)
# library(geometry)
# library(rgl)
# library(Rvcg)
#
# res <- fread(system.file("data/faceAllYear.csv",package = "flowBasedClustering"))
# vertices <- sapply(unique(res$Date)[1:3], function(X){
#   sapply(1, function(Y){
#     print(X)
#     resSel <- res[Date == X & Period == Y]
#     if(nrow(resSel)>0)
#     {
#     out <- getVertices(as.matrix(resSel[, .SD, .SDcols = c("BE", "DE", "FR", "NL")]),
#                 resSel$RAM_0)
#     #tp <- convhulln(out)
#
#     data.table(Date = X, Period = Y,  mesh = list(as.mesh3d(deldir::deldir(out[,1],out[,2],z =out[,3]))))
#     # cbind(out[c(tp[,1], tp[1,1]),1],
#     #                                          out[c(tp[,2], tp[1,2]),2],
#     #                                          out[c(tp[,3], tp[1,3]),3]))
#     }else{
#       NULL
#     }
#
#   }, simplify = FALSE)
# }, simplify = FALSE)
#
# vertices <- rbindlist(lapply(vertices, rbindlist))
#
# re2 <- vcgClostKD(vertices[1, ]$mesh[[1]], vertices[2, ]$mesh[[1]], borderchk = TRUE)$quality
# dd <- sum(re2[re2>0])
#
#
#
#
#
#


#
#
# library(rgeos )
#
# tt1 <- vertices[Date == '01/11/2015', .SD, .SDcols = c("V1", "V2", "V3")]
# tt2 <- vertices[Date == '02/11/2015', .SD, .SDcols = c("V1", "V2", "V3")]
# tt1 <- tt1[1:2]
# tt2 <- tt2[1]
# TRI <- dist(rbind(tt1, tt2))
# h <- sqrt(TRI[3]^2 - (TRI[2]^2-((TRI[1]^2+TRI[3]^2)/(2*TRI[2]))^2))
#
#
#
#
# convertToPoly <- function(dta, date)
# {
# poly <- as.matrix(dta[Date == date, .SD, .SDcols = c("V1","V2", "V3")])
# polyStruct <- apply(poly,1,function(X){paste(round(X,0), collapse = " ")})
# polyStruct <- paste(polyStruct, collapse = ",")
# p2 = readWKT(paste0("POLYGON((", polyStruct, "))"))
# p2}
# poly1 <- convertToPoly(vertices, "01/11/2015")
# poly2 <- convertToPoly(vertices, "02/11/2015")
#
# gDistance(poly1, poly2)
