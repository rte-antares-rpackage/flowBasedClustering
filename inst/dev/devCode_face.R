library(data.table)
library(geometry)
library(rgl)
library(Rvcg)
library(pipeR)

res <- fread(system.file("data/faceAllYear.csv",package = "flowBasedClustering"))

system.time(vertices <- data.table::rbindlist(lapply(c("19/03/2016", "20/03/2016"), function(X){
  print(X)
  data.table::rbindlist(lapply(1:24, function(Y){
    resSel <- res[Date == X & Period == Y]
    print(Y)
    if(nrow(resSel)>0)
    {
      out <- getVertices(as.matrix(resSel[, .SD, .SDcols = c("BE", "DE", "FR", "NL")]),
                         resSel$RAM_0)

      # triangulation
      tc <- geometry::delaunayn(out, full = F)
      # tetramesh(tc,out,alpha=0.7)

      # tri
      tc_tri <- geometry::surf.tri(out, tc)

      # rajout d'une colonne 1
      out2 <- cbind(out, rep(1, nrow(out)))

      mesh <- rgl::tmesh3d(as.vector(t(out2[as.vector(t(tc_tri)),])),1:(nrow(tc_tri)*3))
      # wire3d(mesh)

      data.table(Date = X, Period = Y,  out = list(out), mesh = list(mesh))
    }else{
      NULL
    }
  }))
})))



# all date combinaisons
res_hour <- data.table(t(combn(unique(vertices$Date), 2)))

# distance par heure
res_dist_hour <- data.table::rbindlist(lapply(1:nrow(res_hour), function(comb){
  print(comb)
  date_1 <- res_hour[comb, V1]
  date_2 <- res_hour[comb, V2]
  v_hours <- intersect(vertices[Date%in% date_1, Period], vertices[Date%in% date_2, Period])
  data.table::rbindlist(lapply(v_hours, function(h){
    # a voir si on somme tout / positifs / negatifs, ...
    x_on_y <- vcgClost(vertices[Date %in% date_1 & Period %in% h, ]$out[[1]],
                       vertices[Date %in% date_2 & Period %in% h, ]$mesh[[1]], borderchk = FALSE, sign = FALSE)$quality
    y_on_x <- vcgClost(vertices[Date %in% date_2 & Period %in% h, ]$out[[1]],
                       vertices[Date %in% date_1 & Period %in% h, ]$mesh[[1]], borderchk = FALSE, sign = FALSE)$quality



    d <- mean(x_on_y^2) + mean(y_on_x^2)
    data.table(Date1 = date_1, Date2 = date_2, Period = h, dist = d)
  }))
}))
