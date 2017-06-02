library(data.table)
library(geometry)
library(rgl)
library(Rvcg)
library(pipeR)
library(ptinpoly)

res <- fread("D:/Users/titorobe/Desktop/Antares/flowBasedClustering/inst/dev/sommets_IWE.txt")
res_ampl <- fread("D:/Users/titorobe/Desktop/clustering_pr_damien_mars2017_Benoit/code_ampl/inter_we/distance_hours_IWE_R.csv")


PTDF <- fread("D:/Users/titorobe/Desktop/clustering_pr_damien_mars2017_Benoit/code_ampl/test_R_benoit/faceAllYear.csv")
PTDF$Date <- gsub("/", "_", PTDF$Date)



colnames(res) <- c("Date", "Period", "X1", "X2", "X3")
PTDF <- PTDF[Date %in% res$Date]

system.time(vertices <- data.table::rbindlist(lapply(unique(res$Date), function(X){
  data.table::rbindlist(lapply(1:24, function(Y){
    print(X)
    # resSel <- res[Date == X & Period == Y]
    out <- as.matrix(res[Date == X & Period == Y, list(X1, X2, X3)])

    if(nrow(out)>0)
    {

      # deja des vertices
      # out <- getVertices(as.matrix(resSel[, .SD, .SDcols = c("BE", "DE", "FR", "NL")]),
      #             resSel$RAM_0)

      # triangulation
      tc <- geometry::delaunayn(out, full = F)

      # tetramesh(tc,out,alpha=0.7)

      # tri
      tc_tri <- geometry::surf.tri(out, tc)

      # rajout d'une colonne 1
      out2 <- cbind(out, rep(1, nrow(out)))

      mesh <- rgl::tmesh3d(as.vector(t(out2[c(as.vector(t(tc_tri))),])),1:((nrow(tc_tri)*3)))
      # wire3d(mesh, col = "green")

      data.table(Date = X, Period = Y,  out = list(out), mesh = list(mesh))
    }else{
      NULL
    }
  }))
})))

# all date combinaisons
res_hour <- data.table(t(combn(unique(vertices$Date), 2)))

# distance par heure
system.time(res_dist_hour <- data.table::rbindlist(lapply(1:nrow(res_hour), function(comb){
  date_1 <- res_hour[comb, V1]
  date_2 <- res_hour[comb, V2]
  v_hours <- intersect(vertices[Date%in% date_1, Period], vertices[Date%in% date_2, Period])
  data.table::rbindlist(lapply(v_hours, function(h){
    # a voir si on somme tout / positifs / negatifs, ...


    x_on_y_All <- vcgClost(vertices[Date%in% date_1 & Period %in% h, ]$out[[1]],
                           vertices[Date%in% date_2 & Period %in% h, ]$mesh[[1]],
                           borderchk = TRUE,
                           sign = TRUE, facenormals = TRUE, barycentric = TRUE)

    x_on_y <- x_on_y_All$quality

    #Si on veux filtrer les points dans les domaines
    # PTDFTp <- PTDF[Date == date_2 & Period == h]
    # x_on_y[which(apply(cbind(vertices[Date%in% date_1 & Period %in% h, ]$out[[1]], -rowSums(vertices[Date%in% date_1 & Period %in% h, ]$out[[1]])), 1, function(X){
    #   all(t(X)%*%
    #          t(as.matrix(PTDFTp[,.SD, .SDcols = c("BE", "DE", "FR", "NL")]))<= PTDFTp$RAM_0)}))] <- 0

    y_on_x_All <- vcgClost(vertices[Date%in% date_2 & Period %in% h, ]$out[[1]],
                           vertices[Date%in% date_1 & Period %in% h, ]$mesh[[1]], borderchk = TRUE, sign = TRUE)


    y_on_x <- y_on_x_All$quality
    #Si on veux filtrer les points dans les domaines
    # PTDFTp <- PTDF[Date == date_1 & Period == h]
    # y_on_x[which(apply(cbind(vertices[Date%in% date_2 & Period %in% h, ]$out[[1]], -rowSums(vertices[Date%in% date_2 & Period %in% h, ]$out[[1]])), 1, function(X){
    #   all(t(X)%*%
    #         t(as.matrix(PTDFTp[,.SD, .SDcols = c("BE", "DE", "FR", "NL")]))<= PTDFTp$RAM_0)}))] <- 0

    d <- mean(x_on_y^2) + mean(y_on_x^2)
    d
    data.table(Date1 = c(date_1, date_2), Date2 = c(date_2, date_1), Period = h, dist = d)
  }))
})))

head(res_dist_hour, n = 10)

# verification

plot(res_ampl$V4, res_dist_hour$dist)
lines(x= c(0,20000000), y = c(0, 20000000))



res_dist_day <- res_dist_hour[,sum(dist), by = c("Date1", "Date2")]
ditM <- dcast(res_dist_day, Date1~Date2,  value.var= "V1")
ditM <- ditM[,.SD, .SDcols = 2:ncol(ditM)]
diag(ditM) <- 0
ditM <- as.dist(ditM)
plot(hclust(ditM))
