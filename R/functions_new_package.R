######### Les packages nécessaires à charger #######
######### Les fonctions nécessaires : #########

####### Fonction récupération des données : getCnesFb #####

getCnesFb <- function(file, nbcl, sizechunk = 10)
{

  file <- split(file, ceiling(seq_along(file)/(sizechunk)))

  oud <- rbindlist(lapply(file, function(ffl){


    cl <- makeCluster(nbcl)
    end <- rbindlist(parSapply(cl, ffl, function(fl){
      library(XML)
      library(data.table)

      PLAN <- xmlParse(fl)


      ns <- c("a" = xmlNamespaceDefinitions(PLAN, simplify = TRUE))

      ##Time
      outT <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:TimeInterval', ns,
                         addFinalizer = FALSE)
      out <- xmlSApply(outT, xmlAttrs)
      tim <- fbTools:::.getTime(out)
      rm(outT)

      out <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult', ns,
                        addFinalizer = FALSE)
      endAll <- rbindlist(lapply(out, function(DD){

        ot <- xmlElementsByTagName(DD, "ptdfs")$ptdfs
        if(is.null(ot))return(data.table(DDDDD = 0))
        oot <- xmlElementsByTagName(ot, "ptdf")
        PT <- unlist(lapply(oot, function(D)
        {
          ct <- xmlElementsByTagName(D, "hub")$hub
          xmlAttrs(ct)
        }))
        vl <- as.numeric(xmlSApply(ot, xmlValue))
        names(vl) <- paste0("ptdf", PT)
        data.table(t(vl))

      }), use.names = TRUE ,fill = TRUE )
      rm(out)

      if('DDDDD'%in%names(endAll))endAll$DDDDD <- NULL
      RAM2 <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult/a:criticalBranch/a:ram', ns,
                         addFinalizer = FALSE)
      RAM <- as.numeric(xmlSApply(RAM2, xmlValue))
      endAll[, ram := RAM]
      rm(RAM2)

      FMAX2 <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult/a:criticalBranch/a:fMax', ns,
                          addFinalizer = FALSE)
      FMAX <- as.numeric(xmlSApply(FMAX2, xmlValue))
      endAll[, fMax := FMAX]
      rm(FMAX2)


      fref2 <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult/a:criticalBranch/a:fRef', ns,
                          addFinalizer = FALSE)
      fref <- as.numeric(xmlSApply(fref2, xmlValue))
      endAll[, fref := fref]
      rm(fref2)


      amr <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult/a:criticalBranch/a:amr', ns,
                        addFinalizer = FALSE)
      amr <- as.numeric(xmlSApply(amr, xmlValue))
      endAll[, amr := amr]

      ltaMargin <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult/a:criticalBranch/a:ltaMargin', ns,
                              addFinalizer = FALSE)
      ltaMargin <- as.numeric(xmlSApply(ltaMargin, xmlValue))
      endAll[, ltaMargin := ltaMargin]

      frm <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult/a:criticalBranch/a:frmMw', ns,
                        addFinalizer = FALSE)
      frm <- as.numeric(xmlSApply(frm, xmlValue))
      endAll[, frm := frm]

      fav <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult/a:criticalBranch/a:fav', ns,
                        addFinalizer = FALSE)
      fav <- as.numeric(xmlSApply(fav, xmlValue))
      endAll[, fav := fav]


      tsoOrigin <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult/a:criticalBranch/a:tsoOrigin', ns,
                              addFinalizer = FALSE)
      tsoOrigin <- xmlSApply(tsoOrigin, xmlValue)
      endAll[, tsoOrigin := tsoOrigin]


      lim2 <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult/a:domainLimit/a:region', ns,
                         addFinalizer = FALSE)
      lim <- xmlSApply(lim2, xmlValue)
      lim <- ifelse(lim == "true", TRUE, FALSE)
      endAll[, presolve := lim]
      rm(lim2)

      branch2 <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult', ns,
                            addFinalizer = FALSE)
      branch <- xmlSApply(branch2, xmlAttrs)
      rm(branch2)
      branch <- t(branch)
      endAll <- cbind(endAll, data.table(branch))
      endAll <- endAll[!c(is.na(endAll[, .SD, .SDcols = 1]))]
      endAll[, timestamp := tim]
      free(PLAN)
      endAll
    }, simplify = FALSE))
    stopCluster(cl)
    end
  }))


}


### On doit uniquement garder les lignes avec presolve = T,
## voir pourquoi



############### getVertices qui permet de récupérer les sommets###############
##### d'un polyèdre à partir des équations de face




getVertices <- function(face, b)
{
  B <- as.matrix(face)
  IDfin <- 1:nrow(B)
  Un <- rep(1, 4)
  grid <- combn(1:nrow(B), 3)
  d <- b+1e-6
  res <- apply(grid, 2, function(gridRaw){
    # For each ijk raw of face, and b, resolve   qr.solve(Bijk, bijk)
    Bijk<- rbind(B[gridRaw[1], ], B[gridRaw[1], ], B[gridRaw[2], ], B[gridRaw[3], ], Un)
    bijk <- c(b[gridRaw[1]], b[gridRaw[1]], b[gridRaw[2]], b[gridRaw[3]], 0)
    x <- try({
      qr.solve(Bijk, bijk)
    }, silent = TRUE)

    if("try-error" %in% class(x)){
      return(NULL)
    }
    # filtering x who not respect all constaints
    if(all(B%*%x<=d)){
      return(x)
    }

  }) %>>%
    unlist %>>%
    matrix(ncol = 4, byrow = TRUE)
  # filtering point which are duplicated
  res <- res[round(rowSums(res), 2) == 0,]
  DD <- dist(res, method = "euclidean", p = 2, upper = FALSE)
  DD <- as.matrix(DD)
  DD[lower.tri(DD, diag = TRUE)] <- 1
  res <- res[which(apply(DD, 2, min)>1e-6),1:3]
  res
}

########### setDiffLastPtdf permet de soustraire une colonne aux autres ########
#### Si aucun ptdf n'est enregistré comem non voulu
### c'est le ptdf en dernière position dans les noms de colonne qui est choisi
## not_wanted_col doit être du type ptdfXX (ex ptdfNL, ptdfFR)

setDiffNotWantedPtdf <- function(PLAN, not_wanted_col = NULL)
{
  col_ptdf <- colnames(PLAN)[grep("ptdf", colnames(PLAN))]
  if (is.null(not_wanted_col)) {
    not_wanted_col <-  col_ptdf[length(col_ptdf)]
  }
  col_ptdf <- col_ptdf[-which(col_ptdf == not_wanted_col)]

  PLAN[, c(col_ptdf) := lapply(col_ptdf, function(ptdf) {
           PLAN[[ptdf]] - PLAN[[not_wanted_col]]
         })]
  PLAN[[not_wanted_col]] <- NULL
  return(PLAN)
}

############### transformTS ###############
#### Permet de jouer sur les dates et passer à l'heure française

.transformTS <- function(dt)
{
  dt$Period <- hour(dt$timestamp) + 1
  dt$Date <- as.character(as.Date(dt$timestamp))
  dt$timestamp <- NULL
  dt
}


######### COmpute the Ax0 matrix ####
##### matrix needed for the projection & check if interior point #####
##### The function also return if a point is interior or external


# .computeAx0_checkIntExt <- function(VERT, PLAN) {
#   Ax0_list <- lapply(1:nrow(VERT), function(X) {
#     V1 <- VERT[X]
#     Ax0 <- (as.matrix(
#       PLAN[, .SD, .SDcols = col_ptdf])%*%t(as.matrix(V1[, .SD, .SDcols = col_ptdf])))
#     if (sum(Ax0 > PLAN[, ram])) {
#       VERT[X, INTERIOR := F]
#       NA
#     } else {
#       Ax0
#     }
#   })
#   Ax0_list <- Ax0_list[!is.na(Ax0_list)]
# }

.flagExtOrInt <- function(Ax0, b) {
  flag <- (sum(Ax0 > b) > 0)
  # vec <- (Ax0 > b)
  # flag <- any(vec)
  # flag <- any((Ax0 > b))
}

######### Compute the distance if external point ####
.getDistExt <- function(V1, PL , ram, Dmat)
{
  # sapply(1:nrow(VERT), function(X){
    # V1 <- VERT[X]
    ## dvec correspond aux coordonnées du sommet d'étude *2
    ## Dmat est créé dans la fonction dEnd, c'est une matrice diagonale,
    ## de diagonale = 2 et de taille le nombre de coordonnées du sommet
    ## (donc la dimension dans laquelle il est défini)
    ## Amat prend l'opposé de l'ensemble des coordonnées des hyperplans
    ## (une colonne = un hyperplan), sans prendre en compte la dernière dimension
    ## (ici NL)
    ## bvec permet de constuire la contrainte, il prend l'opposé de la variable
    ## ram qui correspond soit à la valeur max de transfert, soit au nb de lignes,
    ## à vérifier..
    ## PL2 <- -t(PL)
    re <- solve.QP(Dmat = Dmat,
                   dvec = V1,
                   Amat = PL,
                   bvec = -ram,meq = 0)
  # settings <- osqpSettings(verbose = FALSE)
  #    re = solve_osqp(P = Dmat, q = V1, A = t(PL), u = -ram, pars = settings)$x


    ## Distance euclidienne entre solution et point en entrée
    # print(re)
    val <- sqrt(sum((abs(re$solution-re$unconstrained.solution))^2))
    # val <- sqrt(sum((abs(V1-re))^2))
    # print(val)
    val
  # })
}
######### Compute the distance if interior point ####


.getDistInt <- function(V1, PL, ram, Ax0)
{

  norm <- rowSums(PL^2)
  val <- min(sqrt((abs(ram^2 + Ax0^2 - 2*ram*Ax0)/norm)))
  # print(val)
  val
}


######### dEnd : Récupération du calcul de distance entre polyèdres #####

dEnd <- function(VERT, PLAN, col_ptdf)
{
  Dmat <- diag(1, nrow = dim(VERT[, .SD, .SDcols = col_ptdf])[2])
  PL <- as.matrix(
    PLAN[, .SD, .SDcols = col_ptdf])
  TPL <- -t(PL)
  ram <- PLAN[, ram]
  VERT <- as.matrix(VERT[, .SD, .SDcols = col_ptdf])
  mean(sapply(1:nrow(VERT), function(X){
    # print(X)
    V1 <- matrix(VERT[X,])
    Ax0 <- PL%*%V1
    flag <- .flagExtOrInt(Ax0, ram)
    if (flag) {
      # print("external")
      # saveRDS(list(V1, flag, Ax0, PL, X, ram), "object.rds")
      .getDistExt(V1 = V1, PL = TPL, ram = ram, Dmat = Dmat)
    } else {
      # print("interior")
      .getDistInt(V1 = V1, PL = PL, ram = ram, Ax0 = Ax0)
    }
  }))
  # Y_X <- get_dist_poly( VERT, PLAN, col_ptdf = col_ptdf)
  # X_Y + Y_X
}

######### Création data.table avec toutes les distances résultantes #####


.getDistMatrixV2 <- function(
  VERT, PLAN, hourWeight){
  set.seed(1234)
  PLAN[, ram := ram + runif(nrow(PLAN))/10000]
  col_ptdf <- colnames(PLAN)[grep("ptdf", colnames(PLAN))]
  res_hour <- data.table(t(combn(unique(VERT[, Date]), 2)))

  rbindlist(sapply(1:nrow(res_hour), function(comb){

    ##To sapply
    date_1 <- res_hour[comb, V1]
    date_2 <- res_hour[comb, V2]
    v_hours <- intersect(VERT[Date%in% date_1, Period], VERT[Date%in% date_2, Period])

    ##To sapply
    # h <- v_hours[1]
    rbindlist(sapply(v_hours, function(h){
      print(h)
      # print(paste("begin", date_1, date_2, Sys.time()))

      DD <- dEnd(VERT[Date == date_1 & Period == h],
                 PLAN[Date == date_2 & Period == h], col_ptdf = col_ptdf)

      # print(paste("end", date_1, date_2, Sys.time()))
      # print(paste("begin", date_2, date_1, Sys.time()))

      DD2 <- dEnd(VERT[Date == date_2 & Period == h],
                  PLAN[Date == date_1 & Period == h], col_ptdf = col_ptdf)

      # print(paste("end", date_2, date_1, Sys.time()))
      d <- DD + DD2
      weigthPond <- hourWeight[h]
      d <- weigthPond * d
      data.table(Date1 = c(date_1, date_2),
                 Date2 = c(date_2, date_1), Period = h, dist = d)
    }, simplify = FALSE))

  }, simplify = FALSE))
}
