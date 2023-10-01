LeavesofGrass <- function(input_file,
              overwrite,
              center,
              scatterLim,
              rf_model,
              correct.topography,
              voxRes,
              minVoxDensity,
              superDF,
              clean){
  
  label = str_split(basename(i), "_", simplify = T  )
  label = paste(label[1], label[2], label[3], "upper", sep = "_" )
  
  angle.file.name<-gsub("_NORM.xyz","_angles.asc", input_file)
  c2c.file<-angle.file.name
  class.file.name<-gsub("_NORM.xyz","_angles_class.asc", input_file)
  gc()

  
  scatterLim = 85
  cols_num = 6
  
  
  if((!file.exists(angle.file.name)|
      overwrite)){

    dat<-readTLSnorms(input_file)



    center = data.frame(X= (abs(max(dat$X))+abs(min(dat$X)))/2,
                        Y = (abs(max(dat$Y))+abs(min(dat$Y)))/2,
                        Z = max(dat$Z))



    dat.angle<-angleCalc(dat,
                         center,
                         scatterLim,
                         cols_num)


    fwrite(dat.angle, file = angle.file.name,
           sep = " ", row.names = FALSE)

    remove(dat,dat.angle)
    gc()

  }
  
  print("Done")

  while(!file.exists(angle.file.name)) Sys.sleep(10)

  class.file = fread(angle.file.name)
  colnames(class.file)[5]<- "dip_deg"

  class.file = class.file[, c(1,2,3,5,4)]

  data.table::fwrite(class.file, file = class.file.name, sep = " ", row.names = FALSE)

  while(!file.exists(class.file.name)) Sys.sleep(5)

  remove(class.file)

  status<-TRUE

print("Voxelizing and calculating LAD...")
if(file.exists(class.file.name)&status){

  voxel_beta_fit(class.file.name,
                 voxRes,
                 minVoxDensity,
                 correct.topography)
}
print("Done")

  print("Calculating G-function...")
  if(file.exists(class.file.name)&
     file.exists(gsub(".asc", "_LAD.txt",class.file.name))&
     file.exists(voxels=gsub(".asc", "_voxels.asc",class.file.name))){
    G_calculations(dat=class.file.name,
                   LAD=gsub(".asc", "_LAD.txt",class.file.name),
                   voxels=gsub(".asc", "_voxels.asc",class.file.name))
  }

  if (superDF){

    dat.class<-LAD<-voxels<-G<-m<-beta<-NULL

    dat.class<-fread(class.file.name)
    LAD<-fread(gsub(".asc", "_LAD.txt",class.file.name))
    voxels<-fread(gsub(".asc", "_voxels.asc",class.file.name))
    G<-fread(gsub("angles_class_voxels.asc", "G_function.txt",gsub(".asc", "_voxels.asc",class.file.name)))

    # get_beta<-function(x){
    m<-NULL
    print("Fit Beta distribution to LAD...")
    # if (nrow(LAD_lim)>1){
    m <- fitdistrplus::fitdist(as.numeric(LAD$a)/90, 'beta', method='mle')

    # Get alpha and beta parametrs
    alpha0 <- m$estimate[1] # parameter 1
    beta0 <- m$estimate[2] # parameter 2
    beta<-data.frame(a= seq(0.01,0.98, 0.01),y=dbeta(seq(0.01,0.98, 0.01),
                                                     alpha0,beta0))
    param<-data.frame(alpha0,beta0)

    print("Finalizing TLSLeAF object...")

    TLSLeAF.dat<-new("TLSLeAF",
                     parameters=data.frame(c(file=input_file,
                                             center,
                                             scatterLim=85,
                                             SS=0.02,
                                             scales=c(0.1,0.5,0.75),
                                             voxRes=voxRes,
                                             superDF=TRUE)),
                     dat=as.data.frame(dat.class),
                     voxels=as.data.frame(voxels),
                     LAD=LAD,
                     Beta_parameters=param,
                     beta=beta,
                     G=G)



    # remove(dat)
    remove(dat.class)
    remove(voxels)
    remove(LAD)
    gc()
    return(TLSLeAF.dat)
  }

  if(clean){
    clean.temp(input_file,c2c.file, clean=TRUE)
  }


}

normalCalc <- function(input_file){
  
  term<-run(paste(cloudcompare, # call Cloud Compare. The .exe file folder must be in the system PATH
                  "-SILENT",
                  "-C_EXPORT_FMT", "ASC", "-PREC", 8, "-EXT", ".xyz", #Set asc as export format
                  "-NO_TIMESTAMP",
                  "-AUTO_SAVE OFF",
                  "-O", input_file, #open the subsampled file
                  "-OCTREE_NORMALS", 0.02, "-MODEL LS",
                  "-SAVE_CLOUDS","FILE", gsub(".xyz","_NORM.xyz", input_file),
                  "-CLEAR",
                  sep = " "))
  
  print("waiting for normal files...")
  while(!file.exists(gsub(".xyz","_NORM.xyz", input_file))) Sys.sleep(5)
  
}


readTLSnorms<-function(input_file){
  
  dat<-vroom(input_file,
             col_names = c("X","Y","Z", "class", "nX","nY","nZ"),
             # col_types= cols(),
             col_types= c(X='d',Y='d',Z='d', class='i',
                          nX='d',nY='d',nZ='d'),
             progress=FALSE, skip = 1)
  
  dat$class = as.integer(0)
  
  return(dat)
  
}

angleCalc<-function(dat, center, scatterLim=85, cols_num){
  
  gc()
  
  #calculate the radius, zenith, and azimuth angles from XYZ coordinatesdat$inc<-RZA(dat[,1:3])
  
  # estimate scattering angle and filter, removing steep angles
  dat$scatter<-scatter(dat, cols_num)
  
  # fwrite(dat, gsub(".asc","_scatter.asc",angle.file.name))
  
  dat<-dat[dat$scatter<=scatterLim,]
  dat<-na.omit(dat)
  
  #Convert normals to leaf orientation and leaf angle
  dat.out<-cbind(dat[,1:4],
                 dip=ConvertNormalToDipAndDipDir(dat[,c('nX','nY','nZ')])[,2])
  
  gc()
  
  dat.out<-dat.out[!is.nan(dat.out$dip),]
  return(dat.out)
  remove(dat, dat.out)
  
}

RZA<-function (dat, deg = FALSE){
  r<-inc<-NULL
  
  colnames(dat)[1:3] <- c("X","Y","Z")
  r <- sqrt(dat$X^2 + dat$Y^2 +dat$Z^2)
  
  inc<-matrix(NA, nrow=length(r))
  inc <- ((acos(dat$Z/r)))
  remove(r)
  gc()
  
  # az <- atan2(scan$y,scan$x)
  
  if(deg==TRUE){
    inc<-inc*(180/pi)
    return(inc)
    # scan$az<-scan$az*(180/pi)
  }
  return(inc)
  
}

scatter<-function(dat, cols_num){
  my.dt<-NULL
  # time<-Sys.time()
  # dat<-cbind(dat, )
  dat.add<-dat[,1:3]/sqrt(rowSums(dat[,1:3]^2))
  colnames(dat.add)<-c("sX","sY","sZ")
  dat<-cbind(dat,dat.add)
  remove(dat.add)
  
  # dat$dot<-geometry::dot(dat[,cols_num+1:3],dat[,c('nX','nY','nZ')],d=2)
  dat$dot<-geometry::dot(dat[,c('nX','nY','nZ')],dat[,c('nX','nY','nZ')],d=2)
  my.dt <- as.data.table(cbind(abs(dat$dot),1))
  dat$dot<-my.dt[,row.min:=pmin(V1,V2)]$row.min
  return(acos(dat$dot) * (180/pi))
  remove(my.dt)
  # print(Sys.time()-time)#
}

ConvertNormalToDipAndDipDir<-function(x) {
  
  Nsign<-dipDir_rad<-dip_rad<-dip_deg<-NULL
  
  colnames(x) <- c("nX","nY","nZ")
  
  # The formula using atan2() with the swapped N.x and N.y already
  # gives the correct results for facets with the normal pointing
  # upwards, so just use the sign of N.z to invert the normals if they
  # point downwards.
  
  Nsign <- as.numeric(rep(1,nrow(x)))
  Nsign[x$nZ < 0] <- -1.0
  
  dipDir_rad <- atan2(Nsign*as.numeric(x$nX), Nsign*as.numeric(x$nY))
  
  # Dip direction is measured in 360 degrees, generally clockwise from North
  dipDir_rad[dipDir_rad < 0] <- dipDir_rad[dipDir_rad < 0] + 2*pi
  
  # acos() returns values in [0, pi] but using abs() all the normals
  # are considered pointing upwards, so the actual result will be in
  # [0, pi/2] as required by the definition of dip.
  # We skip the division by r because the normal is a unit vector.
  dip_rad <- acos(abs(x$nZ))
  
  dipDir_deg = dipDir_rad * (180/pi)
  dip_deg = dip_rad * (180/pi)
  
  return(data.frame(dipDir_deg,
                    dip_deg)
  )
  
}

voxel_beta_fit<-function(class.file.name, 
                         voxRes=0.1, 
                         minVoxDensity=5, 
                         correct.topography=TRUE){
  dat.class.in<-voxels<-LAD<-LAD_lim<-m<-beta<-NULL
  
 
  gc()
  # dat.class.in<-vroom::vroom(class.file.name, delim = "\t",
  #                            col_names = c("X","Y","Z", "dip_deg","class"),
  #                            col_types= c(X='d',Y='d',Z='d',dip_deg='d',class='i'),
  #                            progress=FALSE, skip=1,
  #                            altrep = FALSE)
  
  dat.class.in<-vroom::vroom(class.file.name, delim = " ", 
                             col_names = c("X","Y","Z", "dip_deg","class"), 
                             col_types= c(X='d',Y='d',Z='d',dip_deg='d',class='i'),
                             progress=FALSE, 
                             altrep = FALSE)

  
  # if(file.exists(class.file.name)) dat.class<-vroom(class.file.name)
  dat.class<-NULL
  print("Correct topography...")
  #Correct topography and calculate the LAD and vertical LAD
  if(correct.topography) dat.class<-normalize_topography(dat.class.in) else dat.class<-dat.class.in
  print("Done")
  # fwrite(dat.class,  file = gsub(".asc", "_topo_correct.asc",class.file.name),
  #        sep = " ", row.names = FALSE)
  
  
  print("Voxelize angle estimates...")
  # leaf angle voxelation and density normalization
  
  if(correct.topography) dat.class.ag<- dat.class[dat.class$Z>1,] else dat.class.ag<-dat.class
  
  voxels<-LAvoxel(na.omit(dat.class.ag), voxRes)
  
  voxels<-voxels[voxels$n>quantile(voxels$n,0.01),]
  voxels<-voxels[voxels$n>minVoxDensity,]
  
  print(str(voxels))
  
  print("Done")
  
  fwrite(voxels,  file = gsub(".asc", "_voxels.asc",class.file.name),
         sep = " ", row.names = FALSE)
  
  #simulate LAD from voxel statistics
  print("Simulate LAD from voxel statistics...")
  
  if(nrow(voxels)>0){
    LAD<-sim_LAD(voxels)
    LAD<-na.exclude(LAD[LAD$a>=0 & LAD$a<=90,])
    fwrite(LAD,  file = gsub(".asc", "_LAD.txt",class.file.name),
           sep = " ", row.names = FALSE)
    
    LAD_lim<-data.frame(a=LAD$a)
    
    #fit beta function and get beta parameters from LAD
    #FIT BETA DISTRIBUTION
    
    # get_beta<-function(x){
    m<-NULL
    print("Fit Beta distribution to LAD...")
    # if (nrow(LAD_lim)>1){
    
    
    m <- fitdistrplus::fitdist(as.numeric(LAD_lim$a)/90, 'beta', method='mle')
    
    # Get alpha and beta parametrs
    alpha0 <- m$estimate[1] # parameter 1
    beta0 <- m$estimate[2] # parameter 2
    beta<-data.frame(a= seq(0.01,0.98, 0.01),y=dbeta(seq(0.01,0.98, 0.01),
                                                     alpha0,beta0))
    param<-data.frame(alpha0,beta0)
    print("Done")
    
    fwrite(beta,  file = gsub(".asc", paste("_Beta_distribution_alpha_", 
                                            round(param[1],2),"_beta_",round(param[2],2),
                                            ".txt", sep=""),class.file.name),
           sep = " ", row.names = FALSE)
    
    remove(m)
    gc()
    # } 
  }
  
  # gc()
}

normalize_topography<-function(x, res = 5){
  las<-r<-topo<-topo.df<-ground<-topo.las.r<-r.p<-topo.p<-slope<-poly.id<-NULL
  
  # defaultW <- getOption("warn")
  # options(warn = -1)
  # crs_tls <- sp::CRS("+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +a=6371007 +b=6371007 +units=m +no_defs")
  crs_tls<-sp::CRS("+init=epsg:26918")
  # crs_tls
  colnames(x)[1:3]<-c("X","Y","Z")
  las<-LAS(x[,1:3], proj4string = crs_tls)
  # projection(las)<-crs
  las@data$dip_deg<-x[,4]
  las@data$class<-x[,5]
  
  r <- raster(xmn=-200, xmx=200, ymn=-200, ymx=200, resolution = res)
  topo<-grid_metrics(las, quantile(Z, 0.01), r)
  crs(topo)<-crs_tls
  # plot(topo)
  
  topo.df<-as.data.frame(rasterToPoints(topo))
  colnames(topo.df)<-c("X","Y","Z")
  
  ws <- seq(3,12, 3)
  th <- seq(0.1, 1.5, length.out = length(ws))
  
  topo.las<-LAS(topo.df)
  crs(topo.las)<-crs_tls
  
  ground<-classify_ground(topo.las, pmf(ws, th), last_returns = FALSE)
  topo.las.r<-grid_terrain(ground, r, kriging(k = 10))
  
  # topo<-focal(topo, matrix(1,3,3))
  # topo.smooth<-disaggregate(topo, 5, method='bilinear')
  # # density.r<-grid_density(las, topo.smooth)
  # # plot(topo, col = viridis::viridis(250))
  # plot(topo.las.r)
  
  r.p<-buffer(topo.las.r, width=1)
  r.p[!is.na(r.p)]<-1
  
  topo.las.p<-rasterToPolygons(r.p, dissolve = TRUE)
  # lines(topo.las.p)
  
  las<-clip_polygon(las, topo.las.p@polygons[[1]]@Polygons[[1]]@coords[,1], topo.las.p@polygons[[1]]@Polygons[[1]]@coords[,2])
  
  # plot(las)
  las<- las - topo.las.r
  # plot(las)
  
  slope<-terrain(topo, opt = "slope", unit = "degrees", neighbors = 8)
  # plot(slope)
  
  topo[is.na(slope)]<-NA
  topo.p<-topo
  topo.p[!is.na(topo.p)]<-1
  
  topo.las.p<-rasterToPolygons(topo.p, dissolve = TRUE)
  # lines(topo.las.p)
  
  poly.id<-length(topo.las.p@polygons[[1]]@Polygons)
  
  topo.las.p@polygons[[1]]@Polygons
  
  a.ls<-c(do.call(rbind, lapply(1:poly.id, function(x){
    area.sub<-topo.las.p@polygons[[1]]@Polygons[[x]]
    return(area.sub@area)
  })))
  
  max.poly.id<-(1:poly.id)[max(a.ls)==a.ls]
  
  las<-clip_polygon(las, topo.las.p@polygons[[1]]@Polygons[[max.poly.id]]@coords[,1], topo.las.p@polygons[[1]]@Polygons[[max.poly.id]]@coords[,2])
  # plot(las)
  return(las@data)
  
  # remove(las)
  
}

LAvoxel<-function(x,res = 0.1){
  # defaultW <- getOption("warn")
  # options(warn = -1)
  las<-voxels<-NULL
  colnames(x)[1:3]<-c("X","Y","Z")
  x<-x[x$class==0,]
  crs_tls<-sp::CRS("+init=epsg:26918")
  las<-LAS(x[,1:3],proj4string = crs_tls)
  
  # crs(las)<-"+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +a=6371007 +b=6371007 +units=m +no_defs"
  # las@data$Z<-x$z_cor
  las@data$dip_deg<-x$dip_deg
  las@data$count<-1
  
  voxels<-voxel_metrics(las, ~mean(dip_deg), res=res)
  colnames(voxels)[4]<-"dip_dir"
  voxels$dip_dir_sd<-voxel_metrics(las, ~sd(dip_deg), res=res)[,4]
  voxels$n<-voxel_metrics(las, ~sum(count), res=res)[,4]
  
  # options(warn = defaultW)
  return(voxels)
  
  remove(las)
  remove(voxels)
}

sim_LAD<-function(x){
  voxels.dup<-NULL
  vox_ls<-list()
  for(i in 1:10){
    voxels.dup<-x
    voxels.dup$a<-apply(voxels.dup,1,FUN = function(xx)  rnorm(1, mean=xx[4], sd=xx[5]))
    vox_ls[[i]]<-voxels.dup
    remove(voxels.dup)
  }
  return(do.call(rbind, vox_ls))
  remove(vox_ls)
}

G_calculations<-function(dat,
                         LAD,
                         voxels){
  
  G_ls<-list()
  if(file.exists(dat)){
    # dat<-NULL
    gc()
    dat<-fread(dat)
    dat<-dat[dat$class==0,]
    
    if(nrow(dat)>0){
      dat$inc<-RZA(dat[,1:3], deg = TRUE)
      dat$inc_bin<-floor(dat$inc)
      dat$dip_bin<-floor(dat$dip_deg)
      dat<-dat[dat$inc_bin>=0&dat$inc_bin<=90,]
      
      dat$count<-1
      
      dat.sub<-rbind(data.frame(inc_bin=dat$inc_bin,
                                dip_bin=dat$dip_bin,
                                count=dat$count),
                     data.frame(inc_bin=0:90,
                                dip_bin=0:90,
                                count=0))
      
      G_test<-aggregate(count~dip_bin, FUN="sum",dat.sub)
      G_test$p<-G_test$count/sum(G_test$count)
      # plot(G_test)
      
      ran_G_ls<-list()
      for(i in 1:length(0:90)) ran_G_ls[[i]]<-data.frame(dip_bin=G_test$dip_bin[G_test$dip_bin==c(0:90)[i]],
                                                         count=G_test$count[G_test$dip_bin==c(0:90)[i]],
                                                         p=G_test$p[G_test$dip_bin==c(0:90)[i]],
                                                         inc_bin=0:90)
      
      G_test<-do.call(rbind, ran_G_ls)
      
      G_test$A_test<-abs(
        ( 1/tan(G_test$inc_bin*(pi/180)) )*
          ( 1/tan(G_test$dip_bin*(pi/180)) )
      )
      
      G_test$A1<-
        cos(G_test$inc_bin*(pi/180))*
        cos(G_test$dip_bin*(pi/180))
      
      G_test$phi<-NA
      G_test$phi[G_test$A_test<=1]<-acos(
        (1/tan(G_test$inc_bin[G_test$A_test<=1]*(pi/180)))*
          (1/tan(G_test$dip_bin[G_test$A_test<=1]*(pi/180)))
      )
      
      G_test$A2<-
        cos(G_test$inc_bin*(pi/180))*
        cos(G_test$dip_bin*(pi/180))*
        (1+(2/pi)*(tan(G_test$phi)-G_test$phi))
      
      G_test$A<-G_test$A1
      G_test$A[G_test$A_test<=1]<-G_test$A2[G_test$A_test<=1]
      
      G_test$G_p<-G_test$A*G_test$p
      
      G_calc<-aggregate(G_p~inc_bin, 
                        FUN= "sum", 
                        G_test, na.rm=TRUE)
      
      G_calc$assumption<-"random"
      G_calc$density<-"non-normalized"
      
      # plot(G_calc$inc_bin,
      #      G_calc$G_p)
      
      G_ls[[1]]<-G_calc
      
      G_test<-aggregate(count~inc_bin+dip_bin, FUN="sum",dat.sub)
      p<-aggregate(count~inc_bin, FUN="sum",
                   G_test)
      
      G_test<-merge(G_test,p, by="inc_bin")
      
      G_test$p<-G_test$count.x/G_test$count.y
      
      G_test$A_test<-abs(
        ( 1/tan(G_test$inc_bin*(pi/180)) )*
          ( 1/tan(G_test$dip_bin*(pi/180)) )
      )
      
      G_test$A1<-
        cos(G_test$inc_bin*(pi/180))*
        cos(G_test$dip_bin*(pi/180))
      
      G_test$phi<-NA
      G_test$phi[G_test$A_test<=1]<-acos(
        (1/tan(G_test$inc_bin[G_test$A_test<=1]*(pi/180)))*
          (1/tan(G_test$dip_bin[G_test$A_test<=1]*(pi/180)))
      )
      
      G_test$A2<-
        cos(G_test$inc_bin*(pi/180))*
        cos(G_test$dip_bin*(pi/180))*
        (1+(2/pi)*(tan(G_test$phi)-G_test$phi))
      
      G_test$A<-G_test$A1
      G_test$A[G_test$A_test<=1]<-G_test$A2[G_test$A_test<=1]
      
      G_test$G_p<-G_test$A*G_test$p
      
      G_calc<-aggregate(G_p~inc_bin, 
                        FUN= "sum", 
                        G_test, na.rm=TRUE)
      
      G_calc$assumption<-"non-random"
      G_calc$density<-"non-normalized"
      
      G_ls[[2]]<-G_calc
    } 
  }
  
  if(file.exists(voxels)){
    dat<-fread(voxels)
    if(nrow(dat)>0){
      print(summary(dat))
      dat$inc<-RZA(dat[,1:3], deg = TRUE)
      dat$inc_bin<-floor(dat$inc)
      dat$dip_bin<-floor(dat$dip_dir)
      dat$count<-1
      
      dat.sub<-rbind(data.frame(inc_bin=dat$inc_bin,
                                dip_bin=dat$dip_bin,
                                count=dat$count),
                     data.frame(inc_bin=0:90,
                                dip_bin=0:90,
                                count=0))
      
      G_test<-aggregate(count~dip_bin, FUN="sum",dat.sub)
      G_test$p<-G_test$count/sum(G_test$count)
      # plot(G_test)
      
      ran_G_ls<-list()
      for(i in 1:length(0:90)) ran_G_ls[[i]]<-data.frame(dip_bin=G_test$dip_bin[G_test$dip_bin==c(0:90)[i]],
                                                         count=G_test$count[G_test$dip_bin==c(0:90)[i]],
                                                         p=G_test$p[G_test$dip_bin==c(0:90)[i]],
                                                         inc_bin=0:90)
      G_test<-do.call(rbind, ran_G_ls)
      
      G_test$A_test<-abs(
        ( 1/tan(G_test$inc_bin*(pi/180)) )*
          ( 1/tan(G_test$dip_bin*(pi/180)) )
      )
      
      G_test$A1<-
        cos(G_test$inc_bin*(pi/180))*
        cos(G_test$dip_bin*(pi/180))
      
      G_test$phi<-NA
      G_test$phi[G_test$A_test<=1]<-acos(
        (1/tan(G_test$inc_bin[G_test$A_test<=1]*(pi/180)))*
          (1/tan(G_test$dip_bin[G_test$A_test<=1]*(pi/180)))
      )
      
      G_test$A2<-
        cos(G_test$inc_bin*(pi/180))*
        cos(G_test$dip_bin*(pi/180))*
        (1+(2/pi)*(tan(G_test$phi)-G_test$phi))
      
      G_test$A<-G_test$A1
      G_test$A[G_test$A_test<=1]<-G_test$A2[G_test$A_test<=1]
      
      G_test$G_p<-G_test$A*G_test$p
      
      G_calc<-aggregate(G_p~inc_bin, 
                        FUN= "sum", 
                        G_test, na.rm=TRUE)
      
      G_calc$assumption<-"random"
      G_calc$density<-"normalized"
      
      G_ls[[3]]<-G_calc
      
      G_test<-aggregate(count~inc_bin+dip_bin, FUN="sum",dat.sub)
      p<-aggregate(count~inc_bin, FUN="sum",
                   G_test)
      
      G_test<-merge(G_test,p, by="inc_bin")
      
      G_test$p<-G_test$count.x/G_test$count.y
      
      G_test$A_test<-abs(
        ( 1/tan(G_test$inc_bin*(pi/180)) )*
          ( 1/tan(G_test$dip_bin*(pi/180)) )
      )
      
      G_test$A1<-
        cos(G_test$inc_bin*(pi/180))*
        cos(G_test$dip_bin*(pi/180))
      
      G_test$phi<-NA
      G_test$phi[G_test$A_test<=1]<-acos(
        (1/tan(G_test$inc_bin[G_test$A_test<=1]*(pi/180)))*
          (1/tan(G_test$dip_bin[G_test$A_test<=1]*(pi/180)))
      )
      
      G_test$A2<-
        cos(G_test$inc_bin*(pi/180))*
        cos(G_test$dip_bin*(pi/180))*
        (1+(2/pi)*(tan(G_test$phi)-G_test$phi))
      
      G_test$A<-G_test$A1
      G_test$A[G_test$A_test<=1]<-G_test$A2[G_test$A_test<=1]
      
      G_test$G_p<-G_test$A*G_test$p
      
      G_calc<-aggregate(G_p~inc_bin, 
                        FUN= "sum", 
                        G_test, na.rm=TRUE)
      
      G_calc$assumption<-"non-random"
      G_calc$density<-"normalized"
      
      G_ls[[4]]<-G_calc
    }
  }
  
  fwrite(do.call(rbind, G_ls),
         file = gsub("angles_class_voxels.asc", "G_function.txt",voxels),
         sep= " ")
  dat<-NULL
  G_ls<-NULL
  gc()
  # return(do.call(rbind, G_ls))
}

TLSLeAF.class<-setClass("TLSLeAF",representation=representation(
  parameters = "data.frame",
  dat = "data.frame",
  voxels="data.frame",
  LAD = 'data.frame',
  Beta_parameters = "data.frame",
  beta="data.frame",
  G="data.frame"
))

clean.temp<-function(output_file, c2c.file, clean=TRUE){
  temp.files<-c(output_file, gsub(".asc","_angles.asc",output_file),
                gsub(".asc","_0_005_NORM.asc",c2c.file),
                gsub(".asc","_0_02_NORM.asc",c2c.file),
                gsub(".asc","_0_05_NORM.asc",c2c.file))[file.exists(c(output_file, gsub(".asc","_angles.asc",output_file),
                                                                      gsub(".asc","_0_005_NORM.asc",c2c.file),
                                                                      gsub(".asc","_0_02_NORM.asc",c2c.file),
                                                                      gsub(".asc","_0_05_NORM.asc",c2c.file)))]
  
  if(clean & length(temp.files)>0) file.remove(temp.files)
  
  gc()
}