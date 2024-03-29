## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 999)
knitr::opts_chunk$set(echo = TRUE, fig.width=6, fig.height=4, out.width = 6, out.heigth = 4)


## ----load, eval=TRUE, echo=FALSE, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
load("spatial_vignette.RData")

## ---- eval = FALSE,echo=FALSE, message=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  library(sp)
#  # locations (155 observed points)
#  data("meuse")
#  # grid of points (3103)
#  data("meuse.grid")
#  meuse.grid$id <- c(1:nrow(meuse.grid))
#  coordinates(meuse)<-c("x","y")
#  coordinates(meuse.grid)<-c("x","y")

## ---- eval = FALSE,echo=FALSE,include=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  par(mfrow=c(1,2))
#  plot(meuse.grid)
#  title("Meuse territory (3103 points)",sub="with reported distance from the river",cex.main=0.8,cex.sub=0.8)
#  plot(meuse)
#  title("Subset of 155 points",sub="with also observed metals concentration",cex.main=0.8,cex.sub=0.8)
#  par(mfrow=c(1,1))

## ---- eval = F,echo=TRUE,message=FALSE,fig.width=6,fig.height=8----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  library(automap)
#  kriging_lead = autoKrige(log(lead) ~ dist, meuse, meuse.grid)
#  plot(kriging_lead,sp.layout = NULL, justPosition = TRUE)

## ---- eval = F,echo=TRUE,message=FALSE,fig.width=6,fig.height=8----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  kriging_zinc = autoKrige(log(zinc) ~ dist, meuse, meuse.grid)
#  plot(kriging_zinc, sp.layout = list(pts = list("sp.points", meuse)))

## ---- eval = F,echo=TRUE,message=FALSE,fig.width=6,fig.height=8----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  r2_lead <- 1 - kriging_lead$sserr/sum((log(meuse$lead)-mean(log(meuse$lead)))^2)
#  r2_lead
#  ## [1] 0.9999997
#  r2_zinc <- 1 - kriging_zinc$sserr/sum((log(meuse$zinc)-mean(log(meuse$zinc)))^2)
#  r2_zinc
#  ## [1] 0.9999999

## ---- eval = F,echo=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  df <- NULL
#  df$id <- meuse.grid$id
#  df$lead.pred <- kriging_lead$krige_output@data$var1.pred
#  df$lead.var <- kriging_lead$krige_output@data$var1.var
#  df$zinc.pred <- kriging_zinc$krige_output@data$var1.pred
#  df$zinc.var <- kriging_zinc$krige_output@data$var1.var
#  df$lon <- meuse.grid$x
#  df$lat <- meuse.grid$y
#  df$dom1 <- 1
#  df <- as.data.frame(df)
#  head(df)
#  ##   id lead.pred  lead.var zinc.pred  zinc.var    lon    lat dom1
#  ## 1  1  5.509360 0.1954937  6.736502 0.2007150 181180 333740    1
#  ## 2  2  5.546006 0.1716895  6.785460 0.1749260 181140 333700    1
#  ## 3  3  5.488913 0.1784052  6.698883 0.1826314 181180 333700    1
#  ## 4  4  5.388320 0.1855561  6.558216 0.1906426 181220 333700    1
#  ## 5  5  5.584415 0.1463018  6.841612 0.1465346 181100 333660    1
#  ## 6  6  5.525538 0.1533757  6.749216 0.1549663 181140 333660    1

## ---- eval = F,echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  library(SamplingStrata)
#  frame <- buildFrameSpatial(df=df,
#                        id="id",
#                        X=c("lead.pred","zinc.pred"),
#                        Y=c("lead.pred","zinc.pred"),
#                        variance=c("lead.var","zinc.var"),
#                        lon="lon",
#                        lat="lat",
#                        domainvalue = "dom1")
#  cv <- as.data.frame(list(DOM=rep("DOM1",1),
#                           CV1=rep(0.01,1),
#                           CV2=rep(0.01,1),
#                           domainvalue=c(1:1) ))
#  cv
#  #    DOM  CV1  CV2 domainvalue
#  # 1 DOM1 0.01 0.01           1

## ---- eval = F,echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  set.seed(1234)
#  solution <- optimStrata (
#    method = "spatial",
#    errors=cv,
#    framesamp=frame,
#    iter = 15,
#    pops = 10,
#    nStrata = 5,
#    fitting = c(r2_lead,r2_zinc),
#    range = c(kriging_lead$var_model$range[2],kriging_zinc$var_model$range[2]),
#    kappa=1,
#    writeFiles = FALSE,
#    showPlot = FALSE,
#    parallel = FALSE)
#  framenew <- solution$framenew
#  outstrata <- solution$aggr_strata
#  ##
#  ## Input data have been checked and are compliant with requirements
#  ## Sequential optimization as parallel = FALSE, defaulting number of cores = 1
#  ##  *** Domain :  1   1
#  ##  Number of strata :  3103
#  ##  *** Sample cost:  61.92901
#  ##  *** Number of strata:  4

## ---- eval = F,echo=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  plotStrata2d(framenew,outstrata,domain=1,vars=c("X1","X2"),
#               labels=c("Lead","Zinc"))

## ---- eval = F,echo=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  frameres <- SpatialPointsDataFrame(data=framenew, coords=cbind(framenew$LON,framenew$LAT) )
#  frameres2 <- SpatialPixelsDataFrame(points=frameres[c("LON","LAT")], data=framenew)
#  frameres2$LABEL <- as.factor(frameres2$LABEL)
#  spplot(frameres2,c("LABEL"), col.regions=bpy.colors(5))

## ---- eval = F,echo=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  s <- selectSampleSpatial(framenew,outstrata,coord_names=c("LON","LAT"))
#  ##
#  ## *** Sample has been drawn successfully ***
#  ##  62  units have been selected from  4  strata

## ---- eval = F,echo=TRUE,message=FALSE,fig.width=6, fig.height=8---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  s <- selectSampleSpatial(framenew,outstrata,coord_names = c("LON","LAT"))
#  coordinates(s) <- ~LON+LAT
#  proj4string(s) <- CRS("+init=epsg:28992")
#  s$LABEL <- as.factor(s$LABEL)
#  library(mapview)
#  mapview(s,zcol="LABEL", map.types = c("OpenStreetMap"))

