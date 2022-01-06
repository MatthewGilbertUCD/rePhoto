#' LineResid
#'
#' @description A housekeeping function that AngleCalcs() uses to exclude locations as being on the wrong side of a line connecting two POR i.e. will return a value of 0 if a given latitude and longitude are on the far side of a line to the actual location. The function is capable of using vectors of latitudes and longitudes, or single values. 
#' @usage LineResid(Lat1,Lat2,m,c,Lat,Long)
#' @param Lat1 value; latitude of one POR
#' @param Lat2 value; latitude of another POR. Importantly, the POR for Lat2 is assumed to be to the right of the POR for Lat1 (verified internally to rePhotograph) 
#' @param m value; slope of the line connecting the two POR
#' @param c value; intercept of the line connecting the two POR
#' @param Lat value or vector; latitudes of one or many locations to be evaluated      
#' @param Long value or vector; longitudes of one or many locations to be evaluated 
#' @details The function is based upon this logic: 1) a location exists where a photograph was taken, 2) two points of reference are ordered from left to right in a photograph and relative to that location, 3) there is a geographic line connecting the POR. Based upon these conditions, then the residuals of any arbitrary location relative to the line are diagnostic of whether the location is closer to the actual location than the line. This provides a means of excluding an arbitrary location as a possible location for the position of the camera i.e. if it is further than the line connecting the POR it can be excluded as the order of points would reverse.
#' @return residuals a vector of values the length of Lat and Long inputs. Values of 0 signify that the Lat and Long inputs are on the opposite side of a the line connectin Lat1 and Lat2
#' @examples not used by user
LineResid=function(Lat1,Lat2,m,c,Lat,Long){
  residuals=Lat-(m*Long+c)
  # Calculate line from A to x then calculate residuals; if residuals are on specific side then exclude points
  #abline(c,m,lwd=2)
  # these are all the options for 
  if(m<0 & Lat1>Lat2){
    # greater than line
    residuals=as.numeric(lapply(residuals,function(x){if(x<0){FALSE}else{TRUE}}))  
  }
  if(m<0 & Lat1<Lat2){
    # greater than line
    residuals=as.numeric(lapply(residuals,function(x){if(x>0){FALSE}else{TRUE}}))   
  }
  if(m>0 & Lat1>Lat2){
    # greater than line
    residuals=as.numeric(lapply(residuals,function(x){if(x<0){FALSE}else{TRUE}}))   
  }
  if(m>0 & Lat1<Lat2){
    # greater than line
    residuals=as.numeric(lapply(residuals,function(x){if(x>0){FALSE}else{TRUE}}))    
  }
  return(residuals)    
} # end LineResid



#' AngleCalcs
#'
#' @description A workhorse function that rePhotograph() uses to do the evaluate of each location as possible location for the camera. 
#' @usage AngleCalcs(data1,out,CameraLens=90,UseOrder=TRUE)
#' @param ... Arguments are inherited from rePhotograph() and have the same characteristics, see note above about 'out'
#' @details This function is separated from the core rePhotograph function because it enables the user to verify the trueness of a prediction of search area if a putative camera location is found through ground searching. For instance, typically 'out' is a large data frame for evaluation, but replacing 'out' with out=dataframe(Lat=40,Long=50) TEST THIS would provide an evaluation of a specific location. 
#' @return Returns 'out' the same output as rePhotograph()
#' @examples see rePhotograph vignette
AngleCalcs=function(data1,out,CameraLens,UseOrder){
  # Calculate theta for historical pictures
  Thetaphoto1=(data1[2,]$X-data1[1,]$X)/((data1[3,]$X-data1[2,]$X)+(data1[2,]$X-data1[1,]$X))
  if(nrow(data1)>3){Thetaphoto2=(data1[2,]$X-data1[1,]$X)/((data1[4,]$X-data1[2,]$X)+(data1[2,]$X-data1[1,]$X))}
  
  angA= atan2((data1[1,]$Lat-out$Lat),(data1[1,]$Long-out$Long))
  angB= atan2((data1[2,]$Lat-out$Lat),(data1[2,]$Long-out$Long))
  angC= atan2((data1[3,]$Lat-out$Lat),(data1[3,]$Long-out$Long))
  if(nrow(data1)>3){
    angD= atan2((data1[4,]$Lat-out$Lat),(data1[4,]$Long-out$Long))  
  }
  # calculate angle differences relative to A; the abs() avoids the sign difference issue
  AGB=abs(angA-angB)
  AGC=abs(angA-angC)
  BGC=abs(angB-angC)
  if(nrow(data1)>3){
    AGD=abs(angA-angD)
    BGD=abs(angB-angD)
  }
  
  # convert to angles < 180 (pi)
  AGB=as.numeric(lapply(AGB,function(x){if(x>pi){2*pi-x}else{x}}))
  AGC=as.numeric(lapply(AGC,function(x){if(x>pi){2*pi-x}else{x}}))  
  BGC=as.numeric(lapply(BGC,function(x){if(x>pi){2*pi-x}else{x}}))  
  if(nrow(data1)>3){
    AGD=as.numeric(lapply(AGD,function(x){if(x>pi){2*pi-x}else{x}}))  
    BGD=as.numeric(lapply(BGD,function(x){if(x>pi){2*pi-x}else{x}}))  
  } 
  
  # check for order i.e. AGB<AGC<AGD  i.e. because the points are entered inorder this should be true
  if(nrow(data1)>3){
    angles=AGB<AGC & AGC<AGD
  }else{
    angles=AGB<AGC
  }
  
  # check for AGC or AGD< lens angle
  if(nrow(data1)>3){
    angles2= AGD < CameraLens*pi/180
  }else{
    angles2= AGC < CameraLens*pi/180
  }
  
  m12=(data1[1,]$Lat-data1[2,]$Lat)/(data1[1,]$Long-data1[2,]$Long)
  c12=data1[1,]$Lat-(m12*data1[1,]$Long)
  residAB=LineResid(data1[1,]$Long,data1[2,]$Long,m12,c12,out$Lat,out$Long)
  m13=(data1[1,]$Lat-data1[3,]$Lat)/(data1[1,]$Long-data1[3,]$Long)
  c13=data1[1,]$Lat-(m13*data1[1,]$Long)
  residAC=LineResid(data1[1,]$Long,data1[3,]$Long,m13,c13,out$Lat,out$Long)
  m23=(data1[2,]$Lat-data1[3,]$Lat)/(data1[2,]$Long-data1[3,]$Long)
  c23=data1[2,]$Lat-(m23*data1[2,]$Long)
  residBC=LineResid(data1[2,]$Long,data1[3,]$Long,m23,c23,out$Lat,out$Long)
  if(nrow(data1)>3){
    m14=(data1[1,]$Lat-data1[4,]$Lat)/(data1[1,]$Long-data1[4,]$Long)
    c14=data1[4,]$Lat-(m14*data1[4,]$Long)
    residAD=LineResid(data1[1,]$Long,data1[4,]$Long,m14,c14,out$Lat,out$Long)
    m24=(data1[2,]$Lat-data1[4,]$Lat)/(data1[2,]$Long-data1[4,]$Long)
    c24=data1[4,]$Lat-(m24*data1[4,]$Long)
    residBD=LineResid(data1[2,]$Long,data1[4,]$Long,m24,c24,out$Lat,out$Long)
    m34=(data1[3,]$Lat-data1[4,]$Lat)/(data1[3,]$Long-data1[4,]$Long)
    c34=data1[4,]$Lat-(m34*data1[4,]$Long)
    residCD=LineResid(data1[3,]$Long,data1[4,]$Long,m34,c34,out$Lat,out$Long)
  }# end if
  
  # 
  if(UseOrder==TRUE){
    if(nrow(data1)>3){
      anglescomb=  angles & angles2 & residAB & residAC & residBC & residAD & residBD & residCD
    }else{
      anglescomb=  angles & angles2 & residAB & residAC & residBC
    }
    include=lapply(anglescomb,function(x){if(x==FALSE){0}else{1}})
  }else{
    include=angles2 # just use the camera angle 
  }
  
  Thetamap1=tan(AGB)/(tan(AGB)+tan(BGC))
  out$diff1=sqrt((Thetamap1-Thetaphoto1)^2)
  out$diff1=out$diff1*as.numeric(include)
  out$diff1=as.numeric(lapply(out$diff1,function(x){if(x==0){1}else{x}}))
  
  # runs this if there are 4 reference points
  if(nrow(data1)>3){  
    #Thetamap2=tan(AGB)/(tan(AGD))
    Thetamap2=tan(AGB)/(tan(AGB)+tan(BGD))
    out$diff2=sqrt((Thetamap2-Thetaphoto2)^2)
    out$diff2=out$diff2*as.numeric(include)
    out$diff2=as.numeric(lapply(out$diff2,function(x){if(x==0){1}else{x}}))  
  } # End If 
  
  # POST PROCESSING
  # Calculates the combination of the differences between the Theta's. If there are 4 points the combination is the geometric mean
  if(nrow(data1)>3){out$diffcomb=sqrt(out$diff1*out$diff2)}else{
    out$diffcomb=out$diff1
  }
  return(out)
} #end function



#' rePhotograph
#'
#' @description rePhotograph is the core function used to evaluate a large geographical grid (a data frame) of possible locations for a historical photograph. The input is a data frame supplying the geographical coordinates of three or four points of reference and the distances between the points of reference in a photograph. The output is the data frame with associated values that help determine the likelihood of the photographic location. Note that the function evaluates a large grid using vectorisation, never-the-less it can take time to complete the calculations ~1 to 10s with the defaults on a off the shelf laptop. 
#' @usage rePhotograph(data1,scaleToSearch=2,PtDensity=1000,CameraLens=90,UseOrder=TRUE,grid=c(0,0,0,0),...)
#' @param data1     a data frame containing 3 or 4 rows, with columns X, Lat and Long corresponding to the X distance between points of reference in the original photograph and the latitude and longitude.   
#' @param scaleToSearch   default 2; a value setting the scale of the region (i.e. grid size) to evaluate. The value applied as a multiple of the maximum range of points in data1 e.g. if the maximum range of latitudes or longitudes is 0.5 degrees, then a grid is evaluated scaleToSearch times the range, or 1 degree. 
#' @param PtDensity   default 1000; a value setting the size of the grid searched i.e. a value of 1000 evaluates a 1000 by 1000 grid of latitudes and longitudes. Note that at values > 1000 the function will take a long time to evaluate. The minimum spatial resolution is thus the maximum range of latitudes or longitudes times the scaleToSearch divided by the PtDensity. For the default values and a 0.5 degree range: 0.5 degree*2/1000 = 3.6 seconds of latitude or longitude.    
#' @param CameraLens    default 90; a value setting the approximate field of view angle for the camera lens used for the original photograph. This value is used to exclude grid locations that are too near to the points of reference to be possible for that camera angle.  
#' @param UseOrder    default TRUE; sets whether the function excludes grid locations based upon the order of the points of reference being wrong. e.g. given an order of points of reference in the photograph, then on the reverse side of points of reference the order of points are reversed.           
#' @param grid    default c(0,0,0,0); sets the boundaries of the grid of latitude and longitudes that are evaluated. This is useful to specify rectangular grids rather than the default square grid. Not used unless grid is specified as non 0 values e.g. grid=c(1,2,3,4) where 1 and 2 are the latitudes and 3 and 4 are the longitudes of the grid to be evaluated. Note this setting overrides scaleToSearch.   
#' @details The rePhotograph function is applied to data for either three or four points of reference (POR). If the latter, the calculations are run twice for groups of three points and the output combined via geometric mean. 
#' @details The rePhotograph function sets up the analysis and calls the AngleCalcs() function to do most of the calculations. 
#' @details The function could be extended to more than four points of reference, but this was not implemented in favor of the following approach. For example with six POR, use three POR to estimate a KML output showing a likely search area, then estimate an independent search area using the remaining three POR. Compare the two independently predicted search areas. In this manner incorrect POR will have less impact than with a composite analysis. 
#' @details Note that expected inputs are: latitudes + for northern and - for southern hemisphere locations, longitudes + for east and - for locations west. The X column in data1 is unitless as the ratios of lengths are used e.g. on a photograph the horizontal distances between points of reference can be measured by ruler (e.g. mm) to simply the X pixel values.    
#' @return rePhotograph returns a large data frame (default 1000x1000 rows) of latitude and longitudes and values that relate to the likelihood of these locations being the original photograph location. The data frame contains these columns:
#' @return  Lat latitudes evaluated
#' @return Long longitudes evaluated
#' @return diff1 a value relating to the likelihood that a location specified by Lat and Long is the location of the camera in the original photograph. This is based upon the first three POR. Values of 1 are default indicators of exclusion from consideration. 
#' @return diff2 the same as diff1 but based upon the first, second and fourth POR. 
#' @return diffcomb the geometric mean of diff1 and diff2, or just diff1 if three POR were input. 
#' @return include logical values (1 or 0) determining if the location was excluded based upon the camera angles and order of POR (if TRUE). A value of 0 indicates exclusion, 1 indicates inclusion.  
#' @return Note that the logic applying 'include' is applied to diff1 and diff2. Thus, to override this logic and see actual diff values, set UseOrder = FALSE and CameraLens=360 
#' @examples See rePhotograph vignette
rePhotograph<-function(data1,scaleToSearch=2,PtDensity=1000,CameraLens=90,UseOrder=TRUE,grid=c(0,0,0,0),...){
  # use user entered grid to evaluate, or the default, a estimated grid based upon the points of reference
  data1=data1[with(data1,order(X)),]
  if(PtDensity>1000){
    print("At PtDensity > 1000 the grid evaluation can take some time")
  }
  
  if(sum(grid)!=0){
    # swap the two so that they are ascending  
    if(grid[3]>grid[4]){
      temp=grid[4]
      grid[4]=grid[3]
      grid[3]=temp
    }
    if(grid[1]>grid[2]){
      temp=grid[2]
      grid[2]=grid[1]
      grid[1]=temp
    }
    Lat=rep(seq(grid[1],grid[2],length.out=PtDensity),PtDensity)
    Long=rep(seq(grid[3],grid[4],length.out=PtDensity),times=1,each=PtDensity)
  }else{
    # Find range of lat and long to search
    avgLat=mean(data1$Lat)
    avgLong=mean(data1$Long)
    range=(scaleToSearch/2)*max(c(max(data1$Lat)-min(data1$Lat),max(data1$Long)-min(data1$Long)))
    Lat=rep(seq(avgLat-range,avgLat+range,length.out=PtDensity),PtDensity)
    Long=rep(seq(avgLong-range,avgLong+range,length.out=PtDensity),times=1,each=PtDensity)
  }
  
  
  # Initiate output dataframe and initial values
  diff1=rep(0,PtDensity^2)
  diff2=rep(0,PtDensity^2)
  include=rep(0,PtDensity^2)
  diffcomb=rep(0,PtDensity^2)
  out=data.frame(Lat,Long,diff1,diff2,diffcomb,include)
  
  out=AngleCalcs(data1=data1,out=out,CameraLens=CameraLens,UseOrder=UseOrder)
  
  if(min(out$diffcomb==1)){
    warning("Warning: No locations were found! There are two likely causes:\n 1) the geographic grid evaluated may be too small (i.e. it doesn't contain the original location).\n     Increase the value of scaleToSearch when calling the rePhotograph function, or\n 2) the grid might be too large (the resolution of the grid evaluated was too coarse).\n     Try specifying a small range of latitude and longitude to evaluate.","\n")
  }
  return(out) # returns the data.frame with the entire grid
} # end function call    


#' plot.search.area
#'
#' @description Used to plot the output of rePhotograph(), showing regions of similar differences i.e. regions where the original photograph was likely to be taken from. The main uses are to visualize the output of rePhotograph() and use these to change the default settings to narrow the result on one area e.g. by changing the grid or scaleToSearch or PtDensity. An important use is to evaluate a suitable threshold to determine the search area output in the outputKML function. For instance, in the examples above a small area of the total grid has a diffcomb value of < 0.05, this is the default threshold for outputKML i.e. only that area will be output as a KML search area for use with GIS software. In the last figure above, a very small region has a diffcomb<0.01, and this could be used to produce a very small search area as is shown in the example above. 
#' @usage plot.search.area(data1,out,extrapoints=data.frame(Lat=0,Long=0),title="Search areas are represented by lower values")
#' @param data1 the data input into rePhotograph providing the locations of the three or four points of reference. These are plotted as +'s in the contour plot 
#' @param out the data output from rePhotograph providing the data frame of locations and results; 
#' @param extrapoints a data.frame of any length that contains latitude and longitudes; allows plotting of other reference points that may be used as guides or possibly to mark a guess at the photograph location.   
#' @param title a character vector providing a title for the plot e.g. a general location
#' @details This function is relatively simple and could be unpacked and reformulated to provide a user specific output. 
#' @return A filled.contour plot conditioned to show relevant aspects of the output of rePhotograph. The 'out' data frame is conditioned into a matrix and plotted as contours of locations that have similar 'diffcomb' values. These regions indicate the likely areas in which the original photograph was likely taken, lower values being more likely.
#' @examples see rePhotograph vignette
plot.search.area<-function(data1,out,extrapoints=data.frame(Lat=0,Long=0),title="Search areas are represented by lower values"){
  data1=data1[with(data1,order(X)),]
  # Create a matrix from 'out' the data.frame
  PtDensity=round(sqrt(nrow(out)),0)
  # Calculates matrix of results to be used for calculating contours
  matr=matrix(out$diffcomb,ncol=PtDensity,nrow=PtDensity) # out$diffcomb
  matr=t(matr)
  colnames(matr)=unique(out$Long)
  rownames(matr)=unique(out$Lat)
  # plot the matrix
  axistcks=sqrt(c(0.0001,0.001,0.01,0.05,0.1,0.2,0.99))
  if(sum(extrapoints$Lat,extrapoints$Long)==0){
    filled.contour(x=unique(out$Long),y=unique(out$Lat),z=matr,levels=c(0.001,0.01,0.05,0.1,0.2,0.99),
                   plot.title = title(xlab="Longitude",ylab="Latitude",main=title),
                   key.title="abs(Thetaphoto-Thetamap)",
                   key.axes=axis(4,at=c(0.001,0.01,0.05,0.1,0.2,0.99)),
                   plot.axes = {points(data1$Long,data1$Lat,pch=3,lwd=2);
                     axis(1); axis(2)
                   }
    )
  }else{
    filled.contour(x=unique(out$Long),y=unique(out$Lat),z=matr,levels=c(0.001,0.01,0.05,0.1,0.2,0.99),
                   plot.title = title(xlab="Longitude",ylab="Latitude",main=title),
                   key.title="abs(Thetaphoto-Thetamap)",
                   key.axes=axis(4,at=c(0.001,0.01,0.05,0.1,0.2,0.99)),
                   plot.axes = {points(data1$Long,data1$Lat,pch=3,lwd=2);
                     points(extrapoints$Long,extrapoints$Lat,pch=21,lwd=2,col="magenta");
                     axis(1); axis(2)
                   }
    )
    
  }
}

#' outputKML
#'
#' @description A function for output of a KML file for use with GIS software. The KML file shows a contour encompassing a search area for the location of the camera in the original photograph.
#' @usage outputKML(out,filename="yourfilename",Thres=0.05)
#' @param out the output of rePhotograph()
#' @param filename the name of the output KML file  
#' @param Thres the threshold for the contour output i.e. the default, Thres = 0.05, includes in the contour all locations that have a out$diffcomb < 0.05. This value can be adjusted based upon the regions shown in plot.search.area()   
#' @details This function requires functions from the sp, rgdal and maptools packages
#' @import sp, rgdal, maptools
#' @return A KML file that can be opened in Google Earth, CalTopo etc 
#' @examples see rePhotograph vignette
outputKML<-function(out,filename="yourfilename",Thres=0.05){
  # Export contour lines to polygon then shape file .kml
  # Create a matrix from 'out' the data.frame
  PtDensity=round(sqrt(nrow(out)),0)
  matr=matrix(out$diffcomb,ncol=PtDensity,nrow=PtDensity) # out$diffcomb
  matr=t(matr)
  colnames(matr)=unique(out$Long)
  rownames(matr)=unique(out$Lat)
  cl<-contourLines(x=unique(out$Long),y=unique(out$Lat),z=matr,levels=c(Thres))
  
  # the following code converts the contourlines into a dataframe, then into a SpatialLinesDataFrame object (shp).
  # note that this code can be replaced with the maptools::ContourLines2SLDF(cl) function, but has been replaced as maptools is due for retirement
  # sourced from here: https://stackoverflow.com/questions/24284356/convert-spatialpointsdataframe-to-spatiallinesdataframe-in-r
  
  clout=as.data.frame(cl[1])
  colnames(clout)=c("id","x","y")
  clout$id=as.character(1)
  for(i in 1:length(cl)){
    if(i>1){
      cltemp=as.data.frame(cl[i])
      colnames(cltemp)=c("id","x","y")
      cltemp$id=as.character(i)
      clout=rbind(clout,cltemp)
    }
  }
  sp::coordinates(clout) <- ~x+y
  sclout <- lapply(split(clout, clout$id), function(k) sp::Lines(list(sp::Line(sp::coordinates(k))), k$id[1L]))
  lines <- sp::SpatialLines(sclout)
  data <- data.frame(id = unique(clout$id))
  rownames(data) <- data$id
  shp <- sp::SpatialLinesDataFrame(lines, data)

  #Build a SpatialPointsData Frame
  sp::proj4string(shp)<-sp::CRS("+proj=longlat +datum=WGS84")   # this specifies the WGS84 datum for maps, and may not work with as much precision when plotted on older topographic maps (errors as much as 100's of meters)
  # Create a kml file that can be opened in Google Earth or CalTopo
  i=1
  if(file.exists(paste(filename,".kml",sep=""))){
    while(file.exists(paste(filename,".kml",sep=""))){
      filename=paste(filename,i,sep="")
      i=i+1
    }
    warning(paste("Warning: Filename in use, saved as: ",filename))
  }
  rgdal::writeOGR(shp, dsn=paste(filename,".kml",sep=""),layer="shp",  driver="KML")
}
