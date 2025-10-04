library(readxl)
library(tidyverse)
library(baffle)
library(png)
library(jpeg)
library(ggplot2)
library(scales)
library(ggpubr)
library(gplots)
library(shape)
library(R2jags)
library(latex2exp)
## to have base plot and ggplots together
library(gridBase)
library(grid)
## alternative combine ggplot and base
library(cowplot)
library(gridGraphics)
library(sjmisc)
library(paletteer)

## Original Script written G.Dauphin in November 2024
## provided as supporting information for reproducibility purposes 

## We empty the workspace because 
## In case something was run in between 
## Good practice would be to open the R project after the model has been run

rm(list=ls())

##_________________________________________

## We load the various objects we need for the various figures
## Figures 1, 4, 5, 6 -> model output 
load("R_Data/data_inits_jags.Rdata")
load("R_Data/Jags_output.Rdata")
## Figures 2,3 -> receivers detections 
load("R_Data/data_fig2-3.Rdata")
##_________________________________________

##_________________________________________
### Figure 1 ####
#### Generating panel with Map with zones  ####
#map_img  <-  readJPEG("R_Data/ReceiversArrays_29112024.jpg")
channel_img <- readPNG("R_Data/Google_Earth_2022_channels_scale.png")

#### Coordinates receivers - upstream/downstream of islands coordinates guessed ####
## small map
coord_sum <- data.frame(
  names_receivers = c("Rx1","R1a","R1b","R2a","Rx2b","Rx2a","R2b","R3a","R3b"),
  # Receivers numbering manuscript
  number_rec = c(3,4,5,6,7,8,9,10,11),
  
  x_coord = c(130,412,548,1122,1254,1278,1440,1552,1550),
  y_coord = 687 - c(424,255,412,260,490,451,518,393,486),
  
  x_text = c(130,412,548,1122,1254,1338,1440,1552,1550),
  y_text = 687 - c(374,205,462,210,540,434,568,343,536),
  col_bg = c("lightskyblue","olivedrab3","hotpink1","olivedrab3","hotpink1","hotpink4","hotpink1","olivedrab3","hotpink1")
)

coord_sum$lab_txt <- sprintf("D[%s]", coord_sum$numb)


## Coordinates zones - a fish can only be in one of the channels in  a given zone
zone_col <-c("grey90","grey10","grey90","grey10","grey90")

col_rec<-c(col2hex("sienna"),
           
           rgb(166,189,219,maxColorValue = 255),
           rgb(116,169,207,maxColorValue = 255),
           rgb(54,144,192,maxColorValue = 255),
           rgb(5,112,179,maxColorValue = 255),
           rgb(3,78,123,maxColorValue = 255)
)

coord_zone <- list(
  data.frame(
    x =  c(130,130,68,350,412,548,548)  ,
    y = 687 - c(687,424,352.4265,183.4265,255,412,687) ),
  data.frame(
    x = c(350,412,548,548,1254,1254,1151,1122,    1110,888),
    y = 687-c(183.4265,255,412,687,687,490,387,260, 207.4483   ,168) ),
  data.frame(
    x = c(1254,1342,1342,1328,1110,1122,1151,1254    ),
    y = 687-c(687,687,394,306,207.4483,260,387, 490 ) ),
  data.frame(
    x = c(1328,1342,1342,1443,1443,1443),
    y = 687-c(306,394,687,687,518,372) ),
  data.frame(
    x = c(1443,1443,1550,1550),
    y = 687-c(372,687,687,368) )
)

## Quick calculation to get zone parallelograms

slope = (412-255)/(548-412) 
intercept = 412-slope*548
slope*350+intercept

intercept2 = 424-slope*130
x2=130-(412-350)
slope*x2+intercept2

x2

slope = (260-387)/(1122-1151)
intercept = 260-slope*1122
slope*1110+intercept

coord_chan <- data.frame(
  ## Coordinates Channels 
  ## Z1a, Z1b, 
  ## Z2a, Z2ab, z2b, 
  ## 23a, Z3_ab_dodge, Z3_ab_no_dodge, Z3_b
  ## Z4a, Z4ab, z4b, 
  ## Z5a, Z5b
  x = c(320,320,
        1070,1125 ,1138, #
        1270,1282,1230,1300,
        1392,1373,1395,  #
        1504,1505),
  
  y = 687 - c(294,428,
              255,382,457,   #
              317,400,435,491,
              400,477,518, #
              399,484),
  numb = c("1,1","1,2",
           "2,1","2,2","2,3",
           "3,1","3,2","3,3","3,4",
           "4,1","4,2","4,3",
           "5,1","5,2")
)
#coord_chan$lab_txt <- sprintf("X[%s]", coord_chan$numb)


coord_path_1 <-data.frame(
  x=c(41,181,417,658,875,1043,1253,1394,1450,1550,1660),
  y=687 - 
    c(458,395,245,205,207,246,310,398,428,391,380),
  I_col = rep(1,11),
  I_path = rep(1,11)
)

coord_path_2 <-data.frame(
  x=c(1450,1497,1529,1663),
  y=687 - 
    c(428,469,485,484),
  I_col = rep(1,4),
  I_path = rep(2,4)
)

coord_path_3 <-data.frame(
  x=c(181,432,681,940,1058,1334,1416,1481,1529),
  y=687 - 
    c(395,434,414,413,435,496,522,504,486),
  I_col = rep(1,9),
  I_path = rep(3,9)
)

coord_path_4 <-data.frame(
  x=c(1334,1390,1450),
  y=687 - 
    c(496,474,428),
  I_col = rep(1,3),
  I_path = rep(4,3)
)

coord_path_5 <-data.frame(
  x=c(1390,1497),
  y=687 - 
    c(474,469),
  I_col =rep(1,2),
  I_path = rep(5,2)
)

coord_path_6 <-data.frame(
  x=c(1043,1098,1140,1238,1329,1390),
  y=687 - 
    c(246,353,385,424,469,474),
  I_col =rep(2,6),
  I_path = rep(6,6)
)

coord_path_7 <-data.frame(
  x=c(1238,1300,1394),
  y=687 - 
    c(424,392,398),
  I_col =rep(3,3),
  I_path = rep(7,3)
)

coord_path_8 <-data.frame(
  x=c(940,1140),
  y=687 - 
    c(413,385),
  I_col =rep(4,2),
  I_path = rep(8,2)
)

list_coord_path <- rbind(coord_path_1,coord_path_2,coord_path_3,coord_path_4,coord_path_5,coord_path_6,coord_path_7,
                         coord_path_8)
I_col <- c("lightblue2","steelblue1","royalblue3","royalblue3","deepskyblue2")
I_rec_col <-c(1,2,2,3,3,4,5,6,6)

channel_location_names <- c('Z1_a','Z1_b','Z2_a','Z2_ab'  ,'Z2_b','Z3_a','Z3_b','Z3_ab','Z4_a','Z4_b')

#### Loading png with a schematic of an RST
RST_png <- readPNG("R_data/smolt_wheel.png")
RST_rev_png <- readPNG("R_data/smolt_wheel_rev.png")

#### Need to run Network.R ####
## No judgment please it's a mess !
## Generates the schematic of the island network pathways
source("Scripts/Network.R")
##_________________________________________


graph_plot2 <- graph_plot +
  annotation_raster(RST_rev_png , ymin = yy+0.8,ymax= yy+0.9565,xmin = 2.3,xmax = 2.6) +
  annotation_raster(RST_rev_png , ymin = yy-1.2565,ymax= yy-1.1,xmin = 4.3,xmax = 4.6) 

#### Combining the photo of the site and the schematic version of the network ###
png("Figures/MS_map_exp.png",width=1500,height=1300,pointsize=16,type="cairo-png") ## "cairo-png" for clean vectorized shapes
#pdf("Figures/MS_map_exp.pdf",width=7,height=6,pointsize=1)
#tiff("Figures/MS_map_exp.tif",width=1500,height=1300)

par(mfrow=c(2,1))
par( mar=c(0,0,4,0))

plot(500,500,xlim=c(1,1695),ylim=c(1,687), type='n', main="", xlab="", ylab="",asp=1,axes=F, yaxs = "i")

# Get the plot information so the image will fill the plot box, and draw it
lim <- par()
rasterImage( channel_img , 
             xleft=1, xright=1695, 
             ybottom=1, ytop=687)

## Plot the paths allowed in the model
temp_path <- list_coord_path 

## Start with the drawing of the zones
coord_names_zone <- c(341,938,1300,1395,1500)

for(j in 1:length(coord_zone)){  
  polygon(x=coord_zone[[j]]$x,
          y=coord_zone[[j]]$y,
          col=rgb(t(col2rgb(col_rec[j+1])),alpha=150,maxColorValue = 255), 
          border=col_rec[j+1], #zone_col[j],
          lwd=2
          )
}
## plot the allowed paths
for( j in unique(temp_path$I_path)){
  temp_path2 <- temp_path %>% filter(I_path==j)
  points(temp_path2$x,temp_path2$y,type="l",col="tomato1",lty=2,lwd=4)
}



## Plot the receivers
for (j in 1:9){
  points(coord_sum$x_coord[j],coord_sum$y_coord[j],pch=22, 
         bg = "white",
         cex=4)
  text(coord_sum$x_coord[j],coord_sum$y_coord[j], labels=parse(text=coord_sum$lab_txt[j]),col="black",cex=1)
}


## clean printing of expressions
ij <- strsplit(coord_chan$numb, ",", fixed = TRUE)
lab_expr <- lapply(ij, function(p) bquote(X[.(p[1]) * "," * .(p[2])]))
lab_expr <- as.expression(lab_expr)


## plot the channel location
for (j in 1:length(coord_chan$x)){
  points(coord_chan$x[j],coord_chan$y[j],pch=21, 
         bg = "khaki1",col=NA, cex =5 )
  text(coord_chan$x[j],coord_chan$y[j], labels=lab_expr[[j]],col="black",cex=1)
}

## Add the zone name
for(j in 1:length(coord_zone)){
  text(coord_names_zone[j], y = 687-647,   labels = paste("Zone",j),col="white",cex=1.2)
}

## Moses
rasterImage( RST_rev_png,
             xleft=1182, xright=1242, 
             ybottom=687-291, ytop=687-260)
## Butters
rasterImage( RST_rev_png,
             xleft=1455, xright=1515, 
             ybottom=687-518, ytop=687-487)

#text(40,650,"A",col="white",cex=4 )

## the last one is the current plot
plot.new()              
vps <- viewport(width=unit(0.8, "npc"),height = unit(0.55, "npc"),y=0.25,x=0.55,mask="none") #baseViewports()
#pushViewport(vps$figure) ##   I am in the space of the network plot
# vp1 <-plotViewport(c(2,1,1,1)) ## create new vp with margins, you play with thess values 
#print(graph_plot,vp = vps1) 

igraph_rec_coord <- data.frame(
  x=c(0,
      1,1,
      2,2,
      3,
      4,
      5,5),
  y=c(yy-0.2,
    yy+0.8,yy-1.20,
    yy+0.8,yy-1.20,
    yy-0.5,
    yy-1.2,
    yy+0.3,yy-1.20)
)
igraph_rec_coord$lab_txt <- sprintf("D[%s]", coord_sum$numb)


graph_plot2<- graph_plot2+
  geom_point(data=igraph_rec_coord,aes(x=x,y=y),size=14,shape=22)+
  geom_text(data = igraph_rec_coord,
            aes(x = x, y = y, label = lab_txt),
            parse = TRUE, vjust = 0.5,size=6)
  


print(graph_plot2 ,vp = vps) 
#text(0.045,1.09,"B",col="black",cex=4 )
dev.off()
##_________________________________________

##_________________________________________
### Figure 2 ####
## Creating a nice graph to showcase the differences in speed between environments 
## we need to create the referential
## y is going to be the speed in m.s-1
## x the different spatial point of interest:
##      Release site - Rx1 - R3a/R3b - HoT - Inner Bay - outer bay - Sobi 
## 
## different graphic elements:
## +we will plot the speeds as transparent points 
##  -Rx1 (release -> Rx1)
##  -R3a/R3b (Rx1 -> R3a/R3b)
##  -HoT (release -> Hot)
##  -Inner Bay (HoT -> inner)
##  -Outer Bay (HoT -> outer)
##  -SoBI (HoT -> SoBI)
## 
## + n = xx above each sets of points 
## + probably want a boxplot 
##
## + polygons from point a to b for which speed is calculated (ideally with some curvature to make it nicer) 
## + Bonus: identify River/ estuary / open ocean with images on the top layer

pal_graph <- rev(viridis(7))

## x coordinates of spatial of points of interest 
## Helps if we decide to change the spreas of the points
x_release <- 1
x_Rx1 <- 10
x_R <- 15
x_HoT <- 20
x_inner <- 27
x_outer <- 35
x_sobi <- 50

## plotting speeds
## First we create polygons from the starting point to the arrival point
## we make a logistic shape that starts at zero and fans out to +/- sd speed 
## at the arrival point (m.s-1 orkm.day=1)

## We create a little function to make things a bit tidier and practical 
## The fuction generates polygons with a logitic shape to represent speed between 2 points
## x_start = start point for speed calculation e.g. x_release
## x_end = end point for speed calculation e.g. x_Rx1
## x_corr = TRUE/FALSE id the starting point is different that the release point 
##          we have to correct the starting point of the logistic
## mean_s and sd_s = mean  and sd of the speed, affects the fan of the polygon
## k = we use this as a graphical parameter to adjust the steepness of the curve of the polygon
## col_p = color of the polygon 
## alpha_col = 1-255 adjusts the level of transparency of the polygon
## ---
## logistic curve
## L  = curve max value
## k  = growth rate
## x0 = sigmoid midpoint (here we choose to have it at the midpoint between start and end)
## x  = real number

logi_curves <- function(x_start=x_start,x_end=x_end,x_corr= FALSE,mean_s=mean_s,sd_s=sd_s,k=k,col_p=col_p,alpha_col=alpha_col){
  if(!isTRUE(x_corr)){
    x0 <- (x_start+x_end)/2
    x <- seq(x_start,x_end,by=0.1)
  }else{
    x0 <- (x_start+x_end)/2 - (x_start-x_release+1)
    x <- seq(x_start,x_end,by=0.1) - (x_start-x_release+1)
  }
  L1 <- mean_s - sd_s
  L2 <- mean_s + sd_s
  temp <- L1 / (1 + exp (-k * (x -x0) ))
  temp2 <- L2 / (1 + exp (-k * (x -x0) ))
  if(!isTRUE(x_corr)){
    polygon(x=c(x,rev(x)),y=c(temp,rev(temp2)),col=rgb(t(col2rgb(col_p)),alpha=alpha_col,maxColorValue = 255),
            border=rgb(t(col2rgb(col_p)),alpha=alpha_col,maxColorValue = 255))
  }else{
    polygon(x=c(x+(x_start-x_release+1),rev(x)+(x_start-x_release+1)),y=c(temp,rev(temp2)),col=rgb(t(col2rgb(col_p)),alpha=alpha_col,maxColorValue = 255),
            border=rgb(t(col2rgb(col_p)),alpha=alpha_col,maxColorValue = 255))
  }
}

## to have the figure in
## 1: m.s-1
## 2: km.day-1
i<-2

if(i==1){
  tiff(filename = "Figures/speed_summary_m_s.tif",width=2500,height=1200,pointsize=24) 
}
if(i==2){
  tiff(filename = "Figures/speed_summary_km_day.tif",width=2500,height=1200,pointsize=24) 
}

##Setting the plot frame + axes
par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(4,5,2,2))

if(i==1){
  plot(0,0,type="n",xlim = c(0,50.6), ylim=c(0,1.7),axes=F,xlab="",ylab=expression(paste("Migration speed ( ",m.s^-1,")")),xaxs="i")
  axis(2,at=c(0,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6,1.8) ,las=2 )
}
if(i==2){
  plot(0,0,type="n",xlim = c(0,50.6), ylim=c(0,150),axes=F,xlab="",ylab=expression(paste("Migration speed( ",km.day^-1,")")),xaxs="i")
  axis(2,at=c(0,25,50,75,100,125,150) ,las=2 )
}
axis(1,at=c(x_release,x_Rx1,x_R,x_HoT,x_inner,x_outer,x_sobi),las=1,
     #labels=rep("",7))
     labels = c("Release site",expression(D[3]),expression(D[10:11]),expression(D[12]*" / HoT"),"Inner Bay", "Outer Bay", "SoBI"))

mtext(at=c(5.5,12.5,17.5,23.5,31,42.5),
      side=rep(1,6),
      line=rep(2.5,6),
      text= c("101 km", "6.35 km","9.5 km","27.8 km","80.5 km","740 km"))

### displaying distance between receivers
if(i==1){
  y_seg <- -0.234
}
if(i==2){
  y_seg <- -20.8
}  
par(xpd=NA)

arrows(4.3,y_seg ,1,y_seg ,lwd=2) ; arrows(6.7,y_seg ,10,y_seg ,lwd=2)
arrows(11.3,y_seg ,10,y_seg ,lwd=2) ; arrows(13.7,y_seg ,15,y_seg ,lwd=2)
arrows(16.3,y_seg ,15,y_seg ,lwd=2) ; arrows(18.7,y_seg ,20,y_seg ,lwd=2)
arrows(22.3,y_seg ,20,y_seg ,lwd=2) ; arrows(24.7,y_seg ,27,y_seg ,lwd=2)
arrows(29.8,y_seg ,27,y_seg ,lwd=2) ; arrows(32.2,y_seg ,35,y_seg ,lwd=2)
arrows(41.3,y_seg ,35,y_seg ,lwd=2) ; arrows(43.7,y_seg ,50,y_seg ,lwd=2)
par(xpd=FALSE)

#### Trick
polygon(x=c(x_release-3,x_Rx1,x_Rx1,x_release-3),y=c(-10,-10,160,160),col="grey85",border=NA)
polygon(x=c(x_Rx1,x_R,x_R,x_Rx1),y=c(-10,-10,160,160),col="grey75",border=NA)
polygon(x=c(x_R,x_sobi+3,x_sobi+3,x_R),y=c(-10,-10,160,160),col="grey65",border=NA)

if(i==1){
  y_rect1 = 1.48
  y_rect2 = 1.61
  y_text = 1.55
}
if(i==2){
  y_rect1 = 111 +32
  y_rect2 = 120.75 +32
  y_text = 116.25 +32
}

round_rect(3.8, y_rect1,6.2,y_rect2,border=NA,col="grey95",xr=0.05,yr=0.05,n=20)
text(5,y_text,"lotic\nhabitat")
round_rect(11.2,y_rect1,13.8,y_rect2,border=NA,col="grey95",xr=0.05,yr=0.05,n=20)
text(12.5,y_text,"island\nnetwork")
round_rect(30.8,y_rect1,33.3,y_rect2,border=NA,col="grey95",xr=0.05,yr=0.05,n=20)
text(32,y_text,"pelagic\nhabitat")

box()

## release-Rx1
if(i==1){
  mean_rx1_speed<-mean((101*1000)/(as.numeric(table_MS$travel_time_to_Rx1)*24*3600),na.rm=T)
  sd_rx1_speed<-sd((101*1000)/as.numeric(table_MS$travel_time_to_Rx1*24*3600),na.rm=T)
}
if(i==2){
  mean_rx1_speed<-mean((101)/(as.numeric(table_MS$travel_time_to_Rx1)),na.rm=T)
  sd_rx1_speed<-sd((101)/as.numeric(table_MS$travel_time_to_Rx1),na.rm=T)
}
logi_curves(x_start=x_release,x_end=x_Rx1,mean=mean_rx1_speed,sd=sd_rx1_speed,k=1.2,col=pal_graph[1],alpha_col=100)

## Rx1->R3a/R3b
if(i==1){
  mean_channel_speed <-mean( (6.355 *1000) / (time_in_channel$tot_time * 60))
  sd_channel_speed<-sd((6.355 *1000) / (time_in_channel$tot_time * 60),na.rm=T)
}
if(i==2){
  mean_channel_speed <-mean( (6.355 ) / (time_in_channel$tot_time / 1440))
  sd_channel_speed<-sd((6.355 ) / (time_in_channel$tot_time / 1440),na.rm=T)
}
logi_curves(x_start=x_Rx1,x_end=x_R,x_corr = TRUE,mean=mean_channel_speed,sd=sd_channel_speed,k=2.5,col=pal_graph[2],alpha_col=100)

## release->HoT
if(i==1){
  mean_HoT_speed <- mean((116.7*1000)/(as.numeric(table_MS$travel_time_to_HoT_first)*24*3600),na.rm=T)
  sd_HoT_speed <- sd((116.7*1000)/(as.numeric(table_MS$travel_time_to_HoT_first)*24*3600),na.rm=T)
}
if(i==2){
  mean_HoT_speed <- mean((116.7)/(as.numeric(table_MS$travel_time_to_HoT_first)),na.rm=T)
  sd_HoT_speed <- sd((116.7)/(as.numeric(table_MS$travel_time_to_HoT_first)),na.rm=T)
}  
logi_curves(x_start=x_release,x_end=x_HoT,x_corr = FALSE,mean=mean_HoT_speed,sd=sd_HoT_speed,k=0.5,col=pal_graph[3],alpha_col=100)

## HoT->Inner
if(i==1){
  mean_inner_speed <- mean((27.8*1000)/(as.numeric(table_MS$travel_time_HoT_inner_first)*24*3600),na.rm=T)
  sd_inner_speed <- sd((27.8*1000)/(as.numeric(table_MS$travel_time_HoT_inner_first)*24*3600),na.rm=T)
}
if(i==2){
  mean_inner_speed <- mean((27.8)/(as.numeric(table_MS$travel_time_HoT_inner_first)),na.rm=T)
  sd_inner_speed <- sd((27.8)/(as.numeric(table_MS$travel_time_HoT_inner_first)),na.rm=T)
}
logi_curves(x_start=x_HoT,x_end=x_inner,x_corr = TRUE,mean=mean_inner_speed,sd=sd_inner_speed,k=1.0,col=pal_graph[4],alpha_col=100)

## HoT->Outer
if(i==1){
  mean_outer_speed <- mean((108.3*1000)/(as.numeric(table_MS$travel_time_HoT_outer_first)*24*3600),na.rm=T)
  sd_outer_speed <- sd((108.3*1000)/(as.numeric(table_MS$travel_time_HoT_outer_first)*24*3600),na.rm=T)
}
if(i==2){
  mean_outer_speed <- mean((108.3)/(as.numeric(table_MS$travel_time_HoT_outer_first)),na.rm=T)
  sd_outer_speed <- sd((108.3)/(as.numeric(table_MS$travel_time_HoT_outer_first)),na.rm=T)
}

logi_curves(x_start=x_HoT,x_end=x_outer,x_corr = TRUE,mean=mean_outer_speed,sd=sd_outer_speed,k=0.6,col=pal_graph[5],alpha_col=100)

## HoT->SoBI
if(i==1){
  mean_sobi_speed <- mean((741.5*1000)/(as.numeric(table_MS$travel_time_HoT_sobi_first)*24*3600),na.rm=T)
  sd_sobi_speed <- sd((741.5*1000)/(as.numeric(table_MS$travel_time_HoT_sobi_first)*24*3600),na.rm=T)
}
if(i==2){
  mean_sobi_speed <- mean((741.5)/(as.numeric(table_MS$travel_time_HoT_sobi_first)),na.rm=T)
  sd_sobi_speed <- sd((741.5)/(as.numeric(table_MS$travel_time_HoT_sobi_first)),na.rm=T)
}

logi_curves(x_start=x_HoT,x_end=x_sobi,x_corr = TRUE,mean=mean_sobi_speed,sd=sd_sobi_speed,k=0.4,col=pal_graph[6],alpha_col=100)

## release-Rx1
if(i==1){
  points(rep(x_Rx1,45),(101*1000)/(as.numeric(table_MS$travel_time_to_Rx1,na.rm=T)*24*3600),
         col=rgb(50,50,50,alpha=100,maxColorValue = 255),pch=16)
  text(x_Rx1,0.95,paste0("n = ",length(na.omit(table_MS$travel_time_to_Rx1))))
}
if(i==2){
  points(rep(x_Rx1,45),(101)/(as.numeric(table_MS$travel_time_to_Rx1,na.rm=T)),
         col=rgb(50,50,50,alpha=100,maxColorValue = 255),pch=16)
  text(x_Rx1,81,paste0("n = ",length(na.omit(table_MS$travel_time_to_Rx1))))
}

segments(x_Rx1,mean_rx1_speed+sd_rx1_speed,x_Rx1,mean_rx1_speed-sd_rx1_speed,col=rgb(t(col2rgb(pal_graph[1])),alpha=200,maxColorValue = 255),lwd=2)
points(x_Rx1,mean_rx1_speed,col=rgb(t(col2rgb(pal_graph[1])),alpha=255,maxColorValue = 255),pch=16,cex=1.4  )

## Rx1->R3a/R3b
if(i==1){
  points(rep(x_R,21),(6.355 *1000) / (time_in_channel$tot_time * 60),
         col=rgb(50,50,50,alpha=100,maxColorValue = 255),pch=16)
  text(x_R,1.7,paste0("n = ",length(na.omit(time_in_channel$tot_time))))
}
if(i==2){
  points(rep(x_R,21),(6.355 ) / (time_in_channel$tot_time /1440),
         col=rgb(50,50,50,alpha=100,maxColorValue = 255),pch=16)
  text(x_R,144,paste0("n = ",length(na.omit(time_in_channel$tot_time))))
}  
segments(x_R,mean_channel_speed+sd_channel_speed,x_R,max(mean_channel_speed-sd_channel_speed,0),col=rgb(t(col2rgb(pal_graph[2])),alpha=200,maxColorValue = 255),lwd=2)
points(x_R,mean_channel_speed,col=rgb(t(col2rgb(pal_graph[2])),alpha=255,maxColorValue = 255),pch=16,cex=1.4  )

## release->HoT
if(i==1){
  points(rep(x_HoT,45),(116.7*1000)/(as.numeric(table_MS$travel_time_to_HoT_first,na.rm=T)*24*3600),
         col=rgb(50,50,50,alpha=100,maxColorValue = 255),pch=16)
  text(x_HoT,0.77,paste0("n = ",length(na.omit(table_MS$travel_time_to_HoT_first))))
} 
if(i==2){
  points(rep(x_HoT,45),(116.7)/(as.numeric(table_MS$travel_time_to_HoT_first,na.rm=T)),
         col=rgb(50,50,50,alpha=100,maxColorValue = 255),pch=16)
  text(x_HoT,67,paste0("n = ",length(na.omit(table_MS$travel_time_to_HoT_first))))
} 
segments(x_HoT,mean_HoT_speed+sd_HoT_speed,x_HoT,mean_HoT_speed-sd_HoT_speed,col=rgb(t(col2rgb(pal_graph[3])),alpha=200,maxColorValue = 255),lwd=2)
points(x_HoT,mean_HoT_speed,col=rgb(t(col2rgb(pal_graph[3])),alpha=255,maxColorValue = 255),pch=16,cex=1.4  )

## HoT->Inner
if(i ==1){
  points(rep(x_inner,45),(27.8*1000)/(as.numeric(table_MS$travel_time_HoT_inner_first,na.rm=T)*24*3600),
         col=rgb(50,50,50,alpha=100,maxColorValue = 255),pch=16)
  text(x_inner,0.61,paste0("n = ",length(na.omit(table_MS$travel_time_HoT_inner_first))))
}
if(i ==2){
  points(rep(x_inner,45),(27.8)/(as.numeric(table_MS$travel_time_HoT_inner_first,na.rm=T)),
         col=rgb(50,50,50,alpha=100,maxColorValue = 255),pch=16)
  text(x_inner,51,paste0("n = ",length(na.omit(table_MS$travel_time_HoT_inner_first))))
}
segments(x_inner,mean_inner_speed+sd_inner_speed,x_inner,mean_inner_speed-sd_inner_speed,col=rgb(t(col2rgb(pal_graph[4])),alpha=200,maxColorValue = 255),lwd=2)
points(x_inner,mean_inner_speed,col=rgb(t(col2rgb(pal_graph[4])),alpha=255,maxColorValue = 255),pch=16,cex=1.4  )

## HoT->Outer
if(i ==1){
  points(rep(x_outer,45),(108.3*1000)/(as.numeric(table_MS$travel_time_HoT_outer_first,na.rm=T)*24*3600),
         col=rgb(50,50,50,alpha=100,maxColorValue = 255),pch=16)
  text(x_outer,0.46,paste0("n = ",length(na.omit(table_MS$travel_time_HoT_outer_first))))
}
if(i ==2){
  points(rep(x_outer,45),(108.3)/(as.numeric(table_MS$travel_time_HoT_outer_first,na.rm=T)),
         col=rgb(50,50,50,alpha=100,maxColorValue = 255),pch=16)
  text(x_outer,39,paste0("n = ",length(na.omit(table_MS$travel_time_HoT_outer_first))))
}
segments(x_outer,mean_outer_speed+sd_outer_speed,x_outer,mean_outer_speed-sd_outer_speed,col=rgb(t(col2rgb(pal_graph[5])),alpha=200,maxColorValue = 255),lwd=2)
points(x_outer,mean_outer_speed,col=rgb(t(col2rgb(pal_graph[5])),alpha=255,maxColorValue = 255),pch=16,cex=1.4  )

## HoT->SoBI
if(i ==1){
  points(rep(x_sobi,45),(741.5*1000)/(as.numeric(table_MS$travel_time_HoT_sobi_first,na.rm=T)*24*3600),
         col=rgb(50,50,50,alpha=100,maxColorValue = 255),pch=16)
  text(x_sobi-0.40,0.37,paste0("n = ",length(na.omit(table_MS$travel_time_HoT_sobi_first))))
}
if(i ==2){
  points(rep(x_sobi,45),(741.5)/(as.numeric(table_MS$travel_time_HoT_sobi_first,na.rm=T)),
         col=rgb(50,50,50,alpha=100,maxColorValue = 255),pch=16)
  text(x_sobi-0.40,30,paste0("n = ",length(na.omit(table_MS$travel_time_HoT_sobi_first))))
}
segments(x_sobi,mean_sobi_speed+sd_sobi_speed,x_sobi,mean_sobi_speed-sd_sobi_speed,col=rgb(t(col2rgb(pal_graph[6])),alpha=200,maxColorValue = 255),lwd=2)
points(x_sobi,mean_sobi_speed,col=rgb(t(col2rgb(pal_graph[6])),alpha=255,maxColorValue = 255),pch=16,cex=1.4  )
abline(h=0,lty=3,col="grey25")
box()

dev.off()
##_________________________________________

##_________________________________________
### Figure 3 ####
## Fonction to aproximate the tidal data as a line - there is a warning, its fine
AF <- approxfun(all_tide$Date_Time,all_tide$predictions.m.)

## for 21 smolts we can calculate a time of residence in the island complex (n=21)
## we plot the alternance day/night and we add the tide cycle (using water level as a proxy)
## and the time of entry and exit of each individual

#png("Figures/tide_sun_smolts.png",height=1200,width=1800,pointsize=20)
png("Figures/tide_sun_smolts_split.png",height=2400,width=1800,pointsize=20)



pal <- paletteer_d("PNWColors::Bay")[3:5]



par(mar=c(5,5.5,2,2))

all_tide_sub <- all_tide %>% filter( between(Date_Time, as.Date("2022-05-25"), as.Date("2022-06-05"))) 


par(mfrow=c(2,1))

all_tide_sub <- all_tide %>% filter( between(Date_Time, as.Date("2022-05-25"), as.Date("2022-05-29"))) 

plot(all_tide_sub$Date_Time,all_tide_sub$predictions.m.,type="l",
     xlab = "Date", ylab="Water Height (m)",cex.lab=1.3)

# subset May/june sunrise data
dat_sun_sub <- dat_sun %>% filter(between(Sunrise_POSIX, as.Date("2022-05-01"), as.Date("2022-07-01")))

polygon(x=c(dat_sun_sub$Nautical_twilight_start_POSIX[1],dat_sun_sub$Nautical_twilight_end_POSIX[61],dat_sun_sub$Nautical_twilight_end_POSIX[61],dat_sun_sub$Nautical_twilight_start_POSIX[1]),y=c(0,0,4,4),col="grey75")
for(i in 1:61){
  polygon(x=c(dat_sun_sub$Sunrise_POSIX[i],dat_sun_sub$Sunset_POSIX[i],dat_sun_sub$Sunset_POSIX[i],dat_sun_sub$Sunrise_POSIX[i]),y=c(0,0,4,4),col="grey95",border=NA)
  polygon(x=c(dat_sun_sub$Nautical_twilight_end_POSIX[i],dat_sun_sub$Nautical_twilight_start_POSIX[i+1],dat_sun_sub$Nautical_twilight_start_POSIX[i+1],dat_sun_sub$Nautical_twilight_end_POSIX[i]),y=c(0,0,4,4),col="grey55",border=NA)
}
points(all_tide$Date_Time,all_tide$predictions.m.,type="l",col="royalblue3",lwd=3)

for (i in 1:21){
  points(time_in_channel$min_time_Rx1[i], AF(time_in_channel$min_time_Rx1[i]),
         col=ifelse(time_in_channel$tot_time[i]<100,pal[1],
                    ifelse(time_in_channel$tot_time[i]<500,pal[2],pal[3])),pch=15)
  
  points(max(c(time_in_channel$max_time_R3a[i],time_in_channel$max_time_R3b[i]),na.rm=T), AF(time_in_channel$min_time_Rx1[i]),
         col=ifelse(time_in_channel$tot_time[i]<100,pal[1],
                    ifelse(time_in_channel$tot_time[i]<500,pal[2],pal[3])),pch=15)
  
  segments(time_in_channel$min_time_Rx1[i],AF(time_in_channel$min_time_Rx1[i]),
           max(c(time_in_channel$max_time_R3a[i],time_in_channel$max_time_R3b[i]),na.rm = T), AF(time_in_channel$min_time_Rx1[i]),
           col=ifelse(time_in_channel$tot_time[i]<100,pal[1],
                      ifelse(time_in_channel$tot_time[i]<480,pal[2],pal[3])))
  text(time_in_channel$min_time_Rx1[i]- 13000, AF(time_in_channel$min_time_Rx1[i])+0.02,
       time_in_channel$Tags_smolts[i])
}

all_tide_sub <- all_tide %>% filter( between(Date_Time, as.Date("2022-05-30"), as.Date("2022-06-05"))) 

plot(all_tide_sub$Date_Time,all_tide_sub$predictions.m.,type="l",
     xlab = "Date", ylab="Water Height (m)",cex.lab=1.3)

# subset May/june sunrise data
dat_sun_sub <- dat_sun %>% filter(between(Sunrise_POSIX, as.Date("2022-05-01"), as.Date("2022-07-01")))

polygon(x=c(dat_sun_sub$Nautical_twilight_start_POSIX[1],dat_sun_sub$Nautical_twilight_end_POSIX[61],dat_sun_sub$Nautical_twilight_end_POSIX[61],dat_sun_sub$Nautical_twilight_start_POSIX[1]),y=c(0,0,4,4),col="grey75")
for(i in 1:61){
  polygon(x=c(dat_sun_sub$Sunrise_POSIX[i],dat_sun_sub$Sunset_POSIX[i],dat_sun_sub$Sunset_POSIX[i],dat_sun_sub$Sunrise_POSIX[i]),y=c(0,0,4,4),col="grey95",border=NA)
  polygon(x=c(dat_sun_sub$Nautical_twilight_end_POSIX[i],dat_sun_sub$Nautical_twilight_start_POSIX[i+1],dat_sun_sub$Nautical_twilight_start_POSIX[i+1],dat_sun_sub$Nautical_twilight_end_POSIX[i]),y=c(0,0,4,4),col="grey55",border=NA)
}
points(all_tide$Date_Time,all_tide$predictions.m.,type="l",col="royalblue3",lwd=3)



for (i in 1:21){
  points(time_in_channel$min_time_Rx1[i], AF(time_in_channel$min_time_Rx1[i]),
         col=ifelse(time_in_channel$tot_time[i]<100,pal[1],
                    ifelse(time_in_channel$tot_time[i]<500,pal[2],pal[3])),pch=15)
  
  points(max(c(time_in_channel$max_time_R3a[i],time_in_channel$max_time_R3b[i]),na.rm=T), AF(time_in_channel$min_time_Rx1[i]),
         col=ifelse(time_in_channel$tot_time[i]<100,pal[1],
                    ifelse(time_in_channel$tot_time[i]<500,pal[2],pal[3])),pch=15)
  
  segments(time_in_channel$min_time_Rx1[i],AF(time_in_channel$min_time_Rx1[i]),
           max(c(time_in_channel$max_time_R3a[i],time_in_channel$max_time_R3b[i]),na.rm = T), AF(time_in_channel$min_time_Rx1[i]),
           col=ifelse(time_in_channel$tot_time[i]<100,pal[1],
                      ifelse(time_in_channel$tot_time[i]<480,pal[2],pal[3])))
  text(time_in_channel$min_time_Rx1[i]- 13000, AF(time_in_channel$min_time_Rx1[i])+0.02,
       time_in_channel$Tags_smolts[i])
}





pos <-legend("topright", 
             pch = c(15,15,15,NA),
             col=c("grey95","grey75","grey55","royalblue3",NA,NA,NA,NA),
             lty=c(NA,NA,NA,1,NA,NA,NA,NA),
             lwd=c(NA,NA,NA,2,NA,NA,NA,NA),
             pt.cex = c(2,2,2,1,NA,NA,NA,NA),
             legend=c("Day (sunrise-sunset)","Nautical Twilight (start-end)","Night","Water height",
                      expression("Time spent between "* D[3] *" and " * D[10:11]),
                      "<100 min", "between 100 min and 8 hours", ">= 8 hours"),
             box.lwd = 0,box.col = "grey45",bg = "white",cex=1.2)

## Being cute and creating a legend that matches the segments for duration
points(x=rep(pos$text$x, times=2) - c(rep(20000,8),rep(10000,8)), 
       y=rep(pos$text$y, times=2), 
       cex=1.3,
       pch=rep(c(NA,NA,NA,NA,NA,15,15,15), times=2), col=rep(c(NA,NA,NA,NA,NA,pal[1],pal[2],pal[3]), times=2))

segments(x0=pos$text$x[6:8]-20000,
         y0=pos$text$y[6:8],
         x1=pos$text$x[6:8]-10000,
         y1=pos$text$y[6:8],
         col=c(pal[1],pal[2],pal[3])
)
box()
dev.off()
##_________________________________________






##_________________________________________
### Figure 4-5 ####

attach.jags(jags_M_5)

p_detect_sum <- read.csv("Output/M5_p_detect.csv")
p_travel_sum <- read.csv("Output/M5_p_travel.csv")

mean_channel <- read.csv("Output/M5_mean_channel.csv")
sd_channel <- read.csv("Output/M5_sd_channel.csv")

prior_p_d <-rbeta(1000000,1,1)
prior_p_d_sum <- data.frame("Prior",mean(prior_p_d), sd(prior_p_d),
                            quantile(prior_p_d,probs=c(0.025)),
                            quantile(prior_p_d,probs=c(0.25)),
                            quantile(prior_p_d,probs=c(0.50)),
                            quantile(prior_p_d,probs=c(0.75)),
                            quantile(prior_p_d,probs=c(0.975)),
                            NA,NA )
names(prior_p_d_sum) <- names(p_detect_sum)

#### Probability of detection ####
p_detect_sum <- rbind(p_detect_sum,
                      prior_p_d_sum)

p_detect_sum$X <- factor(p_detect_sum$X,
                                levels = c("p_RP1","p_MM","p_Rx1","p_R1a","p_R1b","p_R2a","p_Rx2b","p_Rx2a","p_R2b","p_R3a","p_R3b","p_Rest","Prior"))

#names_rec <-c("RP1","MM","Rx1","R1a","R1b","R2a","Rx2b","Rx2a","R2b","R3a","R3b","Rest","Prior")


names_rec_expr <- c(
  lapply(1:11, function(i) bquote(D[.(i)])),
  expression(D[12] * " / HoT"),
  expression("Prior")
)

col_p_det <- c(rep("black",12),"red")
names(col_p_det) <- levels(p_detect_sum$X)

p_detect_sum <- p_detect_sum %>% mutate(X_num = as.numeric(X))

polygon_coord <- data.frame(zone= c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4)    ),
                           x= c(3.5,5.5,5.5,3.5,5.5,7.5,7.5,5.5,7.5,8.5,8.5,7.5,8.5,9.5,9.5,8.5,9.5,11.5,11.5,9.5),
                           y= c(rep(c(0,0,1,1),5)    )  )
col_polygon <- c(           rgb(166,189,219,maxColorValue = 255,alpha=170),
                            rgb(116,169,207,maxColorValue = 255,alpha=170),
                            rgb(54,144,192,maxColorValue = 255,alpha=170),
                            rgb(5,112,179,maxColorValue = 255,alpha=170),
                            rgb(3,78,123,maxColorValue = 255,alpha=170)
)

plot_p_detect <- ggplot(p_detect_sum,aes(x=X,y=X50.,col=X))+
  annotate(geom="rect",xmin = 12.5, xmax = 13.5, ymin = 0, ymax = 1,
           col=NA, fill = alpha("grey85", .6))+
  geom_polygon(data = polygon_coord, aes(x=x,y=y,col=factor(zone),fill=factor(zone)),col=NA)+
  scale_fill_manual(values = col_polygon, name="Zones"  )+# c(1:5))+
  geom_vline(xintercept = 12.5,linetype="dashed")+
  geom_point(cex=3)+
  geom_errorbar(aes(x=X,ymin = X2.5., ymax = X97.5.),linewidth = 0.5,width=0)+
  geom_errorbar(aes(x=X,ymin = X25., ymax = X75.),linewidth = 1.5,width=0)+
  ##ggtitle("Probability of detection") +
  xlab("Receivers")+
  ylab("")+
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  scale_x_discrete(labels=names_rec_expr ,expand=c(0,0.4))+
  scale_colour_manual(name = "grp",values =col_p_det,guide="none")+
  theme_bw()+
  theme( panel.grid.major.x = element_blank()
         )  

ggsave("Figures/MS_Fig_p_detect.png",plot_p_detect,width=1200,height = 500,units = "px",dpi=120)

#### Probability of travel ####
p_travel_sum <- rbind(p_travel_sum,
                      prior_p_d_sum)

## We correct the ordering (names were from a previous version of the model)
p_travel_sum$X <- factor(p_travel_sum$X,
                         levels = c("p_Z1a","p_Z2a","p_Z2b","p_dodge","p_Z3b","p_Z5b_Z4a","p_Z5b_Z4ab", "Prior"  ))

names_trav <- c(bquote(italic(pt["1,1"])),
                bquote(italic(pt["2,1"])),
                bquote(italic(pt["2,3"])),
                bquote(italic(pt["3,2"])),
                bquote(italic(pt["4,3"])),
                bquote(italic(pt["5,2a"])),
                bquote(italic(pt["5,2b"])),
                "prior"
                )
  
polygon_coord2 <- data.frame(zone= c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4)    ),
                            x= c(0.5,1.5,1.5,0.5,1.5,3.5,3.5,1.5,3.5,4.5,4.5,3.5,4.5,5.5,5.5,4.5,5.5,7.5,7.5,5.5),
                            y= c(rep(c(0,0,1,1),5)    )  )
col_polygon2 <- c(           rgb(166,189,219,maxColorValue = 255,alpha=170),
                             rgb(116,169,207,maxColorValue = 255,alpha=170),
                             rgb(54,144,192,maxColorValue = 255,alpha=170),
                             rgb(5,112,179,maxColorValue = 255,alpha=170),
                             rgb(3,78,123,maxColorValue = 255,alpha=170)
)

col_p_trav  <- c(rep("black",7),"red")
names(col_p_trav) <- levels(p_travel_sum$X)

plot_p_travel <- ggplot(p_travel_sum,aes(x=X,y=X50.,col=X))+
  annotate(geom="rect",xmin = 7.5, xmax = 8.5, ymin = 0, ymax = 1,
           col=NA, fill = alpha("grey85", .6))+
  geom_polygon(data = polygon_coord2, aes(x=x,y=y,col=factor(zone),fill=factor(zone)),col=NA)+
  scale_fill_manual(values = col_polygon2, name="Zones"  )+# c(1:5))+
  
  geom_vline(xintercept = 7.5,linetype="dashed")+
  geom_point(cex=3)+
  geom_errorbar(aes(x=X,ymin = X2.5., ymax = X97.5.),linewidth = 0.5,width=0)+
  geom_errorbar(aes(x=X,ymin = X25., ymax = X75.),linewidth = 1.5,width=0)+
  ##ggtitle("Probability of travel") +
  xlab("Probability of travel towards")+
  ylab("")+
  
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  scale_x_discrete(labels=names_trav , #TeX(names_trav$x_lab[1:8]), #
                   expand=c(0,0.4))+

  scale_colour_manual(name = "grp",values =col_p_trav,guide="none")+
  theme_bw()+
  theme( panel.grid.major.x = element_blank()
  ) 

ggsave("Figures/MS_Fig_p_travel.png",plot_p_travel,width=900,height = 500,units = "px",dpi=120)

ggarrange(plot_p_detect,plot_p_travel,nrow=2)
##_________________________________________


##_________________________________________
### Figure 6 - Probability to go through an RST ####
#### Calculate the number of smolts in an RST channel ####
Z2_a_names <- character() # Moses
Z3_ab_dodge_names <- character() # no RST
Z4_ab_names <- character() # no RST
Z4_b_names <- character() # Butters

fish_ID <- character()

for (i in 1:data_ac$n_fish){
  fish_ID[i] <- paste("ID_",i,sep="")
  Z2_a_names[i] <- paste("Z2_a[",i,"]",sep="")
  Z3_ab_dodge_names[i] <- paste("Z3_ab_dodge[",i,"]",sep="")  
  Z4_ab_names[i] <- paste("Z4_ab[",i,"]",sep="")
  Z4_b_names[i] <- paste("Z4_b[",i,"]",sep="")
}

Z2_a_sum <- as.data.frame(jags_M_5$BUGSoutput$summary[row.names(jags_M_5$BUGSoutput$summary)%in%Z2_a_names,])
Z3_ab_dodge_sum <- as.data.frame(jags_M_5$BUGSoutput$summary[row.names(jags_M_5$BUGSoutput$summary)%in%Z3_ab_dodge_names,])
Z4_ab_sum <- as.data.frame(jags_M_5$BUGSoutput$summary[row.names(jags_M_5$BUGSoutput$summary)%in%Z4_ab_names,])
Z4_b_sum <- as.data.frame(jags_M_5$BUGSoutput$summary[row.names(jags_M_5$BUGSoutput$summary)%in%Z4_b_names,])

p_RST_channel <- data.frame(fish_ID = fish_ID,
                            fish_ID2 = df_jags$smolt_ID,
                            Z2_a =  Z2_a_sum$mean,
                            Z3_ab_dodge =  Z3_ab_dodge_sum$mean,
                            Z4_ab = Z4_ab_sum$mean,
                            Z4_b = Z4_b_sum$mean)

p_RST_channel <- p_RST_channel %>%  mutate(
  p_moses = Z2_a ,
  p_no_RST = Z3_ab_dodge+Z4_ab,
  p_butters = Z4_b) %>%
  select(fish_ID2,p_moses,p_no_RST,p_butters) %>%
  arrange(across(.cols=c( "p_butters","p_moses"))) %>% 
  rowid_to_column() 

df_bar <- p_RST_channel %>%
  select(rowid,fish_ID2,p_moses,p_no_RST,p_butters) %>%
  pivot_longer(!c(rowid,fish_ID2),names_to = "channels",values_to = "prop")

p_barplot <- ggplot(df_bar, aes(fill=channels, y=prop, x=reorder(fish_ID2,-rowid) )) + 
  geom_bar(position="fill", stat="identity")  +
  geom_text(aes(label = ifelse(after_stat(round(df_bar$prop*100))!=0,after_stat(round(df_bar$prop*100)),"" ) ),
            colour = "white",
            position = position_fill(vjust = 0.5)) +
  scale_y_continuous(labels = percent,limits = c(0,1), expand = c(0, 0)) +
  scale_x_discrete(expand=c(0,0))+
  scale_fill_manual(values=c(p_butters = "steelblue4",
                             p_moses = "skyblue2",
                             p_no_RST ="grey65"),
                    name="Channel",labels = c("Butters", "Moses", "no RST")
                    ) +
  xlab("Smolt ID")+
  ylab("")+
  theme_bw(base_size = 14) +
  theme( panel.grid.major.x = element_blank(),
         axis.text.x = element_text(angle=90))

ggsave(p_barplot,filename = "Figures/MS_Fig_barplot_channel_loc.png",width=1400,height=800,units = "px",dpi=90)  

##### Summary histogram of probabilities to be in a channel with an RST or not
par(mfrow=c(1,3),mar=c(2,2,2,2))
hist(p_RST_channel$p_moses,breaks=20,main="Moses")
hist(p_RST_channel$p_butters,breaks=20, main="Butters")
hist(p_RST_channel$p_no_RST,breaks=20, main="no RST")

write.csv(p_RST_channel,"Output/M5_p_RST_channel.csv",row.names = F)
##_________________________________________
