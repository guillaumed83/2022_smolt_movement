library(data.table)
library(readxl)
library(tidyverse)
library(png)
library(lubridate)
library(xtable)
library(viridis)
library(baffle) ## rounded corner rectangle


## Thank you DFO for not letting me do this on my work computer !
# install.packages("remotes")
# library(remotes) 
# install_github('ocean-tracking-network/glatos', build_vignettes = TRUE)
# library(glatos)

#### Data processing ####
par_default <- par()


## Reading the summary file to get the transmitter names
Tags_smolts_ID <- sort(c(
  "61836","61844","61845","61846","61847","61835","61838","61837","61839","61840",
  "61841","61842","61907","61908","61909","61691","61690","61692","61693","61834",
  "61843","61898","61906","61899","61900","61901","61902","61903","61904","61905",
  "61860","61859","61870","61861","61848","61849","61850","61851","61852","61853",
  "61854","61855","61856","61857","61858"))



## Biological Characteristics files provided summer/fall 2023 by CAG
## updated with release times on 26-03-2024 by BC
dat_2022_BC <- read_xlsx("Data/2023_exp/Smolt tagging ASF 2022.xlsx")

## ASF Receivers detection summary
ASF_det <- read.csv("Data/ASF_detection/Rest detection history for_Guillaume.csv")

## Conversion date and time of end surgery into posix format for easy time calculations
## not sure what format the time was entered in but its no accurate so we have to extract only the characters corresponding to the time

n_last <- 8
time_surgery <- as.character(dat_2022_BC$`TIME OUT Surgery`)
time_surgery <- substr(time_surgery, nchar(time_surgery) - n_last + 1, nchar(time_surgery))
time_release <- as.character(dat_2022_BC$`release time (NB)`)
time_release <- substr(time_release, nchar(time_release) - n_last + 1, nchar(time_release)) 

dat_2022_BC$end_surgery_time_posix <- as.POSIXct(paste(dat_2022_BC$DATE, time_surgery ), format = "%Y-%m-%d %H:%M:%S",tz="America/Curacao")
dat_2022_BC$release_time_posix <- as.POSIXct(paste(dat_2022_BC$DATE, time_release ), format = "%Y-%m-%d %H:%M:%S",tz="America/Curacao")

## Create a column with actual TAG_ID
dat_2022_BC$Transmitter <- paste0("A69-1602-",dat_2022_BC$`TAG #`)

dat_BC_ASF<-dat_2022_BC %>% left_join(ASF_det)



mean(dat_2022_BC$`FORK LENGTH (mm)`)
round(sd(dat_2022_BC$`FORK LENGTH (mm)`),1)

mean(dat_2022_BC$`WEIGHT (G)`)
round(sd(dat_2022_BC$`WEIGHT (G)`),1)




### Reading tidal data
files.tide <- list.files("./Data/Tide", full.names = T, recursive = T, pattern = ".*.csv")
temp <- lapply(files.tide, function(f){
  dat <- read.csv(f) #fread(f,sep="",header = T,strip.white = F)
}
)
all_tide<-rbindlist(temp)
## Remove the last 4 characters from the string
names(all_tide)[1] <- "Date_Time"
all_tide <- all_tide %>% mutate(Date_Time = substring(Date_Time,1,nchar(Date_Time)-4) ) 
all_tide$Date_Time <- ymd_hm(all_tide$Date_Time,tz="America/Curacao")
tz(all_tide$Date_Time)

## NRC data
## We need to to a bit of formatting to get a convenient posix format

dat_sun <- read_xlsx("./Data/Sun/NRC_sunset_sunrise.xlsx")

dat_sun <- dat_sun %>% mutate(Month_num = case_when(Month == "Jan" ~ "01",
                                                    Month == "Feb" ~ "02",
                                                    Month == "Mar" ~ "03",
                                                    Month == "Apr" ~ "04",
                                                    Month == "May" ~ "05",
                                                    Month == "Jun" ~ "06",
                                                    Month == "Jul" ~ "07",
                                                    Month == "Aug" ~ "08",
                                                    Month == "Sep" ~ "09",
                                                    Month == "Oct" ~ "10",
                                                    Month == "Nov" ~ "11",
                                                    Month == "Dec" ~ "12",
                                                    .default = NA
                                                    )) %>%
                       mutate(Day = case_when(nchar(as.character(Day))==1 ~ paste0("0",as.character(Day)),
                                              .default = as.character(Day)) ) %>%
                       mutate(Date = paste(Year,Month_num,Day,sep="-")) %>%  
  
                       mutate(Nautical_twilight_start = substring(Nautical_twilight_start,nchar(Nautical_twilight_start)+1,nchar(Nautical_twilight_start)+8),
                              Civil_twilight_start = substring(Civil_twilight_start,nchar(Civil_twilight_start)+1,nchar(Civil_twilight_start)+8),
                              Sunrise = substring(Sunrise,nchar(Sunrise)+1,nchar(Sunrise)+8),
                              Local_noon = substring(Local_noon,nchar(Local_noon)+1,nchar(Local_noon)+8),                      
                              Sunset = substring(Sunset,nchar(Sunset)+1,nchar(Sunset)+8),
                              Civil_twilight_end = substring(Civil_twilight_end,nchar(Civil_twilight_end)+1,nchar(Civil_twilight_end)+8),
                              Nautical_twilight_end = substring(Nautical_twilight_end,nchar(Nautical_twilight_end)+1,nchar(Nautical_twilight_end)+8)) %>%
                      mutate( Nautical_twilight_start_POSIX = ymd_hms(paste0(Date," ", Nautical_twilight_start),tz="America/Curacao"),
                              Sunrise_POSIX = ymd_hms(paste0(Date," ", Sunrise),tz="America/Curacao"),
                              Sunset_POSIX = ymd_hms(paste0(Date," ", Sunset),tz="America/Curacao"),
                              Nautical_twilight_end_POSIX = ymd_hms(paste0(Date," ", Nautical_twilight_end),tz="America/Curacao")
                              )





### Raw data - couldnt install Rtools and Glatos on my work computer so could convert the files from vrl to csv
### did it on personnal computer and reimported csv files on dfo computer

### Reading asf detection data
files.tide <- list.files("./Data/ASF_detection/ASF_2022/csv", full.names = T, recursive = T, pattern = ".*.csv")
temp <- lapply(files.tide, function(f){
  dat <- read.csv(f) #fread(f,sep="",header = T,strip.white = F)
}
)
all_asf_det <-rbindlist(temp)
names(all_asf_det)[1] <- "Date_Time"
## Pings were recorded in UTC
all_asf_det$Date_Time <- ymd_hms(all_asf_det$Date_Time,tz="UTC")#"America/Curacao")#

## we need to Convert to AST
all_asf_det$Date_Time_AST <- with_tz(all_asf_det$Date_Time, "America/Curacao")

all_asf_det <- all_asf_det %>% mutate(Tags_smolts = sub(".*-*-", "", Transmitter) ) %>%
  filter(Tags_smolts %in% Tags_smolts_ID )

## Read the ASF receivers file
## Contains deployment info + coordinates
ASF_rec <- read_xlsx("Data/ASF_detection/ASF_2022/2022 deployment For Guil.xlsx")

## Looking at the coordinates of individual Receivers
unique_coord <- ASF_rec %>% distinct(Chaleur_Dist,Region,Station_Name,DEPLOY_LAT,DEPLOY_LONG)
unique(ASF_rec$Station_Name)


## Pulling all receiver files provided post 20224 experiments and merging them in one data frame

files <- list.files("./Data/DFO - 15 receiver files", full.names = T, recursive = T, pattern = ".*.csv")
temp <- lapply(files, function(f){
  dat <- read.csv(f) #fread(f,sep="",header = T,strip.white = F)
        } )

all_pings<-rbindlist(temp)
names(all_pings)[1] <- "Date_Time"
## Pings were recorded in UTC
all_pings$Date_Time <- ymd_hms(all_pings$Date_Time,tz="UTC")#"America/Curacao")#

## we need to Convert to AST
all_pings$Date_Time_AST <- with_tz(all_pings$Date_Time, "America/Curacao")

all_pings <- all_pings %>% mutate(Tags_smolts = sub(".*-*-", "", Transmitter) ) %>%
                           filter(Tags_smolts %in% Tags_smolts_ID )

#### Alright here we are going too merge the 2 detections files
#### We have to be careful because the ASF owns some of the receivers that were part of the 2022 experiment
#### We are going to assume that the have the authorative files and therefore drop these data 
## Red Pine: VR2Tx-487957
## MM1: VR2W-135789
## MM2: VR2Tx-487953
## REST4: VR2W-135787
## REST5: VR2Tx-135784
## REST6: VR2W-135785  


## Receivers in both files
rec_dupl <- c("VR2Tx-487957","VR2W-135789","VR2Tx-487953","VR2W-135787","VR2Tx-135784","VR2W-135785")

all_pings2 <- all_pings %>% filter(!Receiver %in% rec_dupl)


### Merging the 2 files

all_pings <- rbind(all_asf_det,all_pings2)

### Summary of all ASFreceivers that detected a fish from the 2022 experiment
sort(unique(all_asf_det$Station.Name))

# "RP1"
all_MM <- c("MM1","MM2") 
HoT <- c("REST4","REST5","REST6")
Inner_Bay <- c("DAL1","DAL2","DAL3")
Outer_Bay <- c("BDC01","BDC02","BDC03","BDC05","BDC06","BDC07","BDC08","BDC09","BDC10","BDC12","BDC13","BDC17","BDC18","BDC19","BDC20",
               "BDC21","BDC22","BDC24","BDC 25","BDC26","BDC27","BDC28","BDC29","BDC30","BDC31","BDC32","BDC33","BDC34","BDC35","BDC36")
SoBI <- c("SB03","SB08","SB10","SB11","SB13","SB17","SB20","SB22","SB23","SB25","SB26","SB27" )
SoBI_W <- c("SBW01","SBW02","SBW04","SBW05","SBW-08") 
SoBI_N <- c("SOBI N04","SOBI N05","SOBI N13","SOBI N14","SOBI N15","SOBI N17","SOBI N18","SOBI N19","SOBI N20","SOBI N21","SOBI N22","SOBI N23")     
   



all_pings$Station.Name <- ordered(all_pings$Station.Name, 
                                  levels = rev(c("RP1",all_MM,"Rx1","R1a","R1b","R2a","Rx2b","Rx2a","R2b","R3a","R3b",HoT,Inner_Bay,Outer_Bay,SoBI,SoBI_W,SoBI_N
                                                 )))

all_pings <- all_pings %>% mutate(Station_array = case_when(Station.Name %in% all_MM ~ "all_MM",
                                                            Station.Name %in% HoT ~ "HoT",
                                                            Station.Name %in% Inner_Bay ~ "Inner_Bay",
                                                            Station.Name %in% Outer_Bay ~ "Outer_Bay",
                                                            Station.Name %in% SoBI ~ "SoBI",
                                                            Station.Name %in% SoBI_W ~ "SoBI_W",
                                                            Station.Name %in% SoBI_N ~ "SoBI_N",
                                                            .default = Station.Name),
                                  Station_array2 = case_when(Station_array %in% c("SoBI","SoBI_W","SoBI_N")~"all_SoBI",
                                                             .default = Station_array)
                                  )

all_pings$Station_array <- ordered(all_pings$Station_array, 
                                  levels = rev(c("RP1","all_MM","Rx1","R1a","R1b","R2a","Rx2b","Rx2a","R2b","R3a","R3b","HoT","Inner_Bay","Outer_Bay","SoBI","SoBI_W","SoBI_N"
                                  )))


all_pings_sum <- all_pings %>% group_by(Tags_smolts, Station_array) %>% summarise(n_detect = n()) %>%
                pivot_wider(names_from = Station_array, values_from = n_detect) %>% 
                select(Tags_smolts,RP1,all_MM,Rx1,R1a,R1b,R2a,Rx2a,Rx2b,R2b,R3a,R3b,HoT,Inner_Bay,Outer_Bay,SoBI,SoBI_W,SoBI_N)


all_pings_sum2 <- all_pings_sum %>% mutate(all_SoBI = sum(c(SoBI,SoBI_W,SoBI_N),na.rm=T)) %>% 
  select(Tags_smolts,RP1, all_MM, Rx1,R1a,R1b,R2a,Rx2a,Rx2b,R2b,R3a,R3b,HoT,Inner_Bay,Outer_Bay,all_SoBI) %>%
  mutate(tot_ping = sum(c(RP1, all_MM, Rx1,R1a,R1b,R2a,Rx2a,Rx2b,R2b,R3a,R3b,HoT,Inner_Bay,Outer_Bay,all_SoBI),na.rm=T)) %>% replace(is.na(.), 0)



dat_BC <- dat_2022_BC %>% select(`TAG #`,`FORK LENGTH (mm)`,`WEIGHT (G)`) %>% mutate(`TAG #` = as.character(`TAG #`))

names(dat_BC) <- c("Tags_smolts","FL","Weight")
all_pings_sum2 <- all_pings_sum2 %>% left_join(dat_BC) %>%
  select(Tags_smolts,FL,Weight,RP1, all_MM, Rx1,R1a,R1b,R2a,Rx2a,Rx2b,R2b,R3a,R3b,HoT,Inner_Bay,Outer_Bay,all_SoBI,tot_ping)

write.csv(all_pings_sum2,"Output/summary_n_detect_array.csv")


## mean and sd detections
all_pings_sum3 <- all_pings_sum2 
all_pings_sum3[all_pings_sum3==0]<-NA
round(sapply(all_pings_sum3[4:19],mean,na.rm=T),1)
round(sapply(all_pings_sum3[4:19],sd,na.rm=T),1)


### Quick check correlation between FL and number of Pings

plot(all_pings_sum2$FL,all_pings_sum2$tot_ping,pch=16)
plot(all_pings_sum2$FL,all_pings_sum2$all_MM,pch=16)

plot(all_pings_sum2$FL,all_pings_sum2$Rx1,pch=16)
plot(all_pings_sum2$FL,all_pings_sum2$R1a,pch=16)
plot(all_pings_sum2$FL,all_pings_sum2$R1b,pch=16)

plot(all_pings_sum2$FL,all_pings_sum2$Rx2b,pch=16)
plot(all_pings_sum2$FL,all_pings_sum2$R3a,pch=16)
plot(all_pings_sum2$FL,all_pings_sum2$R3b,pch=16)

plot(all_pings_sum2$FL,all_pings_sum2$HoT,pch=16)

### Average migration time to the network of islands (Rx1)
### And outside of Campbellton (Rest array)

## return the Tag ID pinged at Rx1
Tags_Rx1 <- all_pings_sum2 %>% filter (Rx1 > 0) %>% .$Tags_smolts
Tags_Rest <- all_pings_sum2 %>% filter (HoT > 0 ) %>% .$Tags_smolts
Tags_Inner <- all_pings_sum2 %>% filter (Inner_Bay > 0 ) %>% .$Tags_smolts
Tags_Outer <- all_pings_sum2 %>% filter (Outer_Bay > 0 ) %>% .$Tags_smolts
Tags_SoBI <- all_pings_sum2 %>% filter (all_SoBI > 0 ) %>% .$Tags_smolts 


time_1st_Rx1 <- all_pings %>% filter(Station.Name=="Rx1",Tags_smolts %in% Tags_Rx1) %>%
  group_by(Tags_smolts) %>% summarise(first_Rx1 = min(Date_Time_AST))
time_last_Rx1 <- all_pings %>% filter(Station.Name=="Rx1",Tags_smolts %in% Tags_Rx1) %>%
  group_by(Tags_smolts) %>% summarise(last_Rx1 = min(Date_Time_AST))

time_1st_Rest <- all_pings %>% filter(Station.Name %in% c("REST4","REST5","REST6") ,Tags_smolts %in% Tags_Rest) %>%
  group_by(Tags_smolts) %>% summarise(first_Rest = min(Date_Time_AST))
time_last_Rest <- all_pings %>% filter(Station.Name %in% c("REST4","REST5","REST6") ,Tags_smolts %in% Tags_Rest) %>%
  group_by(Tags_smolts) %>% summarise(last_Rest = max(Date_Time_AST))

Time_1st_inner <- all_pings %>% filter(Station_array=="Inner_Bay",Tags_smolts %in% Tags_Inner) %>%
  group_by(Tags_smolts) %>% summarise(first_inner = min(Date_Time_AST))
Time_last_inner <- all_pings %>% filter(Station_array=="Inner_Bay",Tags_smolts %in% Tags_Inner) %>%
  group_by(Tags_smolts) %>% summarise(last_inner = min(Date_Time_AST))

Time_1st_outer <- all_pings %>% filter(Station_array=="Outer_Bay",Tags_smolts %in% Tags_Outer) %>%
  group_by(Tags_smolts) %>% summarise(first_outer = min(Date_Time_AST))
Time_last_outer <- all_pings %>% filter(Station_array=="Outer_Bay",Tags_smolts %in% Tags_Outer) %>%
  group_by(Tags_smolts) %>% summarise(last_outer = min(Date_Time_AST))
  
Time_1st_sobi <- all_pings %>% filter(Station_array2=="all_SoBI",Tags_smolts %in% Tags_SoBI) %>%
  group_by(Tags_smolts) %>% summarise(first_sobi = min(Date_Time_AST))
Time_last_sobi <- all_pings %>% filter(Station_array2=="all_SoBI",Tags_smolts %in% Tags_SoBI) %>%
  group_by(Tags_smolts) %>% summarise(last_sobi = min(Date_Time_AST))


all_times <- dat_2022_BC %>% select(`TAG #`,`FORK LENGTH (mm)`,`WEIGHT (G)`,release_time_posix) %>% mutate(`TAG #` = as.character(`TAG #`)) %>%
  rename(Tags_smolts =`TAG #` ) %>%  left_join(time_1st_Rx1) %>% left_join(time_last_Rx1 ) %>%
                                     left_join(time_1st_Rest) %>% left_join(time_last_Rest) %>%
                                     left_join(Time_1st_inner) %>% left_join(Time_last_inner) %>%
                                     left_join(Time_1st_outer) %>% left_join(Time_last_outer) %>%
                                     left_join(Time_1st_sobi) %>% left_join(Time_last_sobi) %>%
  mutate(travel_time_to_Rx1 = as.numeric(difftime(first_Rx1, release_time_posix, units = "days")), #round(first_Rx1-release_time_posix,2), 
         travel_time_to_Rx1_last = as.numeric(difftime(last_Rx1, release_time_posix, units = "days")), #round(last_Rx1-release_time_posix,2),
         travel_time_to_HoT_first = as.numeric(difftime(first_Rest, release_time_posix, units = "days")), #round(first_Rest-release_time_posix,2),
         travel_time_to_HoT_last = as.numeric(difftime(last_Rest, release_time_posix, units = "days")), #round(last_Rest-release_time_posix,2),
         travel_time_HoT_inner_first = as.numeric(difftime(first_inner, last_Rest, units = "days")), #round(first_inner-last_Rest,2),
         travel_time_HoT_outer_first = as.numeric(difftime(first_outer, last_Rest, units = "days")), #round(first_outer-last_Rest,2),
         travel_time_HoT_sobi_first = as.numeric(difftime(first_sobi, last_Rest, units = "days")) #round(first_sobi-last_Rest,2)
         )
temp <- all_times %>% filter(!is.na(first_sobi))


temp2 <- temp %>% select(Tags_smolts,`FORK LENGTH (mm)`,travel_time_to_HoT_last,travel_time_HoT_sobi_first) %>% arrange(`FORK LENGTH (mm)`)
par(mfrow=c(1,1))
barplot(t(as.matrix(temp2[,3:4])),
        names.arg=temp2$Tags_smolts,
        main="Total migration time from Release to SoBI arrays",
        xlab= "smolt IDs",
        ylan= "Number of Days",
        ylim=c(0,60),
        las=2,
        col=c("lightblue","deepskyblue4"),
        legend.text = c("Days from release to last HoT ping",
                        "Days from HoT to SoBI")
        
        )
box()          

table_MS <- all_times %>% 
  select(Tags_smolts,`FORK LENGTH (mm)`,`WEIGHT (G)`,travel_time_to_Rx1,travel_time_to_HoT_first,travel_time_to_HoT_last,travel_time_HoT_inner_first,
         travel_time_HoT_outer_first,travel_time_HoT_sobi_first) %>% arrange(Tags_smolts)
write.csv(table_MS,"output/BCsum_time.csv")


## Summary travelling time
round(sapply(table_MS[4:9],mean,na.rm=T ),2)
round(sapply(table_MS[4:9],sd,na.rm=T ),2)


round(sapply(table_MS[4:9],mean,na.rm=T ),1)


### Average travel times calculated for MS 


### Speed release -> Rx1
mean(table_MS$travel_time_to_Rx1,na.rm=T)
sd(table_MS$travel_time_to_Rx1,na.rm=T)
##speed km/h
mean(101/(as.numeric(table_MS$travel_time_to_Rx1)*24),na.rm=T)
## in m/s
## mean 
mean((101*1000)/(as.numeric(table_MS$travel_time_to_Rx1)*24*3600),na.rm=T)
## sd
sd((101*1000)/(as.numeric(table_MS$travel_time_to_Rx1)*24*3600),na.rm=T)

## in bodylength/s
## mean 
mean((101*1000)/(as.numeric(table_MS$travel_time_to_Rx1)*24*3600)/(table_MS$`FORK LENGTH (mm)`*0.001),na.rm=T)
## sd
sd((101*1000)/(as.numeric(table_MS$travel_time_to_Rx1)*24*3600),na.rm=T)




### Speed release -> HoT (first)
##speed km/h
mean(116.7/(as.numeric(table_MS$travel_time_to_HoT_first)*24),na.rm=T)
## in m/s
## mean 
mean((116.7*1000)/(as.numeric(table_MS$travel_time_to_HoT_first)*24*3600),na.rm=T)
## sd
sd((116.7*1000)/(as.numeric(table_MS$travel_time_to_HoT_first)*24*3600),na.rm=T)

### Speed release -> HoT (last)
##speed km/h
mean(116.7/(as.numeric(table_MS$travel_time_to_HoT_last)*24),na.rm=T)
## in m/s
## mean 
mean((116.7*1000)/(as.numeric(table_MS$travel_time_to_HoT_last)*24*3600),na.rm=T)
## sd
sd((116.7*1000)/(as.numeric(table_MS$travel_time_to_HoT_last)*24*3600),na.rm=T)




### Speed HoT -> Inner Bay
mean(27.8/as.numeric(table_MS$travel_time_HoT_inner_first)/24,na.rm=T)
## in m/s
## mean
mean((27.8*1000)/(as.numeric(table_MS$travel_time_HoT_inner_first)*24*3600),na.rm=T)
## sd
sd((27.8*1000)/(as.numeric(table_MS$travel_time_HoT_inner_first)*24*3600),na.rm=T)



### Speed HoT -> Outer Bay
mean(108.3/as.numeric(table_MS$travel_time_HoT_outer_first)/24,na.rm=T)
## in m/s
## mean
mean((108.3*1000)/(as.numeric(table_MS$travel_time_HoT_outer_first)*24*3600),na.rm=T)
## sd
sd((108.3*1000)/(as.numeric(table_MS$travel_time_HoT_outer_first)*24*3600),na.rm=T)


### Speed HoT -> SoBI
mean(741.5/as.numeric(table_MS$travel_time_HoT_sobi_first)/24,na.rm=T)
## in m/s
## mean
mean((741.5*1000)/(as.numeric(table_MS$travel_time_HoT_sobi_first)*24*3600),na.rm=T)
## sd
sd((741.5*1000)/(as.numeric(table_MS$travel_time_HoT_sobi_first)*24*3600),na.rm=T)



ggplot(table_MS,aes(x=`FORK LENGTH (mm)`,y=travel_time_to_Rx1))+
  geom_point()

mean(table_MS$travel_time_to_HoT_first,na.rm=T)
sd(table_MS$travel_time_to_HoT_first,na.rm=T)

mean(table_MS$travel_time_to_HoT_last,na.rm=T)
sd(table_MS$travel_time_to_HoT_last,na.rm=T)
min(table_MS$travel_time_to_HoT_last,na.rm=T)
max(table_MS$travel_time_to_HoT_last,na.rm=T)


117.36/as.numeric(mean(table_MS$travel_time_to_HoT_first,na.rm=T))/24
## in m/s
(117.36*1000)/as.numeric(mean(table_MS$travel_time_to_HoT_first,na.rm=T))/24/3600

117.36/as.numeric(median(table_MS$travel_time_to_HoT_first,na.rm=T))/24

mean(table_MS$travel_time_to_HoT_last,na.rm=T)
sd(table_MS$travel_time_to_HoT_last,na.rm=T)
117.36/as.numeric(mean(table_MS$travel_time_to_HoT_last,na.rm=T))/24
## in m/s
(117.36*1000)/as.numeric(mean(table_MS$travel_time_to_HoT_last,na.rm=T))/24/3600

117.36/as.numeric(median(table_MS$travel_time_to_HoT_last,na.rm=T))/24



ggplot(table_MS,aes(x=`FORK LENGTH (mm)`,y=travel_time_to_HoT_first))+
  geom_point()
ggplot(table_MS,aes(y=`FORK LENGTH (mm)`,x=travel_time_to_HoT_last))+
  geom_point()
summary(lm(`FORK LENGTH (mm)`~travel_time_to_HoT_first,table_MS))

all_pings_sum2_long <- all_pings_sum2 %>% select(-c(FL,Weight,tot_ping)) %>% pivot_longer(!Tags_smolts ,names_to = "receivers") #%>% mutate(log_v =log(value+0.001)) 
all_pings_sum2_long$receivers <- ordered(all_pings_sum2_long$receivers , 
                                  levels = c("RP1","all_MM","Rx1","R1a","R1b","R2a","Rx2b","Rx2a","R2b","R3a","R3b","HoT","Inner_Bay","Outer_Bay","all_SoBI"))

ggplot(all_pings_sum2_long,aes(x=value))+
  geom_histogram(bins = 100)+
    facet_wrap(~receivers,scales="free_x")

## Summarising the number of pings per receivers

mean(all_pings_sum2$tot_ping)
sd(all_pings_sum2$tot_ping)

all_pings_sum2_long  %>%  group_by(receivers) %>% summarise(avg_det = mean(na_if(value,0),na.rm=T),
                                                                         sd_det = sd(na_if(value,0),na.rm=T))

all_pings_sum2_boolean <- cbind(all_pings_sum2[,c(1:3)],  ifelse(all_pings_sum2[,-c(1:3)]==0,0,1))
all_pings_sum2_boolean  <- data.frame(all_pings_sum2_boolean) %>% select(-tot_ping) %>% mutate(n_rec_det = rowSums(.[,-c(1:3)]))

hist(all_pings_sum2_boolean$n_rec_det)

## Number of smolt detected at each receiver/array
lapply(all_pings_sum2_boolean[,-c(1:3)],sum)

all_pings_sum2_freq <- all_pings_sum2 %>% pivot_longer(c(RP1, all_MM, Rx1,R1a,R1b,R2a,Rx2a,Rx2b,R2b,R3a,R3b,HoT,Inner_Bay,Outer_Bay,all_SoBI) ) %>%
  mutate(freq = value/tot_ping) %>% 
  select(Tags_smolts,name,freq) %>% group_by(Tags_smolts) %>%
  pivot_wider(names_from = name, values_from = freq) %>%
  select(Tags_smolts,RP1, all_MM, Rx1,R1a,R1b,R2a,Rx2a,Rx2b,R2b,R3a,R3b,HoT,Inner_Bay,Outer_Bay,all_SoBI) %>%
  mutate(Tot_freq = sum(c(RP1, all_MM, Rx1,R1a,R1b,R2a,Rx2a,Rx2b,R2b,R3a,R3b,HoT,Inner_Bay,Outer_Bay,all_SoBI),na.rm=T))

## Adding a column for the 2nd version of the detection history graph
all_pings <- all_pings %>% mutate(lvl_2 = recode(Station_array, "RP1" = 15,
                                                               "all_MM" = 10,
                                                               "Rx1" = 8,
                                                               "R1a" = 7,
                                                               "R1b" = 7,
                                                               "R2a" = 6,
                                                               "Rx2a" = 5,
                                                               "Rx2b" = 5,
                                                               "R2b" = 4,
                                                               "R3a" = 3,
                                                               "R3b" = 3,
                                                               "HoT" = 1,
                                                               "Inner_Bay" = -2,
                                                               "Outer_Bay" = -5,
                                                               "SoBI"=-10,
                                                               "SoBI_W"=-9.8,
                                                               "SoBI_N"=-9.6),
                                  Station_Duration = recode(Station_array, "RP1"    = 1,
                                                                          "all_MM" = 2,
                                                                          "Rx1"    = 3,
                                                                          "R1a"    = 4,
                                                                          "R1b"    = 5,
                                                                          "R2a"    = 6,
                                                                          "Rx2a"   = 7,
                                                                          "Rx2b"   = 8,
                                                                          "R2b"    = 9,
                                                                          "R3a"    = 10,
                                                                          "R3b"    = 11,
                                                                          "HoT"    = 12,
                                                                          "Inner_Bay" = 13,
                                                                          "Outer_Bay" = 14,
                                                                          "SoBI"      = 15,
                                                                          "SoBI_W"    = 15,
                                                                          "SoBI_N"    = 15) )

all_pings_boolean <- data.frame( smolt_ID=all_pings_sum$Tags_smolts , ifelse(is.na(all_pings_sum[,-1]),0,1) )

write.csv(all_pings_boolean,"Output/summary_From_raw_boolean.csv")

## Raw detection probability for single channels

sum(all_pings_boolean$RP1)
sum(all_pings_boolean$RP1)/45

sum(all_pings_boolean$all_MM)
sum(all_pings_boolean$all_MM)/45

sum(all_pings_boolean$Rx1)
sum(all_pings_boolean$Rx1)/45

sum(all_pings_boolean$HoT)
sum(all_pings_boolean$HoT)/45


## Number of smolts not detected in the channels
all_pings_boolean %>% filter(R1a== 0, R1b == 0, R2a == 0 , Rx2b ==0 , Rx2a ==0, R2b == 0, R3a == 0, R3b ==0)


## Number of smolts detected in the Moses RST channel
all_pings_boolean %>% filter(R2a == 1)

## Number of smolts detected in the butters RST channel
all_pings_boolean %>% filter(R2b == 1)

## Number of smolts detected in the middle channel /dodging RST
all_pings_boolean %>% filter(Rx2a == 1)


### Adding the ASF detection status
# 
# dat_BC_ASF_temp <- dat_BC_ASF  %>% select(`TAG #`,Transmitter,HoT,Chal_Inner_Bay,Chal_Outer_Bay,SoBI,SoBI_N) %>% rename( "smolt_ID" = `TAG #` ) 
# all_det <- all_pings_boolean %>% mutate(smolt_ID=as.numeric(smolt_ID)) %>% left_join(dat_BC_ASF_temp)
# 
# write.csv(all_det,"Output/detections_2022_for_JD.csv",row.names = F)



## number of receivers hit when a fish is pinged in the channel

temp <- all_pings_boolean %>% filter(R1a== 1 | R1b == 1 | R2a == 1 | Rx2b ==1 | Rx2a ==1 | R2b == 1 | R3a == 1 | R3b ==1)
temp <- temp %>% mutate(n_receivers = R1a + R1b + R2a + Rx2b + Rx2a + R2b +R3a +R3b)


temp %>% filter(n_receivers >= 3)

par(mfrow=c(1,1),mar=c(5.1,4.1,4.1,2.1))
hist(temp$n_receivers,main= "Number of receivers pinged",xlab="")

## summary number of fish detected at x receivers within the island network
table(temp$n_receivers)

Time_in_zone <- all_pings %>% group_by(Tags_smolts,Station_Duration) %>% summarise(First_ping = min(Date_Time_AST),
                                                                   Last_ping = max(Date_Time_AST) ) %>% 
              mutate(Duration_stay = difftime(Last_ping, First_ping,units = "mins")) %>% 
  pivot_wider(names_from = Station_Duration, values_from = Duration_stay) %>% 
  select(Tags_smolts,First_ping,Last_ping,'1','2','3','4','5','6','7','8','9','10','11','12','13','14','15')

Time_in_zone2 <- all_pings %>% group_by(Tags_smolts,Station_Duration) %>% summarise(First_ping = min(Date_Time_AST),
                                                                                   Last_ping = max(Date_Time_AST) ) %>% 
                              mutate(Duration_stay = difftime(Last_ping, First_ping,units = "mins")) %>% 
                              select(-c(First_ping,Last_ping)) %>%
                              pivot_wider(names_from = Station_Duration, values_from = Duration_stay) %>% 
                              select(Tags_smolts,'1','2','3','4','5','6','7','8','9','10','11','12','13','14','15')


## For each fish calculate the time spent between Rx1 and R3a/R3b

time_in_channel <-
  all_pings %>% filter(between(Station_Duration,3,11)) %>% 
  group_by(Tags_smolts,Station.Name) %>% summarise(min_time=min(Date_Time_AST),
                                                   max_time=max(Date_Time_AST)) %>% 
  filter(Station.Name == "Rx1" | Station.Name == "R3a"  | Station.Name == "R3b" | Station.Name == "REST4" | Station.Name == "REST5" | Station.Name == "REST6"   ) %>%
  pivot_wider(names_from = Station.Name,values_from = c(min_time,max_time) ) %>%
  filter( (!is.na(min_time_Rx1) & !is.na(min_time_R3a) ) | (!is.na(min_time_Rx1) & !is.na(min_time_R3b) ) )  %>%                  
  mutate(tot_time = difftime( max(c(max_time_R3a,max_time_R3b),na.rm = T),min_time_Rx1, units = "mins"))  %>% 
  mutate(tot_time = as.numeric(tot_time) )%>%
  arrange(min_time_Rx1)%>%
  #           recode(tot_time,"-Inf"=NA) ) %>%
  filter(!is.na(tot_time) & tot_time>0) %>%
  mutate(time_cat = ifelse(tot_time<100,"fast",
                           ifelse(tot_time<500,"intermediate","slow")))


mean(time_in_channel$tot_time/60)
sd(time_in_channel$tot_time/60)

time_in_channel %>% group_by(time_cat) %>% tally()

## Speed  Distance between Rx1 and R3a/b measured on google Earth
## 6.53 and 6.18km respectively: avg 6.355

6.355 / (mean(time_in_channel$tot_time) / 60) 
## in m/s
mean( (6.355 *1000) / (time_in_channel$tot_time* 60))
sd((6.355 *1000) / (time_in_channel$tot_time * 60))

6.355 / (median(time_in_channel$tot_time) / 60 )

6.355 / (time_in_channel$tot_time / 60)

## Histogram of residence time in the island complex
## Threshold picked semi-arbitraly to reflect the histogram distribtion 
## and the fact that some fish are going through very quickly (less than 1.5 hour, n = 10 )
## some are getting stuck (12+ hours , n=9) and some are in between (between 1.5 and 5 hours, n=2 )
hist(time_in_channel$tot_time,n=100)
abline(v=c(100,480),col="red"     )


par(mfrow=c(2,3))
plot(table_MS$`FORK LENGTH (mm)`,table_MS$travel_time_to_Rx1)
summary(lm(travel_time_to_Rx1~`FORK LENGTH (mm)`,table_MS))

plot(table_MS$`FORK LENGTH (mm)`,table_MS$travel_time_to_HoT_first)
summary(lm(travel_time_to_HoT_first~`FORK LENGTH (mm)`,table_MS))

plot(table_MS$`FORK LENGTH (mm)`,table_MS$travel_time_HoT_inner_first)
summary(lm(travel_time_HoT_inner_first~`FORK LENGTH (mm)`,table_MS))


plot(table_MS$`FORK LENGTH (mm)`,table_MS$travel_time_HoT_outer_first)
summary(lm(travel_time_HoT_outer_first~`FORK LENGTH (mm)`,table_MS))


plot(table_MS$`FORK LENGTH (mm)`,table_MS$travel_time_HoT_sobi_first)
summary(lm(travel_time_HoT_sobi_first~`FORK LENGTH (mm)`,table_MS))


### Storing the objects necessary to make the figures 
### This will be in the public repository

save(list=c("table_MS","time_in_channel","all_tide","dat_sun"),file="data_fig2-3.Rdata")



#### Creating a nice graph to showcase the differences in speed between environments ####

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
## at the arrival point (m.s-1)

## We create a little function to make things a bit tidier and practical 
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




#png(filename = "figures/speed_summary.png",width=1600,height=750,pointsize=16)

## to have the figure in
## 1: m.s-s
## 2: km.day-1
i<-2

if(i==1){
  tiff(filename = "figures/speed_summary_m_s.tif",width=2500,height=1200,pointsize=24) 
}
if(i==2){
  tiff(filename = "figures/speed_summary_km_day.tif",width=2500,height=1200,pointsize=24) 
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
     labels = c("Release site","Rx1","R3a/R3b","HoT","Inner Bay", "Outer Bay", "SoBI"))

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

#### Trick####

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
#####

dev.off()

#### Creating a figure withe daylight and tide #### 

Sum_times <- dat_2022_BC %>% select(`TAG #`,`FORK LENGTH (mm)`,`WEIGHT (G)`,release_time_posix) %>% mutate(`TAG #` = as.character(`TAG #`)) %>%
  rename(Tags_smolts =`TAG #` ) %>%  right_join(time_1st_Rx1) %>%
                                     right_join(time_last_Rx1) %>%
                                     right_join(time_1st_Rest) %>%
                                     right_join(time_last_Rest)      

time_in_channel2 <-
  all_pings %>% filter(between(Station_Duration,3,12)) %>% 
  group_by(Tags_smolts,Station_Duration) %>% summarise(min_time=min(Date_Time_AST),
                                                   max_time=max(Date_Time_AST)) %>% 
  filter(Station_Duration == 3 | Station_Duration == 10  | Station_Duration == 11 | Station_Duration == 12  ) %>%
  pivot_wider(names_from = Station_Duration,values_from = c(min_time,max_time) ) 





BC_all <- dat_2022_BC %>% select(`TAG #`,`FORK LENGTH (mm)`,`WEIGHT (G)`,release_time_posix) %>% mutate(`TAG #` = as.character(`TAG #`)) %>%
  rename(Tags_smolts =`TAG #` ) 

time_in_channel2 <- BC_all %>% left_join(time_in_channel2)



time_in_channel3 <- time_in_channel2 %>% mutate(time_to_1st_Rx1 = round(difftime( min_time_3,release_time_posix, units = "days"),2),
                                                time_at_Rx1  = round(difftime( max_time_3,min_time_3, units = "hours"),2),
                                                time_to_rest = round(difftime( min_time_12,release_time_posix, units = "days"),2),
                                                time_last_rest = round(difftime( max_time_12,release_time_posix, units = "days"),2),
                                                time_at_rest = round(difftime( max_time_12,min_time_12, units = "hours"),2),
                                                time_Rx1_to_rest = round(difftime( max_time_12,min_time_3, units = "days"),2)
                                                )

average_time_Rx1 <- mean(time_in_channel3$time_at_Rx1,na.rm=T)
sd_time_Rx1 <- sd(time_in_channel3$time_at_Rx1,na.rm=T)

average_time_rest <- mean(time_in_channel3$time_at_rest,na.rm=T)
sd_time_rest <- sd(time_in_channel3$time_at_rest,na.rm=T)



time_in_channel3_rx1_rest <- time_in_channel3 %>% filter(!is.na(time_Rx1_to_rest))

hist(as.numeric(time_in_channel3_rx1_rest$time_Rx1_to_rest),breaks=20) 

plot(time_in_channel3$time_to_1st_Rx1,rep(5,45),xlim=c(0,15),ylim=c(0,10))

for (i in 1:45){
  if(!is.na(time_in_channel3$time_to_1st_Rx1[i])){
    segments(0,10,time_in_channel3$time_to_1st_Rx1[i],5 )
  }
}

## Fonction to aproximate the tidal data as a line - there is a warning, its fine
AF <- approxfun(all_tide$Date_Time,all_tide$predictions.m.)

## for 21 smolts we can calculate a time of residence in the island complex (n=21)
## we plot the alternance day/night and we add the tide cycle (using water level as a proxy)
## and the time of entry and exit of each individual


# col_speed =c("#4d9221","#e6f5d0","#c51b7d")
# col_speed =c("#4d9221","#f1b6da","#c51b7d")
# col_speed =c("#df65b0","#ce1256","#67001f")


col_speed =c("#fd8d3c","#e31a1c","#800026")

png("Figures/tide_sun_smolts.png",height=1200,width=1800,pointsize=20,type="cairo-png")

par(mar=c(5,5.5,2,2))

all_tide_sub <- all_tide %>% filter( between(Date_Time, as.Date("2022-05-25"), as.Date("2022-06-05"))) 

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
                                                           col=ifelse(time_in_channel$tot_time[i]<100,col_speed[1],
                                                           ifelse(time_in_channel$tot_time[i]<500,col_speed[2],col_speed[3])),pch=15)
  
  points(max(c(time_in_channel$max_time_R3a[i],time_in_channel$max_time_R3b[i]),na.rm=T), AF(time_in_channel$min_time_Rx1[i]),
                                                           col=ifelse(time_in_channel$tot_time[i]<100,col_speed[1],
                                                           ifelse(time_in_channel$tot_time[i]<500,col_speed[2],col_speed[3])),pch=15)
  
  segments(time_in_channel$min_time_Rx1[i],AF(time_in_channel$min_time_Rx1[i]),
           max(c(time_in_channel$max_time_R3a[i],time_in_channel$max_time_R3b[i]),na.rm = T), AF(time_in_channel$min_time_Rx1[i]),
           col=ifelse(time_in_channel$tot_time[i]<100,col_speed[1],
                      ifelse(time_in_channel$tot_time[i]<480,col_speed[2],col_speed[3])),
           lwd=3)
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
        "Time spent between Rx1 and R3a/b",
        "<100 min", "between 100 min and 8 hours", ">= 8 hours"),
       box.lwd = 0,box.col = "grey45",bg = "white",cex=1.2)

## Being cute and creating a legend that matches the segments for duration

points(x=rep(pos$text$x, times=2) - c(rep(35000,8),rep(10000,8)), 
       y=rep(pos$text$y, times=2), 
       cex=1.3,
       pch=rep(c(NA,NA,NA,NA,NA,15,15,15), times=2), col=rep(c(NA,NA,NA,NA,NA,col_speed[1],col_speed[2],col_speed[3]), times=2))

segments(x0=pos$text$x[6:8]-35000,
         y0=pos$text$y[6:8],
         x1=pos$text$x[6:8]-10000,
         y1=pos$text$y[6:8],
         col=c(col_speed[1],col_speed[2],col_speed[3])
           )

box()

dev.off()


##################

png("Figures/tide_sun_smolts_split.png",height=2400,width=1800,pointsize=22,type="cairo-png")


par(mfrow=c(2,1))
par(mar=c(2,5.5,1,2),oma=c(2,2,2,2))

all_tide_sub <- all_tide %>% filter( between(Date_Time, as.Date("2022-05-25"), as.Date("2022-05-29"))) 

plot(all_tide_sub$Date_Time,all_tide_sub$predictions.m.,type="l",
     xlab = "", ylab="Water Height (m)",cex.lab=1.3)

# subset May/june sunrise data
dat_sun_sub <- dat_sun %>% filter(between(Sunrise_POSIX, as.Date("2022-05-01"), as.Date("2022-07-01")))

polygon(x=c(dat_sun_sub$Nautical_twilight_start_POSIX[1],dat_sun_sub$Nautical_twilight_end_POSIX[61],dat_sun_sub$Nautical_twilight_end_POSIX[61],dat_sun_sub$Nautical_twilight_start_POSIX[1]),y=c(0,0,4,4),col="grey75")
for(i in 1:61){
  polygon(x=c(dat_sun_sub$Sunrise_POSIX[i],dat_sun_sub$Sunset_POSIX[i],dat_sun_sub$Sunset_POSIX[i],dat_sun_sub$Sunrise_POSIX[i]),y=c(0,0,4,4),col="grey95",border=NA)
  polygon(x=c(dat_sun_sub$Nautical_twilight_end_POSIX[i],dat_sun_sub$Nautical_twilight_start_POSIX[i+1],dat_sun_sub$Nautical_twilight_start_POSIX[i+1],dat_sun_sub$Nautical_twilight_end_POSIX[i]),y=c(0,0,4,4),col="grey55",border=NA)
  
}
points(all_tide$Date_Time,all_tide$predictions.m.,type="l",col="royalblue3",lwd=5)

for (i in 1:21){
  points(time_in_channel$min_time_Rx1[i], AF(time_in_channel$min_time_Rx1[i]),
         col=ifelse(time_in_channel$tot_time[i]<100,col_speed[1],
                    ifelse(time_in_channel$tot_time[i]<500,col_speed[2],col_speed[3])),pch=15)
  
  points(max(c(time_in_channel$max_time_R3a[i],time_in_channel$max_time_R3b[i]),na.rm=T), AF(time_in_channel$min_time_Rx1[i]),
         col=ifelse(time_in_channel$tot_time[i]<100,col_speed[1],
                    ifelse(time_in_channel$tot_time[i]<500,col_speed[2],col_speed[3])),pch=15)
  
  segments(time_in_channel$min_time_Rx1[i],AF(time_in_channel$min_time_Rx1[i]),
           max(c(time_in_channel$max_time_R3a[i],time_in_channel$max_time_R3b[i]),na.rm = T), AF(time_in_channel$min_time_Rx1[i]),
           col=ifelse(time_in_channel$tot_time[i]<100,col_speed[1],
                      ifelse(time_in_channel$tot_time[i]<480,col_speed[2],col_speed[3])),
           lwd=3)
  text(time_in_channel$min_time_Rx1[i]- 13000, AF(time_in_channel$min_time_Rx1[i])+0.02,
       time_in_channel$Tags_smolts[i])
}

pos <-legend("bottomright", 
             pch = c(15,15,15,NA),
             col=c("grey95","grey75","grey55","royalblue3",NA,NA,NA,NA),
             lty=c(NA,NA,NA,1,NA,NA,NA,NA),
             lwd=c(NA,NA,NA,2,NA,NA,NA,NA),
             pt.cex = c(2,2,2,1,NA,NA,NA,NA),
             legend=c("Day (sunrise-sunset)","Nautical Twilight (start-end)","Night","Water height",
                      "Time spent between Rx1 and R3a/b",
                      "<100 min", "between 100 min and 8 hours", ">= 8 hours"),
             box.lwd = 0,box.col = "grey45",bg = "white",cex=1.2,bty="o+")

## Being cute and creating a legend that matches the segments for duration

points(x=rep(pos$text$x, times=2) - c(rep(17000,8),rep(10000,8)), 
       y=rep(pos$text$y, times=2), 
       cex=1.3,
       pch=rep(c(NA,NA,NA,NA,NA,15,15,15), times=2), col=rep(c(NA,NA,NA,NA,NA,col_speed[1],col_speed[2],col_speed[3]), times=2))

segments(x0=pos$text$x[6:8]-17000,
         y0=pos$text$y[6:8],
         x1=pos$text$x[6:8]-10000,
         y1=pos$text$y[6:8],
         col=c(col_speed[1],col_speed[2],col_speed[3])
)



box()

all_tide_sub <- all_tide %>% filter( between(Date_Time, as.Date("2022-05-31"), as.Date("2022-06-05"))) 



plot(all_tide_sub$Date_Time,all_tide_sub$predictions.m.,type="l",
     xlab = "Date", ylab="Water Height (m)",cex.lab=1.3)

# subset May/june sunrise data
dat_sun_sub <- dat_sun %>% filter(between(Sunrise_POSIX, as.Date("2022-05-01"), as.Date("2022-07-01")))

polygon(x=c(dat_sun_sub$Nautical_twilight_start_POSIX[1],dat_sun_sub$Nautical_twilight_end_POSIX[61],dat_sun_sub$Nautical_twilight_end_POSIX[61],dat_sun_sub$Nautical_twilight_start_POSIX[1]),y=c(0,0,4,4),col="grey75")
for(i in 1:61){
  polygon(x=c(dat_sun_sub$Sunrise_POSIX[i],dat_sun_sub$Sunset_POSIX[i],dat_sun_sub$Sunset_POSIX[i],dat_sun_sub$Sunrise_POSIX[i]),y=c(0,0,4,4),col="grey95",border=NA)
  polygon(x=c(dat_sun_sub$Nautical_twilight_end_POSIX[i],dat_sun_sub$Nautical_twilight_start_POSIX[i+1],dat_sun_sub$Nautical_twilight_start_POSIX[i+1],dat_sun_sub$Nautical_twilight_end_POSIX[i]),y=c(0,0,4,4),col="grey55",border=NA)
  
}
points(all_tide$Date_Time,all_tide$predictions.m.,type="l",col="royalblue3",lwd=5)

for (i in 1:21){
  points(time_in_channel$min_time_Rx1[i], AF(time_in_channel$min_time_Rx1[i]),
         col=ifelse(time_in_channel$tot_time[i]<100,col_speed[1],
                    ifelse(time_in_channel$tot_time[i]<500,col_speed[2],col_speed[3])),pch=15)
  
  points(max(c(time_in_channel$max_time_R3a[i],time_in_channel$max_time_R3b[i]),na.rm=T), AF(time_in_channel$min_time_Rx1[i]),
         col=ifelse(time_in_channel$tot_time[i]<100,col_speed[1],
                    ifelse(time_in_channel$tot_time[i]<500,col_speed[2],col_speed[3])),pch=15)
  
  segments(time_in_channel$min_time_Rx1[i],AF(time_in_channel$min_time_Rx1[i]),
           max(c(time_in_channel$max_time_R3a[i],time_in_channel$max_time_R3b[i]),na.rm = T), AF(time_in_channel$min_time_Rx1[i]),
           col=ifelse(time_in_channel$tot_time[i]<100,col_speed[1],
                      ifelse(time_in_channel$tot_time[i]<480,col_speed[2],col_speed[3])),
           lwd=3)
  text(time_in_channel$min_time_Rx1[i]- 13000, AF(time_in_channel$min_time_Rx1[i])+0.02,
       time_in_channel$Tags_smolts[i])
}

# pos <-legend("topright", 
#              pch = c(15,15,15,NA),
#              col=c("grey95","grey75","grey55","royalblue3",NA,NA,NA,NA),
#              lty=c(NA,NA,NA,1,NA,NA,NA,NA),
#              lwd=c(NA,NA,NA,2,NA,NA,NA,NA),
#              pt.cex = c(2,2,2,1,NA,NA,NA,NA),
#              legend=c("Day (sunrise-sunset)","Nautical Twilight (start-end)","Night","Water height",
#                       "Time spent between Rx1 and R3a/b",
#                       "<100 min", "between 100 min and 8 hours", ">= 8 hours"),
#              box.lwd = 0,box.col = "grey45",bg = "white",cex=1.2)
# 
# ## Being cute and creating a legend that matches the segments for duration
# 
# points(x=rep(pos$text$x, times=2) - c(rep(17000,8),rep(10000,8)), 
#        y=rep(pos$text$y, times=2), 
#        cex=1.3,
#        pch=rep(c(NA,NA,NA,NA,NA,15,15,15), times=2), col=rep(c(NA,NA,NA,NA,NA,"green","orange","red"), times=2))
# 
# segments(x0=pos$text$x[6:8]-17000,
#          y0=pos$text$y[6:8],
#          x1=pos$text$x[6:8]-10000,
#          y1=pos$text$y[6:8],
#          col=c("green","orange","red")
# )

box()
dev.off()










#### Graphic summary ####

## Generate a figure for each fish's travel history
## Not sure where the bottleneck is but it takes a while to run the loop  ~1. min future Guillaume to deal with
## Investigate the raster part
##

t0 <- Sys.time()
pdf("Figures/Suppl_Mat_A_smolt_travel_history.pdf",width=6,height=4,pointsize=2)

range_time <- pretty(all_pings$Date_Time_AST,n=13)

## Coordinates receivers - upstream/downstream of islands coordinates guessed
## small map
coord_sum <- data.frame(
  names_receivers = c("Rx1","R1a","R1b","R2a","Rx2a","Rx2b","R2b","R3a","R3b"),
  x_coord = c(636,826,860,1498,1595,1576,1742,1842,1858),
  y_coord = 1958 - c(1226,1132,1262,1160,1310,1358,1394,1288,1370),
  
  x_text = c(636,826,860,1498,1595,1576,1742,1842,1858),
  y_text = 1958 - c(1126,1032,1362,1040,1230,1458,1494,1188,1470),
  col_bg = c("lightskyblue","olivedrab3","hotpink1","olivedrab3","hotpink4","hotpink1","hotpink1","olivedrab3","hotpink1")
)
  
##large map
coord_sum_LG <- data.frame(
  names_receivers = c("RP1","all_MM","HoT","Inner_Bay","Outer_Bay","SoBI"),
  x_coord = c(130,134,175,204,294,1002),
  y_coord = 816 - c(649,646,636,631,651,138),
  
  x_text = c(130,134-40,175,234,294+10,1002),
  y_text = (816 - c(649+40,646-40,636-40,631+45,651-40,138-45)),  
  
  col_bg = c("lightskyblue","lightskyblue","deepskyblue4","deepskyblue4","deepskyblue4","deepskyblue4")
)

## Version 1
# layout_matrix <- matrix(c(1,1,1,2,2,2,2,2,
#                           1,1,1,2,2,2,2,2,
#                           1,1,1,3,3,3,3,3,
#                           1,1,1,3,3,3,3,3), ncol = 8,byrow=T)                  # Define position matrix

##Version 2
layout_matrix <- matrix(c(1,1,1,1,2,2,2,2,
                          1,1,1,1,2,2,2,2,
                          1,1,1,1,3,3,3,3,
                          1,1,1,1,3,3,3,3), ncol = 8,byrow=T)                  # Define position matrix

layout(layout_matrix)           


for (i in 1:length(Tags_smolts_ID)){
 # temp <- all_pings %>% filter(Tags_smolts==Tags_smolts_ID[i])
  temp <- all_pings %>% filter(Tags_smolts==Tags_smolts_ID[i])
  ## Version 1
  # par( mar=c(7,4,2,2))
  #   plot(temp$Date_Time_AST,temp$Station.Name,pch=16,axes = F, main = Tags_smolts_ID[i], xlab="",ylab="", xlim=c(min(range_time),max(range_time)),ylim=c(0.5,15))
  # axis(2,at=1:15,las=2,
  #      labels = rev( c("RP1","MM1","MM2","Rx1","R1a","R1b","R2a","Rx2a","Rx2b","R2b","R3a","R3b","REST4","REST5","REST6")))
  # axis(1,at=range_time ,labels=as.character(range_time),las=2 )
  # 
  # ## color polygons to indicate the different zones
  # ##upstream islands
  # polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(11.5,11.5,16,16) , 
  #         col="lightskyblue",border = NA)
  # ##Moses side Channels
  # polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(10.5,10.5,11.5,11.5) , 
  #         col="olivedrab3",border = NA)
  # polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(8.5,8.5,9.5,9.5) , 
  #         col="olivedrab3",border = NA)
  # polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(4.5,4.5,5.5,5.5) , 
  #         col="olivedrab3",border = NA)
  # ## Butters side Channels
  # polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(9.5,9.5,10.5,10.5) , 
  #         col="hotpink1",border = NA)
  # polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(7.5,7.5,8.5,8.5) , 
  #         col="hotpink4",border = NA)
  # polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(6.5,6.5,7.5,7.5) , 
  #         col="hotpink1",border = NA)
  # polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(5.5,5.5,6.5,6.5) , 
  #         col="hotpink1",border = NA)
  # polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(3.5,3.5,4.5,4.5) , 
  #         col="hotpink1",border = NA)
  # ## Downstream islands
  # polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(-1,-1,3.5,3.5) , 
  #         col="deepskyblue4",border = NA)
  # box()
  # 
  # abline(v=range_time , lty=3 , col="grey65")
  # 
  # points(temp$Date_Time,temp$Station.Name,pch=16)
  # 
  # legend("topright",pt.bg=c("lightskyblue","olivedrab3","hotpink1","hotpink4","deepskyblue4"),legend=c("Upstream islands","Moses channel","Butters channel","","Downstream islands"),
  #        col=rep(NA,5),pch=rep(22,5),bg="white")
  # 
  # 
  
  ## Version 2
  # col_v2 <- as.data.frame(t(col2rgb(rev(c("lightskyblue","lightskyblue","lightskyblue","lightskyblue",
  #                                         "olivedrab3","hotpink1","olivedrab3","hotpink1","hotpink4","hotpink1","olivedrab3","hotpink1",
  #                                         "deepskyblue4","deepskyblue4","deepskyblue4")))))

  col_v2 <- as.data.frame(t(col2rgb(c("lightskyblue","lightskyblue","lightskyblue",
                                          "olivedrab3","hotpink1","olivedrab3","hotpink4","hotpink1","hotpink1","olivedrab3","hotpink1",
                                          "deepskyblue4","deepskyblue4","deepskyblue4","deepskyblue4","deepskyblue4","deepskyblue4"))))
  
  par( mar=c(7,7.5,2,0))
  
  plot(temp$Date_Time_AST,temp$lvl_2,pch=16,axes = F, main = Tags_smolts_ID[i], xlab="",ylab="", xlim=c(min(range_time),max(range_time)),ylim=c(-10.5,16))
  axis(2,at=c(-10,-9.8,-9.6,-5,-2,1,3,4,5,6,7,8,10,15),las=2,
       labels = F)
  
  axis(2,at=c(-10.3,-9.8,-9.3,-5,-2,1,3,4,5,6,7,8,10,15),las=2,tick=F,cex.axis=0.8,
       labels = rev( c("RP1","MM1 / MM2","Rx1","R1a / R1b","R2a","Rx2a/Rx2b","R2b","R3a / R3b","HoT","Inner Bay","Outer Bay","SoBI","SoBI W","SoBI N")))

  axis(1,at=range_time ,labels=as.character(range_time),las=2 )
  ##  polygons to indicate the different zones
  ##upstream islands
  polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(10.5,10.5,17,17) , 
          col="grey85",border = NA)
  polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(8.5,8.5,10.5,10.5) , 
          col="grey95",border = NA) 
  polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(7.5,7.5,8.5,8.5) , 
          col="grey85",border = NA)  
  polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(6.5,6.5,7.5,7.5) , 
          col="grey95",border = NA)   
  polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(5.5,5.5,6.5,6.5) , 
          col="grey85",border = NA) 
  polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(4.5,4.5,5.5,5.5) , 
          col="grey95",border = NA) 
  polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(3.5,3.5,4.5,4.5) , 
          col="grey85",border = NA)  
  polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(2.5,2.5,3.5,3.5) , 
          col="grey95",border = NA) 
  polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(-1,-1,2.5,2.5) , 
          col="grey85",border = NA) 
  polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(-3.5,-3.5,-1,-1) , 
          col="grey95",border = NA)
  polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(-8,-8,-3.5,-3.5) , 
          col="grey85",border = NA)
  polygon(x=c(min(range_time) - lubridate::days(1),max(range_time) + lubridate::days(1) ,max(range_time) + lubridate::days(1),min(range_time) - lubridate::days(1)) , y=c(-12,-12,-8,-8) , 
          col="grey95",border = NA)
  box()
  
  abline(v=range_time , lty=3 , col="grey65")
  points(temp$Date_Time_AST,temp$lvl_2,pch=21,bg=rgb(col_v2[as.numeric(temp$Station_Duration),],alpha = 150,maxColorValue = 255),cex=2 ,col="grey15",lwd=0.5)
  
  legend("topright",pt.bg=c("lightskyblue","olivedrab3","hotpink1","hotpink4","deepskyblue4"),legend=c("Upstream islands","Moses channel","Butters channel","","Downstream islands"),
         col=rep(NA,5),pch=rep(22,5),bg="white")
  
  ### Map channels
  ##Large
  channel_img <- readPNG("image_channels_large_scale_update.png")
  par( mar=c(0,0,0,0))
  
  plot(500,500,xlim=c(1,1454),ylim=c(1,816), type='n', main="", xlab="", ylab="",asp=1,axes=F,
       
       yaxs = "i")
  
  # Get the plot information so the image will fill the plot box, and draw it
  lim <- par()
  rasterImage( channel_img , 
               xleft=1, xright=1454, 
               ybottom=1, ytop=816)
  #grid()
  
  temp_j <- c(1,2,12,13,14,15)
  for (j in 1:6){
     if (j<6){
     points(coord_sum_LG$x_coord[j],coord_sum_LG$y_coord[j],pch=21, 
           bg = ifelse(is.na(all_pings_sum[i,temp_j[j]+1]) , "grey75", coord_sum_LG$col_bg[j]), cex =3 )
     }else{
       points(coord_sum_LG$x_coord[j],coord_sum_LG$y_coord[j],pch=21, 
              bg = ifelse(is.na(all_pings_sum[i,temp_j[j]+1]) & is.na(all_pings_sum[i,temp_j[j]+2]) & is.na(all_pings_sum[i,temp_j[j]+3]) ,
                          "grey75", coord_sum_LG$col_bg[j]), cex =3 )
     }
       
     text(coord_sum_LG$x_text[j],coord_sum_LG$y_text[j], labels=coord_sum_LG$names_receivers[j],col="white")
  }
  ## small
  channel_img <- readPNG("image_channels.png")
  par( mar=c(0,0,0,0))
  plot(500,500,xlim=c(1,2519),ylim=c(1,1558), type='n', main="", xlab="", ylab="",asp=1,axes=F,
       yaxs = "i")
  
  # Get the plot information so the image will fill the plot box, and draw it
  lim <- par()
  rasterImage( channel_img , 
              xleft=1, xright=2519, 
              ybottom=1, ytop=1958)
  #grid()

  all_pings_sum_small<- all_pings_sum %>% select(-c(RP1,all_MM,HoT,Inner_Bay,Outer_Bay,SoBI,SoBI_W,SoBI_N))
  
  for (j in 1:9){
    
    points(coord_sum$x_coord[j],coord_sum$y_coord[j],pch=21, 
           bg = ifelse(is.na(all_pings_sum_small[i,j+1]) , "grey75", coord_sum$col_bg[j]), cex =3 )
    
    text(coord_sum$x_text[j],coord_sum$y_text[j], labels=coord_sum$names_receivers[j])
  }
}

dev.off()
t1<- Sys.time()
t1-t0











