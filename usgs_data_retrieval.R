library(dataRetrieval)
library(lubridate)

siteNumber <- "06770500" 
info <- readNWISsite(siteNumber)
parameterCd <- "00060"

#Raw daily data:
rawDailyData <- readNWISdv(siteNumber,parameterCd,
                           "1934-04-01","2022-12-31") %>% 
  mutate(discharge_m3_sec = X_00060_00003*0.0283168,
         code = X_00060_00003_cd) %>% 
  select(-X_00060_00003,-X_00060_00003_cd) %>% 
  mutate(year = substr(Date, 1, 4)) %>% 
  mutate(date = yday(Date))

raw_water_height <- readNWISdv(siteNumber,"00065",
                               "1934-04-01","2022-12-31") %>% 
  mutate(height_m = X_00065_00003*0.3048,
         code = X_00065_00003_cd) %>% 
  select(-X_00065_00003,-X_00065_00003_cd) %>% 
  mutate(year = substr(Date, 1, 4)) %>% 
  mutate(date = yday(Date))


############################################################Temp

ne_temp <- readNWISdv(siteNumber,"00010",
                      "1934-04-01","2022-12-31") %>% 
  mutate(temp = X_00010_00003,
         code = X_00010_00003_cd) %>% 
  select(-X_00010_00003,-X_00010_00003_cd) %>% 
  mutate(year = substr(Date, 1, 4)) %>% 
  mutate(date = yday(Date))


######################################################Precipitation

library(rnoaa)

# create a data frame for Prince William latitude and longitude
lat_lon_df <- data.frame(id = "ct",
                         lat = 40.796058442444256,
                         lon = -98.49295982440889)

# find 10 closest monitors to  Prince William
mon_near_ct <- 
  meteo_nearby_stations(
    lat_lon_df = lat_lon_df,
    lat_colname = "lat",
    lon_colname = "lon",
    var = "PRCP",
    year_min = 2019,
    year_max = 2022,
    limit = 10,
  )


mon_near_ct


ct_prcp_dat <- 
  meteo_pull_monitors(
    monitors = mon_near_ct$ct$id[1],
    date_min = "1934-01-01",
    date_max = "2022-12-31",
    var = "PRCP"
  )


head(ct_prcp_dat)



mon_temp_near_ct <- 
  meteo_nearby_stations(
    lat_lon_df = lat_lon_df,
    lat_colname = "lat",
    lon_colname = "lon",
    var = "TAVG",
    year_min = 2019,
    year_max = 2022,
    limit = 10,
  )


mon_temp_near_ct



temp <- meteo_pull_monitors(
  monitors = mon_temp_near_ct$ct$id[1],
  date_min = "1934-01-01",
  date_max = "2022-12-31",
  var = "TAVG"
)


head(temp)

###########################################################Data Work


rawDailyData %>% 
  filter(year>=2019) %>% 
  group_by(year) %>% 
  summarise(mean=mean(discharge_m3_sec,na.rm = TRUE))


rawDailyData %>% 
  filter(year<2019) %>%  
  group_by(year) %>% 
  summarise(mean=mean(discharge_m3_sec,na.rm = TRUE),
            sd=sd(discharge_m3_sec),
            min=min(discharge_m3_sec),
            median=median(discharge_m3_sec)) %>% 
  ungroup() %>% 
  summarise(mean=mean(mean))



before_2019_discharge <- rawDailyData %>% 
  separate(Date,c("year","month","day")) %>% 
  filter(year<=2019) %>%  
  group_by(month,day) %>% 
  summarise(mean=mean(discharge_m3_sec,na.rm = TRUE)) %>% 
  mutate(year="2019",
         discharge_m3_sec=mean) %>% 
  select(-mean) %>% 
  unite(date,c("year","month","day"),sep = "-") %>% 
  mutate(date=as.Date(date)) %>% 
  mutate(day_of_year=yday(date),
         year = "pre-2020") %>%
  select(-date)

before_2019_discharge_monthly <- rawDailyData %>% 
  separate(Date,c("year","month","day")) %>% 
  filter(year<=2019) %>%  
  group_by(month) %>% 
  summarise(mean=mean(discharge_m3_sec,na.rm = TRUE)) %>% 
  mutate(year="2019")

river_discharge_summary <- rawDailyData %>% 
  separate(Date,c("year","month","day")) %>% 
  filter(year>2019) %>% 
  select(year,month,day,discharge_m3_sec) %>% 
  unite(date,c("year","month","day"),sep = "-") %>% 
  mutate(date=as.Date(date)) %>% 
  mutate(day_of_year=yday(date),
         year = substr(date, 1, 4)) %>%
  select(-date) %>% 
  rbind(before_2019_discharge)



river_discharge_summary_monthly <- rawDailyData %>% 
  separate(Date,c("year","month","day")) %>% 
  filter(year>2019) %>% 
  select(year,month,discharge_m3_sec) %>% 
  group_by(month,year) %>% 
  summarise(mean=mean(discharge_m3_sec)) %>% 
  rbind(before_2019_discharge_monthly) %>% 
  ungroup() %>% 
  mutate(discharge_m3_sec=mean) %>% 
  select(-mean) %>% 
  mutate(year=as.factor(ifelse(year<="2019","Historic",year)),
         month=as.factor(month),
         month=month.name[month])

rawDailyData %>% 
  separate(Date,c("year","month","day")) %>% 
  filter(year>2019) %>% 
  select(year,discharge_m3_sec) %>%
  group_by(year) %>% 
  summarise(mean=mean(discharge_m3_sec,na.rm = TRUE))


rawDailyData %>% 
  separate(Date,c("year","month","day")) %>% 
  filter(year<=2019) %>% 
  summarise(mean=mean(discharge_m3_sec,na.rm = TRUE))


####################################################Discharge tests

days_over_8k<-rawDailyData %>% 
  separate(Date,c("year","month","day")) %>% 
  mutate(cfs=discharge_m3_sec*35.314666212661) %>% 
  ungroup() %>% 
  select(year,month,day,cfs) %>% 
  filter(cfs>=8000) %>% 
  mutate(one=1) %>% 
  group_by(year) %>% 
  summarise(count=sum(one))



days_over_12k<-rawDailyData %>% 
  separate(Date,c("year","month","day")) %>% 
  mutate(cfs=discharge_m3_sec*35.314666212661) %>% 
  ungroup() %>% 
  select(year,month,day,cfs) %>% 
  filter(cfs>=12000) %>% 
  mutate(one=1) %>% 
  group_by(year) %>% 
  summarise(count=sum(one))


days_over_18k<-rawDailyData %>% 
  separate(Date,c("year","month","day")) %>% 
  mutate(cfs=discharge_m3_sec*35.314666212661) %>% 
  ungroup() %>% 
  select(year,month,day,cfs) %>% 
  filter(cfs>=18000) %>% 
  mutate(one=1) %>% 
  group_by(year) %>% 
  summarise(count=sum(one))

test <- rawDailyData %>% 
  separate(Date,c("year","month","day")) %>% 
  mutate(cfs=discharge_m3_sec*35.314666212661) %>% 
  ungroup() %>% 
  select(year,month,day,cfs) %>% 
  filter(cfs>=18000) %>% 
  mutate(one=1) %>% 
  group_by(year) 

################################################################################



prcp_summaries <- ct_prcp_dat %>% 
  mutate(day=yday(date),
         year = substr(date, 1, 4),
         month=substr(date,6,7)) %>% 
  filter(prcp!="NA") %>% 
  filter(year>=2020) %>% 
  group_by(month,year) %>% 
  summarise(prcp=sum(prcp)) %>% 
  ungroup() %>% 
  rbind(historic_prcp) %>% 
  mutate(prcp=prcp/10)

ct_prcp_dat %>% 
  mutate(day=yday(date),
         year = substr(date, 1, 4),
         month=substr(date,6,7)) %>% 
  filter(prcp!="NA") %>% 
  filter(year>=2020) %>% 
  group_by(year) %>% 
  summarise(prcp=sum(prcp)) %>% 
  ungroup() %>%
  mutate(prcp=prcp/10)




historic_prcp <- ct_prcp_dat %>% 
  mutate(day=yday(date),
         year = substr(date, 1, 4),
         month=substr(date,6,7)) %>% 
  filter(prcp!="NA") %>% 
  filter(year<2020) %>% 
  group_by(month,year) %>% 
  summarise(prcp=sum(prcp)) %>% 
  ungroup() %>% 
  group_by(month) %>% 
  summarise(prcp=mean(prcp)) %>% 
  mutate(year="pre-2020")


ct_prcp_dat %>% 
  mutate(day=yday(date),
         year = substr(date, 1, 4),
         month=substr(date,6,7)) %>% 
  filter(prcp!="NA") %>% 
  filter(year<2020) %>% 
  group_by(year) %>% 
  mutate(prcp=prcp/10) %>% 
  summarise(prcp=sum(prcp)) %>% 
  ungroup() %>% 
  summarise(mean=mean(prcp),
            se=se(prcp))





temp %>% 
  mutate(temp_c=tavg/10,
         year = as.numeric(substr(date, 1, 4)),
         month=as.numeric(substr(date,6,7))) %>% 
  filter(month>4) %>% 
  filter(month<9) %>% 
  filter(year>2019) %>% 
  group_by(year,month) %>% 
  summarise(mean=mean(temp_c),
            se=sd(temp_c)/sqrt(length((temp_c))))




temp %>% 
  mutate(temp_c=tavg/10,
         year = as.numeric(substr(date, 1, 4)),
         month=as.numeric(substr(date,6,7))) %>% 
  filter(month>4) %>% 
  filter(month<9) %>% 
  filter(year<2019) %>% 
  group_by(month) %>% 
  summarise(mean=mean(temp_c),
            se=sd(temp_c)/sqrt(length((temp_c))))


##############################################Daily Precip

daily_prcp <- ct_prcp_dat %>% 
  mutate(day=yday(date),
         year = substr(date, 1, 4),
         month=substr(date,6,7)) %>% 
  filter(prcp!="NA") %>% 
  filter(year>=2020) %>% 
  group_by(day,year) %>% 
  rbind(historic_prcp_daily)


historic_prcp_daily <- ct_prcp_dat %>% 
  mutate(day=yday(date),
         year = substr(date, 1, 4),
         month=substr(date,6,7)) %>% 
  filter(prcp!="NA") %>% 
  filter(year<2020) %>% 
  group_by(day) %>% 
  summarise(prcp=mean(prcp,na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(year="pre-2020")


################################################Daily temp



temp_daily <- temp %>% 
  mutate(temp_c=tavg/10,
         year = as.numeric(substr(date, 1, 4)),
         month=as.numeric(substr(date,6,7)),
         day=as.numeric(substr(date,9,10))) %>% 
  filter(month>4) %>% 
  filter(month<9) %>% 
  filter(year>2019) %>% 
  select(year,month,day,temp_c) %>% 
  rbind(temp_hist_daily)



temp_hist_daily <- temp %>% 
  mutate(temp_c=tavg/10,
         year = as.numeric(substr(date, 1, 4)),
         month=as.numeric(substr(date,6,7)),
         day=as.numeric(substr(date,9,10))) %>% 
  filter(month>4) %>% 
  filter(month<9) %>% 
  filter(year<2019) %>% 
  select(year,month,day,temp_c) %>% 
  group_by(month,day) %>% 
  summarise(temp_c=mean(temp_c)) %>% 
  mutate(year="pre-2020") %>% 
  ungroup()












historic_precip <- precip %>% 
  filter(Year!="NA",
         Year!="Max",
         Year!="Min",
         Year!="Mean",
         Year!="2023",
         Year!="2022",
         Year!="2021",
         Year!="2020",
         average!="M") %>%
  gather(month,precip_in,2:13) %>% 
  select(Year,month,precip_in) %>% 
  mutate(percip_in=as.numeric(precip_in)) %>% 
  group_by(month) %>% 
  summarise(mean=2.54*mean(percip_in,na.rm = TRUE)) %>% 
  mutate(Year="Historic")


study_precip <- precip %>% 
  filter(Year!="NA",
         Year!="Max",
         Year!="Min",
         Year!="Mean",
         average!="M") %>%
  mutate(Year=as.numeric(Year)) %>% 
  filter(Year>2019) %>% 
  gather(month,precip_in,2:13) %>% 
  select(Year,month,precip_in) %>% 
  mutate(precip_in_num=as.numeric(precip_in)) %>% 
  group_by(month,Year) %>% 
  summarise(mean=2.54*precip_in_num) %>%
  mutate(Year=as.character(Year)) %>% 
  rbind(historic_precip)

temp %>% 
  filter(Year!="NA",
         Year!="Max",
         Year!="Min",
         Year!="Mean",
         Year!="2023",
         Year!="2022",
         Year!="2021",
         Year!="2020",
         average!="M") %>%
  mutate(average=as.numeric(average),
         average_c=(average-32)*(5/9)) %>% 
  select(Year,may,june,july,august) %>%
  gather(month,temp_f,2:5) %>% 
  mutate(temp_f=as.numeric(temp_f),
         temp_c=((temp_f)-32)*(5/9)) %>% 
  summarise(mean=mean(temp_c,na.rm=TRUE),
            sd=sd(temp_c,na.rm=TRUE))





my_lat <- 40.78776201698288 *2*pi/360
my_lon <- -98.46208907206997*2*pi/360

inventory_url <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"

inventory <- read_table(inventory_url,
                        col_names = c("station","lat","lon","variable","start","end"))

# Distance, d = 3963.0 * arccos[(sin(lat1) * sin(lat2)) + cos(lat1) * cos(lat2) * cos(long2 – long1)]
# 
# The obtained distance, d, is in miles. If you want your value to be in units of kilometers, multiple d by 1.609344.
# d in kilometers = 1.609344 * d in miles


my_station <- inventory %>% 
  mutate(lat_r=lat*2*pi/360,
         lon_r=lon*2*pi/360,
         d=1.609344*3963*acos((sin(lat_r)*sin(my_lat))+cos(lat_r)*cos(my_lat)*cos(my_lon-lon_r))) %>%  
  filter(start<1960) %>% 
  filter(end>2022) %>% 
  top_n(n=-1,d) %>% 
  distinct(station) %>% 
  pull(station)

station_daily <- glue("https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/{my_station}.csv.gz")

local_weather <- read_csv(station_daily,
                          col_names = c("station","date","variable","value","a","b","c","d")) %>% 
  select(date,variable,value) %>% 
  pivot_wider(names_from = "variable",values_from = "value",
              values_fill = 0) %>% 
  select(date,PRCP,SNOW) %>% 
  mutate(date=ymd(date)) %>% 
  separate(date,c("year","month","day")) %>% 
  group_by(month,day,year) %>% 
  mutate(PRCP=PRCP/100,
         SNOW=SNOW/10,
         tot_precip=sum(PRCP,SNOW)) %>% 
  ungroup()
