# Script to compile and plot data from iButtons
# Updated 2017-Jan-27

# Libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(lubridate)
library(caTools)
library(readr) # part of the "tidyverse" which if you install.packages
library(viridis)
#("tidyverse") will give you most of these (dplyr, ggplot2, readr, tidyr, etc)


# Import ibutton files and compile ----------------------------------------

site<-"520M15_31"

# (remember Ctrl or Cmd + Shift + R will insert a section, shows up in the little outline button at top right of a script)

# use the readr package to import...can specify column names on import
ibuttons<-read_csv("data/520M15_31.csv",skip=8, col_names = c("datetime_s", "Temp_C"))

#colnames(ibuttons)<-c("DateTime", "Temp_C", "Btn")

# Convert Seconds to Datetime ---------------------------------------------

# Fix Datetime in one line! (you were really close to this already)
ibuttons$datetime_s<-as.POSIXct(ibuttons$datetime_s*86400, origin = "1899-12-30",tz = "UTC") 

# add the year/yday/month
ibuttons$year<-year(ibuttons$datetime_s)
ibuttons$mon<-month(ibuttons$datetime_s)
ibuttons$yday<-yday(ibuttons$datetime_s)


# Calculate Averages ------------------------------------------------------

## Get a 7-day mean (I had to remove "Number" from group_by and select)
df.dy<-ibuttons %>%
  filter(!is.na(year)) %>% 
  group_by(year, mon, yday)%>%
  dplyr::summarize("t.avg"= mean(Temp_C,na.rm=TRUE),
                   "t.min"= min(Temp_C,na.rm=TRUE),
                   "t.max"= max(Temp_C,na.rm=TRUE)) %>%  
  mutate("temp.7.avg"= runmean(t.avg, k=7, endrule="mean",align="right"),
         "temp.7.min"= runmin(t.min, k=7, align="right"),
         "temp.7.max"= runmax(t.max, k=7, align="right")) %>% 
  mutate("datetime"=ymd(strptime(paste0(year,"-", mon,"-", yday),
                                 format = "%Y-%m-%j"))) %>%
  select(datetime,year,mon,yday,t.avg:temp.7.max) %>%  
  as.data.frame()


# Make some Plots ---------------------------------------------------------

# Plot the Daily Temperature  
ggplot() + 
  geom_ribbon(data=df.dy, aes(datetime,ymax=temp.7.max,ymin=temp.7.min),
              fill="gray70",colour="gray60",lty=2,size=0.7,alpha=0.4)+
  geom_line(data=df.dy,aes(datetime,temp.7.avg), col="maroon",size=1)+
  ylab(expression(paste("Avg Daily Water Temperature ( ",degree,"C)"))) +
  theme_bw() +
  ggtitle(paste0(site,": 7-Day Avg Water Temperature"))

ggplot() + 
  geom_line(data=ibuttons[month(ibuttons$datetime_s) < 9,], aes(x=datetime_s,y=Temp_C, color=Temp_C)) + 
  xlab("") + ylab(expression(paste("Temperature (",degree,"C)")))+theme_bw() + scale_color_viridis()

# also you should check out the purrr package...it is pretty slick at applying looping type "apply" functions over all kinds of things. Pretty sweet.





