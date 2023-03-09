# This code is to pull data from the PME DO sensor

# Required libraries
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(latex2exp)
library(ggplot2)
library(gridExtra)
library(devtools)
install_github("LimpopoLab/hydrostats")
library(hydrostats)

fn <- "151899.txt" # remember to add file extension
site <- "Kruger_Gate_Weir/" # remember to add trailing /
dir <- "/Users/davidkahler/Documents/Hydrology_and_WRM/Limpopo_Basin_Study/LimpopoData/" # remember to add trailing /
d <- read_csv(paste0(dir,site,fn), skip = 9, col_names = FALSE)

# note columns
# Unix Timestamp, UTC_Date_&_Time, Central African Time, Battery, Temperature, Dissolved Oxygen, Dissolved Oxygen Saturation, Q
# (Second), (none), (none), (Volt), (deg C), (mg/l), (%), (none)
d <- rename(d, unix=X1, utc=X2, cat=X3, battery=X4, temperature=X5, do=X6, do_sat=X7, Q=X8)

# Align to standardized times - every 10 minutes
# Moves, does not interpolate data.
for (i in 1:nrow(d)) {
     yr <- year(as_datetime(d$unix[i]))
     mo <- month(as_datetime(d$unix[i]))
     dy <- day(as_datetime(d$unix[i]))
     hr <- hour(as_datetime(d$unix[i]))
     mn <- 10 * round( (minute(as_datetime(d$unix[i]))) / 10 )
     if (mn > 50) {
          mn <- 0
          hr <- hr + 1
     }
     d$dt[i] <- ymd_hms(paste0(yr,"-",mo,"-",dy,"T",hr,"-",mn,"-00"))
     #d$sast[i] <- with_tz(as_datetime(d$unix[i]), tzone = "Africa/Johannesburg")
}

temp <- ggplot(d) +
     geom_line(aes(x=cat,y=temperature)) +
     ylab(TeX('Temperature $(^o C)$')) +
     xlab(element_blank()) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 0.4) +
     theme(axis.text = element_text(face = "plain", size = 12))
do <- ggplot(d) +
     geom_line(aes(x=cat,y=do)) +
     xlab("Date (interval = 10 minutes)") +
     ylab(TeX('Dissolved Oxygen $(mg/l)$')) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 0.4) +
     theme(axis.text = element_text(face = "plain", size = 12))
gtemp <- ggplotGrob(temp)
gdo <- ggplotGrob(do)
grid::grid.newpage()
do_out <- grid::grid.draw(rbind(gtemp,gdo))
ggsave(paste0(dir,site,"do.eps"), do_out, device = "eps", dpi = 72)
rm(temp,gtemp,do,gdo)


#month_trend$label <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#month_trend$label <- factor(month_trend$label, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

# with_tz(as_datetime(x$X2[i]), tzone = "Africa/Johannesburg")




