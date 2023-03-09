# This code is to pull data from the PME C7 Turbidity sensor

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

fn <- "151000.txt" # remember to add file extension
site <- "Kruger_Gate_Weir/" # remember to add trailing /
dir <- "/Users/davidkahler/Documents/Hydrology_and_WRM/Limpopo_Basin_Study/LimpopoData/" # remember to add trailing /
t <- read_csv(paste0(dir,site,fn), skip = 9, col_names = FALSE)

# note columns
# Unix Timestamp, UTC_Date_&_Time, Central African Time, Battery, Temperature, Sensor, Gain
# (Second), (none), (none), (Volt), (deg C), (test), ()
t <- rename(t, unix=X1, utc=X2, cat=X3, battery=X4, temperature=X5, turbidity=X6, gain=X7)

# Align to standardized times - every 10 minutes
# Moves, does not interpolate data.
for (i in 1:nrow(t)) {
     yr <- year(as_datetime(t$unix[i]))
     mo <- month(as_datetime(t$unix[i]))
     dy <- day(as_datetime(t$unix[i]))
     hr <- hour(as_datetime(t$unix[i]))
     mn <- 10 * round( (minute(as_datetime(t$unix[i]))) / 10 )
     if (mn > 50) {
          mn <- 0
          hr <- hr + 1
     }
     t$dt[i] <- ymd_hms(paste0(yr,"-",mo,"-",dy,"T",hr,"-",mn,"-00"))
}

temp <- ggplot(t) +
     geom_line(aes(x=cat,y=temperature)) +
     ylab(TeX('Temperature $(^o C)$')) +
     xlab(element_blank()) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 0.4) +
     theme(axis.text = element_text(face = "plain", size = 12))
turbidity <- ggplot(t) +
     geom_line(aes(x=cat,y=turbidity)) +
     xlab(element_blank()) +
     ylab("Turbidity (uncalibrated)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 0.4) +
     theme(axis.text = element_text(face = "plain", size = 12))
gain <- ggplot(t) +
     geom_point(aes(x=cat,y=gain)) +
     xlab("Date (interval = 10 minutes)") +
     ylab("Gain") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 0.4) +
     theme(axis.text = element_text(face = "plain", size = 12))
gtemp <- ggplotGrob(temp)
gturb <- ggplotGrob(turbidity)
ggain <- ggplotGrob(gain)
grid::grid.newpage()
turb_out <- grid::grid.draw(rbind(gtemp,gturb))
ggsave(paste0(dir,site,"turb.eps"), turb_out, device = "eps", dpi = 72)
rm(temp,gtemp,turbidity,gturb,gain,ggain)


# COMPARE temperature records
# needs to transition to full, independent code
plot(d$temperature[6:16541],t$temperature)


