# This code is to pull data from the HOBO water level logger

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

fn <- "Kruger_Gate_level.csv" # remember to add file extension
site <- "Kruger_Gate_Weir/" # remember to add trailing /
dir <- "/Users/davidkahler/Documents/Hydrology_and_WRM/Limpopo_Basin_Study/LimpopoData/" # remember to add trailing /
level <- read_csv(paste0(dir,site,fn), skip = 2, col_names = FALSE, col_types = "icddccccc")

# note columns
# "#","Date Time, GMT+02:00","Full Range, μS/cm (LGR S/N: 21362347, SEN S/N: 21362347)","Temp, °C (LGR S/N: 21362347, SEN S/N: 21362347)","Coupler Detached (LGR S/N: 21362347)","Coupler Attached (LGR S/N: 21362347)","Host Connected (LGR S/N: 21362347)","End Of File (LGR S/N: 21362347)"
level <- level %>%
     rename(loc=X2, lev=X3, temperature=X4) %>% # local datetime in SAST - should check at each download, pressure in absolute, kPa
     mutate(sast=force_tz(mdy_hms(loc), tzone = "Africa/Johannesburg")) %>%
     mutate(utc=as_datetime(sast)) %>%
     mutate(height=(1000*(lev-98))/(9.8*997)) %>% # height under water
     select(utc,sast,temperature,lev,height)

temp <- ggplot(level) +
     geom_line(aes(x=sast,y=temperature)) +
     ylab(TeX('Temperature $(^o C)$')) +
     xlab(element_blank()) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 0.4) +
     theme(axis.text = element_text(face = "plain", size = 12))
lev <- ggplot(level) +
     geom_line(aes(x=sast,y=height)) +
     xlab("Date (interval = 10 minutes)") +
     ylab("Depth (m)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 0.4) +
     theme(axis.text = element_text(face = "plain", size = 12))
gtemp <- ggplotGrob(temp)
glev <- ggplotGrob(lev)
grid::grid.newpage()
lev_out <- grid::grid.draw(rbind(gtemp,glev))
ggsave(paste0(dir,site,"lev.eps"), lev_out, device = "eps", dpi = 72)
rm(temp,gtemp,lev,glev)

# Temperature and flow analysis
# appears to have a reciprocal relationship in late December and early January
range <- c(ymd_hms("2022-12-16T00:00:00"),as.numeric(ymd_hms("2023-01-16T00:00:00")))
temp <- ggplot(level) +
     geom_line(aes(x=sast,y=temperature)) +
     ylab(TeX('Temperature $(^o C)$')) +
     xlab(element_blank()) +
     xlim(range) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 0.4) +
     theme(axis.text = element_text(face = "plain", size = 12))
lev <- ggplot(level) +
     geom_line(aes(x=sast,y=height)) +
     xlab("Date (interval = 10 minutes)") +
     ylab("Depth (m)") +
     xlim(range) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 0.4) +
     theme(axis.text = element_text(face = "plain", size = 12))
gtemp <- ggplotGrob(temp)
glev <- ggplotGrob(lev)
grid::grid.newpage()
lev_out <- grid::grid.draw(rbind(gtemp,glev))




