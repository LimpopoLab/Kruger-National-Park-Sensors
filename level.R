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
ec <- read_csv(paste0(dir,site,fn), skip = 2, col_names = FALSE)

# note columns
# "#","Date Time, GMT+02:00","Full Range, μS/cm (LGR S/N: 21362347, SEN S/N: 21362347)","Temp, °C (LGR S/N: 21362347, SEN S/N: 21362347)","Coupler Detached (LGR S/N: 21362347)","Coupler Attached (LGR S/N: 21362347)","Host Connected (LGR S/N: 21362347)","End Of File (LGR S/N: 21362347)"
ec <- ec %>%
     rename(loc=X2, ec=X3, temperature=X4) %>% # local datetime in SAST - should check at each download
     mutate(sast=force_tz(mdy_hms(loc), tzone = "Africa/Johannesburg")) %>%
     mutate(utc=as_datetime(sast)) %>%
     select(utc,sast,temperature,ec)

temp <- ggplot(ec) +
     geom_line(aes(x=sast,y=temperature)) +
     ylab(TeX('Temperature $(^o C)$')) +
     xlab(element_blank()) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 0.4) +
     theme(axis.text = element_text(face = "plain", size = 12))
cond <- ggplot(ec) +
     geom_line(aes(x=sast,y=ec)) +
     xlab("Date (interval = 10 minutes)") +
     ylab(TeX("Conductivity %({\mu} S/cm)$")) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 0.4) +
     theme(axis.text = element_text(face = "plain", size = 12))
gtemp <- ggplotGrob(temp)
gcond <- ggplotGrob(cond)
grid::grid.newpage()
cond_out <- grid::grid.draw(rbind(gtemp,gcond))
ggsave(paste0(dir,site,"cond.eps"), cond_out, device = "eps", dpi = 72)
rm(temp,gtemp,cond,gcond)