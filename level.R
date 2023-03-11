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

# plot
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

# Format for repository
offsetHRS <- 2 # could look at an automated way to populate this in the future.
levCUAHSI <- level %>%
     mutate(LocalDateTime=as.character(sast)) %>%
     mutate(UTCOffset=offsetHRS) %>%
     mutate(DateTimeUTC=as.character(utc)) %>%
     mutate(WTMP=temperature) %>%
     select(LocalDateTime,UTCOffset,DateTimeUTC,WTMP,RIVS) %>%
     pivot_longer(c(WTMP,RIVS), names_to = "VariableCode", values_to = "DataValue")
# Sabie River sites:
st <- "Kruger Gate Weir"
# st <- "Lower Sabie"
# Olifants River sites:
# st <- "Mamba Weir"
# st <- "Balule"
SiteCode = array(st, dim=nrow(cuahsi))
MethodCode = array("HOBO Water Level U20L-01", dim=nrow(cuahsi))
SourceCode = array("LimpopoResilienceLab", dim=nrow(cuahsi))
QualityControlLevelCode = array("", dim=nrow(cuahsi))
for (i in 1:nrow(cuahsi)) {
     if (cuahsi$VariableCode[i] == "WTMP") {
          QualityControlLevelCode[i] <- 0
     } else if (cuahsi$VariableCode[i] == "RIVS") {
          QualityControlLevelCode[i] <- 2
     }
}

upload <- data.frame(cuahsi$DataValue,cuahsi$LocalDateTime,cuahsi$UTCOffset,cuahsi$DateTimeUTC,SiteCode,cuahsi$VariableCode,MethodCode,SourceCode,QualityControlLevelCode)
upload <- upload %>%
     rename(
          DataValue=cuahsi.DataValue,
          LocalDateTime=cuahsi.LocalDateTime,
          UTCOffset=cuahsi.UTCOffset,
          DateTimeUTC=cuahsi.DateTimeUTC,
          VariableCode=cuahsi.VariableCode
     )
today <- Sys.Date()
h <- names(upload)
s <- length(h)
hl <- array("a", dim = c(1,s))
for (i in 1:length(h)) {
     hl[1,i] <- h[i]
}
hl <- data.frame(hl)
write_csv(hl, paste0(st, "_", today, ".cuahsi.csv"), append = TRUE, eol = "\n") # writes headers
write_csv(upload, paste0(st, "_", today, ".cuahsi.csv"), na = "NA", append = TRUE, eol = "\n") # writes data, UNIX standard end of line, comma-delimited, decimal point used "."

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

st <- as.numeric(which(level$utc==range[1]))
en <- as.numeric(which(level$utc==range[2]))
zoom <- level[st:en,1:5]
ggplot(zoom) +
     geom_point(aes(x=height,y=temperature)) +
     xlim(c(0.5,1.0)) +
     ylim(c(21,31)) +
     xlab("Depth (m)") +
     ylab(TeX('Temperature $(^o C)$')) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))







