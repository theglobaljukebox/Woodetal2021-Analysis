library(googlesheets4)
library(ggplot2)
library(dplyr)
library(stringr)

gjb_metadata = read_sheet("https://docs.google.com/spreadsheets/d/1tb3Nip43e4LaJbglaXzcCTP2CiMyrgwIsU2egk3tfNM/edit#gid=1190601304")
gjb_metadata$plot_longitude = ifelse(gjb_metadata$Society_longitude <= -25, 
                                     gjb_metadata$Society_longitude + 360, 
                                     gjb_metadata$Society_longitude)

# get correct column type
is.na(gjb_metadata$cantometrics_samplesize) = lengths(gjb_metadata$cantometrics_samplesize) == 0 
gjb_metadata$cantometrics_samplesize = as.numeric(unlist(gjb_metadata$cantometrics_samplesize))

#### Data sets ####
cantometrics = gjb_metadata[!is.na(gjb_metadata$C_cid), c("Society_latitude", "plot_longitude", "cantometrics_samplesize", "society_id")]
minutage = gjb_metadata[!is.na(gjb_metadata$M_cid), c("Society_latitude", "plot_longitude", "minutage_samplesize", "society_id")]
phonotactics = gjb_metadata[!is.na(gjb_metadata$Ph_cid), c("Society_latitude", "plot_longitude", "phonotactics_samplesize", "society_id")]
parlametrics = gjb_metadata[!is.na(gjb_metadata$P_cid), c("Society_latitude", "plot_longitude", "parlametrics_samplesize", "society_id")]


# make instrument and ensemble datasets
instrument_ids = read_sheet("https://docs.google.com/spreadsheets/d/1_efOLdVyGgXhBpqW2feW8YZQnLTZfleBCbyZjdX5X8A/edit#gid=1505037649", 
                            sheet = "Instruments Metadata", range = "A2:O1782") %>% 
  pull("Possible_GJB_All_cid_Matches") %>% 
  gsub("^(.*?),.*", "\\1", .)

instrument_samplesize = as.data.frame(table(instrument_ids))
instrument_samplesize$instrument_ids = as.numeric(as.character(instrument_samplesize$instrument_ids))
instruments = left_join(instrument_samplesize, gjb_metadata, by = c("instrument_ids" = "society_id")) %>% 
  select(c("Society_latitude", "plot_longitude", "Freq", "instrument_ids"))

ensemble_ids = read_sheet("https://docs.google.com/spreadsheets/d/1_efOLdVyGgXhBpqW2feW8YZQnLTZfleBCbyZjdX5X8A/edit#gid=1505037649", 
                            sheet = "Ensembles Metadata", range = "A2:M778") %>% 
  pull("Possible_GJB_All_cid_Matches") %>%
  gsub("^(.*?),.*", "\\1", .)
ensembles_samplesize = as.data.frame(table(ensemble_ids))
ensembles_samplesize$ensemble_ids = as.numeric(as.character(ensembles_samplesize$ensemble_ids))
ensembles = left_join(ensembles_samplesize, gjb_metadata, by = c("ensemble_ids" = "society_id")) %>% 
  select(c("Society_latitude", "plot_longitude", "Freq", "ensemble_ids"))

#### Maps ####
world <- ggplot2::map_data('world2', 
                           wrap=c(-25,335), #rewrapping the worldmap, i.e. shifting the center. 
                           ylim=c(-55,90)) #cutting out antarctica (not obligatory) and the northermost part where there are no language points in glottolog

basemap <- ggplot() +
  geom_polygon(data=world, aes(x=long, #plotting the landmasses
                               y=lat,group=group),
               colour="gray90",
               fill="gray90", size = 0.5) +
  theme(#all of theme options are set such that it makes the most minimal plot, no legend, not grid lines etc
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.line = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0, "mm"))   +
    coord_map(projection = "vandergrinten") + #a non-rectangular world map projection that is a decen compromise between area and distances accuracy
  ylim(-55,90) #cutting out antarctica (not obligatory) 


map_cantometrics = basemap + geom_point(data = cantometrics,
                        aes(x=plot_longitude, y=Society_latitude, size = cantometrics_samplesize), 
                        shape = 21, 
                        alpha = 0.6, 
                        stroke = 0.4, 
                        fill = "#ED5C4D") + 
                  scale_size_continuous(range = c(1, 5)) + 
  theme(legend.position = "bottom", legend.title=element_blank(), text = element_text(size=20))

map_minutage = basemap + geom_point(data = minutage,
                                        aes(x=plot_longitude, y=Society_latitude, size = minutage_samplesize), 
                                        shape = 21, 
                                        alpha = 0.6, 
                                        stroke = 0.4, 
                                        fill = "#9AAE09") + 
                                        scale_size_continuous(range = c(1, 5)) + 
  theme(legend.position = "bottom", legend.title=element_blank())


map_phonotactics = basemap + geom_point(data = phonotactics, 
                                        aes(x=plot_longitude, y=Society_latitude, size = phonotactics_samplesize), 
                                        shape = 21, 
                                        alpha = 0.6, 
                                        stroke = 0.4, 
                                        fill = "#0050BC") + 
  theme(legend.position = "bottom", legend.title=element_blank())

map_instrument = basemap + geom_point(data = instruments,
                                        aes(x=plot_longitude, y=Society_latitude, size = Freq), 
                                        shape = 21, 
                                        alpha = 0.6, 
                                        stroke = 0.4, 
                                        fill = "#FFC876") + 
  theme(legend.position = "bottom", legend.title=element_blank())

map_ensembles = basemap + geom_point(data = ensembles,
                                      aes(x=plot_longitude, y=Society_latitude, size = Freq), 
                                      shape = 21, 
                                      alpha = 0.6, 
                                      stroke = 0.4, 
                                      fill = "#FFC875") + 
  theme(legend.position = "bottom", legend.title=element_blank())


map_parlametrics = basemap + geom_point(data = parlametrics,
                                      aes(x=plot_longitude, y=Society_latitude, size = parlametrics_samplesize), 
                                      shape = 21, 
                                      alpha = 0.6, 
                                      stroke = 0.4, 
                                      fill = "#9A576E") +
                              scale_size_continuous(range = c(1, 5)) + 
                              theme(legend.position = "bottom", legend.title=element_blank())

# pdf('database_maps.pdf', width = 8, height = 7)
# individual maps
jpeg("figs/cantometrics.jpeg")
map_cantometrics
dev.off()

pdf("figs/cantometrics.pdf", width = 13.3)
map_cantometrics
dev.off()

jpeg("figs/minutage.jpeg")
map_minutage
dev.off()

jpeg("figs/phonotactics.jpeg")
map_phonotactics
dev.off()

jpeg("figs/instrument.jpeg")
map_instrument
dev.off()

jpeg("figs/ensemble.jpeg")
map_ensembles
dev.off()


jpeg("figs/parlametrics.jpeg")
map_parlametrics
dev.off()
