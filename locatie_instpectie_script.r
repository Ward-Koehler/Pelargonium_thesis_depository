library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(writexl)

# =========================
# 1. Inlezen bestanden
# =========================

# long-format dataset
spur_field_only_species <- read_excel("Spurs_field_only_species.xlsx", sheet = 1)
View(spur_field_only_species)
# collapsed dataset met coördinaten
# pas bestandsnaam en sheet aan indien nodig
collapsed <- read_excel("collapsed_dataset.xlsx", sheet = 2)
View(collapsed)

# =========================
# 2. Locatie-library maken
# =========================

location_lib <- tibble(
  location_id = 1:19,
  Location_name = c(
    "Helderberg Nature Reserve",
    "Kirstenbosch",
    "Rondebosch Common",
    "Signal Hill and Lion's Head",
    "Silvermine",
    "Kogelberg",
    "Cederbergen",
    "Robertson",
    "Rhodes Memorial",
    "Cape Point",
    "Meadowridge Common",
    "West Coast National Park",
    "Witzenberg Valley",
    "Elandsberg Reserve",
    "Worcester",
    "Bain's Kloof",
    "Cape Town",
    "Jonkershoek",
    "Newlands Ravine"
  )
)

# =========================
# 3. Species-library maken
#    op basis van volgorde
#    in de Excel-file
# =========================
species_lib <- spur_field_only_species %>%
  distinct(Species) %>%
  mutate(species_id = row_number()) %>%
  select(species_id, Species)

# =========================
# 4. Location-code splitsen
#    vb. 1.1 = locatie 1, species 1
# =========================

spur_long_linked <- spur_field_only_species %>%
  mutate(
    location_code = as.character(Location),
    location_id = as.integer(str_extract(location_code, "^[0-9]+")),
    species_id  = as.integer(str_extract(location_code, "(?<=\\.)[0-9]+"))
  ) %>%
  left_join(location_lib, by = "location_id") %>%
  left_join(species_lib, by = "species_id", suffix = c("", "_from_code"))

View(spur_long_linked)
# optioneel: check of speciescode overeenkomt met Species-kolom
mismatch <- spur_long_linked %>%
  filter(!is.na(Species_from_code), Species != Species_from_code)

if(nrow(mismatch) > 0){
  message("Let op: er zijn speciescodes die niet overeenkomen met de Species-kolom.")
  print(mismatch)
}

# =========================
# 5. Collapsed dataset opschonen
#    Verwacht kolommen zoals:
#    Location, Species,
#    South_deg, South_min, South_sec,
#    East_deg, East_min, East_sec
# =========================

# PAS DEZE KOLOMNAMEN AAN AAN JOUW BESTAND
View(collapsed)
collapsed_clean <- collapsed %>%
  mutate(
    Species = Species %>%
      str_remove("^P\\.\\s*") %>%   # verwijder "P."
      str_squish() %>%             # trim + dubbele spaties weg
      str_replace_all("\\s+", "") %>%  # alle spaties eruit
      paste0("P. ", .)             # "P." weer toevoegen
  )  %>% 

  rename(
    Locatie_raw = 'Location',
    Species_raw = "Species",
    south_deg = "Degrees South",
    south_min = 'Minutes South',
    south_sec = "Seconds South",
    east_deg  = "Degrees East",
    east_min  = "Minutes East",
    east_sec  = "Seconds East"
  ) %>%
  mutate(
    Location_name = str_squish(Locatie_raw),
    Species = str_squish(Species_raw),

    # decimal degrees
    latitude  = -(as.numeric(south_deg) + as.numeric(south_min)/60 + as.numeric(south_sec)/3600),
    longitude =  (as.numeric(east_deg) + as.numeric(east_min)/60  + as.numeric(east_sec)/3600)
  ) %>%
  mutate(
    location_id = match(Location_name, unique(Location_name))
  ) %>%
  group_by(Location_name) %>%
  mutate(
    row_id_within_location = row_number()
  ) %>%
  ungroup() %>%
  mutate(
    Location = paste0(location_id, ".", row_id_within_location)
  ) %>% 
  distinct(Location_name, Species, latitude, longitude, Location)
View(collapsed_clean)

# =========================
# 6. Join coördinaten aan long dataset
# =========================
View(spur_long_linked)

spur_field_only_species_with_coords <- spur_long_linked %>%
  left_join(
    collapsed_clean2,
    by = c("Location")
  )

View(spur_field_only_species_with_coords)
# =========================
# 7. Resultaat opslaan
# =========================

write_xlsx(
  spur_field_only_species_with_coords,
  "Spurs_field_only_species_with_coordinates.xlsx"
)

# =========================
# visualiseren op kaart (optioneel)
# =========================

library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# inlezen
dat <- read_excel("Spurs_field_only_species_with_coordinates.xlsx")
View(dat)
# unieke locaties
locs <- dat %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  distinct(Locatie, latitude, longitude)

# maak sf object
locs_sf <- st_as_sf(
  locs,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# landkaart
world <- ne_countries(scale = "medium", returnclass = "sf")

# achtergrondkaart
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world, fill = "grey97", color = "grey75", linewidth = 0.2) +
  geom_sf(aes(color = Locatie), data = locs_sf, size = 3) +
  coord_sf(
    xlim = c(17.5, 19.8),
    ylim = c(-34.8, -32.3),
    expand = FALSE
  ) +
  labs(
    title = "Sampling locations",
    x = "Longitude",
    y = "Latitude",
    color = "Location"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

# =========================
# kaart met leaflet (optioneel)
# =========================

library(readxl)
library(dplyr)
library(leaflet)

# inlezen
dat <- read_excel("Spurs_field_only_species_with_coordinates.xlsx")

# unieke locaties
locs <- dat %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(Locatie)) %>%
  distinct(Locatie, latitude, longitude)

# kleuren per locatie
pal <- colorFactor(
  palette = "Set1",
  domain = locs$Locatie
)

# interactieve kaart
leaflet(locs) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    color = ~pal(Locatie),
    fillColor = ~pal(Locatie),
    fillOpacity = 0.9,
    radius = 6,
    stroke = TRUE,
    weight = 1,
    popup = ~paste0(
      "<b>Locatie:</b> ", Locatie, "<br>",
      "<b>Latitude:</b> ", round(latitude, 5), "<br>",
      "<b>Longitude:</b> ", round(longitude, 5)
    )
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~Locatie,
    title = "Location",
    opacity = 1
  )


# ========================
# inclusief observaties
# =========================

obs <- dat %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(Locatie))

pal <- colorFactor("Set1", domain = obs$Locatie)

leaflet(obs) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    color = ~pal(Locatie),
    fillColor = ~pal(Locatie),
    fillOpacity = 0.7,
    radius = 5,
    stroke = TRUE,
    weight = 1,
    popup = ~paste0(
      "<b>Locatie:</b> ", Locatie, "<br>",
      "<b>Species:</b> ", Species.x, "<br>",
      "<b>Latitude:</b> ", round(latitude, 5), "<br>",
      "<b>Longitude:</b> ", round(longitude, 5)
    )
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~Locatie,
    title = "Location"
  )
