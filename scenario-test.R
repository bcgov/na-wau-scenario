# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.



source('header.R')
source('packages.R')

# Load basic watershed file

wau_base <- st_read("data/aqua_sfE.gpkg", crs=3005) %>%
  st_cast(to="POLYGON") %>%
  st_make_valid()


ecoprov <- bcmaps::ecoprovinces() %>%
  st_make_valid() %>%
  st_collection_extract(type = "POLYGON") %>%
  mutate(ecoprovince_area = st_area(.),
         ecoprovince_area  =as.numeric(set_units(ecoprovince_area, ha))) %>%
  select(c(ECOPROVINCE_NAME, ecoprovince_area))

nr_area <- bcmaps::nr_areas() %>%
  st_make_valid() %>%
  st_collection_extract(type = "POLYGON") %>%
  mutate(nr_area = st_area(.),
         nr_area  =as.numeric(set_units(nr_area, ha))) %>%
  select(c(AREA_NAME, nr_area))


intersect_pa <- function(input1, input2){
  input1 <- st_make_valid(input1)
  input2 <- st_make_valid(input2)
  output <- st_intersection(input1, input2) %>%
    st_make_valid() %>%
    st_collection_extract(type = "POLYGON") %>%
    mutate(polygon_id = seq_len(nrow(.)))
  output
}

wau_scenario <- intersect_pa(wau_base, nr_area)

wau_full <- intersect_pa(wau_scenario, ecoprov)

wau_full <- wau_full %>%
  relocate(c(ECOPROVINCE_NAME, ecoprovince_area, AREA_NAME, nr_area), .after= ASSESSMENT_UNIT_SOURCE_ID) %>%
  group_by(ASSESSMENT_UNIT_GROUP, ecoprovince_area) %>%
  summarise(ecoprovince_area = sum(ecoprovince_area),
            nr_area=sum(nr_area))

saveRDS(wau_full, file = 'out/wau_full')

write_sf(wau_full, "out/wau_full.gpkg")


wau_full <- st_read("out/wau_full.gpkg", crs=3005) %>%
  st_cast(to="POLYGON") %>%
  st_make_valid()

ch_overlap <- st_read("c:/tmp/og-env-analysis/CriticalHabitat.gdb", layer='CriticalHabitats_0826', crs=3005)

ch_overlap <- ch_overlap %>%
  rename_all(tolower) %>%
  st_cast(to = "MULTIPOLYGON", warn = FALSE) %>%
  st_make_valid() %>%
  st_cast(to = "POLYGON", warn = FALSE)


ch_flat <- st_read("c:/tmp/og-env-analysis/CriticalHabitat.gdb", layer='critical_flat', crs=3005)

ch_flat <- ch_flat %>%
  rename_all(tolower) %>%
  st_cast(to = "MULTIPOLYGON", warn = FALSE) %>%
  st_make_valid() %>%
  st_cast(to = "POLYGON", warn = FALSE)


wau_ch <- st_intersection(wau_full, ch_overlap)

write_sf(wau_ch, "out/wau_ch.gpkg")

saveRDS(wau_ch, file = 'out/wau_ch')

wau_ch<- readRDS(file = 'out/wau_ch.rds')

wau_ch_sum <- wau_ch %>%
  mutate(ch_area = as.numeric(st_area(.)),
         ch_area = as.numeric(set_units(ch_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(aqua_id) %>%
  mutate(n_species = n_distinct(common_name_english)) %>%
  group_by(aqua_id, n_species, scientific_name, common_name_english) %>%
  summarise(species_area = sum(ch_area)) %>%
  group_by(aqua_id) %>%
  mutate(species_scientific = paste0(scientific_name, collapse = ", ")) %>%
  mutate(species_common = paste0(common_name_english, collapse = ", ")) %>%
  group_by(aqua_id, n_species, species_scientific, species_common) %>%
  summarise(ch_area_by_wau = sum(species_area))


wau_ch_species_table <- wau_ch %>%
  mutate(ch_area = as.numeric(st_area(.)),
         ch_area = as.numeric(set_units(ch_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(aqua_id) %>%
  mutate(n_species = n_distinct(common_name_english)) %>%
  group_by(aqua_id, n_species, scientific_name, common_name_english) %>%
  summarise(species_area = sum(ch_area))

saveRDS(wau_ch_species_table, 'out/wau_ch_species_table.rds')

summary <- wau_ch_sum %>%
  group_by(n_species) %>%
  summarise(n_wau = n())


#not working invalid geometry

herd_areas<- function(herd){
  area <- st_read("C:/tmp/og-env-analysis/data/Priority_Final_Flat_2021.gdb", layer=herd, crs=3005) %>%
    st_make_valid() %>%
    st_cast(to = "MULTIPOLYGON", warn = FALSE)
  area
}

barkerville <- herd_areas('Barkerville_final_flat')

barkerville <- st_read("data/priority-caribou/barkerville.gpkg", layer=output, crs=3005) %>%
  st_zm() %>%
  st_make_valid() %>%
  st_cast(to = "POLYGON", warn = FALSE) %>%
barkerville_sum <- barkerville %>%
  mutate(hab_area = st_area(.),
         hab_area = as.numeric(set_units(hab_area, ha)))
  group_by(BCHab_code) %>%
  summarise(hab_area_type = sum(hab_area))
