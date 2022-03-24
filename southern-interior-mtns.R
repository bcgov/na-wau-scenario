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

# Code for Southern Interior Mtns Scenario

wau_full <- st_read("out/wau_full.gpkg", crs=3005) %>%
  st_cast(to="POLYGON") %>%
  st_make_valid()

wau_sim <- wau_full %>%
  filter(ECOPROVINCE_NAME == "SOUTHERN INTERIOR MOUNTAINS")

write_sf(wau_sim, "out/wau_south-int-mtns.gpkg")

wau_base <- st_read("data/wshd_w_elements.gpkg", crs=3005) %>%
  st_cast(to="POLYGON") %>%
  st_make_valid() %>%
  st_drop_geometry() %>%
  select(aqua_id, UnProtectedArea_ha:wetlands_haclassN)

# any ch present



wau_ch_species_no <- readRDS('out/wau_ch_species_table.rds')

wau_intact_scenario <- wau_sim %>%
  left_join(wau_base, by= "aqua_id") %>%
  left_join(wau_ch_species_no, by="aqua_id") %>%
  filter(common_name_english != "Caribou - Southern Mountain population") %>%
  mutate(percent_intact = round(Intact_ha/ASSESSMENT_UNIT_AREA_HA*100, digit=0))


write_sf(wau_intact_scenario, "out/wau_south-int-mtns_intact.gpkg")

wau_ch <- wau_intact_scenario %>%
  filter(n_species >= 1)

write_sf(wau_ch, "out/wau_ch_south-int-mtns_intact.gpkg")

wau_intact_50 <- wau_intact_scenario %>%
  filter(percent_intact >50) %>%
  mutate(contiguous_area = st_area(.),
         contiguous_area = as.numeric(set_units(contiguous_area, ha)),
         wau = n_distinct(aqua_id),
         max_contiguous = max(contiguous_area),
         total_area=sum(ASSESSMENT_UNIT_AREA_HA)) %>%
  group_by(contiguous_area, max_contiguous, total_area, wau) %>%
  summarise(n_wau = n()) %>%
  arrange(desc(contiguous_area), desc(n_wau))

intact_50_list <-  wau_intact_scenario %>%
  filter(percent_intact >50) %>%
  pull(unique(aqua_id))

wau_intact_50_sum <- wau_intact_50 %>%
  st_drop_geometry() %>%
  arrange(desc(contiguous_area), desc(n_wau)) %>%
  dplyr::slice_head(n=1)


write_sf(wau_intact_50, "out/wau_intact_50.gpkg")

#WAU 60
wau_intact_60 <- wau_intact_scenario %>%
  filter(percent_intact >60) %>%
  mutate(contiguous_area = st_area(.),
         contiguous_area = as.numeric(set_units(contiguous_area, ha)),
         wau = n_distinct(aqua_id),
         max_contiguous = max(contiguous_area),
         total_area=sum(ASSESSMENT_UNIT_AREA_HA)) %>%
  group_by(contiguous_area, max_contiguous, total_area, wau) %>%
  summarise(n_wau = n()) %>%
  arrange(desc(contiguous_area), desc(n_wau))

intact_60_list <-  wau_intact_scenario %>%
  filter(percent_intact >60) %>%
  pull(unique(aqua_id))

wau_intact_60_sum <- wau_intact_60 %>%
  st_drop_geometry() %>%
  arrange(desc(contiguous_area), desc(n_wau)) %>%
  dplyr::slice_head(n=1)


write_sf(wau_intact_60, "out/wau_intact_60.gpkg")


## WAU 70

wau_intact_70 <- wau_intact_scenario %>%
  filter(percent_intact >70) %>%
  mutate(contiguous_area = st_area(.),
         contiguous_area = as.numeric(set_units(contiguous_area, ha)),
         wau = n_distinct(aqua_id),
         max_contiguous = max(contiguous_area),
         total_area=sum(ASSESSMENT_UNIT_AREA_HA)) %>%
  group_by(contiguous_area, max_contiguous, total_area, wau) %>%
  summarise(n_wau = n()) %>%
  arrange(desc(contiguous_area), desc(n_wau))

intact_70_list <-  wau_intact_scenario %>%
  filter(percent_intact >70) %>%
  pull(unique(aqua_id))

wau_intact_70_sum <- wau_intact_70 %>%
  st_drop_geometry() %>%
  arrange(desc(contiguous_area), desc(n_wau)) %>%
  dplyr::slice_head(n=1)


write_sf(wau_intact_70, "out/wau_intact_70.gpkg")


## WAU 80

wau_intact_80 <- wau_intact_scenario %>%
  filter(percent_intact >80) %>%
  mutate(contiguous_area = st_area(.),
         contiguous_area = as.numeric(set_units(contiguous_area, ha)),
         wau = n_distinct(aqua_id),
         max_contiguous = max(contiguous_area),
         total_area=sum(ASSESSMENT_UNIT_AREA_HA)) %>%
  group_by(contiguous_area, max_contiguous, total_area, wau) %>%
  summarise(n_wau = n()) %>%
  arrange(desc(contiguous_area), desc(n_wau))

intact_80_list <-  wau_intact_scenario %>%
  filter(percent_intact >80) %>%
  pull(unique(aqua_id))

wau_intact_80_sum <- wau_intact_80 %>%
  st_drop_geometry() %>%
  arrange(desc(contiguous_area), desc(n_wau)) %>%
  dplyr::slice_head(n=1)


write_sf(wau_intact_80, "out/wau_intact_80.gpkg")


## WAU 90

wau_intact_90 <- wau_intact_scenario %>%
  filter(percent_intact >90) %>%
  mutate(contiguous_area = st_area(.),
         contiguous_area = as.numeric(set_units(contiguous_area, ha)),
         wau = n_distinct(aqua_id),
         max_contiguous = max(contiguous_area),
         total_area=sum(ASSESSMENT_UNIT_AREA_HA)) %>%
  group_by(contiguous_area, max_contiguous, total_area, wau) %>%
  summarise(n_wau = n()) %>%
  arrange(desc(contiguous_area), desc(n_wau))

intact_90_list <-  wau_intact_scenario %>%
  filter(percent_intact >90) %>%
  pull(unique(aqua_id))

wau_intact_90_sum <- wau_intact_90 %>%
  st_drop_geometry() %>%
  arrange(desc(contiguous_area), desc(n_wau)) %>%
  dplyr::slice_head(n=1)


write_sf(wau_intact_90, "out/wau_intact_90.gpkg")

######################################################################################

wau_intact_ch_50 <- wau_intact_scenario %>%
  filter(percent_intact >50) %>%
  filter(n_species >= 2) %>%
  mutate(contiguous_area = st_area(.),
         contiguous_area = as.numeric(set_units(contiguous_area, ha)),
         wau = n_distinct(aqua_id),
         max_contiguous = max(contiguous_area),
         total_area=sum(ASSESSMENT_UNIT_AREA_HA)) %>%
  group_by(contiguous_area, max_contiguous, total_area, wau) %>%
  summarise(n_wau = n()) %>%
  arrange(desc(contiguous_area), desc(n_wau))

intact_ch_50_list <-  wau_intact_scenario %>%
  filter(percent_intact >50) %>%
  filter(n_species >= 2) %>%
  pull(unique(aqua_id))

wau_intact_ch_50_sum <- wau_intact_ch_50 %>%
  st_drop_geometry() %>%
  arrange(desc(contiguous_area), desc(n_wau)) %>%
  dplyr::slice_head(n=1)


write_sf(wau_intact_ch_50, "out/wau_intact_ch_50.gpkg")

#WAU 60
wau_intact_ch_60 <- wau_intact_scenario %>%
  filter(percent_intact >60) %>%
  filter(n_species >= 2) %>%
  mutate(contiguous_area = st_area(.),
         contiguous_area = as.numeric(set_units(contiguous_area, ha)),
         wau = n_distinct(aqua_id),
         max_contiguous = max(contiguous_area),
         total_area=sum(ASSESSMENT_UNIT_AREA_HA)) %>%
  group_by(contiguous_area, max_contiguous, total_area, wau) %>%
  summarise(n_wau = n()) %>%
  arrange(desc(contiguous_area), desc(n_wau))

intact_ch_60_list <-  wau_intact_scenario %>%
  filter(percent_intact >60) %>%
  filter(n_species >= 2) %>%
  pull(unique(aqua_id))

wau_intact_ch_60_sum <- wau_intact_ch_60 %>%
  st_drop_geometry() %>%
  arrange(desc(contiguous_area), desc(n_wau)) %>%
  dplyr::slice_head(n=1)


write_sf(wau_intact_ch_60, "out/wau_intact_ch_60.gpkg")


## WAU 70

wau_intact_ch_70 <- wau_intact_scenario %>%
  filter(percent_intact >70) %>%
  filter(n_species >= 2) %>%
  mutate(contiguous_area = st_area(.),
         contiguous_area = as.numeric(set_units(contiguous_area, ha)),
         wau = n_distinct(aqua_id),
         max_contiguous = max(contiguous_area),
         total_area=sum(ASSESSMENT_UNIT_AREA_HA)) %>%
  group_by(contiguous_area, max_contiguous, total_area, wau) %>%
  summarise(n_wau = n()) %>%
  arrange(desc(contiguous_area), desc(n_wau))

intact_ch_70_list <-  wau_intact_scenario %>%
  filter(percent_intact >70) %>%
  filter(n_species >= 2) %>%
  pull(unique(aqua_id))

wau_intact_ch_70_sum <- wau_intact_ch_70 %>%
  st_drop_geometry() %>%
  arrange(desc(contiguous_area), desc(n_wau)) %>%
  dplyr::slice_head(n=1)


write_sf(wau_intact_ch_70, "out/wau_intact_ch_70.gpkg")


## WAU 80

wau_intact_ch_80 <- wau_intact_scenario %>%
  filter(percent_intact >80) %>%
  filter(n_species >= 2) %>%
  mutate(contiguous_area = st_area(.),
         contiguous_area = as.numeric(set_units(contiguous_area, ha)),
         wau = n_distinct(aqua_id),
         max_contiguous = max(contiguous_area),
         total_area=sum(ASSESSMENT_UNIT_AREA_HA)) %>%
  group_by(contiguous_area, max_contiguous, total_area, wau) %>%
  summarise(n_wau = n()) %>%
  arrange(desc(contiguous_area), desc(n_wau))

intact_ch_80_list <-  wau_intact_scenario %>%
  filter(percent_intact >80) %>%
  filter(n_species >= 2) %>%
  pull(unique(aqua_id))

wau_intact_ch_80_sum <- wau_intact_ch_80 %>%
  st_drop_geometry() %>%
  arrange(desc(contiguous_area), desc(n_wau)) %>%
  dplyr::slice_head(n=1)


write_sf(wau_intact_ch_80, "out/wau_intact_ch_80.gpkg")


## WAU 90

wau_intact_ch_90 <- wau_intact_scenario %>%
  filter(percent_intact >90) %>%
  filter(n_species >= 2) %>%
  mutate(contiguous_area = st_area(.),
         contiguous_area = as.numeric(set_units(contiguous_area, ha)),
         wau = n_distinct(aqua_id),
         max_contiguous = max(contiguous_area),
         total_area=sum(ASSESSMENT_UNIT_AREA_HA)) %>%
  group_by(contiguous_area, max_contiguous, total_area, wau) %>%
  summarise(n_wau = n()) %>%
  arrange(desc(contiguous_area), desc(n_wau))

intact_ch_90_list <-  wau_intact_scenario %>%
  filter(percent_intact >90) %>%
  filter(n_species >= 2) %>%
  pull(unique(aqua_id))

wau_intact_ch_90_sum <- wau_intact_ch_90 %>%
  st_drop_geometry() %>%
  arrange(desc(contiguous_area), desc(n_wau)) %>%
  dplyr::slice_head(n=1)

write_sf(wau_intact_ch_90, "out/wau_intact_ch_90.gpkg")

######################################################################################

wau_intact_ch_wr_50 <- wau_intact_scenario %>%
  filter(percent_intact >50) %>%
  filter(common_name_english == "Whitebark Pine") %>%
  mutate(contiguous_area = st_area(.),
         contiguous_area = as.numeric(set_units(contiguous_area, ha)),
         wau = n_distinct(aqua_id),
         max_contiguous = max(contiguous_area),
         total_area=sum(ASSESSMENT_UNIT_AREA_HA)) %>%
  group_by(contiguous_area, max_contiguous, total_area, wau) %>%
  summarise(n_wau = n()) %>%
  arrange(desc(contiguous_area), desc(n_wau))

intact_ch_wr_50_list <-  wau_intact_scenario %>%
  filter(percent_intact >50) %>%
  filter(common_name_english == "Whitebark Pine") %>%
  pull(unique(aqua_id))

wau_intact_ch_wr_50_sum <- wau_intact_ch_wr_50 %>%
  st_drop_geometry() %>%
  arrange(desc(contiguous_area), desc(n_wau)) %>%
  dplyr::slice_head(n=1)


write_sf(wau_intact_ch_wr_50, "out/wau_intact_ch_wr_50.gpkg")

#WAU 60
wau_intact_ch_wr_60 <- wau_intact_scenario %>%
  filter(percent_intact >60) %>%
  filter(common_name_english == "Whitebark Pine") %>%
  mutate(contiguous_area = st_area(.),
         contiguous_area = as.numeric(set_units(contiguous_area, ha)),
         wau = n_distinct(aqua_id),
         max_contiguous = max(contiguous_area),
         total_area=sum(ASSESSMENT_UNIT_AREA_HA)) %>%
  group_by(contiguous_area, max_contiguous, total_area, wau) %>%
  summarise(n_wau = n()) %>%
  arrange(desc(contiguous_area), desc(n_wau))

intact_ch_wr_60_list <-  wau_intact_scenario %>%
  filter(percent_intact >60) %>%
  filter(common_name_english == "Whitebark Pine") %>%
  pull(unique(aqua_id))

wau_intact_ch_wr_60_sum <- wau_intact_ch_wr_60 %>%
  st_drop_geometry() %>%
  arrange(desc(contiguous_area), desc(n_wau)) %>%
  dplyr::slice_head(n=1)


write_sf(wau_intact_ch_wr_60, "out/wau_intact_ch_wr_60.gpkg")


## WAU 70

wau_intact_ch_wr_70 <- wau_intact_scenario %>%
  filter(percent_intact >70) %>%
  filter(common_name_english == "Whitebark Pine") %>%
  mutate(contiguous_area = st_area(.),
         contiguous_area = as.numeric(set_units(contiguous_area, ha)),
         wau = n_distinct(aqua_id),
         max_contiguous = max(contiguous_area),
         total_area=sum(ASSESSMENT_UNIT_AREA_HA)) %>%
  group_by(contiguous_area, max_contiguous, total_area, wau) %>%
  summarise(n_wau = n()) %>%
  arrange(desc(contiguous_area), desc(n_wau))

intact_ch_wr_70_list <-  wau_intact_scenario %>%
  filter(percent_intact >70) %>%
  filter(common_name_english == "Whitebark Pine") %>%
  pull(unique(aqua_id))

wau_intact_ch_wr_70_sum <- wau_intact_ch_wr_70 %>%
  st_drop_geometry() %>%
  arrange(desc(contiguous_area), desc(n_wau)) %>%
  dplyr::slice_head(n=1)


write_sf(wau_intact_ch_wr_70, "out/wau_intact_ch_wr_70.gpkg")


## WAU 80

wau_intact_ch_wr_80 <- wau_intact_scenario %>%
  filter(percent_intact >80) %>%
  filter(common_name_english == "Whitebark Pine") %>%
  mutate(contiguous_area = st_area(.),
         contiguous_area = as.numeric(set_units(contiguous_area, ha)),
         wau = n_distinct(aqua_id),
         max_contiguous = max(contiguous_area),
         total_area=sum(ASSESSMENT_UNIT_AREA_HA)) %>%
  group_by(contiguous_area, max_contiguous, total_area, wau) %>%
  summarise(n_wau = n()) %>%
  arrange(desc(contiguous_area), desc(n_wau))

intact_ch_wr_80_list <-  wau_intact_scenario %>%
  filter(percent_intact >80) %>%
  filter(common_name_english == "Whitebark Pine") %>%
  pull(unique(aqua_id))

wau_intact_ch_wr_80_sum <- wau_intact_ch_wr_80 %>%
  st_drop_geometry() %>%
  arrange(desc(contiguous_area), desc(n_wau)) %>%
  dplyr::slice_head(n=1)


write_sf(wau_intact_ch_wr_80, "out/wau_intact_ch_wr_80.gpkg")


## WAU 90

wau_intact_ch_wr_90 <- wau_intact_scenario %>%
  filter(percent_intact >90) %>%
  filter(common_name_english == "Whitebark Pine") %>%
  mutate(contiguous_area = st_area(.),
         contiguous_area = as.numeric(set_units(contiguous_area, ha)),
         wau = n_distinct(aqua_id),
         max_contiguous = max(contiguous_area),
         total_area=sum(ASSESSMENT_UNIT_AREA_HA)) %>%
  group_by(contiguous_area, max_contiguous, total_area, wau) %>%
  summarise(n_wau = n()) %>%
  arrange(desc(contiguous_area), desc(n_wau))

intact_ch_wr_90_list <-  wau_intact_scenario %>%
  filter(percent_intact >90) %>%
  filter(common_name_english == "Whitebark Pine") %>%
  pull(unique(aqua_id))

wau_intact_ch_wr_90_sum <- wau_intact_ch_wr_90 %>%
  st_drop_geometry() %>%
  arrange(desc(contiguous_area), desc(n_wau)) %>%
  dplyr::slice_head(n=1)


######################################################################################

wau_intact_ch_og_50 <- wau_intact_scenario %>%
  filter(percent_intact >50) %>%
  filter(n_species >= 2) %>%
  filter(OG_ha > 0) %>%
  mutate(contiguous_area = st_area(.),
         contiguous_area = as.numeric(set_units(contiguous_area, ha)),
         wau = n_distinct(aqua_id),
         max_contiguous = max(contiguous_area),
         total_area=sum(ASSESSMENT_UNIT_AREA_HA)) %>%
  group_by(contiguous_area, max_contiguous, total_area, wau) %>%
  summarise(n_wau = n()) %>%
  arrange(desc(contiguous_area), desc(n_wau))

intact_ch_og_50_list <-  wau_intact_scenario %>%
  filter(percent_intact >50) %>%
  filter(n_species >= 2) %>%
  filter(OG_ha > 0) %>%
  pull(unique(aqua_id))

wau_intact_ch_og_50_sum <- wau_intact_ch_og_50 %>%
  st_drop_geometry() %>%
  arrange(desc(contiguous_area), desc(n_wau)) %>%
  dplyr::slice_head(n=1)


write_sf(wau_intact_ch_og_50, "out/wau_intact_ch_og_50.gpkg")

#WAU 60
wau_intact_ch_og_60 <- wau_intact_scenario %>%
  filter(percent_intact >60) %>%
  filter(n_species >= 2) %>%
  filter(OG_ha > 0) %>%
  mutate(contiguous_area = st_area(.),
         contiguous_area = as.numeric(set_units(contiguous_area, ha)),
         wau = n_distinct(aqua_id),
         max_contiguous = max(contiguous_area),
         total_area=sum(ASSESSMENT_UNIT_AREA_HA)) %>%
  group_by(contiguous_area, max_contiguous, total_area, wau) %>%
  summarise(n_wau = n()) %>%
  arrange(desc(contiguous_area), desc(n_wau))

intact_ch_og_60_list <-  wau_intact_scenario %>%
  filter(percent_intact >60) %>%
  filter(n_species >= 2) %>%
  filter(OG_ha > 0) %>%
  pull(unique(aqua_id))

wau_intact_ch_og_60_sum <- wau_intact_ch_og_60 %>%
  st_drop_geometry() %>%
  arrange(desc(contiguous_area), desc(n_wau)) %>%
  dplyr::slice_head(n=1)


write_sf(wau_intact_ch_og_60, "out/wau_intact_ch_og_60.gpkg")


## WAU 70

wau_intact_ch_og_70 <- wau_intact_scenario %>%
  filter(percent_intact >70) %>%
  filter(n_species >= 2) %>%
  filter(OG_ha > 0) %>%
  mutate(contiguous_area = st_area(.),
         contiguous_area = as.numeric(set_units(contiguous_area, ha)),
         wau = n_distinct(aqua_id),
         max_contiguous = max(contiguous_area),
         total_area=sum(ASSESSMENT_UNIT_AREA_HA)) %>%
  group_by(contiguous_area, max_contiguous, total_area, wau) %>%
  summarise(n_wau = n()) %>%
  arrange(desc(contiguous_area), desc(n_wau))

intact_ch_og_70_list <-  wau_intact_scenario %>%
  filter(percent_intact >70) %>%
  filter(n_species >= 2) %>%
  filter(OG_ha > 0) %>%
  pull(unique(aqua_id))

wau_intact_ch_og_70_sum <- wau_intact_ch_og_70 %>%
  st_drop_geometry() %>%
  arrange(desc(contiguous_area), desc(n_wau)) %>%
  dplyr::slice_head(n=1)


write_sf(wau_intact_ch_og_70, "out/wau_intact_ch_og_70.gpkg")


## WAU 80

wau_intact_ch_og_80 <- wau_intact_scenario %>%
  filter(percent_intact >80) %>%
  filter(n_species >= 2) %>%
  filter(OG_ha > 0) %>%
  mutate(contiguous_area = st_area(.),
         contiguous_area = as.numeric(set_units(contiguous_area, ha)),
         wau = n_distinct(aqua_id),
         max_contiguous = max(contiguous_area),
         total_area=sum(ASSESSMENT_UNIT_AREA_HA)) %>%
  group_by(contiguous_area, max_contiguous, total_area, wau) %>%
  summarise(n_wau = n()) %>%
  arrange(desc(contiguous_area), desc(n_wau))

intact_ch_og_80_list <-  wau_intact_scenario %>%
  filter(percent_intact >80) %>%
  filter(n_species >= 2) %>%
  filter(OG_ha > 0) %>%
  pull(unique(aqua_id))

wau_intact_ch_og_80_sum <- wau_intact_ch_og_80 %>%
  st_drop_geometry() %>%
  arrange(desc(contiguous_area), desc(n_wau)) %>%
  dplyr::slice_head(n=1)


write_sf(wau_intact_ch_og_80, "out/wau_intact_ch_og_80.gpkg")


## WAU 90

wau_intact_ch_og_90 <- wau_intact_scenario %>%
  filter(percent_intact >90) %>%
  filter(n_species >= 2) %>%
  filter(OG_ha > 0) %>%
  mutate(contiguous_area = st_area(.),
         contiguous_area = as.numeric(set_units(contiguous_area, ha)),
         wau = n_distinct(aqua_id),
         max_contiguous = max(contiguous_area),
         total_area=sum(ASSESSMENT_UNIT_AREA_HA)) %>%
  group_by(contiguous_area, max_contiguous, total_area, wau) %>%
  summarise(n_wau = n()) %>%
  arrange(desc(contiguous_area), desc(n_wau))

intact_ch_og_90_list <-  wau_intact_scenario %>%
  filter(percent_intact >90) %>%
  filter(n_species >= 2) %>%
  filter(OG_ha > 0) %>%
  pull(unique(aqua_id))

wau_intact_ch_og_90_sum <- wau_intact_ch_og_90 %>%
  st_drop_geometry() %>%
  arrange(desc(contiguous_area), desc(n_wau)) %>%
  dplyr::slice_head(n=1)

wau_id_common <- Reduce(intersect, list(intact_50_list, intact_60_list, intact_70_list, intact_80_list, intact_90_list,
                                        intact_ch_50_list, intact_ch_60_list, intact_ch_70_list, intact_ch_80_list, intact_ch_90_list,
                                        intact_ch_og_50_list, intact_ch_og_60_list, intact_ch_og_70_list, intact_ch_og_80_list,
                                        intact_ch_og_90_list,
                                        intact_ch_wr_50_list, intact_ch_wr_60_list, intact_ch_wr_70_list, intact_ch_wr_80_list,
                                        intact_ch_wr_90_list
                                        ))






sim_map <- bcmaps::ecoprovinces() %>%
  filter(ECOPROVINCE_NAME == "SOUTHERN INTERIOR MOUNTAINS")

intact_map <- ggplot() +
  geom_sf(sim_map, mapping = aes(colour = NULL)) +
  geom_sf(data = wau_intact_scenario, mapping = aes(fill = percent_intact), colour = NA) +
  #geom_sf(data = map_pa_background, fill="white", alpha = 0.4) +
  scale_fill_viridis_c() +
  labs(title = "Intact WAUs",
       fill = "% intactness") +
  theme_minimal()

intact_map

ggsave("out/intact_map.tiff", units="in", width=9, height=9, dpi=300)

intact_wr_map <- ggplot() +
  geom_sf(sim_map, mapping = aes(colour = NULL)) +
  geom_sf(data = wau_intact_ch_wr_50, mapping = aes(fill = "purple"), colour = NA) +
  geom_sf(data = wau_intact_ch_wr_60, mapping = aes(fill = "darkblue"), colour = NA) +
  geom_sf(data = wau_intact_ch_wr_70, mapping = aes(fill = "indigo"), colour = NA) +
  geom_sf(data = wau_intact_ch_wr_80, mapping = aes(fill = "green"), colour = NA) +
  geom_sf(data = wau_intact_ch_wr_90, mapping = aes(fill = "orange"), colour = NA) +
  #geom_sf(data = map_pa_background, fill="white", alpha = 0.4) +
  #scale_fill_viridis_c() +
  labs(title = "Intactness WAUs",
       fill = "Size of Contiguous Area") +
  theme_minimal()

intact_wr_map

ggsave("out/intact_wr_map.tiff", units="in", width=9, height=9, dpi=300)


intact_50_90_ch_map <- ggplot() +
  geom_sf(sim_map, mapping = aes(colour = NULL)) +
  geom_sf(data = wau_intact_ch_50, mapping = aes(fill = "purple"), colour = NA) +
  geom_sf(data = wau_intact_ch_90, mapping = aes(fill = "darkblue"), colour = NA) +
  #geom_sf(data = map_pa_background, fill="white", alpha = 0.4) +
  #scale_fill_viridis_c() +
  labs(title = "Intactness WAUs",
       fill = "Size of Contiguous Area") +
  theme_minimal()

intact_50_90_ch_map

ggsave("out/intact_50_90_ch_map.tiff", units="in", width=9, height=9, dpi=300)







wau_intact_60 <- wau_sim %>%
  left_join(wau_base, by= "aqua_id") %>%
  left_join(wau_ch_species_no, by="aqua_id") %>%
  filter(common_name_english != "Caribou - Southern Mountain population") %>%
  mutate(percent_intact = round(Intact_ha/ASSESSMENT_UNIT_AREA_HA*100, digit=0)) %>%
  filter(percent_intact >60) %>%
  mutate(contiguous_area = st_area(.),
         contiguous_area = as.numeric(set_units(contiguous_area, ha)),
         max_contiguous = max(contiguous_area)) %>%
  group_by(contiguous_area) %>%
  summarise(total_area=sum(ASSESSMENT_UNIT_AREA_HA),
            n_wau = n()) %>%
  arrange(desc(n_wau), desc(contiguous_area))


write_sf(wau_intact_50, "out/wau_intact_60.gpkg")
priority_sim <- wau_ch_sim %>%




