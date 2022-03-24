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


# all wau in southern interior

wau_si <- wau_full %>%
  filter(ECOPROVINCE_NAME == "SOUTHERN INTERIOR")

write_sf(wau_si, "out/wau_si.gpkg")

wau_base <- st_read("data/wshd_w_elements.gpkg", crs=3005) %>%
  st_cast(to="POLYGON") %>%
  st_make_valid() %>%
  st_drop_geometry() %>%
  select(aqua_id, UnProtectedArea_ha:wetlands_haclassN)

# any ch present

wau_ch_si <- wau_si %>%
  left_join(wau_base, by= "aqua_id") %>%
  filter(CH_ha > 0)

wau_ch_si_n <- unique(wau_ch_si$aqua_id)

write_sf(wau_ch_si, "out/wau_ch_si.gpkg")

#ch present with 2 or more species in WAU

#wau_ch_2 <- wau_ch_si %>%
  #filter(n_species >= 2)

#unique(wau_ch_2$ASSESSMENT_UNIT_GROUP)

#write_sf(wau_ch_2, "out/wau_ch_2.gpkg")
#requires both ch and kba

wau_ch_kba <- wau_si %>%
  left_join(wau_base, by= "aqua_id") %>%
  filter(CH_ha > 0 & KBA_ha > 0)

unique(wau_ch_kba$aqua_id)


write_sf(wau_ch_kba, "out/wau_ch_kba.gpkg")


wau_ch_kba_og <- wau_si %>%
  left_join(wau_base, by= "aqua_id") %>%
  filter(CH_ha > 0 & KBA_ha > 0 & OG_ha >0)

unique(wau_ch_kba_og$aqua_id)


write_sf(wau_ch_kba_og, "out/wau_ch_kba_og.gpkg")

wau_ch_kba_og <- wau_si %>%
  left_join(wau_base, by= "aqua_id") %>%
  filter(CH_ha > 0 & KBA_ha > 0 & OG_ha >0)

unique(wau_ch_kba_og$aqua_id)


write_sf(wau_ch_kba_og, "out/wau_ch_kba_og.gpkg")

#ch in upper 20 percentile

wau_ch_80 <- wau_si %>%
  left_join(wau_base, by= "aqua_id") %>%
  mutate(percentile = CH_ha/max(CH_ha)) %>%
  filter(percentile >= 0.8)

unique(wau_ch_80$aqua_id)

write_sf(wau_ch_80, "out/wau_ch_80.gpkg")

#ch in upper 20 percentile + OG

wau_ch_80_og <- wau_si %>%
  left_join(wau_base, by= "aqua_id") %>%
  mutate(percentile = CH_ha/max(CH_ha)) %>%
  filter(percentile >= 0.8 & OG_ha > 0)

unique(wau_ch_80_og$aqua_id)

write_sf(wau_ch_80_og, "out/wau_ch_80_og.gpkg")

wau_ch_80_og_80 <- wau_si %>%
  left_join(wau_base, by= "aqua_id") %>%
  mutate(percentile_ch = CH_ha/max(CH_ha),
         percentile_og = OG_ha/max(OG_ha)) %>%
  filter(percentile_og >= 0.8 & percentile_ch >= 0.8)

unique(wau_ch_80_og_80$aqua_id)

write_sf(wau_ch_80_og_80, "out/wau_ch_80_og_80.gpkg")


wau_ch_80_og_80 <- wau_si %>%
  left_join(wau_base, by= "aqua_id") %>%
  mutate(percentile_ch = CH_ha/max(CH_ha),
         percentile_og = OG_ha/max(OG_ha)) %>%
  filter(percentile_og >= 0.8 & percentile_ch >= 0.8)

unique(wau_ch_80_og_80$aqua_id)

write_sf(wau_ch_80_og_80, "out/wau_ch_80_og_80.gpkg")

wau_og_80 <- wau_si %>%
  left_join(wau_base, by= "aqua_id") %>%
  mutate(percentile_og = OG_ha/max(OG_ha)) %>%
  filter(percentile_og >= 0.8)

unique(wau_og_80$aqua_id)

write_sf(wau_og_80, "out/wau_og_80.gpkg")


wau_ch_no_og <- wau_si %>%
  left_join(wau_base, by= "aqua_id") %>%
  filter(CH_ha > 0 & OG_ha == 0)

unique(wau_ch_no_og$aqua_id)


write_sf(wau_ch_no_og, "out/wau_ch_no_og.gpkg")

wau_ch_no_ch <- wau_si %>%
  left_join(wau_base, by= "aqua_id") %>%
  filter(CH_ha == 0 & OG_ha > 0)

unique(wau_ch_no_ch$aqua_id)


write_sf(wau_ch_no_ch, "out/wau_ch_no_ch.gpkg")


wau_ch <- wau_si %>%
  left_join(wau_base, by= "aqua_id") %>%
  filter(CH_ha >0)

wau_ch_n <-unique(wau_ch$aqua_id)


write_sf(wau_ch, "out/wau_ch.gpkg")

wau_ch <- wau_si %>%
  left_join(wau_base, by= "aqua_id") %>%
  filter(CH_ha >0)

wau_ch_n <-unique(wau_ch$aqua_id)


write_sf(wau_ch, "out/wau_ch.gpkg")


wau_ch_og <- wau_si %>%
  left_join(wau_base, by= "aqua_id") %>%
  filter(CH_ha >0 & OG_ha >0)

wau_ch_og_n <-unique(wau_ch_og$aqua_id)


write_sf(wau_ch_og, "out/wau_ch_og.gpkg")
