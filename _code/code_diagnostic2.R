# Created:       2/4/2020
# last edited:   2/17/2020 added apsim cc/sc versus sawyer cc/sc
#                2/18/2020
#
# purpose: look at how apsim tweaks change monopen
# author: gina vnichols@iastate.edu


rm(list = ls())
#devtools::install_github("vanichols/saapsim", force = T)
library(saapsim) #--my package
library(tidyverse)



# exp yield cpen ----------------------------------------------------------

cpenH <-
  sad_tidysawyer %>%
  group_by(crop, site, rotation) %>%
  mutate(nmax = max(nrate_kgha)) %>%
  filter(nrate_kgha == nmax) %>%
  select(crop, site, year, rotation, nrate_kgha, yield_kgha) %>%
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>%
  mutate(cpenH = sc - cc) %>%
  filter(!is.na(cpenH)) %>%
  filter(site == "ames")



# keeping track of oats ---------------------------------------------------
oat_key <-
  read_csv("sims_prelim-testing-gina/data-raw/rd_oat-descriptions-v1.csv") %>%
  separate_rows(category, sep = ",")


# read in .out files ------------------------------------------------------

my_dir <- "sims_prelim-testing-gina/"

apd <-
  saf_readapout(my_dir) %>%
  select(file, year, crop_yield) %>%
  mutate(yield_kgha = saf_buac_to_kgha_corn(crop_yield)) %>%
  separate(file, into = c("crop", "Nrate", "oat_desc")) %>%
  # remove old files w/o oat desc
  filter(!is.na(oat_desc)) %>%
  mutate(oat = parse_number(oat_desc)) %>%
  select(-oat_desc,-crop_yield) %>%
  # merge wi/oat_key
  left_join(oat_key) %>%
  mutate(
    nrate_kgha = parse_number(Nrate),
    crop = "corn",
    rotation = "cc",
    site = "ames"
  )

#--look at it
apd %>%
  ggplot(aes(nrate_kgha, yield_kgha)) +
  geom_point(aes(color = oat_what))

apd %>%
  ggplot(aes(nrate_kgha, yield_kgha)) +
  geom_point(aes(color = oat_what)) +
  guides(color = F) +
  facet_wrap( ~ oat_what)

baseapd <-
  apd %>%
  filter(oat_what == "base_cs") %>%
  select(site, crop, year, yield_kgha, nrate_kgha) %>%
  rename(basey_kgha = yield_kgha) %>%
  distinct()

# compare yields at highest n rate ----------------------------------------

#--sd of sawyer ames ccpen
saw_sd <- cpenH %>%
  ungroup() %>%
  summarise(cpen_sd = sd(cpenH)) %>%
  round() %>%
  select(cpen_sd) %>% pull()

apcpenH <-
  apd %>%
  select(site, crop, year, oat_what, yield_kgha, nrate_kgha) %>%
  filter(nrate_kgha == max(nrate_kgha),
         oat_what != "base_cs") %>%
  left_join(baseapd) %>%
  mutate(cpen_kgha = basey_kgha - yield_kgha)

#--look at spread/sd compared to exp
apcpenH %>%
  group_by(site, crop, oat_what) %>%
  mutate(cpen_sd = round(sd(cpen_kgha), 0)) %>%
  ggplot(aes(reorder(oat_what, cpen_kgha, mean, na.rm = T), cpen_kgha)) +
  geom_hline(data = cpenH, aes(yintercept = cpenH)) +
  geom_hline(
    data = cpenH %>% summarise(cpenH = mean(cpenH)),
    aes(yintercept = cpenH),
    color = "black",
    size = 3
  ) +
  geom_jitter(aes(color = as.factor(year)),
              size = 4,
              width = 0.1) +
  geom_text(y = -1500, aes(label = paste("sd=", cpen_sd))) +
  geom_text(
    x = 1,
    y = 4000,
    label = paste("exp sd = ", saw_sd),
    color = "red"
  ) +
  geom_boxplot() +
  coord_flip() +
  labs(x = NULL,
       title = "Ames")

ggsave("_figs/diag_oat-sd.png")

library(corrr)
#--are the yield gaps linearly related?
# get r2 to put on fig
# proof of concept
apcpenH %>%
  #filter(oat_what == "dec grain grwth rate") %>%
  select(crop, site, year, cpen_kgha, oat_what) %>%
  left_join(cpenH) %>% 
  filter(site == "ames", oat_what == "dec GGR") %>% 
  nest(data = everything()) %>% 
  mutate(cor = data %>% map(~cor(.$cpenH, .$cpen_kgha)))

mycors <- 
  apcpenH %>%
  #filter(oat_what == "dec grain grwth rate") %>%
  select(crop, site, year, cpen_kgha, oat_what) %>%
  left_join(cpenH) %>% 
  group_by(oat_what) %>% 
  nest() %>% 
  mutate(mycor = data %>% map(~cor(.$cpenH, .$cpen_kgha))) %>% 
  unnest(mycor)
  


apcpenH %>%
  #filter(oat_what == "dec grain grwth rate") %>%
  select(crop, site, year, cpen_kgha, oat_what) %>%
  left_join(cpenH) %>%
  ggplot(aes(cpenH, cpen_kgha)) +
  geom_abline() +
  geom_point(aes(color = as.factor(year))) +
  geom_text(data = mycors, x = 500, y = 500, aes(label = round(mycor, 2))) +
  facet_wrap( ~ oat_what, scales = "free")

ggsave("_figs/diag_oat-1to1-cpen.png")


#--dec grain growth rate looks promising,
#--at marsden the 4yr rot kernals were bigger in the year it had more yields

devtools::install_github("vanichols/maRsden")
library(maRsden)

mrs_plotkey %>%
  left_join(mrs_cornbio_vn) %>%
  filter(organ == "grain500",
         mass_gpl != 0) %>%
  ggplot(aes(rot_trt, mass_gpl / 500)) +
  geom_point(aes(color = as.factor(year))) +
  facet_grid(. ~ year)

mrs_rootdepth %>%
  left_join(mrs_plotkey) %>%
  ggplot(aes(doy, rootdepth_cm, color = rot_trt)) +
  geom_smooth(method = "lm") +
  facet_grid(. ~ year)

library(lme4)
mrs_rootdepth %>%
  left_join(mrs_plotkey) -> a %>%
  group_by(rot_trt) %>%
  nest()
lm(rootdepth_cm ~ doy * rot_trt, data = .) %>%
  summary()

