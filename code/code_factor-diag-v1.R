# Created:      2/24/2020
# last edited:   
#
# purpose: evaluate each factor individually and in combination
# author: gina vnichols@iastate.edu


rm(list = ls())
#devtools::install_github("vanichols/saapsim", force = T)
library(saapsim) #--my package
library(tidyverse)
library(janitor)

# keeping track of oats ---------------------------------------------------

oat_key <-
  read_csv("sims_factor-analysis-ames-gina/data-raw/rd_oat-descriptions-v2.csv") %>%
  separate_rows(category, sep = ",") %>% 
  remove_empty("rows") %>% 
  remove_empty("cols")


myfacs <- oat_key %>% select(category) %>% distinct()

# john sawyer gap at highest N amt ----------------------------------------------------------

jsgap_h <-
  sad_tidysawyer %>%
  mutate(nrate_kgha = round(nrate_kgha, 0)) %>% 
  group_by(crop, site, rotation) %>%
  mutate(nmax = max(nrate_kgha)) %>%
  filter(nrate_kgha == nmax) %>%
  select(crop, site, year, rotation, nrate_kgha, yield_kgha) %>%
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>%
  mutate(jsgap_kgha = sc - cc) %>%
  filter(!is.na(jsgap_kgha)) %>%
  filter(site == "ames") %>% 
  select(crop, site, year, nrate_kgha, jsgap_kgha) %>% 
  # make js an oat
  rename(gap_kgha = jsgap_kgha) %>% 
  mutate(oat_what = "js exp") %>% 
  ungroup() %>% 
  crossing(myfacs)

# read in .out files ------------------------------------------------------

my_dir <- "sims_factor-analysis-ames-gina/"

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


# get things I want -------------------------------------------------------

#--extract 'base' SC yield first (at high N)
apsc_h <-
  apd %>%
  filter(oat_what == "base_cs") %>%
  select(site, crop, year, yield_kgha, nrate_kgha) %>%
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  rename(apsc_kgha = yield_kgha) %>%
  distinct() 

#--extract oat yields at high N
apoat_h <-
  apd %>%
  filter(oat_what != "base_cs") %>%
  select(site, crop, year, oat_what, yield_kgha, nrate_kgha) %>%
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  rename(apoat_kgha = yield_kgha) %>%
  distinct() 

#--apsim gap
apgap_h <-
  apd %>%
  filter(nrate_kgha == max(nrate_kgha),
         oat_what != "base_cs") %>%
  left_join(apsc_h) %>%
  mutate(apgap_kgha = apsc_kgha - yield_kgha) %>% 
  select(crop, site, year, oat_what, category, nrate_kgha, apgap_kgha)


# get yield and gap datas -------------------------------------------------

apgap_tmp <- 
  apgap_h %>% 
  rename(gap_kgha = apgap_kgha) 

dat_gap <- 
  jsgap_h  %>% 
  bind_rows(apgap_tmp) %>% 
  mutate(type_id = ifelse(oat_what == "js exp", "exp", "apsim"))


# make sotiris fig --------------------------------------------------------

mycols <- c("exp" = "#47E3FF", "apsim" = "#FF6347")


dat_gap %>% 
  filter(category == '3 factor') %>% 
  ggplot(aes(reorder(oat_what, gap_kgha, mean), gap_kgha, color = type_id)) +
  geom_point() + 
  geom_boxplot(alpha = 0.5) + 
  coord_flip() + 
  facet_grid(category~., scales = "free") + 
  labs(x = NULL,
       y = "Continuous Corn Yield Gap (kg/ha)") +
  scale_color_manual(values = mycols) + 
  theme_light()

#--make 'nice' figs for presentation slides for Millie

fac1 <- 
  dat_gap %>% 
  filter(category == "1 factor") %>% 
  ggplot(aes(reorder(oat_what, gap_kgha, mean), gap_kgha, color = type_id)) +
  geom_point() + 
  geom_boxplot(alpha = 0.5) + 
  coord_flip() + 
  facet_grid(.~category, scales = "free") + 
  scale_color_manual(values = mycols) + 
  labs(x = NULL,
       y = "Continuous Corn Yield Gap (kg/ha)") +
  theme_light()
fac1

fac2 <- 
  dat_gap %>% 
  filter(category == "2 factor") %>% 
  ggplot(aes(reorder(oat_what, gap_kgha, mean), gap_kgha, color = type_id)) +
  geom_point() + 
  geom_boxplot(alpha = 0.5) + 
  coord_flip() + 
  facet_grid(.~category, scales = "free") + 
  scale_color_manual(values = mycols) + 
  labs(x = NULL,
       y = "Continuous Corn Yield Gap (kg/ha)") +
  theme_light()
fac2

library(patchwork)

fac1 / fac2 + plot_layout(heights = c(1, 3))

fac1 + fac2
ggsave("_figs/diag_factors.png", width = 10, height = 4)




#--look at spread/sd compared to exp
apgap_h %>%
  group_by(site, crop, oat_what) %>%
  mutate(apgap_sd = round(sd(apgap_kgha), 0)) %>%
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

