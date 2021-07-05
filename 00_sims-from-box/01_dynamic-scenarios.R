# goal: combine results into something useful
# created: 12/8/2020
# updated: 12/11/2020 (added nash)
#          12/15/2020 (created new script for my more organized sets)
#          7/5/2021 moved to github folder, looking through...
#
# notes: 

rm(list = ls())

library(tidyverse)
library(saapsim) #--for conversions
library(tidysawyer2) #--for obs data
library(patchwork)
library(scales)

theme_set(theme_bw())


# data --------------------------------------------------------------------

#--misc data
set_id_key_full <- read_csv("00_sims-from-box/01_create-parameter-grids/01_set-params-round2.csv") %>% 
  mutate(set_scrcat = paste(set_script, set_pcat, sep = "-"))

set_id_key <- 
  set_id_key_full %>%
  select(set_id, set_script, set_pdes) %>% 
  distinct()

#--sim data
simsraw <- read_csv("00_sims-from-box/02_wrangle-apsim-output/02_sims.csv")

sims <- 
  simsraw %>%
  filter(till == "normal")

s.gaps <- read_csv("00_sims-from-box/02_wrangle-apsim-output/02_sims-gaps.csv") %>% 
  left_join(set_id_key_full)

# look at obs, checking ---------------------------------------------------

#--use new method of getting obs gaps, 7/5/2021

o.gaps <- 
  read_csv("00_empirical-n-cont/dat_gap-components.csv") %>% 
  select(site, year, nonngap) %>%
  rename("ogap_kgha" = nonngap) %>% 
  mutate(ogap_buac = saf_kgha_to_buac_corn(ogap_kgha)) %>% 
  filter(!is.na(ogap_kgha))



# combine them ------------------------------------------------------------

#--ranges for reference in figs
o.gaps.range <- 
  o.gaps %>% 
  group_by(site) %>% 
  summarise(maxgap = max(ogap_buac, na.rm = T),
            mingap = min(ogap_buac, na.rm = T))

r.gaps <- 
  s.gaps %>% 
  rename("gap_buac" = "sgap_buac") %>% 
  bind_rows(
    o.gaps %>% 
      mutate(set_id = 0,
             set_script = "observed",
             set_pcat = "observed", 
             set_pdes = "observed",
             set_scrcat = "observed",
             till = "normal") %>% 
      rename("gap_buac" = "ogap_buac") 
    ) %>% 
  # bind_rows(
  #   o.gaps %>% 
  #     mutate(set_id = 0,
  #            set_script = "observed",
  #            set_pcat = "observed", 
  #            set_pdes = "observed",
  #            set_scrcat = "observed",
  #            till = "reduced") %>% 
  #     rename("gap_buac" = "ogap_buac") 
  # ) %>% 
  select(-cc, -sc)


# figs --------------------------------------------------------------------

  
r.gaps %>% 
  filter(till == "normal") %>% 
  filter(site %in% c("ames", "nash")) %>% 
  filter(set_script %in% c("combo", "no scripts", "observed")) %>% 
  mutate(set_script = factor(set_script,
                             levels = c("observed",
                                        "no scripts",
                                        "combo"))) %>%
  ggplot(aes(set_pdes, gap_buac)) + 
  geom_hline(yintercept = 0) +
  geom_violin(aes(color = site, fill = site)) + 
  geom_point(aes(fill = site), pch = 21) + 
  geom_hline(data = o.gaps.range %>%  filter(site %in% c("ames", "nash")),
             aes(yintercept = maxgap, color = site),
             linetype = "dashed") +
  geom_hline(data = o.gaps.range %>%  filter(site %in% c("ames", "nash")),
             aes(yintercept = mingap, color = site),
             linetype = "dashed") +
  scale_x_discrete(labels = label_wrap(15)) +
  coord_flip() + 
  facet_grid(. ~ site, scales = "free_y")
  


#--make a fgure for update
r.gaps %>% 
  filter(till == "normal", site == "ames") %>% 
  filter(set_script %in% c("combo", "no scripts", "observed")) %>% 
  mutate(set_script = factor(set_script,
                             levels = c("observed",
                                        "no scripts",
                                        "combo")),
         set_lab = case_when(
           set_id == 0 ~ "Observed gap",
           set_id == 1 ~ "Simulated gap, no scripts",
           TRUE ~ paste("Script Scenario ", str_sub(paste(set_id), -1, -1))),
         set_lab = factor(set_lab),
         set_lab = fct_relevel(set_lab, "Observed gap", "Simulated gap, no scripts"),
         set_lab = fct_rev(set_lab)
  ) %>%
  ggplot(aes(set_lab, gap_buac)) + 
  geom_hline(yintercept = 0) +
  geom_violin(aes(fill = set_lab %in% c("Observed gap", "Simulated gap, no scripts"))) + 
  geom_point(fill = "white", pch = 21) + 
  geom_hline(data = o.gaps.range %>% filter(site == "ames"), aes(yintercept = maxgap),
             color = "black",
             linetype = "dashed") +
  geom_hline(data = o.gaps.range %>% filter(site == "ames"), aes(yintercept = mingap),
             color = "black",
             linetype = "dashed") +
  geom_vline(xintercept = 9.5) +
  guides(fill = F, color = F) +
  coord_flip() +
  scale_fill_manual(values = c("red2", "dodgerblue3")) + 
  labs(title = "New APSIM scripts improve ability to capture continuous corn gap",
       subtitle = "Example in Ames, 2001-2016",
       y = "Gap between continuous corn and rotated corn yields (bu/ac)")

ggsave("fig_violin_ames-committee-update.png")



#--how do they look on a 1:1 line (are we getting penalties in the right years?)

s.gaps %>% 
  left_join(o.gaps) %>% 
  filter(till == "normal") %>% 
  filter(set_script %in% c("combo", "no scripts")) %>% 
  ggplot(aes(ogap_kgha, sim_gap_kgha)) + 
  geom_point(aes(color = set_script), size = 4) +
  geom_abline() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~site+set_id)
  


# assessing fits ----------------------------------------------------------


# modelling efficiency ----------------------------------------------------

library(dynatopmodel) #--has NSE nash-suth efficiency
library(tidytext)

tst <-
  s.gaps %>%
  filter(set_id == 1, site == "ames") %>%
  left_join(o.gaps) 

NSE(qsim = tst$sim_gap_buac, qobs = tst$ogap_buac)


s.gaps %>%
  left_join(o.gaps) %>%
  filter(set_script == "combo"|set_script == "no scripts") %>% 
  group_by(site, set_id, set_pdes) %>%
  nest() %>%
  mutate(fval = data %>% map_dbl(~NSE(.x$sim_gap_buac, .x$ogap_buac))) %>% 
  ggplot(aes(reorder_within(set_pdes, fval, site), fval)) + 
  geom_col() + 
  facet_wrap(~site, scales = "free") + 
  scale_x_reordered() +
  coord_flip() + 
  labs(title = "Nash-S Modelling Efficiecny",
       subtitle = "Larger is Better")


# rmse ----------------------------------------------------

library(Metrics)

s.gaps %>%
  filter(till == "normal") %>% 
  left_join(o.gaps) %>%
  filter(is.na(set_pcat))

drmse <- 
  s.gaps %>%
  filter(till == "normal") %>% 
  left_join(o.gaps) %>%
  group_by(site, set_id, set_script, set_pdes, set_pcat) %>%
  nest() %>%
  mutate(fval = data %>% map_dbl(~rmse(.x$ogap_buac, .x$sim_gap_buac))) 

# Q: Does the kill script help predict the gap?
drmse %>% 
  #filter(site == "ames") %>% 
  filter(set_script %in% c("no scripts", "kill")) %>% 
  ggplot(aes(set_pdes, fval)) + 
  geom_hline(data = drmse %>% 
               filter(set_pcat == "no scripts") %>% 
               ungroup() %>% 
               select(site, fval),
             aes(yintercept = fval)) +
  geom_col(aes(fill = set_pcat)) + 
  facet_grid(site~set_pcat, scales = "free_x", space = "free_x") + 
  labs(title = "Including wet kill better captures CC penalty in Ames",
       subtitle = "Cold and dry kill does not")

# Q: Does the rue script with res help predict the gap?
drmse %>% 
  filter(set_script %in% c("no scripts", "rue")) %>% 
  ggplot(aes(set_pdes, fval)) + 
  geom_hline(data = drmse %>% 
               filter(set_pcat == "no scripts") %>% 
               ungroup() %>% 
               select(site, fval),
             aes(yintercept = fval)) +
  geom_col(aes(fill = set_pcat)) + 
  facet_grid(site~set_pcat, scales = "free_x", space = "free_x") + 
  labs(title = "Including residue-based penalty better captures CC penalty",
       subtitle = "Blanks are SWIM crashes in Nashua")

# Q: Does the xf help predict the gap?
drmse %>% 
  filter(set_script %in% c("no scripts", "xf")) %>% 
  ggplot(aes(set_pdes, fval)) + 
  geom_hline(data = drmse %>% 
               filter(set_pcat == "no scripts") %>% 
               ungroup() %>% 
               select(site, fval),
             aes(yintercept = fval)) +
  geom_col(aes(fill = set_pcat)) + 
  facet_grid(site~set_pcat, scales = "free_x", space = "free_x") + 
  labs(title = "Including root-related things does not help CC penalty",
       subtitle = "Cold and wet penalties ARE being triggered, root depth isn't important")


# Q: Do the combos help?
drmse %>% 
  filter(set_script %in% c("no scripts", "combo")) %>% 
  ggplot(aes(reorder(set_pdes, fval), fval)) + 
  geom_hline(data = drmse %>% 
               filter(set_pcat == "no scripts") %>% 
               ungroup() %>% 
               select(site, fval),
             aes(yintercept = fval)) +
  geom_col(aes(fill = set_pcat)) + 
  facet_grid(site~., scales = "free_x", space = "free_x") + 
  coord_flip()


#--is the root depth being affected?
sims %>% 
  filter(set_script %in% c("no scripts", "xf"), rot2 == "cc") %>% 
  filter(site == "ames") %>% 
  ggplot(aes(year, RootDepth/1000)) +
  geom_point(aes(color = set_pdes), size = 3) + 
  geom_line(aes(color = set_pdes)) + 
  scale_y_reverse() + 
  labs(
    title = "Is xf reduction affecting maximum rooting depth?",
    subtitle = "Maximum rooting depth is only reduced 5 cm in most extreme case")



#--which nashua ones am i missing
drmse %>% 
  filter(set_script %in% c("no scripts", "rue")) %>% 
  filter(site == "nash") %>% 
  filter(is.na(fval))

s1103 <- 
  s.gaps %>%
  filter(till == "normal") %>% 
  left_join(o.gaps) %>% 
  filter(set_id %in% c(1103),
         site == "nash")
  
sims %>% 
  filter(set_script == "combo"|set_script == "no scripts") %>% 
  ggplot(aes(year, corn_buac, color = set_script)) + 
  geom_point(size = 5) + 
  facet_grid(.~rot2)

#--it KILLED it in 2013?!?


r.gaps %>% 
  filter(till == "normal", year != 2013) %>% 
  # mutate(set_script = factor(set_script,
  #                            levels = c("observed", 
  #                                       "no scripts",
  #                                       "kill",
  #                                       "xf",
  #                                       "rue"))) %>%
  ggplot(aes(set_pdes, gap_buac)) + 
  geom_hline(yintercept = 0) +
  geom_violin(aes(color = site, fill = site)) + 
  geom_hline(data = o.gaps.range, aes(yintercept = maxgap, color = site),
             linetype = "dashed") +
  geom_hline(data = o.gaps.range, aes(yintercept = mingap, color = site),
             linetype = "dashed") +
  coord_flip() + 
  facet_grid(set_script ~ site, scales = "free_y")
