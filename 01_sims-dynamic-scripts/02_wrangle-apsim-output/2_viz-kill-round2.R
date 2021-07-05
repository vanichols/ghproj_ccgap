# goal: combine results into something useful
# created: 12/8/2020
# updated: 12/11/2020 (added nash)
#          12/15/2020 (created new script for my more organized sets)
#
# notes: 

rm(list = ls())

library(tidyverse)
library(saapsim) #--for conversions
library(tidysawyer2) #--for obs data
library(patchwork)


#--getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
curdir <- paste(getwd())

theme_set(theme_bw())


# data --------------------------------------------------------------------

#--misc data
set_id_key_full <- read_csv("../01_create-parameter-grids/01_set-params-round2.csv") %>% 
  mutate(set_scrcat = paste(set_script, set_pcat, sep = "-"))

set_id_key <- 
  set_id_key_full %>%
  select(set_id, set_script, set_pdes) %>% 
  distinct() %>% 
  add_row(set_id = 0, set_script = "obs", set_pdes = "obs") %>% 
  arrange(set_id)

#--sim data
simsraw <- read_csv("02_sims.csv")

sims <- 
  simsraw %>%
  filter(till == "normal")

s.gaps <- read_csv("02_sims-gaps.csv")

#--obs data

obs <- read_csv("02_obs.csv") %>%  
  filter(site %in% c("ames", "nash")) 

o.gaps <- read_csv("02_obs-gaps.csv") %>% 
  filter(site %in% c("ames", "nash")) 



# wrangle -----------------------------------------------------------------

# NOTE: 12/15, set 1008 breaks Nashua. SO I'm removing it
sims <- 
  simsraw %>% 
  left_join(set_id_key) %>%  
  filter(grepl("kill|scripts", set_script)) %>% 
  filter(set_id == 1| set_id > 1000) %>% 
  filter(!(set_id == 1008 & site == "nash"))


# look at gaps ------------------------------------------------------------

#--just ensure all of the sims ran
s.gaps %>% 
  left_join(o.gaps) %>% 
  select(site, set_id, till, year, sim_gap_buac, obs_gap_buac) %>% 
  pivot_longer(sim_gap_buac:obs_gap_buac) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  ggplot() +
  geom_line(aes(year, sim_gap_buac, color = as.factor(set_id)), size = 2) + 
  geom_line(aes(year, obs_gap_buac), linetype = "dashed") + 
  facet_grid(site~till+set_id) 


#--gap sizes by target

o.gaps.range <- 
  o.gaps %>% 
  group_by(site) %>% 
  summarise(maxgap = max(obs_gap_buac, na.rm = T),
            mingap = min(obs_gap_buac, na.rm = T))

r.gaps <- 
  s.gaps %>% 
  rename("gap_buac" = "sim_gap_buac") %>% 
  bind_rows(
    o.gaps %>% 
      mutate(set_id = 0,
             till = "normal") %>% 
      rename("gap_buac" = "obs_gap_buac") 
    ) %>% 
  bind_rows(
    o.gaps %>% 
      mutate(set_id = 0,
             till = "reduced") %>% 
      rename("gap_buac" = "obs_gap_buac") 
  ) %>% 
  select(site, set_id, till, year, gap_buac) %>% 
  left_join(set_id_key)
  
r.gaps %>% 
  filter(till == "normal") %>% 
  # mutate(set_target = factor(set_target, 
  #                            levels = c("no scripts", "cold", "wet", "dry"))) %>% 
  ggplot(aes(set_pdes, gap_buac)) + 
  geom_violin(aes(color = site, fill = site)) + 
  # geom_hline(data = o.gaps.range, aes(yintercept = maxgap, color = site),
  #            linetype = "dashed") +
  # geom_hline(data = o.gaps.range, aes(yintercept = mingap, color = site),
  #            linetype = "dashed") +
  coord_flip() 

#--how can the gap get SMALLER than the no=script one?
r.gaps %>% 
  filter(till == "normal") %>% 
  filter(site == "nash") %>% 
  filter(set_id %in% c(1, 1007)) %>% 
  select(site, set_desc, till, year, gap_buac) %>% 
  #pivot_wider(names_from = set_desc, values_from = gap_buac) %>% 
  ggplot(aes(year, gap_buac)) + 
  geom_point(aes(color = set_desc), size = 5)

#--so in 2006, the cold kill actually made the gap smaller. Why?
# --it's miniscule
sims %>% 
  filter(site == "nash") %>% 
  filter(set_id %in% c(1, 1007)) %>% 
  filter(till == "normal") %>% 
  filter(year == 2006) %>% 
  ggplot(aes(rot2, corn_buac)) +
  geom_col(aes(fill = set_desc), position = "dodge") 

#--the cc yielded more in cold kill. how many plants were killed?
sims %>% 
  filter(site == "nash") %>% 
  filter(set_id %in% c(1007)) %>% 
  filter(till == "normal") %>% 
  filter(year == 2006) %>% 
  mutate(PlantK = PlantDensityS-PlantDensityH) %>% 
  select(PlantK)
  

library(gt)

r.gaps %>% 
  filter(till == "normal") %>% 
  group_by(site, set_id, till) %>% 
  summarise(mingap = min(gap_buac, na.rm = T),
            maxgap = max(gap_buac, na.rm = T)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  ungroup() %>% 
  arrange(set_id) %>% 
  left_join(set_id_key) %>% 
  select(till, site, set_script, set_pdes, mingap, maxgap) %>% 
  gt() %>% 
  tab_row_group(
    group = "Ames",
    rows = site == "ames") %>% 
  tab_row_group(
    group = "Nashua",
    rows = site == "nash") %>% 
  cols_hide(
    columns = vars(till, site)
  )

# investigate kill  ------------------------------------------------------------

library(RColorBrewer)
n <- 11
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))


#--how many plants are dying each year?
# note: in 2009 nashua didn't get to maturity, so it was never 'harvested', plants are 0 at harvest htus
sims %>% 
  filter( !(year == 2009 & site == "nash")) %>% 
  filter(till == "normal") %>% 
  mutate(pct_kill = (PlantDensityS - PlantDensityH)/PlantDensityS * 100) %>% 
  ggplot(aes(year, pct_kill, group = interaction(set_pdes, rot2))) + 
  geom_point(aes(color = interaction(rot2, site)), size = 2) + 
  geom_line(aes(color = interaction(rot2, site)), size = 2) + 
  geom_hline(yintercept = 5, linetype = "dashed") +
  facet_grid(till+site~set_pdes, scales = "free", labeller = label_wrap_gen(width = 15)) + 
  labs(title = "More plants are killed in NT CC compared to SC")


#--dry penalty isn't doing much
# sims %>% 
#   filter(set_target == "dry"2, 4)) %>% 
#   select(set_desc, till, rot2, year, PlantDensityS, PlantDensityH) %>% 
#   mutate(pct_kill = (PlantDensityS - PlantDensityH)/PlantDensityS * 100) %>% 
#   ggplot(aes(year, pct_kill, group = interaction(set_desc, rot2))) + 
#   geom_line(aes(color = set_desc), size = 2) + 
#   geom_hline(yintercept = 5, linetype = "dashed") +
#   facet_grid(rot2~till, scales = "free") + 
#   labs(title = "Dry kill only affects rotated corn")

ggsave("fig_kill-dry-only-affects-rotC.png")


#--is the temp kill doing anything?
# sims %>% 
#   left_join(set_id_key) %>% 
#   filter(set_id %in% c(2, 6)) %>% 
#   select(site, set_pdes, till, rot2, year, PlantDensityS, PlantDensityH) %>% 
#   mutate(pct_kill = (PlantDensityS - PlantDensityH)/PlantDensityS * 100) %>% 
#   ggplot(aes(year, pct_kill, group = interaction(set_pdes, rot2))) + 
#   geom_line(aes(color = set_desc), size = 2) + 
#   geom_hline(yintercept = 5, linetype = "dashed") +
#   facet_grid(rot2~till+site, scales = "free") + 
#   labs(title = "2012 was a temperature kill?") +
#   scale_y_continuous(limits = c(0, 50))


# sims %>% 
#   #filter(rot2 == "sc") %>% 
#   filter(set_id %in% c(2, 7, 8, 10)) %>%
#   select(set_desc, till, rot2, year, PlantDensityS, PlantDensityH) %>% 
#   mutate(pct_kill = (PlantDensityS - PlantDensityH)/PlantDensityS * 100,
#          set_desc = as.factor(set_desc),
#          set_desc = fct_reorder(set_desc, pct_kill, mean),
#          set_desc = fct_rev(set_desc)) %>% 
#   ggplot(aes(year, pct_kill, group = interaction(rot2, set_desc))) + 
#   geom_line(aes(color = rot2), size = 2) + 
#   geom_hline(yintercept = 5, linetype = "dashed") +
#   scale_color_manual(values = c("cc" = "gold2", "sc" = "green4")) +
#   facet_grid(set_desc~till) + 
#   labs(title = "AMES - Wet kill most influential, strongly responds to residue",
#        subtitle = "Cold kill only active in 2004, dry kill only active for rotated corn",
#        x = NULL,
#        y = "Percentage of plants killed (%)") + 
#   theme(strip.text.y = element_text(angle = 0))

ggsave("fig_kill-component-effects.png")


#--is the difference in gap vals from noscripts related to the pact kill?

simgaps %>%  
  select(site, set_desc, till, year, sim_gap_buac) %>% 
  pivot_wider(names_from = set_desc, values_from = sim_gap_buac) %>% 
  janitor::clean_names() %>% 
  mutate(gap_diff = kill - no_scripts) %>% 
  select(-no_scripts, -kill) %>% 
  left_join(
    ames %>% 
      filter(set_id == 2) %>% 
      select(set_desc, set_id, till, rot2, year, PlantDensityS, PlantDensityH) %>% 
      mutate(pct_kill = (PlantDensityS - PlantDensityH)/PlantDensityS * 100) %>% 
      select(-PlantDensityS, -PlantDensityH)
  ) %>% 
  ggplot(aes(gap_diff, pct_kill)) + 
  geom_point(size = 3) + 
  facet_grid(.~till) + 
  labs(title = "Pct kill is not related to change in gap from no scripts",
       subtitle = "")
  

k3 <- 
  simgaps %>%  
  select(site, set_desc, till, year, sim_gap_buac) %>% 
  pivot_wider(names_from = set_desc, values_from = sim_gap_buac) %>% 
  janitor::clean_names() %>% 
  mutate(gap_diff = kill - no_scripts) %>% 
  ggplot(aes(year, gap_diff)) + 
  geom_col() + 
  facet_grid(.~till) + 
  labs(y = "kill gap - no script gap, bu/ac")
  
  
k1/k3  

k1
  

# sot style fig -----------------------------------------------------------

obs_stats <- 
  obsgaps %>% 
  group_by(site) %>% 
  summarise(gap_mean = mean(obs_gap_buac, na.rm = T),
            gap_sd = sd(obs_gap_buac, na.rm = T)) %>% 
  mutate(dat_type = "obs") %>% 
  ungroup()


sim_stats <- 
  simgaps %>% 
  group_by(site, till, set_id, set_desc) %>% 
  summarise(gap_mean = mean(sim_gap_buac, na.rm = T),
            gap_sd = sd(sim_gap_buac, na.rm = T)) %>% 
  mutate(dat_type = "sim") %>% 
  ungroup()

#--visualize
bind_rows(sim_stats, 
          obs_stats %>% 
            full_join(sim_stats %>% select(site, till))
          ) %>% 
  mutate(sim_id = ifelse(is.na(set_desc), 
                         paste(site, dat_type, sep = "-"),
                         paste(site, dat_type, set_desc, set_id, sep = "-")),
         sim_id = as.factor(sim_id),
         sim_id = fct_reorder(sim_id, gap_mean)) %>% 
  ggplot(aes(sim_id, gap_mean)) + 
  geom_col(aes(fill = set_desc)) +
  facet_grid(.~till) + 
  coord_flip()


  
  