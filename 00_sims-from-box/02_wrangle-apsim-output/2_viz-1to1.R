# goal: combine results into something useful
# created: 1/5/2021
# updated: 1/6/2021 - add slope?
#
# notes: 

rm(list = ls())

library(tidyverse)
library(saapsim) #--for conversions
library(tidysawyer2) #--for obs data
library(patchwork)
library(scales)

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
  distinct()

#--sim data
simsraw <- read_csv("02_sims.csv")

sims <- 
  simsraw %>%
  filter(till == "normal")

s.gaps <- read_csv("02_sims-gaps.csv")

#--obs data

obs <- read_csv("02_obs.csv") %>%  
  filter(site %in% c("ames", "nash")) 

o.gaps <- read_csv("02_obs-gaps.csv")


gaps <- 
  s.gaps %>% 
  left_join(o.gaps) %>% 
  select(-cc, -sc, -sgap_buac, -ogap_buac)

# rmse --------------------------------------------------------------------

library(Metrics)

drmse <- 
  gaps %>%
  filter(till == "normal") %>% 
  group_by(set_id, site, till) %>%
  nest() %>%
  mutate(rmse = round(data %>% map_dbl(~rmse(.x$ogap_kgha, .x$sgap_kgha)), 0)) %>% 
  select(-data)


# slope -------------------------------------------------------------------

dslope <- 
  gaps %>%
  filter(till == "normal") %>% 
  group_by(set_id, site, till) %>%
  nest() %>% 
  mutate(mod = data %>% map(~lm(sgap_kgha ~ ogap_kgha, data = .)),
         coefs = mod %>% map(coef),
         coefs2 = coefs %>% map(broom::tidy)) %>% 
  unnest(cols = coefs2) %>% 
  filter(names == "ogap_kgha") %>% 
  select(-data, -mod, -coefs, -names) %>% 
  rename("slope" = x)


dstats <- 
  drmse %>% 
  left_join(dslope)

# gaps viz ------------------------------------------------------------

# 1:1 w/o scripts
nosc <-
  gaps %>% 
  filter(set_id == 1) %>% 
  select(-set_id) 

### kill ----

##--ordered by inc rmse

kill_order <- 
  drmse %>% 
  left_join(set_id_key) %>% 
  filter(site == "ames",
         set_script == "kill") %>% 
  arrange(rmse) %>% 
  pull(set_pdes)

drmse_kill <- 
  drmse %>% 
  left_join(set_id_key) %>% 
  filter(set_script == "kill") %>% 
  mutate(set_pdes = factor(set_pdes, levels = kill_order)) %>% 
  arrange(set_pdes)

gaps %>% 
  left_join(set_id_key) %>% 
  filter(set_script == "kill",
         till == "normal") %>% 
  mutate(set_pdes = factor(set_pdes, levels = kill_order)) %>%
  arrange(set_pdes) %>% 
  ggplot(aes(ogap_kgha, sgap_kgha)) + 
  #--no scripts
  geom_point(data =
               nosc %>%
               filter(till == "normal"),
             aes(ogap_kgha, sgap_kgha),
             color = "gray30") +
  geom_point(aes(color = set_pdes)) + 
  geom_text(data = drmse_kill,
            x = 1000, y = 4000,
            aes(label = paste("RMSE=", rmse))) +
  guides(color = F) +
  geom_abline() +
  geom_smooth(method = "lm", se = F) +
  coord_cartesian(xlim = c(-250, 4000), ylim = c(-250, 4000)) +
  facet_grid(site~set_pdes, labeller = label_wrap_gen(10)) + 
  labs(title = "Ordered by inc Ames-RMSE",
       x = "Observed penalty (kg/ha)",
       y = "Simulated penalty (kg/ha)")
  

ggsave("fig_1to1_kill.png")

#--I'm curious about the 1% kill all the time
gaps %>% 
  filter(set_id %in% c(1011, 1)) %>%
  pivot_longer(sgap_kgha:ogap_kgha) %>%
  unite(name, set_id, col = "thing") %>% 
  pivot_wider(names_from = thing, values_from = value) %>% 
  ggplot() + 
  geom_segment(aes(x = ogap_kgha_1, y = sgap_kgha_1, 
                   xend = ogap_kgha_1011, yend = sgap_kgha_1011),
               linetype = "dotted") +
  geom_point(aes(ogap_kgha_1, sgap_kgha_1), color = "gray50", size = 2) +
  geom_point(aes(ogap_kgha_1011, sgap_kgha_1011), color = "red", size = 2) +
  geom_abline() +
  facet_grid(.~site) + 
  labs(title = paste(
    set_id_key %>% 
      filter(set_id == 1011) %>% 
      pull(set_pdes)))
  



### rue ----

##--ordered by inc rmse

rue_order <- 
  drmse %>% 
  left_join(set_id_key) %>% 
  filter(site == "ames",
         set_script == "rue") %>% 
  arrange(rmse) %>% 
  pull(set_pdes)

drmse_rue <- 
  drmse %>% 
  left_join(set_id_key) %>% 
  filter(set_script == "rue") %>% 
  mutate(set_pdes = factor(set_pdes, levels = rue_order)) %>% 
  arrange(set_pdes)


gaps %>% 
  left_join(set_id_key) %>% 
  filter(set_script == "rue",
         till == "normal") %>% 
  mutate(set_pdes = factor(set_pdes, levels = rue_order)) %>%
  arrange(set_pdes) %>% 
  ggplot(aes(ogap_kgha, sgap_kgha)) + 
  #--no scripts
  geom_point(data = 
               nosc %>% 
               filter(till == "normal"), 
             aes(ogap_kgha, sgap_kgha),
             color = "gray30") +
  geom_point(aes(color = set_pdes)) + 
  geom_text(data = drmse_rue,
            x = 1000, y = 4000,
            aes(label = paste("RMSE=", rmse))) +
  # geom_text(data = drmse %>%
  #             left_join(set_id_key) %>%
  #             filter(set_script == "rue"),
  #           x = 1000, y = 3000, color = "red",
  #           aes(label = paste("Set = ", set_id))) +
  guides(color = F) +
  geom_abline() +
  geom_smooth(method = "lm", se = F) +
  coord_cartesian(xlim = c(-250, 4000), ylim = c(-250, 4000)) +
  facet_grid(site~set_pdes, labeller = label_wrap_gen(10))

ggsave("fig_1to1_rue.png")


### xf ----


##--ordered by inc rmse

xf_order <- 
  drmse %>% 
  left_join(set_id_key) %>% 
  filter(site == "ames",
         set_script == "xf") %>% 
  arrange(rmse) %>% 
  pull(set_pdes)

drmse_xf <- 
  drmse %>% 
  left_join(set_id_key) %>% 
  filter(set_script == "xf") %>% 
  mutate(set_pdes = factor(set_pdes, levels = xf_order)) %>% 
  arrange(set_pdes)


gaps %>% 
  left_join(set_id_key) %>% 
  filter(set_script == "xf",
         till == "normal") %>% 
  mutate(set_pdes = factor(set_pdes, levels = xf_order)) %>%
  arrange(set_pdes) %>%  
  ggplot(aes(ogap_kgha, sgap_kgha)) + 
  #--no scripts
  geom_point(data = 
               nosc %>% 
               filter(till == "normal"), 
             aes(ogap_kgha, sgap_kgha),
             color = "gray30") +
  geom_point(aes(color = set_pdes)) + 
  geom_text(data = drmse_xf,
            x = 1000, y = 3000,
            aes(label = paste("RMSE=", rmse))) +
  # geom_text(data = drmse %>%
  #             left_join(set_id_key) %>%
  #             filter(set_script == "xf"),
  #           x = 1000, y = 2000, color = "red",
  #           aes(label = paste("Set = ", set_id))) +
  guides(color = F) +
  geom_abline() +
  geom_smooth(method = "lm", se = F) +
  coord_cartesian(xlim = c(-250, 3000), ylim = c(-250, 3000)) +
  facet_grid(site~set_pdes, labeller = label_wrap_gen(10))

ggsave("fig_1to1_xf.png")

#--I'm curious about the 50% xf all the time
gaps %>% 
  filter(set_id %in% c(1208, 1)) %>%
  pivot_longer(sgap_kgha:ogap_kgha) %>%
  unite(name, set_id, col = "thing") %>% 
  pivot_wider(names_from = thing, values_from = value) %>% 
  ggplot() + 
  geom_segment(aes(x = ogap_kgha_1, y = sgap_kgha_1, 
                   xend = ogap_kgha_1208, yend = sgap_kgha_1208),
               linetype = "dotted") +
  geom_point(aes(ogap_kgha_1, sgap_kgha_1), color = "gray50", size = 2) +
  geom_point(aes(ogap_kgha_1208, sgap_kgha_1208), color = "red", size = 2) +
  geom_text(aes(ogap_kgha_1, sgap_kgha_1, label = year)) +
  geom_abline() +
  facet_grid(.~site) + 
  labs(title = paste(
    set_id_key %>% 
      filter(set_id == 1208) %>% 
      pull(set_pdes)))




# doesn't work any more ---------------------------------------------------


###best ames----

mysets_ames <- c(1011, 1107, 1208, 1003, 1104, 1203, 2001, 2009, 2013)

mystatic_ames <- c(1011, 1107, 1208)
mydynamic_ames <- c(1003, 1104, 1203)
myboth_ames <- c(2001, 2013, 2009)


s.types_ames <- 
  s.gaps %>% 
  filter(set_id %in% mysets_ames) %>%  
  mutate(type = case_when(
    set_id %in% c(1011, 1107, 1208) ~ "static",
    set_id %in% c(1003, 1104, 1203) ~ "dynamic",
    TRUE ~ "both"))



# ames figs --------------------------------------------------------------

fig_stat_ames <- 
  s.types_ames %>% 
  filter(type == "static") %>% 
  left_join(o.gaps) %>% 
  ggplot(aes(ogap_kgha, sgap_kgha)) + 
  geom_point(data = nosc %>%
               filter(site == "ames", till == "normal") %>% 
               select(-set_pdes, -set_script),
             aes(ogap_kgha, sgap_kgha),
             color = "gray30") +
  geom_point(aes(color = set_pdes), size = 2) + 
  geom_text(data = drmse %>% 
              filter(site == "ames",
                     set_id %in% mystatic_ames),
            x = 500, y = 3000, aes(label = paste("RMSE = ", fval2))) +
  geom_abline() +
  guides(color = F) +
  geom_smooth(method = "lm", se = F, linetype = "dashed", color = "black") +
  scale_x_continuous(limits = c(-250, 4000)) + 
  scale_y_continuous(limits = c(-250, 4000)) +
  facet_grid(type~set_script + set_pdes) + 
  labs(title = "Ames")

fig_stat_ames

fig_dynam_ames <- 
  s.types_ames %>% 
  filter(type == "dynamic") %>% 
  left_join(o.gaps) %>% 
  ggplot(aes(ogap_kgha, sgap_kgha)) + 
  geom_point(data = nosc %>%
               filter(site == "ames", till == "normal") %>% 
               select(-set_pdes, -set_script),
             aes(ogap_kgha, sgap_kgha),
             color = "gray30") +
  geom_point(aes(color = set_pdes), size = 2) + 
  geom_text(data = drmse %>% 
              filter(site == "ames",
                     set_id %in% mydynamic_ames),
            x = 500, y = 3000, aes(label = paste("RMSE = ", fval2))) +
  geom_abline() +
  guides(color = F) +
  geom_smooth(method = "lm", se = F, linetype = "dashed", color = "black") +
  scale_x_continuous(limits = c(-250, 4000)) + 
  scale_y_continuous(limits = c(-250, 4000)) +
  facet_grid(type~set_script + set_pdes) + 
  labs(title = "Ames")

fig_dynam_ames

fig_both_ames <- 
  s.types_ames %>% 
  filter(type == "both") %>% 
  left_join(o.gaps) %>% 
  ggplot(aes(ogap_kgha, sgap_kgha)) + 
  geom_point(data = nosc %>%
               filter(site == "ames", till == "normal") %>% 
               select(-set_pdes, -set_script),
             aes(ogap_kgha, sgap_kgha),
             color = "gray30") +
  geom_point(aes(color = set_pdes), size = 2) + 
  geom_text(data = drmse %>% 
              filter(site == "ames",
                     set_id %in% myboth_ames),
            x = 500, y = 3000, aes(label = paste("RMSE = ", fval2))) +
  geom_abline() +
  guides(color = F) +
  geom_smooth(method = "lm", se = F, linetype = "dashed", color = "black") +
  scale_x_continuous(limits = c(-250, 4000)) + 
  scale_y_continuous(limits = c(-250, 4000)) +
  facet_grid(type~set_script + set_pdes) + 
  labs(title = "Ames")

fig_both_ames

library(patchwork)

ames <- fig_stat_ames/fig_dynam_ames

# look at gaps nash ------------------------------------------------------------

#--add in other sims, do ames kill to start
###kill rmse----
s.gaps %>% 
  filter(set_script == "kill",
         till == "normal", site == "nash") %>% 
  left_join(o.gaps) %>% 
  ggplot(aes(ogap_kgha, sgap_kgha)) + 
  geom_point(data = nosc %>% 
               filter(site == "nash", till == "normal") %>% 
               select(-set_pdes, -set_script),
             aes(ogap_kgha, sgap_kgha),
             color = "gray30") +
  geom_point(aes(color = set_pdes)) + 
  geom_text(data = drmse %>% 
              filter(site == "nash", set_script == "kill"),
            x = 2000, y = 6000, aes(label = paste("RMSE = ", fval2))) +
  geom_text(data = drmse %>% 
              filter(site == "nash", set_script == "kill"),
            x = 2000, y = 4000, aes(label = paste("set_id = ", set_id))) +
  geom_abline() +
  geom_smooth(method = "lm", se = F) +
  scale_x_continuous(limits = c(-250, 7000)) + 
  scale_y_continuous(limits = c(-250, 7000)) +
  facet_wrap(~set_pdes)

###rue rmse----
s.gaps %>% 
  filter(set_script == "rue",
         till == "normal",
         site == "nash") %>% 
  left_join(o.gaps) %>% 
  ggplot(aes(ogap_kgha, sgap_kgha)) + 
  geom_point(data = nosc %>% 
               filter(site == "nash", till == "normal") %>% 
               select(-set_pdes, -set_script),
             aes(ogap_kgha, sgap_kgha),
             color = "gray30") +
  geom_point(aes(color = set_pdes)) + 
  geom_text(data = drmse %>% 
              filter(site == "nash", set_script == "rue"),
            x = 2000, y = 6000, aes(label = paste("RMSE = ", fval2))) +
  geom_text(data = drmse %>% 
              filter(site == "nash", set_script == "rue"),
            x = 2000, y = 4000, aes(label = paste("set_id = ", set_id))) +
  
  geom_abline() +
  geom_smooth(method = "lm", se = F) +
  scale_x_continuous(limits = c(-250, 7000)) + 
  scale_y_continuous(limits = c(-250, 7000)) +
  facet_wrap(~set_pdes)

###xf rmse----
s.gaps %>% 
  filter(set_script == "xf",
         till == "normal", 
         site == "nash") %>% 
  left_join(o.gaps) %>% 
  filter(set_id != 1) %>% 
  ggplot(aes(ogap_kgha, sgap_kgha)) + 
  geom_point(data = nosc %>% 
               filter(site == "nash", till == "normal") %>% 
               select(-set_pdes, -set_script),
             aes(ogap_kgha, sgap_kgha),
             color = "gray30") +
  geom_point(aes(color = set_pdes)) + 
  geom_text(data = drmse %>% 
              filter(site == "nash", set_script == "xf"),
            x = 2000, y = 6000, aes(label = paste("RMSE = ", fval2))) +
  geom_text(data = drmse %>% 
              filter(site == "nash", set_script == "xf"),
            x = 2000, y = 4000, aes(label = paste("set_id = ", set_id)), color= "red") +
  geom_abline() +
  geom_smooth(method = "lm", se = F) +
  scale_x_continuous(limits = c(-250, 7000)) + 
  scale_y_continuous(limits = c(-250, 7000)) +
  facet_wrap(~set_pdes)

###combo rmse----
s.gaps %>% 
  filter(set_script == "combo",
         till == "normal") %>% 
  left_join(o.gaps) %>% 
  filter(set_id != 1) %>% 
  ggplot(aes(ogap_kgha, sgap_kgha)) + 
  geom_point(data = nosc %>% 
               filter(site == "nash", till == "normal") %>% 
               select(-set_pdes, -set_script),
             aes(ogap_kgha, sgap_kgha),
             color = "gray30") +
  geom_point(aes(color = set_pdes)) + 
  geom_text(data = drmse %>% 
              filter(site == "nash", set_script == "combo"),
            x = 2000, y = 6000, aes(label = paste("RMSE = ", fval2))) +
  geom_text(data = drmse %>% 
              filter(site == "nash", set_script == "combo"),
            x = 2000, y = 4000, aes(label = paste("set_id = ", set_id))) +
  geom_abline() +
  geom_smooth(method = "lm", se = F) +
  scale_x_continuous(limits = c(-250, 7000)) + 
  scale_y_continuous(limits = c(-250, 7000)) +
  facet_wrap(~set_pdes)


###best----

mysets_nash <- c(1011, 1107, 1208, 1003, 1104, 1202, 2012, 2002, 2006)

mystatic_nash <- c(1011, 1107, 1208)
mydynamic_nash <- c(1003, 1104, 1202)
myboth_nash <- c(2012, 2002, 2006)


s.types_nash <- 
  s.gaps %>% 
  filter(set_id %in% mysets_nash) %>%  
  mutate(type = case_when(
    set_id %in% c(mystatic_ames, mystatic_nash) ~ "static",
    set_id %in% c(mydynamic_ames, mydynamic_nash) ~ "dynamic",
    TRUE ~ "both"))


# nash figs --------------------------------------------------------------

fig_stat_nash <- 
  s.types_nash %>% 
  filter(type == "static") %>% 
  left_join(o.gaps) %>% 
  ggplot(aes(ogap_kgha, sgap_kgha)) + 
  geom_point(data = nosc %>%
               filter(site == "nash", till == "normal") %>% 
               select(-set_pdes, -set_script),
             aes(ogap_kgha, sgap_kgha),
             color = "gray30") +
  geom_point(aes(color = set_pdes), size = 2) + 
  geom_text(data = drmse %>% 
              filter(site == "nash",
                     set_id %in% mystatic_nash),
            x = 500, y = 3000, aes(label = paste("RMSE = ", fval2))) +
  geom_abline() +
  guides(color = F) +
  geom_smooth(method = "lm", se = F, linetype = "dashed", color = "black") +
  scale_x_continuous(limits = c(-250, 4000)) + 
  scale_y_continuous(limits = c(-250, 4000)) +
  facet_grid(type~set_script + set_pdes) + 
  labs(title = "nash")

fig_stat_nash

fig_dynam_nash <- 
  s.types_nash %>% 
  filter(type == "dynamic") %>% 
  left_join(o.gaps) %>% 
  ggplot(aes(ogap_kgha, sgap_kgha)) + 
  geom_point(data = nosc %>%
               filter(site == "nash", till == "normal") %>% 
               select(-set_pdes, -set_script),
             aes(ogap_kgha, sgap_kgha),
             color = "gray30") +
  geom_point(aes(color = set_pdes), size = 2) + 
  geom_text(data = drmse %>% 
              filter(site == "nash",
                     set_id %in% mydynamic_nash),
            x = 500, y = 3000, aes(label = paste("RMSE = ", fval2))) +
  geom_abline() +
  guides(color = F) +
  geom_smooth(method = "lm", se = F, linetype = "dashed", color = "black") +
  scale_x_continuous(limits = c(-250, 4000)) + 
  scale_y_continuous(limits = c(-250, 4000)) +
  facet_grid(type~set_script + set_pdes) + 
  labs(title = "nash")

fig_both_nash <- 
  s.types_nash %>% 
  filter(type == "both") %>% 
  left_join(o.gaps) %>% 
  ggplot(aes(ogap_kgha, sgap_kgha)) + 
  geom_point(data = nosc %>%
               filter(site == "nash", till == "normal") %>% 
               select(-set_pdes, -set_script),
             aes(ogap_kgha, sgap_kgha),
             color = "gray30") +
  geom_point(aes(color = set_pdes), size = 2) + 
  geom_text(data = drmse %>% 
              filter(site == "nash",
                     set_id %in% myboth_nash),
            x = 500, y = 3000, aes(label = paste("RMSE = ", fval2))) +
  geom_abline() +
  guides(color = F) +
  geom_smooth(method = "lm", se = F, linetype = "dashed", color = "black") +
  scale_x_continuous(limits = c(-250, 4000)) + 
  scale_y_continuous(limits = c(-250, 4000)) +
  facet_grid(type~set_script + set_pdes) + 
  labs(title = "nash")

fig_both_nash

nash <- fig_stat_nash/fig_dynam_nash



# ames and nash -----------------------------------------------------------

ames | nash
