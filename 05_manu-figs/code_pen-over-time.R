# Created:    april 2 2021
#
# purpose: make gap over time fig
#
# notes: 
# last edited:   

rm(list = ls())
#devtools::install_github("femiguez/nlraa")
library(tidysawyer2) 
library(tidyverse)
library(saapsim)
library(fancycut)
library(patchwork)
library(scales)

theme_set(theme_bw())
sc_color <- "#2a89c3" 
cc_color <- "#fdb462"


# data --------------------------------------------------------------------

gaps_alln <-
  ilia_yields %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(ogap_kgha = sc - cc,
         ogap_pct = ogap_kgha/sc) %>% 
  mutate(nrate = cut_interval(nrate_kgha, n = 3),
         nrateF = as.numeric(nrate),
         nrateF2 = case_when(
           nrateF == 1 ~ "Low (0-90 kgN/ha)",
           nrateF == 2 ~ "Med (90-180 kgN/ha)",
           nrateF == 3 ~ "High (180-270 kgN/ha")
  ) 


gaps_highn <- 
  gaps_alln %>%
  filter(nrateF == 3)
  



# over time ---------------------------------------------------------------


fig_gap <-
  ilia_yields %>%
  bind_rows(
    gaps_alln %>%
      mutate(
        nrate = cut_interval(nrate_kgha, n = 3),
        nrateF = as.numeric(nrate),
        nrateF2 = case_when(
          nrateF == 1 ~ "Low (0-90 kgN/ha)",
          nrateF == 2 ~ "Med (90-180 kgN/ha)",
          nrateF == 3 ~ "High (180-270 kgN/ha")
      )  %>%
      mutate(rotation = "gap",
             yield_kgha = ogap_kgha)
  ) %>%
  bind_rows(
    gaps_alln %>%
      mutate(
        nrate = cut_interval(nrate_kgha, n = 3),
        nrateF = as.numeric(nrate),
        nrateF2 = case_when(
          nrateF == 1 ~ "Low (0-90 kgN/ha)",
          nrateF == 2 ~ "Med (90-180 kgN/ha)",
          nrateF == 3 ~ "High (180-270 kgN/ha")
      )  %>%
      mutate(rotation = "gap_pct",
             yield_kgha = ogap_pct)
  ) %>% 
  mutate(
    nrate = cut_interval(nrate_kgha, n = 3),
    nrateF = as.numeric(nrate),
    nrateF2 = case_when(
      nrateF == 1 ~ "Low (0-90 kgN/ha)",
      nrateF == 2 ~ "Med (90-180 kgN/ha)",
      nrateF == 3 ~ "High (180-270 kgN/ha"
    )
  ) %>%
  arrange(nrate) %>% 
  filter(nrateF == 3) %>% 
  filter(rotation != "gap_pct") 


f1 <- 
  fig_gap %>%
  mutate(rot_nice = case_when(
    grepl("sc", rotation) ~ "Rotated maize",
    grepl("cc", rotation) ~ "Continuous maize",
    grepl("gap", rotation) ~ "Continuous maize penalty"),
    rot_nice = factor(rot_nice, levels = c("Rotated maize", "Continuous maize", "Continuous maize penalty"))
  ) %>% 
  ggplot(aes(year, yield_kgha/1000)) + 
  geom_jitter(aes(shape = rot_nice, color = rot_nice)) + 
  geom_smooth(method = "lm", se = F, aes(color = rot_nice), size = 2) +
  scale_color_manual(values = c("Continuous maize" = cc_color, 
                                "Rotated maize" = sc_color,
                                "Continuous maize penalty" = "red")) +
  scale_shape_manual(values = c("Continuous maize" = 24, 
                                "Rotated maize" = 21,
                                "Continuous maize penalty" = 22)) +
  labs(x = NULL,
       color = NULL,
       shape = NULL,
       y = "Maize Grain\n(dry Mg ha-1)") + 
  theme(legend.direction = "horizontal", 
        legend.justification = c(0, 1),
        legend.position = c(0.05, 0.95),
        legend.background = element_rect(color = "black"),
        axis.title.y = element_text(angle = 0, vjust = 0.5))


f1

# windmill of gaps --------------------------------------------------------
#--are percent and actual gap reasonably related? yes
fig_gap_only %>% 
  ggplot(aes(ogap_kgha, ogap_pct)) + 
  geom_point()

fig_gap_only <- 
  fig_gap %>%
  mutate(ogap_rel = 1 - ogap_pct) %>% 
  mutate(rot_nice = case_when(
    grepl("sc", rotation) ~ "Rotated maize",
    grepl("cc", rotation) ~ "Continuous maize",
    grepl("gap", rotation) ~ "Continuous maize penalty"),
    rot_nice = factor(rot_nice, levels = c("Rotated maize", "Continuous maize", "Continuous maize penalty")),
    state = 
      case_when(
        grepl("IA", state) ~ "Iowa",
        grepl("IL", state) ~ "Illinois"
      )
  ) %>%
  filter(rotation == "gap", 
         !is.na(ogap_pct)) 

#--what is mean red, if we don't include 'increases'

mn_ogap <- 
  fig_gap_only %>% 
  filter(ogap_kgha > 0) %>% 
  summarise(mean_ogap = mean(ogap_pct)) %>% 
  pull(mean_ogap)

f2 <-
  fig_gap_only %>%
  arrange(-ogap_pct) %>% 
  mutate(n = 1:n()) %>% 
  ggplot(aes(n, ogap_pct)) +
  geom_hline(yintercept = mn_ogap, linetype = "dashed") +
  geom_segment(aes(color = state, x = n, xend = n, y = 0, yend = ogap_pct)) +
  geom_point(aes(color = state), size = 3) +
  geom_text(x = 300, y = 0.17, label = "Mean yield reduction of 14%", check_overlap = T, hjust = 1, fontface = "italic") +
  scale_y_continuous(labels = label_percent()) +
  labs(x = NULL,
       y = "Continuous maize\nyield penalty\n(% rotated maize\ngrain yield)",
       color = NULL) +
  scale_color_manual(values = c("Iowa" = "gray80",
                                "Illinois" = "gray20")) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text.x = element_blank(),
        legend.justification = c(1, 1),
        legend.position = c(0.95, 0.95),
        legend.direction = "horizontal",
        legend.background = element_rect(color = "black"))


f2flip <- 
  fig_gap_only %>% 
  arrange(ogap_pct) %>% 
  mutate(n = 1:n()) %>% 
    ggplot(aes(n, ogap_pct)) +
    #geom_col(aes(fill = state), size = 5) +
    geom_hline(yintercept = mn_ogap, linetype = "dashed") +
    geom_segment(aes(color = state, x = n, xend = n, y = 0, yend = ogap_pct)) +
    geom_point(aes(color = state), size = 3) +
    geom_text(x = 100, y = 0.2, label = "Mean yield reduction of 14%", check_overlap = T, hjust = 0, fontface = "italic") +
    scale_y_continuous(labels = label_percent()) +
    labs(x = NULL,
         y = "Continuous maize yield reduction as percentage of rotated maize grain yield",
         color = NULL) + 
    scale_color_manual(values = c("Iowa" = "gray80", 
                                  "Illinois" = "gray20")) +
    theme(#axis.title.y = element_text(angle = 0, vjust = 0.5),
          axis.text.y = element_blank(),
          legend.direction = "horizontal",
          legend.justification = c(0, 1),
          legend.position = c(0.05, 0.95),
          legend.background = element_rect(color = "black"), 
          axis.ticks.y = element_blank()) + 
    coord_flip()
  
f2flip

# combine -----------------------------------------------------------------

#f1+f2 + plot_layout(widths = c(3, 1))

#f1/f2 + plot_layout(heights = c(2, 1.25))

f1

ggsave("05_manu-figs/fig_gap-over-time.png")


# alternative -------------------------------------------------------------

#cont corn yields driving gap

f3 <- 
  gaps_alln  %>%
  select(state, site, nrateF2, year, cc, sc, ogap_kgha) %>% 
  pivot_longer(cc:sc, names_to = "rotation") %>%
  mutate(rot_nice = case_when(
    grepl("sc", rotation) ~ "Rotated maize",
    grepl("cc", rotation) ~ "Continuous maize",
    grepl("gap", rotation) ~ "Continuous maize penalty"),
    rot_nice = factor(rot_nice, levels = c("Continuous maize", "Rotated maize"))
  ) %>% 
  ggplot(aes(value/1000, ogap_kgha/1000)) + 
  geom_point(aes(color = rot_nice, shape = rot_nice)) + 
  geom_smooth(method = "lm", se = F, color = "black") +
  guides(shape = F,
         color = F) +
  labs(y = "Continuous maize\nyield penalty\n(dry Mg ha-1)",
         x = "Maize grain yield\n(dry Mg ha-1)") +
  scale_color_manual(values = c("Continuous maize" = cc_color, 
                                "Rotated maize" = sc_color)) +
  scale_shape_manual(values = c("Continuous maize" = 24, 
                                "Rotated maize" = 21)) +
  facet_grid(. ~ rot_nice) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5), 
        strip.background = element_blank(),
        strip.text = element_text(size = rel(1.2)))


f2 / f3 + plot_layout(heights = c(1.2, 1))

ggsave("05_manu-figs/fig_gap-summary.png")
