# Created:      8/17/2021
#
# purpose: make oat fig for talk
#
#
# notes: keep apsim sims in box, all r code in github
# last edited:   3/31/2020 (I was confused...)
#                11/23/2020 cleaning, added rfv50% (oat22)
#                1/11/2021 moved sims to github, much easier
#                7/5/2021 trying to figure out what I did...
#                7/5/2021 moving fig creation to separate folder for manu figs
#               7/8/2021 ordering by scenarios listed in manu NEVERMIND
#               7/29/2021 adding x axis labels
#               8/17/2021 changing colors/fonts for talk
#
# NOTES: need to get colors updated, palette is on laptop??

rm(list = ls())
library(saapsim) #--has some functions
library(tidysawyer2) #--has sawyer data
library(tidyverse)
library(janitor)

source("05_talk-figs/talk-palette3.R")


#--wrote it, edited by hand
# gaps_filt %>% 
#   select(dtype, oat_nu, oat_what) %>% 
#   distinct() %>% 
#   write_csv("05_manu-figs/oats_nice.csv")
# 
oats_nice <- 
  read_csv("05_manu-figs/oats_nice.csv") %>% 
  mutate(oat_what_nice = case_when(
    oat_nu == 0 ~ "Penalty in Ames Iowa",
    oat_nu == 1 ~ "Less plants",
    oat_nu == 2 ~ "Delayed emerg.",
    oat_nu == 3 ~ "Foliar disease",
    oat_nu == 22 ~ "Less root growth",
    oat_nu == 5 ~ "Less root function",
    oat_nu == 6 ~ "Low early vigor",
    TRUE ~ oat_what_nice
  ))


#--calculated in the named folder
gaps_filt <- read_csv("01_sims-oat-by-hand/dat_tidy-hand-oats.csv")

#--note, I want to get the errors on the experimentally observed ones

cis <- 
  read_csv("00_exp-variability/dat_gap-cis.csv") %>% 
  filter(site == "ames")


new_exp <- 
  gaps_filt %>% 
  select(dtype, oat_nu, year, oat_what, category, notes) %>% 
  filter(dtype == "exp_gap") %>% 
  left_join(
    cis %>% 
      select(year, gap_kgha, gap_hi, gap_lo)
    )

gaps_filt_cis <- 
  gaps_filt %>% 
  filter(dtype != "exp_gap") %>% 
  bind_rows(new_exp)


# windmill ----------------------------------------------------------------



yrs_ord <- 
  gaps_filt_cis %>% 
  filter(oat_what == "exp gap", category == "1 factor") %>% 
  arrange(-gap_kgha) %>% 
  select(year) %>% 
  pull()

gaps_filt_wind <- 
  gaps_filt_cis %>%
  left_join(oats_nice) %>% 
  arrange(oat_what_order) %>% 
  mutate(year = factor(year, levels = yrs_ord),
         oat_what = fct_inorder(oat_what),
         oat_what_nice = fct_inorder(oat_what_nice)) %>% 
  group_by(dtype, oat_what, category) %>% 
  mutate(mngap = mean(gap_kgha, na.rm = T))

wind_theme_V <-    theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

wind_theme_H <-    
  theme_bw() +
  theme(strip.text = element_text(size = rel(1.4)),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = rel(1.3)))



# 1 and 5 together --------------------------------------------------------

f1 <- 
  gaps_filt_wind %>%
  filter(category %in% c("1 factor"), oat_what != "dec RFV10%") 

f5 <- 
  gaps_filt_wind %>%
  filter(category %in% c("5 factor"), oat_what != "dec RFV10%") %>%
  filter(!grepl("gap", oat_what))
  

#--kl, rfv, k#, rue, emerg, plant pop

f_dat <- 
  f1 %>% 
  bind_rows(f5)  %>% 
  mutate(oat_scen1 = oat_what_order - 2,
         oat_scen_lab = ifelse(oat_scen1 <= 0, paste(oat_what_nice),
                           ifelse(oat_scen1 == 16, "Combine Scenarios 1, 3, 4, 5, 6",
                                  paste0("Scenario ", oat_scen1)))
         ) 

f_dat_oat_labs <- f_dat %>% pull(oat_scen_lab) %>% unique()


f_dat %>% 
  filter(oat_nu == 0) %>% 
  arrange(gap_kgha) %>% 
  filter(gap_kgha == min(gap_kgha)|gap_kgha == max(gap_kgha))



# get labels better -------------------------------------------------------

#--what are the scenarios
f1 %>% 
  bind_rows(f5) %>% 
  select(oat_nu, oat_what, oat_what_nice) %>% 
  distinct()


f_dat %>% 
  ungroup() %>% 
  #--make last once less wordy
  mutate(oat_what_nice2 = ifelse(category == "5 factor", "Combo", as.character(oat_what_nice)),
         oat_what_nice2 = fct_inorder(oat_what_nice2)) %>% 
  mutate(oat_scen_lab = factor(oat_scen_lab, levels = f_dat_oat_labs)) %>% 
  mutate(
    #year = factor(year, levels = yrs_ord),
    col1 = case_when(
      oat_what == "exp gap" ~ "A",
      oat_what == "current apsim gap" ~ "B",
      oat_what == "late emergence" ~ "C",
      TRUE ~ "D")
  ) %>% 
  ggplot(aes(year, gap_kgha/1000)) + 
  geom_bar(aes(fill = col1, color = col1),
           position = "dodge", 
           stat = "identity", 
           #color = "black"
  ) +
  #geom_hline(aes(yintercept = mngap), size = 1, type = "dotted") +
  #geom_linerange(aes(ymin = gap_lo, ymax = gap_hi), color = "gray40") +
  facet_grid(.~oat_what_nice2, labeller = label_wrap_gen(width = 10)) + 
  guides(fill = F, color = F) +
  scale_x_discrete(breaks = c(NA, NA)) +
  scale_fill_manual(values = c("C" = clr_blu, "B" = clr_div, "A" = clr_red, "D" = clr_blu)) +
  scale_color_manual(values = c("C" = clr_blu, "B" = clr_div, "A" = clr_red, "D" = clr_blu)) +
  labs(#title = "Ames",
    x = "Year, ordered by largest to smallest Ames penalty",
    y = "Continuous\nmaize\npenalty") + 
  wind_theme_H  +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title = element_text(size = rel(1.5)))
  
ggsave("05_talk-figs/fig_oat1.png", width = 11, height = 6)


# colors change ------------------------------------------------------------



f_dat %>% 
  ungroup() %>% 
  #--make last once less wordy
  mutate(oat_what_nice2 = ifelse(category == "5 factor", "Combo", as.character(oat_what_nice)),
         oat_what_nice2 = fct_inorder(oat_what_nice2)) %>% 
  mutate(oat_scen_lab = factor(oat_scen_lab, levels = f_dat_oat_labs)) %>% 
  mutate(
    #year = factor(year, levels = yrs_ord),
    col1 = case_when(
      oat_what == "exp gap" ~ "A",
      oat_what == "current apsim gap" ~ "B",
      oat_what == "late emergence" ~ "C",
      TRUE ~ "D")
  ) %>% 
  ggplot(aes(year, gap_kgha/1000)) + 
  geom_bar(aes(fill = col1, color = col1),
           position = "dodge", 
           stat = "identity", 
           #color = "black"
  ) +
  #geom_hline(aes(yintercept = mngap/1000), size = 1, linetype = "dashed") +
  #geom_linerange(aes(ymin = gap_lo, ymax = gap_hi), color = "gray40") +
  facet_grid(.~oat_what_nice2, labeller = label_wrap_gen(width = 10)) + 
  guides(fill = F, color = F) +
  scale_x_discrete(breaks = c(NA, NA)) +
  scale_fill_manual(values = c("C" ="gray80", "B" = clr_div, "A" = clr_red, "D" = clr_blu)) +
  scale_color_manual(values = c("C" = "gray80", "B" = clr_div, "A" = clr_red, "D" = clr_blu)) +
  labs(#title = "Ames",
    x = "Year, ordered by largest to smallest Ames penalty",
    y = "Continuous\nmaize\npenalty") + 
  wind_theme_H  +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title = element_text(size = rel(1.5)))

ggsave("05_talk-figs/fig_oat2.png", width = 11, height = 6)




# means appear ------------------------------------------------------------



f_dat %>% 
  ungroup() %>% 
  #--make last once less wordy
  mutate(oat_what_nice2 = ifelse(category == "5 factor", "Combo", as.character(oat_what_nice)),
         oat_what_nice2 = fct_inorder(oat_what_nice2)) %>% 
  mutate(oat_scen_lab = factor(oat_scen_lab, levels = f_dat_oat_labs)) %>% 
  mutate(
    #year = factor(year, levels = yrs_ord),
    col1 = case_when(
      oat_what == "exp gap" ~ "A",
      oat_what == "current apsim gap" ~ "B",
      oat_what == "late emergence" ~ "C",
      TRUE ~ "D")
  ) %>% 
  ggplot(aes(year, gap_kgha/1000)) + 
  geom_bar(aes(fill = col1, color = col1),
           position = "dodge", 
           stat = "identity", 
           #color = "black"
  ) +
 geom_hline(aes(yintercept = mngap/1000), size = 1, linetype = "dashed") +
  #geom_linerange(aes(ymin = gap_lo, ymax = gap_hi), color = "gray40") +
  facet_grid(.~oat_what_nice2, labeller = label_wrap_gen(width = 10)) + 
  guides(fill = F, color = F) +
  scale_x_discrete(breaks = c(NA, NA)) +
  scale_fill_manual(values = c("C" ="gray80", "B" = clr_div, "A" = clr_red, "D" = clr_blu)) +
  scale_color_manual(values = c("C" = "gray80", "B" = clr_div, "A" = clr_red, "D" = clr_blu)) +
  labs(#title = "Ames",
    x = "Year, ordered by largest to smallest Ames penalty",
    y = "Continuous\nmaize\npenalty") + 
  wind_theme_H  +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title = element_text(size = rel(1.5)))

ggsave("05_talk-figs/fig_oat3.png", width = 11, height = 6)




# colors change-------------------------------------------------------

f_dat %>% 
  ungroup() %>% 
  #--make last once less wordy
  mutate(oat_what_nice2 = ifelse(category == "5 factor", "Combination", as.character(oat_what_nice)),
         oat_what_nice2 = fct_inorder(oat_what_nice2)) %>% 
  mutate(oat_scen_lab = factor(oat_scen_lab, levels = f_dat_oat_labs)) %>% 
  mutate(
    #year = factor(year, levels = yrs_ord),
    col1 = case_when(
      oat_what == "exp gap" ~ "A",
      oat_what == "current apsim gap" ~ "B",
      oat_what == "late emergence" ~ "C",
      TRUE ~ "D")
  ) %>% 
  ggplot(aes(year, gap_kgha)) + 
  geom_bar(aes(fill = col1, color = col1),
           position = "dodge", 
           stat = "identity", 
           #color = "black"
  ) +
 # geom_hline(aes(yintercept = mngap), size = 1, type = "dotted") +
  #geom_linerange(aes(ymin = gap_lo, ymax = gap_hi), color = "gray40") +
  facet_grid(.~oat_what_nice2, labeller = label_wrap_gen(width = 10)) + 
  guides(fill = F, color = F) +
  scale_x_discrete(breaks = c(NA, NA)) +
  scale_fill_manual(values = c("C" ="gray80", "B" = clr_div, "A" = clr_red, "D" = clr_blu)) +
  scale_color_manual(values = c("C" = "gray80", "B" = clr_div, "A" = clr_red, "D" = clr_blu)) +
  labs(#title = "Ames",
    x = "Year, ordered by largest to smallest observed penalty",
    y = (expression(atop("Continuous maize penalty", 
                         paste("Ames IA, (kg "~ha^-1*")"))))) + 
  wind_theme_H  +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title = element_text(size = rel(1.5)))

ggsave("05_talk-figs/fig_oat2.png", width = 12, height = 6.25)

# error bars-------------------------------------------------------

f_dat %>% 
  ungroup() %>% 
  #--make last once less wordy
  mutate(oat_what_nice2 = ifelse(category == "5 factor", "Combination", as.character(oat_what_nice)),
         oat_what_nice2 = fct_inorder(oat_what_nice2)) %>% 
  mutate(oat_scen_lab = factor(oat_scen_lab, levels = f_dat_oat_labs)) %>% 
  mutate(
    #year = factor(year, levels = yrs_ord),
    col1 = case_when(
      oat_what == "exp gap" ~ "A",
      oat_what == "current apsim gap" ~ "B",
      oat_what == "late emergence" ~ "C",
      TRUE ~ "D")
  ) %>% 
  ggplot(aes(year, gap_kgha)) + 
  geom_bar(aes(fill = col1, color = col1),
           position = "dodge", 
           stat = "identity", 
           #color = "black"
  ) +
  geom_hline(aes(yintercept = mngap), size = 1, type = "dotted") +
  geom_linerange(aes(ymin = gap_lo, ymax = gap_hi), color = "gray40") +
  facet_grid(.~oat_what_nice2, labeller = label_wrap_gen(width = 10)) + 
  guides(fill = F, color = F) +
  scale_x_discrete(breaks = c(NA, NA)) +
  scale_fill_manual(values = c("C" ="gray80", "B" = clr_div, "A" = clr_red, "D" = clr_blu)) +
  scale_color_manual(values = c("C" = "gray80", "B" = clr_div, "A" = clr_red, "D" = clr_blu)) +
  labs(#title = "Ames",
    x = "Year, ordered by largest to smallest observed penalty",
    y = "Continuous\nmaize penalty\nAmes IA") + 
  wind_theme_H  +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title = element_text(size = rel(1.5)),
        strip.text = element_text(size = rel(1.3)))

ggsave("05_talk-figs/fig_oat4.png", width = 12, height = 6.25)
