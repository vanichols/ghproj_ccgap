# Gina Nichols, Jan 25 2021
# Goal: explore variability in difference estimate


library(tidyverse)
library(tidysawyer2) #--this is a private repo right now, sorry
library(grafify)

theme_set(theme_bw())

dat_miranda <- ia_yields_se %>%  
  group_by(site) %>% #--each site has it's own maximum nrate
  filter(nrate_kgha == max(nrate_kgha))

ylds <- 
  dat_miranda %>% 
  select(site, year, rotation, nrate_kgha, yield_kgha) %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  filter(!is.na(sc)) 
  

sds <- 
  dat_miranda %>% 
  select(site, year, rotation, nrate_kgha, sd_kgha) %>% 
  pivot_wider(names_from = rotation, values_from = sd_kgha) %>% 
  rename("cc_sd" = cc,
         "sc_sd" = sc) %>% 
  filter(!is.na(sc_sd))
sds

dat <- 
  ylds %>% 
  left_join(sds) %>% 
  mutate(
    gap_kgha = sc - cc
  ) %>% 
  filter(!is.na(gap_kgha)) %>% 
  filter(!(site == "lewi" & year == 2013))
dat    


# uncertainty -------------------------------------------------------------

#1. How to estimate the uncertainty around the DIFFERENCE of cc and sc
# NOTE: THIS IS WRONG? Use https://link.springer.com/chapter/10.1007%2F978-94-011-7241-7_15
# THis might be wrong. Wiating on miranda

dat2 <-
  dat %>%
  expand_grid(., n = c(4, 8, 16)) %>% 
  mutate(
    term1 = (cc_sd) ^ 2 / (n-1),
    term2 = (sc_sd) ^ 2 / (n-1),
    gap_sd = sqrt(term1 + term2)
  ) %>% 
  select(-term1, -term2, -cc, -sc, -nrate_kgha)


dat2 %>% 
  ggplot(aes(year, gap_kgha)) + 
  geom_linerange(aes(ymin = gap_kgha - 1.96*gap_sd,
                     ymax = gap_kgha + 1.96*gap_sd,
                     color = as.factor(n))) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  facet_wrap(~site)

#--what I actually want is 95% confidence intervals around the difference value...
dat_sig <- 
  dat2 %>% 
  mutate(gap_hi = gap_kgha + 1.96*gap_sd,
         gap_lo = gap_kgha - 1.96*gap_sd,
         sig_ind = case_when(
           (gap_lo < 0 & gap_hi > 0) ~ "ns",
           TRUE ~ "sig")
  )

#--write it to use in other places
dat_sig %>% 
  filter(n == 4) %>% 
  select(site, year, gap_kgha, gap_sd, gap_hi, gap_lo, sig_ind) %>% 
  write_csv("00_exp-variability/dat_gap-cis.csv")


#2. What years were the differences 'real'? I can assess this visually or programmatically (does the 95 CI include 0). 

dat_sig %>% 
  ggplot(aes(year, gap_kgha)) + 
  geom_linerange(aes(ymin = gap_kgha - 1.96*gap_sd,
                     ymax = gap_kgha + 1.96*gap_sd,
                     color = sig_ind)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(n~site, scales = "free") + 
  scale_color_manual(values = c("gray80", "red")) +
  labs(title = "Which years had sig diffs?") 


#--reorder that fig
library(tidytext)

dat_sig %>% 
  filter(n == 4) %>% 
  mutate(site = as.factor(site),
         year = reorder_within(year, -gap_kgha, site)) %>% 
  ggplot(aes(year, gap_kgha)) + 
  geom_linerange(aes(ymin = gap_kgha - 1.96*gap_sd,
                     ymax = gap_kgha + 1.96*gap_sd,
                     color = sig_ind), size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~site, scales = "free") + 
  scale_x_reordered() +
  scale_color_manual(values = c("gray70", "red")) +
  labs(title = "How confident are we in the yearly variation of the penalty?",
       subtitle = "Best case, can only say it was 'high' or 'low'") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("00_exp-variability/fig_uncertain-variability.png")


#--fix y scale to compare across sites
dat_sig %>% 
  filter(n == 4) %>% 
  mutate(site = as.factor(site),
         year = reorder_within(year, -gap_kgha, site)) %>% 
  ggplot(aes(year, gap_kgha)) + 
  geom_linerange(aes(ymin = gap_kgha - 1.96*gap_sd,
                     ymax = gap_kgha + 1.96*gap_sd,
                     color = sig_ind), size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~site, scales = "free_x") + 
  scale_x_reordered() +
  scale_color_manual(values = c("gray70", "red")) +
  labs(title = "Is site or year explaining more variation?",
       subtitle = "Greater variation over years than by site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("00_exp-variability/fig_site-effect.png")



dat_sig %>% 
  unite(site, year, col = "sy") %>% 
  select(sy) %>% 
  distinct() %>% 
  count()

#--combine into one
dat_sig %>% 
  filter(n == 4) %>% 
  unite(site, year, col = "site_year") %>% 
  ggplot(aes(reorder(site_year, -gap_kgha), gap_kgha)) + 
  geom_linerange(aes(ymin = gap_kgha - 1.96*gap_sd,
                     ymax = gap_kgha + 1.96*gap_sd,
                     color = sig_ind), size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("gray80", "red")) +
  labs(title = "Continuous corn penalty",
       subtitle = "104 site years",
       x = NULL, 
       y = "95%CI yield penalty (kg/ha)") + 
  theme(axis.text.x = element_blank())

ggsave("00_exp-variability/fig_iowa-penalty.png")

#--express as a percent
dat_sig %>% 
  filter(n == 4) %>% 
  left_join(ylds) %>% 
  mutate(yld_cc = (cc),
         gap_p = gap_kgha/yld_cc,
         gap_sdp = gap_sd/yld_cc,
         gap_hip = gap_hi/yld_cc,
         gap_lop = gap_lo/yld_cc) %>%
  unite(site, year, col = "site_year") %>% 
  ggplot(aes(reorder(site_year, -gap_p), gap_p)) + 
  geom_point(aes(color = sig_ind, size = yld_cc)) +
  # geom_linerange(aes(ymin = gap_lop,
  #                    ymax = gap_hip,
  #                    color = sig_ind), size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("gray80", "red")) +
  labs(title = "Continuous corn penalty",
       subtitle = "104 site years",
       x = NULL, 
       y = "95%CI yield penalty (percent)") + 
  theme(axis.text.x = element_blank())

#ggsave("00_exp-variability/fig_iowa-penalty.png")


#--weight line by avg yield

dat_sig %>% 
  filter(n == 4) %>% 
  left_join(ylds) %>% 
  mutate(yld_avg = (cc + sc)/2) %>% 
  unite(site, year, col = "site_year") %>% 
  ggplot(aes(reorder(site_year, -gap_kgha), gap_kgha)) + 
  geom_linerange(aes(ymin = gap_kgha - 1.96*gap_sd,
                     ymax = gap_kgha + 1.96*gap_sd,
                     color = yld_avg)) +
#  geom_point(aes(y = gap_kgha, size = yld_avg)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_grafify_c() +
  labs(title = "Continuous corn penalty",
       subtitle = "104 site years",
       x = NULL, 
       y = "95%CI yield penalty (kg/ha)") + 
  theme(axis.text.x = element_blank())

dat_sig %>% 
  filter(n == 4) %>% 
  left_join(ylds) %>% 
  mutate(yld_avg = (cc + sc)/2) %>% 
  unite(site, year, col = "site_year") %>% 
  ggplot(aes(yld_avg, gap_kgha)) + 
  geom_linerange(aes(ymin = gap_kgha - 1.96*gap_sd,
                     ymax = gap_kgha + 1.96*gap_sd, 
                     color = sig_ind), size = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("gray70", "red")) +
  labs(title = "Penalty not strongly related to size of yield")

ggsave("00_exp-variability/fig_yld-vs-pensize.png")

# power analysis ----------------------------------------------------------

#--get % sig at each n
dat_sig_pct <- 
  dat_sig %>% 
  #mutate(num_dif = ifelse(gap_kgha > 0, "num_pen", "none")) %>% 
  group_by(n, sig_ind) %>% 
  summarise(nsigs = n()) %>%
  group_by(n) %>% 
  mutate(ntot = sum(nsigs),
         n_pct = round(nsigs/ntot*100, 0)) %>% 
  filter(sig_ind == "sig")


dat_sig %>%
  unite(site, year, col = "site_year") %>% 
  ggplot(aes(reorder(site_year, -gap_kgha), gap_kgha)) + 
  geom_linerange(aes(ymin = gap_kgha - 1.96*gap_sd,
                     ymax = gap_kgha + 1.96*gap_sd,
                     color = sig_ind)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(data = dat_sig_pct, 
            x = 90, y = 4000,
            aes(label = paste0(n_pct, "% of site years sig"))) +
  facet_grid(n~.) + 
  scale_color_manual(values = c("gray80", "red")) +
  labs(title = "How many reps will it take?",
       subtitle = "Power analysis using 105 site years of experimental data") 

ggsave("00_exp-variability/fig_power-analysis.png")

dat_sig %>% 
  filter(!is.na(gap_kgha)) %>% 
  mutate(num_dif = ifelse(gap_kgha > 0, "num_pen", "none")) %>% 
  select(sig_ind, num_dif) %>% 
  group_by(num_dif, sig_ind) %>% 
  summarise(n = n()) %>% 
  filter(num_dif == "num_pen") %>% 
  mutate(ntot = sum(n),
         n_pct = n/ntot)


# take most extreme years -------------------------------------------------


dat_sig %>% 
  mutate(gap_hi = gap_kgha + 1.96*gap_sd,
         gap_lo = gap_kgha - 1.96*gap_sd,
         sig_ind = case_when(
           (gap_lo < 0 & gap_hi > 0) ~ "ns",
           TRUE ~ "sig")
  ) %>%
  filter(sig_ind == "sig") %>% 
  ggplot(aes(year, gap_kgha)) + 
  geom_linerange(aes(ymin = gap_kgha - 1.96*gap_sd,
                     ymax = gap_kgha + 1.96*gap_sd,
                     color = sig_ind)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~site, scales = "free")

#--what if I just take the most extreme years in ames
dat_sig %>% 
  filter(site == "ames") %>% 
  select(site, year, gap_kgha, gap_sd, gap_hi, gap_lo, sig_ind) %>% 
  arrange(-gap_kgha)

dat_sig %>% 
  mutate(gap_hi = gap_kgha + 1.96*gap_sd,
         gap_lo = gap_kgha - 1.96*gap_sd,
         sig_ind = case_when(
           (gap_lo < 0 & gap_hi > 0) ~ "ns",
           TRUE ~ "sig")
  ) %>%
  filter(site == "ames", n == 4) %>% 
  ggplot(aes(reorder(year, -gap_kgha), gap_kgha)) + 
  geom_linerange(aes(ymin = gap_kgha - 1.96*gap_sd,
                     ymax = gap_kgha + 1.96*gap_sd,
                     color = sig_ind)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~site, scales = "free")

#--2003 very high, 2009 very low, maybe 2013 no diff

#--do the multicomparison within ames

dat_ames <- dat_sig %>% filter(site == "ames", n == 4) %>% unite(site, year, col = "site_year")

dat_hi <- 
  dat_ames %>%
  select(site_year, gap_hi) 

dat_hi  

dat_lo <- 
  dat_ames %>% 
  select(site_year, gap_lo) %>% 
  pivot_wider(names_from = site_year, values_from = gap_lo)

dat_lo  

dat_ames2 <- 
  dat_ames %>% 
  select(site_year, gap_hi, gap_lo) %>% 
  pivot_wider(names_from = site_year, values_from = gap_lo) %>% 
  fill(2:ncol(.), .direction = "downup") %>% 
  pivot_longer(!gap_hi, names_to = "site_year", values_to = "gap_lo") %>% 
  arrange(-gap_hi) %>% 
  mutate(sigdif = ifelse(gap_hi > gap_lo, FALSE, TRUE)) %>% 
  select(gap_hi, site_year, sigdif) %>%
  rename("site_year2" = site_year) %>% 
  left_join(dat_hi, by = "gap_hi") %>%
  filter(!(site_year == site_year2)) %>% 
  unite(site_year, site_year2, col = "comparison", sep = "-") %>% 
  select(-gap_hi) 

vec_ames <- dat_ames2$sigdif
names(vec_ames) <- dat_ames2$comparison
vec_ames

vec_amesL <- multcompLetters(vec_ames)

dat_ames_lets <- 
  tibble(site_year =  vec_amesL %>% pluck("Letters") %>% names(),
         siglet = vec_amesL %>% pluck("Letters") %>% as.vector())

#----add to the plot

dat_sig %>%
  filter(site == "ames", n == 4) %>%
  unite(site, year, col = "site_year") %>% 
  ggplot(aes(reorder(site_year, -gap_kgha), gap_kgha)) + 
  geom_linerange(aes(ymin = gap_lo,
                     ymax = gap_hi,
                     color = sig_ind)) + 
  geom_text(data = dat_ames_lets, aes(x = site_year, y = 3500, label = siglet)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 45))

#--maybe pick years 2001, 2014?

oats <- read_csv("01_sims-oat-by-hand/dat_tidy-hand-oats.csv")

oats %>% 
  filter(year %in% c(2001, 2014)) %>% 
  mutate(oat_what = case_when(
           grepl("exp gap", oat_what) ~ "1_exp gap",
           grepl("current", oat_what) ~ "2_current apsim gap",
           TRUE ~ oat_what)) %>% 
  filter(category == "1 factor") %>% 
  ggplot(aes(year, gap_kgha)) + 
  geom_bar(aes(fill = oat_what),
           #alpha = oat_what %in% c("1_exp gap")),
           position = "dodge", stat = "identity") +#, color = "black") + 
  facet_grid(category ~oat_what) + 
  guides(fill = F, alpha = F) +
  labs(title = "Ames",
       x = NULL) 


#3. Given the variation we see in measuring yields at this N rate (note the variation itself varies each year), can I figure out what the least significnat difference I am able to detect is? Can I get this estimate on a site-basis?

#4. Flip side, can I make a graph of replicates by smallest detectable difference for each site? To help each site decide if it's feasible, or to identify sites that might give more precise values. Or perhaps it's better to pool information across sites. 

sds %>% 
  pivot_longer(cc_sd:sc_sd) %>% 
  ggplot(aes(year, value)) + 
  geom_point() + 
  facet_grid(.~site, scales = "free")
