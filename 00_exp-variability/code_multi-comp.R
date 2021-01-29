# Gina Nichols, Jan 25 2021
# Goal: explore variability in difference estimate
library(purrr)
library(repurrrsive)

library(tidyverse)
library(tidysawyer2) 
library(grafify)
library(multcompView)

theme_set(theme_bw())

dat <- read_csv("00_exp-variability/dat_gap-cis.csv") %>% 
  unite(site, year, col = "site_year", sep = "")


dat %>%  
  ggplot(aes(reorder(site_year, -gap_hi), gap_kgha)) + 
  geom_linerange(aes(ymin = gap_lo,
                     ymax = gap_hi,
                     color = sig_ind)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("gray80", "red")) +
  labs(title = "How many reps will it take?",
       subtitle = "Power analysis using 105 site years of experimental data") 


# letters ---------------------------------------------------------

dat_real <- 
  dat %>% 
  arrange(-gap_hi) 

dat_real %>%  
  ggplot(aes(reorder(site_year, -gap_kgha), gap_kgha)) + 
  geom_linerange(aes(ymin = gap_lo,
                     ymax = gap_hi,
                     color = sig_ind)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("gray80", "red")) #+
  #coord_flip()


dat_hi <- 
  dat_real %>%
  select(site_year, gap_hi) 

dat_hi  

dat_lo <- 
  dat_real %>% 
  select(site_year, gap_lo) %>% 
  pivot_wider(names_from = site_year, values_from = gap_lo)

dat_lo  

dat_real2 <- 
  dat_real %>% 
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

vec_real <- dat_real2$sigdif
names(vec_real) <- dat_real2$comparison
vec_real

vec_realL <- multcompLetters(vec_real)

dat_lets <- 
  tibble(site_year =  vec_realL %>% pluck("Letters") %>% names(),
         siglet = vec_realL %>% pluck("Letters") %>% as.vector())

#--not sure how to get the number of 'groups' from this...are there only 2?
dat_real %>%  
  ggplot(aes(reorder(site_year, -gap_kgha), gap_kgha)) + 
  geom_linerange(aes(ymin = gap_lo,
                     ymax = gap_hi,
                     color = sig_ind)) +
  geom_text(data = dat_lets, aes(x = site_year, y = -100, label = siglet)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("gray80", "red")) 

dat_lets


# letters practice---------------------------------------------------------

dat_prac <- 
  dat %>% 
  arrange(-gap_hi) %>% 
  slice(1:5, 50:55) %>% 
  filter(sig_ind == "sig")

dat_prac %>%  
  ggplot(aes(reorder(site_year, -gap_hi), gap_kgha)) + 
  geom_linerange(aes(ymin = gap_lo,
                     ymax = gap_hi)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("gray80", "red")) 


dat_hi <- 
  dat_prac %>%
  select(site_year, gap_hi) 

dat_hi  

dat_lo <- 
  dat_prac %>% 
  select(site_year, gap_lo) %>% 
  pivot_wider(names_from = site_year, values_from = gap_lo)

dat_lo  


dat_prac2 <- 
  dat_prac %>% 
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

vec_prac <- dat_prac2$sigdif
names(vec_prac) <- dat_prac2$comparison

vec_pracL <- multcompLetters(vec_prac)

dat_lets_prac <- 
  tibble(site_year =  vec_pracL %>% pluck("Letters") %>% names(),
         siglet = vec_pracL %>% pluck("Letters") %>% as.vector())

dat_prac %>%  
  ggplot(aes(reorder(site_year, -gap_hi), gap_kgha)) + 
  geom_linerange(aes(ymin = gap_lo,
                     ymax = gap_hi)) +
  geom_text(data = dat_lets_prac, aes(x = site_year, y = -100, label = siglet), size = 10) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("gray80", "red")) 


# try table thing ---------------------------------------------------------

inthing <- letters[1:3]

for (i in 1:length(inthing)){
 
  i <- 1
  alph.tmp <- inthing[i]
  
  if (i == 1) {
  
   dat.tmp <- 
     dat_lets_prac %>% 
     mutate(mylet = alph.tmp) %>% 
     filter(grepl(alph.tmp, siglet)) %>% 
            mutate(myletT = "Y")
   
   dat_tbl_prac <- dat.tmp
   
  } else {
    
    dat.tmp <- 
      dat_lets_prac %>% 
      mutate(mylet = alph.tmp) %>% 
      filter(grepl(alph.tmp, siglet)) %>% 
      mutate(myletT = "Y") 
    
    dat_tbl_prac <- dat_tbl_prac %>% bind_rows(dat.tmp)
    
  }
}

dat_tbl_prac %>% 
  pivot_wider(names_from = mylet, values_from = myletT) %>% 
  replace(is.na(.), "N") %>%
  select(-siglet) %>% 
  pivot_longer(!site_year) %>% 
  ggplot(aes(name, site_year)) + 
  geom_tile(aes(fill = value))

dat_matrix_prac <- 
  dat_prac %>% 
  select(site_year, gap_hi, gap_lo) %>% 
  pivot_wider(names_from = site_year, values_from = gap_lo) %>% 
  fill(2:ncol(.), .direction = "downup") %>% 
  pivot_longer(!gap_hi, names_to = "site_year", values_to = "gap_lo") %>% 
  arrange(-gap_hi) %>% 
  mutate(sigdif = ifelse(gap_hi > gap_lo, TRUE, FALSE)) %>% 
  select(gap_hi, site_year, sigdif) %>% 
  pivot_wider(names_from = site_year, values_from = sigdif) %>% 
  left_join(dat_hi) %>% 
  select(site_year, everything(), -gap_hi)

mat_prac <- as.matrix(dat_matrix_prac %>% select(-site_year),
                      dimnames = list(dat_matrix_prac$site_year))

dimnames(mat_prac)
multcompLetters(mat_prac)


## 4. cor matrix
##
set.seed(4)
x100 <- matrix(rnorm(100), ncol=5,
               dimnames=list(NULL, LETTERS[1:5]) )


dif3L <- multcompLetters(dif3)

##
## 1. a logical vector indicating signficant differences
##
dif3 <- c(FALSE, FALSE, TRUE)
names(dif3) <- c("A-B", "A-C", "B-C")
dif3L <- multcompLetters(dif3)
