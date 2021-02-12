# created 2/12/21
# gina
# purpose: mike thinks I can get some kind of info from aonr stuff
# updated

library(tidysawyer2)
library(tidyverse)



# dat ---------------------------------------------------------------------

ilia_aonr %>% 
  ggplot(aes(year, aonr, color = rotation)) + 
  geom_jitter() + 
  facet_wrap(~site)

#--filter out years where aonr is max value


ilia_aonr %>%
  anti_join( ilia_aonr %>% 
              group_by(site) %>%
              filter(aonr > max(aonr) - 5)) %>% 
  ggplot(aes(year, aonr, color = rotation)) + 
  geom_point() + 
  facet_wrap(~site)

