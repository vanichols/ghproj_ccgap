library(tidyverse)
library(tibble)
library(ggplot2)
library(tidyr)
library(ggmosaic)

datA <- tibble(year = c(2003, 2004, 2005),
       yield = c(55, 34, 10),
       nfrac = c(50, 40, 80),
       ofrac = c(50, 60, 20),
       site = "ames")


datB <- tibble(year = c(1999, 2000),
               yield = c(5, 37),
               nfrac = c(10, 30),
               ofrac = c(90, 70),
               site = "brow")


dat <- bind_rows(datA, datB)


# miranda -----------------------------------------------------------------

#--made a function from her code

mirandaFun <- function(f.dat = datA) {
  
  f.site <- f.dat %>% pull(site) %>% unique()
  dat2 <- f.dat
  
  dat2$nfrac.p <- f.dat$nfrac / 100
  dat2$ofrac.p <- f.dat$ofrac / 100
  dat2$yield <- 100*f.dat$yield
  
  plot_dat <- data.frame(Year = rep(dat2$year, times = dat2$yield))    
  
  Type <- c()
  for(i in 1:nrow(f.dat)) {
    Type <- c(Type,
              rep("nfrac", dat2$yield[i] * dat2$nfrac.p[i]),
              rep("ofrac", dat2$yield[i] * dat2$ofrac.p[i]))
  }
  plot_dat$Type <- as.factor(Type)

  res <- plot_dat %>% as_tibble() %>% mutate(site = f.site)
  
  return(res)
    
}

#--do the function on each site's dataset
pdatA <- mirandaFun(f.dat = datA)
pdatB <- mirandaFun(f.dat = datB)

#--combine them
pdat <- bind_rows(pdatA, pdatB)


#--oh no! How do I make sure they are scaled correctly? 
# brow should have smaller widths than ames
#Is there a way?
ggplot(pdat) + 
  geom_mosaic(aes(x = product(Type, Year), fill = Type)) +
  labs(x="Year", y = "Type") +
  facet_grid(.~site, scales = "free")

# read this. https://jtr13.github.io/cc19/ggmosaic.html

#--whoa this isn't right. 
  ggplot(pdat) + 
  geom_mosaic(aes(x = product(Type), fill = Type, conds = product(Year, site)))
  

  ggplot(pdat) + 
    geom_mosaic(aes(x = product(Type, Year), fill = Type, conds = product(site)))

  #bah. 
  
  
  mirandaFun <- function(f.dat = dat) {
    
    #f.site <- f.dat %>% pull(site) %>% unique()
    dat2 <- f.dat
    
    dat2$nfrac.p <- f.dat$nfrac / 100
    dat2$ofrac.p <- f.dat$ofrac / 100
    dat2$yield <- 100*f.dat$yield
    
    plot_dat <- data.frame(Year = rep(dat2$year, times = dat2$yield))    
    
    Type <- c()
    for(i in 1:nrow(f.dat)) {
      Type <- c(Type,
                rep("nfrac", dat2$yield[i] * dat2$nfrac.p[i]),
                rep("ofrac", dat2$yield[i] * dat2$ofrac.p[i]))
    }
    plot_dat$Type <- as.factor(Type)
    plot_dat$site <- rep(dat2$site, times = dat2$yield)
    
    #res <- plot_dat %>% as_tibble() %>% mutate(site = f.site)
    res <- plot_dat
    return(res)   
  }
  
  pdat <- mirandaFun(dat)
  pdat$fs <- as.factor(paste(pdat$Type, pdat$site)) # crude factor for colors
  
  ggplot(pdat) + geom_mosaic(aes(x = product(Type, Year), fill = fs))
  