# make a dag for prev crop -> plant pop
# 11/11/20 looked at it
# 11/17/20 fernando'd it

library(dagitty)
library(ggdag)


nt_coords <- list(
  x = c(PRVCP = 0,
        
        RES_AMT = 2,
        RES_TYPE = 2,
        SDPRESS = 2,
        
        FDPRESS = 3,
        
        SOILT = 3, SOILM = 4,
        PHYS = 4.5, 
        EROS = 5.5, RAIN = 6, SPDGRM = 4, 
        SDLGSURV = 6, 
        POP = 7),
  y = c(
    PRVCP = 0,
    
    RES_AMT = 0,
    RES_TYPE = 2,
    
    FDPRESS = 2,
    SDPRESS = -6,
    
    SOILT = -2, SOILM = -1,
    PHYS = 0, 
    EROS = -1, RAIN = 0, SPDGRM = -5, 
    SDLGSURV = -6, 
    POP = -6)
  )

nt_dag <- dagify(
  #-prev crop
  RES_AMT ~ PRVCP,
  RES_TYPE ~ PRVCP,
  SDPRESS ~ PRVCP,
  
  #-res type
  FDPRESS ~ RES_AMT,
  FDPRESS ~ RES_TYPE,
  
  #-res amt
  PHYS ~ RES_AMT,
  SOILT ~ RES_AMT,
  SOILM ~ RES_AMT,
  
    #--phys
  EROS ~ PHYS,
  RAIN ~ PHYS,
  SDLGSURV ~ PHYS,
  SDLGSURV ~ EROS,
  SDLGSURV ~ RAIN,
  EROS ~ PHYS,

    #--soil disease
  SPDGRM ~ SOILT,
  SDPRESS ~ SOILT,
  SDLGSURV ~ SOILT,
  SOILM ~ SOILT,
  SDPRESS ~ SOILM,
  SDLGSURV ~ SOILM,
  SDPRESS ~ SPDGRM,
  POP ~ SDLGSURV,
  SDLGSURV ~ SDPRESS,
  
  
  
  exposure = "PRVCP",
  outcome = "POP", 
  coords = nt_coords
  )


ggdag(nt_dag, node_size = 20, text_size = 2.5) + 
  geom_dag_node(color = "brown", internal_color = "orange") + 
  geom_dag_text(color = "white") + 
  ggtitle("Previous Crop (PRVCP), Plant Population (POP)") + 
  theme_dag_blank() + 
  theme(panel.background = element_rect(color = "black", fill = "gray80"))

ggsave("00_dags/fig_prevcrop-plantpop.png")
