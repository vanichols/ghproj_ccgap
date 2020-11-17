# make a dag for prev crop -> plant pop
# 11/11/20 looked at it
# 11/17/20 fernando'd it

library(dplyr)
library(dagitty)
library(ggdag)


# full, short term --------------------------------------------------------------

coords1 <- list(
  x = c(PRVCP = 0,
        
        RES_AMT = 2,
        RES_TYPE = 2,
        RES_CN = 2,
        SDPRESS = 2,
        RT_CHN = 2,
        AMF = 2,
        
        ROOTS = 9,
        SOILN = 4,
        FDPRESS = 4,
        
        SOILT = 2.5, SOILM = 4,
        PHYS = 4.5, 
        EROS = 5.5, RAIN = 6, SPDGRM = 4, 
        SDLGSURV = 6, 
        POP = 7,
        YLD = 9),
  y = c(
    PRVCP = 0,
    
    RES_AMT = 0,
    RES_TYPE = 2,
    RES_CN = 4,
    
    ROOTS = -8,
    SOILN = 4,
    FDPRESS = 2,
    SDPRESS = -6,
    RT_CHN = -8,
    AMF = -10,
    
    SOILT = -2, SOILM = -2,
    PHYS = 0, 
    EROS = -2, RAIN = 0, SPDGRM = -5, 
    SDLGSURV = -6, 
    POP = -4,
    YLD = 0)
  )

dag1 <- dagify(
  #-prev crop
  RES_AMT ~ PRVCP,
  RES_TYPE ~ PRVCP,
  SDPRESS ~ PRVCP,
  RES_CN ~ PRVCP,
  RT_CHN ~ PRVCP,
  AMF ~ PRVCP,
  
  #-res type
  FDPRESS ~ RES_AMT,
  FDPRESS ~ RES_TYPE,
  
  #-res amt
  PHYS ~ RES_AMT,
  SOILT ~ RES_AMT,
  SOILM ~ RES_AMT,
  
  SOILN ~ RES_CN,
  SOILN ~ RES_AMT,
  
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
  ROOTS ~ SDPRESS,
  ROOTS ~ RT_CHN,
  ROOTS ~ AMF,
  
  YLD ~ SOILN,
  YLD ~ ROOTS,
  YLD ~ POP,
  YLD ~ FDPRESS,
  
  exposure = "PRVCP",
  outcome = "YLD", 
  latent = c("RES_CN", "RES_TYPE", "SDPRESS"), #--what is a latent variable?
  coords = coords1
  )

#--original
tidy_dagitty(dag1) %>% 
  ggdag(., node_size = 12, text_size = 1.5) + 
  geom_dag_node(color = "brown", internal_color = "orange") + 
  geom_dag_text(color = "white") + 
  ggtitle("Short Term\nPrevious Crop (PRVCP), Yield (YLD)") + 
  theme_dag_blank() + 
  theme(panel.background = element_rect(color = "black", fill = "gray70"))



#--playing
tidy_dagitty(dag1) %>% 
  mutate(clr = ifelse((name == "SOILN" & to == "YLD")|
                         (name == "PHYS" & to == "SDLGSURV")|
                      (name == "EROS" & to == "SDLGSURV")|
                        (name == "RAIN" & to == "SDLGSURV"),
                      "A", "B")) %>% 
  ggdag(., node_size = 12, text_size = 1.5) + 
  geom_dag_node(aes(color = clr), internal_color = "orange") + 
  geom_dag_text(color = "white") +
  guides(color = F) +
  ggtitle("Short Term\nPrevious Crop (PRVCP), Yield (YLD)") + 
  theme_dag_blank() + 
  theme(panel.background = element_rect(color = "black", fill = "gray70"))

ggsave("00_dags/dag_prevcrop-yld-v2.png")


#--latent vars?
tidy_dagitty(dag1) %>% 
  ggdag_paths(., shadow = TRUE) +
  theme_dag(base_size = 14) +
  theme(legend.position = "none", strip.text = element_blank()) + 
  # set node aesthetics
  scale_color_manual(values = "#0072B2", na.value = "grey80") + 
  # set label aesthetics
  scale_fill_manual(values = "#0072B2", na.value = "grey80") + 
  # set arrow aesthetics
  ggraph::scale_edge_color_manual(values = "#0072B2", na.value = "grey80") +
  ggtitle("Open paths from previous crop to yields")




# example. not sure what latent variable means
# https://malco.io/2019/09/17/tidy-causal-dags-with-ggdag-0-2-0/
smoking_ca_dag <- dagify(cardiacarrest ~ cholesterol,
                           cholesterol ~ smoking + weight,
                           smoking ~ unhealthy,
                           weight ~ unhealthy,
                           labels = c("cardiacarrest" = "Cardiac\n Arrest", 
                                      "smoking" = "Smoking",
                                      "cholesterol" = "Cholesterol",
                                      "unhealthy" = "Unhealthy\n Lifestyle",
                                      "weight" = "Weight"),
                           latent = "unhealthy",
                           exposure = "smoking",
                           outcome = "cardiacarrest") %>% 
    tidy_dagitty()
  
  smoking_ca_dag
  ggdag(smoking_ca_dag, text = FALSE, use_labels = "label")
  
  
  
  
  ggdag_paths(smoking_ca_dag, text = FALSE, use_labels = "label", shadow = TRUE) +
    theme_dag(base_size = 14) +
    theme(legend.position = "none", strip.text = element_blank()) + 
    # set node aesthetics
    scale_color_manual(values = "#0072B2", na.value = "grey80") + 
    # set label aesthetics
    scale_fill_manual(values = "#0072B2", na.value = "grey80") + 
    # set arrow aesthetics
    ggraph::scale_edge_color_manual(values = "#0072B2", na.value = "grey80") +
    ggtitle("Open paths from smoking to cardiac arrest")

