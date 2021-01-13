# make a dag for prev crop -> plant pop
# 11/11/20 looked at it
# 11/17/20 fernando'd it

library(dplyr)
library(dagitty)
library(ggdag)

wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}



# 1. full, short term --------------------------------------------------------------

coords1 <- list(
  x = c(PRVCP = 0,
        
        AMF = 2,
        
        EMDOY = 5,
        EROS = 5.5, 
        FOLD = 4,
        
        PHYS = 4.5,
        POP = 7.5,
        
        RAIN = 6, 
        RES_A = 2,
        RES_T = 2,
        RES_CN = 2,
        ROOTS = 9,
        RT_CHN = 2,
        
        SOILD = 4,
        SOILM = 4,
        SOILN = 4,
        SOILT = 2.5, 
        SPDGRM = 2.5, 
        SSURV = 6, 
        
        WEA = 6,
        
        YLD = 9),
  y = c(
    PRVCP = 0,
    
    AMF = -15,
    
    
    EROS = -2, 
    EMDOY = -18,
    FOLD = 6,
    
    PHYS = 0, 
    POP = -3.5,
    
    RAIN = 0,
    RES_A = 0,
    RES_T = 3,
    RES_CN = 6,
    ROOTS = -12,
    RT_CHN = -12,
    
    
    SOILD = -9,
    SOILM = -3,
    SOILN = 9,
    SOILT = -3, 
    SPDGRM = -9, 
    SSURV = -6, 
    
    YLD = 0,
    WEA = 12)
  )

dag1 <- dagify(
  AMF ~ PRVCP,
  
  EMDOY ~ SPDGRM,
  EROS ~ PHYS,
  
  FOLD ~ RES_A,
  FOLD ~ RES_T,
  FOLD ~ WEA,
  
  PHYS ~ RES_A,
  POP ~ SSURV,
  
  RAIN ~ PHYS,
  RAIN ~ WEA,
  
  RES_A ~ PRVCP,
  RES_T ~ PRVCP,
  RES_CN ~ PRVCP,
  
  ROOTS ~ SOILD,
  ROOTS ~ RT_CHN,
  ROOTS ~ AMF,
  RT_CHN ~ PRVCP,
  
  #SOILD ~ PRVCP,
  SOILD ~ SOILT,
  SOILD ~ SOILM,
  SOILD ~ SPDGRM,
  
  SOILT ~ RES_A,
  SOILM ~ RES_A,
  SOILM ~ WEA,
  SOILM ~ SOILT,
  
  SOILN ~ RES_CN,
  SOILN ~ RES_A,
  SOILN ~ WEA,
  
  SOILT ~ WEA,
  
  
  SPDGRM ~ SOILT,
  
  SSURV ~ PHYS,
  SSURV ~ EROS,
  SSURV ~ RAIN,
  SSURV ~ SOILT,
  SSURV ~ SOILM,
  SSURV ~ SOILD,
  YLD ~ EMDOY,
  YLD ~ SOILN,
  YLD ~ ROOTS,
  YLD ~ POP,
  YLD ~ FOLD,
  
  exposure = "PRVCP",
  outcome = "YLD", 
  #latent = c("RES_CN", "RES_T", "SOILD"), #--what is a latent variable?
  coords = coords1
  )

#--original
tidy_dagitty(dag1) %>% 
  ggdag(., node_size = 5, text_size = 1.5) + 
  geom_dag_node(color = "brown", internal_color = "orange") + 
  geom_dag_text(color = "white") + 
  ggtitle("Short Term\nPrevious Crop (PRVCP), Yield (YLD)") + 
  theme_dag_blank() + 
  theme(panel.background = element_rect(color = "black", fill = "gray70"))


mykey <- "AMF = Arbuscular microrhizal fungi population EMDOY = Day of emergence EROS = Soil erosion FOLD = Foliar disease PHYS = Physical protection of soil POP = Plant population RAIN = Rain impact RES_CN = Residue CN ratio RES_T = Residue type RES_A = Residue amount ROOTS = Root growth/function RT_CHN = Root channels SOILD = Soil-borne disease SOILM = Soil moisture SOILN = Soil nitrogen SOILT = Soil temperature SPDGRM = Speed of germination SSURV = Seedling survival WEA = Weather"

mykeyonecol <- "AMF = Arbuscular microrhizal fungi population\nEMDOY = Day of emergence\nEROS = Soil erosion\nFOLD = Foliar disease\nPHYS = Physical protection of soil\nPOP = Plant population\nRAIN = Rain impact\nRES_CN = Residue CN ratio\nRES_T = Residue type\nRES_A = Residue amount\nROOTS = Root growth/function\nRT_CHN = Root channels\nSOILD = Soil-borne disease\nSOILM = Soil moisture\nSOILN = Soil nitrogen\nSOILT = Soil temperature\nSPDGRM = Speed of germination\nSSURV = Seedling survival\nWEA = Weather"
  

#--highlight things tried in apsim
tidy_dagitty(dag1) %>% 
  mutate(clr = ifelse((name == "SOILN" & to == "YLD")|
                        (name == "FOLD" & to == "YLD")|
                        (name == "POP" & to == "YLD")|
                        (name == "EMDOY" & to == "YLD")|
                        (name == "ROOTS" & to == "YLD"),
                      "A", "B")) %>% 
  ggdag(., node_size = 5, text_size = 1.5) + 
  geom_dag_node(aes(color = clr), internal_color = "orange") + 
  geom_dag_text(color = "white") +
  guides(color = F) +
  ggtitle("Short Term\nPrevious Crop (PRVCP), Yield (YLD)") + 
  scale_color_manual(values = c("darkblue", "gold4")) +
  theme_dag_blank() + 
  theme(panel.background = element_rect(color = "black", fill = "gray70")) +
  labs(caption = wrapper(mykey, width = 130)) + 
  theme(panel.background = element_rect(color = "black", fill = "gray70"),
        plot.caption = element_text(hjust = 0)) 


ggsave("00_dags/dag_prevcrop-yld-v2.png", height = 8, width = 8)




# 2. disconnect some things -----------------------------------------------

coords2 <- list(
  x = c(PRVCP = 0,
        AMF = 2,
        FOLD = 4,
        
        RES_A = 2,
        RES_T = 2,
        RES_CN = 2,
        ROOTS = 9,
        
        RT_CHN = 2,
        
        SOILD = 4.5,
        
        SOILN = 4,
        SOILT = 2.5, 
        SOILM = 4,
        
        PHYS = 5, 
        SPDGRM = 3, 
        SSURV = 6, 
        POP = 7.5,
        
        #WEA = 6,
        
        YLD = 9),
  y = c(
    PRVCP = 0,
    AMF = -15,
    FOLD = 3,
    PHYS = 0, 
    
    RES_A = 0,
    RES_T = 3,
    RES_CN = 6,
    ROOTS = -12,
    RT_CHN = -12,
    
    WEA = 6,
    
    SOILD = -10,
    SOILN = 6,
    SOILT = -6, 
    SOILM = -2,
    
    SPDGRM = -10, 
    SSURV = -6, 
    POP = -3.5,
    YLD = 0)
)

dag2 <- dagify(
  #-prev crop
  RES_A ~ PRVCP,
  RES_T ~ PRVCP,
  #SOILD ~ PRVCP,
  RES_CN ~ PRVCP,
  RT_CHN ~ PRVCP,
  AMF ~ PRVCP,
  
  #-res type
  FOLD ~ RES_A,
  FOLD ~ RES_T,
  
  #-res amt
  PHYS ~ RES_A,
  SOILT ~ RES_A,
  SOILM ~ RES_A,
  
  SOILN ~ RES_CN,
  SOILN ~ RES_A,
  
  #--phys
  #EROS ~ PHYS,
  #RAIN ~ PHYS,
 # SSURV ~ PHYS,
#  SSURV ~ EROS,
 # SSURV ~ RAIN,
  
  #FOLD ~ WEA,
  #SOILM ~ WEA,
  #RAIN ~ WEA,
  
  #--soil disease
  SPDGRM ~ SOILT,
  SOILD ~ SOILT,
  SSURV ~ SOILT,
  SOILM ~ SOILT,
  SOILD ~ SOILM,
  SSURV ~ SOILM,
  SOILD ~ SPDGRM,
  POP ~ SSURV,
  SSURV ~ SOILD,
  ROOTS ~ SOILD,
  ROOTS ~ RT_CHN,
  ROOTS ~ AMF,
  
  #YLD ~ SOILN,
  YLD ~ ROOTS,
  YLD ~ POP,
  YLD ~ FOLD,
  
  exposure = "PRVCP",
  outcome = "YLD", 
  #latent = c("RES_CN", "RES_T", "SOILD"), #--what is a latent variable?
  coords = coords2
)


tidy_dagitty(dag2) %>% 
  mutate(clr = ifelse((name == "SOILN" & to == "YLD")|
                        (name == "PHYS" & to == "RES_A")|
                        (name == "EROS" & to == "SSURV")|
                        (name == "RAIN" & to == "SSURV"),
                      "A", "B")) %>% 
  ggdag(., node_size = 5, text_size = 1.5) + 
  geom_dag_node(aes(color = clr), internal_color = "orange") + 
  geom_dag_text(color = "white") +
  guides(color = F) +
  ggtitle("Short term, high N rates, \nPrevious Crop (PRVCP), Yield (YLD)") + 
  theme_dag_blank() + 
  theme(panel.background = element_rect(color = "black", fill = "gray70"))

ggsave("00_dags/dag_prevcrop-yld-v3.png")

impliedConditionalIndependencies(dag2)



#--tilled system, simplified

dag2 <- dagify(
  #-prev crop
  RES_A ~ PRVCP,
  RES_T ~ PRVCP,
  SOILD ~ PRVCP,
  RES_CN ~ PRVCP,
  RT_CHN ~ PRVCP,
  AMF ~ PRVCP,
  
  #-res type
  FOLD ~ RES_A,
  FOLD ~ RES_T,
  
  #-res amt
  PHYS ~ RES_A,
  SOILT ~ RES_A,
  SOILM ~ RES_A,
  
  SOILN ~ RES_CN,
  SOILN ~ RES_A,
  
  #--phys
  EROS ~ PHYS,
  RAIN ~ PHYS,
  # SSURV ~ PHYS,
  #  SSURV ~ EROS,
  # SSURV ~ RAIN,
  EROS ~ PHYS,
  
  FOLD ~ WEA,
  SOILM ~ WEA,
  RAIN ~ WEA,
  
  #--soil disease
  SPDGRM ~ SOILT,
  SOILD ~ SOILT,
  SSURV ~ SOILT,
  SOILM ~ SOILT,
  SOILD ~ SOILM,
  SSURV ~ SOILM,
  SOILD ~ SPDGRM,
  POP ~ SSURV,
  SSURV ~ SOILD,
  ROOTS ~ SOILD,
  ROOTS ~ RT_CHN,
  ROOTS ~ AMF,
  
  #YLD ~ SOILN,
  YLD ~ ROOTS,
  YLD ~ POP,
  YLD ~ FOLD,
  
  exposure = "PRVCP",
  outcome = "YLD", 
  #latent = c("RES_CN", "RES_T", "SOILD"), #--what is a latent variable?
  coords = coords1
)





# 3. no soil N connection---------------------------------------------------------------

coords3 <- list(
  x = c(PRVCP = 0,
        
        AMF = 2,
        
        EMDOY = 5,
        EROS = 5.5, 
        FOLD = 4,
        
        PHYS = 4.5,
        POP = 7.5,
        
        RAIN = 6, 
        RES_A = 2,
        RES_T = 2,
        RES_CN = 2,
        ROOTS = 9,
        RT_CHN = 2,
        
        SOILD = 4,
        SOILM = 4,
        SOILN = 4,
        SOILT = 2.5, 
        SPDGRM = 2.5, 
        SSURV = 6, 
        
        WEA = 6,
        
        YLD = 9),
  y = c(
    PRVCP = 0,
    
    AMF = -15,
    
    
    EROS = -2, 
    EMDOY = -18,
    FOLD = 6,
    
    PHYS = 0, 
    POP = -3.5,
    
    RAIN = 0,
    RES_A = 0,
    RES_T = 3,
    RES_CN = 6,
    ROOTS = -12,
    RT_CHN = -12,
    
    
    SOILD = -9,
    SOILM = -3,
    SOILN = 9,
    SOILT = -3, 
    SPDGRM = -9, 
    SSURV = -6, 
    
    YLD = 0,
    WEA = 12)
)

dag3 <- dagify(
  AMF ~ PRVCP,
  
  EMDOY ~ SPDGRM,
  EROS ~ PHYS,
  
  FOLD ~ RES_A,
  FOLD ~ RES_T,
  FOLD ~ WEA,
  
  PHYS ~ RES_A,
  POP ~ SSURV,
  
  RAIN ~ PHYS,
  RAIN ~ WEA,
  
  RES_A ~ PRVCP,
  RES_T ~ PRVCP,
  RES_CN ~ PRVCP,
  
  ROOTS ~ SOILD,
  ROOTS ~ RT_CHN,
  ROOTS ~ AMF,
  RT_CHN ~ PRVCP,
  
  #SOILD ~ PRVCP,
  SOILD ~ SOILT,
  SOILD ~ SOILM,
  SOILD ~ SPDGRM,
  
  SOILT ~ RES_A,
  SOILM ~ RES_A,
  SOILM ~ WEA,
  SOILM ~ SOILT,
  
  SOILN ~ RES_CN,
  SOILN ~ RES_A,
  SOILN ~ WEA,
  
  SOILT ~ WEA,
  
  
  SPDGRM ~ SOILT,
  
  SSURV ~ PHYS,
  SSURV ~ EROS,
  SSURV ~ RAIN,
  SSURV ~ SOILT,
  SSURV ~ SOILM,
  SSURV ~ SOILD,
  YLD ~ EMDOY,
  #YLD ~ SOILN,
  YLD ~ ROOTS,
  YLD ~ POP,
  YLD ~ FOLD,
  
  exposure = "PRVCP",
  outcome = "YLD", 
  #latent = c("RES_CN", "RES_T", "SOILD"), #--what is a latent variable?
  coords = coords3
)

#--highlight things tried in apsim
tidy_dagitty(dag3) %>% 
  mutate(clr = ifelse((name == "FOLD" & to == "YLD")|
                        (name == "POP" & to == "YLD")|
                        (name == "EMDOY" & to == "YLD")|
                        (name == "ROOTS" & to == "YLD"),
                      "A", "B")) %>% 
  ggdag(., node_size = 5, text_size = 1.5) + 
  geom_dag_node(aes(color = clr), internal_color = "orange") + 
  geom_dag_text(color = "white") +
  guides(color = F) +
  ggtitle("Short Term\nPrevious Crop (PRVCP), Yield (YLD)") + 
  scale_color_manual(values = c("darkblue", "gold4")) +
  theme_dag_blank() + 
  theme(panel.background = element_rect(color = "black", fill = "gray70")) +
  labs(caption = wrapper(mykey, width = 130)) + 
  theme(panel.background = element_rect(color = "black", fill = "gray70"),
        plot.caption = element_text(hjust = 0)) 




# playing -----------------------------------------------------------------


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

