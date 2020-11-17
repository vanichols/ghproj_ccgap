#--how does residue cause disease?
#--updated 11/11 based on Alison email

remove(list = ls())

library(dagitty)
library(ggdag)

#--fernando-style

coords_surf <- list(x = c(RES = 0, 
                      SOILENV = 1, FPOT = 1,
                      WEA = 1.5,
                      SPOT = 2,
                      FDIS = 2,
                      SDIS = 3), 
                y = c(RES = 0, 
                      SOILENV = 1, FPOT = -1,
                      WEA = 0,
                      SPOT = 1,
                      FDIS = -1,
                      SDIS = 1))

dag_surf <- dagify(
  SOILENV ~ RES,
  SOILENV ~ WEA,
  SPOT ~ SOILENV,
  SDIS ~ SPOT,
  
  FPOT ~ RES,
  FDIS ~ FPOT,
  FDIS ~ WEA,
  coords = coords_surf
)

ggdag(dag_surf, node_size = 20, text_size = 2.5) + 
  geom_dag_node(color = "brown", internal_color = "orange") + 
  geom_dag_text(color = "white") + 
  ggtitle("Surface Residue (RES), Seedling Disease (SDIS), Foliar Disease (FDIS)") + 
  theme_dag_blank() + 
  theme(panel.background = element_rect(color = "black", fill = "gray80"))

ggsave("00_dags/fig_surf-res-disease.png")
