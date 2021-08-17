# talk palette for rotation

clr_cc <- "gold"
#  hex"#FFD700"

clr_rot <- "green4"
#  hex "#008B00" 

clr_div <- "darkorchid4"
#  hex "#68228B"

clr_red <- "#FF4500"

clr_blu <- "#00B2EE"

clr_or <- "#EE9A00"

clr_ltpur <- "#AB82FF"


#--general theme with bigger fonts
bigtheme <- 
  theme(
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size =rel(1.5)),
    panel.grid = element_blank(),
    axis.text = element_text(size = rel(1.5)),
    axis.title = element_text(size = rel(1.5)))


#--help with axis labels
yld_lab <- (expression(atop("Corn Yield", paste("(Mg "~ha^-1*")"))))

n_lab <- (expression("Nitrogen fertilization rate (kg N "~ha^-1*")"))