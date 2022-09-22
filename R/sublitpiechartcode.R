#loading in neccesary packages#
require(tidyverse)
require(plyr)
require(ggplot2)

# Import data set #
library(readxl)
GC_sublit_data <- read_excel("Desktop/GC_Analysis_sublit/GC_sublit_data.xlsx")
View(GC_sublit_data)
#rename#
sublit <- GC_sublit_data

#Organizning data#
sublit %>% 
  #Filtering for usable papers#
  filter(usable=="y") %>%
  #summarising number of papers under each conditions (4 in total)#
  ddply(.(`Measured fitness`, `Fitness implications of GCs`), summarise, value=sum(value)) %>%
  #combining columns: 'Measured fitness' and 'fitness implications of GC's' for creating plot#
  unite(., FM_and_I, c(`Measured fitness`, `Fitness implications of GCs`), sep='&') %>%
  #plot for piechart#
  ggplot(aes(x="", y=value, fill=FM_and_I)) +
  geom_col(color="black") +
  coord_polar(theta = "y") +
  geom_text(aes(label = value), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("#BE2A3E", "#EC754A",
                             "#EACF65", "#3C8D53"), 
                    guide=guide_legend(title=NULL), labels=c("FNM and NI", "FNM and I", "FM and NI", "FM and I")) +
  theme_void()

