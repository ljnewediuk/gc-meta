#loading in neccesary packages#
require(tidyverse)
require(plyr)
require(ggplot2)

# Import data set #
library(readxl)
GC_sublit_data <- read_excel("input/GC_sublit_data.xlsx")
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
  
  ddply(.(FM_and_I), summarise, ratio=value/35, percent=round((value/35)*100, digits = 0)) %>%
 mutate(perc=case_when(percent>0 ~ "%")) %>%
unite(., percent, c('percent', 'perc'), sep="") %>%
  #plot for barchart#
  ggplot(aes(x=factor(FM_and_I, levels=c("n&y", "y&y", "y&n", "n&n"), labels = c("Did not measure fitness\nmade implication\non population health", "Measured fitness\nmade implication\non population health", "Measured fitness\ndid not make implication\non population health", "Did not measure fitness\ndid not make implication\non population health")), y=ratio*100, fill=FM_and_I)) +
  geom_col(color="black") +
  geom_text(aes(label = percent), colour="white",  
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "BrBG", guide=NULL) +
  labs(x=NULL, y="Percentage of papers (%)") +
  scale_y_continuous(limits=c(0,60), breaks=seq(0, 60, 15)) +
  theme(panel.border = element_rect(colour = "black", size=1, fill=FALSE),
    panel.background = element_rect(fill = "white"), 
    axis.title.y = element_text(size = 15), 
    axis.text = element_text(size=13))

# Save plot
ggsave('figures/bar_chart.tiff', 
       last_plot(), device = 'tiff', 
       width = 32, height = 12, units = 'cm', dpi = 300)




