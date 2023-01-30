#loading in neccesary packages#
require(tidyverse)
require(plyr)
require(ggplot2)

# Import data set #
sublit <- read.csv("input/GC_sublit_data.csv")


#Organizing data#
barchart <- sublit %>% 
  #Filtering for usable papers#
  filter(usable=="y") %>%
  #summarizing number of papers under each conditions (4 in total)#
  ddply(.(`Measured.fitness`, `Fitness.implications.of.GCs`), summarise, value=sum(value)) %>%
  #combining columns: 'Measured fitness' and 'fitness implications of GC's' for creating plot#
  unite(., FM_and_I, c(`Measured.fitness`, `Fitness.implications.of.GCs`), sep='&') %>%
  
  ddply(.(FM_and_I), summarise, ratio=value/35, percent=round((value/35)*100, digits = 0)) %>%
 mutate(perc=case_when(percent>0 ~ "%")) %>%
unite(., percent, c('percent', 'perc'), sep="") %>%
  #plot for barchart#
  ggplot(aes(x=factor(FM_and_I, levels=c("n&y", "y&y", "y&n", "n&n"), labels = c("No fitness\nPH inference", "Fitness\nPH inference", "Fitness\nno PH inference", "No fitness\nno PH inference")), y=ratio*100, fill=FM_and_I)) +
  geom_col(color="black") +
  geom_text(aes(label = percent), colour="black",  
            position = position_stack(vjust = 0.5), size = 5) +
  scale_fill_brewer(palette = "PuBu", guide=NULL, direction = -1) +
  labs(x=NULL, y="Percentage of papers (%)") +
  scale_y_continuous(limits=c(0,60), breaks=seq(0, 60, 15)) +
  theme(legend.position = 'none',
        axis.title.x = element_text(size = 18, vjust = -3),
        axis.title.y = element_text(size = 18, vjust = 4),
        axis.text = element_text(size = 18),
        panel.background = element_rect(colour = 'black', fill = 'white'),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        panel.grid = element_blank())

# Save plot
ggsave(filename='bar_chart.tiff', path = "Desktop/gcfigures", 
       barchart, device = 'tiff', 
       width = 32, height = 12, units = 'cm', dpi = 300)




