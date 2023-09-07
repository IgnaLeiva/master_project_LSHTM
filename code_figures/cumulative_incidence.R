library(tidyverse)
library(survival)
library(survminer)
library(patchwork)

#################################################################
##                        Load the data                        ##
##                                                             ##
#################################################################

data <- ('the data')

#################################################################
##                        Kaplan-Meier                         ##
##                                                             ##
#################################################################


model.km.mace <- survfit(Surv((time_mace), mace) ~ exposure3, data = data)

#################################################################
##                        Cumulative plot                      ##
##                        and Modifications                    ##
#################################################################

# Cumulative plot
cum_plot.mace.original <- ggsurvplot(model.km.mace,
                                     fun = 'cumhaz',
                                     legend.labs = c('SU', 'DPP4i', 'SGLT2i'),
                                     xlab = 'Years from prescription',
                                     ylab = 'Cumulative Incidence',
                                     censor = F,
                                     fontsize = 4.5,
                                     risk.table = T,pval = T) + 
  theme_survminer(font.tickslab = c(12, "plain", "black"),
                  font.y = c(14, "plain", "black"),
                  font.x = c(12, "plain", "black")) 


# Get the ggplot form to modify axis and colors
cum_plot_mace_ggplot <- cum_plot.mace.original$plot +
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) + scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = c('#471164FF', '#287C8EFF', '#D8B905'))


#################################################################
##                        Sub-plot (zooming)                   ##
##                                                             ##
#################################################################

sub_mace_plot <- (ggsurvplot(model.km.mace, 
                             fun = 'cumhaz',censor = F) +
                    theme_survminer(font.tickslab = c(12, "plain", "black"),
                                    font.y = c(14, "plain", "black"),
                                    font.x = c(12, "plain", "black")))$plot +
  scale_y_continuous(expand = c(0,0), limits = c(0, .12)) + scale_x_continuous(expand = c(0,0)) +
  theme(legend.position = 'none',
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size =10),
        axis.title.x = element_blank(),
        panel.grid.major.y = element_line()) +
  scale_color_manual(values = c('#471164FF', '#287C8EFF', '#D8B905'))


#################################################################
##                     Add the Zoomed plot                       ##
##              to original cumulative plot                    ##
#################################################################

# data frame for labels located inside the plot (at each curve)
label_data <- data.frame(x = c(5.45,5.7,5.6),
                         y = c(0.88,0.75,0.54),
                         label = c('SU','DPP4i','SGLT2i'))


# Add the zoomed plot and labels into the plot
final.mace.plot <- (cum_plot_mace_ggplot + 
                      theme(legend.position = 'none')) + 
  annotation_custom(ggplotGrob(sub_mace_plot), xmin = 1, xmax = 5.8, ymin = 0.26, ymax = 0.98) +
  geom_label(data = label_data, aes(x,y, label =label),fill = 'white', size = 4, stat = 'unique')



#################################################################
##                      Table with Number                      ##
##                         at risk                             ##
#################################################################

# Get data from the original table made using ggsurvplot. 
# Check to what exposure the y-axis represents to relevel (create a new variable exposure)
tbl.dta <- ggplot_build(cum_plot.mace.original$table)
tbl.dta <- tbl.dta[["data"]][[1]] |> mutate(exposure = case_when(y == 1 ~ 'SGLT2i',
                                                  y == 2 ~ 'DPP4i',
                                                  y == 3 ~ 'SU'),
                                            exposure = factor(exposure, levels = c('SGLT2i', 'DPP4i', 'SU')))
# Plotting the table
tbl <- tbl.dta |>
  ggplot(aes(x,y = exposure)) + 
  geom_text(label = tbl.dta$label, size = 4.5) +
  scale_x_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(0,6), clip = 'off') +   # 'clip off' helps to set text outside of the plot
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(l=50, r = 20),
        panel.background = element_rect(fill = 'white'),
        legend.position = 'none'
        ) + 
  geom_text(aes(x = -0.9,label = exposure, color = exposure), size = 4.5, hjust = 0, fontface = 'bold') +
  scale_color_manual(values = c('#D8B905', '#287C8EFF', '#471164FF'))+
  annotate('text', x=-.9, y =4, label = 'No at risk', hjust = 0, size = 3.8, fontface = 'bold.italic')




#################################################################
##                    FINAL PLOT                               ##
##       Cumulative plots plus table                           ##
#################################################################

cumulative_incidence <-  final.mace.plot / tbl +plot_layout(heights = c(6,1))

ggsave(paste0("output/cumulative_incidence.tiff"), cumulative_incidence, width = 10, height = 7)
                                    