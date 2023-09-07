
library(tidyverse)
setwd('set your working directory')

# Read files with the result of the regressions
file <- list.files(full.names = T, pattern = '.csv')
file <- list.files('data_regressions/new',full.names = T, pattern = '.csv')
filenames <- stringr::str_to_lower(stringr::str_remove(tools::file_path_sans_ext(basename(file)), '_regression'))
data <- lapply(file, readr::read_csv)
names(data) <- filenames

dta <- dplyr::bind_rows(data, .id = 'Source')

#Create a new variable to that contain the HRs (95%CI) (character variable) and rename some variables
dta <- dta |> mutate(estimate_lab = paste0(sprintf( '%.2f',(round(estimate,2))), ' (',sprintf( '%.2f',(round(conf.low,2))), '-', sprintf( '%.2f',(round(conf.high,2))), ')'),
                     include = ifelse(conf.high >= 1.6, TRUE, FALSE),
                     Outcome = stringr::str_remove(Source, 'iv_|cox_'),
                     Outcome =case_when(
                       Outcome == 'mace' ~ 'MACE',
                       Outcome == 'mi' ~ 'MI',
                       Outcome == 'stroke' ~ 'Stroke',
                       Outcome == 'cvd' ~ 'CVD'
                     ))

# Rename values
dta$term[str_detect(dta$term, '.*exposure3DPP*.')] <- 'DPP4i vs SU'
dta$term[str_detect(dta$term, '.*exposure3SGLT*.')] <- 'SGLT2i vs SU'
dta$term <- factor(dta$term, levels = c("DPP4i vs SU","SGLT2i vs SU","SGLT2i vs DPP4i"))
dta$model[dta$model =='NO MLTC'] <- 'Without MLTC-M'
dta$model[dta$model =='YES MLTC'] <- 'With MLTC-M'
dta$model[dta$model =='NO CV'] <- 'Without Cardiovascular D.'
dta$model[dta$model =='YES CV'] <- 'With Cardiovascular D.'
dta$model[dta$model =='Adjusted'] <- 'Overall'

# Specify the data 
data <- dta |> filter(mltc == 'Dichotomised' & (method == '2SRI' | method == 'cox')& Outcome == 'MACE' & (model == 'Without MLTC-M' | model == 'With MLTC-M' | model == 'Overall')) |>
  mutate(model = factor(model, levels = c('Overall', 'With MLTC-M', 'Without MLTC-M')),
         # this to limit the wide of the CI
  conf.high2 = ifelse(conf.high >= 1.6 , 1.6, conf.high),
  method = ifelse(method == 'cox', 'Cox', 'IV')
  ) 

plot_data <- data |>
  ggplot(aes(y = fct_rev(term),x = estimate, group = method, color = method), na.rm = TRUE) +
  geom_vline(xintercept = 1, linetype = 'dashed',linewidth = 0.3 ) +
  geom_point(shape = 19,size = 3, 
             position = position_dodge(.5), 
             show.legend = T) +
  geom_errorbar(aes(xmin= conf.low, xmax = conf.high2, group = method),
                width =.25,
                size = 0.8,
                position = position_dodge(.5), 
                show.legend = T) +
  
  # Values of each HR and CI located on the right of the plot
  geom_label(aes(y = fct_rev(term), x = 2.1, label = estimate_lab, group = method),
             size = 4, fill = 'white',
             label.size = NA,
             #hjust = 0,
             position = position_dodge(.5),
             show.legend = F,
             inherit.aes = F) +
  
  labs(x = '\n\nHazard Ratio (95% Confidence Interval)',
       color = 'Model')+
  
  # Zooming the axis
  coord_cartesian(ylim = c(1,3), xlim=c(0.005,2.5), clip="off") +
  facet_wrap(~ model) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y =  element_text(size = 10), 
        axis.text.x =  element_text(size = 10), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill= '#471164FF'), #'#471164FF'
        strip.text = element_text(colour = 'white', face = 'bold', size = 12.5),
        legend.position = 'right',
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 11, face = 'bold')#,
       # legend.title.align = 0.5
  ) +
  scale_color_manual(values = c(#'#E56B6F',
                                #'#3D9B9D',
                                '#38A3A5',
                                '#6C3483')) +
  # Label for HRs and CI
  geom_text(x = 2.1, y = 3.35, label =  'HR (95% CI)', fontface='bold', size = 4, show.legend = F, color = 'Black') +
  guides(color = guide_legend(reverse = T)) +
  
  # Segment to construct arrows under the x-axis
  geom_segment(aes(x = 1.05, xend = 1.7 , y =0.15, yend =0.15), inherit.aes = T,
               #position = position_dodge(.5),
               show.legend = F,
               #data = arrows,
               color = 'black',
               arrow =arrow(type = 'closed', length = unit(1.8, 'mm')),
               linewidth = 0.11) +
  geom_segment(aes(x = 0.95, xend = 0.3 , y =0.15, yend =0.15), inherit.aes = T,
               #position = position_dodge(.5),
               show.legend = F,
               #data = arrows,
               color = 'black',
               arrow =arrow(type = 'closed', length = unit(1.8, 'mm')),
               linewidth = 0.11) +
  
  # Text under the arrows
  geom_text(x = 1, y = 0.06, label =  'Favours A          Favours B', size = 3,
            show.legend = F, color ='black') 



#############################################################
# The following code is for obtaining the exact coordinates #
# of the plot to position a segment with an arrows          #
#############################################################

# To get the y points
dt <- ggplot_build(plot_data) 

# Extract from the list called "data",  the dataframe number  4. It contains coordinates
dt <- dt[["data"]][[4]]

# Extract all Y-coordinates  which meet that criteria. x and xend are set visually.
arrows <- data.frame(x = 1, 
                     xend = 1.6,
                     y =dt$y[dt$label %in% data$estimate_lab[data$conf.high>= 1.6]],
                     yend =dt$y[dt$label %in% data$estimate_lab[data$conf.high>= 1.6]],
                     estimate_lab= dt$label[dt$label %in% data$estimate_lab[data$conf.high>= 1.6]]) |>
  left_join(data, by = "estimate_lab") |> 
  mutate(row= row_number(),
         label = c('Comparison\n(A vs B)') )

# Add the segment to the plot
plot_data_final <- plot_data +   geom_segment(aes(x = x, xend = xend, y =y, yend =yend, color = method), inherit.aes = T,
                                                   #position = position_dodge(.5),
                                                   show.legend = F,
                                                   data = arrows,
                                                   arrow =arrow(type = 'closed', length = unit(3.1, 'mm')),
                                                   linewidth = 0.11)  #0.01


### Create data frame for the text "comparison A vs B" on the y axis
dat_text <- data.frame(
  label = c('Comparison\n(A vs B)', '', ''),
  method =c( 'cox', 'cox', 'cox'),
  model   = c('Overall', 'Without MLTC-M', 'With MLTC-M')
)

# Add the text to the plot.
plot_data_final0 <- plot_data_final + geom_text(
  data= dat_text,
  aes(x = -0.57, y = 3.5, label = label),
  size = 4, fontface = 'bold',
  color = 'black'
) 

## Note: it is best to save the graph for viewing. When the chart is opened here using the zoom command it looks unconfigured. 
ggsave("outputs/forestplot/plot_data_final2.tiff", plot_data_final0, width = 12, height = 5)

