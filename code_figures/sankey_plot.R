library(ggsankey)
library(tidyverse)


df.ltc <- 'data frame with indiviudals an LTC, one row per indivual and one column per LTC'
comorbidities_names <-  'data frame with the name of all included LTC'

# Calculate prevalence of LTC by age group

preval_age <- df.ltc |>
  select(co_age_cat4, comorbidities_names$variable) |>
  group_by(co_age_cat4) |> 
  mutate(total_group = n()) |> ungroup()|>
  pivot_longer(
    cols = 2:27,
    names_to = 'Long-term condition',
    values_to = 'do_have'
  ) |>
  filter(do_have == 1) |>
  group_by(co_age_cat4, `Long-term condition` ) |>
  mutate(number = sum(do_have,na.rm = T)) |> 
  distinct()|>
  ungroup() |>
  mutate(prevalence = number/total_group) |>
  select(co_age_cat4, 'Long-term condition', prevalence ) |>
pivot_wider(
  names_from = co_age_cat4,
  values_from = prevalence
 
) 

# NOTE: if there are prevalences with the same number, the code will merge them, evaluate how many decimals can be used 
#.      to avoid the problem


# Assign a rank to each LTC at each age group

preval_age <- preval_age|>
  mutate(`18-49 yrs` = rank(-`18-49 yrs`),
         `50-59 yrs` = rank(-`50-59 yrs`),
         `60-69 yrs` = rank(-`60-69 yrs`),
         `70+ yrs` = rank(-`70+ yrs`))|>
  arrange((`18-49 yrs`))  |>
  select('Long-term condition','18-49 yrs', "50-59 yrs" ,"60-69 yrs" ,"70+ yrs")


# Make longer
long_table_age <- preval_age |>
  make_long(everything()) |>
  mutate(labels = node)

# This for loop replace the name of the LTC with the rank of the LTC rank for the `18-49 yrs`.
# This is to get a horizontal direct join between the label and the rank point of each LTC at `18-49 yrs` 

for(i in 1:nrow(preval_age)){
  for(j in 1:nrow(long_table_age)){
    if(long_table_age$node[j] == preval_age$`Long-term condition`[i]){
      long_table_age$node[long_table_age$node == long_table_age$node[j]] <- preval_age$`18-49 yrs`[i]
      long_table_age$labels[long_table_age$labels == long_table_age$labels[j]] <- NA
    }
  }
}

# Set colors
color_flu <- c()
for(i in 1:length(comorbidities_names$variable)){
  color <- rep(i,5)
  color_flu <- c(color_flu, color)
}

# Making the Sankey graph
plot_final <- long_table_age |>
  mutate(node = factor(node, levels = c(length(comorbidities_names$variable):1)),
         next_node = factor(next_node, levels = c(length(comorbidities_names$variable):1))) |>
  ggplot(aes(x = x,
             next_x = next_x,
             node = node,
             next_node = next_node,
             fill = factor(color_flu), 
             label = labels)) +
  geom_sankey(flow.alpha = .5,
              node.color = "white") +
  geom_sankey_label(size = 3.5, color = "gray30", fill = "white") +
  scale_fill_viridis_d(direction = -1,option = 'H') +
  #scale_fill_viridis(discrete = T) +
  theme_sankey(base_size = 18) +
  labs(x = 'Age Groups',
       y= "") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12,face = 'bold', hjust = 0),
        axis.text.x = element_text(angle=0,hjust=0.5,vjust=0.5, size = 10),
        axis.title.x = element_text(size = 10))


# Obtain coordinates to set LTC labes

coord <- ggplot_build(plot_final)$data[[3]][1:length(comorbidities_names$variable),] 

# Labels

label_cb <- preval_age |> mutate(name = NA)

# Replace the LTC abbreviation with the long name
for(i in 1:length(label_cb$`Long-term condition`)){
  for(j in 1:length(comorbidities_names$variable)){
    if(label_cb$`Long-term condition`[i] == comorbidities_names$variable[j]){
      label_cb$name[i] <- comorbidities_names$long_name[j]
      
    }
  }
}

# Make a data set to plot it
annotations <- data.frame(
  x = c(rep(1,length(comorbidities_names$variable))),
  y = rev(coord$y),
  label = label_cb$name,
  next_x = c(rep(NA,length(comorbidities_names$variable))),
  node = c(rep(NA,length(comorbidities_names$variable))),
  next_node = c(rep(NA,length(comorbidities_names$variable))),
  disease2 = c(rep(NA,length(comorbidities_names$variable))))
annotations[11,3] <- 'COPD'


# Final plot
plot_sankey_rank <-plot_final + 
  geom_label(data = annotations, aes(x,y, label =label),fill = 'white', size = 3.5, stat = 'unique')

# save plor
ggsave(paste0( "outputs/forestplot/sankey_rank.tiff"), plot_sankey_rank, width = 10, height = 7)
