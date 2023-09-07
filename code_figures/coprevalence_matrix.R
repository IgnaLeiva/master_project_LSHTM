library(tidyverse)
library(ggcorrplot)

data <- 'read or load the data'


#################################################################
##                   Matrix co-prevalence                      ##
##                         2015 to 2020                        ##
#################################################################

# rename long term conditions
matrix_dat <- data |>
  rename(Obesity = obesity,
         `IHD` = co_ihd,
         `Cancer (any)` = co_cancer,
         `COPD`= co_copd,
         `Reduced RF` = ckd_stage_3ab,
         `MI` = co_mi,
         `AF`= co_afib,
         `HF` = co_hf,
         Stroke = co_stroke,
         Asthma = co_asthma,
         `Neurological`= co_neuro,
         `UA` = co_ua,
         `SMI`  = co_smi,
         `PAD` = co_pad,
         Epilepsy = co_epilepsy,
         `RA` = co_rharthritis,
         Blindness = co_blind,
         `VHD` = co_valvular,
         `IBS` = co_ibs,
         `Liver disease` = co_liver,
         Amputation = co_amputation,
         Hypoglycaemia = co_hypoglycaemia,
         HIV = co_hiv,
         Depression = co_depression,
         Hypertension = co_hypertension,
         Thyroid = co_thyroid)

# Make the matrix
co_preval_matrix <- matrix(NA, nrow = ncol(data), ncol = ncol(data) )


# For loop to calculate coprevalences
for(i in 1:ncol(matrix_dat)){
  for(j in 1:ncol(matrix_dat)){
    co_preval_matrix[i,j] <- round(sum(matrix_dat[,i] & matrix_dat[,j],na.rm = T)/nrow(matrix_dat), digits = 2)
  }
}


# assign name to columns and rows
colnames(co_preval_matrix) <- colnames(matrix_dat)
rownames(co_preval_matrix) <- colnames(matrix_dat)



# Plor the matrix using ggcorrplot to only obtain the lower part of the matrix
plot <- ggcorrplot(co_preval_matrix, type = 'lower')

# As ggcorrplotis not that pretty I decided to take the coordinates and used ggplot

final_plot <- plot[["data"]] |>
  ggplot(aes(Var1, Var2, fill = value)) +
  geom_tile() +
  labs(x = '',
       y = '',
  ) +
  scale_fill_viridis(discrete = F, direction = -1, alpha = .4, name = 'Co-prevalence', na.value = 'white', option = 'C') +
  scale_y_discrete(position = 'right') +
  
  geom_text(aes(label = value), size = 3) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(color = 'lightyellow', fill = 'white'),
    axis.line = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = .95, vjust = .2, size = 10),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9, face = 'bold'),
    legend.position = c(0.08,0.93),
    legend.direction = 'horizontal'
  ) +
  guides(fill
         = guide_colorbar(title.position = 'top',title.hjust = 0.5))

ggsave(paste0( "outputs/matrix.tiff"), final_plot, width = 10, height = 7)
