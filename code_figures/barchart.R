library(tidyverse)

data <- 'read or load the data'


#################################################################
##                   Bar chat comorbidities                    ##
##                         2015 to 2020                        ##
#################################################################

# Rename variables
barchat_dat <- data |>
  rename(Obesity = obesity,
         `Ischemic heart disease` = co_ihd,
         `Cancer (any)` = co_cancer,
         `Chronic obstructive pulmonary disease`= co_copd,
         `Reduced renal function` = ckd_stage_3ab,
         `Myocardial infarction` = co_mi,
         ` Atrial fibrillation`= co_afib,
         `Heart failure` = co_hf,
         Stroke = co_stroke,
         Asthma = co_asthma,
         `Neurological disorders`= co_neuro,
         `Unstable angina` = co_ua,
         `Severe mental illness`  = co_smi,
         `Peripheral artery disease` = co_pad,
         Epilepsy = co_epilepsy,
         `Rheumatoid arthritis` = co_rharthritis,
         Blindness = co_blind,
         `Valvular heart disease` = co_valvular,
         `Irritable bowel syndrome` = co_ibs,
         `Liver disease` = co_liver,
         Amputation = co_amputation,
         Hypoglycaemia = co_hypoglycaemia,
         HIV = co_hiv,
         `Thryoid disease` = co_thyroid,
         Hypertension = co_hypertension,
         Depression = co_depression
  )

# Create a empty data frame to store outputs from the next for loop
df.bartchart <- data.frame(comorbid = NULL, MLTC = NULL)

# This is two calculate the number of patient by its number of LTC
for(i in 1:ncol(barchat_dat)){
  dt <- barchat_dat %>% 
    mutate(n = ifelse(barchat_dat[[i]]== 1, rowSums(.,na.rm = T)-1,rowSums(.,na.rm = T)))  |>
    filter(barchat_dat[[i]]== 1) # this take patients who have the disease
  y <- data.frame(comorbid= rep(colnames(dt[i]), nrow(dt)), MLTC=dt$n)
  df.bartchart <- rbind(df.bartchart,y)
}

# Arrange the data
df.bartchart <- df.bartchart |> 
  mutate(`MLTC-M`= as.factor(ifelse(MLTC > 2, "3+", MLTC)),
         comorbid = as.factor(comorbid)) |>
  group_by(comorbid,`MLTC-M`) |> 
  summarise(n=n() ,.groups = 'drop') 

# The next code is to handle the order of the LTC

# 1. create empty variable in the dataset
df.bartchart$rank <- NA

# Look for the rank of the 3+ to sort by this
ranking <- df.bartchart |> group_by(comorbid) |> mutate(prop = n/ sum(n)) |> filter(`MLTC-M`== '3+') |>
  arrange(desc(prop)) |> ungroup()|> mutate(rank = row_number())

for(i in 1:nrow(df.bartchart)){
  for(j in 1:nrow(ranking)){
    if(df.bartchart$comorbid[i] == ranking$comorbid[j]){
      df.bartchart$rank[i] <- ranking$rank[j]
    }
  }
}


# The actual plot
bartchart_plot <- ggplot(df.bartchart, aes(fill=`MLTC-M`, y=n, x=as.factor(rank))) + 
  geom_bar(position="fill", stat="identity",alpha=0.65) +
  scale_x_discrete(labels = ranking$comorbid) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  coord_flip() +
  scale_fill_viridis_d(direction = -1)+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_text(angle=0,hjust=0.5,vjust=0.5, size = 10),
    axis.ticks.y= element_blank(),
    
    # This is configuration for x axis on top
    axis.text.y = element_text(size = 11,hjust=0.99,vjust=0.5),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    plot.title = element_text(size = 12, face = 'bold'))+
  labs(#title = 'People with an specific long-term condition with MLTC',
    y= 'Proportion of people living with the long-term condition',
    x='',
    fill = 'Number of\nadditional LTC') 

ggsave(paste0("outputs/bartchart.tiff"), bartchart_plot, width = 10, height = 7)
