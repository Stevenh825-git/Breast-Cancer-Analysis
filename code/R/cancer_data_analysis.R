install.packages('tidyverse')
install.packages('janitor')
install.packages('ggpubr')
library(tidyverse)
tidyverse_update()
library(janitor)
library(ggpubr)

cancer_data <- read_csv('Breast_Cancer_Data.csv')

#Look at data
head(cancer_data,6)
colnames(cancer_data)

str(cancer_data)

# Ensure data names contain valid characters
cancer_data <- clean_names(cancer_data)

temp <- head(cancer_data,20)

# See the average expression level for 4 proteins in  the data set
protein1_mean <- c(mean(cancer_data$protein1))
protein2_mean <- c(mean(cancer_data$protein2))
protein3_mean <- c(mean(cancer_data$protein3))
protein4_mean <- c(mean(cancer_data$protein4))

protein_means <- data.frame(protein1_mean,protein2_mean,protein3_mean,protein4_mean)

# Make the genders be lowercase
cancer_data <- cancer_data %>% 
  mutate(gender = tolower(gender))


glimpse(cancer_data)

View(head(cancer_data,30))


# Created a data frame containing stats on the whole data set
#       Includes: proteins 1 - 4 means; survival rate; amount of patients with unknown status; 
#                 percent of cancer stages
Overall_stats <- data.frame(surgery_year = 'Overall', count = length(cancer_data$age), protein1_mean = mean(cancer_data$protein1),  protein2_mean = mean(cancer_data$protein2),
                   protein3_mean = mean(cancer_data$protein3),  protein4_mean = mean(cancer_data$protein4),
                   survival_rate = length(which(cancer_data$patient_status == 'Alive')) / 
                                   length(cancer_data$patient_status)*100,
                   unknown_status = length(which(cancer_data$patient_status == "Unknown")),
                   stage1_percent  = length(which(cancer_data$tumour_stage == 'I'))/ length(cancer_data$tumour_stage)*100,
                   stage2_percent  = length(which(cancer_data$tumour_stage == 'II'))/ length(cancer_data$tumour_stage)*100,
                   stage3_percent  = length(which(cancer_data$tumour_stage == 'III'))/ length(cancer_data$tumour_stage)*100)

# Created a data frame containing stats by year
#       Includes: proteins 1 - 4 means; survival rate; amount of patients with unknown status; 
#                 percent of cancer stages

year_stats <- cancer_data %>% 
  group_by(surgery_year) %>% 
  summarize(count = n(), protein1_mean = mean(protein1),  protein2_mean = mean(protein2),
            protein3_mean = mean(protein3),  protein4_mean = mean(protein4),
            survival_rate = length(which(patient_status == 'Alive')) / 
              length(patient_status)*100,
            unknown_status = length(which(patient_status == "Unknown")),
            stage1_percent  = length(which(tumour_stage == 'I'))/ length(tumour_stage)*100,
            stage2_percent  = length(which(tumour_stage == 'II'))/ length(tumour_stage)*100,
            stage3_percent  = length(which(tumour_stage == 'III'))/ length(tumour_stage)*100)

# Combined the two data sets
yearly_and_overall <- rbind(year_stats,Overall_stats)

## Ideas: 
##  1. Create a graph for protein levels, and color by patient_status
##  2. Group by year and compare the amount of patients in the data, and color by status
##  3. Compare survival rate by surgery type 
##

######################################################################################



# PROTEIN PLOTS
###########################################
# protein 1 vs 2
ggplot(data = cancer_data,mapping= aes(x = protein1,y = protein2)) +
  geom_point(aes(color = patient_status)) +
  geom_smooth(method = lm, se = FALSE) +
  stat_cor(method = "pearson", label.x = -1, label.y = 3) +
  labs(title = "Expression Levels Between Protein 1 & Protein 2",
       x = "Protein 1",
       y = "Protein 2", 
       color = "Patient Status") +
  theme(plot.background = element_rect(fill = '#FBD7CD'),
        legend.background = element_rect(fill = "#FBD7CD"))

# protein 1 vs 3
ggplot(data = cancer_data,mapping = aes(x = protein1,y=protein3)) +
  geom_point(aes(color = patient_status)) +
  geom_smooth(method = lm, se = FALSE) +
  stat_cor(method = "pearson", label.x = -1, label.y = 3) +
  labs(title = "Expression Levels Between Protein 1 & Protein 3",
       x = "Protein 1",
       y = "Protein 3", 
       color = "Patient Status") +
  theme(plot.background = element_rect(fill = '#FBD7CD'),
        legend.background = element_rect(fill = "#FBD7CD"))

# protein 1 vs 4
ggplot(data = cancer_data, mapping = aes(x = protein1,y=protein4)) +
  geom_point(aes(color = patient_status)) +
  geom_smooth(method = lm, se = FALSE) +
  stat_cor(method = "pearson", label.x = -1, label.y = 3) +
  labs(title = "Expression Levels Between Protein 1 & Protein 4",
       x = "Protein 1",
       y = "Protein 4", 
       color = "Patient Status") +
  theme(plot.background = element_rect(fill = '#FBD7CD'),
        legend.background = element_rect(fill = "#FBD7CD"))

# protein 2 vs 3
ggplot(data = cancer_data, mapping = aes(x = protein2,y=protein3)) +  
  geom_point(aes(color = patient_status)) +
  geom_smooth(method = lm, se = FALSE) +
  stat_cor(method = "pearson", label.x = -1, label.y = 3) +
  labs(title = "Expression Levels Between Protein 2 & Protein 3",
       x = "Protein 2",
       y = "Protein 3", 
       color = "Patient Status") +
  theme(plot.background = element_rect(fill = '#FBD7CD'),
        legend.background = element_rect(fill = "#FBD7CD"))

# protein 2 vs 4
ggplot(data = cancer_data, mapping = aes(x = protein2,y=protein4)) +
  geom_point(aes(color = patient_status)) +
  geom_smooth(method = lm, se = FALSE) +
  stat_cor(method = "pearson", label.x = -1, label.y = 3) +
  labs(title = "Expression Levels Between Protein 2 & Protein 4",
       x = "Protein 2",
       y = "Protein 4", 
       color = "Patient Status") +
  theme(plot.background = element_rect(fill = '#FBD7CD'),
        legend.background = element_rect(fill = "#FBD7CD"))

# protein 3 vs 4
ggplot(data = cancer_data, mapping = aes(x = protein3,y=protein4)) +
  geom_point(aes(color = patient_status)) +
  geom_smooth(method = lm, se = FALSE) +
  stat_cor(method = "pearson", label.x = -1, label.y = 3) +
  labs(title = "Expression Levels Between Protein 3 & Protein 4",
       x = "Protein 3",
       y = "Protein 4", 
       color = "Patient Status") +
  theme(plot.background = element_rect(fill = '#FBD7CD'),
        legend.background = element_rect(fill = "#FBD7CD"))


#########################################






# Grouped  bar chart showing the patient status for each surgery type divided by cancer stage
ggplot(data = cancer_data, aes(x = tumour_stage))+
  geom_bar(aes(fill = patient_status),position = 'dodge') + 
  geom_text(aes(label = after_stat(count),group = patient_status), stat ="count", 
            size = 3, position = position_dodge(0.8), vjust=-0.5) +
  facet_wrap(~surgery_type,scales = 'free')  + 
  ylim(0,45) +
  labs(title = "Surgeries Performed at Different Stages of Cancer",
       subtitle = "Split By Patient Survival",
       x = "Tumor Stage",
       y = "Amount of Patients", 
       fill = "Patient Status") +
  theme(plot.background = element_rect(fill = '#FBD7CD'),
        legend.background = element_rect(fill = "#FBD7CD"))


#Create a histogram with the x axis = years , y axis = count , color = tumor_stage, second bar color = patient_status

cancer_data %>% 
  filter(patient_status != 'Unknown') %>% 
  ggplot() +
    geom_histogram(aes(x = age, fill = tumour_stage),position = position_dodge(4),
                    binwidth = 5) +
    labs(title = "Age Range of Patients",
         subtitle = "Split By Tumor Stage",
         x = "Age",
         y = "Amount of Patients", 
         fill = "Tumor Stage",
         caption = "Bin Range of 5") +
    theme(plot.background = element_rect(fill = '#FBD7CD'),
          legend.background = element_rect(fill = "#FBD7CD"),
          plot.caption.position = 'plot') +
    facet_wrap(~patient_status)


# Heat map
cancer_data %>% 
  group_by(surgery_year,patient_status) %>% 
  summarize(count = n()) %>% 
  ggplot(mapping =aes(x = surgery_year, y = patient_status,fill = count)) +
    geom_tile(color = 'black', lwd = 1.5,) +
    geom_text(aes(label = count), color = 'black',size = 6) +
    coord_fixed() +
  scale_fill_distiller(palette = 'OrRd',direction = +1) +
  labs(title = "Current Patient Survival Split By Year",
       y = "Patient Status",
       x = "Year of Surgery", 
       fill = "Number of Patients",
       caption = 'Some patients whereabouts are unknown') +
  theme(plot.background = element_rect(fill = '#FBD7CD'),
        panel.background = element_rect(fill = '#FBD7CD'),
        legend.background = element_rect(fill = "#FBD7CD"),
        plot.caption.position = 'plot')



