#devtools::install_github("thomasp85/ggforce")
extrafont::loadfonts(device="win")
library(readxl)
library(tidyverse)
#devtools::install_github("gadenbuie/ggpomological")
library(ggpomological)
library(ggforce)
library(ggalluvial)

data <- read_excel("data/jobs_by_field.xlsx", skip = 3)
colnames(data)[1] <- "job"

data_long <- 
  data %>%
  gather(key = var, value = percent, -job) %>%
  filter(var != "Total") %>%
  mutate(job_cat = ifelse(job == "Academia", "Academia", "Employment"),
         field = case_when(
           var == "Life sciences" ~ "STEM",
           var == "Mathematics and computer sciences" ~ "STEM",
           var == "Physical sciences and earth sciences" ~ "STEM",
           var == "Engineering" ~ "STEM",
           var == "Psychology and social sciences" ~ "non-STEM",
           var == "Education" ~ "non-STEM",
           var == "Humanities and arts" ~ "non-STEM",
           var == "Other" ~ "non-STEM"
         )
  )

data_summary <-
  data_long %>%
  group_by(job_cat, var) %>%
  mutate(sum = sum(percent)) %>%
  ungroup() %>%
  group_by(job_cat, field) %>%
  summarize(percent = mean(sum))
  

data_sex <- read_xlsx("data/sex_by_field.xlsx", skip = 3) %>%
  filter(field != "All fields") %>%
  select(field, sex, percent = "2017") %>%
  mutate(field = case_when(field == "Life sciences" ~ "STEM",
                           field == "Mathematics and computer sciences" ~ "STEM",
                           field == "Physical sciences and earth sciences" ~ "STEM",
                           field == "Engineering" ~ "STEM",
                           field == "Psychology and social sciences" ~ "non-STEM",
                           field == "Education" ~ "non-STEM",
                           field == "Humanities and arts" ~ "non-STEM",
                           field == "Other" ~ "non-STEM"
                          )) %>%
  group_by(field, sex) %>%
  summarize(percent = mean(percent)) 

sex_by_job <- read_excel("data/sex_by_job.xlsx") %>%
  gather(key = sex, value = percent, -job)
  
  
data_comb <- 
  data_sex %>%
  left_join(., sex_by_job, by = "sex") %>%
  #select(sex, field, job_cat, percent, percent_sex) %>%
  gather(key = type, value = value, -sex, -field, -job) %>%
  gather_set_data(1:3)

data_comb$x <- fct_relevel(data_comb$x, "sex", "field", "job")

ggplot(data_comb, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(aes(fill = sex), alpha = 0.3, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'white')

data1 <- reshape2::melt(Titanic)
head(data1)
data1 <- gather_set_data(data1, 1:4)
head(data1)

data$x <- fct_relevel(data$x, "Age", "Class", "Survived", "Sex")

ggplot(data, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Sex), alpha = 0.3, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'white')


data(vaccinations)
levels(vaccinations$response) <- rev(levels(vaccinations$response))
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           y = freq,
           fill = response, label = response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("vaccination survey responses at three points in time")

