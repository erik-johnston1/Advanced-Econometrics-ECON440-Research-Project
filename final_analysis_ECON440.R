
#Info on loading in data set from DDI (xml file)
# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

#set up
library(ipumsr)
library(tidyverse)
library(knitr)
library(kableExtra)

#loading in data
ddi <- read_ipums_ddi("/Users/erikjohnston/Desktop/school/ECON_440/researchpaper/usa_00003.xml")
data <- read_ipums_micro(ddi)

#cleaning
data_cleaned <-data %>% 
  mutate(MARST = if_else(MARST < 4, 1, 0)) %>% #may need to ask dr. kuehn abt this - "separated" = married status?
  select(-RACED & -VETSTATD & -VET01LTR & -FAMSIZE & -POVERTY) %>% 
  filter(INCTOT > 0 & INCTOT < 9999997) %>% 
  filter(FTOTINC < 9999997) %>% 
  filter(VETSTAT != 0 & VETSTAT != 9) %>% 
  mutate(VETSTAT = if_else(VETSTAT == 2, 1, 0)) %>% 
  filter(AGE > 17) %>% # change if ca enlist younger than 18 idk answer to that
  mutate(SEX = if_else(SEX == 1, 1, 0)) %>% #male = 0, female = 0
  mutate(RACE = if_else(RACE %in% c(1, 4, 5), 1, 0)) # included asian-americans in 1 because tend to make more than other minorities - I'm not sure this is the right thinking if we want to isolate effect of race? if not take out 4 and 5

          #exploring poverty rates - this says roughly 85% of sample is above pov line - I made a var IMPOV, 1 = below pov line, 0 = above pov line
          impoverished <- data %>%
            mutate(POVERTY = as.numeric(POVERTY)) %>% 
            filter(POVERTY > 100)
          length(impoverished$POVERTY)/length(data$POVERTY)
          
#analysis
          
        #kuehn code (see lec6.R for trimming instructions if necessary)
          logit_nsw <- glm(treat ~ age + agesq + agecube + educ + educsq + 
                             marr + nodegree + black + hisp + re74 + re75 + u74 +
                             u75 + interaction1, family = binomial(link = "logit"), 
                           data = nsw_dw_cpscontrol)
          
          nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
            mutate(pscore = logit_nsw$fitted.values)
          
          # mean pscore 
          pscore_control <- nsw_dw_cpscontrol %>% 
            filter(treat == 0) %>% 
            pull(pscore) %>% 
            mean()
          
          pscore_treated <- nsw_dw_cpscontrol %>% 
            filter(treat == 1) %>% 
            pull(pscore) %>% 
            mean()
          
          # histogram
          nsw_dw_cpscontrol %>% 
            filter(treat == 0) %>% 
            ggplot() +
            geom_histogram(aes(x = pscore))
          
          nsw_dw_cpscontrol %>% 
            filter(treat == 1) %>% 
            ggplot() +
            geom_histogram(aes(x = pscore))
          
          #Inverse Probability Weight Estimator
          N <- nrow(nsw_dw_cpscontrol)
          #- Manual with normalized weights using all data
          nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
            mutate(d1 = treat/pscore,
                   d0 = (1-treat)/(1-pscore))
          
          s1 <- sum(nsw_dw_cpscontrol$d1)
          s0 <- sum(nsw_dw_cpscontrol$d0)
          
          nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
            mutate(y1 = (treat*re78/pscore)/(s1/N),
                   y0 = ((1-treat)*re78/(1-pscore))/(s0/N),
                   norm = y1 - y0)
          nsw_dw_cpscontrol %>% 
            pull(ht) %>% 
            mean()
          
          nsw_dw_cpscontrol %>% 
            pull(norm) %>% 
            mean()
          
#our analysis shell
          
# Dimensions of the dataset used for pscore
print(dim(data_cleaned))
          
#Calculates propensity score of being a veteran on the following covariates
logit_vet <- glm(VETSTAT ~ SEX + AGE + MARST + RACE + FTOTINC, family = binomial(link = "logit"), 
                      data = data_cleaned)

summary(logit_vet)

#Adding propensity scores (probabilities) to the dataset under column name, pscore
data_cleaned <- data_cleaned %>% 
  mutate(pscore = logit_vet$fitted.values)

#Checking that the p score was actually added to the dataset
colnames(data_cleaned)

#Checking the lengths are the same
nrow(data_cleaned)
length(logit_vet$fitted.values)

#Calculates the mean propensity scores separately for non-veterans and veterans
pscore_control <- data_cleaned %>% 
  filter(VETSTAT == 0) %>% 
  pull(pscore) %>% 
  mean()

pscore_treated <- data_cleaned %>% 
  filter(VETSTAT == 1) %>% 
  pull(pscore) %>% 
  mean()
 
#Plots histogram of propensity scores for nonveterans        
data_cleaned %>% 
  filter(VETSTAT == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore), fill = "#00bfc4") +
  labs(title = "P-Score for Non-Veterans", x = "Score", y = "Number of Non-Veterans") + 
  theme_minimal()
    #looks like we'll need to trim - I think?

#Plots histogram of propensity scores for veterans
data_cleaned %>% 
  filter(VETSTAT == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore), fill = "#F8766D") +
  labs(title = "P-Score for Veterans", x = "Score", y = "Number of Veterans") + 
  theme_minimal()

#Checking for overlap between veterans vs nonveterans
data_cleaned %>% 
  ggplot(aes(x = pscore, fill = as.factor(VETSTAT))) +
  geom_density(alpha = 0.5) + 
  labs(
    title = "Overlap of Propensity Scores for Veterans and Nonveterans",
    x = "Propensity Score",
    fill = "Veteran Status"
  ) +
  scale_fill_manual(
    values = c("0" = "#00bfc4", "1" = "#F8766D"), 
    labels = c("Nonveterans", "Veterans")
  )

#Following code is to see the mean effect without weighting or trimming

#Calculate mean of INCTOT for veterans (VETSTAT = 1) and non-veterans (VETSTAT = 0)
mean_veterans <- data_cleaned %>%
  filter(VETSTAT == 1) %>%
  summarise(mean_income = mean(INCTOT, na.rm = TRUE)) %>%
  pull(mean_income)

mean_nonveterans <- data_cleaned %>%
  filter(VETSTAT == 0) %>%
  summarise(mean_income = mean(INCTOT, na.rm = TRUE)) %>%
  pull(mean_income)

#Calculate the mean effect (difference between veterans and non-veterans)
mean_effect_no_weighting <- mean_veterans - mean_nonveterans

mean_effect_no_weighting
    #7371.478
    #this means that those who are veterans earn $7371.48 more than non-veterans

N <- nrow(data_cleaned)

#Following code is to see the mean effect with weighting and no trimming

#Calculate weights for treated (d1) and control (d0) groups using inverse propensity score weighting
data_cleaned <- data_cleaned %>%  
  mutate(d1 = VETSTAT / pscore,  # Weight for treated group
         d0 = (1 - VETSTAT) / (1 - pscore))  # Weight for control group

#Sum of weights for each group
s1 <- sum(data_cleaned$d1)
s0 <- sum(data_cleaned$d0)

# Estimate the potential outcomes for each individual
data_cleaned <- data_cleaned %>%  
  mutate(y1 = (VETSTAT * INCTOT / pscore) / (s1/N),  # Weighted potential outcome for treated group
         y0 = ((1 - VETSTAT) * INCTOT / (1 - pscore)) / (s0/N))  # Weighted potential outcome for control group

# Compute the norm (difference between potential outcomes)
data_cleaned <- data_cleaned %>% 
  mutate(norm = y1 - y0)

#Pull the mean of the norm to get the ATE
mean_effect <- data_cleaned %>% 
  pull(norm) %>% 
  mean()

mean_effect
#4675.732 

#Following code is to trim after weighting

#Set threshold for trimming (e.g., 0.05 and 0.95)
lower_threshold <- 0.05
upper_threshold <- 0.95

#Trim the data by keeping only the observations within the common support range
data_trimmed <- data_cleaned %>%
  filter(pscore >= lower_threshold & pscore <= upper_threshold)

#Sum of weights for treated and control groups
s1_trimmed <- sum(data_trimmed$d1)
s0_trimmed <- sum(data_trimmed$d0)

#Estimate potential outcomes for each individual in the trimmed dataset
data_trimmed <- data_trimmed %>%
  mutate(y1 = (VETSTAT * INCTOT / pscore) / (s1_trimmed / nrow(data_trimmed)),
         y0 = ((1 - VETSTAT) * INCTOT / (1 - pscore)) / (s0_trimmed / nrow(data_trimmed)),
         norm = y1 - y0)

#Calculate the mean of the treatment effect (norm)
mean_effect_trimmed <- mean(data_trimmed$norm, na.rm = TRUE)

#Output the mean treatment effect
mean_effect_trimmed
#1788.898 with thresholds from 0.05 to 0.95

#should we add tests to see if this is statistically significant? 
#bootstrapping - random samples based on VETSTAT, n = 100

set.seed(440)

for (i in 1:100) {
  subsample <- data_cleaned %>%
    group_by(VETSTAT) %>%
    sample_frac(0.01) %>%
    ungroup()
  
  write.csv(subsample, file.path("/Users/erikjohnston/Desktop/ECON_440/researchpaper/subsamples", paste0("subsample_", i, ".csv")), row.names = FALSE)
}

#Data summary to include in paper

#Mean Inc by VETSTAT
data_cleaned %>% 
  group_by(VETSTAT) %>% 
  summarize(mean_inc = mean(INCTOT)) %>% 
  ggplot(aes(x = factor(VETSTAT), y = mean_inc, fill = factor(VETSTAT))) + 
  geom_bar(stat = "identity") + 
  labs(title = "Mean Income by Veteran Status",
       y = "Mean Income",
       x = "Veteran Status",
       fill = "Veteran Status") +
  theme_minimal()

#VETSTAT by RACE
ggplot(data_cleaned, aes(x = factor(RACE), fill = factor(VETSTAT))) +
  geom_bar(position = "fill") +
  labs(x = "Race (1 = White/Asian, 0 = Other)", y = "Proportion",
       fill = "Veteran Status",
       title = "Proportion of Veteran Status by Race") +
  theme_minimal()

#VETSTAT by AGE
ggplot(data_cleaned, aes(x = AGE, fill = factor(VETSTAT))) +
  geom_histogram(binwidth = 5, position = "dodge", alpha = 0.7) +
  labs(x = "Age (years)", y = "Frequency", fill = "Veteran Status",
       title = "Age Distribution by Veteran Status") +
  theme_minimal()

#Family Income vs Personal Income - not used
ggplot(data_cleaned, aes(x = FTOTINC, y = INCTOT, color = factor(VETSTAT))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "Family Total Income", y = "Total Personal Income", color = "Veteran Status",
       title = "Relationship Between Personal and Family Income") +
  theme_minimal()

# Presenting the results table using knitr::kable - not used, results formatted in paper
results <- data.frame(
  Method = c("Simple Difference in Means", "Propensity Score Weighting", "Trimmed Propensity Score Model"),
  Treatment_Effect = c(mean_effect_noweightnotrim, mean_effect_weighted, mean_effect_trimmed)
)

kable(
  results,
  col.names = c("Method", "Estimated Treatment Effect"),
  caption = "Comparison of Treatment Effects Across Methods"
)