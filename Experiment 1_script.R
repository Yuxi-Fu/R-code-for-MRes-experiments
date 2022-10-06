library(tidyverse)
library(stats)
library(broom)
library(car)
library(afex)
library(rstatix)
library(emmeans)
library(ggpubr)
library(ggpmisc)
library(moments)
library(MASS)
library(performance)
library(Hmisc)

####data cleaning####
task_1 <- read_csv("task_1.csv")
head(task_1)

ques_1 <- read_csv("questionnaire_1.csv")
head(ques_1)


###### Filter lists######
data_t <- task_1[,c('Participant External Session ID', 'Screen Name', 
                    'Reaction Time', 'Response', 'ANSWER', 'Correct', 
                    'randomise_blocks', 'Order', 'Stimulus')]
data_q <- ques_1[,c('Participant External Session ID', 'Question Key', 
                    'Response')]

###### Rename######
data_t <- data_t %>%
  rename(ID = 'Participant External Session ID',
         session = 'Screen Name', 
         RT = 'Reaction Time', 
         block = 'randomise_blocks', 
         answer = 'ANSWER')

data_q <- data_q %>%
  rename(ID = 'Participant External Session ID',
         question = 'Question Key')

###### Demographics-demo######
#q.gender
gender <- data_q %>%
  filter(question == "gender")

gender <- gender %>%
  rename(gender = 'Response')

gender <- gender[,-grep("question", colnames(gender))]


#q.age
age <- data_q %>%
  filter(question == "birth.date-year")

age <- age %>%
  rename(age = 'Response')

age <- age[,-grep("question", colnames(age))]


#combine + check NA
demo <- left_join(age, gender, by = 'ID')

demo %>%
  is.na() %>%
  sum()

head(demo)


demo_summary <- demo %>%
  summarise(number = n(), mean_RT = mean(age), 
            sd_RT = sd(age), median_RT = median(age), 
            min_RT = min(age), max_RT = max(age))

######task######
head(data_t)
task <- data_t %>%
  filter(session == "Response")

#check NA
task %>%
  is.na() %>%
  sum()


#修改block为condition

w_task <- task %>%
  rename(length = 'block')

w_task <- w_task %>%
  mutate(length = recode(length,
                         "1" = "short",
                         "2" = "long",
                         "3" = "short",
                         "4" = "long",
                         "5" = "short",
                         "6" = "long",
                         "7" = "short",
                         "8" = "long"))

#“,”做“且”用，"|"做“或”用
w_same_task <- w_task %>%
  filter(Stimulus == "5_400F_2s_400UF.mp3"| 
           Stimulus == "14_400UF_2s_400F.mp3"|
           Stimulus == "23_400F_2s_400UF.mp3"|
           Stimulus == "32_400UF_2s_400F.mp3"|
           Stimulus == "41_800F_2s_800UF.mp3"|
           Stimulus == "50_800UF_2s_800F.mp3"|
           Stimulus == "59_800F_2s_800UF.mp3"|
           Stimulus == "68_800UF_2s_800F.mp3")

w_n_task <- w_task %>%
  filter(Stimulus != "5_400F_2s_400UF.mp3",
         Stimulus != "14_400UF_2s_400F.mp3",
         Stimulus != "23_400F_2s_400UF.mp3",
         Stimulus != "32_400UF_2s_400F.mp3",
         Stimulus != "41_800F_2s_800UF.mp3",
         Stimulus != "50_800UF_2s_800F.mp3",
         Stimulus != "59_800F_2s_800UF.mp3",
         Stimulus != "68_800UF_2s_800F.mp3")

# add difficulty
w_n_task <- w_n_task %>%
  mutate(Stimulus = recode(Stimulus,
                           "1_400F_2s_200UF.mp3" = "easy", "2_400F_2s_250UF.mp3" = "slightly easy",
                           "9_400F_2s_600UF.mp3" = "easy", "8_400F_2s_550UF.mp3" = "slightly easy",
                           "10_400UF_2s_200F.mp3" = "easy", "11_400UF_2s_250F.mp3" = "slightly easy",
                           "18_400UF_2s_600F.mp3" = "easy", "17_400UF_2s_550F.mp3" = "slightly easy",
                           "19_200F_2s_400UF.mp3" = "easy", "20_250F_2s_400UF.mp3" = "slightly easy",
                           "27_600F_2s_400UF.mp3" = "easy", "26_550F_2s_400UF.mp3" = "slightly easy",
                           "28_200UF_2s_400F.mp3" = "easy", "29_250UF_2s_400F.mp3" = "slightly easy",
                           "36_600UF_2s_400F.mp3" = "easy", "35_550UF_2s_400F.mp3" = "slightly easy",
                           "37_800F_2s_400UF.mp3" = "easy", "38_800F_2s_500UF.mp3" = "slightly easy",
                           "45_800F_2s_1200UF.mp3" = "easy", "44_800F_2s_1100UF.mp3" = "slightly easy",
                           "46_800UF_2s_400F.mp3" = "easy", "47_800UF_2s_500F.mp3" = "slightly easy",
                           "54_800UF_2s_1200F.mp3" = "easy", "53_800UF_2s_1100F.mp3" = "slightly easy",
                           "55_400F_2s_800UF.mp3" = "easy", "56_500F_2s_800UF.mp3" = "slightly easy",
                           "63_1200F_2s_800UF.mp3" = "easy", "62_1100F_2s_800UF.mp3" = "slightly easy",
                           "64_400UF_2s_800F.mp3" = "easy", "65_500UF_2s_800F.mp3" = "slightly easy",
                           "72_1200UF_2s_800F.mp3" = "easy", "71_1100UF_2s_800F.mp3" = "slightly easy",
                           "4_400F_2s_350UF.mp3" = "difficult", "3_400F_2s_300UF.mp3" = "slightly difficult",
                           "6_400F_2s_450UF.mp3" = "difficult", "7_400F_2s_500UF.mp3" = "slightly difficult",
                           "13_400UF_2s_350F.mp3" = "difficult", "12_400UF_2s_300F.mp3" = "slightly difficult",
                           "15_400UF_2s_450F.mp3" = "difficult", "16_400UF_2s_500F.mp3" = "slightly difficult",
                           "22_350F_2s_400UF.mp3" = "difficult", "21_300F_2s_400UF.mp3" = "slightly difficult",
                           "24_450F_2s_400UF.mp3" = "difficult", "25_500F_2s_400UF.mp3" = "slightly difficult",
                           "31_350UF_2s_400F.mp3" = "difficult", "30_300UF_2s_400F.mp3" = "slightly difficult",
                           "33_450UF_2s_400F.mp3" = "difficult", "34_500UF_2s_400F.mp3" = "slightly difficult",
                           "40_800F_2s_700UF.mp3" = "difficult", "39_800F_2s_600UF.mp3" = "slightly difficult",
                           "42_800F_2s_900UF.mp3" = "difficult", "43_800F_2s_1000UF.mp3" = "slightly difficult",
                           "49_800UF_2s_700F.mp3" = "difficult", "48_800UF_2s_600F.mp3" = "slightly difficult",
                           "51_800UF_2s_900F.mp3" = "difficult", "52_800UF_2s_1000F.mp3" = "slightly difficult",
                           "58_700F_2s_800UF.mp3" = "difficult", "57_600F_2s_800UF.mp3" = "slightly difficult",
                           "60_900F_2s_800UF.mp3" = "difficult", "61_1000F_2s_800UF.mp3" = "slightly difficult",
                           "67_700UF_2s_800F.mp3" = "difficult", "66_600UF_2s_800F.mp3" = "slightly difficult",
                           "69_900UF_2s_800F.mp3" = "difficult", "70_1000UF_2s_800F.mp3" = "slightly difficult"))

w_n_task <- w_n_task %>%
  rename(difficulty = 'Stimulus')

#####Download#####
library(openxlsx)
write_csv(x = demo, file = "demo.csv")
write_csv(x = w_n_task, file = "w_n_task.csv")
write_csv(x = w_same_task, file = "w_same_task.csv")



####Read and Analysis####

#### demo####
demo <- read_csv("demo.csv")

demo_summary_gender <- demo %>%
  group_by(gender) %>%
  summarise(number = n(), mean_C = mean(age), 
            sd_C = sd(age), median_C = median(age), 
            min_C = min(age), max_C = max(age))
demo_summary_gender

demo_summary_age <- demo %>%
  summarise(number = n(), mean_C = mean(age), 
            sd_C = sd(age), median_C = median(age), 
            min_C = min(age), max_C = max(age))
demo_summary_age


#### Read-Correct####

w_n_task <- read_csv("1n_task.csv")
head(w_n_task)

##### describe#####
C_summary_l <- w_n_task %>%
  group_by(Length) %>%
  summarise(number = n(), mean_C = mean(Correct), 
            sd_C = sd(Correct))


C_summary_o <- w_n_task %>%
  group_by(Order) %>%
  summarise(number = n(), mean_C = mean(Correct), 
            sd_C = sd(Correct))


C_summary_d <- w_n_task %>%
  group_by(Difficulty) %>%
  summarise(number = n(), mean_C = mean(Correct), 
            sd_C = sd(Correct))

C_summary_l
C_summary_o
C_summary_d


##### Diff Plots#####
#Length
set.seed(1234)
box.d.l <- w_n_task %>%
  ggplot(aes(x = Length, y = Correct, colour = Length, shape = Length)) +
  geom_jitter(width = 0.08, alpha = 0.3, size = 1) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(width = 0.4, alpha = 0.3) +
  stat_summary(fun.data = "mean_cl_boot", size = 0.5, color = "black") +
  theme(text = element_text(size = 9)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 1.2)) +
  theme_minimal() +
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  guides(colour = "none", shape = "none") +
  labs(x = "Length",
       y = "Proportion of Correct Responses") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10))
box.d.l

#Order
set.seed(1234)
box.d.o <- w_n_task %>%
  ggplot(aes(x = Order, y = Correct, colour = Order, shape = Order)) +
  geom_jitter(width = 0.08, alpha = 0.3, size = 1) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(width = 0.4, alpha = 0.3) +
  stat_summary(fun.data = mean_cl_boot, size = 0.5, color = "black") +
  theme(text = element_text(size = 9)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 1.2)) +
  theme_minimal() +
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  guides(colour = "none", shape = "none") +
  labs(x = "Order",
       y = "Proportion of Correct Responses") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10))
box.d.o


#Difficulty
w_n_task$Difficulty <- factor(w_n_task$Difficulty,
                        levels = c("Difficult", "Slightly difficult", 
                                   "Slightly easy", "Easy"))

mean.d <- w_n_task %>%
  group_by(Difficulty) %>%
  summarise(ave = mean(Correct)) %>%
  ungroup()

set.seed(1234)
box.d.d <- w_n_task %>%
  ggplot(aes(x = Difficulty, y = Correct, colour = Difficulty, shape = Difficulty)) +
  geom_jitter(width = 0.08, alpha = 0.3, size = 1) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(width = 0.4, alpha = 0.3) +
  geom_line(data = mean.d, mapping = aes(x = Difficulty, y = ave, group = 1),
            colour = "black", size = 0.8) +
  stat_summary(fun.data = mean_cl_boot, size = 0.5, color = "black") +
  theme(text = element_text(size = 9)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 1.2)) +
  theme_minimal() +
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  guides(colour = "none", shape = "none") +
  labs(x = "Difficulty",
       y = "Proportion of Correct Responses") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10))
box.d.d



##### Hypothesis#####
ggqqplot(w_n_task$Correct)

lm.c <- lm(Correct ~ Length*Order*Difficulty, data = w_n_task)
check_model(lm.c)


##### ANOVA#####
anova_C <- aov_4(Correct ~ Length*Order*Difficulty + (1 + Length*Order*Difficulty | ID),
                 data = w_n_task)
summary(anova_C)
anova_C

model_c <- emmeans(anova_C, pairwise ~ Difficulty, adjust = "bonferroni")
model_c




#### Read-Same illusion####
same_task <- read_csv("2same_task.csv")

##### describe#####
s_summary_l <- same_task %>%
  group_by(Length) %>%
  summarise(number = n(), mean_C = mean(illusion), 
            sd_C = sd(illusion))


s_summary_o <- same_task %>%
  group_by(Order) %>%
  summarise(number = n(), mean_C = mean(illusion), 
            sd_C = sd(illusion))

s_summary_l
s_summary_o


##### Hypothesis#####
ggqqplot(same_task, "illusion")

lm.s <- lm(illusion ~ Length * Order, data = same_task)
check_model(lm.s)


##### Same Plots#####
#Length
set.seed(1234)
box.s.l <- same_task %>%
  ggplot(aes(x = Length, y = illusion, colour = Length, shape = Length)) +
  geom_jitter(width = 0.08, alpha = 0.3, size = 1) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(width = 0.4, alpha = 0.3) +
  stat_summary(fun = "mean", size = 0.5, color = "black") +
  theme(text = element_text(size = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 1.2)) +
  theme_minimal() +
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  guides(colour = "none", shape = "none") +
  labs(x = "Length",
       y = "Proportion of Filled Intervals Selected as Longer") 
box.s.l

#Order
set.seed(1234)
box.s.o <- same_task %>%
  ggplot(aes(x = Order, y = illusion, colour = Order, shape = Order)) +
  geom_jitter(width = 0.08, alpha = 0.3, size = 1) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(width = 0.4, alpha = 0.3) +
  stat_summary(fun = "mean", size = 0.5, color = "black") +
  theme(text = element_text(size = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 1.2)) +
  theme_minimal() +
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  guides(colour = "none", shape = "none") +
  labs(x = "Order",
       y = "Proportion of Filled Intervals Selected as Longer") 
box.s.o



##### t test#####
t.test(same_task$illusion, mu=0.5)


##### ANOVA#####
anova_s <- aov_4(illusion ~ Length*Order + (1 + Length*Order | ID),
                 data = same_task)
summary(anova_s)
anova_s

model_sl <- emmeans(anova_s, pairwise ~ Length, adjust = "bonferroni")
model_sl

model_so <- emmeans(anova_s, pairwise ~ Order)
model_so



#### Read-Err illusion####
e_task <- read_csv("3e_task3.csv")

##### describe#####
e_summary_l <- e_task %>%
  group_by(Length) %>%
  summarise(number = n(), mean_C = mean(illusion), 
            sd_C = sd(illusion))


e_summary_o <- e_task %>%
  group_by(Order) %>%
  summarise(number = n(), mean_C = mean(illusion), 
            sd_C = sd(illusion))


e_summary_d <- e_task %>%
  group_by(Difficulty) %>%
  summarise(number = n(), mean_C = mean(illusion), 
            sd_C = sd(illusion))

e_summary_l
e_summary_o
e_summary_d



##### Hypothesis#####
lm.e <- lm(illusion ~ Length*Order*Difficulty, data = e_task)
check_model(lm.e)

##### Err Plots#####
#Length
set.seed(1234)
box.e.l <- e_task %>%
  ggplot(aes(x = Length, y = illusion, colour = Length, shape = Length)) +
  geom_jitter(width = 0.08, alpha = 0.3, size = 1) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(width = 0.4, alpha = 0.3) +
  stat_summary(fun = "mean", size = 0.5, color = "black") +
  theme(text = element_text(size = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 1.2)) +
  theme_minimal() +
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  guides(colour = "none", shape = "none") +
  labs(x = "Length",
       y = "Proportion of Filled Intervals Selected as Longer") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11))
box.e.l

#Order
set.seed(1234)
box.e.o <- e_task %>%
  ggplot(aes(x = Order, y = illusion, colour = Order, shape = Order)) +
  geom_jitter(width = 0.08, alpha = 0.3, size = 1) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(width = 0.4, alpha = 0.3) +
  stat_summary(fun = "mean", size = 0.5, color = "black") +
  theme(text = element_text(size = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 1.2)) +
  theme_minimal() +
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  guides(colour = "none", shape = "none") +
  labs(x = "Order",
       y = "Proportion of Filled Intervals Selected as Longer") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11))
box.e.o


#Difficulty
e_task$Difficulty <- factor(e_task$Difficulty,
                            levels = c("Difficult", "Slightly difficult",
                                       "Slightly easy", "Easy"))

mean.de <- e_task %>%
  group_by(Difficulty) %>%
  summarise(ave = mean(illusion)) %>%
  ungroup()

set.seed(1234)
box.e.d <- e_task %>%
  ggplot(aes(x = Difficulty, y = illusion, colour = Difficulty, shape = Difficulty)) +
  geom_jitter(width = 0.08, alpha = 0.3, size = 1) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(width = 0.4, alpha = 0.3) +
  geom_line(data = mean.de, mapping = aes(x = Difficulty, y = ave, group = 1),
            colour = "black", size = 0.8) +
  stat_summary(fun = "mean", size = 0.5, color = "black") +
  theme(text = element_text(size = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 1.2)) +
  theme_minimal() +
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  guides(colour = "none", shape = "none") +
  labs(x = "Difficulty",
       y = "Proportion of Filled Intervals Selected as Longer") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11))
box.e.d



##### T test#####
t.test(e_task$illusion, mu=0.5)


##### ANOVA#####
anova_e <- aov_4(illusion ~ Length*Order*Difficulty + (1 + Length*Order*Difficulty | ID),
                 data = e_task3)
summary(anova_e)
anova_e

model_el <- emmeans(anova_e, pairwise ~ Length, adjust = "bonferroni")
model_el

model_eo <- emmeans(anova_e, pairwise ~ Order, adjust = "bonferroni")
model_eo


