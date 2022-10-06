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
library(lme4)
library(lmerTest)
library(pbkrtest)



#### demo####
data_q <- read_csv("consent.csv")

#q.gender
gender <- data_q %>%
  filter(Question == "gender")

gender <- gender %>%
  rename(gender = 'Response')

gender <- gender[,-grep("Question", colnames(gender))]

#q.age
age <- data_q %>%
  filter(Question == "birth.date-year")

age <- age %>%
  rename(age = 'Response')

age <- age[,-grep("Question", colnames(age))]


#combine + check NA
demo <- left_join(age, gender, by = 'ID')

demo %>%
  is.na() %>%
  sum()

head(demo)

library(openxlsx)
write_csv(x = demo, file = "C:/Users/Administrator/Desktop/Project/EXP 2/Data/demo.csv")

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
w_n_task <- read_csv("1_Acc.csv")
head(w_n_task)

##### describe#####
C_summary_l <- w_n_task %>%
  group_by(Gap) %>%
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
######Gap#####
w_n_task <- w_n_task %>%
  mutate(Gap = factor(Gap))
w_n_task <- w_n_task %>%
  mutate(Order = factor(Order))
w_n_task <- w_n_task %>%
  mutate(Difficulty = factor(Difficulty))
head(w_n_task)

mean.dg <- w_n_task %>%
  group_by(Gap) %>%
  summarise(ave = mean(Correct)) %>%
  ungroup()

set.seed(1234)
vio.d.g <- w_n_task %>%
  ggplot(aes(x = Gap, y = Correct, colour = Gap, shape = Gap)) +
  geom_violin(aes(fill = Gap), alpha = 0.2, width = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1) +
  geom_line(data = mean.dg, mapping = aes(x = Gap, y = ave, group = 1),
            colour = "black", size = 0.8) +
  theme(text = element_text(size = 10)) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 1.2)) +
  theme_minimal() +
  guides(color = 'none', shape = 'none', fill = 'none') +
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  labs(x = "Gap", 
       y = "Proportion of Correct Responses") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11))
vio.d.g

#Order
set.seed(1234)
vio.d.o <- w_n_task %>%
  ggplot(aes(x = Order, y = Correct, colour = Order, shape = Order,
             label = sprintf("%.3f", Order))) +
  geom_violin(aes(fill = Order), alpha = 0.2, width = 0.5) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1) +
  theme(text = element_text(size = 10)) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 1.2)) +
  theme_minimal() +
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  guides(colour = "none", shape = "none", fill = 'none') +
  labs(x = "Order",
       y = "Proportion of Correct Responses") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11))
vio.d.o


#Difficulty
w_n_task$Difficulty <- factor(w_n_task$Difficulty,
                              levels = c("Difficult", "Slightly difficult", 
                                         "Slightly easy", "Easy"))

mean.dd <- w_n_task %>%
  group_by(Difficulty) %>%
  summarise(ave = mean(Correct)) %>%
  ungroup()

set.seed(1234)
vio.d.d <- w_n_task %>%
  ggplot(aes(x = Difficulty, y = Correct, colour = Difficulty, shape = Difficulty)) +
  geom_violin(aes(fill = Difficulty), alpha = 0.2, width = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1) +
  geom_line(data = mean.dd, mapping = aes(x = Difficulty, y = ave, group = 1),
            colour = "black", size = 0.8) +
  theme(text = element_text(size = 10)) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 1.2)) +
  theme_minimal() +
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  guides(colour = "none", shape = "none", fill = 'none') +
  labs(x = "Difficulty",
       y = "Proportion of Correct Responses") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11))
vio.d.d


##### Hypothesis#####
lm.c <- lm(Correct ~ Gap*Order*Difficulty, data = w_n_task)
check_model(lm.c)

##### ANOVA#####
anova_C <- aov_4(Correct ~ Gap*Order*Difficulty + (1 + Gap*Order*Difficulty | ID),
                 data = w_n_task)
summary(anova_C)
anova_C

model_cg <- emmeans(anova_C, pairwise ~ Gap, adjust = "bonferroni")
model_cg

model_cd <- emmeans(anova_C, pairwise ~ Difficulty, adjust = "bonferroni")
model_cd



#### Read-Same illusion####
same_task <- read_csv("2_Same.csv")

##### describe#####
s_summary_l <- same_task %>%
  group_by(Gap) %>%
  summarise(number = n(), mean_C = mean(Illusion), 
            sd_C = sd(Illusion))


s_summary_o <- same_task %>%
  group_by(Order) %>%
  summarise(number = n(), mean_C = mean(Illusion), 
            sd_C = sd(Illusion))

s_summary_l
s_summary_o


##### Hypothesis#####
lm.s <- lm(Illusion ~ Gap * Order, data = same_task)
check_model(lm.s) #not fit well - build mixed model

sm_model <- lmer(Illusion ~ Gap*Order + (1 + Gap*Order| ID), data = same_task) #over fit

sm_model <- lmer(Illusion ~ Gap*Order + (1| ID), data = same_task)
check_model(sm_model) #Normality - fit well

##### Same Plots#####
######Gap#####
same_task <- same_task %>%
  mutate(Gap = factor(Gap))

mean.sg <- same_task %>%
  group_by(Gap) %>%
  summarise(ave = mean(Illusion)) %>%
  ungroup()

set.seed(1234)
vio.s.g <- same_task %>%
  ggplot(aes(x = Gap, y = Illusion, colour = Gap, shape = Gap)) +
  geom_violin(aes(fill = Gap), alpha = 0.2, width = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1) +
  geom_line(data = mean.sg, mapping = aes(x = Gap, y = ave, group = 1),
            colour = "black", size = 0.8) +
  stat_summary(fun = "mean", size = 0.5, color = "black") +
  theme(text = element_text(size = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 1.2)) +
  theme_minimal() +
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  guides(colour = "none", shape = "none", fill = 'none') +
  labs(x = "Gap",
       y = "Proportion of Filled Intervals Selected as Longer") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11))
vio.s.g

#Order
set.seed(1234)
vio.s.o <- same_task %>%
  ggplot(aes(x = Order, y = Illusion, colour = Order, shape = Order)) +
  geom_violin(aes(fill = Order), alpha = 0.2, width = 0.5) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1) +
  stat_summary(fun = "mean", size = 0.5, color = "black") +
  theme(text = element_text(size = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 1.2)) +
  theme_minimal() +
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  guides(colour = "none", shape = "none", fill = 'none') +
  labs(x = "Order",
       y = "Proportion of Filled Intervals Selected as Longer") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11))
vio.s.o


##### t test#####
t.test(same_task$Illusion, mu=0.5)


##### ANOVA#####
##LMM
same_task <- same_task %>%
  mutate(Gap = factor(Gap))
same_task <- same_task %>%
  mutate(Order = factor(Order))
head(same_task)

sm_model <- lmer(Illusion ~ Gap*Order + (1| ID), data = same_task)
check_model(sm_model)
summary(sm_model)
anova(sm_model)

model_sg <- emmeans(sm_model, pairwise ~ Gap, adjust = "bonferroni")
model_sg

model_s <- emmeans(sm_model, pairwise ~ Gap*Order, adjust = "bonferroni")
model_s



#### Read-Err illusion####
e_task2 <- read_csv("3.2_Err.csv")
head(e_task)

##### describe#####
e_summary_l <- e_task2 %>%
  group_by(Gap) %>%
  summarise(number = n(), mean_C = mean(Illusion, na.rm = TRUE), 
            sd_C = sd(Illusion, na.rm = TRUE))


e_summary_o <- e_task2 %>%
  group_by(Order) %>%
  summarise(number = n(), mean_C = mean(Illusion, na.rm = TRUE), 
            sd_C = sd(Illusion, na.rm = TRUE))


e_summary_d <- e_task2 %>%
  group_by(Difficulty) %>%
  summarise(number = n(), mean_C = mean(Illusion, na.rm = TRUE), 
            sd_C = sd(Illusion, na.rm = TRUE))

e_summary_l
e_summary_o
e_summary_d



##### Hypothesis#####
lm.e <- lm(Illusion ~ Gap*Order*Difficulty, data = e_task2)
check_model(lm.e) # not fit well

#over fit models
m_model <- lmer(Illusion ~ Gap*Order*Difficulty + (1 +Gap*Order*Difficulty | ID), data = e_task2)
m_model <- lmer(Illusion ~ Gap*Order*Difficulty + (1 +Gap*Ordery | ID), data = e_task2)

#final model
m_model <- lmer(Illusion ~ Gap*Order*Difficulty + (1 | ID), data = e_task2)
check_model(m_model) #fit well


##### Err Plots#####
#Gap
e_task2 <- e_task2 %>%
  mutate(Gap = factor(Gap))

mean.eg <- e_task2 %>%
  group_by(Gap) %>%
  summarise(ave = mean(Illusion)) %>%
  ungroup()

set.seed(1234)
vio.e.g <- e_task2 %>%
  ggplot(aes(x = Gap, y = Illusion, colour = Gap, shape = Gap)) +
  geom_violin(aes(fill = Gap), alpha = 0.2, width = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1) +
  geom_line(data = mean.eg, mapping = aes(x = Gap, y = ave, group = 1),
            colour = "black", size = 0.7) +
  stat_summary(fun = "mean", size = 0.5, color = "black") +
  theme(text = element_text(size = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 1.2)) +
  theme_minimal() +
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  guides(colour = "none", shape = "none", fill='none') +
  labs(x = "Gap",
       y = "Proportion of Filled Intervals Selected as Longer") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11))
vio.e.g

#Order
set.seed(1234)
vio.e.o <- e_task2 %>%
  ggplot(aes(x = Order, y = Illusion, colour = Order, shape = Order)) +
  geom_violin(aes(fill = Order), alpha = 0.2, width = 0.5) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1) +
  stat_summary(fun = "mean", size = 0.5, color = "black") +
  theme(text = element_text(size = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 1.2)) +
  theme_minimal() +
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  guides(colour = "none", shape = "none", fill='none') +
  labs(x = "Order",
       y = "Proportion of Filled Intervals Selected as Longer") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11))
vio.e.o


#Difficulty
e_task2$Difficulty <- factor(e_task2$Difficulty,
                            levels = c("Difficult", "Slightly difficult",
                                       "Slightly easy", "Easy"))
head(e_task2)

mean.ed <- e_task2 %>%
  group_by(Difficulty) %>%
  summarise(ave = mean(Illusion)) %>%
  ungroup()

set.seed(1234)
vio.e.d <- e_task2 %>%
  ggplot(aes(x = Difficulty, y = Illusion, colour = Difficulty, shape = Difficulty)) +
  geom_violin(aes(fill = Difficulty), alpha = 0.2, width = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1) +
  geom_line(data = mean.ed, mapping = aes(x = Difficulty, y = ave, group = 1),
            colour = "black", size = 0.7) +
  stat_summary(fun = "mean", size = 0.5, color = "black") +
  theme(text = element_text(size = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 1.2)) +
  theme_minimal() +
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  guides(colour = "none", shape = "none", fill = 'none') +
  labs(x = "Difficulty",
       y = "Proportion of Filled Intervals Selected as Longer") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11))
vio.e.d


e_go <- e_task2 %>%
  group_by(Gap,Order) %>%
  summarise(number = n(), mean_C = mean(Illusion), 
            sd_C = sd(Illusion))
e_go

#t test
UFgap5 <- e_task2 %>%
  filter(Order == "Unfilled_Filled", Gap == "5")

FUgap5 <- e_task2 %>%
  filter(Order == "Filled_Unfilled", Gap == "5")

UF <- e_task2 %>%
  filter(Order == "Unfilled_Filled")

gap5 <- e_task2 %>%
  filter(Gap == "5")

t.test(UFgap5$Illusion, mu=0.5) #No Significant p = 0.224
t.test(FUgap5$Illusion, mu=0.5) #0.033
t.test(UF$Illusion, mu=0.5)
t.test(gap5$Illusion, mu=0.5) #0.018

#####ANOVA#####
e_task2 <- e_task2 %>%
  mutate(Gap = factor(Gap))
e_task2 <- e_task2 %>%
  mutate(Order = factor(Order))
e_task2 <- e_task2 %>%
  mutate(Difficulty = factor(Difficulty))
head(e_task2)

m_model <- lmer(Illusion ~ Gap*Order*Difficulty + (1 | ID), data = e_task2)
check_model(m_model)
summary(m_model)
anova(m_model)

model_eg <- emmeans(m_model, pairwise ~ Gap, adjust = "bonferroni")
model_eg

pwc3 <- emmeans(m_model, pairwise ~ Order*Gap, adjust = "bonferroni")
pwc3 


as.data.frame(pwc3)

#####plots#####
data_35Un <- e_task2 %>%
  filter(Order == "Unfilled_Filled")

pwc_un <- data_35Un %>%
  emmeans_test(Illusion ~ Gap, p.adjust.method = "bonferroni") 
pwc_un

pwc_un_p <- pwc_un %>% 
  add_xy_position(x = "Gap")

pwc_un_p <- pwc_un_p[c(-1,-2),]
pwc_un_p

plot3_line <- e_task2 %>%
  group_by(Gap, Order) %>%
  ggline(x = "Gap", y = "Illusion", 
         color = "Order", shape = "Order", add = "mean_se") +
  geom_signif(comparisons = list(c("1", "3")),
              y_position=c(0.71), 
              tip_length = c(0.01, 0.01),
              map_signif_level=TRUE, color = "#F8766D") +
  stat_pvalue_manual(pwc_un_p, 
                     y.position=c(0.65), 
                     tip.length = 0.01,
                     map_signif_level=TRUE, color = "#00BFC4") +
  geom_signif(comparisons = list(c("1", "5")),
              y_position=c(0.74), 
              tip_length = c(0.01, 0.01),
              map_signif_level=TRUE, color = "black") +
  theme(text = element_text(size = 10)) +
  labs(x = "Gap",
       y = "Proportion of Filled Intervals Selected as Longer") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11),
        legend.position = "right")
plot3_line



#####plot 2#####
data_35Un <- e_task2 %>%
  filter(Order == "Unfilled_Filled")

pwc_un <- data_35Un %>%
  emmeans_test(Illusion ~ Gap, p.adjust.method = "bonferroni") 
pwc_un

pwc_un_p <- pwc_un %>% 
  add_xy_position(x = "Gap")

pwc_un_p <- pwc_un_p[c(-1,-2),]
pwc_un_p

plot3_line2 <- e_task2 %>%
  group_by(Gap, Order) %>%
  ggline(x = "Gap", y = "Illusion", 
         color = "Order", shape = "Order", add = "mean_se", size = 1.1) +
  geom_signif(comparisons = list(c("1", "3")),
              y_position=c(0.71), 
              tip_length = c(0.01, 0.01),
              map_signif_level=TRUE, color = "#F8766D") +
  stat_pvalue_manual(pwc_un_p, 
                     y.position=c(0.65), 
                     tip.length = 0.01,
                     map_signif_level=TRUE, color = "#00BFC4") +
  geom_signif(comparisons = list(c("1", "5")),
              y_position=c(0.74), 
              tip_length = c(0.01, 0.01),
              map_signif_level=TRUE, color = "black") +
  theme(text = element_text(size = 14)) +
  labs(x = "Gap",
       y = "Proportion of Filled Intervals Selected as Longer") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11),
        legend.position = "top")
plot3_line2


plot3_line <- e_task2 %>%
  group_by(Order, Gap) %>%
  ggline(x = "Order", y = "Illusion", 
         color = "Gap", shape = "Gap", add = "mean_se") +
    labs(title = "Effects of Gap and Order on Proportion of Filled Intervals",
       x = "Gap",
       y = "Proportion of Filled Intervals") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11),
        legend.position = "right")
plot3_line

