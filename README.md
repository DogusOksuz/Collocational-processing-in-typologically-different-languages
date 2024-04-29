
#Study 1 Corpus Study

mean_zipf_scale_freq <- corpus_data %>% summarise(
  freq_mean = mean(zipf_scale_freq),
  freq_sd = sd(zipf_scale_freq))
mean_zipf_scale_freq
  
corpus_data_lang_agg <- corpus_data %>%
  group_by(Language) %>%
  summarise(mean_frequency_counts = mean(zipf_scale_freq),
            sd_frequency_counts = sd(zipf_scale_freq)) 
corpus_data_lang_agg

corpus_data_lemma_agg <- corpus_data %>%
  group_by(lematisation_status) %>%
  summarise(mean_frequency_counts = mean(zipf_scale_freq),
            sd_frequency_counts = sd(zipf_scale_freq))
corpus_data_lemma_agg            

corpus_data_form <- filter(corpus_data, lematisation_status == 'form')
corpus_data_form
corpus_data_form_agg <- corpus_data_form  %>%
  group_by(Language) %>%
  summarise(mean_frequency_counts = mean(zipf_scale_freq),
            sd_frequency_counts = sd(zipf_scale_freq))

corpus_data_form_agg

corpus_data_lemma <- filter(corpus_data, lematisation_status == 'Inf_family')

corpus_data_lemma_agg <- corpus_data_lemma  %>%
  group_by(Language) %>%
  summarise(mean_frequency_counts = mean(zipf_scale_freq),
            sd_frequency_counts = sd(zipf_scale_freq))
corpus_data_lemma_agg

box_plot <- ggplot(data = corpus_data, aes(x = Language, y = zipf_scale_freq)) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(alpha = 0) +
  facet_grid(lematisation_status ~ .) +
  theme_classic() +
  labs(y = "Frequency")
box_plot

range(corpus_data$zipf_scale_freq)

#violin plots for frequency counts
Figure_1 <- ggplot(corpus_data, aes(x = Language, y= zipf_scale_freq, fill = Language)) +
  geom_violin(alpha = .4) +
  geom_boxplot(width = .2, fatten = NULL, alpha = .6) +
  stat_summary(fun = "mean", geom = "point") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = .1) +
  facet_wrap(~factor(lematisation_status,
                     levels = c("Inf_family", "Form"),
                     labels = c("Inf_family frequency", 
                                "Form frequency"))) +
  theme_minimal() + 
  scale_y_continuous(name = "Zipf Scale Frequency Counts")
  scale_fill_brewer(palette = "Dark2") +
  guides(fill = "none")

Figure_1
Figure_1 + theme(text = element_text(size = 12))                    # for text 
Figure_1 + theme(text = element_text(size = 25))                    # for presentation - font sizes

corpus_data
language <- as.factor(corpus_data$Language)
language <- as_tibble(language)
language
lemmatisation <- as.factor(corpus_data$lematisation_status)
lemmatisation <- as_tibble(lemmatisation)
lemmatisation

corpus_freq <- cbind(corpus_data, language)
corpus_freq <- rename(corpus_freq, Lang = value)
corpus_freq <- cbind(corpus_freq, lemmatisation)
corpus_freq <- rename(corpus_freq, lemmat = value)

corpus_freq <- select(corpus_freq, Collocations, coll_length, Lang, lemmat, zipf_scale_freq)
corpus_freq <- as_tibble(corpus_freq)
corpus_freq

corpus_freq_form <- filter(corpus_freq, lemmat == 'Form')
corpus_freq_Inf_family <- filter(corpus_freq, lemmat == 'Inf_family')

#Study 2 - Reaction time study 2

#Data wrangling 1

reaction_time <- read.csv("reaction_time_data_Eng&Turk.csv")
reaction_time
reaction_time <- as_tibble(reaction_time)
reaction_time <- select(reaction_time, Participant:Language)
reaction_time
summary(reaction_time)
sample_n(reaction_time, 4) #to see randomly selected 4 columns
reaction_time %>% print(width = Inf) #to display all columns 
reaction_time_subset <- select(reaction_time, Participant, OrderofOccurrence, ItemType, item, AdjectiveFreq, NounFreq, LemNounFreq, CollFreq, LemCollFreq, LDscores, Length, ReactionTime, Language)
reaction_time_subset <- reaction_time_subset %>% relocate(Language, .after = Participant) #selecting columns we need 
reaction_time_subset <- mutate(reaction_time_subset, responsetime_ms = ReactionTime * 1000)
reaction_time_subset
#Data wrangling 2

reaction_time_subset <- reaction_time_subset %>% mutate(OrderofOccurrence_cat = cut(OrderofOccurrence, breaks = c(-Inf, 1, 2), labels = c("first", "second")))
reaction_time_subset <- reaction_time_subset %>% relocate(OrderofOccurrence_cat, .before = OrderofOccurrence)
reaction_time_subset <- reaction_time_subset %>% modify_at(c("Language"), as.factor)
reaction_time_subset <- reaction_time_subset %>% modify_at(c("ItemType"), as.factor)
reaction_time_subset <- reaction_time_subset %>% mutate(log_RT = log(responsetime_ms))
reaction_time_subset <- reaction_time_subset %>% mutate(Adject_freq = scale(AdjectiveFreq, center = TRUE))
reaction_time_subset <- reaction_time_subset %>% mutate(noun_freq = scale(NounFreq, center = TRUE))
reaction_time_subset <- reaction_time_subset %>% mutate(Lem_Noun_Freq = scale(LemNounFreq, center = TRUE))
reaction_time_subset <- reaction_time_subset %>% mutate(Coll_Freq = scale(CollFreq, center = TRUE))
reaction_time_subset <- reaction_time_subset %>% mutate(Lem_Coll_Freq = scale(LemCollFreq, center = TRUE))
reaction_time_subset <- reaction_time_subset %>% mutate(Length_centered = scale(Length, center = TRUE))
reaction_time_m <- select(reaction_time_subset, Participant, Language, OrderofOccurrence_cat, ItemType, item, Adject_freq, noun_freq, Lem_Noun_Freq, Coll_Freq, Lem_Coll_Freq, Length_centered, LDscores, responsetime_ms, log_RT)
reaction_time_m  %>% rename(adj_freq = Adject_freq,
                            noun_freq = noun_freq,
                            lem_noun_freq = Lem_Noun_Freq,
                            coll_freq = Coll_Freq,
                            lem_coll_freq = Lem_Coll_Freq,
                            length = Length_centered)

reaction_time_m %>% print(width = Inf) #to display all columns 
reaction_time_m
summary(reaction_time_m)
#exploring the distribution of DVs

plot1 <- ggplot(reaction_time_m, aes(x = responsetime_ms)) +    #histogram for response times in milliseconds 
  geom_histogram()

plot2<-ggplot(reaction_time_m, aes(x = log_RT)) +    #histogram for log response times in milliseconds 
  geom_histogram()

grid.arrange(plot1, plot2, ncol = 2)
#Hand coding of categorical variables

contrasts(reaction_time_m$ItemType)
contrasts(reaction_time_m$Language) <- c(.5, -.5)
contrasts(reaction_time_m$Language)
#Data_visualisation (Figure 2)

evtools::install_github("psyteachr/introdataviz")

Split_violin <- ggplot(reaction_time_m, aes(x = ItemType, y = responsetime_ms, fill = Language)) +
  introdataviz::geom_split_violin(alpha = .4, trim = FALSE) +
  geom_boxplot(width = .2, alpha = .6, fatten = NULL, show.legend = FALSE) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = F, 
               position = position_dodge(.175)) +
  scale_x_discrete(name = "Item Type", labels = c("Baseline", "High frequency", "Low frequency")) +
  scale_y_continuous(name = "Response time (ms)",
                     breaks = seq(500, 3000, 500), 
                     limits = c(500, 3000)) +
  scale_fill_brewer(palette = "Set1", name = "Language") +
  theme_minimal()

Split_violin + theme(axis.text = element_text(size = 12))   
#Mixed-effect model (Model 3)

vif(lm(responsetime_ms ~ Language + ItemType + OrderofOccurrence_cat + Adject_freq + noun_freq + Coll_Freq + Length_centered + Lem_Noun_Freq + Lem_Coll_Freq + LDscores, data = reaction_time_m)) #to detect multi collinearities
vif(lm(responsetime_ms ~ Language + OrderofOccurrence_cat + Adject_freq + noun_freq + Coll_Freq + Length_centered + Lem_Noun_Freq + Lem_Coll_Freq +LDscores, data = reaction_time_m)) #to detect multi collinearities
vif(lm(responsetime_ms ~ Language + OrderofOccurrence_cat + Adject_freq + noun_freq + Coll_Freq + Length_centered + Lem_Noun_Freq  +LDscores, data = reaction_time_m)) #to detect multi collinearities
vif(lm(responsetime_ms ~ Language + OrderofOccurrence_cat + Adject_freq + noun_freq + ItemType  + Length_centered + Lem_Noun_Freq  +LDscores, data = reaction_time_m)) #to detect multi collinearities
#Hand coding of categorical variables

contrasts(reaction_time_m$Language) <- c(.5, -.5)
contrasts(reaction_time_m$Language)
#Maximal model - Mixed-effect model (Model 3)

mod1_max <- lmer(log_RT ~ Language + Coll_Freq + OrderofOccurrence_cat + Lem_Noun_Freq + Adject_freq + noun_freq + Length_centered + Language*Coll_Freq + Language*Adject_freq + Language*noun_freq + Coll_Freq*Adject_freq + Coll_Freq*noun_freq + 
                   Lem_Noun_Freq*Language + Coll_Freq*Lem_Noun_Freq + (1+Coll_Freq|Participant) + (1+Adject_freq|Participant) + (1+noun_freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod1_max)
options(scipen = 999)
#Mixed-effect model 3 - step-by-step-model comparison

mod1_1 <- lmer(log_RT ~ Language + Coll_Freq + Lem_Noun_Freq + Adject_freq + noun_freq + Length_centered + Language*Coll_Freq + Language*Adject_freq + Language*noun_freq + Coll_Freq*Adject_freq + Coll_Freq*noun_freq + 
                 Lem_Noun_Freq*Language + Coll_Freq*Lem_Noun_Freq + (1+Coll_Freq|Participant) + (1+Adject_freq|Participant) + (1+noun_freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod1_1)
mod1_2 <- lmer(log_RT ~ Language + Coll_Freq + Lem_Noun_Freq + Adject_freq + noun_freq + Length_centered + Language*Coll_Freq + Language*Adject_freq + Language*noun_freq + Coll_Freq*Adject_freq + Coll_Freq*noun_freq + 
                 Lem_Noun_Freq*Language + (1+Coll_Freq|Participant) + (1+Adject_freq|Participant) + (1+noun_freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod1_2)
mod1_3 <- lmer(log_RT ~ Language + Coll_Freq + Lem_Noun_Freq + Adject_freq + noun_freq + Length_centered + Language*Coll_Freq + Language*Adject_freq + Language*noun_freq + Coll_Freq*Adject_freq + Coll_Freq*noun_freq + 
                 (1+Coll_Freq|Participant) + (1+Adject_freq|Participant) + (1+noun_freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod1_3)
mod1_4 <- lmer(log_RT ~ Language + Coll_Freq + Lem_Noun_Freq + Adject_freq + noun_freq + Length_centered + Language*Coll_Freq + Language*Adject_freq + Language*noun_freq + Coll_Freq*noun_freq + 
                 (1+Coll_Freq|Participant) + (1+Adject_freq|Participant) + (1+noun_freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod1_4)
mod1_5 <- lmer(log_RT ~ Language + Coll_Freq + Lem_Noun_Freq + Adject_freq + noun_freq + Length_centered + Language*Coll_Freq + Language*Adject_freq + Coll_Freq*noun_freq + 
                 (1+Coll_Freq|Participant) + (1+Adject_freq|Participant) + (1+noun_freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod1_5)
mod1_6 <- lmer(log_RT ~ Language + Coll_Freq + Lem_Noun_Freq + Adject_freq + noun_freq + Length_centered + Language*Coll_Freq + Coll_Freq*noun_freq + 
                 (1+Coll_Freq|Participant) + (1+Adject_freq|Participant) + (1+noun_freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod1_6)
mod1_7 <- lmer(log_RT ~ Language + Coll_Freq + Adject_freq + noun_freq + Length_centered + Language*Coll_Freq + Coll_Freq*noun_freq + 
                 (1+Coll_Freq|Participant) + (1+Adject_freq|Participant) + (1+noun_freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod1_7)
mod1_8 <- lmer(log_RT ~ Language + Coll_Freq + Adject_freq + noun_freq + Length_centered + Language*Coll_Freq + Coll_Freq*noun_freq + 
                 (1+Coll_Freq|Participant) + (1+noun_freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod1_8)
mod1_9 <- lmer(log_RT ~ Language + Coll_Freq + Adject_freq + noun_freq + Length_centered + Language*Coll_Freq + Coll_Freq*noun_freq + 
                 (1+Coll_Freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod1_9)

mod1_10 <- lmer(log_RT ~ Language + Coll_Freq + Adject_freq + noun_freq + Length_centered + Language*Coll_Freq + 
                  (1+Coll_Freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod1_10)
anova(mod1_9, mod1_10)

mod1_9 <- lmer(log_RT ~ Language + Coll_Freq + Adject_freq + noun_freq + Length_centered + Language*Coll_Freq + Coll_Freq*noun_freq + 
                 (1+Coll_Freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod1_9) #best fitting model

#eliminating 0 frequencies
reaction_time_mmm <- filter(reaction_time_m, Lem_Coll_Freq > 0)

summary(reaction_time_mmm)
mod1_9_exc_baseline <- lmer(log_RT ~ Language + Coll_Freq + Adject_freq + noun_freq + Length_centered + Language*Coll_Freq + Coll_Freq*noun_freq + 
                              (1+Coll_Freq|Participant) + (1|item), data = reaction_time_mmm, REML = FALSE)
summary(mod1_9_exc_baseline) #best fitting model

r.squaredGLMM(mod1_9, null.RE = TRUE)
mean(reaction_time_m$responsetime_ms)
mean(reaction_time_m$log_RT)
mean(reaction_time_m[reaction_time_m$Language=="Turkish",]$log_RT)
mean(reaction_time_m[reaction_time_m$Language=="English",]$log_RT)
CI_mod1_9 <- confint(mod1_9, method = "Wald")
CI_mod1_9
#analysing interactions (Model 3)

mtrends(mod1_9, ~ pairwise ~ Language, var = "Coll_Freq")
emtrends(mod1_9, ~ pairwise ~ Language, var = "Lem_Coll_Freq")

int_plot <- ggplot(reaction_time_m,
                   aes(x = Coll_Freq,
                       y = log_RT,
                       color = Language)) +
  theme_bw() +
  labs(x = "Collocation frequency",
       y= "Log response time",
       color = "Language")
int_plot +
  geom_point(alpha = .3, 
             size = .10) +
  geom_smooth(method = "lm")

int_plot +
  geom_smooth(method = "lm",
              se = F)  + theme(axis.text = element_text(size = 12))   


int_plott <- ggplot(reaction_time_m,
                    aes(x = Coll_Freq,
                        y = log_RT,
                        color = Language)) +
  geom_point(size = .9,
             alpha = .3) +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_color_brewer(type = "qual", 
                     palette = 3) +
  labs(x = "Collocation frequency",
       y= "log response time",
       color = "Language")
int_plott
#Mixed-effect model (Model 4)

vif(lm(responsetime_ms ~ Language + ItemType + OrderofOccurrence_cat + Adject_freq + noun_freq + Coll_Freq + Length_centered, data = reaction_time_m)) #to detect multicollinearities
#Mixed-effect model 4 - step-by-step-model comparison

vif(lm(responsetime_ms ~ Language + ItemType + OrderofOccurrence_cat + Adject_freq + noun_freq + Coll_Freq + Length_centered, data = reaction_time_m)) #to detect multicollinearities

mod2_max <- lmer(log_RT ~ Language + ItemType + OrderofOccurrence_cat + Adject_freq + noun_freq + Length_centered + Language*ItemType + Language*Adject_freq + Language*noun_freq + ItemType*Adject_freq + ItemType*noun_freq + Language*OrderofOccurrence_cat +
                   (1+ItemType|Participant) + (1+Adject_freq|Participant) + (1+noun_freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod2_max) #maximal model

mod2_1 <- lmer(log_RT ~ Language + ItemType + OrderofOccurrence_cat + Adject_freq + noun_freq + Length_centered + Language*ItemType + Language*Adject_freq + Language*noun_freq + ItemType*Adject_freq + ItemType*noun_freq + Language*OrderofOccurrence_cat +
                 (1+ItemType|Participant) + (1+noun_freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod2_1)

mod2_2 <- lmer(log_RT ~ Language + ItemType + OrderofOccurrence_cat + Adject_freq + noun_freq + Length_centered + Language*ItemType + Language*Adject_freq + Language*noun_freq + ItemType*Adject_freq + ItemType*noun_freq + Language*OrderofOccurrence_cat +
                 (1+ItemType|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod2_2)

mod2_3 <- lmer(log_RT ~ Language + ItemType + OrderofOccurrence_cat + Adject_freq + noun_freq + Length_centered + Language*ItemType + Language*noun_freq + ItemType*Adject_freq + ItemType*noun_freq + Language*OrderofOccurrence_cat +
                 (1+ItemType|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod2_3)

mod2_4 <- lmer(log_RT ~ Language + ItemType + OrderofOccurrence_cat + Adject_freq + noun_freq + Length_centered + Language*ItemType + Language*noun_freq + ItemType*Adject_freq + ItemType*noun_freq + 
                 (1+ItemType|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod2_4)

mod2_5 <- lmer(log_RT ~ Language + ItemType + Adject_freq + noun_freq + Length_centered + Language*ItemType + Language*noun_freq + ItemType*Adject_freq + ItemType*noun_freq + 
                 (1+ItemType|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod2_5)

mod2_6 <- lmer(log_RT ~ Language + ItemType + Adject_freq + noun_freq + Length_centered + Language*ItemType + Language*noun_freq + ItemType*noun_freq + 
                 (1+ItemType|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod2_6)

mod2_7 <- lmer(log_RT ~ Language + ItemType + Adject_freq + noun_freq + Length_centered + Language*ItemType + ItemType*noun_freq + 
                 (1+ItemType|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod2_7)
CI_mod2_7
r.squaredGLMM(mod2_8, null.RE = TRUE)


#does not make a significant improvement in the model fit according to the log-likelihood ratio tests 
mod2_8 <- lmer(log_RT ~ Language + ItemType + Adject_freq + noun_freq + Length_centered + ItemType*noun_freq + 
                 (1+ItemType|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
both_lang_m$ItemType <- relevel(both_lang_m$ItemType, ref = "Low_freq")


mod2_7_relevel <- lmer(log_RT ~ Language + ItemType + Adject_freq + noun_freq + Length_centered + Language*ItemType + ItemType*noun_freq + 
                         (1+ItemType|Participant) + (1|item), data = both_lang_m, REML = FALSE)
summary(mod2_7_relevel)
CI_relevel <- confint(mod_7_relevel, method = "Wald")
CI_relevel

mod2_8 <- lmer(log_RT ~ Language + ItemType + Adject_freq + noun_freq + Length_centered + ItemType*noun_freq + 
                 (1+ItemType|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod2_8) #best fitting model
anova(mod_8, mod_7)
CI_mod2_8 <- confint(mod2_8, method = "Wald")
CI_mod2_8
#analysing interactions (Model 4)

emtrends(mod2_8, ~ pairwise ~ ItemType, var = "noun_freq")

interaction_plot <- ggplot(reaction_time_m,
                           aes(x = noun_freq,
                               y = log_RT,
                               color = ItemType)) +
  theme_bw() +
  labs(x = "Noun frequency",
       y= "Log response time",
       color = "ItemType")

interaction_plot +
  geom_point(alpha = .3, 
             size = .10) +
  geom_smooth(method = "lm")

interaction_plot +
  geom_smooth(method = "lm",
              se = F) + theme(axis.text = element_text(size = 12))  


interaction_plott <- ggplot(reaction_time_m,
                            aes(x = noun_freq,
                                y = log_RT,
                                color = ItemType)) +
  geom_point(size = .9,
             alpha = .3) +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_color_brewer(type = "qual", 
                     palette = 3) +
  labs(x = "Collocation frequency",
       y= "log response time",
       color = "Language")
interaction_plott
#Mixed-effect model 5 #Log Dice

mod3_max <- lmer(log_RT ~ Language + LDscores + OrderofOccurrence_cat + Lem_Noun_Freq + Adject_freq + noun_freq + Length_centered + Language*LDscores + Language*Adject_freq + Language*noun_freq + LDscores*Adject_freq + LDscores*noun_freq + 
                   Lem_Noun_Freq*Language + LDscores*Lem_Noun_Freq + (1+LDscores|Participant) + (1+Adject_freq|Participant) + (1+noun_freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod3_max) #maximal model
#Mixed-effect model 5 - step-by-step-model comparison

mod3_max <- lmer(log_RT ~ Language + LDscores + OrderofOccurrence_cat + Lem_Noun_Freq + Adject_freq + noun_freq + Length_centered + Language*LDscores + Language*Adject_freq + Language*noun_freq + LDscores*Adject_freq + LDscores*noun_freq + 
                   Lem_Noun_Freq*Language + LDscores*Lem_Noun_Freq + (1+LDscores|Participant) + (1+Adject_freq|Participant) + (1+noun_freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod3_max)

mod3_1 <- lmer(log_RT ~ Language + LDscores + OrderofOccurrence_cat + Lem_Noun_Freq + Adject_freq + noun_freq + Length_centered + Language*LDscores + Language*Adject_freq + Language*noun_freq + LDscores*Adject_freq + 
                 Lem_Noun_Freq*Language + LDscores*Lem_Noun_Freq + (1+LDscores|Participant) + (1+Adject_freq|Participant) + (1+noun_freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod3_1)

mod3_2 <- lmer(log_RT ~ Language + LDscores + Lem_Noun_Freq + Adject_freq + noun_freq + Length_centered + Language*LDscores + Language*Adject_freq + Language*noun_freq + LDscores*Adject_freq + 
                 Lem_Noun_Freq*Language + LDscores*Lem_Noun_Freq + (1+LDscores|Participant) + (1+Adject_freq|Participant) + (1+noun_freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod3_2)

mod3_3 <- lmer(log_RT ~ Language + LDscores + Lem_Noun_Freq + Adject_freq + noun_freq + Length_centered + Language*LDscores + Language*noun_freq + LDscores*Adject_freq + 
                 Lem_Noun_Freq*Language + LDscores*Lem_Noun_Freq + (1+LDscores|Participant) + (1+Adject_freq|Participant) + (1+noun_freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod3_3)

mod3_4 <- lmer(log_RT ~ Language + LDscores + Lem_Noun_Freq + Adject_freq + noun_freq + Length_centered + Language*LDscores + Language*noun_freq + LDscores*Adject_freq + 
                 LDscores*Lem_Noun_Freq + (1+LDscores|Participant) + (1+Adject_freq|Participant) + (1+noun_freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod3_4)

mod3_5 <- lmer(log_RT ~ Language + LDscores + Adject_freq + noun_freq + Length_centered + Language*LDscores + Language*noun_freq + LDscores*Adject_freq + 
                 LDscores*Lem_Noun_Freq + (1+LDscores|Participant) + (1+Adject_freq|Participant) + (1+noun_freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod3_5)

mod3_6 <- lmer(log_RT ~ Language + LDscores + Adject_freq + noun_freq + Length_centered + Language*LDscores + Language*noun_freq + LDscores*Adject_freq + 
                 (1+LDscores|Participant) + (1+Adject_freq|Participant) + (1+noun_freq|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod3_6)

mod3_7 <- lmer(log_RT ~ Language + LDscores + Adject_freq + noun_freq + Length_centered + Language*LDscores + Language*noun_freq + LDscores*Adject_freq + 
                 (1+LDscores|Participant) + (1+noun_freq|Participant) + (1|item), data = reaction_time_m, control = lmerControl(calc.derivs = FALSE))
summary(mod3_7)

mod3_8 <- lmer(log_RT ~ Language + LDscores + Adject_freq + noun_freq + Length_centered + Language*LDscores + Language*noun_freq + LDscores*Adject_freq + 
                 (1+LDscores|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod3_8)

mod3_9 <- lmer(log_RT ~ Language + LDscores + Adject_freq + noun_freq + Length_centered + Language*noun_freq + Language*LDscores +
                 (1+LDscores|Participant) + (1|item), data = reaction_time_m, REML = FALSE)
summary(mod3_9)
options(scipen = 999)

mod3_10 <- lmer(log_RT ~ Language + LDscores + Adject_freq + noun_freq + Length_centered + Language*noun_freq + 
                  (1+LDscores|Participant) + (1|item), data = reaction_time_m, control = lmerControl(calc.derivs = FALSE))
summary(mod3_10)
CI_mod3_10 <- confint(mod3_10, method = "Wald")
CI_mod3_10



