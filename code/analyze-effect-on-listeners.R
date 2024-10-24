## ---------------------------------------------------------------------
# LOADING PACKAGES
## ---------------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(readxl)
library(lme4)

## ---------------------------------------------------------------------
# LOADING DATA
## ---------------------------------------------------------------------
d.sim <- 
  read_excel("code/similarities_with_w2vacc.xlsx") %>%
  filter(
    Experiment == "1a",
    PartOfExp == "test") %>%
  rename(ExposureTalkerID = TrainingTalkerID) %>%
  group_by(SentenceID) %>%
  mutate(
    # Recode exposure talker ID, so that it's *not* sensitive to the order of exposure
    # but does capture whenever the exposure talker(s) differed.
    ExposureTalkerID = case_when(
      Condition2 == "Single talker" ~ ExposureTalkerID, 
      Condition2 %in% c("Multi-talker", "Talker-specific") ~ paste("Exposure for", Condition2, "- TestTalkerID", TestTalkerID),
      T ~ paste("Exposure for", Condition2)),
    KeywordID = as.factor(as.numeric(as.factor(Keyword)))) %>%
  # Create shorter unique label for ExposureTalkerID
  group_by(Condition) %>%
  mutate(ExposureTalkerID = LETTERS[as.numeric(as.factor(ExposureTalkerID))]) %>%
  ungroup() %>%
  mutate(     
    ProportionDiphoneOverlap = diphone_overlapped / NumDiphone_word,
    # Make sure ID variables are factors
    across(
      c(WorkerID, ExposureTalkerID, TestTalkerID, SentenceID, KeywordID), 
      factor),
    Condition = factor(Condition2, levels = c("Control", "Single talker", "Multi-talker", "Talker-specific")))

## ---------------------------------------------------------------------
# SANITY CHECKS
## ---------------------------------------------------------------------

# We first check that the similarity values meet basic checks.
# Check that similarity is identical for each unique combination of condition, exposure talker, test
# talker, sentence and keyword.
d.sim %>%
  group_by(Condition, ExposureTalkerID, TestTalkerID, SentenceID, KeywordID) %>%
  filter(sim_mean_max != first(sim_mean_max))

# Check that the similarity differs even within test talker, but only for the single talker condition
# SOMETHING STILL LOOKS POTENTIALLY OFF HERE: while there are now some keywords that have more than 1
# similarity value for each keyword and test talker, *most* keywords seem to have only one unique 
# similarity value per test talker even in the single talker condition (where similarity should depend
# on the exposure talker, of which there were five different ones per test talker)
d.sim %>%
  group_by(Condition, TestTalkerID, SentenceID, KeywordID) %>%
  summarise(n = length(unique(sim_mean_max))) %>%
  filter(n > 1) %>%
  View()

## ---------------------------------------------------------------------
# VISUALIZING SIMILARITY AND ITS RELATION TO DIPHONE OVERLAP
## ---------------------------------------------------------------------

# Correlation between similarity and diphone overlap
d.sim %>%
  group_by(Condition, ExposureTalkerID, TestTalkerID, SentenceID, KeywordID) %>%
  summarise(across(c(sim_mean_max, ProportionDiphoneOverlap), mean)) %>%
  mutate(
    TestTalkerID = paste("Test talker", TestTalkerID),
    Condition = ifelse(Condition == "Single talker", paste(Condition, ExposureTalkerID, sep = "- Exp"), as.character(Condition))) %>%
  ungroup() %>%
  ggplot(aes(x = ProportionDiphoneOverlap, y = sim_mean_max)) +
  geom_point(alpha = .1, size = .5) +
  geom_text(
    data = . %>% group_by(Condition, TestTalkerID) %>% summarise(r2 = round(cor(ProportionDiphoneOverlap, sim_mean_max)^2 * 100, 1)), 
    aes(label = paste0("R2=", r2, "%")), 
    x = .01, y = .01, size = 2, hjust = 0, vjust = 0) +
  facet_grid(Condition ~ TestTalkerID) +
  theme_bw()

# Summarize distribution of similarity and diphone overlap by condition and test talker
d.sim %>%
  group_by(Condition, ExposureTalkerID, TestTalkerID, SentenceID, KeywordID) %>%
  summarise(across(c(sim_mean_max, ProportionDiphoneOverlap), mean)) %>%
  ggplot() +
  geom_histogram(aes(x = sim_mean_max), fill = "blue", alpha = .5, color = NA) +
  geom_histogram(aes(x = ProportionDiphoneOverlap), fill = "red", alpha = .5, color = NA) +
  facet_grid(TestTalkerID ~ Condition) +
  theme_bw()

## ---------------------------------------------------------------------
# ANALYSES
## ---------------------------------------------------------------------

# Code predictors for analysis
# Center all similarity measures (to reduce collinearity in case we want
# to include interactions of these predictors with other predictors).
d.sim %<>% mutate(
  across(
    c(ProportionDiphoneOverlap, contains("sim_mean")),
    ~ .x - mean(.x, na.rm = T)))
contrasts(d.sim$Condition) <- MASS::contr.sdif(4)
colnames(contrasts(d.sim$Condition)) <- c("ST.vs.CTRL", "MT.vs.ST", "TS.vs.MT")

# A (conservative) baseline model that accounts for variation in accuracy by participant
# sentence and keyword (nested in sentence)
m.baseline <- 
  glmer(
    formula = IsCorrect ~ 1 + (1 | WorkerID) + (1 | SentenceID / KeywordID), 
    data = d.sim, 
    family = binomial)

# Replicating Xie et al (2021) in a frequentist GLMM by adding the exposure condition to the
# baseline model
m.replication <- 
  glmer(
    formula = IsCorrect ~ 1 + Condition + (1 | WorkerID) + (1 | SentenceID / KeywordID), 
    data = d.sim, 
    family = binomial)

# Condition has significant effects so that (1) single talker exposure leads to better transcription
# accuracy during test than control exposure, (2) multi-talker exposure leads to numerically but not 
# significantly better accuracy than single talker exposure, and (3) talker-specific exposure leads 
# to significantly better accuracy than multi-talker exposure. 
#
# As would be expected if condition explains why some participants performed better/worse during test,
# the estimated variance of cross-participant differences reduced once exposure condition is included
# in the model.
summary(m.replication)
# Additionally, the inclusion of exposure condition overall significantly improves the deviance explained.
anova(m.replication, m.baseline)

# Does similarity from test tokens to exposure token predict how accurately a keyword is transcribed 
# during test? To this end, we used the wav2vec model to calculate for each test keyword how similar 
# its diphone components were (on average) to the maximally similar diphones observed during exposure.
m.test_to_exposure_similarity <- 
  glmer(
    formula = IsCorrect ~ 1 + sim_mean_max + 
      (1 | WorkerID) + (1 | SentenceID / KeywordID), 
    data = d.sim, 
    family = binomial)

summary(m.test_to_exposure_similarity)

# How would we test whether these effects are purely caused by diphone overlap?
# GIVEN THE HIGH CORRELATIONS BETWEEN SIMILARITY AND DIPHONE OVERLAP, IT'S NOT 
# POSSIBLE TO TEASE THOSE EFFECTS APART IN THE MODEL, BUT WE CAN CHECK WHETHER
# SIMILARITY STILL MATTERS WHEN ONLY CASE WITH 100% DIPHONE OVERLAP ARE INCLUDED
# IN THE ANALYSIS
glmer(
  formula = IsCorrect ~ 1 + sim_mean_max + ProportionDiphoneOverlap + 
    (1 | WorkerID) + (1 | SentenceID / KeywordID), 
  data = d.sim, 
  family = binomial) %>%
  summary()

# Similarity still has a very strong effect if only keywords with 100% diphone 
# overlap are considered.
glmer(
  formula = IsCorrect ~ 1 + sim_mean_max + 
    (1 | WorkerID) + (1 | SentenceID / KeywordID), 
  data = d.sim %>% filter(ProportionDiphoneOverlap == max(ProportionDiphoneOverlap)), 
  family = binomial) %>%
  summary()


# Does similarity explain the effects of exposure condition? YES, BUT WHAT IS ODD
# IS THAT THE EFFECT OF SIMILARITY SEEMS TO BE LARGELY ORTHOGONAL TO (INDEPENDENT
# OF) THE EFFECT OF EXPOSURE CONDITION. WE WOULD EXPECT EXPOSURE CONDITION TO LEAD
# TO DIFFERENCES IN THE SIMILARITY BETWEEN TEST KEYWORDS AND EXPOSURE.
m.similarity_vs_condition <- 
  glmer(
    formula = IsCorrect ~ 1 + sim_mean_max + Condition +
      (1 | WorkerID) + (1 | SentenceID / KeywordID), 
    data = d.sim, 
    family = binomial)

summary(m.similarity_vs_condition)
anova(m.similarity_vs_condition, m.test_to_exposure_similarity)

# Does similarity explain the effects of unique exposure-test talker combinations?
m.similarity_vs_exposure_test_talker_combinations <- 
  glmer(
    formula = IsCorrect ~ 1 + sim_mean_max + Condition +
      (1 | WorkerID) + (1 | SentenceID / KeywordID) + (1 | TestTalkerID / ExposureTalkerID), 
    data = d.sim, 
    family = binomial, 
    # Deal with convergence issues: better but slower optimizer
    control = glmerControl(optimizer = c("bobyqa"))) 

summary(m.similarity_vs_exposure_test_talker_combinations)
anova(m.similarity_vs_exposure_test_talker_combinations, m.test_to_exposure_similarity)
