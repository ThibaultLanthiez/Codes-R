library(ggplot2)
library(tidyr)
library(dplyr)
library(foreign)
library(reshape2)
library(broom)
library(plotly)
library(rmarkdown)
library(jtools)
library(haven)
library(robustbase)
library(plyr)
library(waffle)
library(ggeffects)
library(openxlsx)

##### Import du sondage ##### 
df = read.csv(file = 'survey.csv')
dim(df) # 1 143 réponses/individus, 69 variables
#colnames(df)

##### Graphiques partie 1 ##### 

##### Avez-vous eu des troubles de santé mentale dans le passé ? ##### 
q1.1 <- as.data.frame(table(df$Have.you.had.a.mental.health.disorder.in.the.past.)) %>%
  mutate(perc = Freq/sum(Freq)) %>%
  dplyr::rename(Réponse = Var1) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(q1.1, aes(x = "", y = Freq, fill = Réponse)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")

##### Avez-vous actuellement des troubles de santé mentale ? ##### 
q1.2 <- as.data.frame(table(df$Do.you.currently.have.a.mental.health.disorder.)) %>%
  mutate(perc = Freq/sum(Freq)) %>%
  dplyr::rename(Réponse = Var1) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(q1.2, aes(x = "", y = Freq, fill = Réponse)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")

##### Avez-vous été diagnostiqué de troubles mentaux par un spécialiste médical? ##### 
q1.3 <- as.data.frame(table(df$Have.you.been.diagnosed.with.a.mental.health.condition.by.a.medical.professional.)) %>%
  mutate(perc = Freq/sum(Freq)) %>%
  dplyr::rename(Réponse = Var1) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(q1.3, aes(x = "", y = Freq, fill = Réponse)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")

##### Avez-vous des antécédents familiaux de trouble de santé mentale ? ##### 
q1.4 <- as.data.frame(table(df$Do.you.have.a.family.history.of.mental.illness.)) %>%
  mutate(perc = Freq/sum(Freq)) %>%
  dplyr::rename(Réponse = Var1) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(q1.4, aes(x = "", y = Freq, fill = Réponse)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")

##### Graphiques partie 2 ##### 

##### Pensez-vous que parler de vos troubles mentaux à votre employeur peut avoir un impact négatif ? ##### 
q2.1 <- as.data.frame(table(df$Do.you.think.that.discussing.a.mental.health.disorder.with.your.employer.would.have.negative.consequences.)) %>%
  mutate(perc = Freq/sum(Freq)) %>%
  dplyr::rename(Réponse = Var1) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(q2.1, aes(x = "", y = Freq, fill = Réponse)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")

##### Est-ce que votre entreprise apporte du soutien contre les troubles de santé mentale en plus de la couverture santé ? ##### 
q2.2 <- as.data.frame(table(df$Does.your.employer.provide.mental.health.benefits.as.part.of.healthcare.coverage.)) %>%
  mutate(perc = Freq/sum(Freq)) %>%
  dplyr::rename(Réponse = Var1) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(q2.2, aes(x = "", y = Freq, fill = Réponse)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")

##### Votre employeur a t'il fait des campagnes de sensibilisation aux troubles de santé mentale au sein de l’entreprise ?##### 
q2.3 <- as.data.frame(table(df$Has.your.employer.ever.formally.discussed.mental.health..for.example..as.part.of.a.wellness.campaign.or.other.official.communication..)) %>%
  mutate(perc = Freq/sum(Freq)) %>%
  dplyr::rename(Réponse = Var1) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(q2.3, aes(x = "", y = Freq, fill = Réponse)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")

##### Auriez-vous du mal à annoncer à votre employeur que vous devez avoir un arrêt de travail pour des causes de santé mentale ? ##### 
q2.4 <- as.data.frame(table(df$Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.direct.supervisor.s..)) %>%
  mutate(perc = Freq/sum(Freq)) %>%
  dplyr::rename(Réponse = Var1) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(q2.4, aes(x = "", y = Freq, fill = Réponse)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")

##### Étiez-vous au courant s’il existait un accompagnement contre les troubles mentaux dans votre ancienne entreprise ? ##### 
q2.5 <- as.data.frame(table(df$Were.you.aware.of.the.options.for.mental.health.care.provided.by.your.previous.employers.)) %>%
  mutate(perc = Freq/sum(Freq)) %>%
  dplyr::rename(Réponse = Var1) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

names(q2.5)

q2.5[q2.5 == "NA"] <- "no response"

ggplot(q2.5, aes(x = "", y = Freq, fill = Réponse)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")

##### Créer la table de contingence ##### 
ab = table(df$Has.your.employer.ever.formally.discussed.mental.health..for.example..as.part.of.a.wellness.campaign.or.other.official.communication.., 
           df$Does.your.employer.provide.mental.health.benefits.as.part.of.healthcare.coverage.)
ab

##### Tester l'indépendance entre les deux variables #####  
chisq.test(ab)
summary(ab)

##### Créer la table de contingence ##### 
cd = table(df$Does.your.employer.provide.mental.health.benefits.as.part.of.healthcare.coverage., 
           df$Do.you.think.that.discussing.a.mental.health.disorder.with.your.employer.would.have.negative.consequences.)
cd

##### Tester l'indépendance entre ces deux variables ##### 
chisq.test(cd)
summary(cd)

##### Créer la table de contingence ##### 
ef = table(df$Has.your.employer.ever.formally.discussed.mental.health..for.example..as.part.of.a.wellness.campaign.or.other.official.communication.., 
           df$Do.you.think.that.discussing.a.mental.health.disorder.with.your.employer.would.have.negative.consequences.)
ef

##### Tester l'indépendance entre ces deux variables ##### 
chisq.test(ef)
summary(ef)

##### Projection sur le plan factoriel ##### 
library(FactoMineR)
bca <- CA(ef)
