##### import csv file ##### 
df = read.csv(file = "mental-heath-in-tech-2016_20161114.csv")
sort(df$What.is.your.age.)

##### eliminate age rows ##### 
df <- df[(df$What.is.your.age. != 3), ]
df <- df[(df$What.is.your.age. != '99'), ]
df <- df[(df$What.is.your.age. != '323'), ]
dim(df)

##### recode gender ##### 
male   = c("Male", "male", "M", "Male ", "m", "man", "Cis male", "Male.", "Male (cis)", "Man", "Sex is male", 
          "cis male", "Malr", "Dude", "I'm a man why didn't you make this a drop down question. You should of asked sex? And I would of answered yes please. Seriously how much text can this take? ", 
          "mail", "M|", "male ", "Cis Male", "Male (trans, FtM)", "cisdude", "cis man", "MALE") 


female = c("Female", "female", "I identify as female.", "female ", "F", "Woman", "fm", "f", "Cis female ", "Transitioned, M2F", 
          "Female ", "woman", "female/woman", "Cisgender Female", "mtf", "fem", 
          "Female (props for making this a freeform field, though)", " Female", "Cis-woman")                                                                                                                                                                                                                                                                                                     

other  = c("Bigender", "non-binary", "Female assigned at birth ", "Genderfluid (born female)", "Other/Transfeminine",
          "Female or Multi-Gender Femme", "Androgynous", "male 9:1 female, roughly", "N/A", "nb masculine", "none of your business", 
          "genderqueer", "Human", "Genderfluid", "Enby", "genderqueer woman", "Queer", "Agender", "Fluid", "Male/genderqueer", "Nonbinary", 
          "human", "Unicorn", "Genderqueer", "Genderflux demi-girl", "female-bodied; no feelings about gender", "", 
          "AFAB", "Transgender woman","Other")  

##### Recode gender ##### 
vect_sexe = c()
for (sexe in df$What.is.your.gender.) {
  if (is.element(sexe, male)){
    vect_sexe <- c(vect_sexe, 'male')
  }
  else if (is.element(sexe, female)){
    vect_sexe <- c(vect_sexe, 'female')
    
  }  
  else if (is.element(sexe, other)){
    vect_sexe <- c(vect_sexe, 'other')
    
  }
}
df$gender_recode = vect_sexe
table(df$gender_recode)

##########  2. Clustering sur les entreprises ##########  
df_entr = df[, c("Do.you.think.that.discussing.a.mental.health.disorder.with.your.employer.would.have.negative.consequences.",
                 "Does.your.employer.provide.mental.health.benefits.as.part.of.healthcare.coverage.",
                 "Does.your.employer.offer.resources.to.learn.more.about.mental.health.concerns.and.options.for.seeking.help.",
                 "Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health.",
                 "Has.your.employer.ever.formally.discussed.mental.health..for.example..as.part.of.a.wellness.campaign.or.other.official.communication..",
                 "Do.you.know.the.options.for.mental.health.care.available.under.your.employer.provided.coverage.",
                 "Is.your.anonymity.protected.if.you.choose.to.take.advantage.of.mental.health.or.substance.abuse.treatment.resources.provided.by.your.employer.",
                 "Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers.",                                                                                             
                 "Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.direct.supervisor.s..",
                 "Have.you.heard.of.or.observed.negative.consequences.for.co.workers.who.have.been.open.about.mental.health.issues.in.your.workplace.",
                 "Do.you.think.that.team.members.co.workers.would.view.you.more.negatively.if.they.knew.you.suffered.from.a.mental.health.issue.")]

##### Encodage des données ##### 
install.packages("caret")
library(caret)
dummy_vars = dummyVars(" ~ .", data=df_entr)
new_df_entr = data.frame(predict(dummy_vars, newdata=df_entr))

##### K-Means Clustering ##### 
nb_cluster_entr = 3
fit = kmeans(new_df_entr, nb_cluster_entr) # 3 clusters
list_cluster_entr = fit$cluster # Associe chaque entreprise à un cluster

##### PCA ##### 
# Pour pouvoir visualiser le résultat du clustering sur un graphique.
pca=prcomp(new_df_entr)
summary(pca)
two_dims_entr = pca$x[, c('PC1','PC2')] # Préserve seulement 41.61% de la variance totale

# Moyenne cluster 1 :
summary_1 = table(df_entr[list_cluster_entr==1, c("Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.direct.supervisor.s..",
                                                  "Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers.",
                                                  "Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health.")])
summary_1 # Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.direct.supervisor.s.. = No/I don't know
# Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers. = No
# Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health. = No

# Moyenne cluster 2 :
summary_2 = table(df_entr[list_cluster_entr==2, c("Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.direct.supervisor.s..",
                                                  "Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers.",
                                                  "Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health.")])
summary_2 # Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.direct.supervisor.s.. = Yes
# Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers. = Yes
# Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health. = Yes

# Moyenne cluster 3 :
summary_3 = table(df_entr[list_cluster_entr==3, c("Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.direct.supervisor.s..",
                                                  "Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers.",
                                                  "Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health.")])
summary_3 # Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.direct.supervisor.s.. = Maybe/No
# Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers. = Maybe/No
# Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health. = I don't know/No

table(list_cluster_entr)

# Supr données manquantes
length(list_cluster_entr) # Avant :  1 430 lignes
df = df[list_cluster_entr != 1, ] # Cluster 1 correspond aux donées manquantes
dim(df) # Après :  1 143 lignes

df_entr = df[, c("Do.you.think.that.discussing.a.mental.health.disorder.with.your.employer.would.have.negative.consequences.",
                 "Does.your.employer.provide.mental.health.benefits.as.part.of.healthcare.coverage.",
                 "Does.your.employer.offer.resources.to.learn.more.about.mental.health.concerns.and.options.for.seeking.help.",
                 "Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health.",
                 "Has.your.employer.ever.formally.discussed.mental.health..for.example..as.part.of.a.wellness.campaign.or.other.official.communication..",
                 "Do.you.know.the.options.for.mental.health.care.available.under.your.employer.provided.coverage.",
                 "Is.your.anonymity.protected.if.you.choose.to.take.advantage.of.mental.health.or.substance.abuse.treatment.resources.provided.by.your.employer.",
                 "Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers.",                                                                                             
                 "Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.direct.supervisor.s..",
                 "Have.you.heard.of.or.observed.negative.consequences.for.co.workers.who.have.been.open.about.mental.health.issues.in.your.workplace.",
                 "Do.you.think.that.team.members.co.workers.would.view.you.more.negatively.if.they.knew.you.suffered.from.a.mental.health.issue.")]

##### Encodage des données ##### 
library(caret)
dummy_vars = dummyVars(" ~ .", data=df_entr)
new_df_entr = data.frame(predict(dummy_vars, newdata=df_entr))

##### K-Means Clustering ##### 
nb_cluster_entr = 2
fit = kmeans(new_df_entr, nb_cluster_entr) # 3 clusters
list_cluster_entr = fit$cluster # Associe chaque entreprise à un cluster

##### PCA ##### 
# Pour pouvoir visualiser le résultat du clustering sur un graphique.
pca=prcomp(new_df_entr)
summary(pca)
two_dims_entr = pca$x[, c('PC1','PC2')] # Préserve seulement 26% de la variance totale

# Moyenne cluster 1 :
summary_1 = table(df_entr[list_cluster_entr==1, c("Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.direct.supervisor.s..",
                                                  "Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers.",
                                                  "Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health.")])
summary_1 # Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.direct.supervisor.s.. = No/I don't know
# Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers. = No
# Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health. = No

# Moyenne cluster 2 :
summary_2 = table(df_entr[list_cluster_entr==2, c("Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.direct.supervisor.s..",
                                                  "Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers.",
                                                  "Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health.")])
summary_2 # Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.direct.supervisor.s.. = Yes
# Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers. = Yes
# Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health. = Yes

table(list_cluster_entr)

##### Légende des 2 clusters (pour le graphique) #####
labels_entr = c("On peut parler ouvertement de ses troubles de santé mentale",
                "Il est difficile de parler ouvertement de ses troubles de santé mentale")
labels_entr

##### Plot ##### 
plot(two_dims_entr, col=list_cluster_entr, cex=1, pch=1,
     main="Clustering des entreprises par rapport à leur vision des troubles mentaux\n(Réduction de la dimension avec une Analyse en Composantes Principales)",
     xlab = "Première composante principale",
     ylab = "Deuxième composante principale")
legend("topleft",legend=labels_entr, col=seq(nb_cluster_entr), pch=1, bty="n")

##### Recode cluster entreprise ##### 
vect_cluster_recode = c()
for (cluster in list_cluster_entr) {
  if (cluster == 1){
    vect_cluster_recode <- c(vect_cluster_recode, "On peut parler ouvertement de ses troubles de santé mentale")
  }
  else if (cluster == 2){
    vect_cluster_recode <- c(vect_cluster_recode, "Il est difficile de parler ouvertement de ses troubles de santé mentale")
    
  }
}
df$cluster_entr_recode = vect_cluster_recode
df$cluster_entr_brute = list_cluster_entr
table(df$cluster_entr_recode)
table(df$cluster_entr_brute)

##########  1. Clustering sur les individus ########## 
df_indi = df[, c("Do.you.have.a.family.history.of.mental.illness.",
                 "Have.you.had.a.mental.health.disorder.in.the.past.",                                                                                                                             
                 "Do.you.currently.have.a.mental.health.disorder.",                                                                                                                     
                 "If.yes..what.condition.s..have.you.been.diagnosed.with.",                                                                                                                      
                 "If.maybe..what.condition.s..do.you.believe.you.have.")]

##### Encodage des données #####
library(caret)
dummy_vars = dummyVars(" ~ .", data=df_indi)
new_df_indi = data.frame(predict(dummy_vars, newdata=df_indi))

##### K-Means Clustering ##### 
nb_cluster_ind = 3
fit = kmeans(new_df_indi, nb_cluster_ind) # 3 clusters
list_cluster_ind = fit$cluster # Associe chaque individu à un cluster

##### PCA ##### 
# Pour pouvoir visualiser le résultat du clustering sur un graphique.
pca=prcomp(new_df_indi)
summary(pca)
two_dims_ind = pca$x[, c('PC1','PC2')]  # PC1&2 préserve 0.5341 de la variance totale

##### Moyenne cluster 1 ##### 
summary_1 = table(df[list_cluster_ind==1, c("Do.you.currently.have.a.mental.health.disorder.", 
                                            "Do.you.have.a.family.history.of.mental.illness.",
                                            "Have.you.had.a.mental.health.disorder.in.the.past.")])
summary_1 # Currently have a mental health disorder : Maybe
# Family history of mental illness : Yes
# Have.you.had.a.mental.health.disorder.in.the.past. = Maybe/Yes

##### Moyenne cluster 2 ##### 
summary_2 = table(df[list_cluster_ind==2, c("Do.you.currently.have.a.mental.health.disorder.", 
                                            "Do.you.have.a.family.history.of.mental.illness.",
                                            "Have.you.had.a.mental.health.disorder.in.the.past.")])
summary_2 # Currently have a mental health disorder : Yes 
# Family history of mental illness : Yes
# Have.you.had.a.mental.health.disorder.in.the.past. = Yes

##### Moyenne cluster 3 ##### 
summary_3 = table(df[list_cluster_ind==3, c("Do.you.currently.have.a.mental.health.disorder.", 
                                            "Do.you.have.a.family.history.of.mental.illness.",
                                            "Have.you.had.a.mental.health.disorder.in.the.past.")])
summary_3 # Currently have a mental health disorder : Yes 
# Family history of mental illness : Yes
# Have.you.had.a.mental.health.disorder.in.the.past. = Yes

##### Légende des 3 clusters (pour le graphique) #####
labels_ind = c("Troubles graves",
               "Problèmes légers",
               "Bonne santé mentale")
labels_ind

##### Plot ##### 
plot(two_dims_ind, col=list_cluster_ind, cex=1, pch=1,
     main="Clustering des individus par rapport à leurs troubles mentaux\n(Réduction de la dimension avec une Analyse en Composantes Principales)",
     xlab = "Première composante principale",
     ylab = "Deuxième composante principale")
legend("topleft",legend=labels_ind, col=seq(nb_cluster_ind), pch=1, bty="n")                           

# Recode 
vect_cluster_recode = c()
for (cluster in list_cluster_ind) {
  if (cluster == 1){
    vect_cluster_recode <- c(vect_cluster_recode, "Troubles graves")
  }
  else if (cluster == 2){
    vect_cluster_recode <- c(vect_cluster_recode, "Problèmes légers")
  }
  else {
    vect_cluster_recode <- c(vect_cluster_recode, "Bonne santé mentale")
  }
}
df$cluster_ind_recode = vect_cluster_recode
df$cluster_ind_brute = list_cluster_ind
table(df$cluster_ind_recode)
table(df$cluster_ind_brute)

# Save
#write.csv(df,"survey.csv", row.names = TRUE)

test = read.csv(file = "survey.csv")
dim(test)
