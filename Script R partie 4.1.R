##### Import du sondage ##### 
df = read.csv(file = 'survey.csv')
dim(df) # 1 143 réponses/individus, 69 variables
#colnames(df)

##### Tester la taille des entreprises ##### 
taille_entreprise = df$How.many.employees.does.your.company.or.organization.have.

contingence_taille_cluster = table(taille_entreprise, df$cluster_ind_recode)
contingence_taille_cluster

test_taille_entreprise = chisq.test(contingence_taille_cluster)
test_taille_entreprise

##### Graphique  ##### 
mosaicplot(contingence_taille_cluster)
mosaicplot(contingence_taille_cluster, main = NULL ,xlab = c("état de santé mentale"), ylab = c("taille des entreprises"))

##### Tester le travail à distance ##### 
work_remotely = df[,c("Do.you.work.remotely.")]

contingence_work_remotel_cluster = table(work_remotely, df$cluster_ind_recode)
contingence_work_remotel_cluster

contingence_work_remotely_cluster = table(work_remotely, df$cluster_ind_recode)
contingence_work_remotely_cluster

test_work_remotely = chisq.test(contingence_work_remotely_cluster)
test_work_remotely

##### Relation age santé mentale ##### 
age = df$What.is.your.age.

##### Graphique ##### 
boxplot(age ~ df$cluster_ind_recode, ylab = c("état de santé mentale"))

##### Rregrouper l'âge en trois catégories ##### 
vect_age = c()
for (age in df$What.is.your.age.){
    {if(age < 30){vect_age <- c(vect_age, 30)}
                else if((age < 40) & (30 <= age)){vect_age <- c(vect_age, 40)}
                   else {vect_age <- c(vect_age, 60)}}
}  

##### Tester la relation age santé mentale ##### 
contingence_age_cluster = table(vect_age, df$cluster_ind_recode)
contingence_age_cluster

test_age = chisq.test(contingence_age_cluster)
test_age
 
##### Tester le genre ##### 
contingence_gender_cluster = table(df$gender_recode , df$cluster_ind_recode)
contingence_gender_cluster

test_gender = chisq.test(contingence_gender_cluster)
test_gender

##### Matrice d'attraction/répulsion ##### 
attraction_gender = (test_gender$observed)/(test_gender$expected)
attraction_gender