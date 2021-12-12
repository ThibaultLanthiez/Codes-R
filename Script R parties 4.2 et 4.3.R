##### Import du sondage ##### 
df = read.csv(file = 'survey.csv')
dim(df) # 1 143 réponses/individus, 69 questions
#colnames(df)

##### Analyse ##### 
table(df$cluster_entr_recode)
table(df$cluster_ind_recode)

table(df$cluster_ind_recode, df$cluster_entr_recode)

test = chisq.test(table(df$cluster_ind_recode, df$cluster_entr_recode))
test

#Indice attraction/répulsion
attraction=(test$observed)/(test$expected)
attraction

##### Modèle ##### 
df = read.csv(file = 'survey.csv')
y = df$Do.you.currently.have.a.mental.health.disorder.
x = df[, c("Do.you.have.a.family.history.of.mental.illness.",
           "What.is.your.age.",                                                                                                                            
           "Do.you.work.remotely.",
           "Are.you.self.employed.",
           "Do.you.think.that.discussing.a.mental.health.disorder.with.your.employer.would.have.negative.consequences.",
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
y_recode <- factor(y)
y_recode <- as.numeric(y_recode)
length(y_recode)

library(caret)
dummy_vars = dummyVars(" ~ .", data=x)
x_encode = data.frame(predict(dummy_vars, newdata=x))
dim(x_encode)

# Train/Test split
x_train  = x_encode[1:900,]
x_test = x_encode[901:1143,]
dim(x_train)
dim(x_test)

y_train  = y_recode[1:900]
y_test = y_recode[901:1143]
length(y_train)
length(y_test)

##### Decision Tree ##### 
library(rpart)
tree = rpart(y_train ~ ., 
             data = x_train, 
             method="class", 
             control = rpart.control(minsplit = 20, minbucket = 7, maxdepth = 10, usesurrogate = 2, xval =10 ))
plot(tree)
text(tree)
pred <- predict(tree, x_train, type = 'class') 
result = table(pred, y_train) # 58% 1143
result
sum(diag(result))/sum(result) #overall accuracy

##### KNN ##### 
# Installing Packages
#install.packages("e1071")
#install.packages("caTools")
#install.packages("class")

# Loading package
library(e1071)
library(caTools)
library(class)

pred <- knn(train = x_train, test = x_test,
              cl = y_train,
              k = 4)
result = table(pred, y_test) # 0.46%
result
sum(diag(result))/sum(result) #overall accuracy


for (i in 1:10) {
  pred <- knn(train = x_train, test = x_test,
              cl = y_train,
              k = i)
  result = table(pred, y_test) # 60.3%
  #result
  print(i)
  acc = sum(diag(result))/sum(result) #overall accuracy
  print(acc)
}
