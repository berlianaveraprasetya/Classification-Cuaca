#Insert Dataset
#install.packages("readxl")
library("readxl")
datatesis <- read_xlsx("C:/Users/Asus/Downloads/Cuaca.xlsx",sheet=1)
datatesis$Suhu <- as.factor(datatesis$Suhu)
datatesis$KR <- as.factor(datatesis$KR)
datatesis$TA <- as.factor(datatesis$TA)
datatesis$PMSL <- as.factor(datatesis$PMSL)
datatesis$Label <- as.factor(datatesis$Label)
str(datatesis)

#Imputasi data
summary(is.na(datatesis)) # dbt=nama data penulis
p <- function(x){sum(is.na(x))/length(x)*100}
apply(datatesis, 2, p)

#install.packages("mice")
library(mice)
#langkah sleanjutnya adalah melakukan imputasi
impute <- mice(datatesis[,], m = 3, seed = 123)
print(impute)

dbt_impute<- complete(impute,1)
summary(is.na(dbt_impute))

#membagi data menjadi dua bagian: training dan testing
library(caret)
set.seed(1104)
acak <- createDataPartition(dbt_impute$Label, p=0.7, list = FALSE)
str(acak)
train <- dbt_impute[acak, ]
test <- dbt_impute[-acak, ]
summary(train)

#membangun model dengan algoritma adaboost
library(adabag)
model.adaboost <- boosting(Label~., data=train, 
                           mfinal=5, control=rpart.control(maxdepth=1),
                           coeflearn='Freund')

#menampilkan stump pertama
model.adaboost$trees[1]
#menampilkan bobot dari stump pertama
model.adaboost$weights[1]
#menampilkan stump kedua
model.adaboost$trees[2]
#menampilkan bobot dari stump kedua
model.adaboost$weights[2]
#menampilkan stump ketiga
model.adaboost$trees[3]
#menampilkan bobot dari stump ketiga
model.adaboost$weights[3]
#menampilkan stump keempat
model.adaboost$trees[4]
#menampilkan bobot dari stump keempat
model.adaboost$weights[4]
#menampilkan stump kelima
model.adaboost$trees[5]
#menampilkan bobot dari stump kelima
model.adaboost$weights[5]

maudiprediksi <- test[1,]
prob1 <- predict(model.adaboost$trees[1], maudiprediksi)
prob2 <- predict(model.adaboost$trees[2], maudiprediksi)
prob3 <- predict(model.adaboost$trees[3], maudiprediksi)
prob4 <- predict(model.adaboost$trees[4], maudiprediksi)
prob5 <- predict(model.adaboost$trees[5], maudiprediksi)

prediksi1 <- ifelse(prob1[[1]][1]>prob1[[1]][2],0,1)
prediksi2 <- ifelse(prob2[[1]][1]>prob2[[1]][2],0,1)
prediksi3 <- ifelse(prob3[[1]][1]>prob3[[1]][2],0,1)
prediksi4 <- ifelse(prob4[[1]][1]>prob4[[1]][2],0,1)
prediksi5 <- ifelse(prob5[[1]][1]>prob5[[1]][2],0,1)

bobot1 <- model.adaboost$weights[1]
bobot2 <- model.adaboost$weights[2]
bobot3 <- model.adaboost$weights[3]
bobot4 <- model.adaboost$weights[4]
bobot5 <- model.adaboost$weights[5]

hasil <- cbind(c(prediksi1, prediksi2, prediksi3, prediksi4, prediksi5),
               c(bobot1, bobot2, bobot3, bobot4, bobot5))
hasil

sumbobot.0 <- (1-prediksi1)*bobot1+
  (1-prediksi2)*bobot2+
  (1-prediksi3)*bobot3+
  (1-prediksi4)*bobot4+
  (1-prediksi5)*bobot5
sumbobot.1 <- prediksi1*bobot1+
  prediksi2*bobot2+
  prediksi3*bobot3+
  prediksi4*bobot4+
  prediksi5*bobot5

prediksifinal <- ifelse(sumbobot.0 > sumbobot.1, 0, 1)
c(sumbobot.0, sumbobot.1, prediksifinal)

prediksi.langsung <- predict(model.adaboost, maudiprediksi)$class
prediksi.langsung

prediksi.boost <- predict(model.adaboost, test)$class
prediksi.boost <- as.factor(prediksi.boost)
confusionMatrix(as.factor(prediksi.boost), 
                test$Label, positive = "Tidak Turun Hujan")

summary(test)
str(prediksi.boost)
