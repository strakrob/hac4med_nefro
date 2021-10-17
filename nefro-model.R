library(dplyr)
library(randomForest)
library(pROC)

nef <- read.csv("nefro3.csv")

data <- data.frame(pct=nef$Prokalcytonina.ng.ml,kre=nef$Kreatynina,mio=nef$Mioglobina.ug.l, aki=nef$AKI.w.trakcie.hospitalizacji)
data <- data[!(data$pct%in% c(NA)),]
data <- data[!(data$kre%in% c(NA)),]
data <- data[!(data$mio%in% c(NA)),]
data <- data[!(data$aki%in% c(NA)),]
data <- transform(data,pct=as.numeric(pct),kre=as.numeric(kre),mio=as.numeric(kre),aki=as.factor(aki))

set.seed(101) 
sample <- sample.int(n = nrow(data), size = floor(.75*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]

dim(train)
dim(test)

rf <- randomForest(
  aki ~ .,
  data=train
)

pred = predict(rf, newdata=test[-4])
cm = table(test[,4], pred)
cm

predictions <- as.data.frame(predict(rf, newdata=test[-4],type='prob'))
roc.aki <- roc(test$aki, predictions$`1`)
plot(roc.aki, print.auc=TRUE)

