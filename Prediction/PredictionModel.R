## Loading H2o Library

library(h2o)
h2o.init(nthreads = -1, max_mem_size = "8G")

## Loading the data
data <- read.csv("./PredictionData.csv",na.strings=c(""," ","NA"))
data$satisfied <- as.numeric(data$satisfied)

## Feature engineering
##
# if the occupation and partner's occupation are the same
data$v271 <- 0
# Ratio of the number of hours worked by them and their partner v251 and v252
data$v272 <- 0
# Are they both employed the same way
data$v273 <- 0
# Do they both have same higher education?
data$v274 <- 0
# Did the person every changed their religion?
data$v275 <- 0
# time supplementary questinaire
data$v276 <- as.Date("1.1.0001",format = "%d.%m.%Y")
# Start of the interview
data$v277 <- as.Date("1.1.0001",format = "%d.%m.%Y")
# End of the interview
data$v278 <- as.Date("1.1.0001",format = "%d.%m.%Y")


for (i in 1:NROW(data)){
  data$v271[i] = data$v150[i] == data$v151[i]
  
  if (data$v251[i] == '.a' | data$v251[i] == '.b' | 
      data$v251[i] == '.c' | data$v251[i] == '.d' | 
      data$v251[i] == '.' | is.na(data$v251[i])){
    if (data$v252[i] == '.a' | data$v252[i] == '.b' | 
        data$v252[i] == '.c' | data$v252[i] == '.d' | 
        data$v252[i] == '.' | is.na(data$v252[i])){
      data$v272[i] = NA
    }
  } else{
    data$v272[i] = as.integer(data$v251[i]) / as.integer(data$v252[i])
  }
  
  data$v273[i] = data$v70[i] == data$v71[i]
  
  data$v274[i] = data$v65[i] == data$v68[i]
  
  data$v275[i] = data$v190[i] == data$v191[i]
  
  data$v276[i] = as.Date(paste(toString(data$v228[i]),toString(data$v229[i]) ,
                               toString(data$v230[i]),sep = "."),format = "%d.%m.%Y")
  
  data$v277[i] = as.Date(paste(toString(data$v125[i]),toString(data$v129[i]) ,
                               toString(data$v134[i]),sep = "."),format = "%d.%m.%Y")
  
  data$v278[i] = as.Date(paste(toString(data$v124[i]),toString(data$v128[i]) ,
                               toString(data$v133[i]),sep = "."),format = "%d.%m.%Y")
}

dataH2o <- as.h2o(data)

splits <- h2o.splitFrame(data = dataH2o, 
                         ratios = c(0.7, 0.15),  #partition data into 70%, 15%, 15% chunks
                         seed = 2020)

train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]

x <- setdiff(names(dataH2o),c("id","satisfied"))
y <- "satisfied"

######################################
glm_fit1 <- h2o.glm(x = x,
                   y = y,
                   training_frame = train,
                   model_id = "glm_fit1",
                   family = "gaussian",
                   lambda_search = TRUE)
glm_perf1 <- h2o.performance(model = glm_fit1,
                            newdata = train)
glm_perf1
######################################
rf_fit1 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train,
                            model_id = "rf_fit1",
                            ntrees = 100,
                            max_depth = 10,
                            seed = 2020)
rf_perf1 <- h2o.performance(model = rf_fit1,
                            newdata = train)
rf_perf1
######################################
dl_fit1 <- h2o.deeplearning(x = x,
                             y = y,
                             training_frame = train,
                             model_id = "dl_fit1",
                             seed = 2020)
dl_perf1 <- h2o.performance(model = dl_fit1,
                            newdata = train)
dl_perf1

######################################
p1 <- predict(glm_fit1,new = valid[,(colnames(valid) %in% x)])
p2 <- predict(rf_fit1,new = valid[,(colnames(valid) %in% x)])
p3 <- predict(dl_fit1,new = valid[,(colnames(valid) %in% x)])

trueValue <- as.h2o(valid$satisfied)
prediction <- h2o.cbind(trueValue, p1$predict,p2$predict,p3$predict)

newModel <- h2o.glm(x = c("predict","predict0","predict1"),
                    y = "satisfied",
                    training_frame = prediction,
                    model_id = "glm_fitNew",
                    family = "gaussian",
                    lambda_search = TRUE)

#summary(newModel)
######################################
pN1 <- predict(glm_fit1,new = test[,(colnames(test) %in% x)])
pN2 <- predict(rf_fit1,new = test[,(colnames(test) %in% x)])
pN3 <- predict(dl_fit1,new = test[,(colnames(test) %in% x)])

prediction2 <- h2o.cbind(pN1$predict,pN2$predict,pN3$predict)
testPrediction <- predict(newModel,new = prediction2)

final <- testPrediction
final[final <= 0.5] <- 0
final[final > 0.5] <- 1
  
result <-  h2o.cbind(test$satisfied,final)
df = as.data.frame(result)

sum = 0
for (i in 1:NROW(df)){
  if (df[i,1] == df[i,2]){
    sum = sum + 1
  }
}
sum/NROW(df)
  
#0.7113897
#0.1574863
#0.1311239

