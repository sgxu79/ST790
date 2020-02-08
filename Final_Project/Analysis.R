library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggrepel)
library(tidyverse)
library(ggmap)
library(gridExtra)
library(mice)
library(xgboost)
library(mlr)
library(caret)
library(glmnet)
library(parallel)
library(parallelMap)
library(h2o)
library(randomForest)
library(foreach)
library(doRNG)
library(forecast)
library(ggpubr)

register_google(key = "AIzaSyAa3RgyHheO0XHXryS54dhtk9CbUq8Ktlc")
data_raw = read.csv(file="Data Science ZExercise_TRAINING_CONFIDENTIAL1.csv")

#Map raw data

dplot = within(data_raw,{
  Latitude = Latitude/1e6
  Longitude = Longitude/1e6
  SaleDollarCnt = (SaleDollarCnt - min(SaleDollarCnt))/(max(SaleDollarCnt)-min(SaleDollarCnt))
  TransMonth = factor(month(as.POSIXlt(TransDate, format="%m/%d/%Y")))
})

p = ggmap(get_googlemap(center = c(Longitude = -122.0, Latitude = 47.5),
                        zoom = 9, scale = 2,
                        maptype ='terrain',
                        color = 'color'))
p + geom_point(aes(x = Longitude, y = Latitude,  colour = SaleDollarCnt),
               data = dplot)+scale_color_gradientn(name = "Sale Price",colours = rev(rainbow(3))) + 
  labs(x="Longitude",y="Latitude") + theme(axis.text=element_text(size=12))

#Categorize the observations into cities

City = rep("Other",nrow(dplot))

for(i in 1:nrow(dplot)){
  if(dplot$Longitude[i] %between% c(-122.25,-122.21) & dplot$Latitude[i] %between% c(47.53,47.59)){
    City[i] = "Mercer Island"
  }
  if(dplot$Longitude[i] %between% c(-122.206,-122.11) & dplot$Latitude[i] %between% c(47.546,47.65)){
    City[i] = "Bellevue"
  }
  if(dplot$Longitude[i] %between% c(-122.43,-122.24) & dplot$Latitude[i] %between% c(47.50,47.73)){
    City[i] = "Seattle"
  }
}

dplot = data.frame(dplot,City)

#Location and time effects

#Marginal

p1 = ggplot(data = dplot, aes(x=Longitude,y=SaleDollarCnt))+
  geom_point(alpha=0.6)+
  stat_quantile(formula = y ~ qss(x,lambda = 1),method = "rqss", quantiles = c(0.99),size = 1)+
  geom_smooth(method="lm",color = "red")+labs(x="Longitude",y="Sale Price")+theme_bw()
p2 = ggplot(data = dplot, aes(x=Latitude,y=SaleDollarCnt))+
  geom_point(alpha = 0.6)+
  stat_quantile(formula = y ~ qss(x,lambda = 1),method = "rqss", quantiles = c(0.99),size = 1)+
  geom_smooth(method="lm",color = "red")+labs(x="Latitude",y="Sale Price")+theme_bw()
p3 =ggplot(data = dplot, aes(x=BuiltYear,y=SaleDollarCnt))+geom_point(alpha = 0.6)+
  stat_quantile(formula = y ~ x, method = "rq", quantiles = c(0.99),size = 1)+
  geom_smooth(method="lm",color = "red")+labs(x="Built Year",y="Sale Price")+theme_bw()

grid.arrange(p1,p2,p3)

#Interaction

ggplot(data = dplot, aes(x=BuiltYear,y=SaleDollarCnt,color = City))+geom_point(alpha = 0.2, size = 0.5)+
  geom_smooth(method = "lm", fill = NA, size = 1)+labs(x="Built Year",y="Sale Price")+theme_bw()

#Data cleaning

data_clean = data_raw[log(data_raw$SaleDollarCnt)>11.7,]
data_clean = data_clean[data_clean$FinishedSquareFeet<8.5e3,]
data_clean = data_clean[log(data_clean$SaleDollarCnt)<14 | log(data_clean$FinishedSquareFeet)>7.1,]
HHV = log(data_clean$BGMedHomeValue)>11
HHV[is.na(HHV)] = TRUE
data_clean = data_clean[HHV,]
data_clean = data_clean[data_clean$Latitude<47800000 & data_clean$Longitude< -121500000,]
data_clean = data_clean[data_clean$BuiltYear<1960 | log(data_clean$SaleDollarCnt)>11,]

#Some preprocessing

data_clean = within(data_clean,{
  StoryCnt = factor(round(StoryCnt))
  ViewType = factor(ifelse(is.na(ViewType),0,ViewType))
  GarageSquareFeet = ifelse(is.na(GarageSquareFeet),0,GarageSquareFeet)
  BedroomCnt = ifelse(BedroomCnt%%1==0,BedroomCnt,NA)
  BathroomCnt = ifelse(BathroomCnt%%0.25==0,BathroomCnt,NA)
})


#Missingness
p1 = ggplot(data = dplot,aes(x=BuiltYear,y=SaleDollarCnt,color = is.na(BGMedRent)))+
  geom_point(size = 0.8)+guides(color=guide_legend(title="Median Rent Missing?"))+
  labs(x="Built Year",y="Sale Dollar")+theme_bw()
p2 = ggplot(data = dplot,aes(x=BGMedIncome,y=SaleDollarCnt,color = is.na(BGMedRent)))+
  geom_point(size = 0.8)+guides(color=guide_legend(title="Median Rent Missing?"))+
  labs(x="Median Income",y="Sale Dollar")+theme_bw()
tiff(file="missing.tiff",
     width=10,height=5,units = "in",res=500)
grid.arrange(p1,p2)
dev.off()

#Multiple Imputation

init = mice(data_raw, maxit=0) 
meth = init$method
predM = init$predictorMatrix


predM[,"SaleDollarCnt"] = 0

meth["SaleDollarCnt"] = ""

meth[which(meth=="pmm")] = "rf"

house_im = parlmice(data_raw,
                    method=meth,predictorMatrix=predM,maxit=5,cluster.seed=1234,n.imp.core=1,n.core=4,printFlag = F)

save(house_im,file="MI_Z.rda")

load(file="MI_Z.rda")

#Extract the 30 imputed datasets, the row is ordered by variable "imp" - index of imputation

dfull_comp = mice::complete(dfull_im,"long")[,-2]

dfull_comp_tr = within(dfull_comp,{
  ViewType = factor(ifelse(ViewType==0,0,1))
})

dfull_comp_tr = subset(dfull_comp_tr,select=-BGMedYearBuilt)

#Extract the first imputed dataset and set as the working dataset

dwork = subset(dfull_comp_tr,.imp==1,select = -.imp)

set.seed(790)

#Divide the dataset into five training fold + 1 validation/testing fold
k = 5

flds.idx = sample(1:nrow(dwork),nrow(dwork),replace = F)

fld.len = round(length(flds.idx)/(k+1))

nfold = numeric(length(nrow(dwork)))

for(i in 1:(k+1)){
  nfold[flds.idx[((i-1)*fld.len+1):min(i*fld.len,nrow(dwork))]]  =  i
}

dwork = data.frame(dwork,nfold)

#Elastic Net
en_tr = subset(dwork,nfold != 6,select = -nfold)

en_ts = subset(dwork,nfold == 6,select = -nfold)

en_tr_label = en_tr$SaleDollarCnt 

en_tr_label = log(en_tr_label) 

en_ts_label = en_ts$SaleDollarCnt 

new_en_tr <- model.matrix(~.+0+BedroomCnt:BathroomCnt+FinishedSquareFeet:LotSizeSquareFeet+Longitude:Latitude+Longitude:Latitude:BuiltYear+
                             BGMedAge:BGMedIncome+BGPctOwn:BGPctKids+BGMedHomeValue:BuiltYear,data = en_tr[,-1]) 

new_en_ts <- model.matrix(~.+0+BedroomCnt:BathroomCnt+FinishedSquareFeet:LotSizeSquareFeet+Longitude:Latitude+Longitude:Latitude:BuiltYear+
                            BGMedAge:BGMedIncome+BGPctOwn:BGPctKids+BGMedHomeValue:BuiltYear,data = en_ts[,-1]) 

new_en_tr_norm =  new_en_tr
new_en_ts_norm =  new_en_ts
for(i in 1:ncol(new_en_tr)){
  new_en_tr_norm[,i] = new_en_tr[,i]-mean(new_en_tr[,i])
  new_en_ts_norm[,i] = new_en_ts[,i]-mean(new_en_tr[,i])
  new_en_ts_norm[,i] = new_en_ts_norm[,i]/norm(new_en_tr_norm[,i],"2")
  new_en_tr_norm[,i] = new_en_tr_norm[,i]/norm(new_en_tr_norm[,i],"2")
}

glmnet_cv_mat = matrix(0,nrow=31,ncol=3)
alpha_vec = c(0,10^seq(-3,0,length.out = 30))

for(i in 1:31){
  glmnet_fit = cv.glmnet(new_en_tr_norm,en_tr_label,
                         alpha=alpha_vec[i],family="gaussian",lambda = c(0,10^seq(-3,1,length.out = 30)),standardize = F)
  glmnet_cv_mat[i,] = c(glmnet_fit$cvm[which(glmnet_fit$lambda==glmnet_fit$lambda.1se)],glmnet_fit$lambda.1se,alpha_vec[i])
}

glmnet_coef = glmnet_cv_mat[which.min(glmnet_cv_mat[,1]),2:3]

best_glmnet_fit = glmnet(new_en_tr_norm,en_tr_label,alpha=glmnet_coef[2],lambda=glmnet_coef[1],standardize = F)

glmnet_fit_pred = exp(predict(best_glmnet_fit,lambda = glmnet_coef[1],newx = new_en_ts_norm,type="response"))

median(abs(glmnet_fit_pred-en_ts_label)/en_ts_label)

mean(abs(glmnet_fit_pred-en_ts_label)/en_ts_label)

#XGBoost

xgb_tr = subset(dwork,nfold != 6,select = -nfold)

xgb_ts = subset(dwork,nfold == 6,select = -nfold)

xgb_tr_label = xgb_tr$SaleDollarCnt 

xgb_ts_label = xgb_ts$SaleDollarCnt 

new_xgb_tr <- model.matrix(~.+0,data = xgb_tr[,-1]) 

new_xgb_ts <- model.matrix(~.+0,data = xgb_ts[,-1])

xgb_dtr = xgb.DMatrix(data = new_xgb_tr,label = xgb_tr_label)

xgb_dts = xgb.DMatrix(data = new_xgb_ts,label = xgb_ts_label)

mape = function(preds, dtrain) {
  target = getinfo(dtrain, "label")
  mape = median(abs(preds-target)/target)
  return(list(metric="mape",value=mape))
}

trainTask = makeRegrTask(data = xgb_tr,target = "SaleDollarCnt")
trainTask = createDummyFeatures(trainTask)

lrn = makeLearner("regr.xgboost")
lrn$par.vals = list(objective="reg:squarederror",nrounds=c(800))
params = makeParamSet(makeIntegerParam("max_depth",lower=3L,upper=10L),
                      makeNumericParam("eta",lower = 0.01, upper = 0.3),
                      makeNumericParam("gamma",lower=0,upper=5),
                      makeNumericParam("lambda",lower=0,upper=1),
                      makeNumericParam("alpha",lower=0,upper=1),
                      makeIntegerParam("min_child_weight",lower = 1L,upper = 6L),
                      makeNumericParam("subsample",lower = 0.5,upper = 0.8), 
                      makeNumericParam("colsample_bytree",lower = 0.5,upper = 0.9))

rdesc <- makeResampleDesc("CV",iters=10L)
ctrl = makeTuneControlRandom(maxit = 100L)

mapeM = makeMeasure(id = "mape",minimize = T,properties = c("regr"),best = 0,fun = function(task,model,pred,feats,extra.args) {
  mape = median(abs(pred$data$response-pred$data$truth)/pred$data$truth)
  return(aape)
})


parallelStartSocket(cpus=8)
parallelExport("mapeM")

xgb.tune = tuneParams(lrn,task=trainTask,resampling = rdesc,par.set = params,control=ctrl,measures = mapeM)

load(file = "Zillow_xgb.rda")

best_xgb_coef = xgb.tune$x

params = c(list(booster="gbtree",objective="reg:squarederror"),best_xgb_coef)

set.seed(790)
bst <- xgb.train(params = params, data = xgb_dtr,nthread = 8, nrounds = 800,feval = mape)

xgb_fit_pred <- predict(bst,new_xgb_ts)

median(abs(xgb_fit_pred-xgb_ts_label)/xgb_ts_label)
mean(abs(xgb_fit_pred-xgb_ts_label)/xgb_ts_label)


importance_matrix <- xgb.importance(colnames(new_xgb_tr), model = bst)

tiff(file="xgb_importance.tiff",
     width=6,height=6,units = "in",res=300)
xgb.plot.importance(importance_matrix[1:10], rel_to_first = TRUE, xlab = "Relative importance")
dev.off()


# PDPs for all 10 features
features <- c("FinishedSquareFeet","Latitude","Longitude","BuiltYear")
pred_fun = function(object,newdata){
  y_min = min(xgb_tr_label)
  y_max = max(xgb_tr_label)
  pred = predict(object,newdata)
  pred = (pred-y_min)/(y_max-y_min)
  return(pred)
}
ice_xgb <- lapply(features, FUN = function(feature) {
  pd <- partial(bst, pred.var = feature, train = new_xgb_tr, type = "regression",
                quantiles = T, ice = T, probs = seq(0.05,0.95,0.05),pred.fun = pred_fun)
  autoplot(pd, alpha = 0.8) + 
    ylim(range(0,1)) + 
    theme_light() + ylab("Predicted Sale Price")
})

tiff(file="xgb_ice.tiff",
     width=10,height=6,units = "in",res=500)
grid.arrange(grobs = ice_xgb, ncol = 2)
dev.off()


#Random Forest
rf_tr = subset(dwork,nfold != 6,select = -nfold)

rf_ts = subset(dwork,nfold == 6,select = -nfold)

rf_tr_label = rf_tr$SaleDollarCnt 

rf_ts_label = rf_ts$SaleDollarCnt 

new_rf_tr <- model.matrix(~.+0,data = rf_tr[,-1]) 

new_rf_ts <- model.matrix(~.+0,data = rf_ts[,-1])

trainTask = makeRegrTask(data = rf_tr,target = "SaleDollarCnt")
trainTask = createDummyFeatures(trainTask)

lrn = makeLearner("regr.randomForest")
params = makeParamSet(makeIntegerParam("ntree",lower=5L,upper=8L,trafo = function(x) 100*x),
                      makeIntegerParam("mtry",lower = 7L, upper = 10L),
                      makeIntegerParam("nodesize",lower= 4L,upper= 8L)
                      )
rdesc <- makeResampleDesc("CV",iters=10L)
ctrl = makeTuneControlRandom(maxit = 50L)
parallelStartSocket(cpus=8)
parallelExport("mapeM")
rf.tune = tuneParams(lrn,task=trainTask,resampling = rdesc,par.set = params,control=ctrl,measures = mapeM)

set.seed(790)
rf_fit = randomForest(x = new_rf_tr, y = rf_tr_label, ntree = 700, mtry = 10, nodesize = 6)

rf_fit_pred = predict(rf_fit,newdata = new_rf_ts)

median(abs(rf_fit_pred-rf_ts_label)/rf_ts_label)
mean(abs(rf_fit_pred-rf_ts_label)/rf_ts_label)

#Neural Net
nn_tr = subset(dwork,nfold != 6,select = -nfold)

nn_ts = subset(dwork,nfold == 6,select = -nfold)

nn_tr_label = nn_tr$SaleDollarCnt 

new_nn_tr <- model.matrix(~.+0,data = nn_tr[,-1])

new_nn_tr = data.frame(SaleDollarCnt = nn_tr$SaleDollarCnt, new_nn_tr)

tr_max = apply(new_nn_tr,2,max)

tr_min = apply(new_nn_tr,2,min)

for(i in 1:ncol(new_nn_tr)){
  new_nn_tr[,i] = (new_nn_tr[,i] - tr_min[i])/(tr_max[i] - tr_min[i])
}

trainTask = makeRegrTask(data = nn_tr,target = "SaleDollarCnt")

lrn = makeLearner("regr.h2o.deeplearning")
lrn$par.vals = list(hidden = c(128, 128, 128),stopping_rounds = 5, stopping_metric = "RMSE", stopping_tolerance = 0.0001)
params = makeParamSet(makeIntegerParam("epochs",lower=8L,upper=11L, trafo = function(x) x*10),
                      makeIntegerParam("input_dropout_ratio",lower= 1L,upper= 2L, trafo = function(x) x/10),
                      makeIntegerParam("max_w2",lower = 1L, upper = 3L, trafo = function(x) 10^x),
                      makeNumericParam("l1",lower = 1e-6, upper = 1e-8),
                      makeNumericParam("l2",lower = 1e-6, upper = 1e-8)
)

rdesc <- makeResampleDesc("CV",iters=10L)
ctrl = makeTuneControlRandom(maxit = 50L)
parallelStartSocket(cpus=8)
parallelExport("mapeM")
nn.tune = tuneParams(lrn,task=trainTask,resampling = rdesc,par.set = params,control=ctrl,measures = mapeM)

nn_ts_label = nn_ts$SaleDollarCnt

new_nn_ts <- model.matrix(~.+0,data = nn_ts[,-1])

for(i in 1:ncol(new_nn_ts)){
  new_nn_ts[,i] = (new_nn_ts[,i] - tr_min[i+1])/(tr_max[i+1] - tr_min[i+1])
}

h2o.init(nthreads=-1)

train.hex = as.h2o(new_nn_tr)

test.hex = as.h2o(new_nn_ts)


m1 <- h2o.deeplearning(
  model_id="zillow_nn", 
  training_frame=train.hex, 
  y="SaleDollarCnt",
  activation="Rectifier",
  hidden=c(128,128,128),
  epochs=100,
  standardize = T,
  input_dropout_ratio=0.1,
  max_w2 = 1e3,
  l1 = 8e-7,
  l2 = 6e-7,
  variable_importances = T
)

head(as.data.frame(h2o.varimp(m1)))

h2o_pre = h2o.predict(m1,newdata = test.hex)

h2o_pre = as.vector(h2o_pre)*(tr_max[1]-tr_min[1]) + tr_min[1]

median(abs(h2o_pre-xgb_ts_label)/xgb_ts_label)
mean(abs(h2o_pre-xgb_ts_label)/xgb_ts_label)

features <- c("FinishedSquareFeet","Latitude","Longitude","BuiltYear")
pred_fun = function(object,newdata){
  newdata = as.h2o(newdata)
  pred = as.vector(h2o.predict(object,newdata))
  return(pred)
}
ice_nn <- lapply(features, FUN = function(feature) {
  pd <- partial(m1, pred.var = feature, train = new_nn_tr, type = "regression",
                quantiles = T, ice = T, probs = seq(0.05,0.95,0.05),pred.fun = pred_fun)
  autoplot(pd, alpha = 0.8) + 
    theme_light() + ylab("Predicted Sale Price")
})

tiff(file="nn_ice.tiff",
     width=10,height=6,units = "in",res=500)
grid.arrange(grobs = ice_nn, ncol = 2)
dev.off()

h2o.shutdown(prompt=FALSE)

#Stacking

XGB_stack_param = list(
  fold_1 = list(max_depth = 6, eta = 0.0308, gamma = 2.64, lambda = 0.919, 
                alpha = 0.917, min_child_weight = 1, subsample = 0.683, colsample_bytree = 0.719),
  fold_2 = list(max_depth = 9, eta = 0.0211, gamma = 4.61, lambda = 0.283, 
                alpha = 0.4, min_child_weight = 3, subsample = 0.783, colsample_bytree = 0.897),
  fold_3 = list(max_depth = 9, eta = 0.0211, gamma = 4.61, lambda = 0.283, 
                alpha = 0.4, min_child_weight = 3, subsample = 0.783, colsample_bytree = 0.897),
  fold_4 = list(max_depth = 9, eta = 0.0211, gamma = 4.61, lambda = 0.283, 
                alpha = 0.4, min_child_weight = 3, subsample = 0.783, colsample_bytree = 0.897),
  fold_5 = list(max_depth = 9, eta = 0.0211, gamma = 4.61, lambda = 0.283, 
                alpha = 0.4, min_child_weight = 3, subsample = 0.783, colsample_bytree = 0.897)
)

NN_stack_param = data.frame(
  rbind(c(80,0.1,1000,8.5e-7,9.19e-7),
        c(90,0.1,10,9.11e-7,9.44e-7),
        c(110,0.1,1000,4.28e-8,2.98e-8),
        c(100,0.1,100,8.73e-7,7.45e-7),
        c(100,0.1,100,8.73e-7,7.45e-7)
  )
)

colnames(NN_stack_param) = c("epochs","input_dropout_ratio","max_w2","l1","l2")

RF_stack_param = data.frame(
  rbind(c(700,10,7),
        c(500,10,4),
        c(500,10,8),
        c(500,10,4),
        c(500,10,8))
)

colnames(RF_stack_param) = c("ntree","mtry","nodesize")

load(file="imp_stack.rda")


result_xgb_tr = lapply(seq_len(30),function(i){
  dwork = subset(dimp_stack,.imp == i, select = -.imp)
  dtrain = subset(dwork, nfold != 6)
  dtest = subset(dwork, nfold == 6)
  do.call("cbind",lapply(seq_len(5), function(j){
    fold = j
    xgb_tr = subset(dtrain, nfold != fold, select = -nfold)
    xgb_ts = subset(dtrain, nfold == fold, select = -nfold)
    xgb_tr_label = xgb_tr$SaleDollarCnt 
    xgb_ts_label = xgb_ts$SaleDollarCnt 
    new_xgb_tr <- model.matrix(~.+0,data = xgb_tr[,-1]) 
    new_xgb_ts <- model.matrix(~.+0,data = xgb_ts[,-1])
    xgb_dtr = xgb.DMatrix(data = new_xgb_tr,label = xgb_tr_label)
    xgb_dts = xgb.DMatrix(data = new_xgb_ts,label = xgb_ts_label)
    xgb_coef = XGB_stack_param[[fold]]
    params = c(list(booster="gbtree",objective="reg:squarederror"),xgb_coef)
    bst <- xgb.train(params = params, data = xgb_dtr,nthread = 16, nrounds = 800)
    xgb_fit_pred <- predict(bst,new_xgb_ts)
    return(xgb_fit_pred)
  }))
})

result_xgb_ts = lapply(seq_len(30),function(i){
  dwork = subset(dimp_stack,.imp == i, select = -.imp)
  xgb_tr = subset(dwork, nfold != 6,select = -nfold)
  xgb_ts = subset(dwork, nfold == 6,select = -nfold)
  xgb_tr_label = xgb_tr$SaleDollarCnt 
  xgb_ts_label = xgb_ts$SaleDollarCnt 
  new_xgb_tr <- model.matrix(~.+0,data = xgb_tr[,-1]) 
  new_xgb_ts <- model.matrix(~.+0,data = xgb_ts[,-1])
  xgb_dtr = xgb.DMatrix(data = new_xgb_tr,label = xgb_tr_label)
  xgb_dts = xgb.DMatrix(data = new_xgb_ts,label = xgb_ts_label)
  xgb_coef = list(max_depth=9,eta=0.0262,gamma=0.978,lambda=0.712,alpha=0.26,min_child_weight=3,subsample=0.734,colsample_bytree=0.708)
  params = c(list(booster="gbtree",objective="reg:squarederror"),xgb_coef)
  set.seed(790)
  bst <- xgb.train(params = params, data = xgb_dtr,nthread = 12, nrounds = 800)
  xgb_fit_pred <- predict(bst,new_xgb_ts)
  xgb_fit_pred
})

save(result_xgb_ts,file = "xgb_oob_ts.rda")

cl <- makeCluster(12)
registerDoParallel(cl)

result_rf_tr = foreach(i = 1:30, .packages = "randomForest") %dopar% {
  imp = i
  dwork = subset(dimp_stack,.imp == imp, select = -.imp)
  dtrain = subset(dwork, nfold != 6)
  dtest = subset(dwork, nfold == 6)
  rf_test = foreach(j = 1:5, .export = "imp", .packages = "randomForest", .combine = rbind, .multicombine = T) %dopar% {
    fold = j
    rf_tr = subset(dtrain, nfold != fold, select = -nfold)
    rf_ts = subset(dtrain, nfold == fold, select = -nfold)
    rf_tr_label = rf_tr$SaleDollarCnt 
    rf_ts_label = rf_ts$SaleDollarCnt 
    new_rf_tr <- model.matrix(~.+0,data = rf_tr[,-1]) 
    new_rf_ts <- model.matrix(~.+0,data = rf_ts[,-1])
    rf_coef = as.numeric(RF_stack_param[fold,])
    set.seed(790)
    rf_fit = randomForest(x = new_rf_tr, y = rf_tr_label, ntree = rf_coef[1], mtry = rf_coef[2], nodesize = rf_coef[3])
    rf_fit_pred = predict(rf_fit,newdata = new_rf_ts)
    cbind(fold,rf_fit_pred) 
  }
}

result_rf_ts = foreach(i = 1:30, .packages = "randomForest") %dopar% {
  imp = i
  dwork = subset(dimp_stack,.imp == imp, select = -.imp)
  rf_tr = subset(dwork, nfold != 6, select = -nfold)
  rf_ts = subset(dwork, nfold == 6, select = -nfold)
  rf_tr_label = rf_tr$SaleDollarCnt 
  rf_ts_label = rf_ts$SaleDollarCnt 
  new_rf_tr <- model.matrix(~.+0,data = rf_tr[,-1]) 
  new_rf_ts <- model.matrix(~.+0,data = rf_ts[,-1])
  rf_coef = as.numeric(RF_stack_param[fold,])
  set.seed(790)
  rf_fit = randomForest(x = new_rf_tr, y = rf_tr_label, ntree = 700, mtry = 10, nodesize = 6)
  rf_fit_pred = predict(rf_fit,newdata = new_rf_ts)
  rf_fit_pred
}

save(result_rf_ts,file="rf_oob_ts.rda")

h2o.init(nthreads=-1)

result_nn_tr = lapply(seq_len(30),function(i){
  dwork = subset(dimp_stack,.imp == i, select = -.imp)
  dtrain = subset(dwork, nfold != 6)
  dtest = subset(dwork, nfold == 6)
  do.call("cbind",lapply(seq_len(5), function(j){
    fold = j
    nn_tr = subset(dtrain, nfold != fold, select = -nfold)
    nn_ts = subset(dtrain, nfold == fold, select = -nfold)
    nn_tr_label = nn_tr$SaleDollarCnt 
    nn_ts_label = nn_ts$SaleDollarCnt 
    new_nn_tr <- model.matrix(~.+0,data = nn_tr[,-1]) 
    new_nn_ts <- model.matrix(~.+0,data = nn_ts[,-1])
    new_nn_tr = data.frame(SaleDollarCnt = nn_tr$SaleDollarCnt, new_nn_tr)
    tr_max = apply(new_nn_tr,2,max)
    tr_min = apply(new_nn_tr,2,min)
    for(i in 1:ncol(new_nn_tr)){
      new_nn_tr[,i] = (new_nn_tr[,i] - tr_min[i])/(tr_max[i] - tr_min[i])
    }
    nn_ts_label = nn_ts$SaleDollarCnt
    new_nn_ts <- model.matrix(~.+0,data = nn_ts[,-1])
    for(i in 1:ncol(new_nn_ts)){
      new_nn_ts[,i] = (new_nn_ts[,i] - tr_min[i+1])/(tr_max[i+1] - tr_min[i+1])
    }
    train.hex = as.h2o(new_nn_tr)
    test.hex = as.h2o(new_nn_ts)
    nn_coef = NN_stack_param[[fold]]
    set.seed(790)
    nn_fit <- h2o.deeplearning(
      model_id="zillow_nn", 
      training_frame=train.hex, 
      y="SaleDollarCnt",
      activation="Rectifier",
      hidden=c(128,128,128),
      epochs=nn_coef[1],
      standardize = T,
      input_dropout_ratio=nn_coef[2],
      max_w2 = nn_coef[3],
      l1 = nn_coef[4],
      l2 = nn_coef[5]
    )
    nn_fit_pred = h2o.predict(nn_fit,newdata = test.hex)
    nn_fit_pred = as.vector(h2o_pre)*(tr_max[1]-tr_min[1]) + tr_min[1]
    nn_fit_pred
  }))
})

result_nn_ts = lapply(seq_len(30),function(i){
  imp = i
  dwork = subset(dimp_stack,.imp == imp, select = -.imp)
  nn_tr = subset(dwork, nfold != 6, select = -nfold)
  nn_ts = subset(dwork, nfold == 6, select = -nfold)
  nn_tr_label = nn_tr$SaleDollarCnt 
  nn_ts_label = nn_ts$SaleDollarCnt 
  new_nn_tr <- model.matrix(~.+0,data = nn_tr[,-1]) 
  new_nn_ts <- model.matrix(~.+0,data = nn_ts[,-1])
  new_nn_tr = data.frame(SaleDollarCnt = nn_tr$SaleDollarCnt, new_nn_tr)
  tr_max = apply(new_nn_tr,2,max)
  tr_min = apply(new_nn_tr,2,min)
  for(i in 1:ncol(new_nn_tr)){
    new_nn_tr[,i] = (new_nn_tr[,i] - tr_min[i])/(tr_max[i] - tr_min[i])
  }
  nn_ts_label = nn_ts$SaleDollarCnt
  new_nn_ts <- model.matrix(~.+0,data = nn_ts[,-1])
  for(i in 1:ncol(new_nn_ts)){
    new_nn_ts[,i] = (new_nn_ts[,i] - tr_min[i+1])/(tr_max[i+1] - tr_min[i+1])
  }
  train.hex = as.h2o(new_nn_tr)
  test.hex = as.h2o(new_nn_ts)
  set.seed(790)
  nn_fit <- h2o.deeplearning(
    model_id="zillow_nn", 
    training_frame=train.hex, 
    y="SaleDollarCnt",
    activation="Rectifier",
    hidden=c(128,128,128),
    epochs=100,
    standardize = T,
    input_dropout_ratio=0.1,
    max_w2 = 1e3,
    l1 = 8e-7,
    l2 = 6e-7
  )
  nn_fit_pred = h2o.predict(nn_fit,newdata = test.hex)
  nn_fit_pred = as.vector(nn_fit_pred)*(tr_max[1]-tr_min[1]) + tr_min[1]
  nn_fit_pred
})

save(result_nn_ts,file="nn_oob_ts.rda")


#Final comparison

dstack_tr = vector("list",30)

for(i in 1:30){
  cur_imp = subset(dimp_stack,.imp == i, select = -.imp)
  dstack_tr[[i]] = as.data.frame(rbind(subset(dwork, nfold == 1, select = -nfold),subset(dwork, nfold == 2, select = -nfold),
                         subset(dwork, nfold == 3, select = -nfold),subset(dwork, nfold == 4, select = -nfold),
                         subset(dwork, nfold == 5, select = -nfold)))
}

load(file = "rf_oob.rda")
load(file = "xgb_oob.rda")
load(file = "nn_oob.rda")

plot(Xoob_tr$nn,dstack_tr[[1]]$SaleDollarCnt)

Xoob_tr = as.data.frame(cbind(c(result_xgb[[1]]),c(result_rf[[1]]),c(result_nn_tr[[1]])))

Xoob_ts = data.frame(result_xgb_ts[[1]],result_rf_ts[[1]],result_nn_ts[[1]])

Xoob_tr = data.frame(Xoob_tr,Response = dstack_tr[[1]]$SaleDollarCnt)

test_fit = lm(log(Response)~log(xgb)+log(rf)+log(nn)-1,data = Xoob_tr)

pred_fit = exp(0.76209*log(Xoob_ts$xgb)+0.10841*log(Xoob_ts$rf)+0.12840*log(Xoob_ts$nn))

pred_fit = exp(predict.lm(test_fit,newdata = log(Xoob_ts)))


xrn_ts = matrix(0,nrow=1926,ncol=30)


for(i in 1:30){
  xgb = c(result_xgb[[i]])
  rf = c(result_rf[[i]])
  nn = c(result_nn_tr[[i]])
  Xoob_tr = data.frame(xgb,rf,nn)
  Xoob_ts = data.frame(result_xgb_ts[[i]],result_rf_ts[[i]],result_nn_ts[[i]])
  colnames(Xoob_ts) = c("xgb","rf","nn")
  Xoob_tr = data.frame(Xoob_tr,Response = dstack_tr[[i]]$SaleDollarCnt)
  xrn_fit = lm(log(Response)~log(xgb)+log(rf)+log(nn)-1,data = Xoob_tr)
  xrn_coef = as.numeric(xrn_fit$coefficients)
  xrn_pred = exp(xrn_coef[1]*log(Xoob_ts$xgb)+xrn_coef[2]*log(Xoob_ts$rf)+xrn_coef[3]*log(Xoob_ts$nn))
  xrn_ts[,i] = xrn_pred
} 


ans = subset(dimp_stack,.imp == 30 & nfold==6)$SaleDollarCnt

plot(log(rowMeans(xrn_ts)),log(ans))
abline(0,1)


p1 = qplot(log(Reduce("+",result_nn_ts)/30),log(ans))+geom_point(color = "Steel Blue", alpha = 0.8)+labs(x = "Predicted Sale Price by NN", y = "Observed Sale Price") + 
  geom_abline(slope = 1, intercept = 0, size = 1.1)+xlim(c(11.5,15.5))+ylim(c(11.5,15.5))+theme_bw()+stat_cor(method = "pearson")
p2 = qplot(log(Reduce("+",result_rf_ts)/30),log(ans))+geom_point(color = "Steel Blue", alpha = 0.8)+labs(x = "Predicted Sale Price by RF", y = "Observed Sale Price") + 
  geom_abline(slope = 1, intercept = 0, size = 1.1)+xlim(c(11.5,15.5))+ylim(c(11.5,15.5))+theme_bw()+stat_cor(method = "pearson")
p3 = qplot(log(Reduce("+",result_xgb_ts)/30),log(ans))+geom_point(color = "Steel Blue", alpha = 0.8)+labs(x = "Predicted Sale Price by XGB", y = "Observed Sale Price") + 
  geom_abline(slope = 1, intercept = 0, size = 1.1)+xlim(c(11.5,15.5))+ylim(c(11.5,15.5))+theme_bw()+stat_cor(method = "pearson")
p4 = qplot(log(rowMeans(xrn_ts)),log(ans))+geom_point(color = "Steel Blue", alpha = 0.8)+labs(x = "Predicted Sale Price by Stacking", y = "Observed Sale Price") + 
  geom_abline(slope = 1, intercept = 0, size = 1.1)+xlim(c(11.5,15.5))+ylim(c(11.5,15.5))+theme_bw()+stat_cor(method = "pearson")

tiff(file="corr.tiff",
     width=10,height=10,units = "in",res=300)
grid.arrange(p1,p2,p3,p4,ncol=2)
dev.off()

hist(log(Reduce("+",result_xgb_ts)/30),freq = F,breaks = 30)
hist(log(Reduce("+",result_nn_ts)/30),freq = F, breaks = 30)

qqplot(log(rowMeans(xrn_ts)),log(ans))
qqplot(log(Reduce("+",result_xgb_ts)/30),log(ans))
abline(0,1)

median(abs(Reduce("+",result_xgb_ts)/30-ans)/ans)
mean(abs(Reduce("+",result_xgb_ts)/30-ans)/ans)

median(abs(Reduce("+",result_rf_ts)/30-ans)/ans)
mean(abs(Reduce("+",result_rf_ts)/30-ans)/ans)

median(abs(Reduce("+",result_nn_ts)/30-ans)/ans)
mean(abs(Reduce("+",result_nn_ts)/30-ans)/ans)

median(abs(rowMeans(xrn_ts)-ans)/ans)
mean(abs(rowMeans(xrn_ts)-ans)/ans)


#Forecast

City = rep("Other",nrow(dwork))

dfore = subset(dwork,select = -nfold)

dfore = within(dfore,{
  Latitude = Latitude/1e6
  Longitude = Longitude/1e6
})

for(i in 1:nrow(dwork)){
  if(dfore$Longitude[i] %between% c(-122.25,-122.21) & dfore$Latitude[i] %between% c(47.53,47.59)){
    City[i] = "Mercer Island"
  }
  if(dfore$Longitude[i] %between% c(-122.206,-122.11) & dfore$Latitude[i] %between% c(47.546,47.65)){
    City[i] = "Bellevue"
  }
  if(dfore$Longitude[i] %between% c(-122.43,-122.24) & dfore$Latitude[i] %between% c(47.50,47.73)){
    City[i] = "Seattle"
  }
}

dfore = data.frame(dfore,City)

dsea = subset(dfore,City=="Seattle",select = -City)

dseatime = subset(dsea,select = c(SaleDollarCnt,BuiltYear))

dseatime <- dseatime %>% arrange(BuiltYear) %>% group_by(BuiltYear)

dst <- dseatime %>% summarise(
  Sale = mean(SaleDollarCnt)
)

dst_tr <- filter(dst, BuiltYear <= 2005)
dst_ts <- filter(dst, BuiltYear > 2005)

ts_sea_tr = ts(log(dst_tr$Sale), frequency = 1,start = 1900, end = 2005)
ts_sea_ts =ts(log(dst_ts$Sale), frequency = 1,start = 2006, end = 2015)

sale_arima = arima(ts_sea_tr,order=c(0,1,1))

farima = forecast(sale_arima,h=10)

autoplot(farima)

dsea_tr = subset(dsea,BuiltYear <= 2005)
dsea_ts = subset(dsea,BuiltYear > 2005)

dsea_tr = within(dsea_tr,{
  SaleRes = log(SaleDollarCnt) - sale_sp$fitted[BuiltYear - 1900 + 1]  
})

dsea_ts = within(dsea_ts,{
  SaleRes = log(SaleDollarCnt) - sale_sp$mean[BuiltYear - 2005]  
})

dsea_tr = within(dsea_tr,{
  SaleRes = log(SaleDollarCnt) - farima$fitted[BuiltYear - 1900 + 1]  
})

dsea_ts = within(dsea_ts,{
  SaleRes = log(SaleDollarCnt) - farima$mean[BuiltYear - 2005]  
})

sea_tr_label = dsea_tr$SaleRes

sea_ts_label = dsea_ts$SaleRes

new_dsea_tr = subset(dsea_tr,selec = -SaleDollarCnt)

new_dsea_ts = subset(dsea_ts,selec = -SaleDollarCnt)

sea_xgb_tr <- model.matrix(~.+0,data = new_dsea_tr[,-19]) 

sea_xgb_ts <- model.matrix(~.+0,data = new_dsea_ts[,-19]) 

xgb_dtr = xgb.DMatrix(data = sea_xgb_tr,label = sea_tr_label)

xgb_dts = xgb.DMatrix(data = sea_xgb_ts,label = sea_ts_label)

trainTask = makeRegrTask(data = new_dsea_tr,target = "SaleRes")
trainTask = createDummyFeatures(trainTask)

lrn = makeLearner("regr.xgboost")
lrn$par.vals = list(objective="reg:squarederror")
params = makeParamSet(makeIntegerParam("nrounds", lower = 4L, upper = 6L, trafo = function(x) x*100),
                      makeIntegerParam("max_depth",lower=3L,upper=10L),
                      makeNumericParam("eta",lower = 0.01, upper = 0.3),
                      makeNumericParam("gamma",lower=0,upper=5),
                      makeNumericParam("lambda",lower=0,upper=1),
                      makeNumericParam("alpha",lower=0,upper=1),
                      makeIntegerParam("min_child_weight",lower = 1L,upper = 6L),
                      makeNumericParam("subsample",lower = 0.5,upper = 0.8), 
                      makeNumericParam("colsample_bytree",lower = 0.5,upper = 0.9))

rdesc <- makeResampleDesc("CV",iters=10L)
ctrl = makeTuneControlRandom(maxit = 100L)

parallelStartSocket(cpus=12)

sea.tune = tuneParams(lrn,task=trainTask,resampling = rdesc,par.set = params,control=ctrl)

save(sea.tune,file="sea_xgb.rda")

load(file="sea_xgb.rda")

params = c(list(booster="gbtree",objective="reg:squarederror"),sea.tune$x[-1])

set.seed(790)

sea_bst = xgb.train(params = params, data = xgb_dtr,nthread = 12, nrounds = 400)

sea_fit = predict(sea_bst,sea_xgb_tr)

fit_ts = data.frame(BuiltYear = new_dsea_tr$BuiltYear, SaleRes = sea_fit)

fit_ts = within(fit_ts,{
  SaleDollarCnt = exp(SaleRes + farima$fitted[BuiltYear - 1900])  
})

fit_ts = subset(fit_ts,select = -SaleRes)

dseaFit <- fit_ts %>% arrange(BuiltYear) %>% group_by(BuiltYear)

dstFit <- dseaFit %>% summarise(
  Sale = mean(SaleDollarCnt)
)

ts_sea_fit =ts(log(dstFit$Sale), frequency = 1,start = 1900, end = 2005)

sea_pred = predict(sea_bst,sea_xgb_ts)

pred_ts = data.frame(BuiltYear = new_dsea_ts$BuiltYear, SaleRes = sea_pred)

pred_ts = within(pred_ts,{
  SaleDollarCnt = exp(SaleRes + farima$mean[BuiltYear - 2005])  
})

pred_ts = subset(pred_ts,select = -SaleRes)

dseaPred <- pred_ts %>% arrange(BuiltYear) %>% group_by(BuiltYear)

dstPred <- dseaPred %>% summarise(
  Sale = mean(SaleDollarCnt)
)

ts_sea_pred =ts(log(dstPred$Sale), frequency = 1,start = 2006, end = 2015)


dfore_plot_1 = data.frame(Time = 1900:2005, actual = farima$x)
dfore_plot_2 = data.frame(Time = 2006:2015, fit = as.numeric(ts_sea_pred), actual = as.numeric(ts_sea_ts))

tiff(file="forecast.tiff",
     width=10,height=5,units = "in",res=300)
autoplot(farima)+labs(y = "log(Averag Sale Price)", x = "Time", title = "Forecast from ARIMA(0,1,1) + XGBoost") +
  geom_point(data = dfore_plot_1, aes(x=Time,y=actual)) + geom_line(data = dfore_plot_2, aes(x=Time,y=actual), color = "Black") +
  geom_point(data = dfore_plot_2, aes(x=Time,y=actual)) + geom_line(data = dfore_plot_2, aes(x=Time,y=fit), linetype = "dashed", color = "Red", size = 1) +
  geom_point(data = dfore_plot_2, aes(x=Time,y=fit), color = "Red", cex = 2.5)
dev.off()

p1 = ggAcf(sale_arima$residuals) + labs(title="")
p2 = ggPacf(sale_arima$residuals) + labs(title="")
tiff(file="apacf.tiff",
     width=12,height=5,units = "in",res=300)
grid.arrange(p1,p2,nrow = 1)
dev.off()





