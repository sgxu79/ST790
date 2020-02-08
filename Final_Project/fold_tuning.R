library(mice)
library(xgboost)
library(mlr)
library(caret)
library(glmnet)
library(parallel)
library(parallelMap)
library(h2o)
library(randomForest)

load(file="MI_Z.rda")

#Extract the 30 imputed datasets, the row is ordered by variable "imp" - index of imputation

dfull_comp = complete(dfull_im,"long")[,-2]

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

dtrain = subset(dwork, nfold != 6)
dtest = subset(dwork, nfold == 6)

#Tuning the three models for a specific fold

fold = 1

fold_name = paste("_fold_",fold,".rda",sep="")

dfold_tr = subset(dtrain, nfold != fold, select = -nfold)
dfold_ts = subset(dtrain, nfold == fold, select = -nfold)

mape = function(preds, dtrain) {
  target = getinfo(dtrain, "label")
  mape = median(abs(preds-target)/target)
  return(list(metric="mape",value=mape))
}

trainTask = makeRegrTask(data = dfold_tr,target = "SaleDollarCnt")
trainTask = createDummyFeatures(trainTask)

#10-fold cross validation with mape as evaluation metric
rdesc <- makeResampleDesc("CV",iters=10L)
mapeM = makeMeasure(id = "mape",minimize = T,properties = c("regr"),best = 0,fun = function(task,model,pred,feats,extra.args) {
  mape = median(abs(pred$data$response-pred$data$truth)/pred$data$truth)
  return(mape)
})

parallelStartSocket(cpus=16)
parallelExport("mapeM")

#XGBoost

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

ctrl = makeTuneControlRandom(maxit = 100L)

xgb.tune = tuneParams(lrn,task=trainTask,resampling = rdesc,par.set = params,control=ctrl,measures = mapeM)

save(xgb.tune, file = paste("xgb",fold_name,sep=""))

#Neural Net
lrn = makeLearner("regr.h2o.deeplearning")
lrn$par.vals = list(hidden = c(128, 128, 128),stopping_rounds = 5, stopping_metric = "RMSE", stopping_tolerance = 0.0001)
params = makeParamSet(makeIntegerParam("epochs",lower=8L,upper=11L, trafo = function(x) x*10),
                      makeIntegerParam("input_dropout_ratio",lower= 1L,upper= 2L, trafo = function(x) x/10),
                      makeIntegerParam("max_w2",lower = 1L, upper = 3L, trafo = function(x) 10^x),
                      makeNumericParam("l1",lower = 1e-8, upper = 1e-6),
                      makeNumericParam("l2",lower = 1e-8, upper = 1e-6)
                      
)
ctrl = makeTuneControlRandom(maxit = 50L)
nn.tune = tuneParams(lrn,task=trainTask,resampling = rdesc,par.set = params,control=ctrl,measures = mapeM)

save(nn.tune, file = paste("nn",fold_name,sep=""))

#Random Forest
lrn = makeLearner("regr.randomForest")
params = makeParamSet(makeIntegerParam("ntree",lower=5L,upper=7L,trafo = function(x) 100*x),
                      makeIntegerParam("mtry",lower = 7L, upper = 10L),
                      makeIntegerParam("nodesize",lower= 4L,upper= 8L)
)
ctrl = makeTuneControlRandom(maxit = 25L)
rf.tune = tuneParams(lrn,task=trainTask,resampling = rdesc,par.set = params,control=ctrl,measures = mapeM)

save(rf.tune, file = paste("rf",fold_name,sep=""))