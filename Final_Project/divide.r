library(mice)

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

#Divide the dataset into five folds

flds.idx = sample(1:nrow(dwork),nrow(dwork),replace = F)

fld.len = length(flds.idx)/5

dtrain_folds = vector("list",5)

for(i in 1:5){
  dtrain_folds[[i]] = dwork[flds.idx[((i-1)*fld.len+1):(i*fld.len)],]
}

#Name the folds accordingly

name(dtrain_folds) = c("XGBoost","RandomForest","NeuralNetwork","Validation","Test")
