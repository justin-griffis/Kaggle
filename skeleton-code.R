#Replace NAs with Means

for(i in 1:ncol(Final.Data)){
  Final.Data[is.na(Final.Data[,i]), i] <- mean(Final.Data[,i], na.rm = TRUE)
}


#R-Squared
rsq <- function (x, y) cor(x, y) ^ 2
R2 <- rsq(data$y,data$yhat)
R2



#Transformations
##SET Dependent Variable of Focus

DepVar <- "Dependent"
y <- Data$Dependent

#Grab List of Dependent Variables

vars <- colnames(Data)
vars <- setdiff(vars, DepVar)
formula_string <- paste(vars, collapse= "+")

#Create Logs of Vars

for(v in 1:length(vars)){
  formula_string = paste(formula_string , paste0('log(', vars[v],')'), sep = '+')
}

#Create Quadratic Relationships

for(v in 1:length(vars)){
  formula_string = paste(formula_string, paste0('I(', vars[v],'^2)'), sep = '+')
}

#Create Interaction Terms

for(v in 1:length(vars)){
  formula_string = paste(formula_string ,  paste0(vars[v],':',vars[v]), sep = '+')
}



#Create Interactions of Logs- both ways

for(v in 1:length(vars)){
  formula_string = paste(formula_string ,  paste0(vars[v],':',paste0('log(', vars[v],')')), sep = '+')
}



for(v in 1:length(vars)){
  formula_string = paste(formula_string ,  paste0(paste0('log(', vars[v],')'),':',vars[v]), sep = '+')
}



fit_rf = randomForest(formula(paste(DepVar ,'~ ', paste(formula_string))),data=Data)
varImpPlot(fit_rf)

VarImp <- as.data.frame(importance(fit_rf))
VarImp <- setDT(VarImp, keep.rownames = TRUE)[]
VarImp <- VarImp[order(VarImp$IncNodePurity),]



#Only keep greater than median

VarImp2 <- VarImp[which(VarImp$IncNodePurity>=median(VarImp$IncNodePurity)),]
Variables.RF <- merge(VarImp2,Series.Code.Lookup,by.x="rn",by.y="Series.Code",all.x=T)

Variables.RF <- Variables.RF %>%
  select(-IncNodePurity) %>%
  dplyr::rename(RandomForest=rn,RF.Name=Series.Name)

Variables.RF$RF.Count <- 1

#Stepwise Regression Method- include interactions

base.mod <- lm(formula(paste(DepVar ,'~  1')), data= Final.Data)  # base intercept only model
all.mod <- lm(formula(paste(DepVar ,'~ ', paste(formula_string))), data= Final.Data) # full model with all predictors
stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 0, steps = 1000)  # perform step-wise algorithm
shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept
print(shortlistedVars)

Variables.Stepwise <- as.data.frame(shortlistedVars)
Variables.Stepwise <- merge(Variables.Stepwise,Series.Code.Lookup,by.x="shortlistedVars",by.y="Series.Code",all.x=T)

Variables.Stepwise <- Variables.Stepwise %>%
    dplyr::rename(Stepwise=shortlistedVars,Stepwise.Name=Series.Name)

Variables.Stepwise$SW.Count <- 1

#Lasso regression

xnam <- colnames(Final.Data)
xnam <- setdiff(xnam, DepVar)
rm(f,x)

f <- as.formula(paste('y ~ ', paste(formula_string)))
x <- model.matrix(f, Final.Data)

#Number of folds and the range

cvglm = cv.glmnet(x, y, family = 'gaussian', lambda = 10^seq(4,-4,-.05)
                  , nfolds=4, type.measure="mse", alpha =1)

glmmod = glmnet(x, y, family='gaussian', lambda = cvglm$lambda.1se, alpha = 1)

#Get coeficient function

tidy_coef2 <- function(x){
  x <- coef(x)
  data.frame(term=rownames(x),
             estimate=matrix(x)[,1],
              stringsAsFactors = FALSE) %>%
    filter(estimate!=0)
}

tidy_coef2(glmmod)

Variables.Lasso <- as.data.frame(tidy_coef2(glmmod))
Variables.Lasso <- merge(Variables.Lasso,Series.Code.Lookup,by.x="term",by.y="Series.Code",all.x=T)
Variables.Lasso <- Variables.Lasso[-1,]

Variables.Lasso <- Variables.Lasso %>%
  select(-estimate) %>%
  dplyr::rename(Lasso=term,Lasso.Name=Series.Name)

Variables.Lasso$L.Count <- 1

#Create Variable Comparisons

###Of Three techniques, see which stay.
Variable.Compare <- merge(Variables.Lasso,Variables.Stepwise,by.x="Lasso",by.y="Stepwise",all.x=T,all.y=T)
Variable.Compare <- merge(Variable.Compare,Variables.RF,by.x="Lasso",by.y="RandomForest",all.x=T,all.y=T)

#Turn NAs into 0s
Variable.Compare$L.Count[is.na(Variable.Compare$L.Count)] <- 0
Variable.Compare$SW.Count[is.na(Variable.Compare$SW.Count)] <- 0
Variable.Compare$RF.Count[is.na(Variable.Compare$RF.Count)] <- 0

Variable.Compare$Sum <- Variable.Compare$RF.Count + Variable.Compare$SW.Count + Variable.Compare$L.Count
table(Variable.Compare$Sum)




Lasso Predict
x2 <- model.matrix(f, Final.Data)
yhat =predict(glmmod, x2)
Lasso.Test <- cbind(Final.Data,yhat)