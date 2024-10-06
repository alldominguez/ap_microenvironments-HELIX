
### --- Install and load packages --- ###
pacman::p_load(tidyverse, mice, miceadds, ggmice, haven,
               patchwork, rio, viridis, visdat, naniar)

################## read the original dataset
library(readr)
helix_db_clean_2 <- read_csv("Data/clean/helix_db_clean_2.csv")
helix_db_clean_2 <- as.data.frame(helix_db_clean_2 )

colnames(helix_db_clean_2)

################## read the dictionary
library(readxl) 

Overview_for_mice_AD <- read_excel("//fs03.isglobal.lan/home/Alan/Overview for mice_AD.xlsx")
dictionary<-as.data.frame(Overview_for_mice_AD)

var_names <- dictionary$Variable

######### we see which variables are already included in the "dictionary"...
var_names_in <- var_names[which(var_names %in% colnames(helix_db_clean_2))]
var_names_in

############## we can add manually the variables that didn't appear... and we want them in the restricted dataset

pos <- which(colnames(helix_db_clean_2) %in% c(var_names_in,"h_cohort"))


length(colnames(helix_db_clean_2))
length(pos)

helix_db_clean_2_sub <- helix_db_clean_2[,pos]

class(helix_db_clean_2_sub)

colnames(helix_db_clean_2_sub)

summary(helix_db_clean_2_sub)

######## convert into a factor
vars_to_fact<-c("e3_asmokyn_p",  "e3_alcpreg_yn",  "e3_ses", "hs_areases_tert_h",  "hs_areases_quint_h", "hs_globalexp2",  "FAS_cat",  "e3_edufc",  
"e3_edupc",  "h_edumc", "h_urban_preg",  "h_parity",  "h_bf","h_cohort", "e3_sex", "h_seabir")

for(l in vars_to_fact){
  helix_db_clean_2_sub[,l] <- as.factor(helix_db_clean_2_sub[,l])
}

summary(helix_db_clean_2_sub)

helix_db_clean_2_sub$hs_areases_tert_h[helix_db_clean_2_sub$hs_areases_tert_h=="null"]<-NA
helix_db_clean_2_sub$hs_areases_quint_h[helix_db_clean_2_sub$hs_areases_quint_h=="null"]<-NA


summary(helix_db_clean_2_sub)

table(dictionary$`Type of transformation (prelim recommendation AD)`)

log_transf_vars<-dictionary$Variable[dictionary$`Type of transformation (prelim recommendation AD)`=="log"]
log_transf_vars<-log_transf_vars[!is.na(log_transf_vars)]

pos<-which(colnames(helix_db_clean_2_sub) %in% log_transf_vars)


summary(helix_db_clean_2_sub[,pos])

### need a c constant


#### libraries needed to run the functions below.

library(e1071)
library(readxl)


#Simple log we find the best constant to add to the log transformation so as to reach symmetry.
skew.score <- function(c, x) (skewness(log(x + c)))^2
transform.log.plus.c.best <- function(x) {
  best.c <- optimise(skew.score, c(0.001, quantile(x,.25,na.rm=T)), x = x)$minimum
  print(best.c)
}


#### log base 2.
skew.score2 <- function(c, x) (skewness(log2(x + c)))^2
transform.log.plus.c2.best <- function(x) {
  best.c <- optimise(skew.score2, c(0.001, quantile(x,.25,na.rm=T)), x = x)$minimum
  print(best.c)
}

colnames(helix_db_clean_2_sub)[59:61]
#hs_mvpa_raw         hs_mvpa        hs_mvpa_alt


c<-transform.log.plus.c.best(helix_db_clean_2_sub[,59])

hist(log(helix_db_clean_2_sub[,59]+c))
hist(log(helix_db_clean_2_sub[,60]+c))
hist(log(helix_db_clean_2_sub[,61]+c))


helix_db_clean_2_sub$hs_sd_wk<-sqrt(helix_db_clean_2_sub$hs_sd_wk)


pos<-59
helix_db_clean_2_sub[,pos]<-log(helix_db_clean_2_sub[,pos]+c)
colnames(helix_db_clean_2_sub)[pos]<-paste0(colnames(helix_db_clean_2_sub)[pos],"_log")

summary(helix_db_clean_2_sub$hs_mvpa_raw_log)

pos<-60
helix_db_clean_2_sub[,pos]<-log(helix_db_clean_2_sub[,pos]+c)
colnames(helix_db_clean_2_sub)[pos]<-paste0(colnames(helix_db_clean_2_sub)[pos],"_log")

summary(helix_db_clean_2_sub[,pos])

pos<-61
helix_db_clean_2_sub[,pos]<-log(helix_db_clean_2_sub[,pos]+c)
colnames(helix_db_clean_2_sub)[pos]<-paste0(colnames(helix_db_clean_2_sub)[pos],"_log")

summary(helix_db_clean_2_sub[,pos])


colnames(helix_db_clean_2_sub)

pos<-30
helix_db_clean_2_sub[,pos]<-log(helix_db_clean_2_sub[,pos])
colnames(helix_db_clean_2_sub)[pos]<-paste0(colnames(helix_db_clean_2_sub)[pos],"_log")

summary(helix_db_clean_2_sub[,pos])

pos<-31
helix_db_clean_2_sub[,pos]<-log(helix_db_clean_2_sub[,pos])
colnames(helix_db_clean_2_sub)[pos]<-paste0(colnames(helix_db_clean_2_sub)[pos],"_log")

summary(helix_db_clean_2_sub[,pos])

pos<-32
helix_db_clean_2_sub[,pos]<-log(helix_db_clean_2_sub[,pos])
colnames(helix_db_clean_2_sub)[pos]<-paste0(colnames(helix_db_clean_2_sub)[pos],"_log")

summary(helix_db_clean_2_sub[,pos])

pos<-48
helix_db_clean_2_sub[,pos] <- log(helix_db_clean_2_sub[,pos])
colnames(helix_db_clean_2_sub)[pos]<-paste0(colnames(helix_db_clean_2_sub)[pos],"_log")

summary(helix_db_clean_2_sub[,pos])



library(mice)
colnames(helix_db_clean_2_sub)
pred=quickpred(helix_db_clean_2_sub,minpuc = 0.4, mincor=0.20, 
               include = c("h_cohort","h_edumc","e3_ses","e3_sex","hs_correct_raven","hs_hitrt2_mean","hs_tmta_responsetime","h_age","e3_bw"),
               exclude=c("HelixID"))
table(apply(pred,1,sum))

names(helix_db_clean_2_sub)[which(apply(pred,1,sum)>25)]



pred<-as.data.frame(pred)


colnames(pred)<-colnames(pred)
rownames(pred)<-colnames(pred)

pred$names<-colnames(pred)

pred<-pred[,c(64,1:63)]

library(openxlsx)
write.xlsx(pred, file="pred_alan_20_04_2022.xlsx")

library(readxl)
pred_alan_20_04_2022 <- read_excel("//fs03.isglobal.lan/home/Alan/pred_alan_20_04_2022_reduced.xlsx")

### here I set to 0 the columns for outcome test B and hrtse

pred_alan_20_04_2022<-as.data.frame(pred_alan_20_04_2022)

colnames(pred_alan_20_04_2022)

rownames(pred_alan_20_04_2022)<-pred_alan_20_04_2022$names


pred_alan_20_04_2022$names<-NULL
pred_alan_20_04_2022$...64<-NULL


pred_alan_20_04_2022$hs_mvpa_alt_log<-NULL

which(rownames(pred_alan_20_04_2022)=="hs_mvpa_alt_log")

pred_alan_20_04_2022<-pred_alan_20_04_2022[-61,]

pred2<-as.matrix(pred_alan_20_04_2022)

helix_db_clean_2_sub$hs_areases_quint_h<-factor(helix_db_clean_2_sub$hs_areases_quint_h)
helix_db_clean_2_sub$hs_areases_tert_h<-factor(helix_db_clean_2_sub$hs_areases_tert_h)

helix_db_clean_2_sub$hs_mvpa_alt_log<-NULL

library(mice)
# Dry run of imputation and set the seed to get reproducible results
imp=mice(helix_db_clean_2_sub,pred=pred2,maxit=0,seed=29834)

#if I have a warning
imp$loggedEvents

#check collinearity
#store<-as.data.frame(imp$loggedEvents)
#fix variables with collinearity if needed in the predictor matrix

#One strategy is to assign 0 when there is much collinerarity in two variables
#In case you want to do that, you need first to have a data frame then change the pred and then matrix again.


##If we feel that a method was no suitable for the imputation for any of the variables. 
#We can redefine manually:


### pmm=predictive mean matching normally works for continuous. 
### logreg , for dichotomous variables
### polyreg, for categorical variables.
##### there are more methods and these are the common and if the variables are well defined in the dataset
##### the mice function assigns automatically the most suitable method for the variables if numeric, or categorical factors.
#### other methods that may be of interest: cart,lda,polr... 
####With also bootstrapped options if never come accross with convergence problems in the imputation models



# check method
method <- as.data.frame(imp$method)
# All seem to be ok
#head(method)


# Now run the imputation with the set variables and methods:
start_time <- Sys.time()
imp=mice(helix_db_clean_2_sub, pred = pred2, method = imp$method, m=20, maxit=10, seed=29834)
end_time <- Sys.time()
end_time - start_time
#Time difference of 5.019929 mins



save( imp,file="imp_vs1_20_04_2022.RData")


#################### post verifications....

load("//fs03.isglobal.lan/home/Alan/imp_vs1_20_04_2022.RData")

library(mice)

#-------- check missing 

# coverting mids object to data frame, this is called the long format. Rows are stacked original dataset with missing+ imputed data 1+ imputed data 2+ ....+ imputed data m.

imp2 <- mice::complete(imp,"long",include=TRUE)

dim(imp2)

dim(imp$data)

#set the number of variables example with 69
#set the number of objects,   the original dataset has 2194 rows   
#Then: in the long format the first element of the first imputed dataset is 2194, the last row or observation of the last dataset (20th) is 46053
for (i in 1:64){
  if (sum(is.na(imp2[1301:27321,i]))>0){  # line numbers from first to last imputed dataset
    print(colnames(imp2[i]))
    print(sum(is.na(imp2[1301:27321,i])))  # line numbers from first to last imputed dataset
  }
}
save(imp2,file="imp.long_vs2_20_04_2022.RData")

load("imp.long_vs2_20_04_2022.RData")

#-------- Density plot

# check density before/after imputation, excluding variables with <2 NA in the original dataset
imp0<-mice::complete(imp,0)
a<-apply(imp0,2,is.na)
b<-as.data.frame(apply(a,2,sum))

vars<-rownames(b)[b$`apply(a, 2, sum)`>1]
out<-lapply(vars, function(x, data){
  densityplot(imp,as.formula(paste0('~', x)));
}, imp)


pdf(file="post_imp_vs2_20_04_2022.pdf")
print(out)
dev.off()




#### quick review if still remains any missing, EXAMPLE with 20 imputed datasets
imp.long_v2 <-imp2
for(k in 1:20){
  cat("imputed dataset ",k,"\n")
  imputed1<- imp.long_v2[imp.long_v2$.imp==k,]
  for(i in 1:64){cat(colnames(imputed1)[i],":",prop.table(table(is.na(imputed1[,i])))*100,"\n")}
  cat("\n")
}



#### Numerica diagnostic tool
# Importantly, these numerical comparisons will be done after adjusting for city or for cohort, to prevent that differences are found when one of the cities/cohorts have very different values than the rest 
#and at the same time they have a high percentage of missing values.

imp0<-complete(imp,0)
imp1<-complete(imp,1)

for (i in 1:dim(imp0)[2]){
  if ((is.numeric(imp1[,i])) && (sum(is.na(imp0[,i]))>(dim(imp1)[1]*0.05))){
    varname<-colnames(imp1[i])
    nmiss<-sum(is.na(imp0[,i]))
    form<-as.formula(paste0(varname,"~h_cohort"))
    lm1<-glm(form,data=imp1)
    m0<-round(mean(lm1$residuals[is.na(imp0[,i])],na.rm = TRUE),3)
    m1<-round(mean(lm1$residuals[!is.na(imp0[,i])],na.rm = TRUE),3)
    dif_m<-round(abs(m0-m1),3)
    sd01<-round(sd(lm1$residuals,na.rm = TRUE),3)
    dif_sd<-ifelse(dif_m>2*sd01,"yes","no")
    a<-var.test(lm1$residuals[is.na(imp0[,i])],lm1$residuals[!is.na(imp0[,i])])
    var_ratio<-a$estimate
    if (dif_sd %in% "yes" || var_ratio<0.5 || var_ratio>2) {
      print("!!!   CHECK THIS VARIABLE   !!!")
      print(paste0(varname, ": N miss=",nmiss))
      print(paste0("             m0=",m0,"            m1=",m1))
      print(paste0("             abs diff=",dif_m, "      sd=", sd01))
      print(paste0("             diff >2SD: ",dif_sd))
      print(paste0("             Variance ratio=",var_ratio))
      imp1$is_miss<-ifelse(is.na(imp0[i]),1,0)
      print(densityplot(~imp1[,i] | imp1$h_cohort,group=imp1$is_miss,xlab=colnames(imp1[i])))
    }
  }
}



# no missing data

# If you want to create new variables for your analysis (or transform some variables as below for IQR scaling), you'll need to create them in all the imputed datasets (using data.frame object instead of mids object).
# You can do that by stacking all the imputed datasets one after the other
# with complete(imp,action="long"), with a variable .imp indication imputation 
# number. if you want to backtransform transformed variables, this would be 
# the place to do it by doing:
#imp.long=complete(imp,action="long",include=T)
#table(imp.long$.imp)
### Example:
#imp.long$newvar=imp.long$CONC_FOUND.MBzP_log + imp.long$CONC_FOUND.MEHHP_log + imp.long$MEP_C_log
# after having created a new variable, you need to transform your stacked dataset to a mids object
#imp.v3=as.mids(imp.long)

load("imp.long_vs2_20_04_2022.RData")

imp.long_v2<-imp2

#### Example of the mother's bmi
imp.long_v2$mbmi<-imp.long_v2$e3_mweight/(imp.long_v2$e3_mheight/100)^2

summary(imp.long_v2$mbmi)



save(imp.long_v2,file="imp.long_v2_20_04_2022.RData")

#------------------------ IQR scaling ---------------------------------#

load("imp.long_v2_20_04_2022.RData")


####### IQR scaling (exposures and covariates) to be done after imputation

# as for above, you need to perform the scaling using data.frame object and then convert it into a mids object

# copy your data.frame (see above for the creation of imp.long)
imp.long.IQR_v2= imp.long_v2

# calculate IQR from 1st imputed dataset
imputed1<- imp.long_v2[imp.long_v2 $.imp=="1",] 




for (i in 1:dim(imp.long.IQR_v2)[2]) {
  if (is.numeric(imp.long.IQR_v2[,i]) & length(table(imp.long.IQR_v2[,i]))>2 & !colnames(imp.long.IQR_v2)[i]%in%c(".imp", ".id","HelixID","hs_correct_raven","hs_sum_domhand","hs_laterality","hs_tmta_responsetime", "hs_tmtb_responsetime", "hs_hitrt2_mean", "hs_hitrtse")) {
    # iqr of first imputed dataset
    iqr.first=quantile(imputed1[,i],.75,na.rm=T)-quantile(imputed1[,i],.25,na.rm=T)
    message(colnames(imp.long.IQR_v2)[i])
    newvar <- paste0(colnames(imp.long.IQR_v2)[i],"_iqr")
    imp.long.IQR_v2[,newvar] <- NA
    for (j in 1:nrow(imp.long.IQR_v2)){
      if (!is.na(imp.long.IQR_v2[j,i]==F)) {
        imp.long.IQR_v2[j,newvar]=imp.long.IQR_v2[j,i]/iqr.first
      }
    }
  }
}

# verify that no missing created by IQR scaling
imp.long.IQR_v2$HelixID<-rep(helix_db_clean_2$HelixID,21)


######### remember about setting the correct number dimensions!

imp.long.IQR_v2<-imp.long.IQR_v2[order(imp.long.IQR_v2$.imp),]
for (i in 1:dim(imp.long.IQR_v2)[2]){
  if (sum(is.na(imp.long.IQR_v2[1301:27321,i]))>0){ # line numbers from first to last imputed dataset
    print(colnames(imp.long.IQR_v2[i]))
    print(sum(is.na(imp.long.IQR_v2[1301:27321,i])))  # line numbers from first to last imputed dataset
  }
}


##### back to original missing values outcomes

colnames(imp.long.IQR_v2)

outcomes<-colnames(helix_db_clean_2_sub_outcomes)

ids<-imp.long.IQR_v2$HelixID[is.na(imp.long.IQR_v2$hs_correct_raven) & imp.long.IQR_v2$.imp==0]
ids

imp.long.IQR_v2$hs_correct_raven[imp.long.IQR_v2$HelixID %in% ids]<-NA

ids<-imp.long.IQR_v2$HelixID[is.na(imp.long.IQR_v2$hs_sum_domhand) & imp.long.IQR_v2$.imp==0]
ids

imp.long.IQR_v2$hs_sum_domhand[imp.long.IQR_v2$HelixID %in% ids]<-NA

ids<-imp.long.IQR_v2$HelixID[is.na(imp.long.IQR_v2$hs_laterality) & imp.long.IQR_v2$.imp==0]
ids

imp.long.IQR_v2$hs_laterality[imp.long.IQR_v2$HelixID %in% ids]<-NA

ids<-imp.long.IQR_v2$HelixID[is.na(imp.long.IQR_v2$hs_tmta_responsetime) & imp.long.IQR_v2$.imp==0]
ids

imp.long.IQR_v2$hs_tmta_responsetime[imp.long.IQR_v2$HelixID %in% ids]<-NA

ids<-imp.long.IQR_v2$HelixID[is.na(imp.long.IQR_v2$hs_tmtb_responsetime) & imp.long.IQR_v2$.imp==0]
ids

imp.long.IQR_v2$hs_tmtb_responsetime[imp.long.IQR_v2$HelixID %in% ids]<-NA

ids<-imp.long.IQR_v2$HelixID[is.na(imp.long.IQR_v2$hs_hitrt2_mean) & imp.long.IQR_v2$.imp==0]
ids

imp.long.IQR_v2$hs_hitrt2_mean[imp.long.IQR_v2$HelixID %in% ids]<-NA

ids<-imp.long.IQR_v2$HelixID[is.na(imp.long.IQR_v2$hs_hitrtse) & imp.long.IQR_v2$.imp==0]
ids

imp.long.IQR_v2$hs_hitrtse[imp.long.IQR_v2$HelixID %in% ids]<-NA


# you must convert this data.frame into a mids object to perform your analyses. Check before that the first variables of your data.frame are .imp and .id. if not: reorder them
colnames(imp.long.IQR_v2) # .


imp.long.IQR_v2$HelixID<-as.factor(imp.long.IQR_v2$HelixID)
imp.IQR_v2<-as.mids(imp.long.IQR_v2)


# you can save your data.frame after IQR scaling   
save(imp.long.IQR_v2,file="imp.long.IQR_v2_20_04_2022.RData")

# you can save your mids object after IQR scaling (this will be the dataset you need to run analyses)
#save(imp.IQR_v2,file="PATH//imp.IQR_v2.RData")

save( imp.IQR_v2,file="imp.IQR_v2_20_04_2022.RData")

load("//fs03.isglobal.lan/home/Alan/imp.long.IQR_v2_20_04_2022.RData")

colnames(imp.long.IQR_v2)[c(64,104)]<-c("hs_sd_wk_sqrt","hs_sd_wk_sqrt_iqr")

library(mice)
imp.IQR_v2<-as.mids(imp.long.IQR_v2)

save( imp.IQR_v2,file="imp.IQR_v2_21_04_2022.RData")  ### this version is just a change in column names for variable hs_sd_wk


load("//fs03.isglobal.lan/home/Alan/imp.long.IQR_v2_20_04_2022.RData")

imp.long.IQR_v2$task_shifting_score<-(imp.long.IQR_v2$hs_tmtb_responsetime - imp.long.IQR_v2$hs_tmta_responsetime ) / imp.long.IQR_v2$hs_tmta_responsetime

summary(imp.long.IQR_v2$task_shifting_score)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-0.8090 -0.1459  0.0157  0.0958  0.2385  4.6271     672

library(mice)
imp.IQR_v2<-as.mids(imp.long.IQR_v2)

save( imp.IQR_v2,file="imp.IQR_v2_27_04_2022.RData")  ### this version is just adds the new outcome variable task shifting score

