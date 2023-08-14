
library(foreign)
library(miceadds)
library(glmnet)
library(haven)
#variable "controls"
# control_vars <- paste("street1848_dummy + ship1849_dummy + delta31_37 + ln_pop1849civil+ ln_pop1849military+fac1849_total2_pc + county_mining + county_landownership + pop1849_young_pc+ edu1849_pri_enrol+ dist1")

#####second column
# Construct the formula as a character string
# formula_str_ols <- paste("delta49_71 ~", paste("rail1848 +addrail71+"), paste(control_vars))
# formula_ols <- as.formula(formula_str_ols)
# formula_str_first_stage <- paste("rail1848 ~", paste("slc1848 + addrail71+"), paste(control_vars))
# formula_first_stage <- as.formula(formula_str_first_stage)
# formula_str_second_stage <- paste("delta49_71 ~", paste("rail1848_hat +addrail71+"), paste(control_vars))
# formula_second_stage <- as.formula(formula_str_second_stage);
# formula_str_reduce <- paste("delta49_71 ~", paste("slc1848 +addrail71+"), paste(control_vars))
# formula_reduce <- as.formula(formula_str_reduce);
##################################################################
##################################################################
##################introduce cric_application function#############
##################################################################


###########################################################################################
##############################column 1#####################################################
###########################################################################################
data <- read_dta("~/Documents/A_JMP/JMP_Yukun/application/railroad/hornung-rail-cross-section.dta");
attach(data)

# Subset the data based on conditions
data1 <- data[ abs(delta31_37) < 0.1,];
data2 <- data1[data1$node1848==0, ];
na_mat <- is.na(data2$areachange1)
data3 <- data2[na_mat, ];
attach(data3)
used_data <- cbind(delta31_37, rail1848,slc1848,street1848_dummy, ship1849_dummy,delta16_31,ln_pop1849civil,ln_pop1849military,
                   fac1849_total2_pc, county_mining, county_landownership, 
                   pop1849_young_pc, edu1849_pri_enrol, dist1);
data_clean <-na.omit(used_data)
data4 <- data.frame(data_clean);

# clus <- data4$kreiskey1849;
# lm.cluster( data=data4, formula = formula_ols, cluster = clus) ##panel A
# lm.cluster( data=data4, formula = formula_first_stage, cluster = clus) ##panel B
# first_stage <- lm( formula = formula_first_stage,data=data3) 
# rail1848_hat <- predict(first_stage);  
# lm.cluster( data=data4, formula = formula_second_stage, cluster = clus) ##panel C
# lm.cluster( data=data4, formula = formula_reduce, cluster = clus) ##panel C

attach(data4)
X  = cbind(street1848_dummy,  ship1849_dummy,county_mining, delta16_31, ln_pop1849civil, ln_pop1849military,
           fac1849_total2_pc,  county_landownership, pop1849_young_pc, edu1849_pri_enrol, dist1);
Z  = slc1848;
Y  = delta31_37;
D  = rail1848;
# ########make X high-dimensional
# X  = cbind(X,
#            X[,4:11]^2,
#            X[,5]*X[,6],X[,5]*X[,7],X[,5]*X[,8],X[,5]*X[,9],X[,5]*X[,10],X[,5]*X[,11],
#            X[,6]*X[,7],X[,6]*X[,8],X[,6]*X[,9],X[,6]*X[,10],X[,6]*X[,11],
#            X[,7]*X[,8],X[,7]*X[,9],X[,7]*X[,10],X[,7]*X[,11],
#            X[,8]*X[,9],X[,8]*X[,10],X[,8]*X[,11],
#            X[,9]*X[,10],X[,9]*X[,11],
#            X[,10]*X[,11],
#            X[,4:11]^3,
#            X[,4]^2*X[,6],X[,4]^2*X[,7],X[,4]^2*X[,8],X[,4]^2*X[,9],X[,4]^2*X[,10],X[,4]^2*X[,11],
#            X[,5]^2*X[,6],X[,5]^2*X[,7],X[,5]^2*X[,8],X[,5]^2*X[,9],X[,5]^2*X[,10],X[,5]^2*X[,11],
#            X[,6]^2*X[,5],X[,6]^2*X[,7],X[,6]^2*X[,8],X[,6]^2*X[,9],X[,6]^2*X[,10],X[,6]^2*X[,11],
#            X[,7]^2*X[,5],X[,7]^2*X[,6],X[,7]^2*X[,8],X[,7]^2*X[,9],X[,7]^2*X[,10],X[,7]^2*X[,11],
#            X[,8]^2*X[,5],X[,8]^2*X[,6],X[,8]^2*X[,7],X[,8]^2*X[,9],X[,8]^2*X[,10],X[,8]^2*X[,11],
#            X[,9]^2*X[,5], X[,9]^2*X[,6], X[,9]^2*X[,7], X[,9]^2*X[,8],X[,9]^2*X[,10],X[,9]^2*X[,11],
#            X[,10]^2*X[,5],X[,10]^2*X[,6],X[,10]^2*X[,7],X[,10]^2*X[,8],X[,10]^2*X[,9],X[,10]^2*X[,11],
#            X[,11]^2*X[,5],X[,11]^2*X[,6],X[,11]^2*X[,7],X[,11]^2*X[,8],X[,11]^2*X[,9],X[,11]^2*X[,10],
#            X[,1]*X[,4:11], X[,2]*X[,4:11],X[,3]*X[,4:11],
#            # X[,1]*X[,4:11]^3, X[,2]*X[,4:11]^3,X[,3]*X[,4:11]^3,
#            # X[,4:11]^4,
#            X[,4]^3*X[,5],X[,4]^3*X[,6],X[,4]^3*X[,7],X[,4]^3*X[,8],X[,4]^3*X[,9],X[,4]^3*X[,10],X[,4]^3*X[,11],
#            X[,5]^3*X[,6],X[,5]^3*X[,7],X[,5]^3*X[,8],X[,5]^3*X[,9],X[,5]^3*X[,10],X[,5]^3*X[,11],
#            X[,6]^3*X[,7],X[,6]^3*X[,8],X[,6]^3*X[,9],X[,6]^3*X[,10],X[,6]^3*X[,11]
#            # X[,7]^3*X[,8],X[,7]^3*X[,9],X[,7]^3*X[,10],X[,7]^3*X[,11]
#            # X[,8]^3*X[,9],X[,8]^3*X[,10],X[,8]^3*X[,11],
#            # X[,9]^3*X[,10],X[,9]^3*X[,11],
#            # X[,10]^3*X[,11]
#            # 
# )

X  = cbind(X,
           X[,4:11]^2,
           X[,5]*X[,6],X[,5]*X[,7],X[,5]*X[,8],X[,5]*X[,9],X[,5]*X[,10],X[,5]*X[,11],
           X[,6]*X[,7],X[,6]*X[,8],X[,6]*X[,9],X[,6]*X[,10],X[,6]*X[,11],
           X[,7]*X[,8],X[,7]*X[,9],X[,7]*X[,10],X[,7]*X[,11],
           X[,8]*X[,9],X[,8]*X[,10],X[,8]*X[,11],
           X[,9]*X[,10],X[,9]*X[,11],
           X[,10]*X[,11],
           X[,4:11]^3,
           X[,4]^2*X[,6],X[,4]^2*X[,7],X[,4]^2*X[,8],X[,4]^2*X[,9],X[,4]^2*X[,10],X[,4]^2*X[,11],
           X[,5]^2*X[,6],X[,5]^2*X[,7],X[,5]^2*X[,8],X[,5]^2*X[,9],X[,5]^2*X[,10],X[,5]^2*X[,11],
           X[,6]^2*X[,5],X[,6]^2*X[,7],X[,6]^2*X[,8],X[,6]^2*X[,9],X[,6]^2*X[,10],X[,6]^2*X[,11],
           X[,7]^2*X[,5],X[,7]^2*X[,6],X[,7]^2*X[,8],X[,7]^2*X[,9],X[,7]^2*X[,10],X[,7]^2*X[,11],
           X[,8]^2*X[,5],X[,8]^2*X[,6],X[,8]^2*X[,7],X[,8]^2*X[,9],X[,8]^2*X[,10],X[,8]^2*X[,11],
           X[,9]^2*X[,5], X[,9]^2*X[,6], X[,9]^2*X[,7], X[,9]^2*X[,8],X[,9]^2*X[,10],X[,9]^2*X[,11],
           X[,10]^2*X[,5],X[,10]^2*X[,6],X[,10]^2*X[,7],X[,10]^2*X[,8],X[,10]^2*X[,9],X[,10]^2*X[,11],
           X[,11]^2*X[,5],X[,11]^2*X[,6],X[,11]^2*X[,7],X[,11]^2*X[,8],X[,11]^2*X[,9],X[,11]^2*X[,10],
           X[,1]*X[,4:11], X[,2]*X[,4:11],X[,3]*X[,4:11],
           # X[,1]*X[,5:12]^2, X[,2]*X[,5:12]^2,X[,3]*X[,5:12]^2,X[,4]*X[,5:12]^2,
           X[,1]*X[,4:11]^3, X[,2]*X[,4:11]^3,X[,3]*X[,4:11]^3,
           X[,4:11]^4,
           X[,4]^3*X[,5],X[,4]^3*X[,6],X[,4]^3*X[,7],X[,4]^3*X[,8],X[,4]^3*X[,9],X[,4]^3*X[,10],X[,4]^3*X[,11],
           X[,5]^3*X[,6],X[,5]^3*X[,7],X[,5]^3*X[,8],X[,5]^3*X[,9],X[,5]^3*X[,10],X[,5]^3*X[,11],
           X[,6]^3*X[,7],X[,6]^3*X[,8],X[,6]^3*X[,9],X[,6]^3*X[,10],X[,6]^3*X[,11],
           X[,7]^3*X[,8],X[,7]^3*X[,9],X[,7]^3*X[,10],X[,7]^3*X[,11],
           X[,8]^3*X[,9],X[,8]^3*X[,10],X[,8]^3*X[,11],
           X[,9]^3*X[,10],X[,9]^3*X[,11],
           X[,10]^3*X[,11]
           
)

K=3;  ###0.41073
#K=7;  ####0.34762 ####0.41868 #0.28107
N = dim(X)[1];
dimX = dim(X)[2];



subsample_numerator       = NULL;
subsample_denominator     = NULL;
Y_Xbeta22_all             = NULL;
Y_beta21_Xbeta22_all      = NULL;
Xgamma_all                = NULL;
beta11_Xbeta12_all        = NULL;
Xbeta12_all               = NULL;



##############################################################
# LASSO OLS OF D on Z and X
##############################################################
#cvfit = cv.glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, standardize = TRUE, intercept = TRUE);
#lambda1 = cvfit$lambda.min
lambda1 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
#lassologitresult1 = glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, lambda=lambda1, alpha=1, family="binomial" );
lassologitresult1 = glmnet( cbind(Z,X), D, lambda=lambda1, alpha=1);

hat_beta11_beta12 = matrix(coef(lassologitresult1),,1);

##############################################################
# LASSO logistic OF Z on  X
##############################################################
# cvfit = cv.glmnet( auxiliaryX, auxiliaryZ, standardize = TRUE, intercept = TRUE );
#lambda2 = cvfit$lambda.min
lambda2 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
lassologitresult2 = glmnet( X, Z, lambda=lambda2, alpha=1, family="binomial" );
hat_gamma = matrix(coef(lassologitresult2),,1);

##############################################################
# LASSO OLS OF Y on  Z and X
##############################################################
#cvfit = cv.glmnet( cbind(auxiliaryZ, auxiliaryX), auxiliaryY, standardize = TRUE, intercept = TRUE );
#lambda3 = cvfit$lambda.min

lambda3 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
lassologitresult3 = glmnet( cbind(Z, X), Y, lambda=lambda3, alpha=1 );
hat_beta21_beta22 = matrix(coef(lassologitresult3),,1);

##############################################################
# COMPUTE Y - Xbeta22, Xgamma, AND beta11+Xbeta12
##############################################################
Y_Xbeta22_all        =   Y - hat_beta21_beta22[1] -X %*% hat_beta21_beta22[3:(dimX+2)];
Y_beta21_Xbeta22_all =   Y-hat_beta21_beta22[1]-hat_beta21_beta22[2]-X %*% hat_beta21_beta22[3:(dimX+2)] ;
Xgamma_all           =   cbind(1,X)%*%hat_gamma;
beta11_Xbeta12_all   =   hat_beta11_beta12[1]+hat_beta11_beta12[2]+X%*%hat_beta11_beta12[3:(dimX+2)];
Xbeta12_all          =  hat_beta11_beta12[1]+X%*%hat_beta11_beta12[3:(dimX+2)];



##############################################################
########## compute two subsample mean#########################
##############################################################
#subsample_denominator = rbind(subsample_denominator,plogis(beta11_Xbeta12)-plogis(Xbeta12)+matrix(mainZ,,1)*(mainD-plogis(beta11_Xbeta12))/plogis(Xgamma)-matrix(1-mainZ,,1)*(mainD-plogis(Xbeta12))/(1-plogis(Xgamma)));
sample_denominator = hat_beta11_beta12[2]+matrix(Z,,1)*(D-beta11_Xbeta12_all)/plogis(Xgamma_all)-matrix(1-Z,,1)*(D-Xbeta12_all)/(1-plogis(Xgamma_all));
sample_numerator   = hat_beta21_beta22[2]+matrix(Z,,1)*Y_beta21_Xbeta22_all/plogis(Xgamma_all)-matrix(1-Z,,1)*Y_Xbeta22_all/(1-plogis(Xgamma_all));


######just for inference use



theta_hat_temp = sum(sample_numerator)/sum(sample_denominator);

#########################################################################
phi_denominator_hat = hat_beta11_beta12[2]+matrix(Z,,1)*(D-beta11_Xbeta12_all)/plogis(Xgamma_all)-matrix(1-Z,,1)*(D-Xbeta12_all)/(1-plogis(Xgamma_all));
phi_numerator_hat   = hat_beta21_beta22[2]+matrix(Z,,1)*Y_beta21_Xbeta22_all/plogis(Xgamma_all)-matrix(1-Z,,1)*Y_Xbeta22_all/(1-plogis(Xgamma_all));
phi_hat             = phi_denominator_hat*theta_hat_temp - phi_numerator_hat;





Gamma = sum(phi_hat^2)/N;

J =  sum(phi_denominator_hat)/N;





sigma2 = Gamma / J^2;
theta_SD_zeroway = sqrt( sigma2 / N );


###end of theta0
CI1_lower  = theta_hat_temp - 2*qnorm(0.975)* theta_SD_zeroway;
CI1_upper  = theta_hat_temp + 2*qnorm(0.975)* theta_SD_zeroway;

theta_hat  = theta_hat_temp;
#} ####end of randomization

theta_hat1 = mean(theta_hat)

print(c(median(CI1_lower), median(CI1_upper), median(CI1_upper)-median(CI1_lower), theta_hat1))


###########################################################################################
##############################column 2#####################################################
###########################################################################################
data <- read_dta("~/Documents/A_JMP/JMP_Yukun/application/railroad/hornung-rail-cross-section.dta");
attach(data)

# Subset the data based on conditions
data1 <- data[ abs(delta49_71) < 0.1,];
data2 <- data1[data1$node1848==0, ];
data3 <- data2[ data2$areachange2 > 1871, ];
attach(data3)
used_data <- cbind(addrail71,delta49_71, rail1848,slc1848,street1848_dummy, ship1849_dummy,delta31_37,ln_pop1849civil,ln_pop1849military,
                   fac1849_total2_pc, county_mining, county_landownership,
                   pop1849_young_pc, edu1849_pri_enrol, dist1,kreiskey1849,delta49_52,delta52_55,delta55_58,delta58_61,delta61_64,
                   delta64_67,delta67_71,addrail52,addrail55,addrail58,addrail61,addrail64,addrail67);
data_clean <-na.omit(used_data)
data4 <- data.frame(data_clean);

# clus <- data4$kreiskey1849;
# lm.cluster( data=data4, formula = formula_ols, cluster = clus) ##panel A
# lm.cluster( data=data4, formula = formula_first_stage, cluster = clus) ##panel B
# first_stage <- lm( formula = formula_first_stage,data=data3)
# rail1848_hat <- predict(first_stage);
# lm.cluster( data=data4, formula = formula_second_stage, cluster = clus) ##panel C
# lm.cluster( data=data4, formula = formula_reduce, cluster = clus) ##panel C

attach(data4)
X  = cbind(addrail71,street1848_dummy,  ship1849_dummy,county_mining, delta31_37, ln_pop1849civil, ln_pop1849military,
           fac1849_total2_pc,  county_landownership, pop1849_young_pc, edu1849_pri_enrol, dist1);
Z  = slc1848;
Y  = delta49_71;
D  = rail1848;
########make X high-dimensional
X  = cbind(X,
           X[,5:12]^2,
           X[,5]*X[,6],X[,5]*X[,7],X[,5]*X[,8],X[,5]*X[,9],X[,5]*X[,10],X[,5]*X[,11],X[,5]*X[,12],
           X[,6]*X[,7],X[,6]*X[,8],X[,6]*X[,9],X[,6]*X[,10],X[,6]*X[,11],X[,6]*X[,12],
           X[,7]*X[,8],X[,7]*X[,9],X[,7]*X[,10],X[,7]*X[,11],X[,7]*X[,12],
           X[,8]*X[,9],X[,8]*X[,10],X[,8]*X[,11],X[,8]*X[,12],
           X[,9]*X[,10],X[,9]*X[,11],X[,9]*X[,12],
           X[,10]*X[,11],X[,10]*X[,12],
           X[,11]*X[,12],
           X[,5:12]^3,
           X[,5]^2*X[,6],X[,5]^2*X[,7],X[,5]^2*X[,8],X[,5]^2*X[,9],X[,5]^2*X[,10],X[,5]^2*X[,11],X[,5]^2*X[,12],
           X[,6]^2*X[,5],X[,6]^2*X[,7],X[,6]^2*X[,8],X[,6]^2*X[,9],X[,6]^2*X[,10],X[,6]^2*X[,11],X[,6]^2*X[,12],
           X[,7]^2*X[,5],X[,7]^2*X[,6],X[,7]^2*X[,8],X[,7]^2*X[,9],X[,7]^2*X[,10],X[,7]^2*X[,11],X[,7]^2*X[,12],
           X[,8]^2*X[,5],X[,8]^2*X[,6],X[,8]^2*X[,7],X[,8]^2*X[,9],X[,8]^2*X[,10],X[,8]^2*X[,11],X[,8]^2*X[,12],
           X[,9]^2*X[,5], X[,9]^2*X[,6], X[,9]^2*X[,7], X[,9]^2*X[,8],X[,9]^2*X[,10],X[,9]^2*X[,11],X[,9]^2*X[,12],
           X[,10]^2*X[,5],X[,10]^2*X[,6],X[,10]^2*X[,7],X[,10]^2*X[,8],X[,10]^2*X[,9],X[,10]^2*X[,11],X[,10]^2*X[,12],
           X[,11]^2*X[,5],X[,11]^2*X[,6],X[,11]^2*X[,7],X[,11]^2*X[,8],X[,11]^2*X[,9],X[,11]^2*X[,10],X[,11]^2*X[,12],
           X[,12]^2*X[,5],X[,12]^2*X[,6],X[,12]^2*X[,7],X[,12]^2*X[,8],X[,12]^2*X[,9],X[,12]^2*X[,10],X[,12]^2*X[,11],
           X[,1]*X[,5:12], X[,2]*X[,5:12],X[,3]*X[,5:12],X[,4]*X[,5:12],
           # X[,1]*X[,5:12]^2, X[,2]*X[,5:12]^2,X[,3]*X[,5:12]^2,X[,4]*X[,5:12]^2,
           X[,1]*X[,5:12]^3, X[,2]*X[,5:12]^3,X[,3]*X[,5:12]^3,X[,4]*X[,5:12]^3,
           X[,5:12]^4,
           X[,5]^3*X[,6],X[,5]^3*X[,7],X[,5]^3*X[,8],X[,5]^3*X[,9],X[,5]^3*X[,10],X[,5]^3*X[,11],X[,5]^3*X[,12],
           X[,6]^3*X[,7],X[,6]^3*X[,8],X[,6]^3*X[,9],X[,6]^3*X[,10],X[,6]^3*X[,11],X[,6]^3*X[,12],
           X[,7]^3*X[,8],X[,7]^3*X[,9],X[,7]^3*X[,10],X[,7]^3*X[,11],X[,7]^3*X[,12],
           X[,8]^3*X[,9],X[,8]^3*X[,10],X[,8]^3*X[,11],X[,8]^3*X[,12],
           X[,9]^3*X[,10],X[,9]^3*X[,11],X[,9]^3*X[,12],
           X[,10]^3*X[,11],X[,10]^3*X[,12],
           X[,11]^3*X[,12]
)


K=3;  ###0.41073
#K=7;  ####0.34762 ####0.41868 #0.28107
N = dim(X)[1];
dimX = dim(X)[2];



subsample_numerator       = NULL;
subsample_denominator     = NULL;
Y_Xbeta22_all             = NULL;
Y_beta21_Xbeta22_all      = NULL;
Xgamma_all                = NULL;
beta11_Xbeta12_all        = NULL;
Xbeta12_all               = NULL;



##############################################################
# LASSO OLS OF D on Z and X
##############################################################
#cvfit = cv.glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, standardize = TRUE, intercept = TRUE);
#lambda1 = cvfit$lambda.min
lambda1 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
#lassologitresult1 = glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, lambda=lambda1, alpha=1, family="binomial" );
lassologitresult1 = glmnet( cbind(Z,X), D, lambda=lambda1, alpha=1);

hat_beta11_beta12 = matrix(coef(lassologitresult1),,1);

##############################################################
# LASSO logistic OF Z on  X
##############################################################
# cvfit = cv.glmnet( auxiliaryX, auxiliaryZ, standardize = TRUE, intercept = TRUE );
#lambda2 = cvfit$lambda.min
lambda2 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
lassologitresult2 = glmnet( X, Z, lambda=lambda2, alpha=1, family="binomial" );
hat_gamma = matrix(coef(lassologitresult2),,1);

##############################################################
# LASSO OLS OF Y on  Z and X
##############################################################
#cvfit = cv.glmnet( cbind(auxiliaryZ, auxiliaryX), auxiliaryY, standardize = TRUE, intercept = TRUE );
#lambda3 = cvfit$lambda.min

lambda3 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
lassologitresult3 = glmnet( cbind(Z, X), Y, lambda=lambda3, alpha=1 );
hat_beta21_beta22 = matrix(coef(lassologitresult3),,1);

##############################################################
# COMPUTE Y - Xbeta22, Xgamma, AND beta11+Xbeta12
##############################################################
Y_Xbeta22_all        =   Y - hat_beta21_beta22[1] -X %*% hat_beta21_beta22[3:(dimX+2)];
Y_beta21_Xbeta22_all =   Y-hat_beta21_beta22[1]-hat_beta21_beta22[2]-X %*% hat_beta21_beta22[3:(dimX+2)] ;
Xgamma_all           =   cbind(1,X)%*%hat_gamma;
beta11_Xbeta12_all   =   hat_beta11_beta12[1]+hat_beta11_beta12[2]+X%*%hat_beta11_beta12[3:(dimX+2)];
Xbeta12_all          =  hat_beta11_beta12[1]+X%*%hat_beta11_beta12[3:(dimX+2)];



##############################################################
########## compute two subsample mean#########################
##############################################################
#subsample_denominator = rbind(subsample_denominator,plogis(beta11_Xbeta12)-plogis(Xbeta12)+matrix(mainZ,,1)*(mainD-plogis(beta11_Xbeta12))/plogis(Xgamma)-matrix(1-mainZ,,1)*(mainD-plogis(Xbeta12))/(1-plogis(Xgamma)));
sample_denominator = hat_beta11_beta12[2]+matrix(Z,,1)*(D-beta11_Xbeta12_all)/plogis(Xgamma_all)-matrix(1-Z,,1)*(D-Xbeta12_all)/(1-plogis(Xgamma_all));
sample_numerator   = hat_beta21_beta22[2]+matrix(Z,,1)*Y_beta21_Xbeta22_all/plogis(Xgamma_all)-matrix(1-Z,,1)*Y_Xbeta22_all/(1-plogis(Xgamma_all));


######just for inference use



theta_hat_temp = sum(sample_numerator)/sum(sample_denominator);

#########################################################################
phi_denominator_hat = hat_beta11_beta12[2]+matrix(Z,,1)*(D-beta11_Xbeta12_all)/plogis(Xgamma_all)-matrix(1-Z,,1)*(D-Xbeta12_all)/(1-plogis(Xgamma_all));
phi_numerator_hat   = hat_beta21_beta22[2]+matrix(Z,,1)*Y_beta21_Xbeta22_all/plogis(Xgamma_all)-matrix(1-Z,,1)*Y_Xbeta22_all/(1-plogis(Xgamma_all));
phi_hat             = phi_denominator_hat*theta_hat_temp - phi_numerator_hat;





Gamma = sum(phi_hat^2)/N;

J =  sum(phi_denominator_hat)/N;





sigma2 = Gamma / J^2;
theta_SD_zeroway = sqrt( sigma2 / N );


###end of theta0
CI2_lower  = theta_hat_temp - 2*qnorm(0.975)* theta_SD_zeroway;
CI2_upper  = theta_hat_temp + 2*qnorm(0.975)* theta_SD_zeroway;

theta_hat2  = theta_hat_temp;
#} ####end of randomization



# 
# 
###########################################################################################
##############################column 3#####################################################
###########################################################################################
data <- read_dta("~/Documents/A_JMP/JMP_Yukun/application/railroad/hornung-rail-cross-section.dta");
attach(data)

# Subset the data based on conditions
data1 <- data[ abs(delta49_52) < 0.1,];
data2 <- data1[data1$node1848==0, ];
data3 <- data2[ data2$areachange2 != 1852, ];
attach(data3)
used_data <- cbind(addrail52,delta49_52, rail1848,slc1848,street1848_dummy, ship1849_dummy,delta31_37,ln_pop1849civil,ln_pop1849military,
                   fac1849_total2_pc, county_mining, county_landownership,
                   pop1849_young_pc, edu1849_pri_enrol, dist1);
data_clean <-na.omit(used_data)
data4 <- data.frame(data_clean);

# clus <- data4$kreiskey1849;
# lm.cluster( data=data4, formula = formula_ols, cluster = clus) ##panel A
# lm.cluster( data=data4, formula = formula_first_stage, cluster = clus) ##panel B
# first_stage <- lm( formula = formula_first_stage,data=data3)
# rail1848_hat <- predict(first_stage);
# lm.cluster( data=data4, formula = formula_second_stage, cluster = clus) ##panel C
# lm.cluster( data=data4, formula = formula_reduce, cluster = clus) ##panel C

attach(data4)
X  = cbind(addrail52,street1848_dummy,  ship1849_dummy,county_mining, delta31_37, ln_pop1849civil, ln_pop1849military,
           fac1849_total2_pc,  county_landownership, pop1849_young_pc, edu1849_pri_enrol, dist1);
Z  = slc1848;
Y  = delta49_52;
D  = rail1848;
########make X high-dimensional
X  = cbind(X,
           X[,5:12]^2,
           X[,5]*X[,6],X[,5]*X[,7],X[,5]*X[,8],X[,5]*X[,9],X[,5]*X[,10],X[,5]*X[,11],X[,5]*X[,12],
           X[,6]*X[,7],X[,6]*X[,8],X[,6]*X[,9],X[,6]*X[,10],X[,6]*X[,11],X[,6]*X[,12],
           X[,7]*X[,8],X[,7]*X[,9],X[,7]*X[,10],X[,7]*X[,11],X[,7]*X[,12],
           X[,8]*X[,9],X[,8]*X[,10],X[,8]*X[,11],X[,8]*X[,12],
           X[,9]*X[,10],X[,9]*X[,11],X[,9]*X[,12],
           X[,10]*X[,11],X[,10]*X[,12],
           X[,11]*X[,12],
           X[,5:12]^3,
           X[,5]^2*X[,6],X[,5]^2*X[,7],X[,5]^2*X[,8],X[,5]^2*X[,9],X[,5]^2*X[,10],X[,5]^2*X[,11],X[,5]^2*X[,12],
           X[,6]^2*X[,5],X[,6]^2*X[,7],X[,6]^2*X[,8],X[,6]^2*X[,9],X[,6]^2*X[,10],X[,6]^2*X[,11],X[,6]^2*X[,12],
           X[,7]^2*X[,5],X[,7]^2*X[,6],X[,7]^2*X[,8],X[,7]^2*X[,9],X[,7]^2*X[,10],X[,7]^2*X[,11],X[,7]^2*X[,12],
           X[,8]^2*X[,5],X[,8]^2*X[,6],X[,8]^2*X[,7],X[,8]^2*X[,9],X[,8]^2*X[,10],X[,8]^2*X[,11],X[,8]^2*X[,12],
           X[,9]^2*X[,5], X[,9]^2*X[,6], X[,9]^2*X[,7], X[,9]^2*X[,8],X[,9]^2*X[,10],X[,9]^2*X[,11],X[,9]^2*X[,12],
           X[,10]^2*X[,5],X[,10]^2*X[,6],X[,10]^2*X[,7],X[,10]^2*X[,8],X[,10]^2*X[,9],X[,10]^2*X[,11],X[,10]^2*X[,12],
           X[,11]^2*X[,5],X[,11]^2*X[,6],X[,11]^2*X[,7],X[,11]^2*X[,8],X[,11]^2*X[,9],X[,11]^2*X[,10],X[,11]^2*X[,12],
           X[,12]^2*X[,5],X[,12]^2*X[,6],X[,12]^2*X[,7],X[,12]^2*X[,8],X[,12]^2*X[,9],X[,12]^2*X[,10],X[,12]^2*X[,11],
           X[,1]*X[,5:12], X[,2]*X[,5:12],X[,3]*X[,5:12],X[,4]*X[,5:12],
           # X[,1]*X[,5:12]^2, X[,2]*X[,5:12]^2,X[,3]*X[,5:12]^2,X[,4]*X[,5:12]^2,
           X[,1]*X[,5:12]^3, X[,2]*X[,5:12]^3,X[,3]*X[,5:12]^3,X[,4]*X[,5:12]^3,
           X[,5:12]^4,
           X[,5]^3*X[,6],X[,5]^3*X[,7],X[,5]^3*X[,8],X[,5]^3*X[,9],X[,5]^3*X[,10],X[,5]^3*X[,11],X[,5]^3*X[,12],
           X[,6]^3*X[,7],X[,6]^3*X[,8],X[,6]^3*X[,9],X[,6]^3*X[,10],X[,6]^3*X[,11],X[,6]^3*X[,12],
           X[,7]^3*X[,8],X[,7]^3*X[,9],X[,7]^3*X[,10],X[,7]^3*X[,11],X[,7]^3*X[,12],
           X[,8]^3*X[,9],X[,8]^3*X[,10],X[,8]^3*X[,11],X[,8]^3*X[,12],
           X[,9]^3*X[,10],X[,9]^3*X[,11],X[,9]^3*X[,12],
           X[,10]^3*X[,11],X[,10]^3*X[,12],
           X[,11]^3*X[,12]
)


K=3;  ###0.41073
#K=7;  ####0.34762 ####0.41868 #0.28107
N = dim(X)[1];
dimX = dim(X)[2];



subsample_numerator       = NULL;
subsample_denominator     = NULL;
Y_Xbeta22_all             = NULL;
Y_beta21_Xbeta22_all      = NULL;
Xgamma_all                = NULL;
beta11_Xbeta12_all        = NULL;
Xbeta12_all               = NULL;



##############################################################
# LASSO OLS OF D on Z and X
##############################################################
#cvfit = cv.glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, standardize = TRUE, intercept = TRUE);
#lambda1 = cvfit$lambda.min
lambda1 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
#lassologitresult1 = glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, lambda=lambda1, alpha=1, family="binomial" );
lassologitresult1 = glmnet( cbind(Z,X), D, lambda=lambda1, alpha=1);

hat_beta11_beta12 = matrix(coef(lassologitresult1),,1);

##############################################################
# LASSO logistic OF Z on  X
##############################################################
# cvfit = cv.glmnet( auxiliaryX, auxiliaryZ, standardize = TRUE, intercept = TRUE );
#lambda2 = cvfit$lambda.min
lambda2 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
lassologitresult2 = glmnet( X, Z, lambda=lambda2, alpha=1, family="binomial" );
hat_gamma = matrix(coef(lassologitresult2),,1);

##############################################################
# LASSO OLS OF Y on  Z and X
##############################################################
#cvfit = cv.glmnet( cbind(auxiliaryZ, auxiliaryX), auxiliaryY, standardize = TRUE, intercept = TRUE );
#lambda3 = cvfit$lambda.min

lambda3 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
lassologitresult3 = glmnet( cbind(Z, X), Y, lambda=lambda3, alpha=1 );
hat_beta21_beta22 = matrix(coef(lassologitresult3),,1);

##############################################################
# COMPUTE Y - Xbeta22, Xgamma, AND beta11+Xbeta12
##############################################################
Y_Xbeta22_all        =   Y - hat_beta21_beta22[1] -X %*% hat_beta21_beta22[3:(dimX+2)];
Y_beta21_Xbeta22_all =   Y-hat_beta21_beta22[1]-hat_beta21_beta22[2]-X %*% hat_beta21_beta22[3:(dimX+2)] ;
Xgamma_all           =   cbind(1,X)%*%hat_gamma;
beta11_Xbeta12_all   =   hat_beta11_beta12[1]+hat_beta11_beta12[2]+X%*%hat_beta11_beta12[3:(dimX+2)];
Xbeta12_all          =  hat_beta11_beta12[1]+X%*%hat_beta11_beta12[3:(dimX+2)];



##############################################################
########## compute two subsample mean#########################
##############################################################
#subsample_denominator = rbind(subsample_denominator,plogis(beta11_Xbeta12)-plogis(Xbeta12)+matrix(mainZ,,1)*(mainD-plogis(beta11_Xbeta12))/plogis(Xgamma)-matrix(1-mainZ,,1)*(mainD-plogis(Xbeta12))/(1-plogis(Xgamma)));
sample_denominator = hat_beta11_beta12[2]+matrix(Z,,1)*(D-beta11_Xbeta12_all)/plogis(Xgamma_all)-matrix(1-Z,,1)*(D-Xbeta12_all)/(1-plogis(Xgamma_all));
sample_numerator   = hat_beta21_beta22[2]+matrix(Z,,1)*Y_beta21_Xbeta22_all/plogis(Xgamma_all)-matrix(1-Z,,1)*Y_Xbeta22_all/(1-plogis(Xgamma_all));


######just for inference use



theta_hat_temp = sum(sample_numerator)/sum(sample_denominator);

#########################################################################
phi_denominator_hat = hat_beta11_beta12[2]+matrix(Z,,1)*(D-beta11_Xbeta12_all)/plogis(Xgamma_all)-matrix(1-Z,,1)*(D-Xbeta12_all)/(1-plogis(Xgamma_all));
phi_numerator_hat   = hat_beta21_beta22[2]+matrix(Z,,1)*Y_beta21_Xbeta22_all/plogis(Xgamma_all)-matrix(1-Z,,1)*Y_Xbeta22_all/(1-plogis(Xgamma_all));
phi_hat             = phi_denominator_hat*theta_hat_temp - phi_numerator_hat;





Gamma = sum(phi_hat^2)/N;

J =  sum(phi_denominator_hat)/N;





sigma2 = Gamma / J^2;
theta_SD_zeroway = sqrt( sigma2 / N );


###end of theta0
CI3_lower  = theta_hat_temp - 2*qnorm(0.975)* theta_SD_zeroway;
CI3_upper  = theta_hat_temp + 2*qnorm(0.975)* theta_SD_zeroway;

theta_hat  = theta_hat_temp;
#} ####end of randomization

theta_hat3 = mean(theta_hat)

###########################################################################################
##############################column 4#####################################################
###########################################################################################
data <- read_dta("~/Documents/A_JMP/JMP_Yukun/application/railroad/hornung-rail-cross-section.dta");
attach(data)

# Subset the data based on conditions
data1 <- data[ abs(delta52_55) < 0.1,];
data2 <- data1[data1$node1848==0, ];
data3 <- data2[ data2$areachange2 != 1855, ];
attach(data3)
used_data <- cbind(addrail55,delta52_55, rail1848,slc1848,street1848_dummy, ship1849_dummy,delta31_37,ln_pop1849civil,ln_pop1849military,
                   fac1849_total2_pc, county_mining, county_landownership,
                   pop1849_young_pc, edu1849_pri_enrol, dist1);
data_clean <-na.omit(used_data)
data4 <- data.frame(data_clean);

# clus <- data4$kreiskey1849;
# lm.cluster( data=data4, formula = formula_ols, cluster = clus) ##panel A
# lm.cluster( data=data4, formula = formula_first_stage, cluster = clus) ##panel B
# first_stage <- lm( formula = formula_first_stage,data=data3)
# rail1848_hat <- predict(first_stage);
# lm.cluster( data=data4, formula = formula_second_stage, cluster = clus) ##panel C
# lm.cluster( data=data4, formula = formula_reduce, cluster = clus) ##panel C

attach(data4)
X  = cbind(addrail55,street1848_dummy,  ship1849_dummy,county_mining, delta31_37, ln_pop1849civil, ln_pop1849military,
           fac1849_total2_pc,  county_landownership, pop1849_young_pc, edu1849_pri_enrol, dist1);
Z  = slc1848;
Y  = delta52_55;
D  = rail1848;
########make X high-dimensional
X  = cbind(X,
           X[,5:12]^2,
           X[,5]*X[,6],X[,5]*X[,7],X[,5]*X[,8],X[,5]*X[,9],X[,5]*X[,10],X[,5]*X[,11],X[,5]*X[,12],
           X[,6]*X[,7],X[,6]*X[,8],X[,6]*X[,9],X[,6]*X[,10],X[,6]*X[,11],X[,6]*X[,12],
           X[,7]*X[,8],X[,7]*X[,9],X[,7]*X[,10],X[,7]*X[,11],X[,7]*X[,12],
           X[,8]*X[,9],X[,8]*X[,10],X[,8]*X[,11],X[,8]*X[,12],
           X[,9]*X[,10],X[,9]*X[,11],X[,9]*X[,12],
           X[,10]*X[,11],X[,10]*X[,12],
           X[,11]*X[,12],
           X[,5:12]^3,
           X[,5]^2*X[,6],X[,5]^2*X[,7],X[,5]^2*X[,8],X[,5]^2*X[,9],X[,5]^2*X[,10],X[,5]^2*X[,11],X[,5]^2*X[,12],
           X[,6]^2*X[,5],X[,6]^2*X[,7],X[,6]^2*X[,8],X[,6]^2*X[,9],X[,6]^2*X[,10],X[,6]^2*X[,11],X[,6]^2*X[,12],
           X[,7]^2*X[,5],X[,7]^2*X[,6],X[,7]^2*X[,8],X[,7]^2*X[,9],X[,7]^2*X[,10],X[,7]^2*X[,11],X[,7]^2*X[,12],
           X[,8]^2*X[,5],X[,8]^2*X[,6],X[,8]^2*X[,7],X[,8]^2*X[,9],X[,8]^2*X[,10],X[,8]^2*X[,11],X[,8]^2*X[,12],
           X[,9]^2*X[,5], X[,9]^2*X[,6], X[,9]^2*X[,7], X[,9]^2*X[,8],X[,9]^2*X[,10],X[,9]^2*X[,11],X[,9]^2*X[,12],
           X[,10]^2*X[,5],X[,10]^2*X[,6],X[,10]^2*X[,7],X[,10]^2*X[,8],X[,10]^2*X[,9],X[,10]^2*X[,11],X[,10]^2*X[,12],
           X[,11]^2*X[,5],X[,11]^2*X[,6],X[,11]^2*X[,7],X[,11]^2*X[,8],X[,11]^2*X[,9],X[,11]^2*X[,10],X[,11]^2*X[,12],
           X[,12]^2*X[,5],X[,12]^2*X[,6],X[,12]^2*X[,7],X[,12]^2*X[,8],X[,12]^2*X[,9],X[,12]^2*X[,10],X[,12]^2*X[,11],
           X[,1]*X[,5:12], X[,2]*X[,5:12],X[,3]*X[,5:12],X[,4]*X[,5:12],
           # X[,1]*X[,5:12]^2, X[,2]*X[,5:12]^2,X[,3]*X[,5:12]^2,X[,4]*X[,5:12]^2,
           X[,1]*X[,5:12]^3, X[,2]*X[,5:12]^3,X[,3]*X[,5:12]^3,X[,4]*X[,5:12]^3,
           X[,5:12]^4,
           X[,5]^3*X[,6],X[,5]^3*X[,7],X[,5]^3*X[,8],X[,5]^3*X[,9],X[,5]^3*X[,10],X[,5]^3*X[,11],X[,5]^3*X[,12],
           X[,6]^3*X[,7],X[,6]^3*X[,8],X[,6]^3*X[,9],X[,6]^3*X[,10],X[,6]^3*X[,11],X[,6]^3*X[,12],
           X[,7]^3*X[,8],X[,7]^3*X[,9],X[,7]^3*X[,10],X[,7]^3*X[,11],X[,7]^3*X[,12],
           X[,8]^3*X[,9],X[,8]^3*X[,10],X[,8]^3*X[,11],X[,8]^3*X[,12],
           X[,9]^3*X[,10],X[,9]^3*X[,11],X[,9]^3*X[,12],
           X[,10]^3*X[,11],X[,10]^3*X[,12],
           X[,11]^3*X[,12]
)


K=3;  ###0.41073
#K=7;  ####0.34762 ####0.41868 #0.28107
N = dim(X)[1];
dimX = dim(X)[2];



subsample_numerator       = NULL;
subsample_denominator     = NULL;
Y_Xbeta22_all             = NULL;
Y_beta21_Xbeta22_all      = NULL;
Xgamma_all                = NULL;
beta11_Xbeta12_all        = NULL;
Xbeta12_all               = NULL;



##############################################################
# LASSO OLS OF D on Z and X
##############################################################
#cvfit = cv.glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, standardize = TRUE, intercept = TRUE);
#lambda1 = cvfit$lambda.min
lambda1 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
#lassologitresult1 = glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, lambda=lambda1, alpha=1, family="binomial" );
lassologitresult1 = glmnet( cbind(Z,X), D, lambda=lambda1, alpha=1);

hat_beta11_beta12 = matrix(coef(lassologitresult1),,1);

##############################################################
# LASSO logistic OF Z on  X
##############################################################
# cvfit = cv.glmnet( auxiliaryX, auxiliaryZ, standardize = TRUE, intercept = TRUE );
#lambda2 = cvfit$lambda.min
lambda2 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
lassologitresult2 = glmnet( X, Z, lambda=lambda2, alpha=1, family="binomial" );
hat_gamma = matrix(coef(lassologitresult2),,1);

##############################################################
# LASSO OLS OF Y on  Z and X
##############################################################
#cvfit = cv.glmnet( cbind(auxiliaryZ, auxiliaryX), auxiliaryY, standardize = TRUE, intercept = TRUE );
#lambda3 = cvfit$lambda.min

lambda3 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
lassologitresult3 = glmnet( cbind(Z, X), Y, lambda=lambda3, alpha=1 );
hat_beta21_beta22 = matrix(coef(lassologitresult3),,1);

##############################################################
# COMPUTE Y - Xbeta22, Xgamma, AND beta11+Xbeta12
##############################################################
Y_Xbeta22_all        =   Y - hat_beta21_beta22[1] -X %*% hat_beta21_beta22[3:(dimX+2)];
Y_beta21_Xbeta22_all =   Y-hat_beta21_beta22[1]-hat_beta21_beta22[2]-X %*% hat_beta21_beta22[3:(dimX+2)] ;
Xgamma_all           =   cbind(1,X)%*%hat_gamma;
beta11_Xbeta12_all   =   hat_beta11_beta12[1]+hat_beta11_beta12[2]+X%*%hat_beta11_beta12[3:(dimX+2)];
Xbeta12_all          =  hat_beta11_beta12[1]+X%*%hat_beta11_beta12[3:(dimX+2)];



##############################################################
########## compute two subsample mean#########################
##############################################################
#subsample_denominator = rbind(subsample_denominator,plogis(beta11_Xbeta12)-plogis(Xbeta12)+matrix(mainZ,,1)*(mainD-plogis(beta11_Xbeta12))/plogis(Xgamma)-matrix(1-mainZ,,1)*(mainD-plogis(Xbeta12))/(1-plogis(Xgamma)));
sample_denominator = hat_beta11_beta12[2]+matrix(Z,,1)*(D-beta11_Xbeta12_all)/plogis(Xgamma_all)-matrix(1-Z,,1)*(D-Xbeta12_all)/(1-plogis(Xgamma_all));
sample_numerator   = hat_beta21_beta22[2]+matrix(Z,,1)*Y_beta21_Xbeta22_all/plogis(Xgamma_all)-matrix(1-Z,,1)*Y_Xbeta22_all/(1-plogis(Xgamma_all));


######just for inference use



theta_hat_temp = sum(sample_numerator)/sum(sample_denominator);

#########################################################################
phi_denominator_hat = hat_beta11_beta12[2]+matrix(Z,,1)*(D-beta11_Xbeta12_all)/plogis(Xgamma_all)-matrix(1-Z,,1)*(D-Xbeta12_all)/(1-plogis(Xgamma_all));
phi_numerator_hat   = hat_beta21_beta22[2]+matrix(Z,,1)*Y_beta21_Xbeta22_all/plogis(Xgamma_all)-matrix(1-Z,,1)*Y_Xbeta22_all/(1-plogis(Xgamma_all));
phi_hat             = phi_denominator_hat*theta_hat_temp - phi_numerator_hat;





Gamma = sum(phi_hat^2)/N;

J =  sum(phi_denominator_hat)/N;





sigma2 = Gamma / J^2;
theta_SD_zeroway = sqrt( sigma2 / N );


###end of theta0
CI4_lower  = theta_hat_temp - 2*qnorm(0.975)* theta_SD_zeroway;
CI4_upper  = theta_hat_temp + 2*qnorm(0.975)* theta_SD_zeroway;

theta_hat  = theta_hat_temp;
#} ####end of randomization

theta_hat4 = mean(theta_hat)
###########################################################################################
##############################column 5#####################################################
###########################################################################################
data <- read_dta("~/Documents/A_JMP/JMP_Yukun/application/railroad/hornung-rail-cross-section.dta");
attach(data)

# Subset the data based on conditions
data1 <- data[ abs(delta55_58) < 0.1,];
data2 <- data1[data1$node1848==0, ];
data3 <- data2[ data2$areachange2 != 1858, ];
attach(data3)
used_data <- cbind(addrail58,delta55_58, rail1848,slc1848,street1848_dummy, ship1849_dummy,delta31_37,ln_pop1849civil,ln_pop1849military,
                   fac1849_total2_pc, county_mining, county_landownership,
                   pop1849_young_pc, edu1849_pri_enrol, dist1);
data_clean <-na.omit(used_data)
data4 <- data.frame(data_clean);

# clus <- data4$kreiskey1849;
# lm.cluster( data=data4, formula = formula_ols, cluster = clus) ##panel A
# lm.cluster( data=data4, formula = formula_first_stage, cluster = clus) ##panel B
# first_stage <- lm( formula = formula_first_stage,data=data3)
# rail1848_hat <- predict(first_stage);
# lm.cluster( data=data4, formula = formula_second_stage, cluster = clus) ##panel C
# lm.cluster( data=data4, formula = formula_reduce, cluster = clus) ##panel C

attach(data4)
X  = cbind(addrail58,street1848_dummy,  ship1849_dummy,county_mining, delta31_37, ln_pop1849civil, ln_pop1849military,
           fac1849_total2_pc,  county_landownership, pop1849_young_pc, edu1849_pri_enrol, dist1);
Z  = slc1848;
Y  = delta55_58;
D  = rail1848;
########make X high-dimensional
X  = cbind(X,
           X[,5:12]^2,
           X[,5]*X[,6],X[,5]*X[,7],X[,5]*X[,8],X[,5]*X[,9],X[,5]*X[,10],X[,5]*X[,11],X[,5]*X[,12],
           X[,6]*X[,7],X[,6]*X[,8],X[,6]*X[,9],X[,6]*X[,10],X[,6]*X[,11],X[,6]*X[,12],
           X[,7]*X[,8],X[,7]*X[,9],X[,7]*X[,10],X[,7]*X[,11],X[,7]*X[,12],
           X[,8]*X[,9],X[,8]*X[,10],X[,8]*X[,11],X[,8]*X[,12],
           X[,9]*X[,10],X[,9]*X[,11],X[,9]*X[,12],
           X[,10]*X[,11],X[,10]*X[,12],
           X[,11]*X[,12],
           X[,5:12]^3,
           X[,5]^2*X[,6],X[,5]^2*X[,7],X[,5]^2*X[,8],X[,5]^2*X[,9],X[,5]^2*X[,10],X[,5]^2*X[,11],X[,5]^2*X[,12],
           X[,6]^2*X[,5],X[,6]^2*X[,7],X[,6]^2*X[,8],X[,6]^2*X[,9],X[,6]^2*X[,10],X[,6]^2*X[,11],X[,6]^2*X[,12],
           X[,7]^2*X[,5],X[,7]^2*X[,6],X[,7]^2*X[,8],X[,7]^2*X[,9],X[,7]^2*X[,10],X[,7]^2*X[,11],X[,7]^2*X[,12],
           X[,8]^2*X[,5],X[,8]^2*X[,6],X[,8]^2*X[,7],X[,8]^2*X[,9],X[,8]^2*X[,10],X[,8]^2*X[,11],X[,8]^2*X[,12],
           X[,9]^2*X[,5], X[,9]^2*X[,6], X[,9]^2*X[,7], X[,9]^2*X[,8],X[,9]^2*X[,10],X[,9]^2*X[,11],X[,9]^2*X[,12],
           X[,10]^2*X[,5],X[,10]^2*X[,6],X[,10]^2*X[,7],X[,10]^2*X[,8],X[,10]^2*X[,9],X[,10]^2*X[,11],X[,10]^2*X[,12],
           X[,11]^2*X[,5],X[,11]^2*X[,6],X[,11]^2*X[,7],X[,11]^2*X[,8],X[,11]^2*X[,9],X[,11]^2*X[,10],X[,11]^2*X[,12],
           X[,12]^2*X[,5],X[,12]^2*X[,6],X[,12]^2*X[,7],X[,12]^2*X[,8],X[,12]^2*X[,9],X[,12]^2*X[,10],X[,12]^2*X[,11],
           X[,1]*X[,5:12], X[,2]*X[,5:12],X[,3]*X[,5:12],X[,4]*X[,5:12],
           # X[,1]*X[,5:12]^2, X[,2]*X[,5:12]^2,X[,3]*X[,5:12]^2,X[,4]*X[,5:12]^2,
           X[,1]*X[,5:12]^3, X[,2]*X[,5:12]^3,X[,3]*X[,5:12]^3,X[,4]*X[,5:12]^3,
           X[,5:12]^4,
           X[,5]^3*X[,6],X[,5]^3*X[,7],X[,5]^3*X[,8],X[,5]^3*X[,9],X[,5]^3*X[,10],X[,5]^3*X[,11],X[,5]^3*X[,12],
           X[,6]^3*X[,7],X[,6]^3*X[,8],X[,6]^3*X[,9],X[,6]^3*X[,10],X[,6]^3*X[,11],X[,6]^3*X[,12],
           X[,7]^3*X[,8],X[,7]^3*X[,9],X[,7]^3*X[,10],X[,7]^3*X[,11],X[,7]^3*X[,12],
           X[,8]^3*X[,9],X[,8]^3*X[,10],X[,8]^3*X[,11],X[,8]^3*X[,12],
           X[,9]^3*X[,10],X[,9]^3*X[,11],X[,9]^3*X[,12],
           X[,10]^3*X[,11],X[,10]^3*X[,12],
           X[,11]^3*X[,12]
)


K=3;  ###0.41073
#K=7;  ####0.34762 ####0.41868 #0.28107
N = dim(X)[1];
dimX = dim(X)[2];



subsample_numerator       = NULL;
subsample_denominator     = NULL;
Y_Xbeta22_all             = NULL;
Y_beta21_Xbeta22_all      = NULL;
Xgamma_all                = NULL;
beta11_Xbeta12_all        = NULL;
Xbeta12_all               = NULL;



##############################################################
# LASSO OLS OF D on Z and X
##############################################################
#cvfit = cv.glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, standardize = TRUE, intercept = TRUE);
#lambda1 = cvfit$lambda.min
lambda1 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
#lassologitresult1 = glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, lambda=lambda1, alpha=1, family="binomial" );
lassologitresult1 = glmnet( cbind(Z,X), D, lambda=lambda1, alpha=1);

hat_beta11_beta12 = matrix(coef(lassologitresult1),,1);

##############################################################
# LASSO logistic OF Z on  X
##############################################################
# cvfit = cv.glmnet( auxiliaryX, auxiliaryZ, standardize = TRUE, intercept = TRUE );
#lambda2 = cvfit$lambda.min
lambda2 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
lassologitresult2 = glmnet( X, Z, lambda=lambda2, alpha=1, family="binomial" );
hat_gamma = matrix(coef(lassologitresult2),,1);

##############################################################
# LASSO OLS OF Y on  Z and X
##############################################################
#cvfit = cv.glmnet( cbind(auxiliaryZ, auxiliaryX), auxiliaryY, standardize = TRUE, intercept = TRUE );
#lambda3 = cvfit$lambda.min

lambda3 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
lassologitresult3 = glmnet( cbind(Z, X), Y, lambda=lambda3, alpha=1 );
hat_beta21_beta22 = matrix(coef(lassologitresult3),,1);

##############################################################
# COMPUTE Y - Xbeta22, Xgamma, AND beta11+Xbeta12
##############################################################
Y_Xbeta22_all        =   Y - hat_beta21_beta22[1] -X %*% hat_beta21_beta22[3:(dimX+2)];
Y_beta21_Xbeta22_all =   Y-hat_beta21_beta22[1]-hat_beta21_beta22[2]-X %*% hat_beta21_beta22[3:(dimX+2)] ;
Xgamma_all           =   cbind(1,X)%*%hat_gamma;
beta11_Xbeta12_all   =   hat_beta11_beta12[1]+hat_beta11_beta12[2]+X%*%hat_beta11_beta12[3:(dimX+2)];
Xbeta12_all          =  hat_beta11_beta12[1]+X%*%hat_beta11_beta12[3:(dimX+2)];



##############################################################
########## compute two subsample mean#########################
##############################################################
#subsample_denominator = rbind(subsample_denominator,plogis(beta11_Xbeta12)-plogis(Xbeta12)+matrix(mainZ,,1)*(mainD-plogis(beta11_Xbeta12))/plogis(Xgamma)-matrix(1-mainZ,,1)*(mainD-plogis(Xbeta12))/(1-plogis(Xgamma)));
sample_denominator = hat_beta11_beta12[2]+matrix(Z,,1)*(D-beta11_Xbeta12_all)/plogis(Xgamma_all)-matrix(1-Z,,1)*(D-Xbeta12_all)/(1-plogis(Xgamma_all));
sample_numerator   = hat_beta21_beta22[2]+matrix(Z,,1)*Y_beta21_Xbeta22_all/plogis(Xgamma_all)-matrix(1-Z,,1)*Y_Xbeta22_all/(1-plogis(Xgamma_all));


######just for inference use



theta_hat_temp = sum(sample_numerator)/sum(sample_denominator);

#########################################################################
phi_denominator_hat = hat_beta11_beta12[2]+matrix(Z,,1)*(D-beta11_Xbeta12_all)/plogis(Xgamma_all)-matrix(1-Z,,1)*(D-Xbeta12_all)/(1-plogis(Xgamma_all));
phi_numerator_hat   = hat_beta21_beta22[2]+matrix(Z,,1)*Y_beta21_Xbeta22_all/plogis(Xgamma_all)-matrix(1-Z,,1)*Y_Xbeta22_all/(1-plogis(Xgamma_all));
phi_hat             = phi_denominator_hat*theta_hat_temp - phi_numerator_hat;





Gamma = sum(phi_hat^2)/N;

J =  sum(phi_denominator_hat)/N;





sigma2 = Gamma / J^2;
theta_SD_zeroway = sqrt( sigma2 / N );


###end of theta0
CI5_lower  = theta_hat_temp - 2*qnorm(0.975)* theta_SD_zeroway;
CI5_upper  = theta_hat_temp + 2*qnorm(0.975)* theta_SD_zeroway;

theta_hat  = theta_hat_temp;
#} ####end of randomization

theta_hat5 = mean(theta_hat)

###########################################################################################
##############################column 6#####################################################
###########################################################################################
data <- read_dta("~/Documents/A_JMP/JMP_Yukun/application/railroad/hornung-rail-cross-section.dta");
attach(data)

# Subset the data based on conditions
data1 <- data[ abs(delta58_61) < 0.1,];
data2 <- data1[data1$node1848==0, ];
data3 <- data2[ data2$areachange2 != 1861, ];
attach(data3)
used_data <- cbind(addrail61,delta58_61, rail1848,slc1848,street1848_dummy, ship1849_dummy,delta31_37,ln_pop1849civil,ln_pop1849military,
                   fac1849_total2_pc, county_mining, county_landownership,
                   pop1849_young_pc, edu1849_pri_enrol, dist1);
data_clean <-na.omit(used_data)
data4 <- data.frame(data_clean);

# clus <- data4$kreiskey1849;
# lm.cluster( data=data4, formula = formula_ols, cluster = clus) ##panel A
# lm.cluster( data=data4, formula = formula_first_stage, cluster = clus) ##panel B
# first_stage <- lm( formula = formula_first_stage,data=data3)
# rail1848_hat <- predict(first_stage);
# lm.cluster( data=data4, formula = formula_second_stage, cluster = clus) ##panel C
# lm.cluster( data=data4, formula = formula_reduce, cluster = clus) ##panel C

attach(data4)
X  = cbind(addrail61,street1848_dummy,  ship1849_dummy,county_mining, delta31_37, ln_pop1849civil, ln_pop1849military,
           fac1849_total2_pc,  county_landownership, pop1849_young_pc, edu1849_pri_enrol, dist1);
Z  = slc1848;
Y  = delta58_61;
D  = rail1848;
########make X high-dimensional
X  = cbind(X,
           X[,5:12]^2,
           X[,5]*X[,6],X[,5]*X[,7],X[,5]*X[,8],X[,5]*X[,9],X[,5]*X[,10],X[,5]*X[,11],X[,5]*X[,12],
           X[,6]*X[,7],X[,6]*X[,8],X[,6]*X[,9],X[,6]*X[,10],X[,6]*X[,11],X[,6]*X[,12],
           X[,7]*X[,8],X[,7]*X[,9],X[,7]*X[,10],X[,7]*X[,11],X[,7]*X[,12],
           X[,8]*X[,9],X[,8]*X[,10],X[,8]*X[,11],X[,8]*X[,12],
           X[,9]*X[,10],X[,9]*X[,11],X[,9]*X[,12],
           X[,10]*X[,11],X[,10]*X[,12],
           X[,11]*X[,12],
           X[,5:12]^3,
           X[,5]^2*X[,6],X[,5]^2*X[,7],X[,5]^2*X[,8],X[,5]^2*X[,9],X[,5]^2*X[,10],X[,5]^2*X[,11],X[,5]^2*X[,12],
           X[,6]^2*X[,5],X[,6]^2*X[,7],X[,6]^2*X[,8],X[,6]^2*X[,9],X[,6]^2*X[,10],X[,6]^2*X[,11],X[,6]^2*X[,12],
           X[,7]^2*X[,5],X[,7]^2*X[,6],X[,7]^2*X[,8],X[,7]^2*X[,9],X[,7]^2*X[,10],X[,7]^2*X[,11],X[,7]^2*X[,12],
           X[,8]^2*X[,5],X[,8]^2*X[,6],X[,8]^2*X[,7],X[,8]^2*X[,9],X[,8]^2*X[,10],X[,8]^2*X[,11],X[,8]^2*X[,12],
           X[,9]^2*X[,5], X[,9]^2*X[,6], X[,9]^2*X[,7], X[,9]^2*X[,8],X[,9]^2*X[,10],X[,9]^2*X[,11],X[,9]^2*X[,12],
           X[,10]^2*X[,5],X[,10]^2*X[,6],X[,10]^2*X[,7],X[,10]^2*X[,8],X[,10]^2*X[,9],X[,10]^2*X[,11],X[,10]^2*X[,12],
           X[,11]^2*X[,5],X[,11]^2*X[,6],X[,11]^2*X[,7],X[,11]^2*X[,8],X[,11]^2*X[,9],X[,11]^2*X[,10],X[,11]^2*X[,12],
           X[,12]^2*X[,5],X[,12]^2*X[,6],X[,12]^2*X[,7],X[,12]^2*X[,8],X[,12]^2*X[,9],X[,12]^2*X[,10],X[,12]^2*X[,11],
           X[,1]*X[,5:12], X[,2]*X[,5:12],X[,3]*X[,5:12],X[,4]*X[,5:12],
           # X[,1]*X[,5:12]^2, X[,2]*X[,5:12]^2,X[,3]*X[,5:12]^2,X[,4]*X[,5:12]^2,
           X[,1]*X[,5:12]^3, X[,2]*X[,5:12]^3,X[,3]*X[,5:12]^3,X[,4]*X[,5:12]^3,
           X[,5:12]^4,
           X[,5]^3*X[,6],X[,5]^3*X[,7],X[,5]^3*X[,8],X[,5]^3*X[,9],X[,5]^3*X[,10],X[,5]^3*X[,11],X[,5]^3*X[,12],
           X[,6]^3*X[,7],X[,6]^3*X[,8],X[,6]^3*X[,9],X[,6]^3*X[,10],X[,6]^3*X[,11],X[,6]^3*X[,12],
           X[,7]^3*X[,8],X[,7]^3*X[,9],X[,7]^3*X[,10],X[,7]^3*X[,11],X[,7]^3*X[,12],
           X[,8]^3*X[,9],X[,8]^3*X[,10],X[,8]^3*X[,11],X[,8]^3*X[,12],
           X[,9]^3*X[,10],X[,9]^3*X[,11],X[,9]^3*X[,12],
           X[,10]^3*X[,11],X[,10]^3*X[,12],
           X[,11]^3*X[,12]
)


K=3;  ###0.41073
#K=7;  ####0.34762 ####0.41868 #0.28107
N = dim(X)[1];
dimX = dim(X)[2];



subsample_numerator       = NULL;
subsample_denominator     = NULL;
Y_Xbeta22_all             = NULL;
Y_beta21_Xbeta22_all      = NULL;
Xgamma_all                = NULL;
beta11_Xbeta12_all        = NULL;
Xbeta12_all               = NULL;



##############################################################
# LASSO OLS OF D on Z and X
##############################################################
#cvfit = cv.glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, standardize = TRUE, intercept = TRUE);
#lambda1 = cvfit$lambda.min
lambda1 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
#lassologitresult1 = glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, lambda=lambda1, alpha=1, family="binomial" );
lassologitresult1 = glmnet( cbind(Z,X), D, lambda=lambda1, alpha=1);

hat_beta11_beta12 = matrix(coef(lassologitresult1),,1);

##############################################################
# LASSO logistic OF Z on  X
##############################################################
# cvfit = cv.glmnet( auxiliaryX, auxiliaryZ, standardize = TRUE, intercept = TRUE );
#lambda2 = cvfit$lambda.min
lambda2 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
lassologitresult2 = glmnet( X, Z, lambda=lambda2, alpha=1, family="binomial" );
hat_gamma = matrix(coef(lassologitresult2),,1);

##############################################################
# LASSO OLS OF Y on  Z and X
##############################################################
#cvfit = cv.glmnet( cbind(auxiliaryZ, auxiliaryX), auxiliaryY, standardize = TRUE, intercept = TRUE );
#lambda3 = cvfit$lambda.min

lambda3 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
lassologitresult3 = glmnet( cbind(Z, X), Y, lambda=lambda3, alpha=1 );
hat_beta21_beta22 = matrix(coef(lassologitresult3),,1);

##############################################################
# COMPUTE Y - Xbeta22, Xgamma, AND beta11+Xbeta12
##############################################################
Y_Xbeta22_all        =   Y - hat_beta21_beta22[1] -X %*% hat_beta21_beta22[3:(dimX+2)];
Y_beta21_Xbeta22_all =   Y-hat_beta21_beta22[1]-hat_beta21_beta22[2]-X %*% hat_beta21_beta22[3:(dimX+2)] ;
Xgamma_all           =   cbind(1,X)%*%hat_gamma;
beta11_Xbeta12_all   =   hat_beta11_beta12[1]+hat_beta11_beta12[2]+X%*%hat_beta11_beta12[3:(dimX+2)];
Xbeta12_all          =  hat_beta11_beta12[1]+X%*%hat_beta11_beta12[3:(dimX+2)];



##############################################################
########## compute two subsample mean#########################
##############################################################
#subsample_denominator = rbind(subsample_denominator,plogis(beta11_Xbeta12)-plogis(Xbeta12)+matrix(mainZ,,1)*(mainD-plogis(beta11_Xbeta12))/plogis(Xgamma)-matrix(1-mainZ,,1)*(mainD-plogis(Xbeta12))/(1-plogis(Xgamma)));
sample_denominator = hat_beta11_beta12[2]+matrix(Z,,1)*(D-beta11_Xbeta12_all)/plogis(Xgamma_all)-matrix(1-Z,,1)*(D-Xbeta12_all)/(1-plogis(Xgamma_all));
sample_numerator   = hat_beta21_beta22[2]+matrix(Z,,1)*Y_beta21_Xbeta22_all/plogis(Xgamma_all)-matrix(1-Z,,1)*Y_Xbeta22_all/(1-plogis(Xgamma_all));


######just for inference use



theta_hat_temp = sum(sample_numerator)/sum(sample_denominator);

#########################################################################
phi_denominator_hat = hat_beta11_beta12[2]+matrix(Z,,1)*(D-beta11_Xbeta12_all)/plogis(Xgamma_all)-matrix(1-Z,,1)*(D-Xbeta12_all)/(1-plogis(Xgamma_all));
phi_numerator_hat   = hat_beta21_beta22[2]+matrix(Z,,1)*Y_beta21_Xbeta22_all/plogis(Xgamma_all)-matrix(1-Z,,1)*Y_Xbeta22_all/(1-plogis(Xgamma_all));
phi_hat             = phi_denominator_hat*theta_hat_temp - phi_numerator_hat;





Gamma = sum(phi_hat^2)/N;

J =  sum(phi_denominator_hat)/N;





sigma2 = Gamma / J^2;
theta_SD_zeroway = sqrt( sigma2 / N );


###end of theta0
CI6_lower  = theta_hat_temp - 2*qnorm(0.975)* theta_SD_zeroway;
CI6_upper  = theta_hat_temp + 2*qnorm(0.975)* theta_SD_zeroway;

theta_hat  = theta_hat_temp;
#} ####end of randomization

theta_hat6 = mean(theta_hat)

###########################################################################################
##############################column 7#####################################################
###########################################################################################
data <- read_dta("~/Documents/A_JMP/JMP_Yukun/application/railroad/hornung-rail-cross-section.dta");
attach(data)

# Subset the data based on conditions
data1 <- data[ abs(delta61_64) < 0.1,];
data2 <- data1[data1$node1848==0, ];
data3 <- data2[ data2$areachange2 != 1864, ];
attach(data3)
used_data <- cbind(addrail64,delta61_64, rail1848,slc1848,street1848_dummy, ship1849_dummy,delta31_37,ln_pop1849civil,ln_pop1849military,
                   fac1849_total2_pc, county_mining, county_landownership,
                   pop1849_young_pc, edu1849_pri_enrol, dist1);
data_clean <-na.omit(used_data)
data4 <- data.frame(data_clean);

# clus <- data4$kreiskey1849;
# lm.cluster( data=data4, formula = formula_ols, cluster = clus) ##panel A
# lm.cluster( data=data4, formula = formula_first_stage, cluster = clus) ##panel B
# first_stage <- lm( formula = formula_first_stage,data=data3)
# rail1848_hat <- predict(first_stage);
# lm.cluster( data=data4, formula = formula_second_stage, cluster = clus) ##panel C
# lm.cluster( data=data4, formula = formula_reduce, cluster = clus) ##panel C

attach(data4)
X  = cbind(addrail64,street1848_dummy,  ship1849_dummy,county_mining, delta31_37, ln_pop1849civil, ln_pop1849military,
           fac1849_total2_pc,  county_landownership, pop1849_young_pc, edu1849_pri_enrol, dist1);
Z  = slc1848;
Y  = delta61_64;
D  = rail1848;
########make X high-dimensional
X  = cbind(X,
           X[,5:12]^2,
           X[,5]*X[,6],X[,5]*X[,7],X[,5]*X[,8],X[,5]*X[,9],X[,5]*X[,10],X[,5]*X[,11],X[,5]*X[,12],
           X[,6]*X[,7],X[,6]*X[,8],X[,6]*X[,9],X[,6]*X[,10],X[,6]*X[,11],X[,6]*X[,12],
           X[,7]*X[,8],X[,7]*X[,9],X[,7]*X[,10],X[,7]*X[,11],X[,7]*X[,12],
           X[,8]*X[,9],X[,8]*X[,10],X[,8]*X[,11],X[,8]*X[,12],
           X[,9]*X[,10],X[,9]*X[,11],X[,9]*X[,12],
           X[,10]*X[,11],X[,10]*X[,12],
           X[,11]*X[,12],
           X[,5:12]^3,
           X[,5]^2*X[,6],X[,5]^2*X[,7],X[,5]^2*X[,8],X[,5]^2*X[,9],X[,5]^2*X[,10],X[,5]^2*X[,11],X[,5]^2*X[,12],
           X[,6]^2*X[,5],X[,6]^2*X[,7],X[,6]^2*X[,8],X[,6]^2*X[,9],X[,6]^2*X[,10],X[,6]^2*X[,11],X[,6]^2*X[,12],
           X[,7]^2*X[,5],X[,7]^2*X[,6],X[,7]^2*X[,8],X[,7]^2*X[,9],X[,7]^2*X[,10],X[,7]^2*X[,11],X[,7]^2*X[,12],
           X[,8]^2*X[,5],X[,8]^2*X[,6],X[,8]^2*X[,7],X[,8]^2*X[,9],X[,8]^2*X[,10],X[,8]^2*X[,11],X[,8]^2*X[,12],
           X[,9]^2*X[,5], X[,9]^2*X[,6], X[,9]^2*X[,7], X[,9]^2*X[,8],X[,9]^2*X[,10],X[,9]^2*X[,11],X[,9]^2*X[,12],
           X[,10]^2*X[,5],X[,10]^2*X[,6],X[,10]^2*X[,7],X[,10]^2*X[,8],X[,10]^2*X[,9],X[,10]^2*X[,11],X[,10]^2*X[,12],
           X[,11]^2*X[,5],X[,11]^2*X[,6],X[,11]^2*X[,7],X[,11]^2*X[,8],X[,11]^2*X[,9],X[,11]^2*X[,10],X[,11]^2*X[,12],
           X[,12]^2*X[,5],X[,12]^2*X[,6],X[,12]^2*X[,7],X[,12]^2*X[,8],X[,12]^2*X[,9],X[,12]^2*X[,10],X[,12]^2*X[,11],
           X[,1]*X[,5:12], X[,2]*X[,5:12],X[,3]*X[,5:12],X[,4]*X[,5:12],
           # X[,1]*X[,5:12]^2, X[,2]*X[,5:12]^2,X[,3]*X[,5:12]^2,X[,4]*X[,5:12]^2,
           X[,1]*X[,5:12]^3, X[,2]*X[,5:12]^3,X[,3]*X[,5:12]^3,X[,4]*X[,5:12]^3,
           X[,5:12]^4,
           X[,5]^3*X[,6],X[,5]^3*X[,7],X[,5]^3*X[,8],X[,5]^3*X[,9],X[,5]^3*X[,10],X[,5]^3*X[,11],X[,5]^3*X[,12],
           X[,6]^3*X[,7],X[,6]^3*X[,8],X[,6]^3*X[,9],X[,6]^3*X[,10],X[,6]^3*X[,11],X[,6]^3*X[,12],
           X[,7]^3*X[,8],X[,7]^3*X[,9],X[,7]^3*X[,10],X[,7]^3*X[,11],X[,7]^3*X[,12],
           X[,8]^3*X[,9],X[,8]^3*X[,10],X[,8]^3*X[,11],X[,8]^3*X[,12],
           X[,9]^3*X[,10],X[,9]^3*X[,11],X[,9]^3*X[,12],
           X[,10]^3*X[,11],X[,10]^3*X[,12],
           X[,11]^3*X[,12]
)


K=3;  ###0.41073
#K=7;  ####0.34762 ####0.41868 #0.28107
N = dim(X)[1];
dimX = dim(X)[2];



subsample_numerator       = NULL;
subsample_denominator     = NULL;
Y_Xbeta22_all             = NULL;
Y_beta21_Xbeta22_all      = NULL;
Xgamma_all                = NULL;
beta11_Xbeta12_all        = NULL;
Xbeta12_all               = NULL;



##############################################################
# LASSO OLS OF D on Z and X
##############################################################
#cvfit = cv.glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, standardize = TRUE, intercept = TRUE);
#lambda1 = cvfit$lambda.min
lambda1 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
#lassologitresult1 = glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, lambda=lambda1, alpha=1, family="binomial" );
lassologitresult1 = glmnet( cbind(Z,X), D, lambda=lambda1, alpha=1);

hat_beta11_beta12 = matrix(coef(lassologitresult1),,1);

##############################################################
# LASSO logistic OF Z on  X
##############################################################
# cvfit = cv.glmnet( auxiliaryX, auxiliaryZ, standardize = TRUE, intercept = TRUE );
#lambda2 = cvfit$lambda.min
lambda2 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
lassologitresult2 = glmnet( X, Z, lambda=lambda2, alpha=1, family="binomial" );
hat_gamma = matrix(coef(lassologitresult2),,1);

##############################################################
# LASSO OLS OF Y on  Z and X
##############################################################
#cvfit = cv.glmnet( cbind(auxiliaryZ, auxiliaryX), auxiliaryY, standardize = TRUE, intercept = TRUE );
#lambda3 = cvfit$lambda.min

lambda3 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
lassologitresult3 = glmnet( cbind(Z, X), Y, lambda=lambda3, alpha=1 );
hat_beta21_beta22 = matrix(coef(lassologitresult3),,1);

##############################################################
# COMPUTE Y - Xbeta22, Xgamma, AND beta11+Xbeta12
##############################################################
Y_Xbeta22_all        =   Y - hat_beta21_beta22[1] -X %*% hat_beta21_beta22[3:(dimX+2)];
Y_beta21_Xbeta22_all =   Y-hat_beta21_beta22[1]-hat_beta21_beta22[2]-X %*% hat_beta21_beta22[3:(dimX+2)] ;
Xgamma_all           =   cbind(1,X)%*%hat_gamma;
beta11_Xbeta12_all   =   hat_beta11_beta12[1]+hat_beta11_beta12[2]+X%*%hat_beta11_beta12[3:(dimX+2)];
Xbeta12_all          =  hat_beta11_beta12[1]+X%*%hat_beta11_beta12[3:(dimX+2)];



##############################################################
########## compute two subsample mean#########################
##############################################################
#subsample_denominator = rbind(subsample_denominator,plogis(beta11_Xbeta12)-plogis(Xbeta12)+matrix(mainZ,,1)*(mainD-plogis(beta11_Xbeta12))/plogis(Xgamma)-matrix(1-mainZ,,1)*(mainD-plogis(Xbeta12))/(1-plogis(Xgamma)));
sample_denominator = hat_beta11_beta12[2]+matrix(Z,,1)*(D-beta11_Xbeta12_all)/plogis(Xgamma_all)-matrix(1-Z,,1)*(D-Xbeta12_all)/(1-plogis(Xgamma_all));
sample_numerator   = hat_beta21_beta22[2]+matrix(Z,,1)*Y_beta21_Xbeta22_all/plogis(Xgamma_all)-matrix(1-Z,,1)*Y_Xbeta22_all/(1-plogis(Xgamma_all));


######just for inference use



theta_hat_temp = sum(sample_numerator)/sum(sample_denominator);

#########################################################################
phi_denominator_hat = hat_beta11_beta12[2]+matrix(Z,,1)*(D-beta11_Xbeta12_all)/plogis(Xgamma_all)-matrix(1-Z,,1)*(D-Xbeta12_all)/(1-plogis(Xgamma_all));
phi_numerator_hat   = hat_beta21_beta22[2]+matrix(Z,,1)*Y_beta21_Xbeta22_all/plogis(Xgamma_all)-matrix(1-Z,,1)*Y_Xbeta22_all/(1-plogis(Xgamma_all));
phi_hat             = phi_denominator_hat*theta_hat_temp - phi_numerator_hat;





Gamma = sum(phi_hat^2)/N;

J =  sum(phi_denominator_hat)/N;





sigma2 = Gamma / J^2;
theta_SD_zeroway = sqrt( sigma2 / N );


###end of theta0
CI7_lower  = theta_hat_temp - 2*qnorm(0.975)* theta_SD_zeroway;
CI7_upper  = theta_hat_temp + 2*qnorm(0.975)* theta_SD_zeroway;

theta_hat  = theta_hat_temp;
#} ####end of randomization

theta_hat7 = mean(theta_hat)
# ##############################column 8#####################################################
# ###########################################################################################
data <- read_dta("~/Documents/A_JMP/JMP_Yukun/application/railroad/hornung-rail-cross-section.dta");
attach(data)

# Subset the data based on conditions
data1 <- data[ abs(delta64_67) < 0.1,];
data2 <- data1[data1$node1848==0, ];
data3 <- data2[ data2$areachange2 != 1867, ];
attach(data3)
used_data <- cbind(addrail67,delta64_67, rail1848,slc1848,street1848_dummy, ship1849_dummy,delta31_37,ln_pop1849civil,ln_pop1849military,
                   fac1849_total2_pc, county_mining, county_landownership,
                   pop1849_young_pc, edu1849_pri_enrol, dist1);
data_clean <-na.omit(used_data)
data4 <- data.frame(data_clean);

# clus <- data4$kreiskey1849;
# lm.cluster( data=data4, formula = formula_ols, cluster = clus) ##panel A
# lm.cluster( data=data4, formula = formula_first_stage, cluster = clus) ##panel B
# first_stage <- lm( formula = formula_first_stage,data=data3)
# rail1848_hat <- predict(first_stage);
# lm.cluster( data=data4, formula = formula_second_stage, cluster = clus) ##panel C
# lm.cluster( data=data4, formula = formula_reduce, cluster = clus) ##panel C

attach(data4)
X  = cbind(addrail67,street1848_dummy,  ship1849_dummy,county_mining, delta31_37, ln_pop1849civil, ln_pop1849military,
           fac1849_total2_pc,  county_landownership, pop1849_young_pc, edu1849_pri_enrol, dist1);
Z  = slc1848;
Y  = delta64_67;
D  = rail1848;
########make X high-dimensional
X  = cbind(X,
           X[,5:12]^2,
           X[,5]*X[,6],X[,5]*X[,7],X[,5]*X[,8],X[,5]*X[,9],X[,5]*X[,10],X[,5]*X[,11],X[,5]*X[,12],
           X[,6]*X[,7],X[,6]*X[,8],X[,6]*X[,9],X[,6]*X[,10],X[,6]*X[,11],X[,6]*X[,12],
           X[,7]*X[,8],X[,7]*X[,9],X[,7]*X[,10],X[,7]*X[,11],X[,7]*X[,12],
           X[,8]*X[,9],X[,8]*X[,10],X[,8]*X[,11],X[,8]*X[,12],
           X[,9]*X[,10],X[,9]*X[,11],X[,9]*X[,12],
           X[,10]*X[,11],X[,10]*X[,12],
           X[,11]*X[,12],
           X[,5:12]^3,
           X[,5]^2*X[,6],X[,5]^2*X[,7],X[,5]^2*X[,8],X[,5]^2*X[,9],X[,5]^2*X[,10],X[,5]^2*X[,11],X[,5]^2*X[,12],
           X[,6]^2*X[,5],X[,6]^2*X[,7],X[,6]^2*X[,8],X[,6]^2*X[,9],X[,6]^2*X[,10],X[,6]^2*X[,11],X[,6]^2*X[,12],
           X[,7]^2*X[,5],X[,7]^2*X[,6],X[,7]^2*X[,8],X[,7]^2*X[,9],X[,7]^2*X[,10],X[,7]^2*X[,11],X[,7]^2*X[,12],
           X[,8]^2*X[,5],X[,8]^2*X[,6],X[,8]^2*X[,7],X[,8]^2*X[,9],X[,8]^2*X[,10],X[,8]^2*X[,11],X[,8]^2*X[,12],
           X[,9]^2*X[,5], X[,9]^2*X[,6], X[,9]^2*X[,7], X[,9]^2*X[,8],X[,9]^2*X[,10],X[,9]^2*X[,11],X[,9]^2*X[,12],
           X[,10]^2*X[,5],X[,10]^2*X[,6],X[,10]^2*X[,7],X[,10]^2*X[,8],X[,10]^2*X[,9],X[,10]^2*X[,11],X[,10]^2*X[,12],
           X[,11]^2*X[,5],X[,11]^2*X[,6],X[,11]^2*X[,7],X[,11]^2*X[,8],X[,11]^2*X[,9],X[,11]^2*X[,10],X[,11]^2*X[,12],
           X[,12]^2*X[,5],X[,12]^2*X[,6],X[,12]^2*X[,7],X[,12]^2*X[,8],X[,12]^2*X[,9],X[,12]^2*X[,10],X[,12]^2*X[,11],
           X[,1]*X[,5:12], X[,2]*X[,5:12],X[,3]*X[,5:12],X[,4]*X[,5:12],
           # X[,1]*X[,5:12]^2, X[,2]*X[,5:12]^2,X[,3]*X[,5:12]^2,X[,4]*X[,5:12]^2,
           X[,1]*X[,5:12]^3, X[,2]*X[,5:12]^3,X[,3]*X[,5:12]^3,X[,4]*X[,5:12]^3,
           X[,5:12]^4,
           X[,5]^3*X[,6],X[,5]^3*X[,7],X[,5]^3*X[,8],X[,5]^3*X[,9],X[,5]^3*X[,10],X[,5]^3*X[,11],X[,5]^3*X[,12],
           X[,6]^3*X[,7],X[,6]^3*X[,8],X[,6]^3*X[,9],X[,6]^3*X[,10],X[,6]^3*X[,11],X[,6]^3*X[,12],
           X[,7]^3*X[,8],X[,7]^3*X[,9],X[,7]^3*X[,10],X[,7]^3*X[,11],X[,7]^3*X[,12],
           X[,8]^3*X[,9],X[,8]^3*X[,10],X[,8]^3*X[,11],X[,8]^3*X[,12],
           X[,9]^3*X[,10],X[,9]^3*X[,11],X[,9]^3*X[,12],
           X[,10]^3*X[,11],X[,10]^3*X[,12],
           X[,11]^3*X[,12]
)


K=3;  ###0.41073
#K=7;  ####0.34762 ####0.41868 #0.28107
N = dim(X)[1];
dimX = dim(X)[2];



subsample_numerator       = NULL;
subsample_denominator     = NULL;
Y_Xbeta22_all             = NULL;
Y_beta21_Xbeta22_all      = NULL;
Xgamma_all                = NULL;
beta11_Xbeta12_all        = NULL;
Xbeta12_all               = NULL;



##############################################################
# LASSO OLS OF D on Z and X
##############################################################
#cvfit = cv.glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, standardize = TRUE, intercept = TRUE);
#lambda1 = cvfit$lambda.min
lambda1 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
#lassologitresult1 = glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, lambda=lambda1, alpha=1, family="binomial" );
lassologitresult1 = glmnet( cbind(Z,X), D, lambda=lambda1, alpha=1);

hat_beta11_beta12 = matrix(coef(lassologitresult1),,1);

##############################################################
# LASSO logistic OF Z on  X
##############################################################
# cvfit = cv.glmnet( auxiliaryX, auxiliaryZ, standardize = TRUE, intercept = TRUE );
#lambda2 = cvfit$lambda.min
lambda2 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
lassologitresult2 = glmnet( X, Z, lambda=lambda2, alpha=1, family="binomial" );
hat_gamma = matrix(coef(lassologitresult2),,1);

##############################################################
# LASSO OLS OF Y on  Z and X
##############################################################
#cvfit = cv.glmnet( cbind(auxiliaryZ, auxiliaryX), auxiliaryY, standardize = TRUE, intercept = TRUE );
#lambda3 = cvfit$lambda.min

lambda3 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
lassologitresult3 = glmnet( cbind(Z, X), Y, lambda=lambda3, alpha=1 );
hat_beta21_beta22 = matrix(coef(lassologitresult3),,1);

##############################################################
# COMPUTE Y - Xbeta22, Xgamma, AND beta11+Xbeta12
##############################################################
Y_Xbeta22_all        =   Y - hat_beta21_beta22[1] -X %*% hat_beta21_beta22[3:(dimX+2)];
Y_beta21_Xbeta22_all =   Y-hat_beta21_beta22[1]-hat_beta21_beta22[2]-X %*% hat_beta21_beta22[3:(dimX+2)] ;
Xgamma_all           =   cbind(1,X)%*%hat_gamma;
beta11_Xbeta12_all   =   hat_beta11_beta12[1]+hat_beta11_beta12[2]+X%*%hat_beta11_beta12[3:(dimX+2)];
Xbeta12_all          =  hat_beta11_beta12[1]+X%*%hat_beta11_beta12[3:(dimX+2)];



##############################################################
########## compute two subsample mean#########################
##############################################################
#subsample_denominator = rbind(subsample_denominator,plogis(beta11_Xbeta12)-plogis(Xbeta12)+matrix(mainZ,,1)*(mainD-plogis(beta11_Xbeta12))/plogis(Xgamma)-matrix(1-mainZ,,1)*(mainD-plogis(Xbeta12))/(1-plogis(Xgamma)));
sample_denominator = hat_beta11_beta12[2]+matrix(Z,,1)*(D-beta11_Xbeta12_all)/plogis(Xgamma_all)-matrix(1-Z,,1)*(D-Xbeta12_all)/(1-plogis(Xgamma_all));
sample_numerator   = hat_beta21_beta22[2]+matrix(Z,,1)*Y_beta21_Xbeta22_all/plogis(Xgamma_all)-matrix(1-Z,,1)*Y_Xbeta22_all/(1-plogis(Xgamma_all));


######just for inference use



theta_hat_temp = sum(sample_numerator)/sum(sample_denominator);

#########################################################################
phi_denominator_hat = hat_beta11_beta12[2]+matrix(Z,,1)*(D-beta11_Xbeta12_all)/plogis(Xgamma_all)-matrix(1-Z,,1)*(D-Xbeta12_all)/(1-plogis(Xgamma_all));
phi_numerator_hat   = hat_beta21_beta22[2]+matrix(Z,,1)*Y_beta21_Xbeta22_all/plogis(Xgamma_all)-matrix(1-Z,,1)*Y_Xbeta22_all/(1-plogis(Xgamma_all));
phi_hat             = phi_denominator_hat*theta_hat_temp - phi_numerator_hat;





Gamma = sum(phi_hat^2)/N;

J =  sum(phi_denominator_hat)/N;





sigma2 = Gamma / J^2;
theta_SD_zeroway = sqrt( sigma2 / N );


###end of theta0
CI8_lower  = theta_hat_temp - 2*qnorm(0.975)* theta_SD_zeroway;
CI8_upper  = theta_hat_temp + 2*qnorm(0.975)* theta_SD_zeroway;

theta_hat  = theta_hat_temp;
#} ####end of randomization

theta_hat8 = mean(theta_hat)
###########################################################################################
##############################column 9#####################################################
###########################################################################################
data <- read_dta("~/Documents/A_JMP/JMP_Yukun/application/railroad/hornung-rail-cross-section.dta");
attach(data)

# Subset the data based on conditions
data1 <- data[ abs(delta67_71) < 0.1,];
data2 <- data1[data1$node1848==0, ];
data3 <- data2[ data2$areachange2 != 1871, ];
attach(data3)
used_data <- cbind(addrail71,delta67_71, rail1848,slc1848,street1848_dummy, ship1849_dummy,delta31_37,ln_pop1849civil,ln_pop1849military,
                   fac1849_total2_pc, county_mining, county_landownership,
                   pop1849_young_pc, edu1849_pri_enrol, dist1);
data_clean <-na.omit(used_data)
data4 <- data.frame(data_clean);

# clus <- data4$kreiskey1849;
# lm.cluster( data=data4, formula = formula_ols, cluster = clus) ##panel A
# lm.cluster( data=data4, formula = formula_first_stage, cluster = clus) ##panel B
# first_stage <- lm( formula = formula_first_stage,data=data3)
# rail1848_hat <- predict(first_stage);
# lm.cluster( data=data4, formula = formula_second_stage, cluster = clus) ##panel C
# lm.cluster( data=data4, formula = formula_reduce, cluster = clus) ##panel C

attach(data4)
X  = cbind(addrail71,street1848_dummy,  ship1849_dummy,county_mining, delta31_37, ln_pop1849civil, ln_pop1849military,
           fac1849_total2_pc,  county_landownership, pop1849_young_pc, edu1849_pri_enrol, dist1);
Z  = slc1848;
Y  = delta67_71;
D  = rail1848;
########make X high-dimensional
X  = cbind(X,
           X[,5:12]^2,
           X[,5]*X[,6],X[,5]*X[,7],X[,5]*X[,8],X[,5]*X[,9],X[,5]*X[,10],X[,5]*X[,11],X[,5]*X[,12],
           X[,6]*X[,7],X[,6]*X[,8],X[,6]*X[,9],X[,6]*X[,10],X[,6]*X[,11],X[,6]*X[,12],
           X[,7]*X[,8],X[,7]*X[,9],X[,7]*X[,10],X[,7]*X[,11],X[,7]*X[,12],
           X[,8]*X[,9],X[,8]*X[,10],X[,8]*X[,11],X[,8]*X[,12],
           X[,9]*X[,10],X[,9]*X[,11],X[,9]*X[,12],
           X[,10]*X[,11],X[,10]*X[,12],
           X[,11]*X[,12],
           X[,5:12]^3,
           X[,5]^2*X[,6],X[,5]^2*X[,7],X[,5]^2*X[,8],X[,5]^2*X[,9],X[,5]^2*X[,10],X[,5]^2*X[,11],X[,5]^2*X[,12],
           X[,6]^2*X[,5],X[,6]^2*X[,7],X[,6]^2*X[,8],X[,6]^2*X[,9],X[,6]^2*X[,10],X[,6]^2*X[,11],X[,6]^2*X[,12],
           X[,7]^2*X[,5],X[,7]^2*X[,6],X[,7]^2*X[,8],X[,7]^2*X[,9],X[,7]^2*X[,10],X[,7]^2*X[,11],X[,7]^2*X[,12],
           X[,8]^2*X[,5],X[,8]^2*X[,6],X[,8]^2*X[,7],X[,8]^2*X[,9],X[,8]^2*X[,10],X[,8]^2*X[,11],X[,8]^2*X[,12],
           X[,9]^2*X[,5], X[,9]^2*X[,6], X[,9]^2*X[,7], X[,9]^2*X[,8],X[,9]^2*X[,10],X[,9]^2*X[,11],X[,9]^2*X[,12],
           X[,10]^2*X[,5],X[,10]^2*X[,6],X[,10]^2*X[,7],X[,10]^2*X[,8],X[,10]^2*X[,9],X[,10]^2*X[,11],X[,10]^2*X[,12],
           X[,11]^2*X[,5],X[,11]^2*X[,6],X[,11]^2*X[,7],X[,11]^2*X[,8],X[,11]^2*X[,9],X[,11]^2*X[,10],X[,11]^2*X[,12],
           X[,12]^2*X[,5],X[,12]^2*X[,6],X[,12]^2*X[,7],X[,12]^2*X[,8],X[,12]^2*X[,9],X[,12]^2*X[,10],X[,12]^2*X[,11],
           X[,1]*X[,5:12], X[,2]*X[,5:12],X[,3]*X[,5:12],X[,4]*X[,5:12],
           # X[,1]*X[,5:12]^2, X[,2]*X[,5:12]^2,X[,3]*X[,5:12]^2,X[,4]*X[,5:12]^2,
           X[,1]*X[,5:12]^3, X[,2]*X[,5:12]^3,X[,3]*X[,5:12]^3,X[,4]*X[,5:12]^3,
           X[,5:12]^4,
           X[,5]^3*X[,6],X[,5]^3*X[,7],X[,5]^3*X[,8],X[,5]^3*X[,9],X[,5]^3*X[,10],X[,5]^3*X[,11],X[,5]^3*X[,12],
           X[,6]^3*X[,7],X[,6]^3*X[,8],X[,6]^3*X[,9],X[,6]^3*X[,10],X[,6]^3*X[,11],X[,6]^3*X[,12],
           X[,7]^3*X[,8],X[,7]^3*X[,9],X[,7]^3*X[,10],X[,7]^3*X[,11],X[,7]^3*X[,12],
           X[,8]^3*X[,9],X[,8]^3*X[,10],X[,8]^3*X[,11],X[,8]^3*X[,12],
           X[,9]^3*X[,10],X[,9]^3*X[,11],X[,9]^3*X[,12],
           X[,10]^3*X[,11],X[,10]^3*X[,12],
           X[,11]^3*X[,12]
)


K=3;  ###0.41073
#K=7;  ####0.34762 ####0.41868 #0.28107
N = dim(X)[1];
dimX = dim(X)[2];



subsample_numerator       = NULL;
subsample_denominator     = NULL;
Y_Xbeta22_all             = NULL;
Y_beta21_Xbeta22_all      = NULL;
Xgamma_all                = NULL;
beta11_Xbeta12_all        = NULL;
Xbeta12_all               = NULL;



##############################################################
# LASSO OLS OF D on Z and X
##############################################################
#cvfit = cv.glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, standardize = TRUE, intercept = TRUE);
#lambda1 = cvfit$lambda.min
lambda1 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
#lassologitresult1 = glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, lambda=lambda1, alpha=1, family="binomial" );
lassologitresult1 = glmnet( cbind(Z,X), D, lambda=lambda1, alpha=1);

hat_beta11_beta12 = matrix(coef(lassologitresult1),,1);

##############################################################
# LASSO logistic OF Z on  X
##############################################################
# cvfit = cv.glmnet( auxiliaryX, auxiliaryZ, standardize = TRUE, intercept = TRUE );
#lambda2 = cvfit$lambda.min
lambda2 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
lassologitresult2 = glmnet( X, Z, lambda=lambda2, alpha=1, family="binomial" );
hat_gamma = matrix(coef(lassologitresult2),,1);

##############################################################
# LASSO OLS OF Y on  Z and X
##############################################################
#cvfit = cv.glmnet( cbind(auxiliaryZ, auxiliaryX), auxiliaryY, standardize = TRUE, intercept = TRUE );
#lambda3 = cvfit$lambda.min

lambda3 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
lassologitresult3 = glmnet( cbind(Z, X), Y, lambda=lambda3, alpha=1 );
hat_beta21_beta22 = matrix(coef(lassologitresult3),,1);

##############################################################
# COMPUTE Y - Xbeta22, Xgamma, AND beta11+Xbeta12
##############################################################
Y_Xbeta22_all        =   Y - hat_beta21_beta22[1] -X %*% hat_beta21_beta22[3:(dimX+2)];
Y_beta21_Xbeta22_all =   Y-hat_beta21_beta22[1]-hat_beta21_beta22[2]-X %*% hat_beta21_beta22[3:(dimX+2)] ;
Xgamma_all           =   cbind(1,X)%*%hat_gamma;
beta11_Xbeta12_all   =   hat_beta11_beta12[1]+hat_beta11_beta12[2]+X%*%hat_beta11_beta12[3:(dimX+2)];
Xbeta12_all          =  hat_beta11_beta12[1]+X%*%hat_beta11_beta12[3:(dimX+2)];



##############################################################
########## compute two subsample mean#########################
##############################################################
#subsample_denominator = rbind(subsample_denominator,plogis(beta11_Xbeta12)-plogis(Xbeta12)+matrix(mainZ,,1)*(mainD-plogis(beta11_Xbeta12))/plogis(Xgamma)-matrix(1-mainZ,,1)*(mainD-plogis(Xbeta12))/(1-plogis(Xgamma)));
sample_denominator = hat_beta11_beta12[2]+matrix(Z,,1)*(D-beta11_Xbeta12_all)/plogis(Xgamma_all)-matrix(1-Z,,1)*(D-Xbeta12_all)/(1-plogis(Xgamma_all));
sample_numerator   = hat_beta21_beta22[2]+matrix(Z,,1)*Y_beta21_Xbeta22_all/plogis(Xgamma_all)-matrix(1-Z,,1)*Y_Xbeta22_all/(1-plogis(Xgamma_all));


######just for inference use



theta_hat_temp = sum(sample_numerator)/sum(sample_denominator);

#########################################################################
phi_denominator_hat = hat_beta11_beta12[2]+matrix(Z,,1)*(D-beta11_Xbeta12_all)/plogis(Xgamma_all)-matrix(1-Z,,1)*(D-Xbeta12_all)/(1-plogis(Xgamma_all));
phi_numerator_hat   = hat_beta21_beta22[2]+matrix(Z,,1)*Y_beta21_Xbeta22_all/plogis(Xgamma_all)-matrix(1-Z,,1)*Y_Xbeta22_all/(1-plogis(Xgamma_all));
phi_hat             = phi_denominator_hat*theta_hat_temp - phi_numerator_hat;





Gamma = sum(phi_hat^2)/N;

J =  sum(phi_denominator_hat)/N;





sigma2 = Gamma / J^2;
theta_SD_zeroway = sqrt( sigma2 / N );


###end of theta0
CI9_lower  = theta_hat_temp - 2*qnorm(0.975)* theta_SD_zeroway;
CI9_upper  = theta_hat_temp + 2*qnorm(0.975)* theta_SD_zeroway;

theta_hat  = theta_hat_temp;
#} ####end of randomization

theta_hat9 = mean(theta_hat)


#############################################################
####################report Confidence interval###############
#############################################################
print(c(median(CI1_lower), median(CI1_upper), median(CI1_upper)-median(CI1_lower), theta_hat1))
print(c(median(CI2_lower), median(CI2_upper), median(CI2_upper)-median(CI2_lower), theta_hat2))
print(c(median(CI3_lower), median(CI3_upper), median(CI3_upper)-median(CI3_lower), theta_hat3))
print(c(median(CI4_lower), median(CI4_upper), median(CI4_upper)-median(CI4_lower), theta_hat4))
print(c(median(CI5_lower), median(CI5_upper), median(CI5_upper)-median(CI5_lower), theta_hat5))
print(c(median(CI6_lower), median(CI6_upper), median(CI6_upper)-median(CI6_lower), theta_hat6))
print(c(median(CI7_lower), median(CI7_upper), median(CI7_upper)-median(CI7_lower), theta_hat7))
print(c(median(CI8_lower), median(CI8_upper), median(CI8_upper)-median(CI8_lower), theta_hat8))
print(c(median(CI9_lower), median(CI9_upper), median(CI9_upper)-median(CI9_lower), theta_hat9))



