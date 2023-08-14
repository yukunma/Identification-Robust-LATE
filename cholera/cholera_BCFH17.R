library(foreign)
library(miceadds)
library(glmnet)
library(readxl)
library(dplyr)
data = read_dta("/Users/may/Documents/A_JMP/JMP_Yukun/application/effect of cholora /111523-V2/data/19th/Merged_1853_1864_data.dta")
attach(data)
#control_var_name = paste("old_sewer + dist_pump + dist_urinal +  dist_cent + dist_pit_fake + dist_square + dist_church + dist_police + dist_fire + dist_thea + dist_pub  + dist_vent + dist_school + dist_bank + r_dist_pit_true + r_dist_pit_fake + r_dist_pump + r_dist_cent + seg_40 + seg_20 + seg_10+ seg_5+ houses") 
control_var_name = paste("old_sewer + dist_pump + dist_urinal") 

# Construct the formula as a character string

formula_str_first_stage <- paste("death_ind ~",  paste("broad +"),paste(control_var_name))
formula_first_stage <- as.formula(formula_str_first_stage)
formula_str_second_stage <- paste("log_rentals_1864 ~", paste("death_ind_hat +"), paste(control_var_name))
formula_second_stage <- as.formula(formula_str_second_stage);
formula_str_reduce <- paste("log_rentals_1864 ~", paste("broad +"), paste(control_var_name))
formula_reduce <- as.formula(formula_str_reduce);




used_data = cbind(log_rentals_1864, block, death_ind, broad, dist_netw, old_sewer, dist_pump, dist_urinal,  dist_cent, dist_pit_fake, dist_square, dist_church, dist_police, dist_fire, dist_thea, dist_pub,  dist_vent, dist_school, dist_bank, r_dist_pit_true, r_dist_pit_fake, r_dist_pump, r_dist_cent, seg_40, seg_20, seg_10, seg_5, houses) 
data2 = data.frame(used_data)
data1 = filter(data2, dist_netw<=0.292)
data_clean <-na.omit(data1)
data4 <- data.frame(data_clean)



attach(data4)
#control_var = cbind(old_sewer, dist_pump, dist_urinal,  dist_cent, dist_pit_fake, dist_square, dist_church, dist_police, dist_fire, dist_thea, dist_pub,  dist_vent, dist_school, dist_bank, r_dist_pit_true, r_dist_pit_fake, r_dist_pump, r_dist_cent, seg_40, seg_20, seg_10, seg_5, houses) 
control_var = cbind(old_sewer, dist_pump, dist_urinal)
clus = block
lm.cluster( data=data4, formula = formula_first_stage, cluster = clus) ##panel B
first_stage <- lm( formula = formula_first_stage, data=data1) 
death_ind_hat <- cbind(1,broad,control_var)%*%matrix(coef(first_stage),,1);  
lm.cluster( data=data4, formula = formula_second_stage, cluster = clus) ##panel C
lm.cluster( data=data4, formula = formula_reduce, cluster = clus) ##panel C

X  = cbind(old_sewer, dist_pump, dist_urinal,  dist_cent, dist_pit_fake, dist_square, dist_church, dist_police, dist_fire, dist_thea, dist_pub,  dist_vent, dist_school, dist_bank, r_dist_pit_true, r_dist_pit_fake, r_dist_pump, r_dist_cent, seg_40, seg_20, seg_10, seg_5, houses) 
#X = cbind(old_sewer, dist_pump, dist_urinal)
Z  = broad
Y  = log_rentals_1864
D  = death_ind



K=5;  ###0.41073
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
# cvfit = cv.glmnet( cbind(Z,X), D, standardize = TRUE, intercept = TRUE);
# lambda1 = cvfit$lambda.min
lambda1 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
#lassologitresult1 = glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, lambda=lambda1, alpha=1, family="binomial" );
lassologitresult1 = glmnet( cbind(Z,X), D, lambda=lambda1, alpha=1);

hat_beta11_beta12 = matrix(coef(lassologitresult1),,1);

##############################################################
# LASSO logistic OF Z on  X
##############################################################
# cvfit = cv.glmnet( X, Z, standardize = TRUE, intercept = TRUE );
# lambda2 = cvfit$lambda.min
lambda2 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
lassologitresult2 = glmnet( X, Z, lambda=lambda2, alpha=1, family="binomial" );
hat_gamma = matrix(coef(lassologitresult2),,1);

##############################################################
# LASSO OLS OF Y on  Z and X
##############################################################
# cvfit = cv.glmnet( cbind(Z, X), Y, standardize = TRUE, intercept = TRUE );
# lambda3 = cvfit$lambda.min

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

