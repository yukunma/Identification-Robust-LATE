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
NUM_RERANDOMIZATION = 20;
theta_hat = matrix(0,NUM_RERANDOMIZATION,1);
########may change later 
NUM_CRITSIMS = 500;
#t_grid_base=c(seq(0,2,by=0.05),seq(2.05,4,by=0.1));
t_grid_base = seq(0,1,by=0.01);
t_grid      = sort(unique(c(-t_grid_base,t_grid_base)));  

theta_base_grid     =  matrix(c(-100:100),201,1);
theta_scaling       =  0.01;
theta_grid0          =  theta_base_grid*theta_scaling-0.5; 
crit_sim_shocks     =  matrix(rnorm(NUM_CRITSIMS,0,1),1,NUM_CRITSIMS)/(N^(0.5));

QLR_store = matrix(0,NUM_RERANDOMIZATION,length(theta_grid0));
QLR_crit_store = matrix(0,NUM_RERANDOMIZATION,length(theta_grid0));
RESULTS = NULL;
CI_aux   = matrix(0,1,length(theta_grid0)+1);
CI1_lower = matrix(0,NUM_RERANDOMIZATION,1);
CI1_upper = matrix(0,NUM_RERANDOMIZATION,1);

Cric_sim <- function(h_store,V0,V_cov_store,V_store,crit_sim_shocks,b_grid,b_0,N){
  
  size_sims=dim(crit_sim_shocks)[2];
  V0_inv_rt = V0^(-0.5);
  AR_store1 = matrix(0, dim(h_store)[2], size_sims);
  AR_store1_min = NULL;
  for (i in 1:dim(h_store)[2]) {
    g_stack_temp = rep(h_store[,i],size_sims)+(V_cov_store[,i]*V0_inv_rt)[1] * crit_sim_shocks;
    V_inv_rt = V_store[,i]^(-0.5);
    g_stack_temp = V_inv_rt*g_stack_temp;
    AR_store1[i,] = N*(g_stack_temp^2); 
  }
  
  
  
  AR_store = AR_store1[1:(dim(AR_store1)[1]-1),];
  
  for (j in 1:size_sims){
    AR_store1_min = cbind(AR_store1_min,min(AR_store1[,j]));
  }
  LR_stats = AR_store1[dim(h_store)[2],]-AR_store1_min;
  QLRcrit=quantile(LR_stats,0.95);
  
  return(list(QLRcrit=QLRcrit));
  
}
#####
####should have loop for random
for (iter in 1 : NUM_RERANDOMIZATION) {
  
  list_dx = matrix(sample(N,replace = FALSE),N,1);
  k = ceiling(list_dx/ceiling(N/K));
  
  subsample_numerator = NULL;
  subsample_denominator = NULL;
  
  for (kdx in 1:max(k)){
    
    auxiliary_indices = which( k != kdx );
    auxiliaryY = Y[auxiliary_indices];
    auxiliaryD = D[auxiliary_indices];
    auxiliaryZ = Z[auxiliary_indices];
    auxiliaryX = X[auxiliary_indices,];
    
    ##############################################################
    # LASSO OLS OF D on Z and X
    ##############################################################
    #cvfit = cv.glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, standardize = TRUE, intercept = TRUE);
    #lambda1 = cvfit$lambda.min
    lambda1 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
    #lassologitresult1 = glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, lambda=lambda1, alpha=1, family="binomial" );
    lassologitresult1 = glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, lambda=lambda1, alpha=1);
    
    hat_beta11_beta12 = matrix(coef(lassologitresult1),,1);
    
    ##############################################################
    # LASSO logistic OF Z on  X
    ##############################################################
    # cvfit = cv.glmnet( auxiliaryX, auxiliaryZ, standardize = TRUE, intercept = TRUE );
    #lambda2 = cvfit$lambda.min
    lambda2 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
    lassologitresult2 = glmnet( auxiliaryX, auxiliaryZ, lambda=lambda2, alpha=1, family="binomial" );
    hat_gamma = matrix(coef(lassologitresult2),,1);
    
    ##############################################################
    # LASSO OLS OF Y on  Z and X
    ##############################################################
    #cvfit = cv.glmnet( cbind(auxiliaryZ, auxiliaryX), auxiliaryY, standardize = TRUE, intercept = TRUE );
    #lambda3 = cvfit$lambda.min
    
    lambda3 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
    lassologitresult3 = glmnet( cbind(auxiliaryZ, auxiliaryX), auxiliaryY, lambda=lambda3, alpha=1 );
    hat_beta21_beta22 = matrix(coef(lassologitresult3),,1);
    ##############################################################
    # GET MAIN SAMPLE (I X J)
    ##############################################################
    
    main_indices = which( k == kdx  );
    mainY = Y[main_indices];
    mainD = D[main_indices];
    mainZ = Z[main_indices];
    mainX = matrix(X[main_indices,],length(main_indices),dim(X)[2]);
    
    ##############################################################
    # COMPUTE Y - Xbeta22, Xgamma, AND beta11+Xbeta12
    ##############################################################
    
    Y_Xbeta22 = mainY - hat_beta21_beta22[1] -mainX %*% hat_beta21_beta22[3:(dimX+2)];
    Y_beta21_Xbeta22 =   mainY-hat_beta21_beta22[1]-hat_beta21_beta22[2]-mainX %*% hat_beta21_beta22[3:(dimX+2)] ;
    Xgamma = cbind(1,mainX)%*%hat_gamma;
    beta11_Xbeta12 =  hat_beta11_beta12[1]+hat_beta11_beta12[2]+mainX%*%hat_beta11_beta12[3:(dimX+2)];
    Xbeta12 =  hat_beta11_beta12[1]+mainX%*%hat_beta11_beta12[3:(dimX+2)];
    
    ##############################################################
    ########## compute two subsample mean#########################
    ##############################################################
    #subsample_denominator = rbind(subsample_denominator,plogis(beta11_Xbeta12)-plogis(Xbeta12)+matrix(mainZ,,1)*(mainD-plogis(beta11_Xbeta12))/plogis(Xgamma)-matrix(1-mainZ,,1)*(mainD-plogis(Xbeta12))/(1-plogis(Xgamma)));
    subsample_denominator = rbind(subsample_denominator,hat_beta11_beta12[2]+matrix(mainZ,,1)*(mainD-beta11_Xbeta12)/plogis(Xgamma)-matrix(1-mainZ,,1)*(mainD-Xbeta12)/(1-plogis(Xgamma)));
    subsample_numerator = rbind(subsample_numerator,hat_beta21_beta22[2]+matrix(mainZ,,1)*Y_beta21_Xbeta22/plogis(Xgamma)-matrix(1-mainZ,,1)*Y_Xbeta22/(1-plogis(Xgamma)));
    
  }
  print(c(iter, mean(subsample_numerator),mean(subsample_denominator),mean(subsample_numerator)/mean(subsample_denominator)));
  theta_hat[iter] = sum(subsample_numerator)/sum(subsample_denominator);
  
  theta_grid = sort(rbind(theta_grid0,theta_hat[iter]))
  
  
  R_stat      =   matrix(0,1,length(theta_grid));
  Crit_store  =   matrix(0,1,length(theta_grid));
  
  for ( m in 1:length(theta_grid)){
    #####calculate Omega_hat(theta_0,theta_0)############
    V0 =  mean((subsample_numerator-theta_grid[m]*subsample_denominator)^2) - mean(subsample_numerator-theta_grid[m]*subsample_denominator)^2;
    #xi               =  rnorm(1,0,sd = Omega_theta0_hat^0.5);
    xi               =  N^0.5*mean(subsample_numerator-theta_grid[m]*subsample_denominator);
    q_theta0         =  N^0.5*mean(subsample_numerator-theta_grid[m]*subsample_denominator)
    
    t_grid_temp = c(t_grid,theta_grid[m])
    V_cov_store = matrix(0,1,length(t_grid_temp));
    V_store      = matrix(0,1,length(t_grid_temp));
    
    h_store                = matrix(0,1,length(t_grid_temp));
    content_inf            = matrix(0,1,length(t_grid_temp));
    
    
    for (n in 1:length(t_grid_temp)){
      
      V_store[,n]      =   mean((subsample_numerator-t_grid_temp[n]*subsample_denominator)^2)-mean(subsample_numerator-t_grid_temp[n]*subsample_denominator)^2;
      V_cov_store[,n] =   mean((subsample_numerator-theta_grid[m]*subsample_denominator)*(subsample_numerator-t_grid_temp[n]*subsample_denominator))-mean(subsample_numerator-theta_grid[m]*subsample_denominator)*mean(subsample_numerator-t_grid_temp[n]*subsample_denominator);
      h_store[,n]                =   mean(subsample_numerator-t_grid_temp[n]*subsample_denominator)- V_cov_store[,n]/V0*q_theta0;
      content_inf[,n]            =   (V_cov_store[,n]/V0*xi+h_store[,n])^2/V_store[,n] 
      
    }
    
    min_part  = min(content_inf)
    R_stat[,m] =  xi^2/V0 - min_part;
    
    
    
    if (R_stat[,m] < qchisq(0.95,1)) {
      
      a          = Cric_sim(h_store,V0,V_cov_store,V_store,crit_sim_shocks,t_grid_temp,theta_grid[m],N)
      Crit_store[,m]  = a$QLRcrit;
      
    } else {
      
      Crit_store[,m]  = qchisq(0.95,1);
      
    }
    
    CI_aux[,m] =  ifelse(R_stat[,m]<=Crit_store[,m], theta_grid[m], NA);
    
  } ###end of theta0
  CI1_lower[iter]  = min(CI_aux[!is.na(CI_aux)])
  CI1_upper[iter]  = max(CI_aux[!is.na(CI_aux)])
  
} ####end of randomization

theta_hat1 = mean(theta_hat)
print(c(median(CI1_lower), median(CI1_upper), median(CI1_upper)-median(CI1_lower), theta_hat1))

