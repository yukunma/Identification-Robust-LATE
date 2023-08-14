
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
# 
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
NUM_RERANDOMIZATION = 10;
theta_hat = matrix(0,NUM_RERANDOMIZATION,1);
########may change later 
NUM_CRITSIMS = 500;
#t_grid_base=c(seq(0,2,by=0.05),seq(2.05,4,by=0.1));
t_grid_base = seq(0,1,by=0.01);
t_grid      = sort(unique(c(-t_grid_base,t_grid_base)));  

theta_base_grid     =  matrix(c(-50:50),101,1);
theta_scaling       =  0.001;
theta_grid0          =  theta_base_grid*theta_scaling; 
crit_sim_shocks     =  matrix(rnorm(NUM_CRITSIMS,0,1),1,NUM_CRITSIMS)/(N^(0.5));

QLR_store = matrix(0,NUM_RERANDOMIZATION,length(theta_grid0));
QLR_crit_store = matrix(0,NUM_RERANDOMIZATION,length(theta_grid0));
RESULTS = NULL;
CI_aux   = matrix(0,1,length(theta_grid0)+1);
CI1_lower = matrix(0,NUM_RERANDOMIZATION,1);
CI1_upper = matrix(0,NUM_RERANDOMIZATION,1);



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

theta_hat = matrix(0,NUM_RERANDOMIZATION,1);
########may change later 
NUM_CRITSIMS = 500;
#t_grid_base=c(seq(0,2,by=0.05),seq(2.05,4,by=0.1));
t_grid_base = seq(0,1,by=0.01);
t_grid      = sort(unique(c(-t_grid_base,t_grid_base)));  

theta_base_grid     =  matrix(c(-50:50),101,1);
theta_scaling       =  0.001;
theta_grid0          =  theta_base_grid*theta_scaling; 
crit_sim_shocks     =  matrix(rnorm(NUM_CRITSIMS,0,1),1,NUM_CRITSIMS)/(N^(0.5));

QLR_store = matrix(0,NUM_RERANDOMIZATION,length(theta_grid0));
QLR_crit_store = matrix(0,NUM_RERANDOMIZATION,length(theta_grid0));
RESULTS = NULL;
CI_aux   = matrix(0,1,length(theta_grid0)+1);
CI2_lower = matrix(0,NUM_RERANDOMIZATION,1);
CI2_upper = matrix(0,NUM_RERANDOMIZATION,1);



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
 CI2_lower[iter]  = min(CI_aux[!is.na(CI_aux)])
 CI2_upper[iter]  = max(CI_aux[!is.na(CI_aux)])

} ####end of randomization

theta_hat2 = mean(theta_hat)



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

theta_hat = matrix(0,NUM_RERANDOMIZATION,1);
########may change later 
NUM_CRITSIMS = 500;
#t_grid_base=c(seq(0,2,by=0.05),seq(2.05,4,by=0.1));
t_grid_base = seq(0,1,by=0.01);
t_grid      = sort(unique(c(-t_grid_base,t_grid_base)));  

theta_base_grid     =  matrix(c(-50:50),101,1);
theta_scaling       =  0.001;
theta_grid0          =  theta_base_grid*theta_scaling; 
crit_sim_shocks     =  matrix(rnorm(NUM_CRITSIMS,0,1),1,NUM_CRITSIMS)/(N^(0.5));

QLR_store = matrix(0,NUM_RERANDOMIZATION,length(theta_grid0));
QLR_crit_store = matrix(0,NUM_RERANDOMIZATION,length(theta_grid0));
RESULTS = NULL;
CI_aux   = matrix(0,1,length(theta_grid0)+1);
CI3_lower = matrix(0,NUM_RERANDOMIZATION,1);
CI3_upper = matrix(0,NUM_RERANDOMIZATION,1);



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
  CI3_lower[iter]  = min(CI_aux[!is.na(CI_aux)])
  CI3_upper[iter]  = max(CI_aux[!is.na(CI_aux)])
  
} ####end of randomization

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

theta_hat = matrix(0,NUM_RERANDOMIZATION,1);
########may change later 
NUM_CRITSIMS = 500;
#t_grid_base=c(seq(0,2,by=0.05),seq(2.05,4,by=0.1));
t_grid_base = seq(0,1,by=0.01);
t_grid      = sort(unique(c(-t_grid_base,t_grid_base)));  

theta_base_grid     =  matrix(c(-50:50),101,1);
theta_scaling       =  0.001;
theta_grid0          =  theta_base_grid*theta_scaling; 
crit_sim_shocks     =  matrix(rnorm(NUM_CRITSIMS,0,1),1,NUM_CRITSIMS)/(N^(0.5));

QLR_store = matrix(0,NUM_RERANDOMIZATION,length(theta_grid0));
QLR_crit_store = matrix(0,NUM_RERANDOMIZATION,length(theta_grid0));
RESULTS = NULL;
CI_aux   = matrix(0,1,length(theta_grid0)+1);
CI4_lower = matrix(0,NUM_RERANDOMIZATION,1);
CI4_upper = matrix(0,NUM_RERANDOMIZATION,1);



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
  CI4_lower[iter]  = min(CI_aux[!is.na(CI_aux)])
  CI4_upper[iter]  = max(CI_aux[!is.na(CI_aux)])
  
} ####end of randomization

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

theta_hat = matrix(0,NUM_RERANDOMIZATION,1);
########may change later 
NUM_CRITSIMS = 500;
#t_grid_base=c(seq(0,2,by=0.05),seq(2.05,4,by=0.1));
t_grid_base = seq(0,1,by=0.01);
t_grid      = sort(unique(c(-t_grid_base,t_grid_base)));  

theta_base_grid     =  matrix(c(-50:50),101,1);
theta_scaling       =  0.001;
theta_grid0          =  theta_base_grid*theta_scaling; 
crit_sim_shocks     =  matrix(rnorm(NUM_CRITSIMS,0,1),1,NUM_CRITSIMS)/(N^(0.5));

QLR_store = matrix(0,NUM_RERANDOMIZATION,length(theta_grid0));
QLR_crit_store = matrix(0,NUM_RERANDOMIZATION,length(theta_grid0));
RESULTS = NULL;
CI_aux   = matrix(0,1,length(theta_grid0)+1);
CI5_lower = matrix(0,NUM_RERANDOMIZATION,1);
CI5_upper = matrix(0,NUM_RERANDOMIZATION,1);



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
  CI5_lower[iter]  = min(CI_aux[!is.na(CI_aux)])
  CI5_upper[iter]  = max(CI_aux[!is.na(CI_aux)])
  
} ####end of randomization

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

theta_hat = matrix(0,NUM_RERANDOMIZATION,1);
########may change later 
NUM_CRITSIMS = 500;
#t_grid_base=c(seq(0,2,by=0.05),seq(2.05,4,by=0.1));
t_grid_base = seq(0,1,by=0.01);
t_grid      = sort(unique(c(-t_grid_base,t_grid_base)));  

theta_base_grid     =  matrix(c(-50:50),101,1);
theta_scaling       =  0.001;
theta_grid0          =  theta_base_grid*theta_scaling; 
crit_sim_shocks     =  matrix(rnorm(NUM_CRITSIMS,0,1),1,NUM_CRITSIMS)/(N^(0.5));

QLR_store = matrix(0,NUM_RERANDOMIZATION,length(theta_grid0));
QLR_crit_store = matrix(0,NUM_RERANDOMIZATION,length(theta_grid0));
RESULTS = NULL;
CI_aux   = matrix(0,1,length(theta_grid0)+1);
CI6_lower = matrix(0,NUM_RERANDOMIZATION,1);
CI6_upper = matrix(0,NUM_RERANDOMIZATION,1);



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
  CI6_lower[iter]  = min(CI_aux[!is.na(CI_aux)])
  CI6_upper[iter]  = max(CI_aux[!is.na(CI_aux)])
  
} ####end of randomization

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

theta_hat = matrix(0,NUM_RERANDOMIZATION,1);
########may change later 
NUM_CRITSIMS = 500;
#t_grid_base=c(seq(0,2,by=0.05),seq(2.05,4,by=0.1));
t_grid_base = seq(0,1,by=0.01);
t_grid      = sort(unique(c(-t_grid_base,t_grid_base)));  

theta_base_grid     =  matrix(c(-50:50),101,1);
theta_scaling       =  0.001;
theta_grid0          =  theta_base_grid*theta_scaling; 
crit_sim_shocks     =  matrix(rnorm(NUM_CRITSIMS,0,1),1,NUM_CRITSIMS)/(N^(0.5));

QLR_store = matrix(0,NUM_RERANDOMIZATION,length(theta_grid0));
QLR_crit_store = matrix(0,NUM_RERANDOMIZATION,length(theta_grid0));
RESULTS = NULL;
CI_aux   = matrix(0,1,length(theta_grid0)+1);
CI7_lower = matrix(0,NUM_RERANDOMIZATION,1);
CI7_upper = matrix(0,NUM_RERANDOMIZATION,1);



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
  CI7_lower[iter]  = min(CI_aux[!is.na(CI_aux)])
  CI7_upper[iter]  = max(CI_aux[!is.na(CI_aux)])
  
} ####end of randomization

theta_hat7 = mean(theta_hat)

###########################################################################################
##############################column 8#####################################################
###########################################################################################
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

theta_hat = matrix(0,NUM_RERANDOMIZATION,1);
########may change later 
NUM_CRITSIMS = 500;
#t_grid_base=c(seq(0,2,by=0.05),seq(2.05,4,by=0.1));
t_grid_base = seq(0,1,by=0.01);
t_grid      = sort(unique(c(-t_grid_base,t_grid_base)));  

theta_base_grid     =  matrix(c(-50:50),101,1);
theta_scaling       =  0.001;
theta_grid0          =  theta_base_grid*theta_scaling; 
crit_sim_shocks     =  matrix(rnorm(NUM_CRITSIMS,0,1),1,NUM_CRITSIMS)/(N^(0.5));

QLR_store = matrix(0,NUM_RERANDOMIZATION,length(theta_grid0));
QLR_crit_store = matrix(0,NUM_RERANDOMIZATION,length(theta_grid0));
RESULTS = NULL;
CI_aux   = matrix(0,1,length(theta_grid0)+1);
CI8_lower = matrix(0,NUM_RERANDOMIZATION,1);
CI8_upper = matrix(0,NUM_RERANDOMIZATION,1);



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
  CI8_lower[iter]  = min(CI_aux[!is.na(CI_aux)])
  CI8_upper[iter]  = max(CI_aux[!is.na(CI_aux)])
  
} ####end of randomization

theta_hat8 = mean(theta_hat)

###########################################################################################
##############################column 8#####################################################
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

theta_hat = matrix(0,NUM_RERANDOMIZATION,1);
########may change later 
NUM_CRITSIMS = 500;
#t_grid_base=c(seq(0,2,by=0.05),seq(2.05,4,by=0.1));
t_grid_base = seq(0,1,by=0.01);
t_grid      = sort(unique(c(-t_grid_base,t_grid_base)));  

theta_base_grid     =  matrix(c(-50:50),101,1);
theta_scaling       =  0.001;
theta_grid0          =  theta_base_grid*theta_scaling; 
crit_sim_shocks     =  matrix(rnorm(NUM_CRITSIMS,0,1),1,NUM_CRITSIMS)/(N^(0.5));

QLR_store = matrix(0,NUM_RERANDOMIZATION,length(theta_grid0));
QLR_crit_store = matrix(0,NUM_RERANDOMIZATION,length(theta_grid0));
RESULTS = NULL;
CI_aux   = matrix(0,1,length(theta_grid0)+1);
CI9_lower = matrix(0,NUM_RERANDOMIZATION,1);
CI9_upper = matrix(0,NUM_RERANDOMIZATION,1);



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
  CI9_lower[iter]  = min(CI_aux[!is.na(CI_aux)])
  CI9_upper[iter]  = max(CI_aux[!is.na(CI_aux)])
  
} ####end of randomization

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



