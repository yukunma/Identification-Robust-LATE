################################################################
##### JMP code --- Yukun Ma#####################################
################################################################

library(glmnet)
library(MASS)

################################################################
################# Set parameters################################
################################################################


N = 500;                  # number of observations
K = 3;                    # number of fold
dimX = 100;               # dimensionality of X
t_0 = 1;                  # true target parameter
NUM_MC = 5;               # number of simulation
crit_sims=100;            # number of conditional simulation


work_directory <- getwd()
sub_directory <- "/20230319_original/"
source(paste0(work_directory, sub_directory, "dgp.R")) # dgp
source(paste0(work_directory, sub_directory, "Cric_sim.R")) 


LIST_RESULTS = NULL;
QLR_store = matrix(0,NUM_MC,length(theta_grid));
QLR_crit_store = matrix(0,NUM_MC,length(theta_grid));
QLR_power  = rep(0,length(theta_grid));
# set grid of points on which to evaluate statistic#
t_grid_base=c(seq(0,2,by=0.02),seq(2.05,4,by=0.05),seq(4.1,10,by=0.1),seq(10.5,20,by=0.5),30,40,50);
t_grid = sort(unique(c(-t_grid_base,t_grid_base)+t_0));  
crit_sim_shocks=matrix(rnorm(crit_sims,0,1),1,crit_sims)/(N^(0.5));

################################################################
################# START MC #################################### 
################################################################

for (t in 1:length(theta_grid)){       #####. t in 1:81 theta_grid=[-7,9]###
  t_grid_temp = c(t_grid,theta_grid[t,]);
  
  for(iter in 1:NUM_MC){
    
    X = mvrnorm(N, muXi, SigmaXi);
    p.z = 1/(1+exp(-(X %*% gamma+gamma0)));
    Z = stats::rbinom(N, size=1, prob = p.z);
    
    u = rnorm(N,0,1);
    p.U2= 1/(1+exp(-(beta11+X%*%beta12)));
    p.U1 = (1-p.U2)/2;
    p.U0 = (1-p.U2)/2;
    U = stats::rbinom(N, size=2, prob = c(p.U0,p.U1,p.U2));
    D = Z*(U==2)+U*(U!=2);
    Y = D*theta_grid[t,1]+X%*%beta22+u;
    
    ################################################################
    # ESTIMATION
    ################################################################
    list_dx = matrix(1:N,N,1);
    
    Y_Xbeta22_all = NULL;
    Y_beta21_Xbeta22_all = NULL;
    Xgamma_all = NULL;
    beta11_Xbeta12_all = NULL;
    Xbeta12_all = NULL;
    sample_denominator = NULL;
    sample_numerator = NULL;
      
      ##############################################################
      # run logit OF D on Z and X
      ##############################################################
      mylogit1 = glm(D ~ Z+X, family = "binomial");
      tilde_beta11_beta12 = matrix(coef(mylogit1),,1);
      ##############################################################
      # run logit OF Z on  X
      ##############################################################
      lambda2 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
      mylogit2 = glmnet(X,Z,lambda = lambda2, alpha=1, family = "binomial");
      tilde_gamma = matrix(coef(mylogit2),,1);
      ##############################################################
      # run OLS OF Y on  Z and X
      ##############################################################
      myols1 = lm(Y~Z+X);
      tilde_beta21_beta22 = matrix(coef(myols1),,1);

            
      ##############################################################
      # COMPUTE Y - Xbeta22, Xgamma, AND beta11+Xbeta12
      ##############################################################
      Xgamma_all = rbind( Xgamma_all,cbind(1,X)%*%tilde_gamma);
      Y_beta21_Xbeta22_all = rbind ( Y_beta21_Xbeta22_all, Y-tilde_beta21_beta22[1]-tilde_beta21_beta22[2]-X %*% tilde_beta21_beta22[3:(dimX+2)] );
      Y_Xbeta22_all = rbind( Y_Xbeta22_all,Y - tilde_beta21_beta22[1] -X %*% tilde_beta21_beta22[3:(dimX+2)]);
      beta11_Xbeta12_all = rbind( beta11_Xbeta12_all , tilde_beta11_beta12[1]+tilde_beta11_beta12[2]+X%*%tilde_beta11_beta12[3:(dimX+2)]);
      Xbeta12_all = rbind( Xbeta12_all , tilde_beta11_beta12[1]+X%*%tilde_beta11_beta12[3:(dimX+2)]);
      
      
      
      
      ##############################################################
      ########## compute sample mean################################
      ##############################################################
      sample_denominator = rbind(sample_denominator,plogis(beta11_Xbeta12_all)-plogis(Xbeta12_all)+matrix(Z,,1)*(D-plogis(beta11_Xbeta12_all))/plogis(Xgamma_all)-((1-matrix(Z,,1))*(D-plogis(Xbeta12_all))/(1-plogis(Xgamma_all))))
      sample_numerator = rbind (sample_numerator, tilde_beta21_beta22[2]+(matrix(Z,,1)*Y_beta21_Xbeta22_all)/(plogis(Xgamma_all))-((1-matrix(Z,,1))*Y_Xbeta22_all)/(1-plogis(Xgamma_all)))

      
    
    print(c(iter,sample_numerator,sample_denominator));
    LIST_RESULTS = rbind(LIST_RESULTS,c(sample_numerator,sample_denominator));
    
    
    

    ##############################################################
    ##########calculate AR statistic##############################
    ##############################################################
    
    g0<-mean(sample_numerator)-t_0*mean(sample_denominator);
    #V0 = cov(sample_numerator-t_0*sample_denominator);
    V0 = N^(-1)* sum((sample_numerator-t_0*sample_denominator)^2)-g0^2;

    ##############################################################
    ##########calculate conventional QLR statistic################
    ##############################################################
    AR_LR = matrix(0,1,length(t_grid_temp));
    h_store = matrix(0,1,length(t_grid_temp));
    V_cov_store = matrix(0, 1,length(t_grid_temp));
    V_store = matrix(0, 1,length(t_grid_temp));
    for (m in 1:length(t_grid_temp)){
      g = mean(sample_numerator)-t_grid_temp[m]*mean(sample_denominator);
      #V = cov(sample_numerator-t_grid_temp[m]*sample_denominator);
      V = N^(-1)*sum((sample_numerator-t_grid_temp[m]*sample_denominator)^2)-g^2;
      V_store[1,m]=V; 
      V_cov = N^(-1)*sum((sample_numerator-t_0*sample_denominator)*
                           (sample_numerator- t_grid_temp[m]*sample_denominator))-
        N^(-2)*sum(sample_numerator-t_0*sample_denominator)*
        sum(sample_numerator- t_grid_temp[m]*sample_denominator);
      #V_cov = cov(sample_numerator-t_grid_temp[m]*sample_denominator,sample_numerator-t_0*sample_denominator);
      V_cov_store[1,m] = V_cov;
      h_store[1,m] = g - V_cov*V0^(-1)*g0;
      AR_LR[1,m] = N*g*V^(-1)*g;
    }
    
    AR_LR_true = AR_LR[,length(AR_LR)];
    AR_LR = AR_LR[,1:length(AR_LR)-1];
    QLR_store[iter,t] =AR_LR[which(t_grid==t_0)]-min(AR_LR);

    if (QLR_store[iter,t] < qchisq(0.95,1)){
      a = Cric_sim(h_store,V0,V_cov_store,V_store,crit_sim_shocks,t_grid_temp,t_0,theta_grid[t,1],N)
      QLR_crit_store[iter,t] = a$QLRcrit;
    }
    else{
      QLR_crit_store[iter,t] = qchisq(0.95,1);
    }
  }
} 



for (t in 1:81){
  QLR_power[t] = mean(QLR_store[,t]>QLR_crit_store[,t]);
}

plot(theta_grid,QLR_power, type="l", lty=2, xlab = "", ylab = "");


