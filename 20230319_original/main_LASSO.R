library(glmnet)
library(MASS)

source("dgp.R")
source("Cric_sim.R")
################################################################
# START MC
################################################################
LIST_RESULTS = NULL;
QLR_store = matrix(0,NUM_MC,length(theta_grid));
QLR_crit_store = matrix(0,NUM_MC,length(theta_grid));
QLR_power  = rep(0,length(theta_grid));

# set grid of points on which to evaluate statistic#
t_grid_base=c(seq(0,2,by=0.02),seq(2.05,4,by=0.05),seq(4.1,10,by=0.1),seq(10.5,20,by=0.5),30,40,50);
t_grid = sort(unique(c(-t_grid_base,t_grid_base)+t_0));  
crit_sim_shocks=matrix(rnorm(crit_sims,0,1),1,crit_sims)/(N^(0.5));

for (t in 1:length(theta_grid)) {
  t_grid_temp = c(t_grid,theta_grid[t,]);
   for(iter in 1:NUM_MC){
  
  

X = mvrnorm(N, muXi, SigmaXi);
p.z = 1/(1+exp(-X %*% gamma-gamma0));
Z = stats::rbinom(N, size=1, prob = p.z);

#auxi = mvrnorm(N, mui, Sigmai);
u = rnorm(N,0,1);
#u = auxi[,1];
#v = auxi[,2];
p.U2= 1/(1+exp(-(beta11+X%*%beta12)));
p.U1 = (1-p.U2)/2;
p.U0 = (1-p.U2)/2;
U = stats::rbinom(N, size=2, prob = c(p.U0,p.U1,p.U2));
D = Z*(U==2)+U*(U!=2);
#D = stats::rbinom(N, size=1, prob = (p.U2+p.U1));
Y = D*theta_grid[t,1]+X%*%beta22+u;

################################################################
# ESTIMATION
################################################################
list_dx = matrix(1:N,N,1);

Y_Xbeta22 = NULL;
Y_beta21_Xbeta22 = NULL;
Xgamma = NULL;
beta11_Xbeta12 = NULL;
Xbeta12 = NULL;
sample_denominator = NULL;
sample_numerator = NULL;
k = ceiling(list_dx / ceiling(N/K));
for(kdx in 1:max(k)){
  
  auxiliary_indices = which( k != kdx );
  auxiliaryY = Y[auxiliary_indices];
  auxiliaryD = D[auxiliary_indices];
  auxiliaryZ = Z[auxiliary_indices];
  auxiliaryX = X[auxiliary_indices,];
  
  ##############################################################
  # LASSO logistic OF D on Z and X
  ##############################################################
  lambda1 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
  lassologitresult1 = glmnet( cbind(auxiliaryZ,auxiliaryX), auxiliaryD, lambda=lambda1, alpha=1, family="binomial" );
  hat_beta11_beta12 = matrix(coef(lassologitresult1),,1);
  
  ##############################################################
  # LASSO logistic OF Z on  X
  ##############################################################
  lambda2 = .1*sqrt((N*((K-1)/K))*log((2+dimX)*(N*((K-1)/K)))) / (N*((K-1)/K));
  lassologitresult2 = glmnet( auxiliaryX, auxiliaryZ, lambda=lambda2, alpha=1, family="binomial" );
  hat_gamma = matrix(coef(lassologitresult2),,1);
  
  ##############################################################
  # LASSO OLS OF Y on  Z and X
  ##############################################################
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
  sample_denominator = rbind(sample_denominator,plogis(beta11_Xbeta12)-plogis(Xbeta12)+matrix(mainZ,,1)*(mainD-plogis(beta11_Xbeta12))/plogis(Xgamma)-matrix(1-mainZ,,1)*(mainD-plogis(Xbeta12))/(1-plogis(Xgamma)))
  sample_numerator = rbind (sample_numerator, hat_beta21_beta22[2]+matrix(mainZ,,1)*Y_beta21_Xbeta22/plogis(Xgamma)-matrix(1-mainZ,,1)*Y_Xbeta22/(1-plogis(Xgamma)))

  
}
print(c(iter,sample_numerator,sample_denominator));
LIST_RESULTS = rbind(LIST_RESULTS,c(sample_numerator,sample_denominator));




##############################################################
##########calculate AR statistic##############################
##############################################################

g0<-mean(sample_numerator)-t_0*mean(sample_denominator);
V0 = N^(-1)* sum((sample_numerator-t_0*sample_denominator)^2)-g0^2;


##############################################################
##########calculate QLR and PQLR statistic####################
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


plot(theta_grid,QLR_power, type="l", lty=2,  xlab = "", ylab = "");




plot(theta_grid,lasso_weak,xlim=range(c(-3,5)),ylim=range(c(0,1)),type="l",lty=1, xlab = "theta", ylab = "power curve");
lines(theta_grid,ols_weak, type="l", lty=2);


