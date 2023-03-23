########################################################
##### JMP code --- Yukun Ma   ##########################
########################################################

library(foreach)
library(doParallel)

start_time <- proc.time() # record start time

########################################################
################# PARAMS ###############################
########################################################

# Basic Constants
N = 500                  # number of observations 
K = 3                    # number of fold
dimX = 100               # dimensionality of X
t_0 = 1                  # true target parameter   
NUM_ITERATIONS = 2500    # number of iterations
NUM_CRITSIMS = 1000      # number of conditional simulations

# Regression Parameters
gamma  = matrix(1, dimX, 1)
gamma0 = -0.5

beta11 = -2
beta12 = matrix(1, dimX, 1)
beta21 = 1.0
beta22 = matrix(1, dimX, 1)

selected_beta_set = 0
if(selected_beta_set == 0){
  beta11 = -2
  beta12 = matrix(c(0.5^(1:dimX)), dimX, 1)
}
if(selected_beta_set == 1){
  beta11 = 0
  beta12 = matrix(1, dimX, 1)
}

# DGP Parameters
theta_base_grid_lo = -40
theta_base_grid_hi = 40
theta_base_grid_length = 81
theta_base_grid = matrix(theta_base_grid_lo:theta_base_grid_hi, theta_base_grid_length, 1)

theta_scaling = 0.2
# Range for theta_grid: [-7,9]
theta_grid = theta_base_grid * theta_scaling + t_0

################################################################
# Define Function critical_sim 
################################################################

critical_sim <- function(h_store, V0, V_cov_store, V_store, crit_sim_shocks, b_grid, b_0, b_true, N) {
  
  size_sims = dim(crit_sim_shocks)[2]
  V0_inv_rt = V0^(-0.5)
  AR_store1 = matrix(0, dim(h_store)[2], size_sims)
  AR_store1_min = NULL
  
  for (m in 1:dim(h_store)[2]) {
    g_stack_temp = rep(h_store[, m], size_sims) + (V_cov_store[, m] * V0_inv_rt)[1] * crit_sim_shocks
    V_inv_rt = V_store[, m]^(-0.5)
    g_stack_temp = V_inv_rt * g_stack_temp
    AR_store1[m,] = N * (g_stack_temp^2)
  }
  
  AR_true = AR_store1[dim(AR_store1)[1],]
  AR_store = AR_store1[1:(dim(AR_store1)[1]-1),]
  b_grid = head(b_grid, -1)
  
  for (ii in 1:size_sims) {
    AR_store1_min = cbind(AR_store1_min, min(AR_store1[, ii]))
  }
  
  LR_stats = AR_store1[which(b_grid == b_0),] - AR_store1_min
  QLRcrit = quantile(LR_stats, 0.95)
  
  return(list(QLRcrit = QLRcrit))
}

################################################################
# set other stuff
################################################################

# Set the mean and variance for X
muXi = matrix(0, dimX, 1)
SigmaXi = matrix(0, dimX, dimX)

for (index1 in 1:dimX) {
  for (index2 in 1:dimX) {
    SigmaXi[index1, index2] = 0.5^abs(index1 - index2)
  }
}


QLR_RESULTS      = NULL
QLR_CRIT_RESULTS = NULL
QLR_POWERS       = rep(0,length(theta_grid));

# set grid of points on which to evaluate statistic#
t_grid_base = c(seq(0,2,by=0.02),seq(2.05,4,by=0.05),seq(4.1,10,by=0.1),seq(10.5,20,by=0.5),30,40,50);
t_grid = sort(unique(c(-t_grid_base,t_grid_base)+t_0));  
crit_sim_shocks=matrix(rnorm(NUM_CRITSIMS,0,1),1,NUM_CRITSIMS)/(N^(0.5));

################################################################
################# START Parallel ###############################
################################################################

number_of_cores = detectCores() # or replace with any positive number <= detectCores().
cl <- makeCluster(number_of_cores)
registerDoParallel(cl)
clusterEvalQ(cl, {
  library(glmnet)
  library(MASS)
})

for (t in 1:length(theta_grid)){       #####. t in 1:81 theta_grid=[-7,9]###
  t_grid_temp = c(t_grid,theta_grid[t,]);

  RESULT = NULL
  # Begin Parallel Computing for Given t
  RESULT <- foreach(iter = 1 : NUM_ITERATIONS, .combine = rbind, .multicombine = TRUE) %dopar% {
    X      = mvrnorm(N, muXi, SigmaXi)
    p.z    = 1 / (1 + exp(- (X %*% gamma + gamma0)))
    Z      = stats::rbinom(N, size = 1, prob = p.z)

    u      = rnorm(N, 0, 1)
    p.U2   = 1 / (1 + exp(-(beta11 + X %*% beta12)))
    p.U1   = (1 - p.U2) / 2
    p.U0   = (1 - p.U2) / 2
    U      = stats::rbinom(N, size = 2, prob = c(p.U0, p.U1, p.U2))
    D      = Z * (U == 2) + U * (U != 2)
    Y      = D * theta_grid[t, 1] + X %*% beta22 + u

    ################################################################
    # ESTIMATION
    ################################################################
    list_dx              = matrix(1:N, N, 1)

    Y_Xbeta22_all        = NULL
    Y_beta21_Xbeta22_all = NULL
    Xgamma_all           = NULL
    beta11_Xbeta12_all   = NULL
    Xbeta12_all          = NULL
    sample_denominators  = NULL
    sample_numerators    = NULL


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
    sample_denominators = rbind(sample_denominators,plogis(beta11_Xbeta12_all)-plogis(Xbeta12_all)+matrix(Z,,1)*(D-plogis(beta11_Xbeta12_all))/plogis(Xgamma_all)-((1-matrix(Z,,1))*(D-plogis(Xbeta12_all))/(1-plogis(Xgamma_all))))
    sample_numerators   = rbind(sample_numerators, tilde_beta21_beta22[2]+(matrix(Z,,1)*Y_beta21_Xbeta22_all)/(plogis(Xgamma_all))-((1-matrix(Z,,1))*Y_Xbeta22_all)/(1-plogis(Xgamma_all)))

    ##############################################################
    ##########calculate AR statistic##############################
    ##############################################################
    g0 = mean(sample_numerators)-t_0*mean(sample_denominators);
    V0 = N^(-1)* sum((sample_numerators-t_0*sample_denominators)^2)-g0^2;

    ##############################################################
    ##########calculate conventional QLR statistic################
    ##############################################################
    AR_LR = matrix(0,1,length(t_grid_temp));
    h_store = matrix(0,1,length(t_grid_temp));
    V_cov_store = matrix(0, 1,length(t_grid_temp));
    V_store = matrix(0, 1,length(t_grid_temp));
    for (m in 1:length(t_grid_temp)){
      g = mean(sample_numerators)-t_grid_temp[m]*mean(sample_denominators);
      V = N^(-1)*sum((sample_numerators-t_grid_temp[m]*sample_denominators)^2)-g^2;
      V_store[1,m] = V;
      V_cov = N^(-1) * sum((sample_numerators - t_0 * sample_denominators) *
                             (sample_numerators - t_grid_temp[m] * sample_denominators)) -
              N^(-2) * sum(sample_numerators - t_0 * sample_denominators) *
                       sum(sample_numerators - t_grid_temp[m] * sample_denominators);
      V_cov_store[1, m] = V_cov
      h_store[1, m]     = g - V_cov * V0^(-1) * g0
      AR_LR[1, m]       = N * g * V^(-1) * g
    }

    AR_LR_true = AR_LR[,length(AR_LR)];
    AR_LR = AR_LR[,1:length(AR_LR)-1];

    QLR_store      = AR_LR[which(t_grid == t_0)]-min(AR_LR);
    QLR_crit_store = NULL

    if (QLR_store < qchisq(0.95,1)){
      a = critical_sim(h_store,V0,V_cov_store,V_store,crit_sim_shocks,t_grid_temp,t_0,theta_grid[t,1],N)
      QLR_crit_store = a$QLRcrit;
    } else{
      QLR_crit_store = qchisq(0.95,1);
    }

    return(list(QLR = QLR_store, QLR_crit = QLR_crit_store))
  }
  # End Parallel Computing for Given t 
  QLR_store_rows = NULL
  QLR_crit_store_rows = NULL
  for(i in 1 : nrow(RESULT)){
    row <- RESULT[i, ]
    QLR_store_rows      = c(QLR_store_rows, row$QLR)
    QLR_crit_store_rows = c(QLR_crit_store_rows, row$QLR_crit)
  }
  QLR_RESULTS[[t]] <- QLR_store_rows
  QLR_CRIT_RESULTS[[t]] <- QLR_crit_store_rows
}

# Stop the parallel processing environment
stopCluster(cl)


for (t in 1:length(theta_grid)){
  QLR_POWERS[t] = mean(QLR_RESULTS[[t]] > QLR_CRIT_RESULTS[[t]]);
}

output_data <- data.frame(theta_grid = theta_grid, QLR_POWERS = QLR_POWERS)
output_params <- data.frame(N = N,
                            K = K,
                            dimX = dimX,
                            t_0 = t_0,
                            NUM_ITERATIONS = NUM_ITERATIONS,
                            NUM_CRITSIMS = NUM_CRITSIMS,
                            gamma0 = gamma0,
                            selected_beta_set = selected_beta_set,
                            beta11 = beta11,
                            beta21 = beta21,
                            theta_base_grid_lo = theta_base_grid_lo,
                            theta_base_grid_hi = theta_base_grid_hi,
                            theta_base_grid_length = theta_base_grid_length, 
                            theta_scaling = theta_scaling, 
                            theta_grid_lo = min(theta_grid),
                            theta_grid_hi = max(theta_grid))

plot(output_data, type="l", lty=2, xlab = "", ylab = "")

filepath = paste0(getwd(), "/results/")
ifelse(!dir.exists(filepath), dir.create(filepath), FALSE)
filepath = paste0(filepath, "OLS/")
ifelse(!dir.exists(filepath), dir.create(filepath), FALSE)
current_time = format(Sys.time(), "%Y%m%d%H%M%S")
data_filename  = paste0(filepath, "betas_", selected_beta_set, "_NUM", NUM_ITERATIONS, "_", current_time, "_data.csv")
parms_filename = paste0(filepath, "betas_", selected_beta_set, "_NUM", NUM_ITERATIONS, "_", current_time, "_parms.csv")

write.csv(output_data, file = data_filename, row.names = FALSE)
write.csv(output_params, file = parms_filename, row.names = FALSE)

# Record end time and print elapsed time
end_time <- proc.time()
elapsed_time <- end_time - start_time
print(elapsed_time)