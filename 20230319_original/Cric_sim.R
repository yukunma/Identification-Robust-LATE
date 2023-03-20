Cric_sim <- function(h_store,V0,V_cov_store,V_store,crit_sim_shocks,b_grid,b_0,b_true,N){
  
  size_sims=dim(crit_sim_shocks)[2];
  V0_inv_rt = V0^(-0.5);
  AR_store1 = matrix(0, dim(h_store)[2], size_sims);
  AR_store1_min = NULL;
  for (m in 1:dim(h_store)[2]) {
    g_stack_temp = rep(h_store[,m],size_sims)+(V_cov_store[,m]*V0_inv_rt)[1] * crit_sim_shocks;
    V_inv_rt = V_store[,m]^(-0.5);
    g_stack_temp = V_inv_rt*g_stack_temp;
    AR_store1[m,] = N*(g_stack_temp^2); 
  }
  
  AR_true = AR_store1[dim(AR_store1)[1],];
  AR_store = AR_store1[1:(dim(AR_store1)[1]-1),];
  b_grid = head(b_grid,-1);
  
  for (ii in 1:size_sims){
    AR_store1_min = cbind(AR_store1_min,min(AR_store1[,ii]));
  }
  LR_stats = AR_store1[which(b_grid==b_0),]-AR_store1_min;
  QLRcrit=quantile(LR_stats,0.95);
  
 # pQLR_stats = AR_store1[which(b_grid==b_0),]-AR_true;
 # pQLRcrit = quantile(pQLR_stats,0.95);
  return(list(QLRcrit=QLRcrit));
  
}