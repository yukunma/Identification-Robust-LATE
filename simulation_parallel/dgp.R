
################################################################
# REGRESSION PARAMETERS
################################################################
gamma0 = -0.5; ###p.z=0.5
#gamma = matrix( c( 0.5^(1:dimX) ), dimX, 1);
#beta12   = matrix( c( 0.5^(1:dimX) ), dimX, 1);
#beta22   = matrix( c( 0.5^(1:dimX) ), dimX, 1);
gamma = matrix( 1, dimX, 1);
beta12   = matrix( 1, dimX, 1);
beta22   = matrix( 1, dimX, 1);
beta11 = -2;
#beta11 = -2; # P.U2=0.5
#beta11 = 0;
beta21 = 1.0;

########set the mean and variance for X#############
muXi = matrix(0,dimX,1);
SigmaXi = matrix(0,dimX,dimX);
for(index1 in 1:dimX){
  for(index2 in 1:dimX){
    SigmaXi[index1,index2] = 0.5^abs(index1-index2);
  }
}

theta_base_grid = matrix(c(-40:40),81,1);
#theta_scaling = 0.05;
theta_scaling = 0.2;
#######range for theta_grid: [-7,9]##################
theta_grid = theta_base_grid*theta_scaling+t_0;
