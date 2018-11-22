### Solution to Exercise 1

# Set the number n
nn <- 100
# Generate the points
tau <- seq(0,pi,length.out=nn)
# Begin by generating a column of 1s
XX <- cbind(rep(1,nn))
# Set K
KK <- 2
# Make a for loop to bind columns with cos and sin values of increasing frequency
for (kk in 1:KK) {
  XX <- cbind(XX,sin(pi/2*tau*kk),cos(pi/2*tau*kk))
}
# Make theta vector
theta <- matrix(1:5,5,1)
# Set a random seed
set.seed(150)
# Generate the y values
yy <- XX%*%theta + rnorm(nn,0,sqrt(1/2))
# Plot data
plot(tau,yy)
grid()
# Compute the ordinary least squares estimate
theta_ols <- solve(t(XX)%*%XX)%*%t(XX)%*%yy
# Initialize the algorithm
theta_lad <- theta_ols
# Set the value of epsilon 
epsilon <- 0.001
# Set the number of iterations R
RR <- 100
for (rr in 1:RR) {
  # Compute the weight matrix 
  WW <- diag(c(1/sqrt((yy-XX%*%matrix(theta_lad,5,1))^2+epsilon)))
  # Compute the new iteration
  theta_lad <- solve(t(XX)%*%WW%*%XX)%*%t(XX)%*%WW%*%yy
}
# Print the east absolute deviation estimator
theta_lad

# Initialize a vector of alpha (theta[1]) to store the true model values
y_true <- rep(theta[1],1000)
# Add the sin and cos values to your model
for (kk in 1:2) {
  y_true <- y_true + theta[2*kk]*sin(pi/2*dum*kk) + theta[2*kk+1]*cos(pi/2*dum*kk)
}
# Plot the true curve on top of your points, in black
lines(dum,y_true,col='black',lwd=2)
# Initialize a vector of alpha* (theta_ridge[1]) to store the ridge model values
y_lad <- rep(theta_lad[1],1000)
# Add the sin and cos values to your model
for (kk in 1:2) {
  y_lad <- y_lad + theta_lad[2*kk]*sin(pi/2*dum*kk) + theta_lad[2*kk+1]*cos(pi/2*dum*kk)
}
# Plot the fitted curve on top of your points, in blue
lines(dum,y_lad,col='blue',lwd=2,lty=2)

# Initialize a vector of alpha* (theta_ols[1]) to store the ols model values
y_ols <- rep(theta_ols[1],1000)
# Add the sin and cos values to your model
for (kk in 1:2) {
  y_ols <- y_ols + theta_ols[2*kk]*sin(pi/2*dum*kk) + theta_ols[2*kk+1]*cos(pi/2*dum*kk)
}
# Plot the fitted curve on top of your points, in red
lines(dum,y_ols,col='red',lwd=2,lty=2)
