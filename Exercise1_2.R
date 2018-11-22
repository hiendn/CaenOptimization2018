### Solution to Exercise 2

# Set the number n
nn <- 100
# Generate the points
tau <- seq(0,pi,length.out=nn)
# Begin by generating a column of 1s
XX <- cbind(rep(1,nn))
# Set K
KK <- 10
# Make a for loop to bind columns with cos and sin values of increasing frequency
for (kk in 1:KK) {
  XX <- cbind(XX,sin(pi/2*tau*kk),cos(pi/2*tau*kk))
}
# Make theta vector
theta <- matrix(c(rep(1,5),rep(0,2*KK-4)),2*KK+1,1)
# Set a random seed
set.seed(200)
# Generate the y values
yy <- XX%*%theta + rnorm(nn,0,sqrt(1/4))

# Compute the ordinary least squares estimate
theta_ols <- solve(t(XX)%*%XX)%*%t(XX)%*%yy
# Inspect the obtained estimates
theta_ols

# Get dimension of theta
DD <- length(theta)
# Make I_bar matrix
I_bar <- diag(c(0,rep(1,DD-1)))
# Set value of lambda
lambda <- 10
# Compute the ridge regression estimator
theta_ridge <- solve(t(XX)%*%XX+lambda*I_bar)%*%t(XX)%*%yy

# Plot the data points
plot(tau,yy,col='grey',
     xlab=expression(tau),ylab='y')
# Plot a grid in the background
grid()
# Make a dummy tau sequence for your plotting device
dum <- seq(0,pi,length.out = 1000)
# Initialize a vector of alpha (theta[1]) to store the true model values
y_true <- rep(theta[1],1000)
# Add the sin and cos values to your model
for (kk in 1:10) {
  y_true <- y_true + theta[2*kk]*sin(pi/2*dum*kk) + theta[2*kk+1]*cos(pi/2*dum*kk)
}
# Plot the true curve on top of your points, in black
lines(dum,y_true,col='black',lwd=2)
# Initialize a vector of alpha* (theta_ridge[1]) to store the ridge model values
y_ridge <- rep(theta_ridge[1],1000)
# Add the sin and cos values to your model
for (kk in 1:10) {
  y_ridge <- y_ridge + theta_ridge[2*kk]*sin(pi/2*dum*kk) + theta_ridge[2*kk+1]*cos(pi/2*dum*kk)
}
# Plot the fitted curve on top of your points, in blue
lines(dum,y_ridge,col='blue',lwd=2,lty=2)

# Initialize a vector of alpha* (theta_ols[1]) to store the ols model values
y_ols <- rep(theta_ols[1],1000)
# Add the sin and cos values to your model
for (kk in 1:10) {
  y_ols <- y_ols + theta_ols[2*kk]*sin(pi/2*dum*kk) + theta_ols[2*kk+1]*cos(pi/2*dum*kk)
}
# Plot the fitted curve on top of your points, in red
lines(dum,y_ols,col='red',lwd=2,lty=2)