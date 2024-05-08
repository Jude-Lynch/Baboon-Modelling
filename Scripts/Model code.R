  ## Cape peninsula Chacma baboon population model ##
par(mfrow=c(1,1))

# Allocate time-span
tspan <- 100

#AF>I = 0.657 so AF>IF & AF>IM = 
1.0308/2  #0.5154

# Class transitions rates
F1m <- 0.3623 #IM>SAM
F1f <- -0.2485 #IF>SAF
F2m <- 0.3670 #SAM>AM
F2f <- 0.6006 #SAF>AF
F3m <- 0.5154 #AF>IM
F3f <- 0.5154 #AF>IF

#proportion that remain in same class (1-Fx)
RIM <- 1-F1m  #IM
RIF <- 1-F1f  #IF
RSAM <- 1-F2m #SAM
RSAF <- 1-F2f #SAF

# Survival rates 
P1m <- 0.9*RIM #IM>IM = 0.6584
P1f <- 0.9*RIF #IF>IF = 0.7435
P2m <- 0.9*RSAM #SAM>SAM = 0.6072
P2f <- 0.9*RSAF #SAF>SAF = 0.2509
P3m <- 0.9 #AM>AM
P3f <- 0.9 #AF>AF
  
# Transition Matrix
# row is the source class and column is the destination class
TM <- matrix(c(P1m, 0, 0, 0, 0, F2m,
               0, P1f, 0, 0, 0, F3f,
               F1m, 0, P2m, 0, 0, 0,
               0, F1f, 0, P2f, 0, 0,
               0, 0, F2m, 0, P3m, 0,
               0, 0, 0, F2f, 0, P3f), nrow = 6, ncol = 6, byrow = TRUE)

# Pre-allocate matrix to store pop densities for each age class across time
n <- matrix(0, nrow = 6, ncol = tspan)

# Initiate pop densities
#  classes: IM, IF, SAM, SAF, AM, AF
#   in 2018:
n[, 1] = c(111, 118, 20, 21, 18, 143)

# simulate pop trajectories
for(t in 1:(tspan-1)){1
  n[, t+1] = TM %*% n[,t]}   # matrix-vector product 

# Plot as time series
plot(1:tspan, n[1,], type = 'l', col = 'lightskyblue1', lwd = 2, xlab = "Time, t", 
     ylab = "Abundance, N(i,t)", ylim = c(0,5000))               #IM
lines(1:tspan, n[2,], col = 'lightpink', lwd = 2)   #IF
lines(1:tspan, n[3,], col = 'royalblue1', lwd = 2)  #SAM
lines(1:tspan, n[4,], col = 'hotpink1', lwd = 2) #SAF
lines(1:tspan, n[5,], col = 'blue3', lwd = 2) #AM
lines(1:tspan, n[6,], col = 'maroon', lwd = 2)   #AF
legend("topleft", legend = c("IM", "IF", "SAM", "SAF", "AM", "AF"), 
       col = c("lightskyblue1", "lightpink", "royalblue1", "hotpink1", "blue3", "maroon"), 
       lwd = 2, cex = 0.5, title = "Classes", bg = "white")

## Actual class sizes in first and last year of data
#  classes: IM, IF, SAM, SAF, AM, AF
#2018 
c(111, 118, 20, 21, 18, 143)
#2023
c(118, 127, 31, 15, 35, 162)


