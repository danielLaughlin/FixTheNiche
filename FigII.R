### Fitness function for Niches

library(ggplot2)

niche <- function(a, b, d){
  fitness <- a + b - d*a*b
}

param <- expand.grid(a=seq(-1,1,0.01),b=seq(-1,1,0.01))
fitness_d0 <- data.frame(param, fitness=c(0))
fitness_d05 <- data.frame(param, fitness=c(0))
fitness_d1 <- data.frame(param, fitness=c(0))

for(i in 1:nrow(fitness_d0)){
    fitness_d0$fitness[i] <- niche(fitness_d0$a[i], fitness_d0$b[i], d=0)
}
for(i in 1:nrow(fitness_d05)){
  fitness_d05$fitness[i] <- niche(fitness_d05$a[i], fitness_d05$b[i], d=0.5)
}
for(i in 1:nrow(fitness_d1)){
  fitness_d1$fitness[i] <- niche(fitness_d1$a[i], fitness_d1$b[i], d=1)
}

p1 <- ggplot(fitness_d0, aes(a, b, z=fitness)) + labs(title="Dispersal = 0") + geom_contour_filled() +
  xlab("Abiotic") + ylab("Biotic") +
  guides(fill = guide_colorsteps(title = "Population
growth rate"))
p2 <- ggplot(fitness_d05, aes(a, b, z=fitness)) + labs(title="Dispersal = 0.5") + geom_contour_filled() +
  xlab("Abiotic") + ylab("Biotic")+
  guides(fill = guide_colorsteps(title = "Population
growth rate"))
p3 <- ggplot(fitness_d1, aes(a, b, z=fitness)) + labs(title="Dispersal = 1") + geom_contour_filled() +
  xlab("Abiotic") + ylab("Biotic")+
  guides(fill = guide_colorsteps(title = "Population
growth rate"))
ggpubr::ggarrange(p1, p2, p3, nrow=1, ncol=3, common.legend=TRUE, legend="right")



###### simulate three species for shared preference #######

### simulate three species with different a functions
t <- seq(0,20,0.1)

## simulate three species
a1a <- function(t) -0.1 + 3* 1/(3+exp(18-1.2*t))
a2a <- function(t) -0.1 + 3* 1/(3.75+exp(8-1.0*t))
a3a <- function(t) -0.1 + 3* 1/(4.75+exp(2-0.3*t))
plot(t, a1a(t), type="l")
lines(t, a2a(t))
lines(t, a3a(t))
                     
### simulate one b function with minimum at 20
b_a <- function(t) -1 + 0.25*((t - 20)^2 / 100)
plot(t, b_a(t), type="l")

# calculate realized fitness
fit1a <- niche(a1a(t), b_a(t), d=0.5)
fit2a <- niche(a2a(t), b_a(t), d=0.5)
fit3a <- niche(a3a(t), b_a(t), d=0.5)
plot(t, fit1a, type="l")
lines(t, fit2a)
lines(t, fit3a)


###### simulate three species for centrifugal organization #######

### simulate three species with different r functions
t <- seq(0,20,0.1)
# f(x) = a * exp(-(x - b)^2 / (2 * c^2)) Where:
#a controls the height of the peak
#b is the position of the peak (in this case, 10)
#c controls the width of the curve
a1b <- function(t) -1 + 2 * exp(-(t - 15)^2 / 60)
a2b <- function(t) -1 + 2 * exp(-(t - 10)^2 / 60)
a3b <- function(t) -1 + 2 * exp(-(t - 5)^2 / 60)
plot(t, a1b(t), type="l")
lines(t, a2b(t))
lines(t, a3b(t))

### simulate one b function with minimum at 10
#phi_b <- function(t) -1 + 1*((t - 10)^2 / 100)
b_b <- function(t) -1 + 1*((t - 10)^2 / 100)
plot(t, b_b(t), type="l") 

# calculate realized fitness
fit1b <- niche(a1b(t), b_b(t), d=0.5)
fit2b <- niche(a2b(t), b_b(t), d=0.5)
fit3b <- niche(a3b(t), b_b(t), d=0.5)
plot(t, fit1b, type="l")
lines(t, fit2b)
lines(t, fit3b)


### plot!
par(mfrow=c(1,3))

plot(t, b_a(t), type="l", main="(i) Competition function",
     ylab="Biotic effects", xlab="Temperature (deg C)", lwd=3) 
lines(t, b_b(t), lty=3, lwd=3)
legend(2,0,legend=c("Shared preference","Centrifugal organization"),lty=c(1,3),bty="n",cex=1)

plot(t, a1a(t), type="l", col="gold", lwd=3, ylim=c(0,1),
     main="(ii) Shared preference", xlab="Temperature (deg C)",
     ylab="Population growth rate")
lines(t, a2a(t), col="grey", lwd=3)
lines(t, a3a(t), col="blue", lwd=3)
lines(t, fit1a, lty=2, col="gold", lwd=3)
lines(t, fit2a, lty=2, col="grey", lwd=3)
lines(t, fit3a, lty=2, col="blue", lwd=3)
legend(0,1,legend=c("Realized niche (Abiotic + Biotic)","Fundamental niche (Abiotic only)"),lty=c(2,1),bty="n",cex=1)
legend(0,0.8,lty=1,col=c("gold","blue"), legend=c("Contracts into warm","Contracts into cold"),bty="n",cex=1)

plot(t, a1b(t), type="l", col="gold", lwd=3, ylim=c(0,1),
     main="(iii) Centrifugal organization", xlab="Temperature (deg C)",
     ylab="Population growth rate")
lines(t, a2b(t), col="grey", lwd=3)
lines(t, a3b(t), col="blue", lwd=3)
lines(t, fit1b, lty=2, col="gold", lwd=3)
lines(t, fit2b, lty=2, col="grey", lwd=3)
lines(t, fit3b, lty=2, col="blue", lwd=3)


###############################################################################


