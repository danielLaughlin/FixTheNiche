# conceptual figure 1

library(sn)

pdf("FigI.pdf",width=12,height=8)

par(mfrow=c(2,3),mar=c(5,6,4,0), lwd=2, bty="n")
t <- seq(-14,14,length.out=200)

r1 <- dnorm(t,mean=0, sd=2)
f1 <- dnorm(t,mean=0, sd=2.5)*1.25
plot(t,r1,type="l", lty=2, col="white", main="(A) Symmetrical, Trivial Contraction", 
     xlab="", ylab="Probability of Occurrence",
     col.axis="white", cex.lab=1.5, cex.main=1.2, lwd=4)
polygon(c(t, rev(t)), c(f1, rep(0, length(f1))), col = "grey30", border=NA)
polygon(c(t, rev(t)), c(r1, rep(0, length(r1))), col = "grey70", border=NA)
text(9,0.15,"High R:F ratio
No directional contraction
No skew")

r1 <- dnorm(t,mean=-5, sd=2)
f1 <- sn::dsn(t, xi = -7.6, omega = 5, alpha = 3)*1.52
plot(t,r1,type="l", lty=2, col="white", main="(B) Skewed positive, Trivial Contraction",
     xlab="", ylab="", col.axis="white", cex.lab=1.5, cex.main=1.2, lwd=4)
polygon(c(t, rev(t)), c(f1, rep(0, length(f1))), col = "grey30", border=NA)
polygon(c(t, rev(t)), c(r1, rep(0, length(r1))), col = "grey70", border=NA)
text(8,0.15,"High R:F ratio
Negative contraction
Positive skew")
legend("topright",legend=c("Realized niche","Fundamental niche"),
       fill=c("grey70","grey30"),border=NA,cex=1.1)

r1 <- dnorm(t,mean=5, sd=2)
f1 <- sn::dsn(t, xi = 7.6, omega = 5, alpha = -3)*1.52
plot(t,r1,type="l", lty=2, col="white", main="(C) Skewed negative, Trivial Contraction",
     xlab="", ylab="", col.axis="white", cex.lab=1.5, cex.main=1.2, lwd=4)
polygon(c(t, rev(t)), c(f1, rep(0, length(f1))), col = "grey30", border=NA)
polygon(c(t, rev(t)), c(r1, rep(0, length(r1))), col = "grey70", border=NA)
text(-8,0.15,"High R:F ratio
Positive contraction
Negative skew")

r1 <- dnorm(t,mean=0, sd=2)
f1 <- dnorm(t,mean=0, sd=5)*2.5
plot(t,r1,type="l", lty=2, col="white", main="(D) Symmetrical, Non-Trivial Contraction", 
     xlab="Environmental gradient", ylab="Probability of Occurrence",
     col.axis="white", cex.lab=1.5, cex.main=1.2, lwd=4)
polygon(c(t, rev(t)), c(f1, rep(0, length(f1))), col = "grey30", border=NA)
polygon(c(t, rev(t)), c(r1, rep(0, length(r1))), col = "grey70", border=NA)
text(9,0.18,"Low R:F ratio
No directional contraction
No skew")

r1 <- dnorm(t,mean=-5, sd=2)
f1 <- sn::dsn(t, xi = -9, omega = 9, alpha = 3)*2.8
plot(t,r1,type="l", lty=2, col="white", main="(E) Skewed positive, Non-Trivial Contraction",
     xlab="Environmental gradient", ylab="", col.axis="white", cex.lab=1.5, cex.main=1.2, lwd=4)
polygon(c(t, rev(t)), c(f1, rep(0, length(f1))), col = "grey30", border=NA)
polygon(c(t, rev(t)), c(r1, rep(0, length(r1))), col = "grey70", border=NA)
text(8,0.15,"Low R:F ratio
Negative contraction
Positive skew")

r1 <- dnorm(t,mean=5, sd=2)
f1 <- sn::dsn(t, xi = 9, omega = 9, alpha = -3)*2.8
plot(t,r1,type="l", lty=2, col="white", main="(F) Skewed negative, Non-Trivial Contraction",
     xlab="Environmental gradient", ylab="", col.axis="white", cex.lab=1.5, cex.main=1.2, lwd=4)
polygon(c(t, rev(t)), c(f1, rep(0, length(f1))), col = "grey30", border=NA)
polygon(c(t, rev(t)), c(r1, rep(0, length(r1))), col = "grey70", border=NA)
text(-8,0.15,"Low R:F ratio
Positive contraction
Negative skew")

dev.off()
