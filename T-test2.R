rm(list=ls())
#### p-value calculation ####
curve(dnorm(x, 5, 2),lwd = 3, xlim = c(-5,15))
x <- rnorm(10, mean = 5, sd = 2)
mean(x)
t.test(x, mu = 7)

##### 中央極限定理 #####
##1. 從常態分佈 (mean = 5, sd = 2) 中隨機抽取 10 個樣本，其平均的分布
mean (rnorm(10, mean = 5, sd = 2))

c.m <- numeric(1000)
c.sd <- numeric(1000)
for( i in 1:1000){
  a <- rnorm(10, 5, 2)
  c.m[i] <- mean(a)
  c.sd[i] <- sd(a)
}

hist(c.m, freq=F, breaks = 19, main = "樣本數 = 10")
curve(dnorm(x, 5, 2/sqrt(10)), add=T, col = 1, lty = 1, lwd = 2)

##2. 把平均的分布標準化
s.c.m <- (c.m-5)/(c.sd/sqrt(10))
hist(s.c.m, freq=F, ylim= c(0,0.4),  main = "樣本數 = 10", breaks = 19)
curve(dnorm(x, 0, 1), add=T, col = 1, lty = 1, lwd = 2)

polygon (c(seq(-4, qnorm(0.025, 0, 1), length = 100), rev(seq(-4, qnorm(0.025, 0, 1), length = 100))),
         c(dnorm (seq(-4, qnorm(0.025, 0, 1), length = 100), 0, 1),rep(0, 100)),
         col = rgb(red=1, green=0, blue=0, alpha = 0.5), border = NA)
polygon (c(seq(qnorm(0.975, 0, 1), 4, length = 100), rev(seq(qnorm(0.975, 0, 1), 4, length = 100))),
         c(dnorm (seq(qnorm(0.975, 0, 1), 4, length = 100), 0, 1),rep(0, 100)),
         col = rgb(red=1, green=0, blue=0, alpha = 0.5), border = NA)

##3. 信賴區間 
text(-2, 0.15, paste0("down = ",round(qnorm(0.025, 0, 1)*(2/sqrt(10))+5, 2)), cex = 1.5)
text( 2, 0.15, paste0("up = ", round(qnorm(0.975, 0, 1)*(2/sqrt(10))+5, 2)), cex = 1.5)


##4. 理論數值與實際數值
length(s.c.m[s.c.m<qnorm(0.025, 0, 1)|s.c.m>qnorm(0.975, 0, 1)])/1000

##5. T 分布 and 常態分佈
hist(s.c.m, freq=F, ylim= c(0,0.4),  main = "樣本數 = 10", breaks = 19)
curve(dnorm(x, 0, 1), add=T, col = 1, lty = 1, lwd = 2)
curve(dt(x, 30), add=T, col = 2, lty = 1, lwd = 2)

abline (v = qnorm(0.025, 0, 1), lwd = 2)
abline (v = qt(0.025,9), col = 2, lwd = 2)
#根據常態切位的實際數值
text(-1.8, 0.35, paste0("PN = ", length(s.c.m[s.c.m<qnorm(0.025, 0, 1)|s.c.m>qnorm(0.975, 0, 1)])/1000), 
     adj=c(0,1), cex = 1.5)
text(-3.5, 0.35, paste0("PT = ", length(s.c.m[s.c.m<qt(0.025, 9)|s.c.m>qt(0.975, 9)])/1000), 
     adj=c(0,1), cex = 1.5, col = 2)

t.test(x, mu = 7)
(mean(x)-7)/(sd(x)/sqrt(10))
pt(-4.976963, 9)*2

##### p-value  #####
n <- c(seq(2,30,1), 40, 60, 80, 100)
rep <- 30

pT.valus <- NULL
pN.valus <- NULL

for (k in 1:rep){
  pT <- rep(NA, length(n))
  pN <- rep(NA, length(n))
  for (j in 1:length(n)){
    s.t <- numeric(1000)
    for( i in 1:1000 ){
      s.m <- rnorm(n[j],5,2)
      s.t[i] <- (mean(s.m)-5)/(sd(s.m)/sqrt(n[j]))
    }
    pT [j] <- length(s.t[s.t<qt(0.025, n[j]-1) | s.t>qt(0.975, n[j]-1)])/1000
    pN [j] <- length(s.t[s.t<qnorm(0.025, 0, 1) | s.t>qnorm(0.975, 0, 1)])/1000
  }
  pT.valus <- cbind (pT.valus, pT)
  pN.valus <- cbind (pN.valus, pN)
}

rownames(pT.valus) <- n
rownames(pN.valus) <- n

place <- c(seq(2,30,1), 33, 35, 37, 39)
plot(place, apply(pT.valus, 1, mean), ylim = c(0,0.4), pch = 19, xaxt = "n",
     xlab = "樣本數", ylab = "P-value", las = 1)
abline (h = 0.05, lty = 3)
arrows(place,apply(pT.valus, 1, mean) + apply(pT.valus, 1, sd),
       place,apply(pT.valus, 1, mean) - apply(pT.valus, 1, sd),
       code=3,length=0.05,angle=90,lty=3, col=1)
axis (1, at = place, labels = n)
axis (2, at = 0.05, labels = 0.05, las = 1)
text (5, 0.08, "T-distribution")

points(place, apply(pN.valus, 1, mean), ylim = c(0,0.3), pch = 19, col = 2)
arrows(place,apply(pN.valus, 1, mean) + apply(pN.valus, 1, sd),
       place,apply(pN.valus, 1, mean) - apply(pN.valus, 1, sd),
       code=3,length=0.05,angle=90,lty=3, col=2)
text (10, 0.15, "Normal-distribution", col =2)


#boxplot(t(pT.valus), at = place, ylim = c(0,0.4))
#boxplot(t(pN.valus), at = place, ylim = c(0,0.4), add = T, col = 2)
#abline (h = 0.05, lty = 3)

