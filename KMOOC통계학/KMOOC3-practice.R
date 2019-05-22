## Ãë¾÷·ü
Job <- scan()
55.6 83.3 43.4 58.1 31.6 55.6 60.7 64.6 73.3 55.6 64.3
52.8 22.7 46.3 71.4 53.8 64.5 67.9 71.4 80.0 59.5 40.5
77.1 58.6 65.4 52.4 66.7 91.3 41.3 72.1 61.9 78.4 63.6
41.0 65.2 81.3 54.8 19.6 50.0 53.1 41.2 56.5

total <- sum(Job)
n <- length(Job)
total/n
mean(Job)
median(Job)
mean(Job, trim=0.1)
mean(Job, trim=0.5)
mean(Job, trim=0)
Job <- sort(Job)
trim <- c(1:3, (n-2):n)
mean(Job[-trim])
mode(Job)
freq <- table(Job)
maxfreq <- max(freq)
which(freq == maxfreq)
ends <- range(Job)
diff(ends)
max(Job) - min(Job)
quantile(Job)
Q <- quantile(Job, prob=c(0.25, 0.5, 0.75))
IQR(Job)
Q[3] - Q[1]
boxplot(Job)
boxplot(Job, horizontal = TRUE, xlab="Ãë¾÷·ü")
var(Job)
sd(Job)
s <- sd(Job)
xbar <- mean(Job)
z <- (Job - xbar)/s
abdev <- abs(Job - median(Job))
scale(Job)
dist.shape <- function(x)
{
  n <- length(x)
  kurtosis <- sum(z^4)/(n-1)
  result <- c(NA, NA)
  IF (n>=2){
    result <- c(ckew, jrt)
  }
}