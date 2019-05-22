### 이항분포
### binom(x,n,p)

# 주사위 세 번 던지기:  X = 1이 나온 횟수 
x <- 0:3
dbinom(x,3,1/6)

# n=8, p=0.3일 때
n <- 8
p <- 0.3
x <-0:8
prob <- dbinom(x,n,p)
names(prob) <- x
barplot(prob,space=20)
abline(h=0)

# 항암제 완치율
# - 어떤 암에 대한 기존 항암제의 완치율은 50%
# - 어느 제약회사에서 새로운 항암제를 개발하여 
#   항암제의 효과를 확인하기 위해 15명의 환자를 
#   대상으로 임상시험
# - 8명이 완치될 확률은?
dbinom(8,15,0.5)

# - 적어도 10명 이상 치유될 확률은? 
#   P(X >= 10) = P(X > 9) = 1-P(X <= 9)
1-pbinom(9,15,0.5)
pbinom(9,15,0.5,lower.tail=F)

# 환자 중 12명의 환자가 치유되었다면 
# -> 12 이상 치유될 확률
1-pbinom(11,15,0.5)


# !계산
factorial(10)
factorial(171)

### 초기하분포
### hyper(x,M,N-M,n)

# 6개가 정상품과 4개의 불량품이 있는 상자에서 임의로 3개의 제품을 
# 비복원 추출한 경우에 3개 중 1개가 불량품일 확률?
dhyper(1,4,6,3)

# 참고
# cdf: F(x)
phyper(0,4,6,3)
phyper(1,4,6,3)

# qauntile: p <= P(X <= x)를 만족하는 최소 x
qhyper(0.5,4,6,3)
qhyper(2/3,4,6,3)

# 난수
rhyper(10,4,6,3)

# 품질관리 ??? Operating Characteristic(OC) curve
# - 50개의 전구들이 들어 있는 상자에서 10개의 전구를 
#   무작위로 선택하여 검사
# - 불량전구의 개수가 1개 이하이면 이 회사의 전구를 구매
# - 만약 이 상자에 5개의 불량품이 있을 때, 구매할 확률은?
# P(X=0)+P(X=1)
phyper(1,5,45,10)

# 불량품이 k개 일 때
M <- 0:50
prob <- phyper(1,M,50-M,10)
plot(M,prob,type="o",xlab="불량품의 수",main="OC curve")




