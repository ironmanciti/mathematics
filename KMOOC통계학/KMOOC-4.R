
### Table 형태의 데이터 읽어오기
smart <- read.table("smart.txt",header=T)

### Comma Separated Value(csv) 형식의 데이터 읽어오기
smart <- read.csv("smart.csv")

head(smart)
tail(smart)

### 분할표
table(gender,model) 
# 1
smarttable <- table(smart$gender,smart$model)
# 2
with(smart,table(gender,model))
# 3
attach(smart)
table(gender,model)
detach(smart)

margin.table(smarttable,1)  ## row
margin.table(smarttable,2)  ## column

prop.table(smarttable)
prop.table(smarttable,1)
prop.table(smarttable,2)
smart.prop <- round(100*prop.table(smarttable,1),1)

# 원도표
par(mfrow=c(1,2))
pie(smart.prop[1,],main="남자")
pie(smart.prop[2,],main="여자")

### 3-차원 분할표
titanic <- read.csv("titanic.csv")
head(titanic)
tail(titanic)

ttn.table <- with(titanic,table(Class,Survived,Group))
ftable(ttn.table)
table3way <- ftable(ttn.table,row.vars="Class",col.vars=c("Group","Survived"))
ttn.ftable <- data.frame(table3way)
ttn.survive <- ttn.ftable[ttn.ftable$Survived == "Yes",]

# 등실별생존율
survive <- with(ttn.survive,tapply(Freq,Class,sum))
total <- with(ttn.ftable,tapply(Freq,Class,sum))
round(100*survive/total,1)

# 그룹별 생존율
survive <- with(ttn.survive,tapply(Freq,Group,sum))
total <- with(ttn.ftable,tapply(Freq,Group,sum))
round(100*survive/total,1)

id <- 1:12
ttn.survive$Rate <- round(100*ttn.survive$Freq/(ttn.ftable$Freq[id]+ttn.ftable$Freq[id+12]),1)

par(mfrow=c(1,1))
ttnbar <- matrix(ttn.survive$Rate,4,3)
row.names(ttnbar) <- c("1등급","2등급","3등급","승무원")
colnames(ttnbar) <- c("남자","어린이","여자")
barplot(ttnbar, beside = TRUE, legend = rownames(ttnbar), ylim = c(0, 100))
abline(h=c(20,40,60,80,100),lty=3)
abline(h=0)

### 지방선거 득표율
election <- read.csv("election.csv")
with(election,by(득표율,정당,summary))

shortsummary <- function(x)
{
  result <- round(c(length(x),mean(x),sd(x),min(x),max(x)),2)
  names(result) <- c("N","평균","표준편차","최솟값","최댓값")
  return(result)
}
with(election,by(득표율,정당,shortsummary))

boxplot(득표율~정당,data=election)
boxplot(득표율~정당,data=election,horizontal=T,xlab="득표율")

### Olympic 육상 100M 우승기록
olympic <- read.csv("100m.csv")
head(olympic)
tail(olympic)

with(olympic,plot(year,record))
male <- subset(olympic,gender=="M")
female <- subset(olympic,gender=="F")
with(male,plot(year,record,ylim=c(9,13),cex=1.2))
with(female,points(year,record,pch=16,cex=1.2))
legend(2000,13,legend=c("남자","여자"),pch=c(1,16),bty="n",cex=1.2)

n <- nrow(male)
Sxy <- sum(male$year*male$record)-sum(male$year)*sum(male$record)/n
Sxy/(n-1)   # 공분산
Sxx <- sum(male$year^2)-sum(male$year)^2/n
Syy <- sum(male$record^2)-sum(male$record)^2/n
Sxy/sqrt(Sxx*Syy)

with(male,cov(year,record))
with(male,cor(year,record))

### 월별 하루평균 출생아 수
birth <- scan()
1385	1309	1322	1278	1194	1201	1197	1238	1410	1407	1377	1146
1535	1363	1395	1346	1257	1226	1231	1270	1299	1237	1244	1096
1451	1400	1394	1336	1271	1272	1294	1337	1391	1351	1286	1107
1427	1309	1243	1225	1149	1105	1167	1174	1235	1163	1128	1031
1330	1313	1226	1239	1153	1139	1174	1179	1263	1176	1079	1055

birth <- ts(birth, start=c(2010,1), frequency=12)
plot(birth,xlab="년월",ylab="출생아수",type="o")
abline(h=c(1100,1300,1500),lty=3)



                 


