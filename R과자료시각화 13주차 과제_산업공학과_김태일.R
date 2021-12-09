##R과 자료시각화 13주차 과제 산업공학과 2016039461 김태일

##데이터 불러오기

getwd()
setwd("C://Users//theil//Desktop//a")
dat = read.csv("dat - control_sepsis (refined).csv", header=T, skip=0)

##1. 각 수치형 변수들의 평균값을 구하고 이를 그룹별로 dot chart를 그리시오.
con = apply(dat[dat$LABEL == 'Control' ,-c(2,12)], 2, mean)
sep = apply(dat[dat$LABEL == 'Sepsis' ,-c(2,12)], 2, mean)

dotchart(con, labels = colnames(dat), main = 'Sepsis control')
par(new=TRUE)
dotchart(sep, labels = colnames(dat), pch=16)


##2. 1번의 변수들의 평균값을 그룹별로 spider plot으로 그리시오

dat_ = dat[,-c(2)]
summ = NULL
for(i in 1:10){
  summ = cbind(summ, tapply(dat_[,i], INDEX = dat_[,11], mean))
}
colnames(summ) = colnames(dat_)[1:10]
stars(rbind(rep(0,10), rep(mean(summ),10), summ),locations = c(0,0), col.lines = c(1,0:3), lwd = 2)
stars(matrix(rep(seq(0,1,by=.2),each=4),ncol=10, byrow=T), locations = c(0,0),len=1, lty=2,lwd=1,key.loc = c(0,0),
      key.labels = paste(colnames(summ),"mean:", apply(summ,2,mean)),add = T)

##3.그룹별로 나이를 10대, 20대, ...80대, 90대 이상으로 구분하여 개수를 count하고 나이대에 따른 각 그룹에 속하는 count를 
##막대그래프로 그리시오.


dat_1 = dat_[, c(1,11)]
for (i in 1:5022){
  if(dat_1[i,1] < 20){dat_1[i,1] = '10대'}
  else if(dat_1[i,1] < 30 & dat_1[i,1] > 19){dat_1[i,1] = '20대'}
  else if(dat_1[i,1] < 40 & dat_1[i,1] > 29){dat_1[i,1] = '30대'}
  else if(dat_1[i,1] < 50 & dat_1[i,1] > 39){dat_1[i,1] = '40대'}
  else if(dat_1[i,1] < 60 & dat_1[i,1] > 49){dat_1[i,1] = '50대'}
  else if(dat_1[i,1] < 70 & dat_1[i,1] > 59){dat_1[i,1] = '60대'}
  else if(dat_1[i,1] < 80 & dat_1[i,1] > 69){dat_1[i,1] = '70대'}
  else if(dat_1[i,1] < 90 & dat_1[i,1] > 79){dat_1[i,1] = '80대'}
  else if(dat_1[i,1] > 89){dat_1[i,1] = '90대 이상'}
}
t.dat_1 = table(dat_1)

barplot(t.dat_1, las=2, col=c(4,2,1), beside=T)

##4.반원형 도넛차트로 그룹별 나이대의 비율을 나타내시오.

par(mfrow = c(1,2))
con1 = dat_1[dat_1$LABEL == "Control",]
sep1 = dat_1[dat_1$LABEL == "Sepsis",]
pie(c(as.data.frame(table(con1))$Freq, sum(as.data.frame(table(con1))$Freq)))
par(new=T)
pie(1, labels="",border = NA, radius=0.5)
rect(-1,-1,1,0,col='white', border=NA)
text(0,0,"그룹별 나이비율 Control", pos=3)
pie(c(as.data.frame(table(sep1))$Freq, sum(as.data.frame(table(sep1))$Freq)))
par(new=T)
pie(1, labels="",border = NA, radius=0.5)
rect(-1,-1,1,0,col='white', border=NA)
text(0,0,"그룹별 나이비율 Sepsis", pos=3)

##5.각 수치형 변수별로 그룹에 따른 히스토그램을 모두 그리시오


par(mfrow=c(3,4))
for(i in 1:10){
  hist(dat_[,i][dat_$LABEL =="Control"], breaks = seq(floor(min(dat_[,i])),ceiling(max(dat_[,i])), by = .1),
       col='red',
       main = paste("Histogram of",names(dat_)[i]), xlab = names(dat_)[i])
  hist(dat_[,i][dat_$LABEL=="Sepsis"], breaks = seq(floor(min(dat_[,i])),ceiling(max(dat_[,i])), by = .1),
       col='blue', add = T)
}

##dat$Hemoglobin, dat$MPV, dat$RBC.COUNT

##6. 5번에서 선정한 3개의 변수에 대하여 그룹별로 boxplot을 그려 비교하시오.

dat_2 = dat[,c(4,8,10,12)]
par(mfrow=c(1,3))
boxplot(Hemoglobin ~ LABEL , data = dat_2, main = paste(names(dat_2)[1], "요약"), horizontal = T, notch = T)
boxplot(MPV ~ LABEL , data = dat_2, main = paste(names(dat_2)[2], "요약"), horizontal = T, notch = T)
boxplot(RBC.COUNT ~ LABEL , data = dat_2, main = paste(names(dat_2)[3], "요약"), horizontal = T, notch = T)


##7.번pairs등 13주차 그래프를 그리기에는 데이터의 크기가 너무 크다. 이를 해결하기 위한 아이디어와 그 근거를 제시하고, 그 
##방법을 이용하여 그룹별 색을 구분하여 pairs그래프를 그리시오.

##이유설명 변수가 너무 많으니까 유의미한 변수만 골라 그린다.

pairs(dat_2[,-4]) ##라벨제외

##8.번 6번의 아이디어를 적용하여 그룹별 색을 구분하여 spider plot을 그리시오.

stars(dat_2[dat_2[,4]=='Control' ,-4], full=T, radius = T, len = 1,  locations = c(0,0), col.lines = 'red',lty=2,lwd=1, key.loc = c(0,0))
stars(dat_2[dat_2[,4]=='Sepsis',-4], full=T, radius = T, len = 1,  locations = c(0,0), col.lines = 'blue',lty=2,lwd=1, key.loc = c(0,0), add=T)

##9.번6번의 아이디어를 이용하여 그룹별 색을 구분하여 parallel-coordinates plot을 그리시오

##변수의 scale을 변경하지 않은 plot

plot(0,0,xlim = c(1,4), ylim = c(0,max(dat_2[,-4])),
     type = "n", xaxt = "n", xlab = "", ylab = "", main = "Sepsis data") # 그래프 틀 생성
axis(1, at = 1:3, labels = names(dat_2)[1:3]) # 좌표축 설정

con_2 = dat_2[dat_2[,4]=='Control',]
sep_2 = dat_2[dat_2[,4]=='Sepsis',]
for (i in 1:nrow(con_2)){
  lines(1:3,con_2[i,-4], col = 'red')
}
for (i in 1:nrow(sep_2)){
  lines(1:3,sep_2[i,-4], col = 'blue')
}

##변수의 scale을 변경한 경우

dat_2$Hemoglobin = (dat_2$Hemoglobin-min(dat_2$Hemoglobin))/(max(dat_2$Hemoglobin)-min(dat_2$Hemoglobin))
dat_2$MPV = (dat_2$MPV-min(dat_2$MPV))/(max(dat_2$MPV)-min(dat_2$MPV))
dat_2$RBC.COUNT = (dat_2$RBC.COUNT-min(dat_2$RBC.COUNT))/(max(dat_2$RBC.COUNT)-min(dat_2$RBC.COUNT))




plot(0,0,xlim = c(1,4), ylim = c(0,1),
     type = "n", xaxt = "n", xlab = "", ylab = "", main = "Sepsis data") # 그래프 틀 생성
axis(1, at = 1:3, labels = names(dat_2)[1:3]) # 좌표축 설정


con_2 = dat_2[dat_2[,4]=='Control',]
sep_2 = dat_2[dat_2[,4]=='Sepsis',]
for (i in 1:nrow(con_2)){
  lines(1:3,con_2[i,-4], col = 'red')
}
for (i in 1:nrow(sep_2)){
  lines(1:3,sep_2[i,-4], col = 'blue')
}


##10.번 6번의 아이디어를 이용하여 5번에서 선정한 3개 변수에 대하여 그룹별 색을 구분하여 3차원
##그래프를 회정하는 애니메이션으로 만들어 저장하시오

install.packages("animation")
install.packages("rgl")
install.packages("plot3D")
animation::ani.record(reset = TRUE)

for (i in seq(0,360, 5)){
  
  plot3D::scatter3D(x = dat_2[,1],
                    y = dat_2[,2],
                    z = dat_2[,3],
                    col = (if(dat_2[,'LABEL']=='Sepsis'){'red'}else{'blue'}),
                    pch = 16,
                    cex = 2,
                    colkey = F,
                    main = "3D plot of Sepsis data",
                    xlab = names(dat_2)[1],
                    ylab = names(dat_2)[2],
                    zlab = names(dat_2)[3],
                    phi = 10, theta = i, r = sqrt(3)
  )

  

  
  animation::ani.record()
  
}

oopts = animation::ani.options(
  interval = 0.1,
  ani.width = 900,
  ani.height = 600
)

animation::saveGIF(expr = animation::ani.replay(),
                   movie.name = "C://Users//theil//Desktop//a//10번.gif")
