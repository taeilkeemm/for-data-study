##R과 자료시각화 기말 과제
##산업공학과 2016039461 김태일


###1번

#iris데이터 불러오기
dat = iris


##1) 설명변수 4개 차원에 대하여 기초분석을 수행하시오.
## EDA 1 : class 별 분포 확인
## histogram
for(i in 1:5){
  hist(dat[,i][dat$Species=="setosa"], freq = F,
       breaks = seq(floor(min(dat[,i])),ceiling(max(dat[,i])), length.out = 20),
       col = scales::alpha(1,.3),
       main = paste("Histogram of",names(dat)[i]), xlab = names(dat)[i])
  hist(dat[,i][dat$Species=="versicolor"],  freq = F,
       breaks = seq(floor(min(dat[,i])),ceiling(max(dat[,i])), length.out = 20),
       col = scales::alpha(2,.3), add = T)
  hist(dat[,i][dat$Species=="virginica"],  freq = F,
       breaks = seq(floor(min(dat[,i])),ceiling(max(dat[,i])), length.out = 20),
       col = scales::alpha(4,.3), add = T)
  legend("topright", legend = rev(levels(dat$Species)), 
         fill = c(scales::alpha(1,.3),scales::alpha(2,.3),scales::alpha(4,.3)))
  readline("press enter")
}



## EDA 2 : 변수간 상관관계 + class 분포 확인

## scatter plot
plot(dat[,1:5], col = dat$Species)

## pairs plot
pairs(dat[,-ncol(dat)], pch = 1, col = dat$Species)
##corrplot
corrplot::corrplot(cor(dat[,-5]))



##EDA 3 : parallel coordinate plot


plot(0,0,xlim = c(1,ncol(dat)), ylim = range(dat[,-5]), type = "n", xaxt = "n", xlab = "", ylab = "", main = "parallel coordinate plot")
# 그래프 틀 생성
axis(1, at = 1:ncol(dat), labels = names(dat), las = 2) # 좌표축 설정

for(i in 1:nrow(dat)){
  lines(1:ncol(dat),dat[i,])
} # 자료 1개당 1개의 선 그리기
# 변수의 순서가 바뀌면 그래프 양상도 달라진다
# 대략 어떤 설명변수들에서 값에 따른 목표변수의 값들이 달라지는지 파악할 수 있다







##EDA 4 : PCA
pca_dt = prcomp(dat[,-5],
                center = T,
                scale. = T)

plot(pca_dt,type = "l")

summary(pca_dt)

# 2개 PC 사용으로 전체 data의 95.81% 의 분산을 설명


plot(pca_dt$x[,1:2], pch = 16, col = cols,
     xlab = paste("PC1 :", format((pca_dt$sdev[1])^2 / sum((pca_dt$sdev)^2) *100, digits = 4),"%"),
     ylab = paste("PC2 :", format((pca_dt$sdev[2])^2 / sum((pca_dt$sdev)^2) *100, digits = 3),"%"),
     main = paste("PCA plot :", 
                  format(sum((pca_dt$sdev[1:2])^2 / sum((pca_dt$sdev)^2)) *100,digits = 4),"%"))

pcs = pca_dt$rotation[,1:2]
pcs[order(abs(pcs[,1]), decreasing = T),]




for(i in 1:4){
  xx = pca_dt$x[,i]
  yy = dat[,4]
  plot(xx ,yy, col = cols,
       main = paste("PC ",i, " vs. ", names(dat)[ncol(dat)],sep=""),
       xlab = paste("PC",i), ylab = names(dat)[ncol(dat)])
  
  lm1 = lm(yy ~ xx)
  pred = predict(lm1, data.frame(xx = seq(min(xx),max(xx),length.out = 100)),interval = "confidence") 
  abline(lm1$coefficients, col = 2, lwd = 2)
  lines(seq(min(xx),max(xx),length.out = 100), pred[,2], lty = 2, col = 2)
  lines(seq(min(xx),max(xx),length.out = 100), pred[,3], lty = 2, col = 2)
  legend("bottomright", legend = paste("corr : ", format(cor(xx,yy), digits = 3))) # 선형상관계수 표기
  readline("press enter")
}

pairs(cbind(pca_dt$x[,1:4], dat$Species))




##EDA 4 : 
##t-SNE
install.packages("Rtsne")
library(Rtsne)
set.seed(1)
tsne.dat = Rtsne::Rtsne(dat[,-ncol(dat)], perplexity = 30, verbose = TRUE, max_iter = 500, check_duplicates = FALSE)


plot(tsne.dat$Y, pch = 16, col = dat$Species,
     xlab = "t-sne 1", ylab = "t-sne 2", main = "t-sne plot")
#같은 종끼리 비슷한 데이터 특성을 가지고있다고 할 수 있다 (뭉쳐있다)


##2) modelling 수행
##1)에서 정한 변수 2개를 사용
library(class)
library(e1071)

table(dat$Species) # 데이터 비율



set.seed(1)
tr.id = sample(x = nrow(dat), size = round(nrow(dat) * 0.8)) 
# training data의 번호를 뽑는다 : 약 80%정도의 데이터를 train, 나머지를 test로 사용
dat.tr = dat[tr.id,]
dat.te = dat[-tr.id,]

table(dat.tr) 
table(dat.te$Species)

dat.tr.x = dat.tr[,-ncol(dat)]
dat.te.x = dat.te[,-ncol(dat)]

dat.tr.y = dat.tr[,ncol(dat)]
dat.te.y = dat.te[,ncol(dat)]

### KNN
knn.result = class::knn(train = dat.tr.x[,-2],
                        test = dat.te.x[,-2],
                        cl = dat.tr.y,
                        k = 3,
                        prob = T) # 가장 가까운 3개의 label을 보고 다수결로 label 결정

### svm
svm.model = e1071::svm(Species ~ . , data = dat.tr) # support vector machine modelling
svm.result = predict(svm.model, newdata = dat.te.x) # SVM 모델에 의한 예측


##validation

(tt.knn = table(knn.result,dat.te.y))
(acc.knn = (tt.knn[1,1]+tt.knn[2,2])/sum(tt.knn)) # 정확도 : 실제값을 맞춘 비율
(prec.knn = tt.knn[2,2]/sum(tt.knn[2,])) # 정밀도 precision: 목표 label(패혈증)에 대하여 예측한 것 중 맞춘 비율
(rec.knn = tt.knn[2,2]/sum(tt.knn[,2])) # 재현율/민감도 recall/sensitivity : 목표 label(패혈증)에 대하여 예측 중 맞춘 비율
(f1.knn = 2 * (prec.knn*rec.knn) / (prec.knn+rec.knn)) # F1 score : 정밀도와 재현율의 조화평균


(tt.svm = table(svm.result,dat.te.y))
(acc.svm = (tt.svm[1,1]+tt.svm[2,2])/sum(tt.svm)) # 정확도 : 실제값을 맞춘 비율
(prec.svm = tt.svm[2,2]/sum(tt.svm[2,])) # 정밀도 precision: 목표 label(패혈증)에 대하여 실제 중 맞춘 비율
(rec.svm = tt.svm[2,2]/sum(tt.svm[,2])) # 재현율/민감도 recall/sensitivity : 목표 label(패혈증)에 대하여 예측 중 맞춘 비율
(f1.svm = 2 * (prec.svm*rec.svm) / (prec.svm+rec.svm)) # F1 score : 정밀도와 재현율의 조화평균

plot(tt.knn)
plot(tt.svm)

matrix(c(acc.knn, prec.knn, rec.knn, f1.knn, acc.svm, prec.svm, rec.svm, f1.svm),
       ncol = 2, dimnames = list(c("acc","prec","rec","f1"),c("KNN","SVM")))
# KNN과 SVM의 성능이비슷하여 어떤 모델이 더 좋다 판단할 수 없다.
# 그래프 상으로도 두 모델에서의 우열을 가리기 힘들다.

##result visualizing

# 결과 표시를 위한 pch setting
correct.knn = (dat.te.y == knn.result)
correct.svm = (dat.te.y == svm.result)

pchs.knn = pchs.svm = rep(1, nrow(dat)) # 1 대신 NA를 넣으면 train data는 출력하지 않는다
# pchs.knn = pchs.svm = rep(NA, nrow(s.dat))

pchs.knn[-tr.id] = ifelse(correct.knn, 16, 4)
pchs.svm[-tr.id] = ifelse(correct.svm, 16, 4)
# 맞춘 점들은 동그라미 , 틀린 점들은 X

### pca plot : 이전에 계산해둔 좌표를 사용한다

par(mfrow=c(1,2))
plot(pca_dt$x[,1:2], pch = pchs.knn, col = dat$Species,
     xlab = paste("PC1 :", format((pca_dt$sdev[1])^2 / sum((pca_dt$sdev)^2) *100, digits = 4),"%"),
     ylab = paste("PC2 :", format((pca_dt$sdev[2])^2 / sum((pca_dt$sdev)^2) *100, digits = 3),"%"),
     main = paste("PCA plot : KNN : ACC = ", format(acc.knn*100, digits = 4)," %"))
legend("topleft", legend = c("train data (Control)","train data (Sepsis)", 
                             "correctly predicted (Control)","correctly predicted (Sepsis)",
                             "incorrectly predicted (Control)","incorrectly predicted (Sepsis)"),
       pch = c(1,1, 16,16, 4,4), col = c(1,2,1,2,1,2), cex = 0.7)

plot(pca_dt$x[,1:2], pch = pchs.svm, col = dat$Species,
     xlab = paste("PC1 :", format((pca_dt$sdev[1])^2 / sum((pca_dt$sdev)^2) *100, digits = 4),"%"),
     ylab = paste("PC2 :", format((pca_dt$sdev[2])^2 / sum((pca_dt$sdev)^2) *100, digits = 3),"%"),
     main = paste("PCA plot : SVM : ACC = ", format(acc.svm*100, digits = 4)," %"))
legend("topleft", legend = c("train data (Control)","train data (Sepsis)", 
                             "correctly predicted (Control)","correctly predicted (Sepsis)",
                             "incorrectly predicted (Control)","incorrectly predicted (Sepsis)"),
       pch = c(1,1, 16,16, 4,4), col = c(1,2,1,2,1,2), cex = .7)
par(par.def)


### t-SNE plot 

par(mfrow=c(1,2))
plot(tsne.dat$Y, pch = pchs.knn, col = dat$Species,
     xlab = "t-sne 1", ylab = "t-sne 2", main = "t-sne plot : KNN")
legend("topright", legend = c("train data (Control)","train data (Sepsis)", 
                              "correctly predicted (Control)","correctly predicted (Sepsis)",
                              "incorrectly predicted (Control)","incorrectly predicted (Sepsis)"),
       pch = c(1,1, 16,16, 4,4), col = c(1,2,1,2,1,2), cex = .7)
plot(tsne.dat$Y, pch = pchs.svm, col = dat$Species,
     xlab = "t-sne 1", ylab = "t-sne 2", main = "t-sne plot : SVM")
legend("topright", legend = c("train data (Control)","train data (Sepsis)", 
                              "correctly predicted (Control)","correctly predicted (Sepsis)",
                              "incorrectly predicted (Control)","incorrectly predicted (Sepsis)"),
       pch = c(1,1, 16,16, 4,4), col = c(1,2,1,2,1,2), cex = .7)







# KNN과 SVM에서 정확도가 76.67%로 같으며 두 모델 모두 높은 정확도를 보인다.
# 두 모델 모두 우수한 판별력을 보이며 시각화 상으로도 어느 한쪽이 더 우수하다고 보기 어렵다.

##모든 변수들을 사용


table(dat$Species) # 데이터 비율



set.seed(1)
tr.id = sample(x = nrow(dat), size = round(nrow(dat) * 0.8)) 
# training data의 번호를 뽑는다 : 약 80%정도의 데이터를 train, 나머지를 test로 사용
dat.tr = dat[tr.id,]
dat.te = dat[-tr.id,]

table(dat.tr) 
table(dat.te$Species)

dat.tr.x = dat.tr[,-ncol(dat)]
dat.te.x = dat.te[,-ncol(dat)]

dat.tr.y = dat.tr[,ncol(dat)]
dat.te.y = dat.te[,ncol(dat)]

### KNN
knn.result = class::knn(train = dat.tr.x[,-2],
                        test = dat.te.x[,-2],
                        cl = dat.tr.y,
                        k = 3,
                        prob = T) # 가장 가까운 5개의 label을 보고 다수결로 label 결정

### svm
svm.model = e1071::svm(Species ~ . , data = dat.tr) # support vector machine modelling
svm.result = predict(svm.model, newdata = dat.te.x) # SVM 모델에 의한 예측







##validation

(tt.knn = table(knn.result,dat.te.y))
(acc.knn = (tt.knn[1,1]+tt.knn[2,2])/sum(tt.knn)) # 정확도 : 실제값을 맞춘 비율
(prec.knn = tt.knn[2,2]/sum(tt.knn[2,])) # 정밀도 precision: 목표 label(패혈증)에 대하여 예측한 것 중 맞춘 비율
(rec.knn = tt.knn[2,2]/sum(tt.knn[,2])) # 재현율/민감도 recall/sensitivity : 목표 label(패혈증)에 대하여 예측 중 맞춘 비율
(f1.knn = 2 * (prec.knn*rec.knn) / (prec.knn+rec.knn)) # F1 score : 정밀도와 재현율의 조화평균


(tt.svm = table(svm.result,dat.te.y))
(acc.svm = (tt.svm[1,1]+tt.svm[2,2])/sum(tt.svm)) # 정확도 : 실제값을 맞춘 비율
(prec.svm = tt.svm[2,2]/sum(tt.svm[2,])) # 정밀도 precision: 목표 label(패혈증)에 대하여 실제 중 맞춘 비율
(rec.svm = tt.svm[2,2]/sum(tt.svm[,2])) # 재현율/민감도 recall/sensitivity : 목표 label(패혈증)에 대하여 예측 중 맞춘 비율
(f1.svm = 2 * (prec.svm*rec.svm) / (prec.svm+rec.svm)) # F1 score : 정밀도와 재현율의 조화평균

plot(tt.knn)
plot(tt.svm)

matrix(c(acc.knn, prec.knn, rec.knn, f1.knn, acc.svm, prec.svm, rec.svm, f1.svm),
       ncol = 2, dimnames = list(c("acc","prec","rec","f1"),c("KNN","SVM")))
# KNN과 SVM의 성능이비슷하여 어떤 모델이 더 좋다 판단할 수 없다.
# 그래프 상으로도 두 모델에서의 우열을 가리기 힘들다.

##result visualizing

# 결과 표시를 위한 pch setting
correct.knn = (dat.te.y == knn.result)
correct.svm = (dat.te.y == svm.result)

pchs.knn = pchs.svm = rep(1, nrow(dat)) # 1 대신 NA를 넣으면 train data는 출력하지 않는다
# pchs.knn = pchs.svm = rep(NA, nrow(s.dat))

pchs.knn[-tr.id] = ifelse(correct.knn, 16, 4)
pchs.svm[-tr.id] = ifelse(correct.svm, 16, 4)
# 맞춘 점들은 동그라미 , 틀린 점들은 X

### pca plot : 이전에 계산해둔 좌표를 사용한다

par(mfrow=c(1,2))
plot(pca_dt$x[,1:4], pch = pchs.knn, col = dat$Species,
     xlab = paste("PC1 :", format((pca_dt$sdev[1])^2 / sum((pca_dt$sdev)^2) *100, digits = 4),"%"),
     ylab = paste("PC2 :", format((pca_dt$sdev[2])^2 / sum((pca_dt$sdev)^2) *100, digits = 3),"%"),
     main = paste("PCA plot : KNN : ACC = ", format(acc.knn*100, digits = 4)," %"))
legend("topleft", legend = c("train data (Control)","train data (Sepsis)", 
                             "correctly predicted (Control)","correctly predicted (Sepsis)",
                             "incorrectly predicted (Control)","incorrectly predicted (Sepsis)"),
       pch = c(1,1, 16,16, 4,4), col = c(1,2,1,2,1,2), cex = 0.7)

plot(pca_dt$x[,1:4], pch = pchs.svm, col = dat$Species,
     xlab = paste("PC1 :", format((pca_dt$sdev[1])^2 / sum((pca_dt$sdev)^2) *100, digits = 4),"%"),
     ylab = paste("PC2 :", format((pca_dt$sdev[2])^2 / sum((pca_dt$sdev)^2) *100, digits = 3),"%"),
     main = paste("PCA plot : SVM : ACC = ", format(acc.svm*100, digits = 4)," %"))
legend("topleft", legend = c("train data (Control)","train data (Sepsis)", 
                             "correctly predicted (Control)","correctly predicted (Sepsis)",
                             "incorrectly predicted (Control)","incorrectly predicted (Sepsis)"),
       pch = c(1,1, 16,16, 4,4), col = c(1,2,1,2,1,2), cex = .7)



### t-SNE plot 

par(mfrow=c(1,2))
plot(tsne.dat$Y, pch = pchs.knn, col = dat$Species,
     xlab = "t-sne 1", ylab = "t-sne 2", main = "t-sne plot : KNN")
legend("topright", legend = c("train data (Control)","train data (Sepsis)", 
                              "correctly predicted (Control)","correctly predicted (Sepsis)",
                              "incorrectly predicted (Control)","incorrectly predicted (Sepsis)"),
       pch = c(1,1, 16,16, 4,4), col = c(1,2,1,2,1,2), cex = .7)
plot(tsne.dat$Y, pch = pchs.svm, col = dat$Species,
     xlab = "t-sne 1", ylab = "t-sne 2", main = "t-sne plot : SVM")
legend("topright", legend = c("train data (Control)","train data (Sepsis)", 
                              "correctly predicted (Control)","correctly predicted (Sepsis)",
                              "incorrectly predicted (Control)","incorrectly predicted (Sepsis)"),
       pch = c(1,1, 16,16, 4,4), col = c(1,2,1,2,1,2), cex = .7)

##두 모델 모두 76.67%의 정확도를 보였으며 그래프 상으로도 우열을 가리기 힘들다.



##각 경우에 대해 Accuracy를 기준으로 최적의 k를 결정하고 그 근거를 제시하시오.
#k값 선정 과정
df = data.frame(c(1,2,3,4,5,6,7),c(0.766, 0.766, 0.766, 0.766, 0.766, 0.766, 0.766))
df2 = data.frame(c(1, 2, 3, 4, 5, 6, 7), c(0.923,0.923,0.96, 0.96, 0.96, 0.96, 0.96))
par(mfrow=c(1,2))
plot(df, xlab = "k수", ylab = "acc")
plot(df2, xlab = "k수", ylab = "f1")

#모든 변수를 적용하였을 때와 1)에서 정한 2개의 변수를 사용하였을 때 모두
#k값을 변경하였을 때, knn모델의 acc는 달라지지 않았다 그러나 f1 경우 k가 1, 2일때 작고, 3이상일 때부터 커져
#k값은 3이상이어야 한다. 우리는 여기서 아이리스 모델의 군집이 3개임을 알고,  k값, 즉 군집수가 작을 수록 데이터 분석이
#용이 하므로 k 값은 3으로 한다.





##3) 1, 2의 모델링 결과를 평가지표와 시각화를 기반으로 최종 결론을 내리시오


##1,2 번을 거치며 데이터 분석을 해 보았으나 변수의 갯수와 knn,svm상관없이 acc가 76.67%로 동일하게 나왔다.
##아이리스 데이터릐 경우 군집화가 쉽고 데이터의 크기가 크지 않기 때문에 데이터 분석방법에 따라 정확도의 차이는 보이지 않는듯 보인다.
##따라서 아이리스 데이터의 경우 변수의 갯수, 분석방법에 구애받지 않고 우수한 군집분석을 할 수 있다.



###2번

##데이터 불러오기

install.packages("data.table")



dat = data.table::fread("C:\\Users\\theil\\Desktop\\a\\Concrete_Data.csv", encoding = 'UTF-8')


dat = data.frame(dat)

head(dat)
dim(dat)

##1) 설명변수들에 대해 기초분석을 수행하시오.

### 1. EDA 1 : 목표변수와 설명변수간의 관계 확인
### scatter plot
### 빨간 실선과 점선은 x축과 y축의 선형상관성을 의미함


cols = topo.colors(nrow(dat))[rank(dat$Concrete.compressive.strength, ties.method = "random")]
cols = rev(rainbow(nrow(dat), start = 0, end = 5/6))[rank(dat$Concrete.compressive.strength, ties.method = "random")]
# 목표변수 값에 따른 coloring



for(i in 1:(ncol(dat)-1)){
  
  xx = dat[,i]
  yy = dat[,ncol(dat)]
  plot(xx ,yy, main = paste(names(dat)[i], " vs. ", names(dat)[ncol(dat)],sep=""),
       xlab = names(dat)[i], ylab = names(dat)[ncol(dat)], col = cols)
  
  lm1 = lm(yy ~ xx)
  pred = predict(lm1, data.frame(xx = seq(min(xx),max(xx),length.out = 100)),interval = "confidence") 
  abline(lm1$coefficients, col = 2)
  lines(seq(min(xx),max(xx),length.out = 100), pred[,2], lty = 2, col = 2)
  lines(seq(min(xx),max(xx),length.out = 100), pred[,3], lty = 2, col = 2)
  legend("bottomright", legend = paste("corr : ", format(cor(xx,yy), digits = 3))) # 선형상관계수 표기
  readline("press enter")
}



### 1. EDA 2 : 변수간 상관관계 파악
### pairs plot
pairs(dat, col = cols)







### 1. EDA 3 : 
### parallel coordinate plot
sc.dat = scale(dat) # 모든 변수들의 scale이 다르므로 변환

plot(0,0,xlim = c(1,ncol(dat)), ylim = range(sc.dat), type = "n", xaxt = "n", xlab = "", ylab = "", main = "parallel coordinate plot")
# 그래프 틀 생성
axis(1, at = 1:ncol(dat), labels = names(dat), las = 2) # 좌표축 설정

for(i in 1:nrow(dat)){
  lines(1:ncol(dat),sc.dat[i,], col = cols[i])
} # 자료 1개당 1개의 선 그리기
# 변수의 순서가 바뀌면 그래프 양상도 달라진다
# 대략 어떤 설명변수들에서 값에 따른 목표변수의 값들이 달라지는지 파악할 수 있다







### 1. EDA 4 : 
### PCA
pca.dat = prcomp(dat[,-ncol(dat)])
summary(pca.dat) # 2개 PC 사용으로 전체 data의 96.7% 의 분산을 설명


plot(pca.dat$x[,1:2], pch = 16, col = cols,
     xlab = paste("PC1 :", format((pca.dat$sdev[1])^2 / sum((pca.dat$sdev)^2) *100, digits = 4),"%"),
     ylab = paste("PC2 :", format((pca.dat$sdev[2])^2 / sum((pca.dat$sdev)^2) *100, digits = 3),"%"),
     main = paste("PCA plot :", 
                  format(sum((pca.dat$sdev[1:2])^2 / sum((pca.dat$sdev)^2)) *100,digits = 4),"%"))

pcs = pca.dat$rotation[,1:2]
pcs[order(abs(pcs[,1]), decreasing = T),]




for(i in 1:4){
  xx = pca.dat$x[,i]
  yy = dat[,ncol(dat)]
  plot(xx ,yy, col = cols,
       main = paste("PC ",i, " vs. ", names(dat)[ncol(dat)],sep=""),
       xlab = paste("PC",i), ylab = names(dat)[ncol(dat)])
  
  lm1 = lm(yy ~ xx)
  pred = predict(lm1, data.frame(xx = seq(min(xx),max(xx),length.out = 100)),interval = "confidence") 
  abline(lm1$coefficients, col = 2, lwd = 2)
  lines(seq(min(xx),max(xx),length.out = 100), pred[,2], lty = 2, col = 2)
  lines(seq(min(xx),max(xx),length.out = 100), pred[,3], lty = 2, col = 2)
  legend("bottomright", legend = paste("corr : ", format(cor(xx,yy), digits = 3))) # 선형상관계수 표기
  readline("press enter")
}

pairs(cbind(pca.dat$x[,1:3], dat$Concrete.compressive.strength), col = cols)





### 1. EDA 4 : 
### t-SNE
set.seed(1)
tsne.dat = Rtsne::Rtsne(dat[,-ncol(dat)], perplexity = 40, verbose = TRUE, max_iter = 500, check_duplicates = FALSE)


plot(tsne.dat$Y, pch = 16, col = cols,
     xlab = "t-sne 1", ylab = "t-sne 2", main = "t-sne plot")



#Corr of Age : 0.329, Corr of Superplaticizer : 0.366, Corr of Cement : 0.498
#Age, Superplaticizer, Cement가 콘크리트 압축강도에 가장 영향을 크게 준다.








##2) 모델링 수행

### 2. modelling 단계

### 2.1. full model

### Linear Regression
lm.model.all = lm(Concrete.compressive.strength ~ . , data = dat)
# lm 모델은 data.frame 형태의 data를 받고, 그 데이터 내의 변수명을 formula에 사용한다
# formula 의 물결 ~ 뒤에 마침표 . 은 나머지 모든 변수들을 설명변수로 사용한다는 선언이다


### SVR
svr.model.all = e1071::svm(Concrete.compressive.strength ~ . , data = dat) 
# support vector machine modelling
# SVM classification 모델과 같은 형식을 사용한다
# 목표변수의 factor / numeric 여부에 따라 자동으로 classification / regression 모드가 변경된다





### 2.2. train-data model (for prediction)
set.seed(1)
tr.id = sample(x = nrow(dat), size = round(nrow(dat) * 0.8)) 
dat.tr = dat[tr.id,]
dat.te = dat[-tr.id,]
# training data의 번호를 뽑는다 : 약 80%정도의 데이터를 train, 나머지를 test로 사용
# MSPE를 계산할 목적으로 뽑는다


### Linear Regression
lm.model.tr = lm(Concrete.compressive.strength ~ . , data = dat.tr)


### SVR
svr.model.tr = e1071::svm(Concrete.compressive.strength ~ . , data = dat.tr) 









### 3. validation

### 3.1. 선형회귀분석의 모델 결과 확인
lm.summ = summary(lm.model.all)
# 선형회귀분석에 대한 통계적 추정 결과들이 summary 함수로 정리된다

lm.summ$r.squared
lm.summ$adj.r.squared
# 독립변수가 2개 이상인 경우 adj.R^2를 사용한다
# 약 0.613 : 모델이 데이터의 61.3% 를 설명한다

lm.summ$sigma^2
# 선형 회귀분석의 MSE 값
# MSE = 108.1422


pred.lm.te = predict(lm.model.tr, dat.te)
# train-data로 학습된 선형 회귀모델로부터 test-data의 종속변수 예측

err.lm.te = dat.te[,ncol(dat)] - pred.lm.te
# test-data의 종속변수 실제값과 예측값의 차 : 예측오차

sum(err.lm.te^2) / nrow(dat.te)
# MSPE = 117.7612




### 3.2. SVR의 모델 결과 확인
svr.summ = summary(svr.model.all)
# SVR에 대한 모델링 결과가 summary 함수로 정리된다

# svr.summ$residuals
# SVR 모델의 residual을 뽑아낼 수 있다

sum(svr.summ$residuals^2) / nrow(dat)
# MSE 는 residual의 제곱합 / dat 갯수 이다
# MSE = 31.66735
# 선형회귀분석의 MSE보다 작다

pred.svr.te = predict(svr.model.tr, dat.te)
# train-data로 학습된 선형 회귀모델로부터 test-data의 종속변수 예측

err.svr.te = dat.te[,ncol(dat)] - pred.svr.te
# test-data의 종속변수 실제값과 예측값의 차 : 예측오차

sum(err.svr.te^2) / nrow(dat.te)
# MSPE = 46.12112
# 선형 회귀분석에 비해 MSE MSPE의 차이가 비교적 크다
# 그럼에도 선형 회귀분석 모델보다 MSPE가 작다



plot(df)




### 4. result visualizing

# 4.1 선형회귀분석의 모델 결과의 시각화

plot(lm.model.all)
# R의 내장함수 lm으로 만들어지는 선형 회귀분석 모델 객체는 
# 자체적으로 모델 결과의 시각화 그림을 보여준다
# 1. residual vs. fitted value : 잔차와 예측값의 관계를 보여준다
#                                이 그림에서 추세(빨간선)가 특정 패턴을 보이거나 (가정1)
#                                점이 중심으로부터 퍼진정도가 달라지면 (가정2)
#                                선형 회귀분석의 기본가정이 위배되었음을 의심할 수 있다
# 2. normal Q-Q plot           : 잔차의 분포가 정규분포를 얼마나 잘 따르는지를 나타내는 그림
#                                점들이 대각선(점선) 위에 잘 모여있을 수록 
#                                잔차가 정규분포를 따른다고 할 수 있다
# 3. 잔차 Scale-Location plot  : 표준화된 잔차와 예측값의 관계를 보여준다
#                                빨간 선이 중앙을 일정하게 지난다면 가정2(잔차의 동분산성)이 잘 맞는것이다
#                                 1의 그림보다 가정2에 대한 판단을 더 쉽게 할 수 있다
# 4. residual vs. Leverage     : 모델 학습에 크게 영향을 주는 관측값을 찾는 용도로 사용된다
#                                 (선형 회귀분석 이론 참고)

hist(lm.summ$residuals)
# 잔차의 히스토그램을 확인해보면 거의 정규분포를 잘 만족하는 것을 볼 수 있다


plot(dat$Concrete.compressive.strength, predict(lm.model.all), 
     xlim = range(dat$Concrete.compressive.strength), ylim = range(dat$Concrete.compressive.strength),
     main = "True vs. Pred. : lm full model", xlab = "True value", ylab = "Pred. value")
abline(a=0,b=1, lty=2, col = 2)
legend("bottomright",legend = "MSE = 108.1422")
# true vs. predict plot : 실제값과 모델에 의한 예측값을 비교한다
#                         대각선에 점들이 더 모일수록 더 정확한 예측을 하고있다 할 수 있다

plot(dat.te$Concrete.compressive.strength, pred.lm.te,
     xlim = range(dat$Concrete.compressive.strength), ylim = range(dat$Concrete.compressive.strength),
     main = "True vs. Pred. : lm prediction error", xlab = "True value", ylab = "Pred. value")
abline(a=0,b=1, lty=2, col = 2)
legend("bottomright",legend = "MSPE = 117.7612")
# true vs. predict plot : 실제값과 모델에 의한 예측값을 비교한다
# 대각선에 점들이 더 모일수록 더 정확한 예측을 하고있다 할 수 있다



# 4.2 SVR 모델 결과의 시각화

plot(svr.summ$fitted, svr.summ$residuals, main = "Residuals vs Fitted",
     xlab = "Fitted values", ylab = "Resoduals")
abline(h = 0 , lty = 2)
# residual vs fitted value



plot(dat$Concrete.compressive.strength, predict(svr.model.all), 
     xlim = range(dat$Concrete.compressive.strength), ylim = range(dat$Concrete.compressive.strength),
     main = "True vs. Pred. : svr full model", xlab = "True value", ylab = "Pred. value")
abline(a=0,b=1, lty=2, col = 2)
legend("bottomright",legend = "MSE = 31.66735")
# true vs. predict plot : 실제값과 모델에 의한 예측값을 비교한다
#                         대각선에 점들이 더 모일수록 더 정확한 예측을 하고있다 할 수 있다

plot(dat.te$Concrete.compressive.strength, pred.svr.te,
     xlim = range(dat$Concrete.compressive.strength), ylim = range(dat$Concrete.compressive.strength),
     main = "True vs. Pred. : svr prediction error", xlab = "True value", ylab = "Pred. value")
abline(a=0,b=1, lty=2, col = 2)
legend("bottomright",legend = "MSPE = 46.12112")
# true vs. predict plot : 실제값과 모델에 의한 예측값을 비교한다
#                         대각선에 점들이 더 모일수록 더 정확한 예측을 하고있다 할 수 있다



par(mfrow = c(2,2))

plot(dat$Concrete.compressive.strength, predict(lm.model.all), 
     xlim = range(dat$Concrete.compressive.strength), ylim = range(dat$Concrete.compressive.strength),
     main = "True vs. Pred. : lm full model", xlab = "True value", ylab = "Pred. value")
abline(a=0,b=1, lty=2, col = 2)
legend("bottomright",legend = "MSE = 108.1422")

plot(dat.te$Concrete.compressive.strength, pred.lm.te,
     xlim = range(dat$Concrete.compressive.strength), ylim = range(dat$Concrete.compressive.strength),
     main = "True vs. Pred. : lm prediction error", xlab = "True value", ylab = "Pred. value")
abline(a=0,b=1, lty=2, col = 2)
legend("bottomright",legend = "MSPE = 117.7612")

plot(dat$Concrete.compressive.strength, predict(svr.model.all), 
     xlim = range(dat$Concrete.compressive.strength), ylim = range(dat$Concrete.compressive.strength),
     main = "True vs. Pred. : svr full model", xlab = "True value", ylab = "Pred. value")
abline(a=0,b=1, lty=2, col = 2)
legend("bottomright",legend = "MSE = 31.66735")

plot(dat.te$Concrete.compressive.strength, pred.svr.te,
     xlim = range(dat$Concrete.compressive.strength), ylim = range(dat$Concrete.compressive.strength),
     main = "True vs. Pred. : svr prediction error", xlab = "True value", ylab = "Pred. value")
abline(a=0,b=1, lty=2, col = 2)
legend("bottomright",legend = "MSPE = 46.12112")






# 이 상황에서는 콘크리트 압축강도 예측을 위해 SVR 모델을 사용하는 것이 더 좋다고 결론


##3) 모델링 결과를 평가 지표와 시각화를 기반으로 선형 회귀분석 모델과 SVR 모델 중 이 데이터를 예측하는데 어느
##모델이 더 좋을지 최종 결론을 내리시오.


