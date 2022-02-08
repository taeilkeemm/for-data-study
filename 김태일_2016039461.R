#참/거짓 1번 문제 답 : F
#참/거짓 2번 문제 답 : F
#참/거짓 3번 문제 답 : F
#참/거짓 4번 문제 답 : F
#참/거짓 5번 문제 답 : T
#참/거짓 6번 문제 답 : F
#참/거짓 7번 문제 답 : T
#참/거짓 8번 문제 답 : F
###############################################
#문제1

#1)
##작업공간 지정
getwd()
setwd("C:/Users/theil/Desktop/a")
##데이터불러오기
test1 = read.csv("test1.csv", header=T, sep="", skip=0)
#2)
color = gl(Color, levels=c("빨", "주", "노", "초","파","남","보"))
#3)
men = (colMeans(test1[test1[,"Gender"]=="Male", "Age"])/(test1.count(test1[,"Gender"]=="Male")))#남자나이평균
women= (colMeans(test1[test1[,"Gender"]=="Female", "Age"])/(test1.count(test1[,"Gender"]=="Female")))#여자나이평균
total = (colMeans(test1[, "Age"])/(test1.count(test1[,"Gender"])))#전체나이평균
mendiff = (men-total)#남자나이평균-전체나이평균
womendiff = (women-diff)#여자나이평균-전체나이펴균
#4)

#5)
(test1[,"Height"]>0.5) & (test1[ , "Weight"]<0.5)
#6)
install.packages("rgl")##패키지설치
rgl::plot3d(test1[ ,["Age", "Height","Weight"])##그래프 그리기
##################################################
#문제2
lotto = function(n = 5){
  draw = sample(1:45 , 6) #어떤 숫자를 뽑을까
  if(n ==1){ ##반복문생성
    draw = sample(1:45 , 6)##n=1일시 벡터로 출력
  }
  else{
    set.seed(19970210)##난수고정
    draw = matrix(1:(5*n), ncol=5)##행렬생성
    for(i in (1:n)){ 
      set.seed(19970210)##난수고정
      draw11 = sample(1:45 , 6) #카드뽑기
      drawn12 = sort(draw11, decreasing=F)##오름차순정렬
      draw[i,] = draw12#행렬에 추가
      i = i+1
    }
    print(draw)##결과출력
  }
}
lotto() ##함수실행
#####################################################
#문제3
##트럼프카드구성
cards = data.frame(shape = rep(c( "Spade","Diamond","Heart","Club "), times = 4, each = 13, len =52),
  number = factor(rep(c("A",2:10,"J","Q","K"), times = 4), levels = c("A",2:10,"J","Q","K")))

#카드뽑기
i = 1
repeat{##뽑기  시행 반복
    ##set.seed(19970210)  ##난수고정
    draw = sample(1:52 , 1, replace=T)  #카드뽑기
    drawn = cards[i,] #뽑은카드 리스트
    drawn
    if(drawn[i,2] == drawn[(i-1),2]){ ##앞카드숫자와 이번 카드 숫자 같은 시 멈춤
      print(drawn) ##리스트 출력
      print("count = ", i) ##몇번째 카드뽑기인지 출력
      break ##정지

    }
    i = 1+1 
         
}

