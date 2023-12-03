library(MASS)   #R 기본적으로 설치되어있는 MASS 패키지에 포함된 보스턴하우징데이터 불러오기    
i=which(Boston$medv==50)  # medv가 50인 자료 
Boston$y<-ifelse(Boston$medv>21,"H","L") # medv변수에 해당되는 값이 21보다 큰경우 H 작거나 같은경우L로 새 목표변수 y에 입력
boston=Boston[-i,-14]# medv가 50인 자료와 medv변수를 제외한 데이터프레임을 생성
head(boston) #보스턴자료 변환 확인 
boston$y=factor(boston$y) #H와L이 범주형변수로 인식될 수 있게 변환함 

fit.all=glm(y~.,family=binomial,boston) #boston 데이터를 이용해 로지스틱회귀 모형을 적합함
fit.step=step(fit.all,direction = "both") #단계적 선택법으로 최종모형을 선택 
fit.step$anova # 어떤 변수가 단계적선택법 과정 중에 제거 되었는지 확인 
summary(fit.step) #최종 로지스틱 회귀 모형 적합 결과 

boston$chas=factor(boston$chas)
boston$rad=factor(boston$rad)
boston$crim=factor(boston$crim)
boston$zn=factort(boston$zn)
boston$indus

boston$nox
boston$rm
boston$age
boston$dis

boston$tax
boston$ptratio
boston$black
boston$lstat
boston$y



fit.all=glm(y~.,family=binomial,boston)
fit.step=step(fit.all,direction = "both")
fit.step$anova
summary(fit.step)
p=predict(fit.step, newdata = boston, type="response")
threshold=0.5
yhat=ifelse(p>threshold,"H","L")
class.tab= table(boston$y, yhat, dnn=c("Actual","Predicted"))
print(class.tab)
sum(boston$y==yhat)/length(boston$y)
sum(boston$y!=yhat)/length(boston$y)
class.tab[1,1]/apply(class.tab,1,sum)[1]
class.tab[2,2]/apply(class.tab,1,sum)[2]
install.packages('ROCR')
library(ROCR)
pred<-prediction(p,boston$y)
perf<-performance(pred,"tpr","fpr")
plot(perf, lty1,col=2,xlim=c(0,1), ylim=c(0,1),xlab="1-Specificity", ylab="Sensitivity", main="ROC Curve")
plot(perf, lty=1,col=2,xlim=c(0,1), ylim=c(0,1),xlab="1-Specificity", ylab="Sensitivity", main="ROC Curve")
lines(x=c(0,1), y=c(0,1),col="grey")
performance(pred,"auc")@y.values
