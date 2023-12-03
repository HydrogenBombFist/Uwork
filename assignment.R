library(MASS)   #R �⺻������ ��ġ�Ǿ��ִ� MASS ��Ű���� ���Ե� �������Ͽ�¡������ �ҷ�����    
i=which(Boston$medv==50)  # medv�� 50�� �ڷ� 
Boston$y<-ifelse(Boston$medv>21,"H","L") # medv������ �ش�Ǵ� ���� 21���� ū��� H �۰ų� �������L�� �� ��ǥ���� y�� �Է�
boston=Boston[-i,-14]# medv�� 50�� �ڷ�� medv������ ������ �������������� ����
head(boston) #�������ڷ� ��ȯ Ȯ�� 
boston$y=factor(boston$y) #H��L�� ������������ �νĵ� �� �ְ� ��ȯ�� 

fit.all=glm(y~.,family=binomial,boston) #boston �����͸� �̿��� ������ƽȸ�� ������ ������
fit.step=step(fit.all,direction = "both") #�ܰ��� ���ù����� ���������� ���� 
fit.step$anova # � ������ �ܰ������ù� ���� �߿� ���� �Ǿ����� Ȯ�� 
summary(fit.step) #���� ������ƽ ȸ�� ���� ���� ��� 

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