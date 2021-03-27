library(caret)
#library(Information) 

lfp <- read.csv('http://inta.gatech.s3.amazonaws.com/mroz_train.csv')
# exclude variable which is not from data 
Data1 <- data.frame(lfp[,-c(1)])

# missing value handling
a=NULL
b=NULL
c=NULL

for(i in 1:ncol(Data1)){
  a[i]=sum(is.na(Data1[,i]))
  b[i]=((a[i]/nrow(Data1))*100)
  c[i]=class(Data1[,i])
}

df=data.frame(names(Data1),a,b,c)
get_mode=function(x){
  u=unique(x)
  u[which.max(tabulate(match(x,u)))]
}

for(i in 1:ncol(Data1)){
  if(class(Data1[,i])=="factor"){
    Data1[is.na(Data1[,i]),i]=get_mode(na.omit(Data1[,i]))
  }else{
    Data1[is.na(Data1[,i]),i]=mean(Data1[,i],na.rm = TRUE)
  }
  
}
sum(is.na(Data1)) #Zero missing value


# Run simple GLM and after that examing in anova test
drop <- "inlf"

glm_model <- glm(inlf~.,data = Data1,family = binomial())
anova_test <- data.frame(anova(glm_model,test = "Chisq"))

x<- data.frame(anova_test)
x1 <- data.frame(x[,5])

# explude variable whose p value >0.05 and keep those variable whose p value <= 0.05 and run it again
name_1 <- colnames(Data1[which((x1<=0.05)==TRUE)])
data1_final <- data.frame(Data1[,c(name_1,"inlf")])

glm_model_final <-  glm(inlf~.,data = data1_final,family = binomial())

predict_f <- predict(glm_model_final,Data1,type = 'response')

#confusion matrix 
table_conf <- table(Data1$inlf,predict_f>0.5)

# original VS predicted 
a <- data.frame(original_value =Data1$inlf
                ,pridicted_value = as.numeric(predict_f>0.5,1,0) )