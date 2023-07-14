#### import data

dev <- read_excel("data_source") # training cohort
vad <-  read_excel("data_source") # validation cohort1


#### Univariable-logistic regression
uni_glm_model<-function(x){
  FML<-as.formula(paste0("MTM==1~",x))  
  glm1<-glm(FML,data = dev,family = binomial)
  glm2<-summary(glm1)
  
  
  OR<-round(exp(coef(glm1)),2)
  SE<-round(glm2$coefficients[,2],3)  
  CI2.5<-round(exp(coef(glm1)-1.96*SE),2)
  CI97.5<-round(exp(coef(glm1)+1.96*SE),2)
  CI<-paste0(CI2.5,'-',CI97.5)
  B<-round(glm2$coefficients[,1],3)
  Z<-round(glm2$coefficients[,3],3)
  P<-round(glm2$coefficients[,4],3)
  
  
  uni_glm_model<-data.frame('characteristics'=x,
                            'B'=B,
                            'SE'=SE,
                            'OR'=OR,
                            'CI'=CI,
                            'Z' =Z,
                            'P'=P)[-1,]
  
  return(uni_glm_model)
}


variable.names<-colnames(dev)[c(1:n)] 
variable.names


uni_glm<-lapply(variable.names,uni_glm_model)
uni_glm

#### select the variable P < 0.05
fml<- as.formula(paste0('MTM==1~',paste0(uni_glm$characteristics[uni_glm$P<0.05],collapse = '+'))) 
fml


model7<-glm(fml,data = dev,family = binomial(logit))   

dev$predmodel7<- predict(newdata=dev,model7,"response")

vad$predmodel7<- predict(newdata=vad,model7,"response")

library(reportROC)

#### achieve the AUC SEN SPE NPV PPV

reportROC(gold = dev$MTM,
          predictor = dev$predmodel7,
          important = "se",
          plot = TRUE)


reportROC(gold = vad$MTM,
          predictor = vad$predmodel7,
          important = "se",
          plot = TRUE)
dev$predMTM <- ifelse(dev$predmodel7 > 0.258,1,0)
vad$predMTM <- ifelse(vad$predmodel7 > 0.258,1,0)


#### Risk distribution of predicted value of MTM-HCC in validation cohort 1.

sort_index <- order(vad$predmodel7)

color_vector <- ifelse(vad$proliferative[sort_index] == 1, "#f89a3b", "#3393a9")
plot(vad$predmodel7[sort_index], type = "p", pch = 19, 
     col = color_vector, xlab = "Patients (increasing risk score)", 
     ylab = "Predicted probability", cex = 2)

abline(h = 0.258, lty = 2)
legend("topleft", c("Pred MTM-HCC", "Pred Non MTM-HCC"), bty = "n", 
       pch = 19, col = c("#f89a3b", "#3393a9"), cex = 1.5)
title("Predicted MTM-HCC in Validation Cohort 1")
