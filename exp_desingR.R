library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(ggExtra)
library(hrbrthemes)
library(faraway)
library(MASS)
library(lme4)
#Bazı öğrenciler tarafından yazılmış bir test hakkında bilgi içerir.
#Okul ortamı, Okul türü, cinsiyet, diğerlerinin yanı sıra ön test puanları gibi özellikleri içerir.
data = read.csv("C:/Users/tolga/Desktop/test_scores.csv") #verinin çekilmesi
attach(data)
newdata = data%>%select(school,school_setting,school_type,teaching_method,gender,posttest)
View(newdata)
str(newdata)
#kategorik değişkenlerin faktör olarak atanması
newdata$school = as.factor(newdata$school)
newdata$school_setting = as.factor(newdata$school_setting)
newdata$school_type = as.factor(newdata$school_type)
newdata$gender = as.factor(newdata$gender)
newdata$teaching_method = as.factor(newdata$teaching_method)
newdata = DataCombine::DropNA(newdata)#Boş değişkenlerin düşürülmesi
schoolls = c("ANKYI","CCAAW","CIMBB","CUQAM","DNQDD","FBUMG")# school değişkeninden 6 tane okul seçilmesi
newdata = newdata[newdata$school%in%schoolls,]
levels(newdata$school)[1:6] = c("Okul1","Okul2","Okul3","Okul4","Okul5","Okul6")
##KEŞİFSEL VERİ ANALİZİ
#Faktör değişkenlerin yüzdelik dağılımı
as.data.frame(prop.table(table(newdata$school)))[1:6,]
as.data.frame(prop.table(table(newdata$school_setting)))
as.data.frame(prop.table(table(newdata$school_type)))
as.data.frame(prop.table(table(newdata$gender)))
as.data.frame(prop.table(table(newdata$teaching_method)))
#Tanımlayıcı istatistikler
newdata%>%group_by(school)%>%summarise(Count = n(),Mean = mean(posttest),StandartDev = sd(posttest),MIN = min(posttest),Medyan = median(posttest),MAX = max(posttest))
newdata%>%group_by(school_setting)%>%summarise(Count = n(),Mean = mean(posttest),StandartDev = sd(posttest),MIN = min(posttest),Medyan = median(posttest),MAX = max(posttest))
newdata%>%group_by(school_type)%>%summarise(Count = n(),Mean = mean(posttest),StandartDev = sd(posttest),MIN = min(posttest),Medyan = median(posttest),MAX = max(posttest))
newdata%>%group_by(gender)%>%summarise(Count = n(),Mean = mean(posttest),StandartDev = sd(posttest),MIN = min(posttest),Medyan = median(posttest),MAX = max(posttest))
newdata%>%group_by(teaching_method)%>%summarise(Count = n(),Mean = mean(posttest),StandartDev = sd(posttest),MIN = min(posttest),Medyan = median(posttest),MAX = max(posttest))

#VERİ GÖRSELLEŞTİRME
ggplot(newdata,aes(x =school,fill = school))+geom_bar()+theme_minimal()+scale_fill_brewer( palette = "Set2")+theme(legend.position = "none")+labs(x = "Okullar",y = "")
ggplot(newdata,aes(x = school,y = posttest,fill = school))+geom_boxplot()+ggtitle("Okullara Göre Sınav Puanları")
ggplot(newdata,aes(x =school_setting,fill = school_setting))+geom_bar()+theme_minimal()+scale_fill_brewer( palette = "Set2")+theme(legend.position = "none")+labs(x = "Okullar",y = "")
ggplot(newdata,aes(x = school_setting,y = posttest,fill = school_setting))+geom_boxplot()
ggplot(newdata,aes(x =school_type,fill = school_type))+geom_bar()+theme_minimal()+scale_fill_brewer( palette = "Set2")+theme(legend.position = "none")+labs(x = "Okullar",y = "")
ggplot(newdata,aes(x = school_type,y = posttest,fill = school_type))+geom_boxplot()
ggplot(newdata,aes(x =gender,fill = gender))+geom_bar()+theme_minimal()+scale_fill_brewer( palette = "Set2")+theme(legend.position = "none")+labs(x = "Okullar",y = "")
ggplot(newdata,aes(x = gender,y = posttest,fill = gender))+geom_boxplot()+ggtitle("Cinsiyete Göre Sınav Puanları")
ggplot(newdata,aes(x =teaching_method,fill = teaching_method))+geom_bar()+theme_minimal()+scale_fill_brewer( palette = "Set2")+theme(legend.position = "none")+labs(x = "Okullar",y = "")
ggplot(newdata,aes(x = teaching_method,y = posttest,fill = teaching_method))+geom_boxplot()+ggtitle("Okullara Göre Sınav Puanları")

#bağımsız değişken görselleştirme
#density plot
ggplot(newdata,aes(x =posttest))+geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  theme_ipsum_pub()+ggtitle("Posttest Değişkeni Dağılımı")+labs(x = "Posttest",y = "")
#boxplot(school)
ggplot(newdata,aes(y = posttest,x = school))+geom_boxplot(alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=10, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")+theme_minimal()
#boxplot(school_type)
ggplot(newdata,aes(y = posttest,x = school_type))+geom_boxplot(alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=10, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")+theme_minimal()
#boxplot(school_settings)
ggplot(newdata,aes(y = posttest,x = school_setting))+geom_boxplot(alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=10, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")+theme_minimal()
#boxplot(gender)
ggplot(newdata,aes(y = posttest,x = gender))+geom_boxplot(alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=10, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")+theme_minimal()
#boxplot(teaching_method)
ggplot(newdata,aes(y = posttest,x = teaching_method))+geom_boxplot(alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=10, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")+theme_minimal()









##############################
#####   One-Way Anova   ######
##############################
one_way_anova = aov(data = newdata,posttest~school)
summary(one_way_anova)
'
p-değeri <0.05 olduğundan H0 hipotezi reddedilir.Seçilen 6 okulun sınav sonuçlarının ortalamaları arasında anlamlı farklılık vardır.
'
##Tek Yönlü ANOVA varsayımları
#1-Artıkların dağılımı
halfnorm(one_way_anova$residuals)
qqline(one_way_anova$residuals,col = 6,lwd = 3,lty = 4)
#normallik testleri
olsrr::ols_test_normality(one_way_anova$residuals,mean = mean(one_way_anova$residuals),sd = sd(one_way_anova$residuals))
'
Testlerin sonucunda sadece Kolmogorov-Smirnov testi normallik varsayımını kabul etmiştir,diğerleri 
H0 hipotezini reddeder.Kolmogorov testini dikkate aldığım için H0 hipotezi reddedilemez,artıkların
dağılımı normaldir.
'
#Varyansların Homojenliği Varsayımı
car::leveneTest(posttest~school,data = newdata)
bartlett.test(posttest~school,data = newdata)
'
Levene ve bartlett testine göre varyansların homojenliği varsayımı geçersizdir.
Dönüşüm yapılarak varyanslar homojen hale getirilmeye çaışılacak.
'
#kök dönüşümü
car::leveneTest(sqrt(posttest)~school,data = newdata)
bartlett.test(sqrt(posttest)~school,data = newdata)
'p value<0.05'
#log dönüşümü
car::leveneTest(log(posttest)~school,data = newdata)
bartlett.test(log(posttest)~school,data = newdata)
#boxcox dönüşümü
bc = MASS::boxcox(one_way_anova)
lambda = bc$x[which.max(bc$y)]
#yeni model
model_boxcox = aov(((posttest)^lambda)~school,data = newdata)
summary(model_boxcox)
car::leveneTest(((posttest)^lambda)~school,data = newdata)
bartlett.test(((posttest)^lambda)~school,data = newdata)
#min-max trans
minmax_transform = function(x)
{
  return((x-min(x))/(max(x)-min(x)))
}
new_posttest = apply(as.data.frame(newdata$posttest),2,FUN = minmax_transform)
car::leveneTest(new_posttest[,1]~school,data = newdata)
bartlett.test(new_posttest[,1]~school,data = newdata)
'Kullanılan dönüşümlerle de varsayım sağlanmadı.Varyansların homojenliği varsayımı 
dikkate alınmadan devam edilecek.'



############################
## Rastgele Blok Tasarımı ##
############################
# Main faktör = school,blok faktör = teaching_method
blok_model = aov(data = newdata,posttest~teaching_method+school)
summary(blok_model)
'
bu sonuçlara göre faktör düzeyleri yani school için p<0.05 olduğundan H0 hipotezi reddedilir.Okulların ortalama sınav puanları arasında 
anlamlı farklılık vardır.
Ayrıca bloklar içinde p value = 0.0157<0.05 olduğundan H0 hipotezi reddedilir.Böylece farklı öğretme tekniklerine göre alınan sınav puanları 
ortalamaları arasında anlamlı farklılık vardır.
Tek yönlü ANOVA modeli kurulduğu zaman MSE= 29 ,bloklama yapıldığı zaman MSE= 27 ye düşmüştür.Bu nedenle bloklama yapmak az da olsa modelin
hassaslığını artırmıştır.
'
# İkili Karşılaştırmalar
'
Rastgele blok tasarımı için Tukey testi ile ikili karşılaştırmalar yapalım
'
TukeyHSD(blok_model)
plot(TukeyHSD(blok_model,which = "school")) #main faktör için grafik
plot(TukeyHSD(blok_model,which = "teaching_method")) #blok faktör için grafik
'
Bu sonuçlara göre FBUMG-ANKYI,CIMBB-CCAAW,FBUMG-CCAAW,FBUMG-CIMBB,DNQDD-CUQAM okullarının sınav puanları ortalaması arasında anlamlı
farklılık yoktur,diğerleri arasında anlamlı farklılık vardır.
'
##########
# ANCOVA #
##########
#Model içinde ortak değişken etkisini azaltmak istediğimizxden dolayı bu ortak değişken yazılmalıdır.(pretest)
newdata = newdata%>%mutate("pretest" = data[data$school%in%schoolls,]$pretest)
ancova_model = aov(data = newdata,posttest~school+pretest)
summary(ancova_model)
#Ortak değişken etkisini arındırmak için ve modelimiz dengeli olmadığı için tip III testi kullanmalıyız.
car::Anova(aov(data = newdata,posttest~pretest+school),type = "III")#]Düzeltilmiş kareler toplamı
'
Denemeler için yani school faktörü için test istatistiği değeri F0 = 459.053 ve p-değeri<0.05 olduğundan H0 hipotezi reddedilir.
böylece deneye katılanların ilk sınav sonuçları göz önünde bulundurulduğunda 2.sınav bakımından okulların etkisinin anlamlı olduğunu
elde ederiz.
'
#ANCOVA kullanmak uygun mu ?
'
pretest için F testi = 459.053 ve p<0.05 olduğundan H0:Beta=0 Hipotezi reddedilir,Ancova kullanmak uygundur.
'

######## Latin Kare Tasarımı
latin_data = newdata[newdata$school =="Okul1"|newdata$school =="Okul2",]
latin_data = latin_data[c(1,45,2,43),]
latin_data$school = as.factor(latin_data$school)
latin_data$gender = as.factor(latin_data$gender)
latin_data$teaching_method = as.factor(latin_data$teaching_method)

lk_des = aov(posttest~gender+teaching_method+school,data = latin_data)
summary(lk_des)



lkdata = newdata%>%dplyr::select(posttest,school,gender,teaching_method)
lkdata = lkdata[lkdata$school%in%c("Okul1","Okul2"),]


lkdata$school = as.factor(lkdata$school)
lkdata$gender = as.factor(lkdata$gender)
lkdata$teaching_method = as.factor(lkdata$teaching_method)

lk = aov(posttest~gender+teaching_method+school,data = newdata)
summary(lk)






