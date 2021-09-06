AT=ordered(OTT_Platform_Survey_Responses_$`appropriate time to watch content with your family`,
           levels=c(1,2,3,4,5),
           labels=c("Strongly Disagree","Disagree","Not Sure","Agree","Strongly Agree"))
CQ=ordered(OTT_Platform_Survey_Responses_$`Content Quality`,
           levels=c(1,2,3,4,5),
           labels=c("Strongly Disagree","Disagree","Not Sure","Agree","Strongly Agree"))
NO_Adv=ordered(OTT_Platform_Survey_Responses_$`No advertisement`,
               levels=c(1,2,3,4,5),
               labels=c("Strongly Disagree","Disagree","Not Sure","Agree","Strongly Agree"))
OTT_over_TV=ordered(OTT_Platform_Survey_Responses_$`OTT platform over television`,
                    levels=c(1,2,3,4,5),
                    labels=c("Strongly Disagree","Disagree","Not Sure","Agree","Strongly Agree"))
convinience=ordered(OTT_Platform_Survey_Responses_$convenience,
                    levels=c(1,2,3,4,5),
                    labels=c("Strongly Disagree","Disagree","Not Sure","Agree","Strongly Agree"))
continue_subs=ordered(OTT_Platform_Survey_Responses_$`continue your subscription`,
                      levels=c(1,2,3,4,5),
                      labels=c("Strongly Disagree","Disagree","Not Sure","Agree","Strongly Agree"))
free_OTT_over_priced=ordered(OTT_Platform_Survey_Responses_$`free OTT platforms over paid ones`,
                             levels=c(1,2,3,4,5),
                             labels=c("Strongly Disagree","Disagree","Not Sure","Agree","Strongly Agree"))
lackof_regional_content=ordered(OTT_Platform_Survey_Responses_$`lack of content on regional languages`,
                                levels=c(1,2,3,4,5),
                                labels=c("Strongly Disagree","Disagree","Not Sure","Agree","Strongly Agree"))
OTT_are_overpriced=ordered(OTT_Platform_Survey_Responses_$`OTT platforms are overpriced`,
                           levels=c(1,2,3,4,5),
                           labels=c("Strongly Disagree","Disagree","Not Sure","Agree","Strongly Agree"))

mean(OTT_Platform_Survey_Responses_$`appropriate time to watch content with your family`)
mean(OTT_Platform_Survey_Responses_$`Content Quality`)
mean(OTT_Platform_Survey_Responses_$`No advertisement`)
mean(OTT_Platform_Survey_Responses_$`OTT platform over television`)
mean(OTT_Platform_Survey_Responses_$convenience)
mean(OTT_Platform_Survey_Responses_$`continue your subscription`)
mean(OTT_Platform_Survey_Responses_$`free OTT platforms over paid ones`)
mean(OTT_Platform_Survey_Responses_$`lack of content on regional languages`)
mean(OTT_Platform_Survey_Responses_$`OTT platforms are overpriced`)

sd(OTT_Platform_Survey_Responses_$`appropriate time to watch content with your family`)
sd(OTT_Platform_Survey_Responses_$`Content Quality`)
sd(OTT_Platform_Survey_Responses_$`No advertisement`)
sd(OTT_Platform_Survey_Responses_$`OTT platform over television`)
sd(OTT_Platform_Survey_Responses_$convenience)
sd(OTT_Platform_Survey_Responses_$`continue your subscription`)
sd(OTT_Platform_Survey_Responses_$`free OTT platforms over paid ones`)
sd(OTT_Platform_Survey_Responses_$`lack of content on regional languages`)
sd(OTT_Platform_Survey_Responses_$`OTT platforms are overpriced`)

newData=data.frame(
  Appropriate_Time=OTT_Platform_Survey_Responses_$`appropriate time to watch content with your family`,
  content_quality=OTT_Platform_Survey_Responses_$`Content Quality`,
  No_adv=OTT_Platform_Survey_Responses_$`No advertisement`,
  OTT_vs_TV=OTT_Platform_Survey_Responses_$`OTT platform over television`,
  Convinience=OTT_Platform_Survey_Responses_$convenience,
  Subscription=OTT_Platform_Survey_Responses_$`continue your subscription`,
  free_OTT_vs_paid=OTT_Platform_Survey_Responses_$`free OTT platforms over paid ones`,
  regional_lang_conten=OTT_Platform_Survey_Responses_$`lack of content on regional languages`,
  overprice_OTT=OTT_Platform_Survey_Responses_$`OTT platforms are overpriced`
)
newData
KMO=function(x){
  x=subset(x,complete.cases(x))
  r=cor(x)
  r
  r2=r^2
  i=solve(r)
  d=diag(i)
  p2=(-i/sqrt(outer(d,d)))^2
  diag(r2) <- diag(p2) <- 0
  KMO=sum(r2)/(sum(r2)+sum(p2))
  MSA=colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO,MSA=MSA))
}
KMO(newData)
t=subset(newData,complete.cases(newData))
o=cor(t)
o=o^2
u=solve(o)
m=diag(u)
p2=(-u/sqrt(outer(m,m)))^2
p2