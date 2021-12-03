case2 <- rio::import(here::here("MIDUS III Final Exam Fall 2021.dta"))
 
#clean the data to exclude "don't know", "refused", "inapp" responses from the alcage, ciage variables
case2 %<>% mutate_at(c("alcage", "cigage"), function(x) ifelse(x>96, NA, x))
 
# make the outcome variable as ordered factor (increasing order as health improves)
case2 %<>% mutate(health = ordered(health, levels = c(4, 3, 2, 1)))
 
# to account for those who never drank or smoke, create a corresponding dichotomous variable
case2 %<>% mutate(
  smoke = ifelse(cigage==96, 0, 1),
  drink = ifelse(alcage==96, 0, 1)
)
#outcome variable
ggplot(case2, aes(health))+geom_bar()
# explanatory variables
table1(~factor(depress) + factor(bp) + factor(smoke) + factor(drink) + age + alcage + cigage, data = case2)
factor(depress)
factor(bp)
factor(smoke)
factor(drink)
# table
table1(~factor(depress) + factor(bp) + age + alcage + cigage | health, data = case2)
factor(depress)
factor(bp)
# univaraite cumulative logit
for(var in c("depress", "bp", "age", "alcage", "cigage")){
  temp <- case2[which(case2[,var]!=96),]
  print(
    vglm(case2$health ~ case2[,var], family=cumulative(parallel=TRUE)) %>% summary()
  )
}
main_mod  <- vglm(health ~ depress + alcage + cigage + age +bp,
                        family=cumulative(parallel=TRUE), data=case2)
 
summary(main_mod)
temp<-summary(main_mod)
# Exponentiated coefficients
res <- data.frame(
coefficients = round(temp@coefficients,3),
# 95% CI
ci_low = round(temp@coef3[,1]-1.96*temp@coef3[,2],3),
ci_high = round(temp@coef3[,1]+1.96*temp@coef3[,2], 3),
#p-value
pval =round(temp@coef3[,4],3)
)
# check for proportional odds assumption
polr_mod <- polr(health ~ depress + alcage + cigage + age +bp,
                 Hess = T, data = case2)
brant(polr_mod)
lrtest(main_mod)
fitted_bp <- data.frame(bp = case2$bp, fitted(main_mod))
pb <- fitted_bp %>% group_by(bp) %>% summarize(mean(X1), mean(X2), mean(X3), mean(X4))
 
pb
 health = case_when(health=="mean(X1)" ~ "Excellent",
                     health=="mean(X2)" ~ "Very good/Good",
                     health=="mean(X3)" ~ "Fair",
                     TRUE ~ "Poor"),
  bp = ifelse(bp==1, "High blood pressure", "No high blood pressure")
) %>% ggplot(aes(health, p)) + geom_bar(aes(group = bp, fill = factor(bp)),stat="identity", position = "dodge") +
  theme(legend.title = element_blank(),
        legend.position = "top")

