alzheimers.data <- read.csv("C:/Users/bushr/OneDrive/Desktop/Alzheimers_data.csv", header=TRUE, sep=",")
#Specifying the reference
gender.rel <- relevel(as.factor(alzheimers.data$gender), ref = "female")
group.rel <- relevel(as.factor(alzheimers.data$group), ref = "Tx")
dementia.rel <- relevel(as.factor(alzheimers.data$dementia), ref = "severe")
#Regression Model 
summary(fitted.model <- glm(worsen ~ gender.rel + group.rel + dementia.rel + offset(log(duration)), 
                            data = alzheimers.data, family = poisson(link = log)))
#Prediction 
alzheimers.data$duration <-rep(1000,12)
pred.cases <- predict(fitted.model, alzheimers.data, type = "response")
sort(pred.cases, decreasing = TRUE)


gss_cat %>% 
  group_by(rincome) %>% 
  summarise(
    avg_age = round(mean(age, na.rm = T),1) 
  ) %>% 
  ggplot(aes(x=avg_age, y=fct_relevel(rincome, "Not applicable"))) + 
  geom_point()
