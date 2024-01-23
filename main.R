library(tidyverse)
library(knitr)
library(readxl)  #library package required for using read_excel()
library(RColorBrewer)  #required for using display.brewer.all()
library(grid)  #required for using grid.layout()
library(sjPlot)  #required for using sjt.df() to convert regression summary into beautiful format
library(scales)
library(broom)  #required for using tidy() on t.test
library(kableExtra)


QWE <- read_excel("./data/churn_data.xlsx")
summary <- as.data.frame(do.call(cbind, lapply(QWE, summary)))
QWEsummary <- modify_if(summary, ~is.numeric(.), ~round(., 2))
kable(QWEsummary)

View(QWE)

QWEdatatype <- QWE %>% summarise_all(typeof) %>% gather
names(QWEdatatype) <- c("Variables", "Data Type")

QWEdatatype$Detail <- c("Discrete","Discrete","Discrete","Continuous","Continuous","Discrete","Discrete","Discrete","Discrete","Discrete","Discrete","Discrete","Discrete")
kable(QWEdatatype, align = "c")
# Customer Happiness Index (CHI)
# The Customer Happiness Index (CHI) is a metric that reflects the degree of satisfaction a customer feels towards your product, service, or experience with your company. The CHI survey question is simple. It asked customers to rate their satisfaction level by giving them three choices: a smiling face emoticon, a neutral face emoticon, and a frowning face emoticon. 
# 
# For example, a CHI question would look like this:
#   
#   How did we do today?
#   
#   🙂 :-l 🙁
# 
# When measuring customer satisfaction, take the number of smiling face emoticon votes against the total number of votes to determine your CHI.
# The Customer Happiness Index speaks about happy customers as a consequence of the manner in which your product or service helped them become successful. It is calculated based on three indicators: product use frequency, the extension of product use, and the preferred features. CHI was created by HubSpot and is a good predictor of customer churn rate.

# a. Distribution of CHI score for December 2011 by different churn outcomes.

plot1 <- QWE %>% ggplot(aes(x = `CHI Score Month 0`, fill = factor(`Churn (1 = Yes, 0 = No)`))) +
  geom_histogram(binwidth = 30, alpha = 0.5, color = "white") +
  stat_bin(binwidth = 30, geom="text", family = "Times New Roman", size=3,
           aes(label=..count.., group = factor(`Churn (1 = Yes, 0 = No)`), y=..count..+ 100)) +
  ggtitle("plot 1: Distribution of CHI Score for December 2011 by Different Churn Outcomes") +
  labs(x = "CHI score for December 2011", y = "Count", fill = "Churn Outcomes") +
  scale_fill_manual(labels = c("Unchurn", "Churn"), values = c("black", "red")) +
  theme_minimal(base_family = "Times New Roman") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))


plot2 <- QWE %>% filter(`Churn (1 = Yes, 0 = No)` == 0) %>%
  ggplot(aes(x = `CHI Score Month 0`))+
  geom_histogram(binwidth = 30,alpha =0.5, color='beige', fill="cornflowerblue") +
  geom_vline(aes(xintercept = mean(`Churn (1 = Yes, 0 = No)`)),color = "red") +
  stat_bin(binwidth = 30,geom="text",family="Times New Roman", size=3,
           aes(label =..count..,y=..count..+50))+
  ggtitle("plot 2: CHI Score for Unchurned Customers") +
  labs(x = "CHI score for December 2011", y = "Count") +
  theme_minimal(base_family = "Times New Roman") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  
plot3 <- QWE %>% filter(`Churn (1 = Yes, 0 = No)` == 1) %>%
  ggplot(aes(x = `CHI Score Month 0`)) +
  geom_histogram(binwidth = 30, alpha = 0.5, color = "white", fill = "maroon") +
  geom_vline(aes(xintercept = mean(`Churn (1 = Yes, 0 = No)`)), color = "black") +
  stat_bin(binwidth = 30, geom="text", family = "Times New Roman", size=3,
           aes(label=..count.., y=..count..+ 5)) +
  ggtitle("plot 3: CHI Score for Churned Customers") +
  labs(x = "CHI score for December 2011", y = "Count") +
  theme_minimal(base_family = "Times New Roman") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

#define a function to be used on plotting
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}  
grid.newpage()  #create a new grid for the plots
pushViewport(viewport(layout = grid.layout(2,2))) #change the composition of the grid
print(plot1, vp = vplayout(1,1:2))
print(plot2, vp = vplayout(2,1))
print(plot3, vp = vplayout(2,2))
mean_unchurn <- mean(QWE$`CHI Score Month 0`[QWE$`Churn (1 = Yes, 0 = No)`==0])
mean_churn <- mean(QWE$`CHI Score Month 0`[QWE$`Churn (1 = Yes, 0 = No)`==1])
sprintf("Mean CHI score of unchurned customers is %f", mean_unchurn)
sprintf("Mean CHI score of churned customers is %f", mean_churn)

# 
# Plot 1 shows the total distribution of CHI score for December 2011 by different churn outcomes. 
# Plot 2 and plot 3 show the distribution of CHI score for unchurned and churned customers. From the three plots, we observed that both groups have most customers with 0 to 30 CHI score, which might be caused by the deficiency of data (resulted in 0 score).
# Aside from this time interval, unchurned customers seem to have the peak value with 120 to 150 CHI score, while the number of churned customers seem to decrease as the CHI score goes higher. 
# Therefore, we made preliminary conjecture that customers who churned generally have lower CHI scores than customers who didn’t left. 
# With further mathematical analysis, we figured out that the mean CHI score of unchurned customers is 88.61 and the mean CHI score of churned customers is 63.27, this phenomenon proves our previous conjecture. 
# In conclusion, its reasonable for us to believe that CHI score does have negative correlation with the possibility of whether a customer decided to churn.


#B. AVERAGE CHURN RATE BY CUSTOMER AGE

ageQWE <- QWE %>% group_by(`Customer Age (in months)`) %>% 
  summarise(`avgChurn` = mean(`Churn (1 = Yes, 0 = No)`))

ageGroup <- QWE %>%
  mutate(`customerAge`=  cut(`Customer Age (in months)`, breaks = c(-Inf,10,20,30,40,50,60, Inf),
         labels = c ("0 to 10 months", "10 to 20 months", "20 to 30 months", "30 to 40 months", "40 to 50 months", "50 to 60 months", "over 60 months"))) %>%
  group_by(`customerAge`, `Churn (1 = Yes, 0 = No)`) %>%
  summarize(n = n()) %>%
  spread(`Churn (1 = Yes, 0 = No)`,n) %>%
  mutate(`1`= ifelse(is.na(`1`) == TRUE, 0, `1`), `Churn Rate` = `1`/(`0`+`1`))
View(ageGroup)

plot4 <- ageQWE %>%
  ggplot(aes(x=`Customer Age (in months)`,y= `avgChurn`)) + 
  geom_point() +
  ggtitle("Average Churn Rate by Customer Age") +
  scale_y_continuous(labels = percent_format()) +
  labs(x='CUSTOMER AGE', y='Average Churn Rate') +
  theme_minimal(base_family = "Times New Roman") 

plot5 <- ageGroup %>%
  ggplot(aes(customerAge)) +
  geom_point(aes(y = `0`/30000, size =6, color="black",alpha = 0.5)) + 
  geom_point(aes(y = `1`/30000, size =6, color="hotpink",alpha = 0.5))+
  geom_text(aes(label = `0`, y = `0`/30000+0.002),family = "Times New Roman", size =3, hjust = -0.05) +
  geom_text(aes(label = `1`, y = `1`/30000),family = "Times New Roman", size = 3, hjust = -1.5) +
  geom_line(aes(y = `Churn Rate`), group = 1) +
  geom_point(aes(y = `Churn Rate`)) +
  ggtitle("plot 5: Average Churn Rate by Customer Age Groups") +
  geom_text(aes(x = customerAge , label = scales::percent(`Churn Rate`), y = `Churn Rate`), family = "Times New Roman", size = 4, vjust = -2) +
  scale_y_continuous(labels = percent_format(), name = expression("Churn Rate"), 
                     sec.axis = sec_axis(~ . *30000 , name = "Number of Customer")) +
  theme_minimal(base_family = "Times New Roman") +
  theme(plot.title = element_text(hjust = 0.5))
  
grid.newpage()  #create a new grid for the plots
pushViewport(viewport(layout = grid.layout(2,1))) #change the composition of the grid
print(plot4, vp = vplayout(1,1))
print(plot5, vp = vplayout(2,1))


# 
# Plot 4 shows the average churn rate grouped by customers age. QWE's customer age range from 0 months to 67 months. 
# From plot 4, we observed that the peak value of the average churn rate comes with customers of 12 months, and another high churn rate comes with customers of 47 months. 
# As the distribution is too scattered, we decided to separate customer age into several sub-groups. Plot 5 shows the averaged churn rate by customer age groups. 
# The big plot in black and red indicates the number of customers who didn't leave and left, and the line shows the churn rate for each group.
# From plot 5, we observed that customers of 10 to 20 months have the highest churn rate. Besides, customers subscribed QWE services within ten months only have 2.91% possibility to leave.
# Furthermore, for customers who have been with QWE for over 20 months, the churn rate goes down as the customer age goes up. 
# Therefore, it's reasonable for us to conclude that new customers are not so likely to churn because they are not familiar with the services, so they want to learn about it. 
# Old customers have already got value from the services, so they are less likely to leave. Customers who have been with QWE for 10 to 20 months are the riskiest group because they are most likely to choose other company.  

  

#C. NUMBER OF CUSTOMERS WHO CHURN BY AGE
plot6 <- QWE %>% group_by(`Customer Age (in months)`) %>%
  summarise(`numberCustomers` = n()) %>%
  ggplot(aes(x= `Customer Age (in months)`,y = `numberCustomers`)) + 
  geom_point() +
  geom_smooth(color = "red", size = 0.5) +
  ggtitle("Number of customers who churn by customer age") + 
  theme_minimal(base_family = "Times New Roman") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

plot6


# Plot 6 shows the number of customers who churn by customer age. The points in the plot indicate the number of customers, and the line shows the general trend. Plot 6 tells us that as the customer age grows higher, the number of customers who left goes down. Despite the possibility that the total number of customers with high age is low, we can conclude that older customers are less likely to unsubscribe the service.  
# 
# #### d. Conclusion
# 
# From the plots above, we made the following conclusion:  
#   1) CHI score does have a negative correlation with the possibility of whether a customer decided to churn. Thus, customers with lower CHI scores are more likely to leave.  
# 2) Customers who have been with QWE for 10 to 20 months are the riskiest group because they are most likely to choose other company.  
# 3) As the customer age grows up, the number of customers who churned goes down. Therefore, new customers are more likely to leave.  
# With the conclusions above, we think QWE Inc. should focus on its new customers and the customers with low CHI scores. They should try harder to convince those customers to extend the contract.  

#PART4 STATISTICAL ANALYSIS




Statistics <- function(x) {
  result <- tidy(t.test(x[QWE$`Churn (1 = Yes, 0 = No)` == 1], x[QWE$`Churn (1 = Yes, 0 = No)` == 0]))
  result <- result[c(2, 3, 5), 8]  # Select specific rows and columns
  result <- round(result, 2)  # Round the values to two decimal places
  return(result)
}

# Apply the function to the 11 variables and save the result as a data frame
t_result <- as.data.frame(sapply(QWE[, c(2, 4:13)], Statistics))

t_result



# PART 5  Logistic Regression


#run general linear regression on customer churn
glm_churn <- glm(`Churn (1 = Yes, 0 = No)`~`Customer Age (in months)` + `CHI Score Month 0` + `CHI Score 0-1` + `Support Cases Month 0` + `Support Cases 0-1` + `SP Month 0` + `SP 0-1` + `Logins 0-1` + `Blog Articles 0-1` + `Views 0-1` + `Days Since Last Login 0-1`, data = QWE)

#show the result of the regression in a statistic table
tab_model(glm_churn, show.se = TRUE, show.stat = TRUE, show.p = TRUE, digits = 4, digits.p = 4)

# **The significant factors affecting customer churn**  
#   
#   According to the result of general linear regression, there are eleven independent variables. Among the independent variables, only four are significant: `Customer Age (in months)`, `CHI Score Month 0`, `CHI Score 0-1`, `Days Since Last Login 0-1`. These four variables have p-values less than 0.05, which means that at 5 percent significance level, the null hypothesis, that the coefficient estimator is equal to zero, can be rejected. Thus, for the four variables, we in favor of the alternative hypothesis that the estimators for them are statistically different from zero, which means that they can be considered as significant factors affecting customer churn. Among these variables, `Customer Age (in months)` and `Days Since Last Login 0-1` have a positive correlation with the possibility of churn with estimators of 0.0008 and 0.0006. `CHI Score Month 0` and `CHI Score 0-1` have a negative correlation with the possibility of churn with estimators of -0.0003 and -0.0004.  
# 
# **Lesson for Aggarwal and Wall**  
#   
#   From the regression result, Aggarwal and Wall should learn that the four most important factors that influence customers' churn outcomes are 1) how long they have been with QWE, 2) CHI score, 3) the change of CHI score, 4) how often they login the website. Therefore, they should work hard on analyzing these variables to decide which segment to focus.



# PART 6  Customer Segmentation

##### a. Create segments in the data based on customer age - "New" (0-6 months), "Medium" (7-12 months), "Old" (13+ months).


#use cut() to breack customers into three age groups
QWE <- QWE %>%
  mutate(segment = cut(`Customer Age (in months)`, breaks = c(-Inf, 6, 12, Inf), labels = c('New', 'Medium', 'Old')))


##### b. Perform logistic regression as part (5) above for each segment separately


#filter out the customers for each group and save as three new data frame
QWEnew <- QWE[QWE$segment=='New',]
QWEmedium <- QWE[QWE$segment=='Medium',]
QWEold <- QWE[QWE$segment=='Old',]

#run general linear regression on each group
glm_new <- glm(`Churn (1 = Yes, 0 = No)`~`Customer Age (in months)` + `CHI Score Month 0` + `CHI Score 0-1` + `Support Cases Month 0` + `Support Cases 0-1` + `SP Month 0` + `SP 0-1` + `Logins 0-1` + `Blog Articles 0-1` + `Views 0-1` + `Days Since Last Login 0-1`, data = QWEnew)
glm_medium <- glm(`Churn (1 = Yes, 0 = No)`~`Customer Age (in months)` + `CHI Score Month 0` + `CHI Score 0-1` + `Support Cases Month 0` + `Support Cases 0-1` + `SP Month 0` + `SP 0-1` + `Logins 0-1` + `Blog Articles 0-1` + `Views 0-1` + `Days Since Last Login 0-1`, data = QWEmedium)
glm_old <- glm(`Churn (1 = Yes, 0 = No)`~`Customer Age (in months)` + `CHI Score Month 0` + `CHI Score 0-1` + `Support Cases Month 0` + `Support Cases 0-1` + `SP Month 0` + `SP 0-1` + `Logins 0-1` + `Blog Articles 0-1` + `Views 0-1` + `Days Since Last Login 0-1`, data = QWEold)

#show the result of three regressions in the table
tab_model(glm_new, show.se = TRUE, show.stat = TRUE, show.p = TRUE, digits = 4, digits.p = 4, title = "Segment: New")
tab_model(glm_medium, show.se = TRUE, show.stat = TRUE, show.p = TRUE, digits = 4, digits.p = 4, title = "Segment: Medium")
tab_model(glm_old, show.se = TRUE, show.stat = TRUE, show.p = TRUE, digits = 4, digits.p = 4, title = "Segment: Old")


# Which variables consistently affect all segments?
#   Do their magnitudes vary significantly across segments? (25 Points)  
# 
# **Segment: New**  
#   
#   For customers in the segment "New", `Customer Age (in months)`, `CHI Score 0-1`, `Support Cases 0-1`, and `Logins 0-1` are the four factors have significant estimators at 5% significance level. `Customer Age (in months)`, `Support Cases 0-1`, and `Logins 0-1` have 0.0101, 0.0029, and 0.0002 positive correlation with churn outcome. `CHI Score 0-1` has -0.0004 negative correlation with churn outcome.  
# 
# **Segment: Medium**  
#   
#   For customers in the segment "Medium", `Customer Age (in months)`, `CHI Score Month 0`, and `Days Since Last Login 0-1` are the three factors have significant estimators at 5% significance level. `Customer Age (in months)` and `Days Since Last Login 0-1` have 0.0203 and 0.0015 positive correlation with churn outcome. `CHI Score Month 0` has -0.0007 negative correlation with churn outcome.  
# 
# **Segment: Old**  
#   
#   For customers in the segment "Old", `Customer Age (in months)` and `CHI Score Month 0` are the two factors have significant estimators at 5% significance level. `Customer Age (in months)` and `CHI Score Month 0` have -0.0019 and -0.0006 negative correlation with churn outcome.  
# Generally, at 5 percent significance level, the variables with significant estimators for each segment are:  
#   1) New: `Customer Age (in months)`, `CHI Score 0-1`, `Support Cases 0-1`, `Logins 0-1`.  
# 2) Medium: `Customer Age (in months)`, `CHI Score Month 0`, `Days Since Last Login 0-1`.  
# 3) Old: `Customer Age (in months)`, `CHI Score Month 0`.  
# 
# **Variables differ across segments**  
#   
#   Among all the significant factors, `CHI Score 0-1`, `Support Cases 0-1` and `Logins 0-1` only have a significant effect on the segment "New". `Days Since Last Login 0-1` only have a significant effect on the segment "Medium", and `CHI Score Month 0` have a significant effect on the segment "Medium" and "Old".   
# 
# **Variable consistently affect all segments**  
#   
#   `Customer Age (in months)` is the variable that consistently affects all segments. It seems like no matter which segment is the customer in, and his/her age always have a significant influence on churn outcome. However, for segment "New" and "Medium", the correlation between the age and the possibility of churn is positive, the same correlation for segment "Old" is negative -0.0019. Thus, for segment "Old", customers are more likely to leave as the age grows.
# 
# ***
#   
#   > ### PART 7  Final Reflections
#   
#   Final Reflections: When you have completed the assignment, please reflect on some of the longer-lasting lessons of this experience. Most students/teams will gain key insights about QWE Inc. and about customer churn patterns in general. Write a thoughtful paragraph describing the team’s one most noteworthy and valuable discovery or insight. Please be specific. (5 Points)
# 
# According to the analysis above, we generated an initial insight into the factors that may influence QWE customers' churn outcome. As Customer Happiness Index score, no matter `CHI Score Month 0` or `CHI Score 0-1`, is one of the most influential variables for the possibility of churn, we think QWE Inc. should pay more efforts on analyzing it. For example, QWE can continuously follow the CHI score for customers in each month to estimate whether a customer has the sign of leaving. By predicting customers decisions in advance, QWE can spend more time pursuing customers to stay before they finally made their decisions, rather than convincing them hurriedly.
# 
# 
# 
# 
# 









