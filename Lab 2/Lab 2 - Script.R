# Read in the data
facebook=read.csv("facebook.csv")

# Part 1

# Question 1
View(facebook)
# Result
# 500 Posts


# Question 2
summary(facebook)
# Result
# Action: 215
# Inspiration: 155
# Product: 130


# Question 3 
# The number of likes on pages without a comment
boxplot(facebook$Page.total.likes ~ facebook$comment, 
        xlab='Comments', ylab='Total Likes', 
        main='Total likes vs comments (0-no comment, 1-comment)',
        col=c('red', 'blue'))

# Question 4
prop.table(table(facebook$comment, facebook$Category), margin=2)
# Result
#      Action Inspiration   Product
# 0 0.3069767   0.1225806 0.1615385
# 1 0.6930233   0.8774194 0.8384615


#Part2

# Question 1
table(facebook$comment)
# Result
#  0   1 
#106 394

# Question 2
table(facebook$Paid)
# Result
#140 paid, 360 unpaid

# Question 3
prop.table(table(facebook$comment, facebook$Paid), margin=2)
# Result
#             0         1
#   0 0.2388889 0.1428571
#   1 0.7611111 0.8571429

# Part 3
simpleLModel = glm(comment ~ Paid, family=binomial(link='logit'), data=facebook)
summary(simpleLModel)
# Result
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.9728   0.5553   0.7389   0.7389   0.7389  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   1.1588     0.1236   9.375   <2e-16 ***
#  Paid          0.6330     0.2713   2.333   0.0196 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 516.59  on 499  degrees of freedom
#Residual deviance: 510.69  on 498  degrees of freedom
#AIC: 514.69

#Create observations of paid and unpaid ads
new.obs = data.frame(Paid=1)
new.obs2 = data.frame(Paid=0)

# predict whether there will be comment or not.
predict(simpleLModel, newdata = new.obs, type= "response")
# Result
#         1 
# 0.8571429 

predict(simpleLModel, newdata = new.obs2, type = "response")
# Result
#         1 
# 0.7611111

# Create a simple logistic regression model to see if the hour a post comes out 
# is a predictor of comments
simpleLModel2 = glm(comment ~ Post.Hour, family=binomial(link='logit'), data=facebook)
summary(simpleLModel2)
# Result
# Deviance Residuals: 
#Min       1Q   Median       3Q      Max  
#-2.3548   0.4001   0.5553   0.6290   1.4823  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)    1.09861    1.15470   0.951    0.341
#Post.Hour2     1.07044    1.26961   0.843    0.399
#Post.Hour3     0.69315    1.18790   0.584    0.560
#Post.Hour4     0.11778    1.22285   0.096    0.923
#Post.Hour5     0.60614    1.38717   0.437    0.662
#Post.Hour6     1.60944    1.54919   1.039    0.299
#Post.Hour7    -0.62861    1.28776  -0.488    0.625
#Post.Hour8    -0.76214    1.29468  -0.589    0.556
#Post.Hour9     0.09097    1.23275   0.074    0.941
#Post.Hour10    0.42121    1.19180   0.353    0.724
#Post.Hour11   -0.43937    1.19770  -0.367    0.714
#Post.Hour12   -0.89097    1.21357  -0.734    0.463
#Post.Hour13    0.60614    1.21699   0.498    0.618
#Post.Hour14    1.38629    1.55456   0.892    0.373
#Post.Hour15   -0.40547    1.44338  -0.281    0.779
#Post.Hour16  -16.66468 1455.39799  -0.011    0.991
#Post.Hour17   -0.40547    1.68325  -0.241    0.810
#Post.Hour18   -1.79176    1.68325  -1.064    0.287
#Post.Hour19  -16.66468 1455.39799  -0.011    0.991
#Post.Hour20   14.46746 1455.39799   0.010    0.992
#Post.Hour22   14.46746 1455.39799   0.010    0.992
#Post.Hour23   14.46746 1455.39799   0.010    0.992

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 516.59  on 499  degrees of freedom
#Residual deviance: 475.66  on 478  degrees of freedom
#AIC: 519.66

#Number of Fisher Scoring iterations: 14


# Create a 95% confidence interval for this simple logistic regression model
confint(simpleLModel2, level = 0.95)
# Result
#2.5 %     97.5 %
#  (Intercept)   -0.9568753   4.105100
#Post.Hour2    -2.0581204   3.412202
#Post.Hour3    -2.3487612   2.823495
#Post.Hour4    -2.9625626   2.329930
#Post.Hour5    -2.6465992   3.303776
#Post.Hour6    -1.7912666   5.039981
#Post.Hour7    -3.7836685   1.731076
#Post.Hour8    -3.9261727   1.610375
#Post.Hour9    -3.0001882   2.327099
#Post.Hour10   -2.6250265   2.560353
#Post.Hour11   -3.4925128   1.711666
#Post.Hour12   -3.9623772   1.293980
#Post.Hour13   -2.4673715   2.806089
#Post.Hour14   -2.0224101   4.824130
#Post.Hour15   -3.7399677   2.384740
#Post.Hour16           NA 278.770854
#Post.Hour17   -4.0445920   3.193767
#Post.Hour18   -5.6887029   1.321109
#Post.Hour19           NA 278.770854
#Post.Hour20 -287.5921858         NA
#Post.Hour22 -217.4115019         NA
#Post.Hour23 -217.4115019         NA


# Part 4

# Question 1
# We want to change these variables to categorical because you cannot 
# complete a logistics regression model with continuous variables
facebook$Post.Hour = factor(facebook$Post.Hour)
facebook$Post.Month = factor(facebook$Post.Month)
facebook$Post.Weekday = factor(facebook$Post.Weekday)

# Here we randomize the data
facebook=facebook[sample(1:nrow(facebook)),]

# Question 2
# multiple logistic regression 
multipleLModel = glm(comment~Paid + Type + Category + Post.Month + Post.Weekday + 
               Post.Hour + Page.total.likes, family=binomial(link='logit'), 
             data=facebook)
summary(multipleLModel)
# Result
#Deviance Residuals: 
# Min       1Q   Median       3Q      Max  
#-2.6621   0.1775   0.4110   0.6698   2.0010  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept)          5.321e-02  8.150e+00   0.007  0.99479   
#Paid                 5.506e-01  3.184e-01   1.729  0.08383 . 
#TypePhoto            3.051e-01  5.550e-01   0.550  0.58252   
#TypeStatus           1.390e+00  7.921e-01   1.755  0.07934 . 
#TypeVideo            1.572e+01  8.539e+02   0.018  0.98531   
#CategoryInspiration  1.008e+00  3.565e-01   2.828  0.00468 **
#  CategoryProduct      5.828e-01  3.715e-01   1.569  0.11672   
#Post.Month2          9.304e-01  1.103e+00   0.843  0.39896   
#Post.Month3         -6.562e-01  1.544e+00  -0.425  0.67085   
#Post.Month4          5.253e-01  2.436e+00   0.216  0.82929   
#Post.Month5          1.150e+00  3.139e+00   0.366  0.71417   
#Post.Month6         -2.355e-02  3.780e+00  -0.006  0.99503   
#Post.Month7          8.356e-01  4.221e+00   0.198  0.84308   
#Post.Month8          1.015e+00  4.487e+00   0.226  0.82106   
#Post.Month9          4.917e-01  4.734e+00   0.104  0.91726   
#Post.Month10        -3.448e-01  4.836e+00  -0.071  0.94316   
#Post.Month11         9.752e-01  4.973e+00   0.196  0.84454   
#Post.Month12        -5.007e-01  5.056e+00  -0.099  0.92113   
#Post.Weekday2        7.257e-02  4.982e-01   0.146  0.88418   
#Post.Weekday3       -1.136e-01  4.997e-01  -0.227  0.82020   
#Post.Weekday4        6.471e-01  5.282e-01   1.225  0.22056   
#Post.Weekday5       -8.120e-02  4.872e-01  -0.167  0.86762   
#Post.Weekday6        3.826e-01  4.976e-01   0.769  0.44197   
#Post.Weekday7       -5.351e-01  4.557e-01  -1.174  0.24028   
#Post.Hour2           1.574e+00  1.368e+00   1.150  0.25001   
#Post.Hour3           1.178e+00  1.265e+00   0.931  0.35193   
#Post.Hour4           6.372e-01  1.306e+00   0.488  0.62573   
#Post.Hour5           1.133e+00  1.496e+00   0.757  0.44887   
#Post.Hour6           2.212e+00  1.640e+00   1.349  0.17745   
#Post.Hour7          -4.618e-01  1.380e+00  -0.335  0.73793   
#Post.Hour8          -1.531e-01  1.389e+00  -0.110  0.91218   
#Post.Hour9           6.025e-01  1.317e+00   0.457  0.64732   
#Post.Hour10          1.176e+00  1.275e+00   0.922  0.35630   
#Post.Hour11          1.001e-01  1.278e+00   0.078  0.93755   
#Post.Hour12         -6.191e-01  1.308e+00  -0.473  0.63606   
#Post.Hour13          8.700e-01  1.305e+00   0.667  0.50490   
#Post.Hour14          2.208e+00  1.707e+00   1.294  0.19581   
#Post.Hour15          1.158e+00  1.653e+00   0.701  0.48337   
#Post.Hour16         -1.841e+01  2.400e+03  -0.008  0.99388   
#Post.Hour17          1.470e+00  1.850e+00   0.795  0.42669   
#Post.Hour18          3.051e-01  1.854e+00   0.165  0.86926   
#Post.Hour19         -1.573e+01  2.400e+03  -0.007  0.99477   
#Post.Hour20          1.487e+01  2.400e+03   0.006  0.99506   
#Post.Hour22          1.569e+01  2.400e+03   0.007  0.99478   
#Post.Hour23          1.498e+01  2.400e+03   0.006  0.99502   
#Page.total.likes    -5.242e-06  9.389e-05  -0.056  0.95547   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 516.59  on 499  degrees of freedom
#Residual deviance: 413.87  on 454  degrees of freedom
#AIC: 505.87

#Number of Fisher Scoring iterations: 15

# Question 3 
anova(multipleLModel, test="Chisq")
# Result
#Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#NULL                               499     516.59              
#Paid              1    5.909       498     510.69  0.015064 *  
#Type              3    7.921       495     502.76  0.047667 *  
#Category          2   20.383       493     482.38 3.748e-05 ***
#Post.Month       11   26.431       482     455.95  0.005596 ** 
#Post.Weekday      6    5.978       476     449.97  0.425673    
#Post.Hour        21   36.096       455     413.88  0.021328 *  
#Page.total.likes  1    0.003       454     413.87  0.955476    

# Part 5
# Question 1
Step.model = step(multipleLModel, direction = "backward")
# Final Result
#Step:  AIC=491.95
#comment ~ Paid + Type + Category + Post.Month
#
#Df Deviance    AIC
#<none>            455.95 491.95
#- Type        3   462.87 492.87
#- Paid        1   460.10 494.10
#- Post.Month 11   482.38 496.38
#- Category    2   468.45 500.45


