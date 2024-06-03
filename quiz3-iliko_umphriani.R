df = read.csv("fl_student_survey.csv",header=TRUE,sep=",",dec=".") 


# 1
miu = 5
xbar = mean(df$sports)
s = sd(df$sports)
n = nrow(df)
t = (xbar - miu) / (s / sqrt(n))
t
# 2) 0.36


# 2
n = nrow(df)
phat = nrow(df[df$political_affiliation == "d",]) / n
p = 0.3
z = (phat - p) / sqrt((phat*(1-p)) / n)
1 - pnorm(z)
# 1) 0.2


# 3
miu = 5
xbar = mean(df$TV)
s = sd(df$TV)
n = nrow(df)

t = (xbar - miu) / (s / sqrt(n))
pt(-abs(t), n-1)
# 3) 0.006


# 4
n = nrow(df)
phat = nrow(df[df$life_after_death == "y",]) / n
p = 0.4
z = (phat - p) / sqrt((phat*(1-p)) / n)
1 - pnorm(z)
# 3) 0.05


# 5
alpha = 0.07
n = nrow(df)
phat = nrow(df[df$life_after_death == "y",]) / n
p = 0.4
z = (phat - p) / sqrt((phat*(1-p)) / n)
pval = 2*pnorm(-abs(z))
ifelse(pval < alpha, "უარვყოფთ", "უარყოფის საფუძველი არ გაგვაჩნია")
