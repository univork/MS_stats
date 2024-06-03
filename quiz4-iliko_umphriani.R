df = read.csv("fl_student_survey.csv")

# 1
x1 = df$age[df$gender == 'm']
x2 = df$age[df$gender == 'f']

alpha = 0.05
n1 = length(x1)
n2 = length(x2)

t = (mean(x1)-mean(x2)) / sqrt(sd(x1) ^ 2/n1 + sd(x2) ^ 2 / n2)
sp = ((n1-1)*(sd(x1)^2) + (n2-1)*(sd(x2)^2)) / ((n1-1) + (n2-1))

(mean(x1) - mean(x2)) + t * sqrt(sp / n1 + sp / n2)

t.test(age ~ gender, data=df, conf.level=.90)


# 2
x1 = df$age[df$gender == 'm']
x2 = df$age[df$gender == 'f']
n1 = length(x1)
n2 = length(x2)

t = (mean(x1)-mean(x2)) / sqrt(sd(x1) ^ 2/n1 + sd(x2) ^ 2 / n2)
1 - pt(t, n1 + n2 - 2)
# 2) 0.26


# 3
x1 = df$high_sch_GPA[df$gender == 'm']
x2 = df$high_sch_GPA[df$gender == 'f']
n1 = length(df$high_sch_GPA[df$gender == 'm'])
n2 = length(df$high_sch_GPA[df$gender == 'f'])

sp = ((n1-1)*(sd(x1)^2) + (n2-1)*(sd(x2)^2)) / ((n1-1) + (n2-1))
t = (mean(x1)-mean(x2)) / sqrt(sp*((1/n1)+(1/n2)))
pnorm(t)
# 2) 0.16


# 4
x1 = df$abortion_legalize[df$gender == 'm']=='y'
x2 = df$abortion_legalize[df$gender == 'f']=='y'
n1 = length(df$abortion_legalize[df$gender == 'm']=='y')
n2 = length(df$abortion_legalize[df$gender == 'f']=='y')


pbar = (sum(x1) + sum(x2)) / (n1 + n2)
p1 = sum(x1)/n1
p2 = sum(x2)/n2
z = (p1-p2)/sqrt(pbar * (1-pbar) * (1/n1 + 1/n2))
pnorm(z)

t.test(x1, x2, alternative = "less")
# 3) 0.14

# 5
x1 = df$vegetarian[df$gender == 'm']=='y'
x2 = df$vegetarian[df$gender == 'f']=='y'
n1 = length(df$vegetarian[df$gender == 'm']=='y')
n2 = length(df$vegetarian[df$gender == 'f']=='y')

pbar = (sum(x1) + sum(x2)) / (n1 + n2)
p1 = sum(x1)/n1
p2 = sum(x2)/n2
z = (p1-p2)/sqrt(pbar * (1-pbar) * (1/n1 + 1/n2))
pnorm(z) * 2

t.test(x1, x2, alternative = "two.sided")
# 1) 0.33