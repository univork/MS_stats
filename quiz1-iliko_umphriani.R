# parking lots
df_parking = read.csv("Ch03_Cars amotsana 28.csv", header = TRUE)

origin.levels = unique(df_parking$Origin)
origin.factors = factor(x=df_parking$Origin, levels=origin.levels, ordered=TRUE)

driver.levels = unique(df_parking$Driver)
driver.factors = factor(x=df_parking$Driver, levels=driver.levels, ordered=TRUE)

tdf = table(origin.factors, driver.factors)

barplot(
  prop.table(t(tdf), 2), 
  col=c("red", "green"), 
  legend.text = c("Student", "Staff"),
  args.legend = list(x="topright", cex=.8)
)



# tattoos
df_tattoos = read.csv("Ch03_Tattoos  amotsana 36.csv", header=TRUE)

location.levels = unique(df_tattoos$Location)
location.factors = factor(x=df_tattoos$Location,levels=location.levels, ordered=TRUE)

hepatitis.levels = unique(df_tattoos$Has.hepatitis.C)
hepatitis.factors = factor(x=df_tattoos$Has.hepatitis.C, levels=hepatitis.levels)

tdf_tattoo = table(hepatitis.factors, location.factors)

barplot(
  prop.table(t(tdf_tattoo), 2), 
  col=c("red", "green", "blue"), 
  legend.text = unique(df_tattoos$Location),
  args.legend = list(x="topright", cex=.8)
)



# emails
install.packages("readxl")
library("readxl")

df_emails = read_excel("Ch04_E-mails.xls")
hist(df_emails$`Number of e-mails`, xlab = "# of Emails", ylab = "# of Students", breaks = 0:max(df_emails$`Number of e-mails`))



# bird species
df_birds = read_excel("Ch04_Bird_species.xls", col_names = c("n"))

vals = hist(df_birds$n, breaks=seq(min(df_birds$n), max(df_birds$n), length.out=5))
text(
  vals$mids,                                      # Add values of histogram on top of bars
  vals$counts,
  labels = vals$counts,
  adj = c(0, -.5)
)
stem(df_birds$n)


# horsepower
df_horse= read_excel("Ch04_Horsepower.xls")

vals = hist(
  df_horse$Horsepower, 
  breaks=seq(min(df_horse$Horsepower), max(df_horse$Horsepower), length.out=10), 
  xlim = c(50, 170), 
  ylim=c(0, 10))
text(
  vals$mids,                                      # Add values of histogram on top of bars
  vals$counts,
  labels = vals$counts,
  adj = c(0, -.5)
)
stem(df_horse$Horsepower)



# acid rain
df_acid = read_excel("Ch04_Acid_rain.xls")

stem(df_acid$`pH Level`)
hist(df_acid$`pH Level`, breaks=seq(min(df_acid$`pH Level`), max(df_acid$`pH Level`), length.out=10))



# marijuana
df_marijuana = read_excel("Ch04_Marijuana.xls")
hist(x=df_marijuana$Percent, )



# hurricanes
df_hurricanes = read_excel("Ch04_Hurricanes.xls")
df_hurricanes$year = 1944:1997
plot(df_hurricanes)


# math score
df_math = read_excel("Ch04_Math_scores.xls")

hist(df_math$`Math Scores`, xlab="Score", ylab="# of States", breaks = 15)

