path="/Users/Srishti/Desktop/ISME/Trimester 3/R/Data/london_crime.csv"
df=read.csv(path)
head(df)
label=levels(unlist(df$major_category))
a=table(df$major_category)
pie(a, label,radius = 1, main = "City pie chart", col = rainbow(length(label)))

library(sqldf)
library(ggplot2)
b=sqldf("SELECT town, COUNT(town)as Count FROM df GROUP BY town") 
b
ggplot(b, aes(x=town, y=Count)) + geom_bar(stat="identity") +
  labs(x="Town", y="No of crimes per city")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

q1q2=table(df$major_category,df$town)
label=levels(unlist(df$major_category))
barplot (q1q2, main="Stacked Bar Plot",ylab="Crime",col=rainbow(length(label)),legends=rownames(q1q2),las=2)

#How do the sum of crimes vary by year?
c=sqldf("SELECT year as y, COUNT(*)as Count FROM df GROUP BY year") 
plot(c,type = "o",col = "blue", xlab = "Year", ylab = "Crimes", 
     main = "Crime chart",legends=c$y)

#How do the sum of crimes in each year vary by month?
d=sqldf("SELECT month as m, COUNT(*)as Count FROM df GROUP BY month") 
plot(d,type = "o",col = "black", xlab = "Month", ylab = "Crimes", 
     main = "Crime chart",legends=d$m)

#Which time of month is worst for each city?
temp=table(df$month,df$town)
barplot (temp, main="Stacked Bar Plot",ylab="Month",col=rainbow(12),legends=rownames(temp),las=2)

#Split in 2009
e=sqldf("SELECT major_category,COUNT(major_category) as Count FROM df WHERE year=2009 GROUP BY major_category")
ggplot(e, aes(x=major_category, y=Count)) + geom_bar(stat="identity") +
  labs(x="Crime Categories", y="Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Month vise crime split
f=table(df$major_category,df$month)
barplot (f, main="Stacked Bar Plot",ylab="Crime",col=rainbow(9),legends=colnames(f),las=2,horiz = TRUE)


t.test(df$month)


g=density(df$year)
plot(g,main='month')
polygon(d,col='red',border='blue')
rug(df$month, col='brown')


