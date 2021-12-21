food <- read.csv("favorite_food_data.csv", fileEncoding = "UTF-8-BOM")
student <- read.csv("student_data.csv", fileEncoding = "UTF-8-BOM")


# 1. Data Visualization

# a. Show the frequency of favourite food types by students.

vis1 <- table(food$Favorite.Food)

vis1.percent <- paste(round(vis1 * 100 / sum(vis1), 0), " % ", names(vis1), sep = "")

pie(vis1, main = "Food Type Appeal", col = rainbow(length(vis1)), labels = vis1.percent)



# b. Show the number of students by their education grade and education year.

vis2 <-  ifelse(student$Education.Grade =="SD",paste("SD",student$Education.Year),
                ifelse(student$Education.Grade == "SMP",paste("SMP",student$Education.Year),
                       ifelse(student$Education.Grade =="SMA",paste("SMA",student$Education.Year), student$Education.Grade)))

vis2 <- table(vis2)

barplot(vis2, col = rainbow(length(vis2)), main = "Number of Students By Grade",
        xlab = "Student Grade", ylab = "Student Count")



# c. Show the average height of all primary school (“SD”) students by their education year and the students’ age must less than equal to 12.

vis3 <- student[student$Age <= 12,]
vis3 <- vis3[vis3$Education.Grade == "SD",]

vis3 <- data.frame(vis3)

vis3 <- aggregate(vis3$Height, list(vis3$Education.Year), mean)

plot(vis3, type = "o", main = "Primary School Average Height",
     col = "red", xlab = "Education Year", ylab = "grades")





# 2. FPA

# Data Preprocessing

data_2 = merge(student, food, by.x = "Student", by.y = "Student.Name")

View(data_2)

data_apriori <- data_2
data_apriori <- data_apriori[!data_apriori$Student == "",]
data_apriori <- data_apriori[data_apriori$Education.Grade != "SD",]
data_apriori <- data_apriori[!duplicated(data_apriori),]


View(data_apriori)


# Data Transformation

trans <- split(data_apriori$Favorite.Food, data_apriori$Student)

# Data Mining
library(arules)

freq <- apriori(trans, parameter = list(support = 0.25, target = "frequent itemsets"))
inspect(freq)


# Association Rules

inspect(ruleInduction(freq, confidence = 0.6))





