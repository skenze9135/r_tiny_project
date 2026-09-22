library(knitr)
library(DiagrammeR) 

base_dir <- getwd()
student_path <- file.path(base_dir, "project_data", "students.csv")
if (!file.exists(student_path)) stop("Student data file not found: ", student_path)

STUDENT_FILE <- read.csv(student_path)

kable(STUDENT_FILE, caption = "Student Dataset View")
str(STUDENT_FILE) 
kable(summary(STUDENT_FILE), caption = "Statistical Summary")

par(mfrow = c(2, 3)) 

total_gender <- aggregate(Marks ~ Gender, data = STUDENT_FILE, FUN = sum)
barplot(total_gender$Marks, 
        names.arg = total_gender$Gender, 
        xlab = "Gender", 
        ylab = "Total Marks", 
        main = "Marks by Gender",
        col = c("pink", "lightblue"))

total_status <- aggregate(Marks ~ Status, data = STUDENT_FILE, FUN = sum)
barplot(total_status$Marks, 
        names.arg = total_status$Status, 
        xlab = "Status", 
        ylab = "Total Marks", 
        main = "Marks by Status",
        col = c("green", "red"))

total_course <- aggregate(Marks ~ Course, data = STUDENT_FILE, FUN = sum)
barplot(total_course$Marks, 
        names.arg = total_course$Course, 
        xlab = "Course", 
        ylab = "Total Marks", 
        main = "Marks by Course",
        col = c("red", "green", "blue"))

counts <- table(STUDENT_FILE$Gender, STUDENT_FILE$Course)
barplot(counts, 
        beside = FALSE,
        xlab = "Course", 
        ylab = "Number of Students", 
        main = "Course by Gender",
        col = c("red", "green"),
        legend.text = rownames(counts))

course_counts <- table(STUDENT_FILE$Course)
pie(course_counts, 
    labels = paste(names(course_counts), "\n", course_counts), 
    main = "Student Count by Course", 
    col = c("red", "green", "blue"))

hist(STUDENT_FILE$Attendance, 
     xlab = "Attendance (%)", 
     ylab = "Frequency", 
     main = "Distribution of Attendance", 
     col = "gold", 
     breaks = 6)

par(mfrow = c(1, 1))