library(tidyverse)

my_data <- read.csv("Dataset/IPEDS_data.csv") %>%
  select(2, 5, 7:8, 22:24, 27:30, 33, 34, 65, 70:73, 75, 78, 86, 111, 124, 126, 128, 131, 134)

my_data <- my_data %>% 
  rename(High.Degree = Highest.degree.offered,
         Longitude = Longitude.location.of.institution,
         Latitude = Latitude.location.of.institution,
         Applicants = Applicants.total,
         Admissions = Admissions.total,
         Enrolled = Enrolled.total,
         SAT.Reading.25 = SAT.Critical.Reading.25th.percentile.score,
         SAT.Reading.75 = SAT.Critical.Reading.75th.percentile.score,
         SAT.Math.25 = SAT.Math.25th.percentile.score,
         SAT.Math.75 = SAT.Math.75th.percentile.score,
         ACT.25 = ACT.Composite.25th.percentile.score,
         ACT.75 = ACT.Composite.75th.percentile.score,
         Percent.Admitted = Percent.admitted...total,
         Tuition = Tuition.and.fees..2013.14,
         Tuition.Instate = Total.price.for.in.state.students.living.on.campus.2013.14,
         Tuition.Outstate = Total.price.for.out.of.state.students.living.on.campus.2013.14,
         State = State.abbreviation,
         Region = Geographic.region,
         Control = Control.of.institution,
         TEnrollment = Undergraduate.enrollment,
         Percent.Women = Percent.of.undergraduate.enrollment.that.are.women,
         Pecent.Instate = Percent.of.first.time.undergraduates...in.state,
         Percent.Outstate = Percent.of.first.time.undergraduates...out.of.state,
         Percent.Foreign = Percent.of.first.time.undergraduates...foreign.countries,
         GradRate.4yr = Graduation.rate...Bachelor.degree.within.4.years..total,
         Percent.Finan.Aid = Percent.of.freshmen.receiving.any.financial.aid
         )

test <- filter(data, Region %in% c("New England", "Southeast"))

word(my_data$Region, 1)
# What to filter: Name, 
# filter(my_data, my_data$SAT.Reading.75 > 780 & my_data$Percent.Women > 40)
# filter(my_data, my_data$ACT.25 > 33 | my_data$SAT.Math.75 > 760 & my_data$Percent.Admitted > 20)
# filter(my_data, my_data$Pecent.Instate < 40, my_data$Percent.Foreign > 15)
# filter(my_data, my_data$Percent.Finan.Aid > 60, my_data$TEnrollemend < 1000, my_data$Control == "Public")
# 
# hist(my_data$Percent.Admitted)


#QR Code
# library(qrcode)
# png("qr.png")
# code <- qr_code("https://boiling-brook-84278.herokuapp.com/")
# plot(code, c("green", "blue"))
# dev.off()

# Cowsay
# library(cowsay)
# library(crayon)
# say("Using our " %+% bgYellow("eyes") %+% " to switch between different views that are " %+% bgCyan("visible") %+% " simultaneously has " %+% red$underline("much lower cognitive load") %+% " than consulting our memory to " %+% green$bold("compare") %+% " a current view with what was seen before. (Munzner and Maguire, 2015, p. 131)",
#     by = "turkey",
#     what_color = "black",
#     by_color = "brown")

# say(what = "fortune",
#     by = "yoda", 
#     what_color = "olivedrab",
#     by_color = "green")
# say("Q: What animal is best at hitting a baseball?\nA: A bat", by = "bat")
