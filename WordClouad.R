setwd("~/Documents/GitHub/CS324IndividualProject")

#libraries
library(tidyverse)
library(dplyr) #ranking frequency
library(grDevices) #png function
library(wordcloud) #wordcloud
library(RColorBrewer) #brewer colors

col_dat <- read.csv("IPEDS_data.csv") %>%
  select(2, 5, 7:8, 22:24, 27:30, 33, 34, 65, 70:73, 75, 78, 86, 111, 124, 126, 128, 131, 134)

col_dat <- col_dat %>% 
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

#scatterplot
#col_scat <- col_dat[order(col_dat$Percent.Admitted) %>% head(300),]
png(file = "scatterplot.png")
plot(col_scat$Tuition, col_scat$Percent.Finan.Aid,
     xlab="Tuition",
     ylab = "Financial Aid Rate",
     #xlim = c(12, 36),
     #ylim = c(1, 60),
     main = "Tuition vs Number of freshmen receiving financial aid")
dev.off()

#wordcloud
col_dat <- col_dat[col_dat$TEnrollment != "",]
col_table <- col_dat[order(-col_dat$TEnrollment) %>% head(40),]
col_table <- select(col_table, 1, 21)

png("wordcloud.png", width=25, height=18, units = 'in', res = 300)
par(mar = rep(0, 4))
set.seed(1337)
wordcloud(words = col_table$Name, freq = col_table$TEnrollment, scale = c(3.5, 0.25),
          max.words=200, colors=brewer.pal(8, "Dark2"))

