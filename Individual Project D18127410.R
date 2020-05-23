rm(list=ls()) #clear workspace

#import datasets
Ratio_of_Girls_to_Boys_in_Education <- read.csv("Ratio of Girls to Boys in Education.csv")
Seats_held_by_Women_in_Parliament <- read.csv("Seats held by Women in Parliament.csv")

#variables3 and 4 aren't relevent for this analyis
head(Ratio_of_Girls_to_Boys_in_Education)
Ratio_of_Girls_to_Boys_in_Education = subset(Ratio_of_Girls_to_Boys_in_Education, select = -c(X.3, X.4) )
head(Ratio_of_Girls_to_Boys_in_Education)
tail(Ratio_of_Girls_to_Boys_in_Education)
str(Ratio_of_Girls_to_Boys_in_Education)
summary(Ratio_of_Girls_to_Boys_in_Education)
sum(is.na(Ratio_of_Girls_to_Boys_in_Education)) #no NA
Ratio_of_Girls_to_Boys_in_Education <- Ratio_of_Girls_to_Boys_in_Education[-c(1),]
head(Ratio_of_Girls_to_Boys_in_Education)
library(dplyr)
names(Ratio_of_Girls_to_Boys_in_Education)
names(Ratio_of_Girls_to_Boys_in_Education)[1] <- "Region_Code"
names(Ratio_of_Girls_to_Boys_in_Education)[3] <- "Year"
names(Ratio_of_Girls_to_Boys_in_Education)[4] <- "Level_of_Education"
names(Ratio_of_Girls_to_Boys_in_Education)[5] <- "Ratio"
names(Ratio_of_Girls_to_Boys_in_Education)[2] <- "Region_Name"
head(Ratio_of_Girls_to_Boys_in_Education)


Ratio_of_Girls_to_Boys_in_Education$Year <- as.numeric(as.character(Ratio_of_Girls_to_Boys_in_Education$Year))
Ratio_of_Girls_to_Boys_in_Education$Ratio <- as.numeric(as.character((Ratio_of_Girls_to_Boys_in_Education)$Ratio))
Ratio_of_Girls_to_Boys_in_Education$Region_Code<- as.numeric(as.character(Ratio_of_Girls_to_Boys_in_Education$Region_Code))
ExGraph1 <- lm(Year ~ Ratio, data = Ratio_of_Girls_to_Boys_in_Education)


Seats_held_by_Women_in_Parliament= subset(Seats_held_by_Women_in_Parliament, select = -c(X.3, X.5, X.6))
names(Seats_held_by_Women_in_Parliament)[1] <- "Region_Code"
names(Seats_held_by_Women_in_Parliament)[2] <- "Region_Name"
names(Seats_held_by_Women_in_Parliament)[3] <- "Year"
names(Seats_held_by_Women_in_Parliament)[6] <- "Percentage_Seats_Held"
Seats_held_by_Women_in_Parliament= subset(Seats_held_by_Women_in_Parliament, select = -c(X.1, X.2)) # deleted X2 as only 66 dates per 1861 obs, X.1 all the same and irrelevant
Seats_held_by_Women_in_Parliament$Region_Code <- as.numeric(as.character(Seats_held_by_Women_in_Parliament$Region_Code))
Seats_held_by_Women_in_Parliament$Year <- as.numeric(as.character(Seats_held_by_Women_in_Parliament$Year))
Seats_held_by_Women_in_Parliament$Percentage_Seats_Held <- as.numeric(as.character(Seats_held_by_Women_in_Parliament$Percentage_Seats_Held))
head(Seats_held_by_Women_in_Parliament)
sum(is.na(Seats_held_by_Women_in_Parliament)) #no NA values
Seats_held_by_Women_in_Parliament <- Seats_held_by_Women_in_Parliament[-c(1),]
library(ggplot2)
install.packages("plotly")
library(plotly)
head(Seats_held_by_Women_in_Parliaments, 80)

FullGlobalData <- merge(Seats_held_by_Women_in_Parliament, Ratio_of_Girls_to_Boys_in_Education)
summary(FullGlobalData)
str(FullGlobalData)
sum(FullGlobalData)
FullGlobalData$Region_Name <- as.character(as.factor(FullGlobalData$Region_Name))
Seats_By_Global_Region <- subset(Seats_held_by_Women_in_Parliament, Region_Code == 1 | Region_Code ==15| Region_Code ==202 | Region_Code ==21 | Region_Code ==419| Region_Code ==30| Region_Code ==35| Region_Code ==34| Region_Code ==145| Region_Code ==150| Region_Code ==9)

Seats_By_Global_Region_Region_Code <- as.numeric(as.character(Worldwide_Seats$P_Region_Code))
#line chart for seats held by region
ggplot(data=Seats_By_Global_Region, aes(x=Year, y=Percentage_Seats_Held, group=Region_Code, color = Region_Code) + geom_line()+geom_point())

#box plots for seats held by region
qplot(x=Year, y=P_Seats_Held, data=Worldwide_Seats, geom="boxplot", group=GlobalRegion, color = GlobalRegion)


#sparkline charts for global seats held by women
#by global region
GlobalRegionSeats <- ggplot(Seats_By_Global_Region, aes(x=Year, y=Percentage_Seats_Held, color=Percentage_Seats_Held)) +scale_colour_gradient(low = "red", high = "green")+ geom_line() + facet_grid( Region_Name ~ .) + labs(y = "% of seats held by women") + theme(strip.text.y = element_text(angle = 0))
GlobalRegionSeatsTrend<- ggplot(Seats_By_Global_Region, aes(x=Year, y=Percentage_Seats_Held, color=Percentage_Seats_Held)) +scale_colour_gradient(low = "red", high = "green")+ geom_line() + facet_grid( Region_Name ~ .) + labs(y = "% of seats held by women") + theme(strip.text.y = element_text(angle = 0)) + geom_smooth(method = "lm")
#by year
GlobalYearSeat<- ggplot(Seats_held_by_Women_in_Parliament, aes(x=Percentage_Seats_Held, y=Region_Name, color=Percentage_Seats_Held)) +scale_colour_gradient(low = "red", high = "green")+ geom_point() + facet_grid( Year ~ .) + theme(strip.text.y = element_text(angle = 0), axis.text.y=element_blank())
GlobalYearSeatTrend <- ggplot(Seats_held_by_Women_in_Parliament, aes(x=Percentage_Seats_Held, y=Region_Name, color=Percentage_Seats_Held)) + geom_smooth()+scale_colour_gradient(low = "red", high = "green")+ geom_point() + facet_grid( Year ~ .) + theme(strip.text.y = element_text(angle = 0), axis.text.y=element_blank())
l

#seats on a national level
Seats_National_Level <- subset(Seats_held_by_Women_in_Parliament, Region_Name == "Egypt" | Region_Name == "Rwanda" | Region_Name == "Bolivia" | Region_Name == "Cuba" | Region_Name == "Japan" | Region_Name == "Thailand" | Region_Name == "United Arab Emirates" | Region_Name == "India" | Region_Name == "United States of America" | Region_Name == "Australia" |Region_Name == "Ireland")
NationalSeatsChart <- ggplot(Seats_National_Level, aes(x=Year, y=Percentage_Seats_Held, color=Percentage_Seats_Held)) +scale_colour_gradient(low = "red", high = "green")+ geom_line() +geom_point()+ facet_grid( Region_Name ~ .) + labs(y = "% of seats held by women") + theme(strip.text.y = element_text(angle = 0)) + geom_hline(yintercept=50, linetype = "dashed",color = "black")

#seats in europe
SeatEurope <-  subset(Seats_held_by_Women_in_Parliament, Region_Name == "Europe")
SeatEUIndiv <-  subset(Seats_held_by_Women_in_Parliament, Region_Name == "Ireland" | Region_Name == "Belgium" | Region_Name =="France" | Region_Name == "Slovenia" | Region_Name == "Spain"  |Region_Name == "Poland"|  Region_Name == "Portugal" | Region_Name == "Romania")
EUSeatsChart <- ggplot(SeatEurope, aes(x=Year, y=Percentage_Seats_Held, color=Percentage_Seats_Held)) +scale_colour_gradient(low = "red", high = "green")+ geom_line() +geom_point()+ facet_grid( Region_Name ~ .) + labs(y = "% of seats held by women") + theme(strip.text.y = element_text(angle = 0)) 
EUIndivSeatsChart <- ggplot(SeatEUIndiv, aes(x=Year, y=Percentage_Seats_Held, color=Percentage_Seats_Held)) +scale_colour_gradient(low = "red", high = "green")+ geom_line() +geom_point()+ facet_grid( Region_Name ~ .) + labs(y = "% of seats held by women") + theme(strip.text.y = element_text(angle = 0)) + geom_hline(yintercept=40, linetype = "dashed",color = "black")

#sparkline charts for girls in education
Education_By_Global_Region <- subset(Ratio_of_Girls_to_Boys_in_Education, Region_Code == 1 | Region_Code ==15| Region_Code ==202 | Region_Code ==21 | Region_Code ==419| Region_Code ==30| Region_Code ==35| Region_Code ==34| Region_Code ==145| Region_Code ==150| Region_Code ==9)
#globally and by global region
GlobalRegionEd <- ggplot(Education_By_Global_Region, aes(x=Year, y=Ratio, group=Level_of_Education, color=Level_of_Education))  + facet_grid( Region_Name ~ .) + geom_line(position=position_dodge(0.2)) + geom_point(position=position_dodge(0.2), size=4)+scale_colour_brewer(palette="Set3")+labs(y = "Ratio of girls to boys in education - Overall") + theme(strip.text.y = element_text(angle = 0)) + geom_hline(yintercept=1, color = "red") 
#nationally
EducationRatiosNational <- subset(Ratio_of_Girls_to_Boys_in_Education, Region_Name == "Egypt" | Region_Name == "Rwanda" | Region_Name == "Bolivia" | Region_Name == "Cuba" | Region_Name == "Japan" | Region_Name == "Thailand" | Region_Name == "United Arab Emirates" | Region_Name == "India" | Region_Name == "United States of America" | Region_Name == "Australia" |Region_Name == "Ireland")
NationalEducationChart <- ggplot(EducationRatiosNational, aes(x=Year, y=Ratio, group=Level_of_Education, color=Level_of_Education)) + facet_grid( Region_Name ~ .) + geom_line(position=position_dodge(0.2)) + geom_point(position=position_dodge(0.2), size=4)+scale_colour_brewer(palette="Set3")+labs(y = "Ratio of girls to boys in education - Overall") + theme(strip.text.y = element_text(angle = 0)) + geom_hline(yintercept=1, color = "red") 

overallglobal <- merge(Seats_held_by_Women_in_Parliament, Ratio_of_Girls_to_Boys_in_Education)

overallglobalprimary <- subset(overallglobal, Level_of_Education == "Ratio of girls to boys in primary education") 
overallglobalseconday<- subset(overallglobal, Level_of_Education == "Ratio of girls to boys in secondary education") 
overallglobaltertiary <- subset(overallglobal, Level_of_Education == "Ratio of girls to boys in tertiary education") 
cor(overallglobal$Percentage_Seats_Held, overallglobal$Ratio) 
cor(overallglobalprimary$Percentage_Seats_Held, overallglobalprimary$Ratio)
cor(overallglobalseconday$Percentage_Seats_Held, overallglobalseconday$Ratio)
cor(overallglobaltertiary$Percentage_Seats_Held, overallglobaltertiary$Ratio)

plot(overallglobal$Percentage_Seats_Held, overallglobal$Ratio) 

linearModel <- lm(Percentage_Seats_Held ~ Ratio, data=overallglobal)  # lm(formula = y ~ x) (Predictor variable =y, response variable = x)
print(linearModel)
summary(linearModel)
plot( overallglobal$Ratio, overallglobal$Percentage_Seats_Held) + abline(linearModel) 

