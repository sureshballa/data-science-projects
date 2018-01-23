## Business Objectives
## The aim of analysis is to identify the root cause of the problem (i.e. cancellation and non-availability of cars) 
## and recommend ways to improve the situation. As a result of your analysis, you should be able to present 
## to the client the root cause(s) and possible hypotheses of the problem(s) and recommend ways to improve them.

## Instruction to reviewer: Please make sure to set current working directory where "Uber Request Data.csv" is present.

install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)

uberRequestData <- read.csv("Uber Request Data.csv", stringsAsFactors = F)

## Initial inspections
head(uberRequestData)
summary(uberRequestData)
nrow(uberRequestData)
nrow(distinct(uberRequestData))

uberRequestData$Pickup.point <- as.factor(uberRequestData$Pickup.point)
uberRequestData$Status <- as.factor(uberRequestData$Status)

## Reusable function to convert timestamp in string format to POSIXct format, supports two format as present in uber request data
getDateTime <- function(timestamp) {
  date_time <- as.POSIXct(timestamp, format = "%d-%m-%Y %H:%M:%S")
  if (is.na(date_time))
  {
    date_time <- as.POSIXct(timestamp, format = "%d/%m/%Y %H:%M")
  }
  date_time
}

## Reusable function to have time slots depending on hour
getTimeSlot <- function(hour) {
  cut(hour, breaks = c(0,4,8,12,13,15,18,21,24), labels = c("night", "early morning", "late morning", "noon", "early afternoon", "late afternoon", "early evening", "late evening"), include.lowest = TRUE, right = FALSE)
}

uberRequestData$Request_date_time <- sapply(uberRequestData$Request.timestamp, getDateTime)
uberRequestData$Request_date_time <- as.POSIXct(uberRequestData$Request_date_time, origin="1970-01-01")
uberRequestData$Request_date <- format(uberRequestData$Request_date_time, "%d")
uberRequestData$Request_hour <- format(uberRequestData$Request_date_time, "%H")
uberRequestData$Request_time_slot <- sapply(as.numeric(uberRequestData$Request_hour), getTimeSlot)
uberRequestData$Request_day <- as.factor(weekdays(uberRequestData$Request_date_time))
uberRequestData$Request_month <- as.factor(months(uberRequestData$Request_date_time))
uberRequestData$Request_quarter <- as.factor(quarters(uberRequestData$Request_date_time))
## To understand if any difference betweek weekday and weekend
uberRequestData$week <- as.factor(ifelse(uberRequestData$Request_day %in% c("Saturday", "Sunday"), "weekend", "weekday"))
summary(uberRequestData$Request_day)
summary(uberRequestData$week)
## Above summary indicates that there is no data available for weekend, so we do not need to consider this dimension

uberRequestData$Drop_date_time <- sapply(uberRequestData$Drop.timestamp, getDateTime)
uberRequestData$Drop_date_time <- as.POSIXct(uberRequestData$Drop_date_time, origin="1970-01-01")
uberRequestData$Drop_date <- format(uberRequestData$Drop_date_time, "%d")
uberRequestData$Drop_hour <- format(uberRequestData$Drop_date_time, "%H")
uberRequestData$Drop_time_slot <- sapply(as.numeric(uberRequestData$Drop_hour), getTimeSlot)
uberRequestData$Drop_day <- as.factor(weekdays(uberRequestData$Drop_date_time))
uberRequestData$Drop_month <- as.factor(months(uberRequestData$Drop_date_time))
uberRequestData$Drop_quarter <- as.factor(quarters(uberRequestData$Drop_date_time))

## Deriving new dimension ride duration, mostly applicable only to completed trips. 
## Estimation duration if available would been help full for cancellations
uberRequestData$RideDuration <- uberRequestData$Drop_date_time - uberRequestData$Request_date_time

## Reusable function t clarify ride duration into three classes - short, medium and long
getRideDurationClass <- function(minutes) {
  cut(minutes, breaks = c(0, 60, 90, 120), labels = c("short", "medium", "long"))
}
uberRequestData$RideDurationClass <- sapply(uberRequestData$RideDuration, getRideDurationClass)

## Inespect structure for new derived coluns and make sure proper types
str(uberRequestData)

## Visually identify the most pressing problems for Uber. 
## Hint: Create plots to visualise the frequency of requests that get cancelled or show 'no cars available'; 
## identify the most problematic types of requests (city to airport / airport to city etc.) 
## and the time slots (early mornings, late evenings etc.) using plots

install.packages("ggplot2")
install.packages("ggthemes")
library(ggplot2)
library("ggthemes")

## Uber Requests By Statuses
ggplot(uberRequestData, aes(x=uberRequestData$Status)) +
  labs(x = "Trip Status", y = "Number of Requests") +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=0, vjust=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  geom_bar() + ggtitle("Uber Requests By Statuses") + 
  geom_text(aes(label = ..count.., y = ..count..), stat= "count", vjust = -0.3, position = position_dodge(width=0.9)) +
  scale_colour_tableau()
## Findings from above graph: Unfulfilled request are way more than fulfilled requests leading to huge supply demand gap
##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Uber Requests By Pickup Points
ggplot(uberRequestData, aes(x=uberRequestData$Pickup.point)) +
  labs(x = "Pickup Points", y = "Number of Requests") +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=0, vjust=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  geom_bar() + ggtitle("Uber Requests By Pickup Points") +
  geom_text(aes(label = ..count.., y = ..count..), stat= "count", vjust = -0.3, position = position_dodge(width=0.9)) +
  scale_colour_tableau()
## Findings from above graph: Slightly more demand from City pickup points compared to Airport pickup point
##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Uber Requests By Time Slots
ggplot(uberRequestData, aes(x=uberRequestData$Request_time_slot)) + 
  labs(x = "Time slots", y = "Number of Requests") +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=0, vjust=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  geom_bar() + ggtitle("Uber Requests By Time Slots") +
  geom_text(aes(label = ..count.., y = ..count..), stat= "count", vjust = -0.3, position = position_dodge(width=0.9)) +
  scale_colour_tableau()
## Findings from above graph: Clearly demand is more during mornings and evenings
##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Begin of Uber Requests By Time Slots and Days

## Labeller for below chart
uberRequestDataByDay <- uberRequestData %>% group_by(Request_day) %>% summarise(total_request = n())
day_names <- c(
  `Monday` = paste0("Monday (", filter(uberRequestDataByDay, uberRequestDataByDay$Request_day == "Monday")$total_request, ")"),
  `Tuesday` = paste0("Tuesday (", filter(uberRequestDataByDay, uberRequestDataByDay$Request_day == "Tuesday")$total_request, ")"),
  `Wednesday` = paste0("Wednesday (", filter(uberRequestDataByDay, uberRequestDataByDay$Request_day == "Wednesday")$total_request, ")"),
  `Thursday` = paste0("Thursday (", filter(uberRequestDataByDay, uberRequestDataByDay$Request_day == "Thursday")$total_request, ")"),
  `Friday` = paste0("Friday (", filter(uberRequestDataByDay, uberRequestDataByDay$Request_day == "Friday")$total_request, ")")
)

ggplot(uberRequestData, aes(x=uberRequestData$Request_time_slot)) + 
  labs(x = "Time slots", y = "Number of Requests") +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=0, vjust=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  geom_bar() + ggtitle("Uber Requests By Time Slots and Days") +
  geom_text(aes(label = ..count.., y = ..count..), stat= "count", vjust = -0.3, position = position_dodge(width=0.9)) +
  facet_wrap(~factor(uberRequestData$Request_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")), ncol = 2, labeller=as_labeller(day_names)) +
  scale_colour_tableau()
## Findings from above graph: Mondays and Fridays are more busy days than mid week days
##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## End of Uber Requests By Time Slots and Days

## Uber Requests By Hours
ggplot(uberRequestData, aes(x=uberRequestData$Request_hour)) +
  labs(x = "Hour of the day", y = "Number of Requests") +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=0, vjust=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  geom_bar() + ggtitle("Uber Requests By Hour") +
  geom_text(aes(label = ..count.., y = ..count..), stat= "count", vjust = -0.3, position = position_dodge(width=0.9)) +
  scale_colour_tableau()
## Findings from above graph: Mornings 5 am to 10 am and Evenings are 5 pm to 11 pm are buys hours
##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


## Request analyis by time slot and pick up point
ggplot(uberRequestData, aes(x = uberRequestData$Request_time_slot, fill = uberRequestData$Pickup.point )) + 
  labs(x = "Time slots", y = "Number of Requests", fill = "Pickup Points") +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=0, vjust=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  geom_bar(alpha = 0.8, position = position_dodge(width = 0.8)) +
  geom_text(aes(label = ..count.., y = ..count..), stat= "count", vjust = -0.3, position = position_dodge(width=0.9)) +
  ggtitle("Uber Requests By Time slots and Pick up points") +
  scale_colour_tableau()
## Findings from above graph: Most request to airport is from early and late mornings
## Findings from above graph: Most request from airport is from early and late evenings
## So could be ppl travelling to work morning and returning by evening (on same day or different day)
##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Request analyis by time slot and trip status
ggplot(uberRequestData, aes(x = uberRequestData$Request_time_slot, fill = uberRequestData$Status )) + 
  labs(x = "Time slots", y = "Number of Requests") +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=0, vjust=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  geom_bar(alpha = 0.8, position = position_dodge(width = 0.8)) +
  geom_text(aes(label = ..count.., y = ..count..), stat= "count", vjust = -0.3, position = position_dodge(width=0.9)) +
  ggtitle("Uber Requests By Time slots and Statuses") +
  scale_fill_manual("Status", values = c("Cancelled" = "#DD4E42", "No Cars Available" = "#F68E89", "Trip Completed" = "#358F5D")) +
  scale_colour_tableau()
## Findings from above graph: Most cancellations are happening in early and late mornings
## Findings from above graph: Most cars are not available in early and late evenings
##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Uber Requests By Time slots, Statuses, Pickup points and Days
ggplot(uberRequestData, aes(x = Request_time_slot, fill = Status )) + 
  labs(x = "Time slots", y = "Number of Requests") +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=0, vjust=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  geom_bar(alpha = 0.8, position = position_dodge(width = 0.65)) +
  geom_text(aes(label = ..count.., y = ..count..), stat= "count", vjust = -0.3, position = position_dodge(width=0.9), size = 3) +
  ggtitle("Uber Requests By Time slots, Statuses, Pickup points and Days") +
  scale_fill_manual("Status", values = c("Cancelled" = "#DD4E42", "No Cars Available" = "#F68E89", "Trip Completed" = "#358F5D")) +
  facet_grid(factor(uberRequestData$Request_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) ~ Pickup.point) +
  scale_colour_tableau()
## Findings from above graph: Most cars are not available in early and late evenings for airport pickup point
## Findings from above graph: Most cancellations are happening in early and late mornings for city pickup points
##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

cancelledUberRequestData <- subset(uberRequestData, uberRequestData$Status == "Cancelled")
noCarsUberRequestData <- subset(uberRequestData, uberRequestData$Status == "No Cars Available")

## Uber Cancelled Requests By Time slots and Pickup points
ggplot(cancelledUberRequestData, aes(x = cancelledUberRequestData$Request_time_slot, fill = cancelledUberRequestData$Pickup.point)) + 
  labs(x = "Time slots", y = "Number of cancelled requests", fill = "Pickup Points") +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=0, vjust=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  ggtitle("Uber Cancelled Requests By Time slots and Pickup points") +
  geom_bar(alpha = 0.8, position = position_dodge(width = 0.8)) +
  geom_text(aes(label = ..count.., y = ..count..), stat= "count", vjust = -0.3, position = position_dodge(width=0.9)) +
  scale_colour_tableau()
## Findings from above graph: Most cancellations are happening in early and late mornings for city pick up point
##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Uber Cars Unavailable By Time slots and Pickup points
ggplot(noCarsUberRequestData, aes(x = noCarsUberRequestData$Request_time_slot, fill = noCarsUberRequestData$Pickup.point)) + 
  labs(x = "Time slots", y = "Number of requests for which cars are not available", fill = "Pickup Points") +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=0, vjust=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  ggtitle("Uber Cars Unavailable By Time slots and Pickup points") +
  geom_bar(alpha = 0.8, position = position_dodge(width = 0.8)) +
  geom_text(aes(label = ..count.., y = ..count..), stat= "count", vjust = -0.3, position = position_dodge(width=0.9)) +
  scale_colour_tableau()
## Findings from above graph: Most cars are not available in in early and late evenings from airport pick up point
##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Find out the gap between supply and demand and show the same using plots.
## Find the time slots when the highest gap exists
## Find the types of requests (city-airport or airport-city) for which the
## gap is the most severe in the identified time slots

## Uber Requests Distribution By Time slots and Trip Statuses
ggplot(uberRequestData, aes(x = uberRequestData$Request_time_slot, fill = uberRequestData$Status )) + 
  labs(x = "Time slots", y = "Requests") +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=0, vjust=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  ggtitle("Uber Requests Distribution By Time slots and Trip Statuses") +
  geom_bar(position = "fill") + 
  scale_fill_manual("Status", values = c("Cancelled" = "#DD4E42", "No Cars Available" = "#F68E89", "Trip Completed" = "#358F5D"))
## Findings from above graph: highest gap percentage exists in night and early evenings
##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Begin of gap analysis by Time slots

uberRequestAggregatedDataByTimeSlots <- uberRequestData %>% group_by(Request_time_slot) %>%
  summarise(
    total_request = n(),
    cancelled_request = sum(Status == "Cancelled"),
    no_cars_available = sum(Status == "No Cars Available"),
    completed_trips = sum(Status == "Trip Completed")
  )

uberRequestAggregatedDataByTimeSlots$gap_in_number_of_requests = (uberRequestAggregatedDataByTimeSlots$cancelled_request + uberRequestAggregatedDataByTimeSlots$no_cars_available)
uberRequestAggregatedDataByTimeSlots$gap_in_percentage = (uberRequestAggregatedDataByTimeSlots$cancelled_request + uberRequestAggregatedDataByTimeSlots$no_cars_available)/uberRequestAggregatedDataByTimeSlots$total_request

ggplot(uberRequestAggregatedDataByTimeSlots, aes(x=Request_time_slot, y=gap_in_number_of_requests)) + 
  labs(x = "Time slots", y = "Gap in number of request unfulfilled") +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=0, vjust=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  ggtitle("Uber Supply Demand Gap By Time slots") +
  geom_bar(stat = "Identity", position = 'dodge') + 
  geom_text(aes(label = gap_in_number_of_requests), vjust = -0.3, position = position_dodge(width=0.9)) +
  scale_colour_tableau()
## Findings from above graph: Highest gaps in terms of failed request are more in evenings and then followed by mornings

## End of gap analysis by Time slots
##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Begin of gap analysis by Time slots and days

uberRequestAggregatedDataByDayAndTimeSlots <- uberRequestData %>% group_by(Request_day, Request_time_slot) %>%
  summarise(
    total_request = n(),
    cancelled_request = sum(Status == "Cancelled"),
    no_cars_available = sum(Status == "No Cars Available"),
    completed_trips = sum(Status == "Trip Completed")
  )

uberRequestAggregatedDataByDayAndTimeSlots$gap_in_number_of_requests = (uberRequestAggregatedDataByDayAndTimeSlots$cancelled_request + uberRequestAggregatedDataByDayAndTimeSlots$no_cars_available)
uberRequestAggregatedDataByDayAndTimeSlots$gap_in_percentage = (uberRequestAggregatedDataByDayAndTimeSlots$cancelled_request + uberRequestAggregatedDataByDayAndTimeSlots$no_cars_available)/uberRequestAggregatedDataByDayAndTimeSlots$total_request

uberRequestGapDataByDay <- uberRequestAggregatedDataByDayAndTimeSlots %>% group_by(Request_day) %>% summarise(total_gap = sum(gap_in_number_of_requests))
day_names_for_gaps <- c(
  `Monday` = paste0("Monday (", filter(uberRequestGapDataByDay, uberRequestGapDataByDay$Request_day == "Monday")$total_gap, ")"),
  `Tuesday` = paste0("Tuesday (", filter(uberRequestGapDataByDay, uberRequestGapDataByDay$Request_day == "Tuesday")$total_gap, ")"),
  `Wednesday` = paste0("Wednesday (", filter(uberRequestGapDataByDay, uberRequestGapDataByDay$Request_day == "Wednesday")$total_gap, ")"),
  `Thursday` = paste0("Thursday (", filter(uberRequestGapDataByDay, uberRequestGapDataByDay$Request_day == "Thursday")$total_gap, ")"),
  `Friday` = paste0("Friday (", filter(uberRequestGapDataByDay, uberRequestGapDataByDay$Request_day == "Friday")$total_gap, ")")
)

ggplot(uberRequestAggregatedDataByDayAndTimeSlots, aes(x=Request_time_slot, y=gap_in_number_of_requests)) + 
  labs(x = "Time slots", y = "Gap in number of request unfulfilled") +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=0, vjust=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  ggtitle("Uber Supply Demand Gap By Time slots and weekdays") +
  geom_bar(stat = "Identity", position = 'dodge') + 
  geom_text(aes(label = gap_in_number_of_requests), vjust = -0.3, position = position_dodge(width=0.9)) +
  facet_wrap(~factor(uberRequestAggregatedDataByDayAndTimeSlots$Request_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")), ncol = 2, labeller=as_labeller(day_names_for_gaps)) +
  scale_colour_tableau()
## Findings from above graph: Highest gaps in terms of failed request are more for thursdays and fridays

## End of gap analysis by Time slots
##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Begin of gap analysis by Time slots and Pickup points

uberRequestAggregatedDataByTimeSlotsAndPickupPoints <- uberRequestData %>% group_by(Request_time_slot, Pickup.point) %>%
  summarise(
    total_request = n(),
    cancelled_request = sum(Status == "Cancelled"),
    no_cars_available = sum(Status == "No Cars Available"),
    completed_trips = sum(Status == "Trip Completed")
  )

uberRequestAggregatedDataByTimeSlotsAndPickupPoints$gap_in_number_of_requests = (uberRequestAggregatedDataByTimeSlotsAndPickupPoints$cancelled_request + uberRequestAggregatedDataByTimeSlotsAndPickupPoints$no_cars_available)
uberRequestAggregatedDataByTimeSlotsAndPickupPoints$gap_in_percentage = (uberRequestAggregatedDataByTimeSlotsAndPickupPoints$cancelled_request + uberRequestAggregatedDataByTimeSlotsAndPickupPoints$no_cars_available)/uberRequestAggregatedDataByTimeSlotsAndPickupPoints$total_request

ggplot(uberRequestAggregatedDataByTimeSlotsAndPickupPoints, aes(x=Request_time_slot, y=gap_in_number_of_requests, fill = Pickup.point)) + 
  labs(x = "Time slots", y = "Gap in number of request unfulfilled", fill = "Pickup Point") +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=0, vjust=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  ggtitle("Uber Supply Demand Gap By Time slots and Pickup points") +
  geom_bar(stat = "Identity", alpha = 1, position = position_dodge(width = 0.8)) +
  geom_text(aes(label = gap_in_number_of_requests), vjust = -0.3, position = position_dodge(width=0.9)) +
  scale_colour_tableau() 
## Findings from above graph: Highest gaps in terms of failed request are more in evenings because of trips not being available from airport
## Findings from above graph: Second highest gaps in terms of failed request are in mornings because of trips not being available from city

## End of gap analysis by Time slots and Pickup points
##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Begin of gap analysis by all three dimensions Day, Time slot and Pickup point

uberRequestAggregatedData <- uberRequestData %>% group_by(Request_day, Request_time_slot, Pickup.point) %>%
  summarise(
    total_request = n(),
    cancelled_request = sum(Status == "Cancelled"),
    no_cars_available = sum(Status == "No Cars Available"),
    completed_trips = sum(Status == "Trip Completed")
  )

uberRequestAggregatedData$gap_in_number_of_requests = (uberRequestAggregatedData$cancelled_request + uberRequestAggregatedData$no_cars_available)
uberRequestAggregatedData$gap_in_percentage = (uberRequestAggregatedData$cancelled_request + uberRequestAggregatedData$no_cars_available)/uberRequestAggregatedData$total_request

ggplot(uberRequestAggregatedData, aes(x = Request_time_slot, y = gap_in_number_of_requests)) + 
  labs(x = "Time slots", y = "Gap in number of request unfulfilled") +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=0, vjust=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  geom_bar(stat = "Identity") +
  geom_text(aes(label = gap_in_number_of_requests), vjust = -0.3, position = position_dodge(width=0.9), size = 3) +
  ggtitle("Uber Supply Demand Gap By Time slots, Pickup points and weekdays") +
  facet_grid(factor(Request_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) ~ Pickup.point) +
  scale_colour_tableau()

## End of gap analysis by all three dimensions Day, Time slot and Pickup point
##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## What do you think is the reason for this issue for the supply-demand gap? 
## Write the answer in less than 100 words. You may accompany the write-up with plot(s).

## Recommend some ways to resolve the supply-demand gap.
## Recommendations in PDF submitted

## write.csv(uberRequestData, "uber_request_data_expanded.csv")