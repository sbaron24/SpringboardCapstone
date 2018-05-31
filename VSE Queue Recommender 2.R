---
  title: "VSE Queue Recommender 2"
author: "Sean Baron"
date: "5/14/2018"
output: html_document
---
  

# Load packages
library(dplyr)
library(ggplot2)

#----------Getting and Cleaning the Data-----------

# load Queue Status table
queue.status <- read.csv("Queue_Status_3.csv") 

# Load Conference Data
conferences <- read.csv("Conferences (1).csv")

# For the tables to be binded, the columns have to be the same. So here I'm adding the missing column for the queue table.
queue.status <- queue.status %>%
  mutate(Is.Callback = as.factor("Yes"))

# Add missing columns for conferences AND take only conferences that are NOT callback conferences, since callback conferences are included in the queues table.
conferences.nq <- conferences %>% filter(Is.Callback == "No") %>% 
  mutate(Callback.Status = NA, Date.Added.to.Queue.Full = NA, Disposition.Date.Full = NA, Disposition.Reason = NA, 
         Disposition.Activity = NA, Time.to.Conference..minutes. = NA)

# Combine Queue and Conference data by row binding.
conference.queues <- bind_rows(queue.status, conferences.nq)

# Change date columns from type factor to type POSIXct.
conference.queues$Date.Added.to.Queue.Full <- as.POSIXct(strptime(conference.queues$Date.Added.to.Queue.Full, format = "%Y/%m/%d %H:%M:%S"))
conference.queues$Conference.Start.Date.Full <- as.POSIXct(strptime(conference.queues$Conference.Start.Date.Full, format = "%Y/%m/%d %H:%M:%S"))
conference.queues$Conference.End.Date.Full <- as.POSIXct(strptime(conference.queues$Conference.End.Date.Full, format = "%Y/%m/%d %H:%M:%S"))
conference.queues$Disposition.Date.Full <- as.POSIXct(strptime(conference.queues$Disposition.Date.Full, format = "%Y/%m/%d %H:%M:%S"))

# Take the difference between conference end and start dates and put output in new column measuring total time spent in conferences
conference.queues$Conference.Length.minutes <- as.numeric(round(difftime(conference.queues$Conference.End.Date.Full, conference.queues$Conference.Start.Date.Full, units = "mins"), 1))

# Rename a Time to Conference column to something cleaner
conference.queues <- conference.queues %>% 
  rename(Minutes.Until.Conference = Time.to.Conference..minutes.)

# Add Conference.Status column for plotting later on 'Complete' and 'Incomplete' conferences
conference.queues <- conference.queues %>% 
  mutate(Conference.Status = ifelse(!is.na(Conference.ID), "Complete", "Incomplete"))

# Table that displays counts and percentages for Queued/Non-queued and Complete/Incomplete Conferences 
# Absolute counts
table(conference.queues$Is.Callback, conference.queues$Conference.Status)

# Percentages
round((table(conference.queues$Is.Callback, conference.queues$Conference.Status)/nrow(conference.queues))*100, 1)


# During the EDA process, I discovered a member ID that had a number of queues entered for a month many standard deviations above the mean for members and yet had no completed conferences in that time. I suspected that it might be a bot clogging up the phone service but confirmed that it is a test member that never got deleted.

# The dataframe below displays summaries of number of queues entered and conferences completed by member, and sorts by calls completed in increasinng order and then by queues entered in desceasing order to isolate the outlier member ID.
conference.queues %>% 
  group_by(Membership.ID) %>% summarise(Queues_Entered = sum(Is.Callback == "Yes"), Calls_Completed = sum(Conference.Status == "Complete")) %>% 
  select(Membership.ID, Queues_Entered, Calls_Completed) %>% 
  arrange(Calls_Completed, desc(Queues_Entered))

# Remove outlier Membership.ID == 84073 - fake member created by service
conference.queues <- conference.queues %>% 
  filter(!(Membership.ID == 84073))

#---------Data Visualization and Statistics----------

# The following graph displays Conference.Status and Is.Callback on on graph. The goal is to visualize on one graph the portion of calls that are completed vs. incompleted, and of those which are initiated from a queue and which are not.

# Store the perecentage of calls that are incomplete to put into barplot 
percent_queue_incomplete <- conference.queues %>% summarise(total_queued = sum(Is.Callback == "Yes"), queue_incomplete = sum(Is.Callback == "Yes" & Conference.Status == "Incomplete"), percentage_queue_incomplete = queue_incomplete/total_queued)
percent_queue_incomplete <- percent_queue_incomplete$percentage_queue_incomplete
percent_queue_incomplete_chr <- paste(round(percent_queue_incomplete, 3)*100, sep = "", "%")

# Barplot consisting of Conference.Status (Complete vs. Incomplete) with fill as Is.Callback (conference from queue or not)
g <- ggplot(conference.queues, aes(x = Conference.Status, fill = Is.Callback)) + 
  geom_bar(position = "stack") + ggtitle("Comparison of Successful Conferences and Queue Usage")

# Plot barplot with theme and labels
g + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Conference Success") + theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  theme(axis.title.y = element_text(margin = margin(t = , r = 15, b = 0, l = 0))) + 
  guides(fill=guide_legend(title="Queue Entry")) + 
  geom_text(x = 2, y = 10000, label = as.character(percent_queue_incomplete_chr))

#----------Relating Queue Usage to 2 Measures of Performance: Number of Completed Conferences and Number of Minutes Chatting with Advisors  

# Create 'count' dataframe with # queues entered, # direct calls made, time between first and last queue entry (for perspective), and total chat time across all conferences.
queue.conf.counts <- conference.queues %>% 
group_by(Membership.ID) %>% 
summarise(Number.Queues.Entered = sum(Is.Callback == "Yes"), Number.Completed.Conferences = sum(Conference.Status ==
"Complete"), date_first_q = min(Date.Added.to.Queue.Full, na.rm = TRUE), date_last_q = max(Date.Added.to.Queue.Full, na.rm
= TRUE), day_diff = round(difftime(date_last_q, date_first_q, units = "days"), 3), total_chat_time =
round(sum(Conference.Length.minutes, na.rm = TRUE), 2))

# Preview 
head(queue.conf.counts)

# Plot Number of Queues Entered and Conferences Completed
ggplot(queue.conf.counts, aes(x = Number.Queues.Entered, y = Number.Completed.Conferences)) + 
geom_point(alpha = 0.3, size = 1) + 
stat_smooth(method = "lm", se = FALSE) + 
ggtitle("Number of Queues Entered vs. Number of Completed Conferences by Member \n (March 30th - April 30th, 2018)") + theme(plot.title = element_text(hjust = 0.5)) + 
xlab("Number of Queues Entered by Member") + 
ylab("Number of Total Conferences by Members") + 
theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) + 
theme(axis.title.y = element_text(margin = margin(t = , r = 15, b = 0, l = 0))) + 
geom_text(x = 100, y = 225, label = paste("r = ", as.character(round(cor(queue.conf.counts$Number.Queues.Entered, queue.conf.counts$Number.Completed.Conferences), 2))))

# Plot Number of Queues Entered and Number of Minutes Chatting with Experts
ggplot(queue.conf.counts, aes(x = Number.Queues.Entered, y = total_chat_time)) + 
geom_point(alpha = 0.3, size = 1) + 
stat_smooth(method = "lm", se = FALSE) + 
ggtitle("Number of Queues Entered vs. Total Chat Time by Member \n (March 30th - April 30th, 2018)") + theme(plot.title = element_text(hjust = 0.5)) + 
xlab("Number of Queues Entered by Member") + 
ylab("Total Chat Time by Members (Minutes)") + 
theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) + 
theme(axis.title.y = element_text(margin = margin(t = , r = 15, b = 0, l = 0))) + 
geom_text(x = 100, y = 1750, label = paste("r = ", as.character(round(cor(queue.conf.counts$Number.Queues.Entered, queue.conf.counts$total_chat_time), 2))))

#--------Building a Recommender---------------

# Load ratings data
ratings <- read.csv("ratings.csv")

# 10% of conferences have ratings (in last four months)
ratings %>% 
  summarise(total_num_ratings = sum(!is.na(Conference.Star.Rating)), total_num_conferences = n(), confs_with_ratings = total_num_ratings/total_num_conferences)

# 8% of experts have ratings (in last four months)
ratings %>% group_by(Expert.ID) %>% 
  summarise(expert_rating_count = sum(!is.na(Conference.Star.Rating)), expert_conference_count = n()) %>% 
  summarise(experts_with_ratings = sum(expert_rating_count==0), total_experts = n(), percentage_experts_with_ratings = experts_with_ratings/total_experts)


#------Creating a Rating Metric------

# Select members that have had >=20 conferences
multimembers <- ratings %>% group_by(Membership.ID) %>% 
  count() %>% 
  filter(n >= 20)

# Create vector of multimembers' member IDs
multimembers <- multimembers$Membership.ID

# Store all conferences from ratings for multimembers in multimember.df
multimember.df <- ratings %>% 
  select(Membership.ID, Expert.ID, Conference.Duration) %>% 
  filter(Membership.ID %in% multimembers)

# Here's an example distribution of conference durations for a member
member.example <- multimember.df %>% filter(Membership.ID == 652592)
ggplot(member.example, aes(x = Conference.Duration)) + geom_density() + 
  ggtitle("Membership ID '652592' Conference Duration Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Conference Duration (minutes)") + 
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) + 
  theme(axis.title.y = element_text(margin = margin(t = , r = 15, b = 0, l = 0)))

# DF of members and total number of conferences they've had - to be left joined to dataframe below for calculating expert frequency metric part of rating score
MultiMember.Total.Conferences <- multimember.df %>% 
  group_by(Membership.ID) %>% 
  count() %>% 
  rename(Member.Total.Conf.Count = n)

# Data frame grouped by Membership.ID that contains z-score sums, expert chat frequency, and final rating score 
multimember.df <- multimember.df %>% 
  group_by(Membership.ID) %>% 
  mutate(Z_score = (Conference.Duration - mean(Conference.Duration))/sd(Conference.Duration)) %>% 
  ungroup() %>% group_by(Membership.ID, Expert.ID) %>% 
  summarise(Z.score.sum = sum(Z_score), Member.Expert.Conf.Count = n()) %>% left_join(MultiMember.Total.Conferences, by = "Membership.ID") %>%
  mutate(Expert.as.Percent.of.Member.Total.Calls = Member.Expert.Conf.Count/Member.Total.Conf.Count, Member.Expert.Rating = Z.score.sum*Expert.as.Percent.of.Member.Total.Calls)

# Store unique member-expert rating scores in data frame My.Rating
My.Ratings <- multimember.df %>% 
  select(Membership.ID, Expert.ID, Member.Expert.Rating)

# Get quantile summary of My.Rating rating scores
summary(My.Ratings$Member.Expert.Rating)

# Plot distribution to show normality of rating scores
ggplot(My.Ratings, aes(x = Member.Expert.Rating)) + 
  geom_density()


# Show that majority of experts are well represented in conferences of 'multimembers' (those with >=20 conferences)
# Get the number of unique experts in the original ratings data from the past 4 months
all.experts.original <- ratings %>% group_by(Expert.ID) %>% select(Expert.ID) %>%  unique

# 587 unique experts are found
all.experts.original$Expert.ID %>% length

# Get the number of unique experts in the new multimember ratings data frame from the past 4 months
all.experts.multi <- My.Ratings %>% group_by(Expert.ID) %>% select(Expert.ID) %>% unique

# 568 unique experts are found
all.experts.multi$Expert.ID %>% length

# So 568 of the 587 experts in all conference data from past 4 months will be able to be recommended by the recommender. 


#------Constructing A Similarity Matrix----------

# Store unique expert IDs in vector
all.experts.multi <- all.experts.multi$Expert.ID

# Create a member-expert ratings matrix initalized with zeros, where rows are unique members and columns are unique experts from My.Ratings. 
memex.ratings.mat <- matrix(data = 0, nrow = length(multimembers), ncol = length(all.experts.multi), dimnames = list(multimembers, all.experts.multi))

# Get member-expert ratings My.Ratings dataframe and fill member-expert ratings matrix
for (row in 1:nrow(My.Ratings)){
  temp_row <- My.Ratings %>% 
    filter(row_number() == row)
  temp_mem <- as.character(temp_row[[1]])
  temp_ex <- as.character(temp_row[[2]])
  temp_rating <- temp_row[[3]]
  memex.ratings.mat[temp_mem, temp_ex] <- temp_rating
}

# Create a helper function to calculate the cosine between two vectors
getCosine <- function(x,y) 
{
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

# Create a expert-expert simiarlity matrix with proper with dimensions 568 x 568
expert.similarity  <- matrix(NA, nrow=ncol(memex.ratings.mat),ncol=ncol(memex.ratings.mat),dimnames=list(colnames(memex.ratings.mat),colnames(memex.ratings.mat)))

# Calculate cosine similarities looping through the columns (experts) of the member-expert matrix and place similarity in corresponding expert.similarity row/column
for(i in 1:ncol(memex.ratings.mat)){
  # Loop through the columns for each column
  for(j in 1:ncol(memex.ratings.mat)){
    # Fill in placeholder with cosine similarities
    expert.similarity[i,j] <- getCosine(as.matrix(memex.ratings.mat[,i]),as.matrix(memex.ratings.mat[,j]))
  }
}

# Convert back to dataframe
expert.similarity <- as.data.frame(expert.similarity)

# Create a top 10 nearest neighbors matrix for each expert, where each expert is a row and each ith column is the ith most similar expert
expert.neighbours <- matrix(NA, nrow=ncol(expert.similarity),ncol=10,dimnames=list(colnames(expert.similarity)))

# Populate the top 10 nearest neightbors matrix by ordering and reindexing each experts column in the similarity matrix and filling that experts row in the nearest neighbors matrix
for(i in 1:ncol(memex.ratings.mat)) 
{
  expert.neighbours[i,] <- head(colnames(expert.similarity[order(expert.similarity[,i], decreasing = TRUE)]), n = 10)
}

# Final top 10 nearest neighbors matrix
head(expert.neighbours)

