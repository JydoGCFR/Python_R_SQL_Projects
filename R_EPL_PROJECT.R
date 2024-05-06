
############################################ JIDE AFOLABI ##############################################
################################ CUSTOM R FUNCTION FOR EPL DATA PROJECT ##########################################

library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)

jide <- EPL_Standings(date = "04/24/2024", season = "2023/24")

EPL_Standings <- function(date, season) {
        
        # 1 - use the season input to download the correct file
        if(season == "2021/22") {
                # download 2021/22 data
                epl_raw <- read_csv("https://www.football-data.co.uk/mmz4281/2122/E0.csv")
        } else if(season == "2022/23") {
                # download 2022/23 data
                epl_raw <- read_csv("https://www.football-data.co.uk/mmz4281/2223/E0.csv")
        } else { epl_raw <- read_csv("https://www.football-data.co.uk/mmz4281/2324/E0.csv")
                # download 2023/24 data
        }
        
        # 2 - Select only the necessary columns from the imported data
        epl_df <- epl_raw %>% 
            select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
        
        # 3 - Handle date formatting
                # 3a - convert input date to date format object
                input_date_clean <- mdy(date)
                
                # 3b - mutate: reformat the date in the raw data from download
                epl_df <- epl_df %>%
                        mutate(Date = dmy(Date))
                     
                # 3c - use input date to filter the raw data (<=) up to and including input date
                epl_df <- epl_df %>%
                    filter(Date <= input_date_clean)
                        
        # 4 - Convert raw data from match-level (rows) to team-level (rows)
               # rename columns so that you can have key variables for each team (home)
                home_df <- epl_df %>% 
                        select(TeamName = HomeTeam,
                               team_goals = FTHG,
                               opp_goals = FTAG,FTR) %>%
                        mutate(match_type = 'Home',
                               record = ifelse(team_goals - opp_goals > 0, "W",
                                               ifelse(opp_goals - team_goals > 0, "L", "D")),                    
                                points = ifelse(team_goals - opp_goals > 0, 3,
                                                  ifelse(opp_goals - team_goals > 0, 0, 1)))
                
                # rename columns so that you can have key variables for each team (away)
                away_df <- epl_df %>% 
                    select(TeamName = AwayTeam,
                           team_goals = FTAG,
                           opp_goals = FTHG, FTR) %>%
                    mutate(match_type = 'Away',
                           record = ifelse(opp_goals - team_goals > 0, "W",
                                                  ifelse(team_goals - opp_goals > 0, "L", "D")),
                           points = ifelse(opp_goals - team_goals > 0, 3,
                                           ifelse(team_goals - opp_goals > 0, 0, 1))) 
                
                # stack dfs with common field names
                epl_clean <- bind_rows(home_df, away_df)
          
        # 5 - Group the dataframe by TeamName, create and calculate all required columns      
                epl_summary <- epl_clean %>%
                    group_by(TeamName) %>%
                    summarize(
                        # record as wins-loses-ties (Record)
                        Record = paste(sum(record == 'W'), "-", sum(record == 'L'), "-", sum(record == 'D')),  
                        # home record (HomeRec)
                        HomeRec = paste(sum(record == 'W' & match_type == 'Home'), "-", sum(record == 'L' & match_type == 'Home'), "-", sum(record == 'D' & match_type == 'Home')), 
                        # away record (AwayRec)      
                        AwayRec = paste(sum(record == 'W' & match_type == 'Away'), "-", sum(record == 'L' & match_type == 'Away'), "-", sum(record == 'D' & match_type == 'Away')),
                        # matches played (MatchesPlayed)      
                        MatchesPlayed = n(),
                        # points (Points)      
                        Points = sum(points),
                        # Total number of wins
                        Wins = sum(ifelse(FTR == "H", 1, 0)),
                        # goals scored (GS)     
                        GS = sum(team_goals),
                        # goals allowed (GA)      
                        GA = sum(opp_goals)) %>%
                    mutate(
                        # goals scored per match (GSM)
                        GSM = GS/MatchesPlayed,
                        # goals allowed per match (GSM)
                        GAM = GA/MatchesPlayed,
                        # points per match (PPM)
                        PPM = Points/MatchesPlayed,
                        # point percentage (PtPct)
                        PtPct = Points/(3 * MatchesPlayed))
                
        # 6 - Do all the sorting in descending order based on points earned
            epl_summary <- epl_summary %>%
                arrange(desc(PPM), desc(Wins), desc(GSM), GAM) %>%
                        # Drop the Wins column
                    select(-Wins)  
        
        # return the result of step 6
        return(epl_summary) 
}

cat(
"An Overview of this Project: 
    
The dataset was utilized by the EPL_Standings function comprises pertinent details pertaining to matches 
in the English Premier League (EPL). The match between two teams is delineated in each row, which 
comprises the following information: date, home team, away team, full-time home goals (FTHG), full-time
away goals (FTAG), and full-time result (FTR). The outcome of the match is denoted in the FTR column 
as either a victory ('H'), a defeat ('A'), or a draw ('D') for the host team.

Critical Variables:
Date: Date of the match.
HomeTeam: Name of the home team.
AwayTeam: Name of the away team.
FTHG: Full-time home goals.
FTAG: Full-time away goals.
FTR: Full-time result (win, lose, or draw).

Approach to Methodology:

Data Retrieval: Using URLs specified by the season parameter, the function retrieves the EPL match data
for the specified season from a website (the current season, and the last two seasons).

Preprocessing of Data: Only the required columns (Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) were 
selected by the function.
The function formated dates by transforming the provided date into a date format object and applying the
modified date to the unprocessed data.
The data was filtered until and including the date of input.

Data Transformation: The function performs a data transformation from the match-level to the team-level
by renaming columns, generating distinct data frames for home and away matches, and computing 
supplementary variables including match type ('Home' or 'Away'), record (including wins, losses, and 
draws), and points.
It consolidated the home and away match data frames into a solitary data frame named 'epl_clean'.

Summary statistics were computed by the function in accordance with the names of the teams involved. 
These statistics include the following: overall record, home record, away record, matches played, 
points, victories (Wins), goals scored (GS), and goals allowed (GA).
The requested metrics such as point percentage (PtPct), goals scored per match (GSM), and goals allowed
per match (GAM) were calculated.

Sorting the final table:
In descending order, the function sorts the summary table according to the following criteria: points 
earned per match (PPM), wins (W), goals scored per match (GSM) and goals allowed per match (GAM).
The Wins column was dropped to enable the final output align with the requested output.
It concludes by returning the categorized and summarized EPL standings as at the inputed date and season.

Refleection: 

Key challenges I encountered included the management of date format variations, the maintenance of 
column name consistency across seasons, and the aggregation of match-level data into team-level 
summaries.

The function performs the following operations: downloads the corresponding data file according to 
the specified season, standardizes date formats, and computes summary statistics for each team using
conditional statements.

Contribution: The function offers a methodical framework for examining and condensing EPL match data,
thereby providing valuable insights into the performance of teams within a designated timeframe. 
This tool aids stakeholders in comprehending team standings, discerning the most successful teams, 
and scrutinizing patterns in points earned, goals scored, and match outcomes.

\n")
