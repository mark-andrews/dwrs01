# Introduction to Data Wrangling using R

In this two day course, we provide a comprehensive practical introduction to data wrangling using R.
In particular, we focus on tools provided by R's `tidyverse`, including `dplyr`, `tidyr`, `purrr`, etc.
Data wrangling is the art of taking raw and messy data and formating and cleaning it so that data analysis and visualization etc may be performed on it. Done poorly, it can be a time consuming, labourious, and error-prone. Fortunately, the tools provided by R's `tidyverse` allow us to do data wrangling in a fast, efficient, and high-level manner, which can have dramatic consequence for ease and speed with which we analyse data. On Day 1 of this course, having covered how to read data of different types into R, we cover in detail all the `dplyr` tools such as `select`, `filter`, `mutate`, etc. Here, we will also cover the pipe operator (`%>%`) to create data wrangling pipelines that take raw messy data on the one end and return cleaned tidy data on the other.
On Day 2, we cover how to perform descriptive or summary statistics on our data using `dplyr`'s `summarize` and `group_by` functions. We then turn to combining and merging data. Here, we will consider how to concatenate data frames, including concatenating all data files in a folder, as well as cover the powerful SQL like join operations that allow us to merge information in different data frames. The final topic we will consider is how to "pivot" data from a "wide" to "long" format and back using `tidyr`'s `pivot_longer` and `pivot_wider`.

## Intended Audience

This course is aimed at anyone who is involved in real world data analysis, where the raw data is messy and complex. Data analysis of this kind is practiced widely throughout academic scientific research, as well as widely throughout the public and private sectors.

## Teaching Format

This course will be practical, hands-on, and workshop based. For some topics, there will a very minimal amount of lecture style presentations, i.e., using slides or blackboard, to introduce and explain key concepts and theories, but almost all of our time will be spent doing data wrangling using R. Any code that the instructor produces during these sessions will be uploaded to a publicly available GitHub site after each session.

The course will take place online using Zoom. On each day, the live video broadcasts will occur between (British Summer Time, UTC+1, timezone) at:

* 10am-12pm
* 1pm-3pm
* 4pm-6pm
 
All sessions will be video recorded and made available to all attendees as soon as possible, hopefully soon after each 2hr session.
  
Attendees in different time zones will be able to join in to some of these live broadcasts, even if all of them are not convenient times.
For example, attendees from North America may be able to join the live sessions at 1pm-3pm and 4pm-6pm, and then catch up with the 10am-12pm recorded session when it is uploaded (which will be soon after 6pm each day). 
By joining any live sessions that are possible, this will allow attendees to benefit from asking questions and having discussions, rather than just watching prerecorded sessions. 

At the start of the first day, we will ensure that everyone is comfortable with how Zoom works, and we'll discuss the procedure for asking questions and raising comments. 

Although not strictly required, using a large monitor or preferably even a second monitor will make the learning experience better, as you will be able to see my RStudio and your own RStudio simultaneously. 

All the sessions will be video recorded, and made available immediately on a private video hosting website. Any materials, such as slides, data sets, etc., will be shared via GitHub.

## Assumed quantitative knowledge

We will assume familiarity with only the most basic of statistical concepts, such as descriptive statistics. We will not even assume that participants will have taken university level courses on statistics.

## Assumed computer background

Minimal prior experience with R and RStudio is required. Attendees should be familiar with some basic R syntax and commands, how to write code in the RStudio console and script editor, how to load up data from files, etc. 

## Equipment and software requirements

Attendees of the course will need to use RStudio. Most people will want to use their own computer on which they install the RStudio desktop software. This can be done Macs, Windows, and Linux, though not on tablets or other mobile devices. Instructions on how to install and configure all the required software, which is all free and open source, will be provided before the start of the course. We will also provide time at the beginning of the workshops to ensure that all software is installed and configured properly. An alternative to using a local installation of RStudio is to use RStudio cloud (https://rstudio.cloud/). This is a free to use and full featured web based RStudio. It is not suitable for computationally intensive work but everything done in this class can be done using RStudio cloud. 

Details on the software that we will use is available [here](software.md).

# Course programme 

## Day 1 

* Topic 1: *Reading in data*. We will begin by reading in data into R using tools such as `readr` and `readxl`. Almost all types of data can be read into R, and here we will consider many of the main types, such as `csv`, `xlsx`, `sav`, etc. Here, we will also consider how to contol how data are parsed, e.g., so that they are read as dates, numbers, strings, etc.
* Topic 2: *Wrangling with `dplyr`*. For the remainder of Day 1, we will next cover the very powerful `dplyr` R package. This package supplies a number of so-called "verbs" --- `select`, `rename`, `slice`, `filter`, `mutate`, `arrange`, etc. --- each of which focuses on a key data manipulation tools, such as selecting or changing variables. These verbs also have `_if`, `_at`, `_all` variants that we will consider. All of these verbs can be chained together using "pipes" (represented by `%>%`). Together, these create powerful data wrangling pipelines that take raw data as input and return cleaned data as output. Here, we will also learn about the key concept of "tidy data", which is roughly where each row of a data frame is an observation and each column is a variable.

## Day 2

* Topic 3: *Summarizing data*. The `summarize` and `group_by` tools in `dplyr` can be used with great effect to summarize data using descriptive statistics. 
* Topic 4: *Merging and joining data frames*. There are multiple ways to combine data frames, with the simplest being "bind" operations, which are effectively horizontal or vertical concatenations. Much more powerful are the SQL like "join" operations. Here, we will consider the `inner_join`, `left_join`, `right_join`, `full_join` operations. In this section, we will also consider how to use `purrr` to read in and automatically merge large sets of files.
* Topic 5: *Pivoting data*. Sometimes we need to change data frames from "long" to "wide" formats. The R package `tidyr` provides the tools `pivot_longer` and `pivot_wider` for doing this.

