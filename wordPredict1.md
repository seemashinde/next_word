Next Word Prediction Presentation
========================================================
author: Seema Shinde
date: 22.Sep.2017
autosize: true

Application Overview
========================================================

- For the Capstone project in my Data Science Specialization through Johns Hopkins University, I was required to develop an application that predicts the next word (in real-time) from a user entered text box. My application features a text box where users enter any text then press the "Submit" button. It predicts next words choose the desired one from list. 

The Objective

- The main goal of this capstone project is to build a shiny application that is able to predict the next word. 

- This exercise was divided into seven sub tasks like data cleansing, exploratory analysis, the creation of a predictive model and more.

- All text mining and natural language processing was done with the usage of a variety of well-known R packages.


Algorithm And Methods
========================================================

- Data loading, selecting US dataset (written in the English language) which contains Internet blogs, Internet news and Twitter messages.

- Sampling 3 files & building Corpus using subsets of 3 files.

- Text cleaning: tokenization, removing Stopwords, Stemming and Profanity filtering.

- Building n-gram model, Creating 2 gram,3 gram and 4gram frequency matrices.
- Building predictive model using n-gram frequency matrices.

- Building shiny App and Deploying it at 

App Usage
========================================================
The user interface of this application is very simple. After entering the text, predicted word get displayed on Submit button press.

![alt text](wordPredict1.png)


Additional Information
========================================================

The next word prediction app is hosted on shinyapps.io: https://seemashinde.shinyapps.io/nextword

The whole code of this application, as well as all the milestone report, related scripts, this presentation etc. can be found in this GitHub repo: https://github.com/seemashinde/CapstoneCoursera

This slide deck is located here: http://rpubs.com/seemashinde/nextword

