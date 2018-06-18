# A simple word predictor

This repository holds scripts for creating a language model and a Shiny web application for a simple word predictor. It was created to complete the Coursera Data Science Specialization Capston project and is my first R project and Shiny App.

You can try the app at: <https://codrin.shinyapps.io/WordPredictor/>

* [Data Science Capstone Notebook Text Prediction App Quanteda DT.Rmd](https://github.com/codrin-kruijne/data-science-capstone/blob/master/Data%20Science%20Capstone%20Notebook%20Text%20Prediction%20App%20Quanteda%20DT.Rmd) holds code for language model creation.
* TODO  holds some code for calculating and plotting some basic text characteristics.
* TODO  holds code for a wordPredictor function that is shared between language model script and app.
* [Word Predictor](https://github.com/codrin-kruijne/data-science-capstone/tree/master/WordPredictor) folder contains files necessary for the Shiny App
* [Data Science Capstone Text Prediction.Rpres](https://github.com/codrin-kruijne/data-science-capstone/blob/master/Data%20Science%20Capstone%20Text%20Prediction.Rpres) is a short presentation about the language model and app development.
* 
* Other .Rmd notebooks are older versions of the language model script; for some reason my Rmd keep breaking and cannot knit when I break off processes. Still have to figure out what is causing this to find a solution.

Still lots to be fixed and optimized:
* Recheck language model creation steps
* Check profanity filter
* Reduce repetitive code in data_descriptives.R and wordPredictionFunction.R
* Do elaborate code profiling to improve computational efficiency