# WorldCupPrediction2018

Aim of this Project
-------------------
This prediction model has been created for Upgrad Fifa World Cup 2018, we might predict its outcome using R and elo ratings from exciting elo package and learn some interesting facts along the way.

Data Collection
---------------
I have only use the amazing kaggle dataset International football results from-1872 to 2017. (Ref :: https://www.kaggle.com/martj42/international-football-results-from-1872-to-2017/home)
It is a huge collection of data on matches spanning more than a century of international soccer games. Thanks to everyone how put this together.

Methodology Used
----------------
The Prediction has three main steps:

1) Calculate the elo rating for each match and create a dataset.
2) Use the updated elo to calculate the probablities of winning the world cup 2018.
3) Simulate the tournament from Knockout Stage to Final using the probablity.
4) We have similated the probalities with k=30,50,100,200 and got the below result as the final prediction

Result
------
As per ELO probality calculation
1) Winner : Brazil
2) Ruuner-up : Spain
3) Third Place: France
4) Fourth Place: England

Acknowledgements
----------------
1) https://www.kaggle.com/martj42/international-football-results-from-1872-to-2017/home
2) https://www.rdocumentation.org/packages/elo/versions/1.0.1
3) https://www.statmethods.net/advgraphs/ggplot2.html
4) https://edomt.github.io/Elo-R-WorldCup/
