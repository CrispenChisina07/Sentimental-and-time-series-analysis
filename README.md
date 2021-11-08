Sentimental and time series analys of Google, Microsoft and Apple from 2015 - 2021

This project analyzes the complex relationship between Twitter data and the financial market instruments (like stock prices and volume). We have investigated Twitter sentiments for about 5 million tweets between January 2015 and January 2021 for Apple, Microsoft, and Google stocks. We studied the relationship in the time frame 2015 to 2021. Our results show a high correlation (up to 0.55) between stock prices and Twitter sentiments in the time frame. The data scraped from Twitter is evaluated using the get_sentiment toolkit of the "Syuzhet" package. Furthermore, the p-value for stock prices (dependent variable) and sentiment scores (independent variable) were observed to be less than 0.01, this suggests that each regressors contributes significantly in explaining the linear relationship.

We also used machine learning algorithms (naive Bayes, random forest, and support vector machines) in R to classify tweets into either positive or negative sentiments, and achieved the best accuracy across all companies using the naive Bayes model:
<img src="project_results/naive_bayes_confusion_matrix.png" width="200">


Here are some graphs from the companies sentimental scores and stock closing prices:
<img src="project_results/google_sentimenal_scores_and_stock_prices_results.png" width="500">
<img src="project_results/apple_and_microsoft_sentimental%20scores_and_stock_prices_results.png" width="500">

Our approach showed good correlation values between the features 0.553 for stock prices of the three companies and their sentimental scores and here are the results:
<br>
<img src="project_results/overall_correlation _matrix.png" width="500">

Also created a word cloud:
<br>
<img src="project_results/word_cloud.png" width="500">
<img src="project_results/word_cloud2.png" width="500">

