# Decision-Trees---Random-Forest-Bagging-Boosting
The goal of this project is to build linear and various tree models and compare model fitness. We have used Boston Housing dataset for this purpose. The response variable of this dataset is medv (Median value of owner-occupied homes) which is continuous quantitative variable. Hence, we will fit a linear model and a regression tree model.

Approach:
•	Split the data into 75% training and 25% testing sets using random seed.
•	Built a linear model using all the variables on training dataset.
•	Analyzed different model performance measures – MSE, Adjusted R^2.
•	Selected effective variables using various model selection techniques – best subset selection, stepwise regression.
•	Finalized a linear model and calculated in-sample and out of sample error.
•	Created a regression tree and pruned it.
•	Designed trees using various ensemble techniques – bagging, random forests and boosting.
•	Finalized a tree model and calculated in-sample and out of sample error.
•	Compared the results with linear model.
