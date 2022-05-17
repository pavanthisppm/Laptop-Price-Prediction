# laptop-price-prediction

Each different laptop has its different software and hardware that causes the price of the laptop to fluctuate.

This study was conducted to model how laptop prices differ with different factors and to build a predictive model. According to the initial descriptive analysis, we saw that there were some outliers in the dataset, the presence of multicollinearity among some variables in the data set, and the response variable was highly skewed. So, we ran 16 models based on the ridge, lasso, elastic net, and random forest, and we selected the best one. According to the Test RMSE value, the best model out of all is the Random Forest model with outliers without log transformation.

The Random Forest model with 10-fold cross-validation gave the test RMSE of 230.2287 and MAPE value of 15.09% when the number of trees ("ntree") was 500 and the number of variables ("mtry") was 44, and the 81.35% of the total variance explained by the fitted model. Furthermore, this model was used to develop the data product application. The data product was deployed on ShinyApps.io.

R, R Shiny



https://user-images.githubusercontent.com/96905837/164538946-872278de-ed42-44ff-9e30-1f88b710694c.mp4


