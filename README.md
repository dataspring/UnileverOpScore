**


Overall Opinion Score Prediction Challenge
==========================================

Introduction and Overview of Solution
-------------------------------------
*Briefly describe your algorithm to predict overall opinion score. And  Provide details of your algorithm including how many of iterations you have to go through to get the final algorithm. We are interested in your train of thoughts.*

From the outset, the focus was on feature selection. Being the first ever competitive entry and with an uphill learning curve mastering R to learn powerful machine learning methods, understood feature engineering plays a crucial role. Features has to be analyzed thoroughly and preprocessed to make it ready so various models can be applied quickly with ease. 

**Features Overview:** 
Training dataset in total has 302 features including the predictive class Overall.Opinion which is assigned a final score based on individual survey questionnaire responses from each survey respondent. By predicting individual respondent score and collating them across products, we could infer present consumer preference of product make, features, etc. and tailor future product versions and target to right market

**Broadly, features development involved the following:**

**Score Munging:** 
Scores for majority of features had a text suffixed with a corresponding score. The major cleanup was to stick to suffixed score and discard rest of the explanatory text. Some score columns had redundant columns which were compressed to one, decreasing missing score values. Apply functions and stringr package in R was used to achieve this. Some features had a corresponding ‘strength.’ feature column and they were discarded due to high missing values.

**Level Reduction:** 
Features like Education.level, Amount.used, Times.used, Dry.method, Annual.household.income had too many options/gradations to rate and were leveled to manageable few 

**Product Mix** 
This group comprises XX number of features primarily indicating percentage of composition in the overall ingredients make-up of the product. Adding all ingredients amounts to 100% and to even out the influence, all weighted down by dividing each ingredient percentage by 100. 

**Derived Features** 
Main smell and foam features pertaining to ‘Smell..when.you.wear.them’ and ‘Amount.of.form’ were further enhanced and augmented from all their smell and foam related feature sets – as most of them were having large amount of missing values. Detergent handle involving scoop, spoon, cup and hands were combined into one. 

In the end 271 features were finalized. Features with less than 40% missing values were chosen and randomForest imputation was used to fill in the missing values and various training methods were tried.

Being a multi-class classification problem, started with Naïve Bayes and CART. These simple models helped to become familiar with the data while initial submissions had a high mse score. 

**Applied and tested the following methods:**

 1. K-Nearest Neighbors (KNN),    
 2. Bagged AdaBoost (AdaBag)    
 3. C5.0
 4. Decision Trees (C5.0)    
 5. Random Forest (RF),    
 6. Stochastic Gradient Boosting (GBM)
    
We didn’t have much luck with the above except RF. RF models with 40% missing value didn’t fare well, so I ended up with all variables in and 10-fold cross validation yielded only a .266 score in leaderboard. RF model was tuned using Caret package with a lesser folds and tuned the mtry. Number of trees were tuned using tuneRF function. But RF was compute intensive and 10 fold cross validation with 269 features took long time to materialize a model. Even attempted AWS and Azure for compute resources but to no significant avail. Also fine-tuned the multi-class summary function by writing a custom function to adhere to MSE criteria rather than the caret provided Accuracy/Kappa functions. This custom function was used while training for RF and GBM models to decide the right tuning parameters for model selection.

Finally, instead of imputing missing values on training observations, they were replaced by a ‘UnAvailable’ level and soon turned to tuning the GBM tree model parameters, as its individual prediction on ‘Overall.Opinion’ is a decimal score rather an absolute score which in turn provided a better MSE score on the leaderboard. 

Using cross validation error, tuned the following parameters: number of trees, bagfrac, shrinkage, and depth. Also tuned the minobinsnode parameter – saw significant improvements when adjusting the parameter downwards from its default setting.

Final Model created using 269 attributes with a GBM model as follows:
 

> gbmFit9Gauss400 <-   gbm(
>     Overall.Opinion ~ .,
>     data = train1.batch,
>     distribution="gaussian", # see the help for other choices
>     n.trees=400, # number of trees
>     shrinkage=0.075, # shrinkage or learning rate,# 0.001 to 0.1 usually work
>     interaction.depth=6, # 1: additive model, 2: two-way interactions, etc.
>     bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
>     train.fraction = 0.65, # fraction of data for training, # first train.fraction*N used for training
>     n.minobsinnode = 8, # minimum total weight needed in each node
>     cv.folds = 10, # do 3-fold cross-validation
>     keep.data=TRUE, # keep a copy of the dataset with the object
>     verbose=TRUE, # don't print out progress
>     n.cores=7) # use only a single  save(gbmFit9Gauss400, file="D:\\Data
> Mining\\02-Dextra-Unilever\\01-NA-All\\gbmFit9Gauss400.RData")
> 

If absolute Overall.Opinion values are required, probability values can be used either from the RF model or other models to decide the absolute value instead of decimal values.

Insights from data
------------------

*Can you find the main attributes or factors and/or ingredients which have the most influence on the overall opinion score?*

10 fold cross validated RF model produced the following attribute importance: (top 20 is given for brevity)

 Whereas 10 fold cross validated GBM model produced (top 20 is given for brevity). Both these models concur to certain extent. But the startling finding is if attributes with no relative influence or importance was used for training and prediction in both cases, the MSE score faired poorly.
 
*Can you find any insights from the data?*

**Data redundancy** – Some features had an associated ‘strength’ feature which was redundant and didn’t add any significant influence and to see if they can fill the missing values in their counterparts was also in vain. Features including ‘Powder.left.on.clothes’ to ‘Skin.Sensitive.Issues’ had an associated ‘strength’ column and were skipped from the training set altogether.

**Data Imputation & Missing Values** – Initially thought of imputing features that have at least 40% missing data might help in predictive influence but found otherwise. After few tuning rounds found that including all finalized 269 features with an ‘UnAvailable’ level for ‘NA’/missing data significantly improved the predictive performance. It’s presumed that some data points (even in a highly missing data column) has an impact on the final prediction and decision tree modelers are able to catch them and can use them to provide better prediction. 

**Data Context** – Major data components include ingredients, usability, foam, smell, fondness and respondent profile. Seems ingredients may not be part of actual survey but included in opinion score to see how ingredient mix might influence the opinion score.

**Scoring Confusion** – Some of the features had a confusing text suffixed with a score, Ignoring the text and assuming the score to better reflect the survey than explanatory text was the methodology used.

Also some of the derived columns had greater influence than being individual and with significant missing data.

Implementation of model/algorithm from the fields selected
----------------------------------------------------------


----------


*Can the idea be implemented? Please explain how.*

The idea can be implemented by using R and perhaps Revolution R with a robust enterprise level handling of execution, security and devops.  Again the objective for prediction results and how Unilever intends to capitalize on this results to ‘improve what’ decides the overall implementation. 

*How easy can it be implemented?*

Model implementation is fairly straight forward from a algorithm perspective but intricacies of survey frequency, data volume, data quality and additional actionable insights required decides the complexity.

*Any risk in the implementation and how to overcome it?*

As above

*State hypothesis.*

Purpose of implementation is to predict the overall opinion score with available dataset

*State assumptions which you made to implement the solution.*

1.	Survey is primarily intended to identify consumer preference of detergents
2.	Survey is of similar type with no change on features
3.	Missing value percentage may vary slightly


Recommendations
---------------


----------


*Recommendations to improve current data collection and how it should be done.*

 - Score Streamlining: Score text and Score number to be consistent and
    meaningful   
 - Score Leveling: More options lead to confusion and
    lethargy in deciding the right score and hence rightsizing the
    levels is required   
 - Redundant Attributes: To be compressed and
    perhaps combined to improve survey questionnaire credibility   
 - Missing Value Avoidance: Ensure scores for individual attributes to have be
    of adequate levels/ cover enough details to minimize ‘NA’/missing
    values thereby improving accuracy  
 - Ingredient Mix being included in the prediction need to be reassessed as most of     the ingredients may  not have any significant influence on the outcome If survey ould be    digitally available in different device form factors both online andoffline for surveyors may alleviate some of the dat  inconsistency/missing issues and centralized but managed questio  bank could improve overall efficiency and survey administration
    
*Recommendations for external and/or additional data to complement your model*
 
 
 - Social media platforms’ publicly available conversation data may help in sentimental analysis of detergent take-up / product issues and again is questionable and has to be analysed 
 - Detergent sales figures by region, demography 
 - Feedback covering incidences, type of issues, and complaints from consumer hotline 
 - Marketing reach from consumer for specific detergent airing – TVR ratings across demography may give some assessment of detergent awareness/reach

Conclusions
-----------


----------
*Provide how this solution will be useful and beneficial.*

 - Assess which ingredient affects the overall opinion score and hence product make
 - Survey data may help in shaping future product make of desired features/attributes
 - With huge survey datasets, survey sampling can be applied to a new product mix and predict it’s market acceptance i.e. overall opinion for the product
 - With external datasets, new product market reach can be predicted for a given ingredient mix and demography reach/consumer preferences

*Provide any conclusions you can make from this challenge.*

This challenge is a right step to explore how survey attributes and detergent ingredient mix affect each other but a first step in utilizing machine learning to explore new avenues to capitalize on data to serve consumers and delight them ultimately. 

Appendix
--------


----------
*Provide any supporting ideas, recommendations, illustrations, articles and charts.
Missing Data % Graph – 30% NA values followed by 40% and 50% brackets.*



