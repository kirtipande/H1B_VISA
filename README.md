# H1B_VISA
## Handling Imbalanced dataset using XGBoost

Hi everyone,

This project involved Classifying a H1B applicant's chances of being certified or not. 

Data : The data was obtained from the Office of Foreign Labor Certification (OFLC).
       The data had ~500k rows and 4 categories : CERTIFIED,WITHDRAWN,CERTIFIED BUT WITHDRAWN,DENIED.
       We chose to only keep the categories CERTIFIED and DENIED for simplicity.
       Removed all the missing values since we did not have a data constraint but time definitely was one!

Imbalance : The dataset had 99% of the rows as CERTIFIED and the remaining 1% as DENIED.
            This made the problem very challenging since it enabled us to  explore ways to handling imbalanced data in an efficient way.
            There are 2 ways in which one can handle imbalanced data 
                  1.Resampling 
                  2.Bagging/Boosting
                  Since, we wanted to stay authentic to our population we chose to not use the Resampling method.

Assumptions : 2016 & 2017 , Only H1B applications

Features created : Wage, STEM/NOT STEM,j ob_groups, employment/worksite regions and waiting time between submission and decision date( Yes I know what you're thinking! wait a while :) )

Data Normalization : Normalized wage and waiting time by applying log transform

Main Objective : To provide with as much accuracy as possible, the chance of denial to an applicant.(since the worst case would be if                    the applicant would be told that his application will be accepted when in reality his application is denied)
                 Hence, our focus is to minimize false negative rate first, then minimize the total error rate

The output with the techniques implemented are given below :

|              Model                | False Negative Rate |Total Error Rate|
| --------------------------------  | ------------------- |----------------|
| Logistic Reg                      |                     |                |
| Stepwise selection                |     68.8%           |     0.9%       |
| With threshold selection          |     44.7%           |     1.9%       |
| LDA with equal prio probabilities |     41.5%           |     17.9%      |
| Random Forest                     |                     |                |
| Without Bagging                   |     44.2%           |     0.6%       |
| With Bagging(3 variables)         |     46.8%           |     0.7%       |
| XGBoost                           |     30.2%           |     1.8%       |

The best model in terms of minimized false negative rate is XGBoost, but we prefered to use the Random Forest model because it had the lowest error rate.

Apart from the techniques mentioned above we also tried implementing Lasso/Ridge but were unsuccessful. Sadly,we let it go due to time constraints and because we heard that these techniques tend to misbehave with classification problems.

Also, k-fold cross validation was implemented to determine optimum parameters throughout the analysis.



  
