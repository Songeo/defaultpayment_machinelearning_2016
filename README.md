# Default of Credit Card Clients

---

### Final project for Machine Learning

Team members: M. Godina, S. Mendizábal, R. Morales.

Created: November 2016

Final review: December 2016

---

#### Introduction

The purpose of this project 
was to predict default paymente of credit card clients
and profile them. The idea is 
to identify risky sectors
in Taiwan during the economic
crisis, between 2005 and 2006. 

The impact the crisis had amoung the taiwanese society
was enormous. One of the most alarming 
effects was a social and health crisis caused by 
the rise of suicide rates among the population,
becoming the second country with higher rates in the world 
according to UN. 

One of the benefits of predicting de probability
of default payment is to prevent the acceptance of 
risky debt and reduce uncertainty, which was the 
reason of the crisis. By doing this 
analysis, the trust in Taiwan's Finance 
System, lost as consecuence of the 
economic crisis, may be recovered and would 
help to strengthen the system. 




#### Methodology

The database had 30 thousand observations and 23 
columns, new features were created 
with these variables and 
some recodifications were done. 

At first, a selection of features was 
performed with Random Forest and achieved an 
accuracy of 80%. Using the Gini Importance
11 variables were selected, excluding 
demographic variables.

Using the selected variables several models 
were performed to predict the 
defaultment and the results obtained were for
Naive Bayes (accuracy 57%),
Random Forest (accuracy 80%),
Support Vector Machine (accuracy 82%), 
Neural Network (accuracy 81%).

Finally, three groups were obtained with 
demographic characteristics
using hierarchical clustering and
contrasting them with 
the probabilities obtained 
with SVM 
the best model according to accuracy, 
the most risky sector were women and
men under 34 years old, single and
just with college degree.




#### Content

* Data obtained from
<https://www.kaggle.com/uciml/default-of-credit-card-clients-dataset>.

* Python and R code in file **src**.

* Final report in file **reports**.


---

**Note:** Version Python 2