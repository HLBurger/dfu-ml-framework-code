# Coding for ML Model Implementation in a Diabetic Foot Ulcer Database
This project contains the structure for statistical research and AI classification applied to the diabetic foot ulcer dataset. Currently, the code is in development and will be expended upon for ease of use. 

## Dataset
The dataset generated and analyzed during this study is not publicly available due to the protection of patient privacy. However, the dataset can be made available upon request from the corresponding author through a collaboration with the study team. 

## Setup

1. Clone the repo:

   ```bash
   git clone https://github.com/HLBurger/dfu-ml-framework-code.git
   cd dfu-ml-framework-code

2. Your project directory should look like this:

   ```
   dfu-ml-framework-code/
   │── visuals/
   │   ├── data_size.R                            # Visualization of performance increase over sample size.
   │   ├── decision_curve_analysis.R              # Visualization of clinical benefit over risk threshold of several prediction models.
   │── src/
   │   ├── data_processing.R                      # Manages dataset processing, including transformations and correlation tests.
   │   ├── Logistic_regression.R                  # Code for logistic regression training, testing and saving.
   │   ├── K_nearest_neighbor.R                   # Code for k nearest neighbor training, testing and saving.
   │   ├── Support_vector_machine.R               # Code for support vector machine training, testing and saving.
   │   ├── Random_forest.R                        # Code for random forest training, testing and saving.
   │   ├── Neural_network.R                       # Code for neural network training, testing and saving.
   |   ├── Bayesian_additive_regression_trees.R   # Code for bayesian additive regression trees training, testing and saving.
   |   ├── predictions.R                          # Used for loading model and predicting out of sample patient healing. 
   │── DESCRIPTION       # Dependencies
   │── README.md         # Documentation
   │── .gitignore        # Excludes unnecessary files from GitHub
   ```
   
3. All required packages are listed in the `DESCRIPTION` file.
   You can either install each library manually or use the following code:
   ```
   install.packages("devtools")
   devtools::install_deps()
   ```

## Use the Code
Depending on the your access to the dataset you can skip to step 3 for model predictions.

1. Data preparation is performed in `data_processing`. Here, feature selection is performed using statistical tests and the data is transformed to fit the machine learning models.
2. The training of models is performed in the respective files. Each model file contains the method for hyperparameter selection, average performance, feature importance (if applicable), and the data for performance increase over sample size (temporary). When you've trained a sufficable model, you can save it by running the last few lines in each file.
3. To predict the wound healing of some out of sample patients, you can use `predictions.R`. Some premade models will be/have been added on which you can test your patients.



