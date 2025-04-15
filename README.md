# Coding for ML Implementation in a Diabetic Foot Ulcer Database
This project contains the structure for statistical research and AI classification applied to the diabetes dataset. Currently, the code is in development and will be expended upon for ease of use. 

## Dataset
The dataset generated and analyzed during this study is not publicly available due to the protection of patient privacy. However, the dataset can be made available upon request from the corresponding author through a collaboration with the study team. 

## Using the Code

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
│── example.R          # Main script to run training and evaluation
│── DESCRIPTION.txt   # Dependencies
│── README.md         # Documentation
│── .gitignore        # Excludes unnecessary files from GitHub
```
  

## ** Install Dependencies **
All required packages are listed in the `DESCRIPTION` file.
You can either install each library manually or use the following code:
```
install.packages("devtools")
devtools::install_deps()
```




