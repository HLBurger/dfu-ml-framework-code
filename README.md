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
│── experiments/
│   ├── Experiment Hidde.ipynb  # Jupyter Notebook you can copy and modify
│   ├── experiment_x.ipynb  # Jupyter Notebook for a specific experiment# Another experiment
│── src/
│   ├── data.py        # Manages dataset processing, including loading and transforming images.
│   ├── model.py       # Defines the convolutional neural network (CNN) architecture.
│   ├── train.py       # Handles model training, including data loading, optimization, and logging.
│   ├── evaluate.py    # Contains the script for evaluating the trained CNN model on the test dataset.
│   ├── utils.py       # Includes helper functions such as visualization and metric calculations.
│   ├── config.py      # Handles configuration settings such as device setup (CPU/GPU).
│── main.py           # Main script to run training and evaluation
│── requirements.txt  # Dependencies
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




