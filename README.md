# Shark Tank: Analyzing Investment Decisions

## Project Overview
This project analyzes the investment decision-making process on the popular TV show "Shark Tank," aiming to uncover whether business metrics or show dynamics drive outcomes. Using predictive models, clustering techniques, and statistical analysis, the study identifies the factors that most influence investment decisions. Results indicate that financial metrics like the ratio of the amount asked to valuation and project category are the strongest predictors of success, while television-related variables like episode or season play a minimal role. 

Entrepreneurs can use these insights to refine their pitches, focusing on robust financial valuations and strategic category positioning.

---

## Features of the Project
- **Tree-Based Predictive Model**: Uses boosted trees to predict investment decisions based on various factors.
- **Discriminant Analysis**: Assesses the likelihood of securing deals using the sharks' profiles and project categories.
- **K-Means Clustering**: Identifies patterns distinguishing successful from unsuccessful pitches.
- **Principal Component Analysis (PCA)**: Explores correlations and dimensionality reductions among features.
- **Categorical Insights**: Highlights significant predictors like project category and financial valuations.

---

## Dataset Description
### Primary Data
- **Source**: Collected data from 496 Shark Tank episodes.
- **Features**:
  - **Description Length**: Text length of the pitch description.
  - **Category**: The type of product or service pitched.
  - **Valuation**: The entrepreneur's valuation of their business.
  - **Ratio of Asked Amount to Valuation**: Measures the entrepreneur's ask relative to their valuation.
  - **Investor Combination**: Unique combinations of sharks in each pitch.
  - **Season and Episode**: Contextual variables related to TV production.
  - **Deal**: Binary indicator of whether the pitch secured an investment.

### Feature Engineering
- **Numeric Features**: Normalized continuous variables for improved modeling.
- **Categorical Features**: Combined low-frequency categories to reduce imbalance.
- **New Variables**: Created additional features like "season part" and "CA indicator" for nuanced analysis.

---

## Methodology
1. **Exploratory Data Analysis (EDA)**:
   - Examined data distributions and addressed missing values.
   - Visualized relationships between predictors and target variables.
2. **Feature Engineering**:
   - Transformed variables using PCA, binning, and standardization.
   - Consolidated sharks and categories to reduce dimensionality.
3. **Model Development**:
   - Logistic Regression: Evaluated statistical significance of predictors.
   - Boosted Trees: Enhanced prediction accuracy and feature importance ranking.
   - Quadratic Discriminant Analysis: Addressed non-linear relationships in predictors.
4. **Unsupervised Learning**:
   - K-Means Clustering: Grouped pitches to reveal underlying patterns.
5. **Evaluation**:
   - Benchmarked models using accuracy, precision, recall, and F1 scores.

---

## Key Results
- **Boosted Trees Accuracy**: Achieved 70% accuracy, outperforming other models.
- **Logistic Regression Insights**:
  - Ratio of Asked Amount to Valuation was the most significant predictor.
  - Project categories like "Fashion and Apparel" negatively influenced success.
- **Minimal Impact of Show Dynamics**: Episode and season variables had low predictive power.
- **Clustering Analysis**: Showed no clear patterns for "deal" vs. "no deal" based on TV characteristics but highlighted key financial features.

---

## Future Improvements
- **Enhanced Dataset**: Incorporate real-world outcomes for rejected pitches.
- **Advanced NLP**: Analyze pitch descriptions for linguistic patterns linked to success.
- **Show Dynamics**: Study additional variables like airtime or shark commentary.

---

## Getting Started
### Dependencies
- **Programming Language**: R
- **Libraries**:
  - ggplot2, glmnet, caret, gbm, randomForest, dplyr, tidyr, MASS, and more.

### Usage
1. Clone the repository:
   ```bash
   git clone https://github.com/your-username/shark-tank-analysis.git
   ```
2. Open the R scripts and load the dataset.
3. Run the scripts for data preprocessing, feature engineering, and model training.
4. Visualize results using the included ggplot2 and PCA plots.

---

## Contact
For questions or collaboration opportunities, please reach out to:

**Name**: Arturo Medina  
**LinkedIn**: [linkedin.com/in/arturo-medina](https://linkedin.com/in/arturo-medina)
