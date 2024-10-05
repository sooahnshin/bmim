# Replication Codes for "$\ell_1$-based Bayesian Ideal Point Model for Multidimensional Politics"

-   To begin, install the `l1ideal` package included in the folder as a `tar.gz` file. The package is used for the proposed method in the paper. It is also available from [this repo](https://github.com/sooahnshin/l1ideal).

    ``` r
    install.packages("l1ideal_1.2.tar.gz", repos = NULL, type="source")
    ```

-   Below is an overview of the replication codes for the paper. Please note that the codes are located in the `codes` folder as R scripts. The folder includes the following subfolders:

    1.  `illustration`: R scripts for generating the illustrative figures.
    2.  `simulation`: R scripts for running the simulation studies presented in the main text.
    3.  `simulation/tuning_param`: R scripts for simulation studies with varying tuning parameter `c` values.
    4.  `simulation/utility`: R scripts for simulation studies with different data generating processes across settings (a) to (c).
    5.  `congress`: R scripts for generating figures for the US Congress analysis.

-   Each R script contains instructions, and some intermediate results are saved in the `data` folder. Due to the size of the data, we have not included it as part of the Supplementary Materials. Instead, the data can be downloaded from [this repo](https://github.com/sooahnshin/bmim).

-   The `data` folder (available from [this repo](https://github.com/sooahnshin/bmim)) includes the following subfolders:

    1.  `illustration`: Intermediate results for the illustrative examples.
    2.  `congress`: Intermediate results for the US Congress analysis. Raw data can be downloaded [here](https://voteview.com/data). Check `01_house_preprocess.R` for data download and preprocessing.
    3.  `simulation`: Intermediate results for the simulation studies.

-   See the following table for an overview of the R scripts and descriptions, organized by the order of figures reported in the main text.

| Figure \# | Description                                                                                       | R Script                                  | Data                            | Notes                                                 |
|---------------|---------------|---------------|---------------|---------------|
| 1         | Estimated Ideal Points for Small Chamber with Two Uncorrelated Dimensions                         | `illustration/01_example1.R`              | synthetic (Section 2 Example 1) |                                                       |
| 2         | Two Voting Clusters with Correlated Dimensions                                                    | `illustration/02_example2.R`              | synthetic (Section 2 Example 2) |                                                       |
| 3         | DW-NOMINATE Scores of the 53rd US House of Representatives (1893-1896)                            | `congress/03_house_visualization.R`       | US Congress                     |                                                       |
| 4         | Illustration of Rotation Invariance Problem                                                       | `illustration/03_rotational_invariance.R` | \-                              |                                                       |
| 5         | Illustration of Cleavages Captured by Different Choices of Distance Measure with Example 1        | `illustration/01_example1.R`              | synthetic (Section 2 Example 1) |                                                       |
| 6         | Simulation Studies with Synthetic Data                                                            | `simulation/01_simulation_master.R`       | synthetic                       | See `## Simulation` below for details about the data. |
| 7         | Multidimensional Ideal Point Estimation of the 53rd US House of Representatives (1893-1896)       | `congress/03_house_visualization.R`       | US Congress                     |                                                       |
| 8         | Multidimensional Ideal Point Estimation of US House of Representatives using the Proposed Method. | `congress/03_house_trajectory.R`          | US Congress                     |                                                       |

-   The following table provides an overview for the supplementary figures.

| Appendix | Figure \# | Description                                                                                                                         | R Script                                          | Data                     | Notes                                 |
|------------|------------|------------|------------|------------|------------|
| F        | 1         | Illustration of Cutting Line in l1 Norm Space                                                                                       | `illustration/04_cutting_line.R`                  | \-                       |                                       |
| G        | 2         | An Illustration of Yea/Nay Positions from BIRT Model in 2-dimensional l2 Norm Space                                                 | `illustration/04_cutting_line.R`                  | \-                       |                                       |
| G        | 3         | Roll Call Votes of the 53rd US H.R. on Two Silver Bills                                                                             | `congress/03_house_visualization.R`               | US Congress              |                                       |
| H        | 4         | Convergence Diagnostics for the Estimated Parameters of BMIM                                                                        | `simulation/01_simulation_master.R`               | synthetic                |                                       |
| H        | Table 1   | Computational Cost of Simulation Studies                                                                                            | `simulation/01_simulation_master.R`               | synthetic                |                                       |
| I        | 5         | Illustration of a Unidimensional Slice Sampler for the BMIM                                                                         | \-                                                | \-                       | Adapted from Figure 1 of Neal (2003). |
| I        | 6         | Simulation Results with Different Tuning Parameters $c$                                                                             | `simulation/tuning_param/03_simulation_results.R` | synthetic (non-partisan) |                                       |
| I        | 7         | Convergence Diagnostics with Different Tuning Parameters $c$                                                                        | `simulation/tuning_param/03_simulation_results.R` | synthetic (non-partisan) |                                       |
| J        | 8         | Illustration of Post-Processing in the BMIM to Fix the Signed Permutation                                                           | `simulation/01_simulation_master.R`               | synthetic (two-party)    |                                       |
| K        | 9         | Simulation Results using WNOMINATE                                                                                                  | `simulation/02_simulation_wn.R`                   | synthetic                |                                       |
| K        | 10        | Simulation Results using BIRT                                                                                                       | `simulation/03_simulation_irt.R`                  | synthetic                |                                       |
| L.1      | 11        | Simulation Studies with BMIM using Synthetic Roll Call Data Generated with Gaussian Utility                                         | `simulation/utility/01_sumulation_master.R`       | synthetic                |                                       |
| L.1      | 12        | Simulation Studies with WNOMINATE using Synthetic Roll Call Data Generated with Gaussian Utility                                    | `simulation/utility/02_simulation_wn.R`           | synthetic                |                                       |
| L.1      | 13        | Simulation Studies with BIRT using Synthetic Roll Call Data Generated with Gaussian Utility                                         | `simulation/utility/03_simulation_irt.R`          | synthetic                |                                       |
| L.2      | 14        | Simulation Studies with BMIM using Synthetic Roll Call Data Generated with Quadratic Utility                                        | `simulation/utility/01_sumulation_master.R`       | synthetic                |                                       |
| L.2      | 15        | Simulation Studies with WNOMINATE using Synthetic Roll Call Data Generated with Quadratic Utility                                   | `simulation/utility/02_simulation_wn.R`           | synthetic                |                                       |
| L.2      | 16        | Simulation Studies with BIRT using Synthetic Roll Call Data Generated with Quadratic Utility                                        | `simulation/utility/03_simulation_irt.R`          | synthetic                |                                       |
| M.1      | 17        | Simulation Studies with BMIM using Synthetic Roll Call Data Generated with Gaussian Utility and Non-uniform Yea/Nay Positions       | `simulation/utility/01_sumulation_master.R`       | synthetic                |                                       |
| M.1      | 18        | Simulation Studies with WNOMINATE using Synthetic Roll Call Data Generated with Gaussian Utility and Non-uniform Yea/Nay Positions  | `simulation/utility/02_simulation_wn.R`           | synthetic                |                                       |
| M.1      | 19        | Simulation Studies with BIRT using Synthetic Roll Call Data Generated with Gaussian Utility and Non-uniform Yea/Nay Positions       | `simulation/utility/03_simulation_irt.R`          | synthetic                |                                       |
| M.2      | 20        | Simulation Studies with BMIM using Synthetic Roll Call Data Generated with Quadratic Utility and Non-uniform Yea/Nay Positions      | `simulation/utility/01_sumulation_master.R`       | synthetic                |                                       |
| M.2      | 21        | Simulation Studies with WNOMINATE using Synthetic Roll Call Data Generated with Quadratic Utility and Non-uniform Yea/Nay Positions | `simulation/utility/02_simulation_wn.R`           | synthetic                |                                       |
| M.2      | 22        | Simulation Studies with BIRT using Synthetic Roll Call Data Generated with Quadratic Utility and Non-uniform Yea/Nay Positions      | `simulation/utility/03_simulation_irt.R`          | synthetic                |                                       |
| N.1      | Table 2   | List of Roll Call Votes on Sherman Act Repeal                                                                                       | `congress/03_house_visualization.R`               | US Congress              |                                       |
| N.1      | 23        | Roll Call Votes of the 53rd US H.R. on Sherman Act Repeal                                                                           | `congress/03_house_visualization.R`               | US Congress              |                                       |
| N.2      | Table 3   | List of Roll Call Votes on Free Silver Override                                                                                     | `congress/03_house_visualization.R`               | US Congress              |                                       |
| N.2      | 24        | Roll Call Votes of the 53rd US H.R. on Free Silver Override                                                                         | `congress/03_house_visualization.R`               | US Congress              |                                       |
| O        | 25        | Roll Call Votes of the 53rd US H.R. on Two Silver Bills                                                                             | `congress/03_house_visualization.R`               | US Congress              |                                       |
| O        | 26        | Multidimensional Ideal Point Estimation of US House of Representatives using the Proposed Method                                    | `congress/05_house_trajectory.R`                  | US Congress              |                                       |
| O        | 27        | Multidimensional Ideal Point Estimation of US House of Representatives using DW-NOMINATE                                            | `congress/05_house_trajectory.R`                  | US Congress              |                                       |
| O        | 28        | Multidimensional Ideal Point Estimation of US House of Representatives using Bayesian IRT                                           | `congress/05_house_trajectory.R`                  | US Congress              |                                       |
| P        | 29        | Multidimensional Ideal Point Estimation of the 52nd to 55th US House of Represen- tatives (1891-1899)                               | `congress/08_house_visualization_dynamic.R`       |                          |                                       |

## Congress

-   The `congress` folder includes R scripts for generating figures for the US Congress analysis.
-   Below is a list of R scripts and their descriptions:
    1.  `01_house_preprocess.R`: Downloads and preprocesses the US House of Representatives data from the voteview website. The pre-processed data is saved in the `data/congress/Rdata` folder.
    2.  `02_house_l1ideal.R`: Fits the proposed model to the US House of Representatives data. Since this code involves running slice sampler, it may take a while to run. The results are saved in the `data/congress/results` folder.
    3.  `03_house_visualization.R`: Generates main figures and tables. Figures 3 and 7 (main), Tables 2 and 3 (Appendix), and Figures 3, 23, 24, 25 (Appendix).
    4.  `04_house_irt.R`: Fits Bayesian IRT for comparison. Since this code involves running MCMC, it may take a while to run. The results are saved in the `data/congress/irt` folder.
    5.  `05_house_trajectory.R`: Generates the trajectory plot for the US House of Representatives from the 52nd to 55th Congress, with comparisons to DW-NOMINATE and Bayesian IRT. Figures 8 (main) and 26-28 (Appendix).
    6.  `06_house_preprocess_dynamic.R`: As an additional analysis, we run a single model for the entire period for dynamic analysis. This code preprocesses the data and saves the results in the `data/congress/Rdata` folder.
    7.  `07_house_l1ideal_dynamic.R`: Fits the proposed model to the US House of Representatives data for the entire period. The results are saved in the `data/congress/results` folder.
    8.  `08_house_visualization_dynamic.R`: Generates main figures and tables for the dynamic analysis. Figure 29 (Appendix).

## Illustration

-   The `illustration` folder includes R scripts for generating the figures used for illustration.
-   Below is a list of R scripts and their descriptions:
    1.  `01_example1.R`: Generates figures for Example 1 in Section 2. It generates synthetic data for a small chamber with two uncorrelated dimensions and fits the model using the proposed method, W-NOMINATE, and Bayesian IRT for comparison. Figures 1 and 5 illustrate the core problem and idea of the paper.
    2.  `02_example2.R`: Generates figures for Example 2 in Section 2. It generates synthetic data for two voting clusters with correlated dimensions and fits the model with the proposed method, W-NOMINATE, and Bayesian IRT for comparison. It generates Figure 2 to illustrate the rotational invariance problem.
    3.  `03_rotational_invariance.R`: Generates figures illustrating the rotational invariance problem. It generates Figure 4.
    4.  `04_cutting_line.R`: Generates figures illustrating the cutting line in l1 norm space. It generates Figure 1 in Appendix F and Figure 2 in Appendix G.

## Simulation

In the paper, we compare the proposed method with W-NOMINATE and Bayesian IRT using simulation studies. The simulation studies are conducted with the following settings:

-   

    (a) non-partisan; `simulation1` in the code. The data can be generated using the following code:

    ``` r
    dat <- l1ideal::generate.data(dimensions = 2, 
                                  n1 = 100, 
                                  n2 = 0, 
                                  n3 = 0, 
                                  n4 = 0,
                                  m = 1000,
                                  mu1 = c(0,0), 
                                  sigma1 = 0.7, 
                                  theta = 1, 
                                  seed = 1)
    ```

-   

    (b) two-party; `simulation2` in the code. The data can be generated using the following code:

    ``` r
    dat <- l1ideal::generate.data(dimensions = 2, 
                                  n1 = 50, 
                                  n2 = 50, 
                                  n3 = 0, 
                                  n4 = 0,
                                  m = 1000,
                                  mu1 = c(0.7,0.7), 
                                  mu2 = c(-0.7,-0.7), 
                                  sigma1 = 0.5, 
                                  sigma2 = 0.5, 
                                  theta = 1, 
                                  seed = 2)
    ```

-   

    (c) multi-party; `simulation3` in the code. The data can be generated using the following code:

    ``` r
    dat <- l1ideal::generate.data(dimensions = 2, 
                                  n1 = 40, 
                                  n2 = 40, 
                                  n3 = 10, 
                                  n4 = 10,
                                  m = 1000,
                                  mu1 = c(1,1), 
                                  mu2 = c(-1,-1), 
                                  mu3 = c(1.2,-1.2), 
                                  mu4 = c(-1.2,1.2),
                                  sigma1 = 0.5, 
                                  sigma2 = 0.5, 
                                  sigma3 = 0.2, 
                                  sigma4 = 0.2, 
                                  theta = 1, 
                                  seed = 3)
    ```

We have three different simulation studies:

1.  **Main**: Simulation studies with different settings (a) to (c) in the main text.
2.  **Appendix I**: Simulation studies with different tuning parameter `c` values using setting (a).
3.  **Appendices L and M**: Simulation studies with different data generating processes across settings (a) to (c).

### Main

-   The `simulation` folder includes `helper.R`, which contains helper functions for simulation studies, and R scripts for running the simulation studies in the main text.
-   Below is a list of R scripts and their descriptions:
    1.  `01_simulation_master.R`: Runs simulation studies with settings (a) to (c) in the main text. It runs `01_simulation1.R`, `02_simulation2.R`, and `03_simulation3.R` for each setting, and generates Figure 6 in the main text. It also generates Figures 4, 6 (first panel), and 8 in the Appendix.
    2.  `02_simulation_wn.R`: Runs simulation studies with W-NOMINATE and generates Figure 9 in the Appendix.
    3.  `03_simulation_irt.R`: Runs simulation studies with Bayesian IRT and generates Figure 10 in the Appendix.

### Appendix I: Tuning parameter `c`

-   The `simulation/tuning_param` folder includes R scripts for the simulation studies in Appendix I. These studies vary the tuning parameter `c` and use the first setting (non-partisan).
-   Below is a list of R scripts and their descriptions:
    1.  `01_simulation_c_small.R`: Runs simulation studies with a small `c` value (c = 1).
    2.  `02_simulation_c_large.R`: Runs simulation studies with a large `c` value (c = 8).
    3.  `03_simulation_results.R`: Generates figures for the simulation studies with different `c` values. It generates Figures 6 and 7 in Appendix I.
-   This folder also includes two versions of the `l1ideal` R package for different `c` values:
    -   `l1ideal_1.4.tar.gz`: For small `c` value (c = 1).
    -   `l1ideal_1.5.tar.gz`: For large `c` value (c = 8).

### Appendices L and M: Different Data Generating Processes

-   The `simulation/utility` folder includes R scripts for simulation studies in Appendices L and M. These studies modify the utility function (either Gaussian or quadratic) and Yea/Nay positions (either uniform or non-uniform). A total of 12 simulation studies are included (3 different data generating processes x 2 different utility functions x 2 different Yea/Nay positions).
-   Below is a list of R scripts and their descriptions:
    1.  `01_sumulation_master.R`: Master R script for running simulation studies with BMIM. It generates Table 1, Figures 11, 14, 17, and 20 in the Appendix.
    2.  `02_simulation_wn.R`: Runs simulation studies with W-NOMINATE and generates Figures 12, 15, 18, and 21 in the Appendix.
    3.  `03_simulation_irt.R`: Runs simulation studies with Bayesian IRT and generates Figures 13, 16, 19, and 22 in the Appendix.
