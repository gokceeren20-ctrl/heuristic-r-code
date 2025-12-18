1. Required Environment

R version: R 4.0 or higher
Operating system: Windows / macOS / Linux
Required R libraries:

-readxl
-dplyr
-lubridate

Required libraries can be installed using:

install.packages(c("readxl", "dplyr", "lubridate"))



2. Repository Structure

The repository contains the following files:

iteration.R : Main script containing greedy initialization, Tabu Search algorithm, and feasibility checks
seferler.xlsx : Input dataset containing trip information
code explanation.txt



3. Input Data Description

The input file seferler.xlsx must include the following columns:

SeferID : Unique identifier of each trip
KalkisZamani : Departure time of the trip (date-time format)

The route direction and travel time are predefined in the code.
Each trip has a fixed travel duration of 3 hours, and the planning horizon covers one full week.



4. Parameter Settings

Key parameters defined in the script:

Number of drivers: 10
Trip duration: 3 hours
Minimum rest time between trips: 6 hours
Tabu Search iterations: 100
Tabu tenure: 5

These parameters can be modified at the beginning of the script to test different scenarios.



5. Running the Code

Open iteration.R in RStudio or any R environment

Update the file path of seferler.xlsx in the script:
dosya_yolu <- "path_to_your_folder/seferler.xlsx"

Run the script from top to bottom



6. Execution Flow

The code executes the following steps sequentially:

Reads and preprocesses trip data
Constructs an initial feasible solution using a greedy assignment heuristic
Improves the initial solution using Tabu Search
Prints iteration-wise current and best idle time values
Outputs the final best solution and its total idle time
Checks feasibility with respect to the minimum rest time constraint


7. Output Description

Console output:
Iteration-wise logs showing current and best idle times
Final best idle time value

Final solution:
Best driver-to-trip assignment stored in memory as best_solution

Feasibility check:
A detailed report indicating whether minimum rest-time constraints are satisfied

Idle time values are reported in hours.


8. Reproducibility Notes

The dataset is predominantly based on real operational data and partially synthetically completed to cover the full planning horizon.
Due to the randomized nature of the Tabu Search neighborhood generation, results may slightly vary between runs.
Setting a random seed in R can be used to obtain reproducible results.

Example:
set.seed(123)

