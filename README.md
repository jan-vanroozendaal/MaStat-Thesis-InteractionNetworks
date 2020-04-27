# MaStat-Thesis-InteractionNetworks
Related to the dissertation "Simulating Joint Information Content to better estimate Interaction Networks" for the degree of Master of Science in Statistical Data Analysis (academic year 2019-20) at Ghent University.

Each of the R-scripts in the folder `R` can run under flexible settings of level of interaction information per triplet by changing the contents of the `ecov` vector.

The folder `Workspaces` contains a total of 252 separate workspaces, each containing a list of results from 1000 simulations. A breakdown of the workspaces is done as follows:
* 7 model layouts
* For each model layout, 12 threshold levels (False, 0.005, 0.01, 0.015, 0.02, 0.025, 0.03, 0.035, 0.04, 0.045, 0.05, True)
* For each threshold level, 3 cases: full synergy, full redundancy or full zero-interaction.
    + 7 * 12 * 3 = 252 unique scenarios

The folder `Visualization` uses combined results from those 252 workspaces to create a bigger, single workspace per visualization topic.
