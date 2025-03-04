This "README.txt" file was generated on 2022-11-15 by Pierre Gianferrara

GENERAL INFORMATION

1. Title of Dataset: Periodic Tapping Skill Learning - Code

2. Author Information
	A. Principal Investigator Contact Information
		Name: John. R. Anderson
		Institution: Carnegie Mellon University
		Address: Baker Hall 3rd floor, 4909 Frew St, Pittsburgh, PA 15213
		Email: ja0s@andrew.cmu.edu

	B. First Author Contact Information
		Name: Pierre Gianferrara
		Institution: Carnegie Mellon University
		Address: Baker Hall 3rd floor, 4909 Frew St, Pittsburgh, PA 15213
		Email: pgianfer@andrew.cmu.edu

	C. Contact for ACT-R related questions
		Name: Dan Bothell
		Institution: Carnegie Mellon University
		Address: Baker Hall 3rd floor, 4909 Frew St, Pittsburgh, PA 15213
		Email: db30@andrew.cmu.edu
		
3. Time of human data collection: START: 2019-08-21, END: 2022-06-06

4. Platform where human data was collected: Amazon mechanical Turk
   Platform where model data was generated: Anderson Lab cluster through "act-r-sf" (code for Space Fortress related ACT-R model runs)
   Software download: http://act-r.psy.cmu.edu/software/
   Ask Dan Bothell (db30@andrew.cmu.edu) for more information

5. Information about funding sources that supported the collection of the data: 
Office of Naval Research Grant N00014-21-1-2586 and AFOSR/AFRL award FA9550-18-1-0251.

SHARING/ACCESS INFORMATION

1. Licenses/restrictions placed on the data:
Users have the right to download and use this code, but are required to cite Gianferrara, Betts, and Anderson (2023) when doing so.

2. Links to other publicly accessible locations of the data: 
KiltHub (CMU repository)
Gianferrara, Pierre; Betts, Shawn; Anderson, John (2022). Periodic Tapping Mechanisms of Skill Learning in a Fast-Paced Video Game. Carnegie Mellon University. Dataset. https://doi.org/10.1184/R1/21599775.v2
https://kilthub.cmu.edu/articles/dataset/Periodic_Tapping_Mechanisms_of_Skill_Learning_in_a_Fast-Paced_Video_Game/21599775

3. Was data derived from another source? Some of the fast human data were presented at the International Conference on Cognitive Modeling in 2020 (see Gianferrara, Betts, & Anderson, 2020).

4. Recommended citation for this dataset: 
Gianferrara, P. G., Betts, S. A., & Anderson, J. R. (2024). Periodic tapping mechanisms of skill learning in a fast-paced video game. Journal of Experimental Psychology: Human Perception and Performance, 50(1), 39.

DATA & FILE OVERVIEW

1. ACT-R - Folder containing the 3 different ACT-R models (*.lisp) that were used to generate the paper data using act-r-sf on the lab cluster. The 4 Lisp files are as follows:

1.1 "MS17_cluster.lisp" - Baseline ACT-R model (from PLOS ONE 2021 paper)

1.2 "streamed_cluster.lisp" - streamed ACT-R model at fast speed

1.3 "coupled_cluster.lisp" - coupled ACT-R model at fast speed

1.4 "periodic-tap_correction.lisp" - Periodic tapping motor extension

2. Demographics - Data & python script that was used to compute the demographic information for this project

3. Inputs - Folder containing all the input data that was used for the analyses in this paper. This folder contains 4 sub-folders as follows:

3.1 Humans_Part_I - All measure files from fast speed groups of humans

3.2 Humans_Part_II - Data (measure files & R spreadsheet) across the coupled & streamed strategies at fast speed

3.3 Models_Part_I - Measure files from the baseline model at the fast game speed

3.4 Models_Part_II - Data (measure files & R spreadsheet) from the baseline, coupled, and streamed ACT-R models at fast speed

4. Analyses_Part_I - R files (with data from "Humans_Part_I") and scripts that were used for baseline model & human comparison at fast speed

5. Analyses_Part_II - Human strategy comparison across streamed & coupled strategies + results from fast-paced ACT-R models of human learning in each strategy