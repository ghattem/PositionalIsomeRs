PositionalIsomeRs
=================
An R package that produces plots to help positively identify positional isomers in post-transcriptionally 
modified (PTM) ribonucleosides (RNs).

Scans in the specified MS3 files are filtered by the MS1 full scan parameters. 
From each of the matching scans the masses closest to two user specified values 
(default value 108 and 109) are identified to compare the intensity values. 
Plots are created to visualize the intensity vs the retention time for both specified masses. 
Plots are also created to compare the log2 ratio of the intensity vs the mass.
