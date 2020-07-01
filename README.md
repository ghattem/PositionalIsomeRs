PositionalIsomeRs
=================
An R package that produces plots to help positively identify positional isomers in post-transcriptionally 
modified (PTM) ribonucleosides (RNs) and small molecule analysis.

All MS3 files in the current directory are read into R. Scans in the specified MS3 files 
are filtered by the MS1 full scan parameters. By default the files are filtered for a precursor
ion mass of 282.12 and a product m/z of 150.08. These values can also be specified by the user.
From each of the matching scans the masses closest to two user specified values (default value 108 and 109)
are identified to compare the intensity values. Plots are created to visualize the intensity vs the retention time for 
both specified masses. Plots are also created to compare the log2 ratio of the intensity vs the mass.
