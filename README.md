# Rhythm-ABR
Codes to run the experiment and analyse the raw data (Matlab) and code for visualisations (R)


Auditory Brainstem Response is a non-invasive technique to measure hearing thresholds (1,2). I adjusted the paradigm to not measure perception of 
sound frequencies but the perception of rhythm. For that end we play the same stimuli in different tempos (6Hz, 25 Hz, 44 Hz). 

Experiments where run on 78 individuals of 12 species of bats in Panama (free living bats) and on 20 bats of C.perspicillata in captivity in Germany.
We tested artificial stimuli in Panama and natural stimuli (Isolation Calls of C. perspicillata (3)) in Germany. 


Included in this repository are the codes to run the experiments in Matlab  including generating artificial stimuli, as well as the first analysis 
including a moving-minimum-substraction procedure (4), a trapezoidal integration and a resampling approach (5) to ensure measured signals are different from noise. 


examplary Literature
1) Jewett,D.L.; Romano, M.N. and Williston, J.S. (1970). Human auditory evoked potentials: Possible 465brain stem components detected on the scalp. Science 167, 1517–1518
2) Corwin, J.T.; Bullock, T.H. and Schweitzer, J. (1982). The auditory brain stem response in five 451vertebrate classes. Electroencephalography and Clinical Neurophysiology 54, 629–641
3) Knörnschild M, Feifel M, Kalko EKV. Mother-offspring recognition in the bat Carollia perspicillata. Animal Behaviour. 2013;86:941-8.
4) Källstrand J, Lewander T, Baghdassarian E, Nielzén S. A new method for analyzing auditory brain-stem response waveforms using a moving-minimum subtraction procedure of digitized analog recordings. Neuropsychiatric disease and treatment. 2014;10:1011-6.
5) Lv J, Simpson DM, Bell SL. Objective detection of evoked potentials using a bootstrap technique. Med Eng Phys. 2007;29(2):191-8.

