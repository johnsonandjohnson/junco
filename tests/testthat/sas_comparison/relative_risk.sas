/* Example Data Set Creation */
data example_data;
    input Patient_ID Treatment $ Gender $ Age_Group $ Response;
    datalines;
1  A Male  Young 1
2  A Male  Young 0
3  A Female Young 0
4  A Female Young 1
5  A Male  Old 1
6  A Female Old 1
7  A Male  Old 0
8  A Female Old 0
9  B Male  Young 1
10 B Male  Young 0
11 B Female Young 1
12 B Female Young 0
13 B Male  Old 1
14 B Female Old 0
15 B Male  Old 1
16 B Female Old 1
17 A Male  Young 0
18 A Male  Young 1
19 A Female Young 1
20 A Female Young 0
21 A Male  Old 1
22 A Female Old 1
23 A Male  Old 0
24 A Female Old 0
25 B Male  Young 0
26 B Male  Young 1
27 B Female Young 1
28 B Female Young 0
29 B Male  Old 1
30 B Female Old 1
31 B Male  Old 0
32 B Female Old 0
33 A Male  Young 1
34 A Male  Young 0
35 A Female Young 1
36 A Female Young 0
37 A Male  Old 0
38 A Female Old 1
39 A Male  Old 1
40 A Female Old 0
41 B Male  Young 1
42 B Male  Young 0
43 B Female Young 1
44 B Female Young 0
45 B Male  Old 1
46 B Female Old 0
47 B Male  Old 1
48 B Female Old 1
49 A Male  Young 0
50 A Female Young 1
;

/* Sort the data by Gender and Age_Group */
proc sort data=example_data;
    by Gender Age_Group;
run;

/* Frequency Analysis with Relative Risk and Confidence Interval */
proc freq data=example_data;
    tables Gender * Age_Group *  Treatment * Response / cmh relrisk; 
run;

/* Export the results to a CSV file */
proc export data=example_data
    outfile="example_data.csv"
    dbms=csv
    replace;
run;
