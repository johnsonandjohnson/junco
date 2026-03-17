/* Import CSV dataset */
PROC IMPORT DATAFILE="./dev/example_data2.csv"
            OUT=work.mydata
            DBMS=CSV 
            REPLACE;
    GETNAMES=YES;  /* Assumes the first row contains variable names */
RUN;

PROC FORMAT;
    VALUE $order_fmt
        'Placebo' = '2'
        'Treatment' = '1';
RUN;

/* Frequency Analysis with Relative Risk and Confidence Interval */
proc freq data=work.mydata order=formatted;
    FORMAT grp $order_fmt.;
    tables f1 * f2 * grp * rsp / cmh relrisk alpha=0.1;
run;