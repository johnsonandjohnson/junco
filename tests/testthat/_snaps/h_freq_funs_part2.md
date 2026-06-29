# layout with horizontal risk diff columns, ref_path spec 'colspan_trt' '' 'ARM' 'B: Placebo'

    Code
      tbl
    Output
                                 Active Study Agent                                                                    Risk Difference (%) (95% CI)                        
                     A: Drug X   C: Combination   Combined A + C   B: Placebo   All Patients   A: Drug X vs B: Placebo   C: Combination vs B: Placebo   Combined vs Placebo
                       N=134         N=132            N=266          N=134         N=400                N=134                       N=132                      N=266       
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      in_ref_col       FALSE         FALSE            FALSE           TRUE                                                                                                 
      vs_ref_stats     FALSE         FALSE            FALSE          FALSE         FALSE                TRUE                         TRUE                      TRUE        

# layout with horizontal risk diff columns, incomplete ref_path 'ARM' 'B: Placebo'

    Code
      tbl
    Output
                                 Active Study Agent                                                                    Risk Difference (%) (95% CI)                        
                     A: Drug X   C: Combination   Combined A + C   B: Placebo   All Patients   A: Drug X vs B: Placebo   C: Combination vs B: Placebo   Combined vs Placebo
                       N=134         N=132            N=266          N=134         N=400                N=134                       N=132                      N=266       
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      in_ref_col                                                                                                                                                           
      vs_ref_stats     FALSE         FALSE            FALSE          FALSE         FALSE                TRUE                         TRUE                      TRUE        

# layout with horizontal risk diff columns, ref_path spec 'colspan_trt' '' 'ARM' 'B: Placebo' but vertical riskdiff_setup

    Code
      tbl
    Output
                                 Active Study Agent                                                                    Risk Difference (%) (95% CI)                        
                     A: Drug X   C: Combination   Combined A + C   B: Placebo   All Patients   A: Drug X vs B: Placebo   C: Combination vs B: Placebo   Combined vs Placebo
                       N=134         N=132            N=266          N=134         N=400                N=134                       N=132                      N=266       
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      in_ref_col       FALSE         FALSE            FALSE           TRUE                                                                                                 
      vs_ref_stats     TRUE           TRUE             TRUE          FALSE         FALSE                FALSE                       FALSE                      FALSE       

# layout with core columns, and vertical riskdiff_setup

    Code
      tbl
    Output
                                 Active Study Agent                                         
                     A: Drug X   C: Combination   Combined A + C   B: Placebo   All Patients
                       N=134         N=132            N=266          N=134         N=400    
      ——————————————————————————————————————————————————————————————————————————————————————
      in_ref_col       FALSE         FALSE            FALSE           TRUE                  
      vs_ref_stats     TRUE           TRUE             TRUE          FALSE         FALSE    

# layout with core columns, vertical riskdiff_setup riskdiff = FALSE

    Code
      tbl
    Output
                                 Active Study Agent                                         
                     A: Drug X   C: Combination   Combined A + C   B: Placebo   All Patients
                       N=134         N=132            N=266          N=134         N=400    
      ——————————————————————————————————————————————————————————————————————————————————————
      in_ref_col       FALSE         FALSE            FALSE           TRUE                  
      vs_ref_stats     FALSE         FALSE            FALSE          FALSE         FALSE    

# layout with core columns, vertical riskdiff_setup incomplete ref_path spec 'ARM' 'B: Placebo'

    Code
      tbl
    Output
                                 Active Study Agent                                         
                     A: Drug X   C: Combination   Combined A + C   B: Placebo   All Patients
                       N=134         N=132            N=266          N=134         N=400    
      ——————————————————————————————————————————————————————————————————————————————————————
      in_ref_col                                                                            
      vs_ref_stats     FALSE         FALSE            FALSE          FALSE         FALSE    

