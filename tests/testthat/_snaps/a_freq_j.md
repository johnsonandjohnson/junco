# a_freq_j with label_map works in a table layout as expected

    Code
      result
    Output
                          A               B      
      ———————————————————————————————————————————
      No Response   30/50 (60.0%)   28/50 (56.0%)
      Response      20/50 (40.0%)   22/50 (44.0%)

# a_freq_j with label_map restricts the values according to row split and label_map

    Code
      result
    Output
                          A              B      
      ——————————————————————————————————————————
      Baseline                                  
        Response A   6/50 (12.0%)   4/50 (8.0%) 
        Response B   6/50 (12.0%)   4/50 (8.0%) 
      Week 1                                    
        Response C   3/50 (6.0%)    7/50 (14.0%)
        Response D   9/50 (18.0%)   7/50 (14.0%)

# a_freq_j_with_exclude allows to exclude row split levels from the analysis

    Code
      result
    Output
                          A              B     
      —————————————————————————————————————————
      Baseline                                 
        Response A   6/50 (12.0%)   4/50 (8.0%)
        Response B   6/50 (12.0%)   4/50 (8.0%)

