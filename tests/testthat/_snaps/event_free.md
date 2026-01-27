# s_event_free works with default arguments

    Code
      res
    Output
      $pt_at_risk
      [1] 159
      attr(,"label")
      [1] "Patients remaining at risk"
      
      $event_free_rate
      [1] 82.21113
      attr(,"label")
      [1] "Event Free Rate (%)"
      
      $rate_se
      [1] 2.72844
      attr(,"label")
      [1] "Standard Error of Event Free Rate"
      
      $rate_ci
      [1] 76.86349 87.55878
      attr(,"label")
      [1] "95% CI"
      
      $event_free_rate_3d
      [1] 82.21113 76.86349 87.55878
      attr(,"label")
      [1] "Event Free Rate (95% CI)"
      
      $event_free_ci
      [1] 0.8221113 0.7686349 0.8755878
      attr(,"label")
      [1] "6-month event-free rate (95% CI)"
      

# s_event_free works with percent format

    Code
      res
    Output
      $pt_at_risk
      [1] 159
      attr(,"label")
      [1] "Patients remaining at risk"
      
      $event_free_rate
      [1] 82.21113
      attr(,"label")
      [1] "Event Free Rate (%)"
      
      $rate_se
      [1] 2.72844
      attr(,"label")
      [1] "Standard Error of Event Free Rate"
      
      $rate_ci
      [1] 76.86349 87.55878
      attr(,"label")
      [1] "95% CI"
      
      $event_free_rate_3d
      [1] 82.21113 76.86349 87.55878
      attr(,"label")
      [1] "Event Free Rate (95% CI)"
      
      $event_free_ci
      [1] 82.21113 76.86349 87.55878
      attr(,"label")
      [1] "6-month event-free rate (%) (95% CI)"
      

# a_event_free works with default arguments in a table layout

    Code
      res
    Output
                                              ARM A               ARM B               ARM C      
      ———————————————————————————————————————————————————————————————————————————————————————————
      3-week event-free rate (95% CI)   0.93 (0.86, 0.99)   0.89 (0.82, 0.96)   0.86 (0.77, 0.95)
      4-week event-free rate (95% CI)   0.88 (0.80, 0.96)   0.86 (0.78, 0.94)   0.83 (0.73, 0.92)
      5-week event-free rate (95% CI)   0.88 (0.80, 0.96)   0.84 (0.75, 0.92)   0.79 (0.69, 0.90)

# a_event_free works with customized arguments in a table layout

    Code
      res
    Output
                                                     ARM A                  ARM B                  ARM C        
      ——————————————————————————————————————————————————————————————————————————————————————————————————————————
        5-week event-free rate (%) (90% CI)   88.00 (79.54, 93.11)   83.56 (74.91, 89.44)   79.09 (68.54, 86.45)
        1-week event-free rate (%) (90% CI)   95.65 (89.14, 98.30)   94.52 (87.96, 97.55)   93.10 (84.99, 96.91)
        7-week event-free rate (%) (90% CI)   84.89 (75.88, 90.73)   79.43 (70.29, 86.03)   75.50 (64.59, 83.47)

