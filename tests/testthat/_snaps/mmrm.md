# fit_mmrm_j works as expected

    Code
      fit
    Output
      $fit
      mmrm fit
      
      Formula:     FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
      Data:        data (used 537 observations from 197 subjects with maximum 4 
      timepoints)
      Weights:     weights
      Covariance:  unstructured (10 variance parameters)
      Inference:   REML
      Deviance:    3386.45
      
      Coefficients: 
                        (Intercept) RACEBlack or African American 
                        30.77747548                    1.53049977 
                          RACEWhite                     SEXFemale 
                         5.64356535                    0.32606192 
                           ARMCDTRT                    AVISITVIS2 
                         3.77423004                    4.83958845 
                         AVISITVIS3                    AVISITVIS4 
                        10.34211288                   15.05389826 
                ARMCDTRT:AVISITVIS2           ARMCDTRT:AVISITVIS3 
                        -0.04192625                   -0.69368537 
                ARMCDTRT:AVISITVIS4 
                         0.62422703 
      
      Model Inference Optimization:
      Converged with code 0 and message: convergence: rel_reduction_of_f <= factr*epsmch
      
      $cov_estimate
                VIS1      VIS2       VIS3       VIS4
      VIS1 40.553664 14.396045  4.9747288 13.3866534
      VIS2 14.396045 26.571483  2.7854661  7.4744790
      VIS3  4.974729  2.785466 14.8978517  0.9082111
      VIS4 13.386653  7.474479  0.9082111 95.5568420
      
      $lsmeans
      $lsmeans$estimates
         AVISIT ARMCD estimate        se       df lower_cl upper_cl  n
      1    VIS1   PBO 33.33186 0.7553974 148.1457 31.83912 34.82461 68
      2    VIS2   PBO 38.17145 0.6117319 147.0330 36.96253 39.38037 69
      3    VIS3   PBO 43.67397 0.4617621 129.8027 42.76042 44.58753 71
      4    VIS4   PBO 48.38576 1.1886495 134.0814 46.03483 50.73669 67
      5    VIS1   TRT 37.10609 0.7625875 143.1765 35.59871 38.61348 66
      6    VIS2   TRT 41.90375 0.6023487 143.4844 40.71313 43.09438 71
      7    VIS3   TRT 46.75452 0.5086319 130.1341 45.74826 47.76078 58
      8    VIS4   TRT 52.78422 1.1877596 132.6244 50.43481 55.13362 67
      9  VIS1+2   PBO 35.75166 0.5583307 184.5838 34.65013 36.85319 91
      10 VIS1+2   TRT 39.50492 0.5605040 172.2042 38.39858 40.61127 87
      
      $lsmeans$contrasts
        ARMCD AVISIT estimate        se       df lower_cl upper_cl   t_stat
      1   TRT   VIS1 3.774230 1.0741470 145.5520 1.651290 5.897170 3.513700
      2   TRT   VIS2 3.732304 0.8588564 145.2771 2.034836 5.429771 4.345667
      3   TRT   VIS3 3.080545 0.6896245 130.9280 1.716296 4.444793 4.466988
      4   TRT   VIS4 4.398457 1.6805453 133.3887 1.074493 7.722422 2.617280
      5   TRT VIS1+2 3.753267 0.7917532 177.7274 2.190820 5.315714 4.740450
             p_value p_value_less p_value_greater relative_reduc
      1 5.891481e-04    0.9997054    2.945740e-04    -0.11323190
      2 2.594462e-05    0.9999870    1.297231e-05    -0.09777736
      3 1.697047e-05    0.9999915    8.485234e-06    -0.07053502
      4 9.886854e-03    0.9950566    4.943427e-03    -0.09090396
      5 4.355241e-06    0.9999978    2.177621e-06    -0.10498163
      
      attr(,"averages")
      attr(,"averages")$`VIS1+2`
      [1] "VIS1" "VIS2"
      
      attr(,"weights")
      [1] "equal"
      attr(,"mult_adj")
      [1] "none"
      
      $vars
      $vars$response
      [1] "FEV1"
      
      $vars$covariates
      [1] "RACE" "SEX" 
      
      $vars$id
      [1] "USUBJID"
      
      $vars$arm
      [1] "ARMCD"
      
      $vars$visit
      [1] "AVISIT"
      
      
      $labels
      $labels$response
        FEV1 
      "FEV1" 
      
      $labels$id
        USUBJID 
      "USUBJID" 
      
      $labels$visit
        AVISIT 
      "AVISIT" 
      
      $labels$arm
        ARMCD 
      "ARMCD" 
      
      $labels$parts
        RACE    SEX 
      "RACE"  "SEX" 
      
      
      $mse
          VIS1     VIS2     VIS3     VIS4 
      40.55366 26.57148 14.89785 95.55684 
      
      $df
          VIS1     VIS2     VIS3     VIS4 
      145.5520 145.2771 130.9280 133.3887 
      
      $cor_struct
      [1] "unstructured"
      
      $ref_level
      [1] "PBO"
      
      $treatment_levels
      [1] "TRT"
      
      $conf_level
      [1] 0.95
      
      $additional
      list()
      
      attr(,"class")
      [1] "tern_model"

# fit_mmrm_j works as expected with subgroup variable

    Code
      fit
    Output
      $fit
      mmrm fit
      
      Formula:     FEV1 ~ RACE + ARMCD * AVISIT * SEX + us(AVISIT | USUBJID)
      Data:        data (used 537 observations from 197 subjects with maximum 4 
      timepoints)
      Weights:     weights
      Covariance:  unstructured (10 variance parameters)
      Inference:   REML
      Deviance:    3351.999
      
      Coefficients: 
                        (Intercept) RACEBlack or African American 
                         28.7521822                     1.7175531 
                          RACEWhite                      ARMCDTRT 
                          5.7594630                     6.0137026 
                         AVISITVIS2                    AVISITVIS3 
                          6.9645335                    11.6396679 
                         AVISITVIS4                     SEXFemale 
                         16.2667278                     3.8010802 
                ARMCDTRT:AVISITVIS2           ARMCDTRT:AVISITVIS3 
                         -0.7561641                    -2.0371827 
                ARMCDTRT:AVISITVIS4            ARMCDTRT:SEXFemale 
                         -0.9542883                    -3.9559505 
               AVISITVIS2:SEXFemale          AVISITVIS3:SEXFemale 
                         -3.9656654                    -2.1865741 
               AVISITVIS4:SEXFemale ARMCDTRT:AVISITVIS2:SEXFemale 
                         -2.1357370                     1.4422441 
      ARMCDTRT:AVISITVIS3:SEXFemale ARMCDTRT:AVISITVIS4:SEXFemale 
                          2.2723999                     2.6845678 
      
      Model Inference Optimization:
      Converged with code 0 and message: convergence: rel_reduction_of_f <= factr*epsmch
      
      $cov_estimate
                VIS1      VIS2       VIS3       VIS4
      VIS1 39.769034 14.628054  4.0556822 11.5527284
      VIS2 14.628054 25.479025  2.1728946  7.6266820
      VIS3  4.055682  2.172895 14.7359667  0.2977923
      VIS4 11.552728  7.626682  0.2977923 96.7804544
      
      $lsmeans
      $lsmeans$estimates
            SEX AVISIT ARMCD estimate        se       df lower_cl upper_cl  n
      1    Male   VIS1   PBO 31.24452 1.1161228 146.1296 29.03869 33.45035 30
      2  Female   VIS1   PBO 35.04560 1.0037299 143.9018 33.06164 37.02956 38
      3    Male   VIS2   PBO 38.20905 0.8407085 146.6290 36.54758 39.87053 35
      4  Female   VIS2   PBO 38.04447 0.8467263 150.3702 36.37145 39.71749 34
      5    Male   VIS3   PBO 42.88419 0.6523134 127.2499 41.59340 44.17497 35
      6  Female   VIS3   PBO 44.49869 0.6466327 128.4731 43.21927 45.77812 36
      7    Male   VIS4   PBO 47.51125 1.7071218 131.7030 44.13432 50.88817 33
      8  Female   VIS4   PBO 49.17659 1.6796401 131.7439 45.85404 52.49915 34
      9    Male   VIS1   TRT 37.25822 1.0894731 141.2086 35.10444 39.41201 32
      10 Female   VIS1   TRT 37.10335 1.0465740 142.7722 35.03457 39.17214 34
      11   Male   VIS2   TRT 43.46659 0.8984293 146.6742 41.69105 45.24213 30
      12 Female   VIS2   TRT 40.78830 0.7774319 143.1776 39.25157 42.32503 41
      13   Male   VIS3   TRT 46.86071 0.7994530 126.6912 45.27870 48.44272 23
      14 Female   VIS3   TRT 46.79166 0.6489339 126.4344 45.50749 48.07584 35
      15   Male   VIS4   TRT 52.57066 1.7589580 130.4640 49.09089 56.05043 31
      16 Female   VIS4   TRT 52.96462 1.6316457 130.5006 49.73672 56.19252 36
      
      $lsmeans$contrasts
           SEX ARMCD AVISIT estimate        se       df   lower_cl upper_cl   t_stat
      1   Male   TRT   VIS1 6.013703 1.5606634 144.3334  2.9289946 9.098411 3.853299
      2 Female   TRT   VIS1 2.057752 1.4510589 143.2825 -0.8104963 4.926001 1.418104
      3   Male   TRT   VIS2 5.257539 1.2323788 147.4096  2.8221266 7.692951 4.266171
      4 Female   TRT   VIS2 2.743832 1.1491301 146.8420  0.4728627 5.014802 2.387747
      5   Male   TRT   VIS3 3.976520 1.0363888 128.1256  1.9258668 6.027173 3.836900
      6 Female   TRT   VIS3 2.292969 0.9180302 127.8695  0.4764720 4.109467 2.497706
      7   Male   TRT   VIS4 5.059414 2.4510202 130.9716  0.2107019 9.908127 2.064208
      8 Female   TRT   VIS4 3.788032 2.3417867 131.1686 -0.8445255 8.420589 1.617582
             p_value p_value_less p_value_greater relative_reduc
      1 1.747911e-04    0.9999126    8.739556e-05    -0.19247223
      2 1.583317e-01    0.9208342    7.916584e-02    -0.05871642
      3 3.539161e-05    0.9999823    1.769580e-05    -0.13759929
      4 1.822401e-02    0.9908880    9.112003e-03    -0.07212171
      5 1.946630e-04    0.9999027    9.733148e-05    -0.09272695
      6 1.376957e-02    0.9931152    6.884784e-03    -0.05152891
      7 4.097196e-02    0.9795140    2.048598e-02    -0.10648877
      8 1.081551e-01    0.9459225    5.407753e-02    -0.07702916
      
      attr(,"averages")
      list()
      attr(,"weights")
      [1] "equal"
      attr(,"mult_adj")
      [1] "none"
      
      $vars
      $vars$response
      [1] "FEV1"
      
      $vars$covariates
      [1] "RACE"
      
      $vars$id
      [1] "USUBJID"
      
      $vars$arm
      [1] "ARMCD"
      
      $vars$visit
      [1] "AVISIT"
      
      $vars$subgroup
      [1] "SEX"
      
      
      $labels
      $labels$response
        FEV1 
      "FEV1" 
      
      $labels$id
        USUBJID 
      "USUBJID" 
      
      $labels$visit
        AVISIT 
      "AVISIT" 
      
      $labels$arm
        ARMCD 
      "ARMCD" 
      
      $labels$parts
        RACE 
      "RACE" 
      
      $labels$subgroup
        SEX 
      "SEX" 
      
      
      $mse
          VIS1     VIS2     VIS3     VIS4 
      39.76903 25.47903 14.73597 96.78045 
      
      $df
          VIS1     VIS2     VIS3     VIS4 
      144.3334 147.4096 128.1256 130.9716 
      
      $cor_struct
      [1] "unstructured"
      
      $ref_level
      [1] "PBO"
      
      $treatment_levels
      [1] "TRT"
      
      $conf_level
      [1] 0.95
      
      $additional
      list()
      
      attr(,"class")
      [1] "tern_model"

