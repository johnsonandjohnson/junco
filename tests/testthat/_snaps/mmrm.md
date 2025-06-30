# get_mmrm_lsmeans can calculate the LS mean results including one- and two-sided p-values

    Code
      result
    Output
      $estimates
         AVISIT ARMCD estimate        se       df lower_cl upper_cl  n
      1    VIS1   PBO 32.70499 0.7805593 146.5629 31.16239 34.24760 68
      2    VIS2   PBO 37.60152 0.6364633 153.4469 36.34415 38.85888 69
      3    VIS3   PBO 43.01353 0.5276137 132.8329 41.96992 44.05714 71
      4    VIS4   PBO 47.97237 1.2198440 135.4067 45.55996 50.38478 67
      5    VIS1   TRT 37.17016 0.7954736 145.3672 35.59797 38.74235 66
      6    VIS2   TRT 41.80098 0.6335350 150.8373 40.54923 43.05273 71
      7    VIS3   TRT 46.65448 0.5813476 133.4308 45.50463 47.80433 58
      8    VIS4   TRT 52.94055 1.2233253 134.4184 50.52109 55.36000 67
      9  VIS1+3   PBO 37.85926 0.5171213 171.0982 36.83850 38.88002 94
      10 VIS1+3   TRT 41.91232 0.5413954 171.8779 40.84368 42.98096 85
      11 VIS2+4   PBO 42.78694 0.7307410 167.6085 41.34430 44.22958 91
      12 VIS2+4   TRT 47.37076 0.7360039 159.3076 45.91718 48.82435 88
      
      $contrasts
        ARMCD AVISIT estimate        se       df lower_cl upper_cl   t_stat
      1   TRT   VIS1 4.465163 1.1144735 145.9847 2.262576 6.667749 4.006522
      2   TRT   VIS2 4.199462 0.8980269 152.2464 2.425259 5.973666 4.676322
      3   TRT   VIS3 3.640953 0.7850740 133.1977 2.088128 5.193778 4.637719
      4   TRT   VIS4 4.968179 1.7275833 134.9465 1.551539 8.384820 2.875797
      5   TRT VIS1+3 4.053058 0.7486811 171.5969 2.575247 5.530868 5.413597
      6   TRT VIS2+4 4.583821 1.0371519 163.4604 2.535878 6.631763 4.419623
             p_value p_value_less p_value_greater relative_reduc
      1 9.784425e-05    0.9999511    4.892213e-05     -0.1365285
      2 6.392000e-06    0.9999968    3.196000e-06     -0.1116833
      3 8.310860e-06    0.9999958    4.155430e-06     -0.0846467
      4 4.685098e-03    0.9976575    2.342549e-03     -0.1035634
      5 2.056257e-07    0.9999999    1.028129e-07     -0.1070559
      6 1.794775e-05    0.9999910    8.973874e-06     -0.1071313
      
      attr(,"averages")
      attr(,"averages")$`VIS1+3`
      [1] "VIS1" "VIS3"
      
      attr(,"averages")$`VIS2+4`
      [1] "VIS2" "VIS4"
      
      attr(,"weights")
      [1] "counterfactual"

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

