      SUBROUTINE UMAT(STRESS,STATEV,DDSDDE,SSE,SPD,SCD,
     1 RPL,DDSDDT,DRPLDE,DRPLDT,STRAN,DSTRAN,
     2 TIME,DTIME,TEMP,DTEMP,PREDEF,DPRED,MATERL,NDI,NSHR,NTENS,
     3 NSTATV,PROPS,NPROPS,COORDS,DROT,PNEWDT,CELENT,
     4 DFGRD0,DFGRD1,NOEL,NPT,KSLAY,KSPT,KSTEP,KINC)
C
      INCLUDE 'ABA_PARAM.INC'
C
      CHARACTER*8 MATERL
      DIMENSION STRESS(NTENS),STATEV(NSTATV),
     1 DDSDDE(NTENS,NTENS),DDSDDT(NTENS),DRPLDE(NTENS),
     2 STRAN(NTENS),DSTRAN(NTENS),DFGRD0(3,3),DFGRD1(3,3),
     3 TIME(2),PREDEF(1),DPRED(1),PROPS(NPROPS),COORDS(3),DROT(3,3)
C

C    LOCAL ARRAYS
C ----------------------------------------------------------------
C ----------------------------------------------------------------
C
      integer, parameter::model1_input_size = 6
      integer, parameter::model1_output_size = 1
      integer, parameter::model1_hidden_layers = 3
      integer, parameter::model1_hidden_size = 9
      real(kind=8), 
     *  dimension(model1_hidden_size, model1_input_size)::model1_w1 ! weight matrix from input layer to first hidden layer
      real(kind=8), dimension(model1_hidden_size)::model1_b1 ! bias vector for first hidden layer
      real(kind=8), 
     *  dimension(model1_hidden_size, model1_hidden_size)::model1_w2 ! weight matrix from hidden layer 1 to hidden layer 2
      real(kind=8), dimension(model1_hidden_size)::model1_b2 ! bias vector for hidden layer 2
      real(kind=8), 
     *  dimension(model1_hidden_size, model1_hidden_size)::model1_w3 ! weight matrix from hidden layer 2 to hidden layer 3
      real(kind=8), dimension(model1_hidden_size)::model1_b3 ! bias vector for hidden layer 3
      real(kind=8), 
     *  dimension(model1_hidden_size, model1_hidden_size)::model1_w4 ! weight matrix from hidden layer 3 to hidden layer 4
      real(kind=8), dimension(model1_hidden_size)::model1_b4 ! bias vector for hidden layer 4
      real(kind=8), 
     *  dimension(model1_output_size, model1_hidden_size)::model1_w5 ! weight matrix from last hidden layer to output layer
      real(kind=8), dimension(model1_output_size)::model1_b5 ! bias vector for output layer
      
      integer, parameter::model2_input_size = 6
      integer, parameter::model2_output_size = 1
      integer, parameter::model2_hidden_layers = 3
      integer, parameter::model2_hidden_size = 9
      real(kind=8), 
     *  dimension(model2_hidden_size, model2_input_size)::model2_w1 ! weight matrix from input layer to first hidden layer
      real(kind=8), dimension(model2_hidden_size)::model2_b1 ! bias vector for first hidden layer
      real(kind=8), 
     *  dimension(model2_hidden_size, model2_hidden_size)::model2_w2 ! weight matrix from hidden layer 1 to hidden layer 2
      real(kind=8), dimension(model2_hidden_size)::model2_b2 ! bias vector for hidden layer 2
      real(kind=8), 
     *  dimension(model2_hidden_size, model2_hidden_size)::model2_w3 ! weight matrix from hidden layer 2 to hidden layer 3
      real(kind=8), dimension(model2_hidden_size)::model2_b3 ! bias vector for hidden layer 3
      real(kind=8), 
     *  dimension(model2_hidden_size, model2_hidden_size)::model2_w4 ! weight matrix from hidden layer 3 to hidden layer 4
      real(kind=8), dimension(model2_hidden_size)::model2_b4 ! bias vector for hidden layer 4
      real(kind=8), 
     *  dimension(model2_output_size, model2_hidden_size)::model2_w5 ! weight matrix from last hidden layer to output layer
      real(kind=8), dimension(model2_output_size)::model2_b5 ! bias vector for output layer
      
      integer, parameter::model3_input_size = 6
      integer, parameter::model3_output_size = 1
      integer, parameter::model3_hidden_layers = 3
      integer, parameter::model3_hidden_size = 9
      real(kind=8), 
     *  dimension(model3_hidden_size, model3_input_size)::model3_w1 ! weight matrix from input layer to first hidden layer
      real(kind=8), dimension(model3_hidden_size)::model3_b1 ! bias vector for first hidden layer
      real(kind=8), 
     *  dimension(model3_hidden_size, model3_hidden_size)::model3_w2 ! weight matrix from hidden layer 1 to hidden layer 2
      real(kind=8), dimension(model3_hidden_size)::model3_b2 ! bias vector for hidden layer 2
      real(kind=8), 
     *  dimension(model3_hidden_size, model3_hidden_size)::model3_w3 ! weight matrix from hidden layer 2 to hidden layer 3
      real(kind=8), dimension(model3_hidden_size)::model3_b3 ! bias vector for hidden layer 3
      real(kind=8), 
     *  dimension(model3_hidden_size, model3_hidden_size)::model3_w4 ! weight matrix from hidden layer 3 to hidden layer 4
      real(kind=8), dimension(model3_hidden_size)::model3_b4 ! bias vector for hidden layer 4
      real(kind=8), 
     *  dimension(model3_output_size, model3_hidden_size)::model3_w5 ! weight matrix from last hidden layer to output layer
      real(kind=8), dimension(model3_output_size)::model3_b5 ! bias vector for output layer
      
      integer, parameter::model4_input_size = 6
      integer, parameter::model4_output_size = 1
      integer, parameter::model4_hidden_layers = 3
      integer, parameter::model4_hidden_size = 9
      real(kind=8), 
     *  dimension(model4_hidden_size, model4_input_size)::model4_w1 ! weight matrix from input layer to first hidden layer
      real(kind=8), dimension(model4_hidden_size)::model4_b1 ! bias vector for first hidden layer
      real(kind=8), 
     *  dimension(model4_hidden_size, model4_hidden_size)::model4_w2 ! weight matrix from hidden layer 1 to hidden layer 2
      real(kind=8), dimension(model4_hidden_size)::model4_b2 ! bias vector for hidden layer 2
      real(kind=8), 
     *  dimension(model4_hidden_size, model4_hidden_size)::model4_w3 ! weight matrix from hidden layer 2 to hidden layer 3
      real(kind=8), dimension(model4_hidden_size)::model4_b3 ! bias vector for hidden layer 3
      real(kind=8), 
     *  dimension(model4_hidden_size, model4_hidden_size)::model4_w4 ! weight matrix from hidden layer 3 to hidden layer 4
      real(kind=8), dimension(model4_hidden_size)::model4_b4 ! bias vector for hidden layer 4
      real(kind=8), 
     *  dimension(model4_output_size, model4_hidden_size)::model4_w5 ! weight matrix from last hidden layer to output layer
      real(kind=8), dimension(model4_output_size)::model4_b5 ! bias vector for output layer
      
      integer, parameter::model5_input_size = 6
      integer, parameter::model5_output_size = 1
      integer, parameter::model5_hidden_layers = 3
      integer, parameter::model5_hidden_size = 9
      real(kind=8), 
     *  dimension(model5_hidden_size,model5_input_size )::model5_w1 ! weight matrix from input layer to first hidden layer
      real(kind=8), dimension(model5_hidden_size)::model5_b1 ! bias vector for first hidden layer
      real(kind=8), 
     *  dimension(model5_hidden_size, model5_hidden_size)::model5_w2 ! weight matrix from hidden layer 1 to hidden layer 2
      real(kind=8), dimension(model5_hidden_size)::model5_b2 ! bias vector for hidden layer 2
      real(kind=8), 
     *  dimension(model5_hidden_size, model5_hidden_size)::model5_w3 ! weight matrix from hidden layer 2 to hidden layer 3
      real(kind=8), dimension(model5_hidden_size)::model5_b3 ! bias vector for hidden layer 3
      real(kind=8), 
     *  dimension(model5_hidden_size, model5_hidden_size)::model5_w4 ! weight matrix from hidden layer 3 to hidden layer 4
      real(kind=8), dimension(model5_hidden_size)::model5_b4 ! bias vector for hidden layer 4
      real(kind=8), 
     *  dimension(model5_output_size,model5_hidden_size )::model5_w5 ! weight matrix from last hidden layer to output layer
      real(kind=8), dimension(model5_output_size)::model5_b5 ! bias vector for output layer
      
      integer, parameter::model6_input_size = 6
      integer, parameter::model6_output_size = 1
      integer, parameter::model6_hidden_layers = 3
      integer, parameter::model6_hidden_size = 9
      real(kind=8), 
     *  dimension(model6_hidden_size, model6_input_size)::model6_w1 ! weight matrix from input layer to first hidden layer
      real(kind=8), dimension(model6_hidden_size)::model6_b1 ! bias vector for first hidden layer
      real(kind=8), 
     *  dimension(model6_hidden_size, model6_hidden_size)::model6_w2 ! weight matrix from hidden layer 1 to hidden layer 2
      real(kind=8), dimension(model6_hidden_size)::model6_b2 ! bias vector for hidden layer 2
      real(kind=8), 
     *  dimension(model6_hidden_size, model6_hidden_size)::model6_w3 ! weight matrix from hidden layer 2 to hidden layer 3
      real(kind=8), dimension(model6_hidden_size)::model6_b3 ! bias vector for hidden layer 3
      real(kind=8), 
     *  dimension(model6_hidden_size, model6_hidden_size)::model6_w4 ! weight matrix from hidden layer 3 to hidden layer 4
      real(kind=8), dimension(model6_hidden_size)::model6_b4 ! bias vector for hidden layer 4
      real(kind=8), 
     *  dimension(model6_output_size,model6_hidden_size)::model6_w5 ! weight matrix from last hidden layer to output layer
      real(kind=8), dimension(model6_output_size)::model6_b5 ! bias vector for output layer
      
      real(kind=8), DIMENSION(6)::B_sym
C
      PARAMETER(ZERO=0.D0, ONE=1.D0, TWO=2.D0, THREE=3.D0, FOUR=4.D0)
      
      real(kind=8), PARAMETER::eps_d=0.00001D0
      real(kind=8), DIMENSION(6)::B_sym1, B_sym2, 
     *       B_sym3, B_sym4, B_sym5, B_sym6
      
      real(kind=8),DIMENSION(6)::C4row1,C4row2,
     * C4row3,C4row4,C4row5,C4row6
      
      real(kind=8)::sig11, 
     *      sig22, sig33, sig12, sig13, sig23
      
      real(kind=8) : :sig11_1, sig11_2, sig11_3,
     * sig11_4, sig11_5, sig11_6
      real(kind=8)::sig22_1, sig22_2, sig22_3,
     * sig22_4, sig22_5, sig22_6
      real(kind=8)::sig33_1, sig33_2, sig33_3,
     * sig33_4, sig33_5, sig33_6
      real(kind=8)::sig12_1, sig12_2, sig12_3,
     * sig12_4, sig12_5, sig12_6
      real(kind=8)::sig13_1, sig13_2, sig13_3,
     * sig13_4, sig13_5, sig13_6
      real(kind=8)::sig23_1, sig23_2, sig23_3,
     * sig23_4, sig23_5, sig23_6
      
      real(kind=8)::model1
      real(kind=8)::model2
      
      
            
! model1_input_layer.weight
      model1_w1(1,1) = -0.13469812273979187d0
      model1_w1(1,2) = -0.3836071491241455d0
      model1_w1(1,3) = -0.24593527615070343d0
      model1_w1(1,4) = -0.04447680711746216d0
      model1_w1(1,5) = -0.34835365414619446d0
      model1_w1(1,6) = 0.12819194793701172d0
      model1_w1(2,1) = 0.9116076231002808d0
      model1_w1(2,2) = -0.06319884210824966d0
      model1_w1(2,3) = -0.1987873762845993d0
      model1_w1(2,4) = 0.10005910694599152d0
      model1_w1(2,5) = 0.046784818172454834d0
      model1_w1(2,6) = -1.6451942920684814d0
      model1_w1(3,1) = 0.9329673051834106d0
      model1_w1(3,2) = 0.3778415024280548d0
      model1_w1(3,3) = 0.1795421540737152d0
      model1_w1(3,4) = 0.24008996784687042d0
      model1_w1(3,5) = -0.06574615836143494d0
      model1_w1(3,6) = -1.5928535461425781d0
      model1_w1(4,1) = -0.5108783841133118d0
      model1_w1(4,2) = 1.1175678968429565d0
      model1_w1(4,3) = 0.7645126581192017d0
      model1_w1(4,4) = -0.04771896079182625d0
      model1_w1(4,5) = 0.18834038078784943d0
      model1_w1(4,6) = -0.6055528521537781d0
      model1_w1(5,1) = 0.9619876146316528d0
      model1_w1(5,2) = 0.1698073446750641d0
      model1_w1(5,3) = 0.3161078691482544d0
      model1_w1(5,4) = -0.1711193323135376d0
      model1_w1(5,5) = -0.20097357034683228d0
      model1_w1(5,6) = -1.475504994392395d0
      model1_w1(6,1) = 0.7777321934700012d0
      model1_w1(6,2) = -0.14268995821475983d0
      model1_w1(6,3) = -0.38557225465774536d0
      model1_w1(6,4) = -0.3388451933860779d0
      model1_w1(6,5) = -0.1837538629770279d0
      model1_w1(6,6) = 0.4017956554889679d0
      model1_w1(7,1) = -0.4213510751724243d0
      model1_w1(7,2) = 1.063257098197937d0
      model1_w1(7,3) = 1.123264193534851d0
      model1_w1(7,4) = 0.22791744768619537d0
      model1_w1(7,5) = 0.13474930822849274d0
      model1_w1(7,6) = -0.5247344970703125d0
      model1_w1(8,1) = -0.27601853013038635d0
      model1_w1(8,2) = -0.30418723821640015d0
      model1_w1(8,3) = -0.09789004921913147d0
      model1_w1(8,4) = -0.04394930601119995d0
      model1_w1(8,5) = 0.20197951793670654d0
      model1_w1(8,6) = -0.12222316861152649d0
      model1_w1(9,1) = -0.38069117069244385d0
      model1_w1(9,2) = -0.04255491495132446d0
      model1_w1(9,3) = -0.10125552862882614d0
      model1_w1(9,4) = 0.3039918541908264d0
      model1_w1(9,5) = -0.2801717221736908d0
      model1_w1(9,6) = 0.2934974133968353d0
! model1_input_layer.bias
      model1_b1(1) = -0.2798804044723511d0
      model1_b1(2) = 0.8122391700744629d0
      model1_b1(3) = 1.0414655208587646d0
      model1_b1(4) = 0.3675607144832611d0
      model1_b1(5) = 0.4114612340927124d0
      model1_b1(6) = 0.6471646428108215d0
      model1_b1(7) = 0.4304043650627136d0
      model1_b1(8) = -0.38262805342674255d0
      model1_b1(9) = 0.020518429577350616d0
! model1_hidden_layers.0.weight
      model1_w2(1,1) = 0.19775906205177307d0
      model1_w2(1,2) = 0.016970187425613403d0
      model1_w2(1,3) = -0.03738364577293396d0
      model1_w2(1,4) = -0.2750706374645233d0
      model1_w2(1,5) = -0.16168448328971863d0
      model1_w2(1,6) = -0.23017124831676483d0
      model1_w2(1,7) = -0.01815512776374817d0
      model1_w2(1,8) = -0.1369202584028244d0
      model1_w2(1,9) = -0.0027474164962768555d0
      model1_w2(2,1) = 0.12371253967285156d0
      model1_w2(2,2) = 0.699917197227478d0
      model1_w2(2,3) = 0.474756121635437d0
      model1_w2(2,4) = -0.9322848916053772d0
      model1_w2(2,5) = 0.3779785931110382d0
      model1_w2(2,6) = 0.7728102803230286d0
      model1_w2(2,7) = -1.1066486835479736d0
      model1_w2(2,8) = 0.001980811357498169d0
      model1_w2(2,9) = -0.31904345750808716d0
      model1_w2(3,1) = 0.2739730179309845d0
      model1_w2(3,2) = 1.0016365051269531d0
      model1_w2(3,3) = 0.8095793128013611d0
      model1_w2(3,4) = -0.9115598797798157d0
      model1_w2(3,5) = 0.7374257445335388d0
      model1_w2(3,6) = 0.36195436120033264d0
      model1_w2(3,7) = -0.7501981258392334d0
      model1_w2(3,8) = 0.16195324063301086d0
      model1_w2(3,9) = -0.10493242740631104d0
      model1_w2(4,1) = 0.02382686734199524d0
      model1_w2(4,2) = 0.5842682719230652d0
      model1_w2(4,3) = 0.6533629894256592d0
      model1_w2(4,4) = -0.8828219175338745d0
      model1_w2(4,5) = 0.7412711977958679d0
      model1_w2(4,6) = 1.0794825553894043d0
      model1_w2(4,7) = -0.47999024391174316d0
      model1_w2(4,8) = 0.0471874475479126d0
      model1_w2(4,9) = 0.12569701671600342d0
      model1_w2(5,1) = -0.23760804533958435d0
      model1_w2(5,2) = 0.6592612862586975d0
      model1_w2(5,3) = 0.3319794833660126d0
      model1_w2(5,4) = -0.9949845671653748d0
      model1_w2(5,5) = 0.6028121113777161d0
      model1_w2(5,6) = 0.4571959674358368d0
      model1_w2(5,7) = -0.9888543486595154d0
      model1_w2(5,8) = 0.2576323449611664d0
      model1_w2(5,9) = 0.09893718361854553d0
      model1_w2(6,1) = 0.2733962833881378d0
      model1_w2(6,2) = -0.3429625332355499d0
      model1_w2(6,3) = 0.599909782409668d0
      model1_w2(6,4) = 1.1071866750717163d0
      model1_w2(6,5) = 0.7473459839820862d0
      model1_w2(6,6) = -1.0699950456619263d0
      model1_w2(6,7) = 0.9158064723014832d0
      model1_w2(6,8) = -0.0035248100757598877d0
      model1_w2(6,9) = 0.3140602707862854d0
      model1_w2(7,1) = 0.2115354835987091d0
      model1_w2(7,2) = 1.5904344320297241d0
      model1_w2(7,3) = 1.1484246253967285d0
      model1_w2(7,4) = -0.42442595958709717d0
      model1_w2(7,5) = 1.1423410177230835d0
      model1_w2(7,6) = 0.21875669062137604d0
      model1_w2(7,7) = -0.4432700276374817d0
      model1_w2(7,8) = -0.049097925424575806d0
      model1_w2(7,9) = 0.09377330541610718d0
      model1_w2(8,1) = 0.27251997590065d0
      model1_w2(8,2) = 0.4723493456840515d0
      model1_w2(8,3) = 0.8154191970825195d0
      model1_w2(8,4) = -0.9054485559463501d0
      model1_w2(8,5) = 0.4041796922683716d0
      model1_w2(8,6) = 0.409211665391922d0
      model1_w2(8,7) = -1.2248148918151855d0
      model1_w2(8,8) = -0.2491961419582367d0
      model1_w2(8,9) = 0.0025573670864105225d0
      model1_w2(9,1) = 0.04392078518867493d0
      model1_w2(9,2) = 0.4943360984325409d0
      model1_w2(9,3) = 0.4870026111602783d0
      model1_w2(9,4) = -0.887800931930542d0
      model1_w2(9,5) = 0.8313723206520081d0
      model1_w2(9,6) = 0.9488052129745483d0
      model1_w2(9,7) = -0.2919897437095642d0
      model1_w2(9,8) = 0.02180197834968567d0
      model1_w2(9,9) = 0.20660531520843506d0
! model1_hidden_layers.0.bias
      model1_b2(1) = -0.03140759468078613d0
      model1_b2(2) = 0.502077043056488d0
      model1_b2(3) = 0.5238674283027649d0
      model1_b2(4) = 0.9840699434280396d0
      model1_b2(5) = -0.1381862908601761d0
      model1_b2(6) = 0.2951226234436035d0
      model1_b2(7) = 0.5820274949073792d0
      model1_b2(8) = 0.41040465235710144d0
      model1_b2(9) = 0.9643180966377258d0
! model1_hidden_layers.1.weight
      model1_w3(1,1) = -0.2640155553817749d0
      model1_w3(1,2) = 1.0330913066864014d0
      model1_w3(1,3) = 0.9654451608657837d0
      model1_w3(1,4) = 0.9338849186897278d0
      model1_w3(1,5) = 0.589454174041748d0
      model1_w3(1,6) = -0.6563485860824585d0
      model1_w3(1,7) = 0.674259603023529d0
      model1_w3(1,8) = 0.9160696268081665d0
      model1_w3(1,9) = 0.7957507371902466d0
      model1_w3(2,1) = 0.2826218903064728d0
      model1_w3(2,2) = 0.8757415413856506d0
      model1_w3(2,3) = 0.9462475180625916d0
      model1_w3(2,4) = 0.5117176175117493d0
      model1_w3(2,5) = 0.6224222183227539d0
      model1_w3(2,6) = -0.5922290086746216d0
      model1_w3(2,7) = 0.748158872127533d0
      model1_w3(2,8) = 0.6556556224822998d0
      model1_w3(2,9) = 0.7096120715141296d0
      model1_w3(3,1) = -0.3063136339187622d0
      model1_w3(3,2) = 0.5434947609901428d0
      model1_w3(3,3) = 0.7479665279388428d0
      model1_w3(3,4) = 0.950836181640625d0
      model1_w3(3,5) = 0.656198263168335d0
      model1_w3(3,6) = -0.5495168566703796d0
      model1_w3(3,7) = 0.45468470454216003d0
      model1_w3(3,8) = 0.9113978147506714d0
      model1_w3(3,9) = 0.8067781329154968d0
      model1_w3(4,1) = -0.010860711336135864d0
      model1_w3(4,2) = -1.3302644491195679d0
      model1_w3(4,3) = -0.829971194267273d0
      model1_w3(4,4) = -0.30553069710731506d0
      model1_w3(4,5) = -1.2494986057281494d0
      model1_w3(4,6) = 0.9860796928405762d0
      model1_w3(4,7) = 1.0003710985183716d0
      model1_w3(4,8) = -0.7922763824462891d0
      model1_w3(4,9) = 0.2789791524410248d0
      model1_w3(5,1) = 0.00664183497428894d0
      model1_w3(5,2) = 0.43820056319236755d0
      model1_w3(5,3) = 0.6074358224868774d0
      model1_w3(5,4) = 0.690498948097229d0
      model1_w3(5,5) = 0.9947195053100586d0
      model1_w3(5,6) = -0.448988676071167d0
      model1_w3(5,7) = 0.6096907258033752d0
      model1_w3(5,8) = 0.7734048366546631d0
      model1_w3(5,9) = 0.8135471343994141d0
      model1_w3(6,1) = -0.3234178423881531d0
      model1_w3(6,2) = -0.3950279951095581d0
      model1_w3(6,3) = -0.14520792663097382d0
      model1_w3(6,4) = -0.5116676092147827d0
      model1_w3(6,5) = -1.12314772605896d0
      model1_w3(6,6) = 0.7295910716056824d0
      model1_w3(6,7) = 0.6274747252464294d0
      model1_w3(6,8) = -0.6486251950263977d0
      model1_w3(6,9) = -0.0933409333229065d0
      model1_w3(7,1) = -0.08067357540130615d0
      model1_w3(7,2) = -1.5734810829162598d0
      model1_w3(7,3) = 0.0003441325679887086d0
      model1_w3(7,4) = 1.113741159439087d0
      model1_w3(7,5) = -1.2436232566833496d0
      model1_w3(7,6) = 0.6139193773269653d0
      model1_w3(7,7) = -1.4576537609100342d0
      model1_w3(7,8) = -0.8343757390975952d0
      model1_w3(7,9) = 0.9701075553894043d0
      model1_w3(8,1) = -0.19086787104606628d0
      model1_w3(8,2) = -0.13475370407104492d0
      model1_w3(8,3) = -0.3010709285736084d0
      model1_w3(8,4) = 0.16454967856407166d0
      model1_w3(8,5) = -0.28864428400993347d0
      model1_w3(8,6) = -0.16512863337993622d0
      model1_w3(8,7) = -0.10388660430908203d0
      model1_w3(8,8) = 0.2311381995677948d0
      model1_w3(8,9) = -0.037766605615615845d0
      model1_w3(9,1) = 0.26862356066703796d0
      model1_w3(9,2) = 0.912767231464386d0
      model1_w3(9,3) = 0.43821829557418823d0
      model1_w3(9,4) = 0.4428088366985321d0
      model1_w3(9,5) = 0.8108982443809509d0
      model1_w3(9,6) = -0.9583638906478882d0
      model1_w3(9,7) = 0.9314131140708923d0
      model1_w3(9,8) = 0.7107577323913574d0
      model1_w3(9,9) = 0.7244106531143188d0
! model1_hidden_layers.1.bias
      model1_b3(1) = 0.3047029674053192d0
      model1_b3(2) = 0.39651015400886536d0
      model1_b3(3) = 0.19591423869132996d0
      model1_b3(4) = 0.20078516006469727d0
      model1_b3(5) = 0.049776215106248856d0
      model1_b3(6) = 0.019554568454623222d0
      model1_b3(7) = 1.164444088935852d0
      model1_b3(8) = -0.09213583171367645d0
      model1_b3(9) = 0.3169953227043152d0
! model1_hidden_layers.2.weight
      model1_w4(1,1) = -0.16681642830371857d0
      model1_w4(1,2) = 0.046557895839214325d0
      model1_w4(1,3) = 0.038636911660432816d0
      model1_w4(1,4) = -0.22464331984519958d0
      model1_w4(1,5) = 0.13828667998313904d0
      model1_w4(1,6) = 0.06320330500602722d0
      model1_w4(1,7) = -0.13909825682640076d0
      model1_w4(1,8) = 0.29405972361564636d0
      model1_w4(1,9) = -0.03319855034351349d0
      model1_w4(2,1) = 1.1029518842697144d0
      model1_w4(2,2) = 1.0877325534820557d0
      model1_w4(2,3) = 0.9293457865715027d0
      model1_w4(2,4) = -0.23112311959266663d0
      model1_w4(2,5) = 0.44142740964889526d0
      model1_w4(2,6) = 0.04726790636777878d0
      model1_w4(2,7) = 1.3973801136016846d0
      model1_w4(2,8) = 0.19142451882362366d0
      model1_w4(2,9) = 1.0369020700454712d0
      model1_w4(3,1) = -0.03779549151659012d0
      model1_w4(3,2) = 0.11966442316770554d0
      model1_w4(3,3) = -0.10886124521493912d0
      model1_w4(3,4) = 1.0706382989883423d0
      model1_w4(3,5) = -0.5340993404388428d0
      model1_w4(3,6) = 0.833625316619873d0
      model1_w4(3,7) = -1.8629179000854492d0
      model1_w4(3,8) = 0.2586528956890106d0
      model1_w4(3,9) = 0.13588884472846985d0
      model1_w4(4,1) = 0.9402278065681458d0
      model1_w4(4,2) = 0.7495278120040894d0
      model1_w4(4,3) = 0.7015464305877686d0
      model1_w4(4,4) = -1.599077582359314d0
      model1_w4(4,5) = 0.7388870716094971d0
      model1_w4(4,6) = -0.9984171390533447d0
      model1_w4(4,7) = -0.5832396149635315d0
      model1_w4(4,8) = 0.11341166496276855d0
      model1_w4(4,9) = 0.9558889269828796d0
      model1_w4(5,1) = 0.8424100875854492d0
      model1_w4(5,2) = 0.8911831974983215d0
      model1_w4(5,3) = 0.7859731912612915d0
      model1_w4(5,4) = -1.4778742790222168d0
      model1_w4(5,5) = 0.8121154308319092d0
      model1_w4(5,6) = -0.85395348072052d0
      model1_w4(5,7) = -0.5993705987930298d0
      model1_w4(5,8) = -0.20195436477661133d0
      model1_w4(5,9) = 0.5842772722244263d0
      model1_w4(6,1) = 0.39027464389801025d0
      model1_w4(6,2) = 0.8167619109153748d0
      model1_w4(6,3) = 0.8968186378479004d0
      model1_w4(6,4) = -1.421700119972229d0
      model1_w4(6,5) = 0.30093449354171753d0
      model1_w4(6,6) = -1.374875545501709d0
      model1_w4(6,7) = -0.5835158824920654d0
      model1_w4(6,8) = 0.2485431730747223d0
      model1_w4(6,9) = 0.7863712906837463d0
      model1_w4(7,1) = 0.005696549080312252d0
      model1_w4(7,2) = -0.31375056505203247d0
      model1_w4(7,3) = 0.153391495347023d0
      model1_w4(7,4) = 0.6450203061103821d0
      model1_w4(7,5) = -0.1333131045103073d0
      model1_w4(7,6) = 0.9533589482307434d0
      model1_w4(7,7) = -2.1629114151000977d0
      model1_w4(7,8) = 0.07488319277763367d0
      model1_w4(7,9) = -0.037278298288583755d0
      model1_w4(8,1) = 0.06270790845155716d0
      model1_w4(8,2) = -0.13565059006214142d0
      model1_w4(8,3) = -0.27401798963546753d0
      model1_w4(8,4) = 1.0544052124023438d0
      model1_w4(8,5) = -0.34791287779808044d0
      model1_w4(8,6) = 0.8590134382247925d0
      model1_w4(8,7) = -2.246894359588623d0
      model1_w4(8,8) = -0.11287641525268555d0
      model1_w4(8,9) = 0.26971590518951416d0
      model1_w4(9,1) = -0.038748789578676224d0
      model1_w4(9,2) = -0.022692715749144554d0
      model1_w4(9,3) = -0.1229865774512291d0
      model1_w4(9,4) = 1.1093887090682983d0
      model1_w4(9,5) = -0.5449976921081543d0
      model1_w4(9,6) = 0.7557334303855896d0
      model1_w4(9,7) = -1.7678850889205933d0
      model1_w4(9,8) = -0.0175325870513916d0
      model1_w4(9,9) = 0.31616494059562683d0
! model1_hidden_layers.2.bias
      model1_b4(1) = -0.26722317934036255d0
      model1_b4(2) = 1.312867283821106d0
      model1_b4(3) = -0.007490935735404491d0
      model1_b4(4) = 0.14485295116901398d0
      model1_b4(5) = 0.2024652063846588d0
      model1_b4(6) = -0.48179227113723755d0
      model1_b4(7) = 0.12916292250156403d0
      model1_b4(8) = 0.027791405096650124d0
      model1_b4(9) = 0.15322674810886383d0
! model1_output_layer.weight
      model1_w5(1,1) = -0.14731256663799286d0
      model1_w5(1,2) = 0.9145877361297607d0
      model1_w5(1,3) = -1.6141928434371948d0
      model1_w5(1,4) = 0.9198148250579834d0
      model1_w5(1,5) = 0.995290219783783d0
      model1_w5(1,6) = 0.8863963484764099d0
      model1_w5(1,7) = -1.6778839826583862d0
      model1_w5(1,8) = -1.426942229270935d0
      model1_w5(1,9) = -1.5802265405654907d0
! model1_output_layer.bias
      model1_b5(1) = 0.6517269611358643d0
      
! model2_input_layer.weight
      model2_w1(1,1) = -0.5299255847930908d0
      model2_w1(1,2) = 0.8808718919754028d0
      model2_w1(1,3) = -0.41312524676322937d0
      model2_w1(1,4) = -0.673916220664978d0
      model2_w1(1,5) = -0.2921382784843445d0
      model2_w1(1,6) = -0.6173754930496216d0
      model2_w1(2,1) = -0.025870107114315033d0
      model2_w1(2,2) = 0.07742974162101746d0
      model2_w1(2,3) = 0.5759472846984863d0
      model2_w1(2,4) = -0.4988672733306885d0
      model2_w1(2,5) = -0.8939213156700134d0
      model2_w1(2,6) = -0.9173519611358643d0
      model2_w1(3,1) = 0.44416260719299316d0
      model2_w1(3,2) = 0.36694979667663574d0
      model2_w1(3,3) = 0.41915470361709595d0
      model2_w1(3,4) = -0.24694539606571198d0
      model2_w1(3,5) = -0.09505141526460648d0
      model2_w1(3,6) = 0.1653681844472885d0
      model2_w1(4,1) = 0.32293617725372314d0
      model2_w1(4,2) = -0.15866024792194366d0
      model2_w1(4,3) = 0.2712552547454834d0
      model2_w1(4,4) = 0.1683521866798401d0
      model2_w1(4,5) = -0.6489424109458923d0
      model2_w1(4,6) = -0.4887305796146393d0
      model2_w1(5,1) = -0.14562639594078064d0
      model2_w1(5,2) = -0.568316638469696d0
      model2_w1(5,3) = 0.6313772201538086d0
      model2_w1(5,4) = -0.14579789340496063d0
      model2_w1(5,5) = -0.4721277058124542d0
      model2_w1(5,6) = -0.6029241681098938d0
      model2_w1(6,1) = 1.0559720993041992d0
      model2_w1(6,2) = -0.3854997456073761d0
      model2_w1(6,3) = -0.654929518699646d0
      model2_w1(6,4) = -0.11376698315143585d0
      model2_w1(6,5) = -0.6576361656188965d0
      model2_w1(6,6) = -0.152118518948555d0
      model2_w1(7,1) = 0.34563177824020386d0
      model2_w1(7,2) = 0.24086160957813263d0
      model2_w1(7,3) = 0.7137513756752014d0
      model2_w1(7,4) = -0.06754875928163528d0
      model2_w1(7,5) = 0.05312767252326012d0
      model2_w1(7,6) = -0.2413329929113388d0
      model2_w1(8,1) = -0.24236170947551727d0
      model2_w1(8,2) = -0.08485883474349976d0
      model2_w1(8,3) = 0.04941919445991516d0
      model2_w1(8,4) = -0.005223512649536133d0
      model2_w1(8,5) = -0.228349506855011d0
      model2_w1(8,6) = 0.1386372447013855d0
      model2_w1(9,1) = 0.3619380593299866d0
      model2_w1(9,2) = -0.44401103258132935d0
      model2_w1(9,3) = 0.03494993597269058d0
      model2_w1(9,4) = -0.668558657169342d0
      model2_w1(9,5) = -0.7678406834602356d0
      model2_w1(9,6) = -0.8310666680335999d0
! model2_input_layer.bias
      model2_b1(1) = -0.015439115464687347d0
      model2_b1(2) = -0.4009411036968231d0
      model2_b1(3) = 0.30798470973968506d0
      model2_b1(4) = -0.43524110317230225d0
      model2_b1(5) = 0.07088731974363327d0
      model2_b1(6) = -0.27703332901000977d0
      model2_b1(7) = -0.08971007168292999d0
      model2_b1(8) = -0.28992411494255066d0
      model2_b1(9) = 0.04835803434252739d0
! model2_hidden_layers.0.weight
      model2_w2(1,1) = 0.714823842048645d0
      model2_w2(1,2) = 0.24130715429782867d0
      model2_w2(1,3) = 0.38813507556915283d0
      model2_w2(1,4) = 0.8016268610954285d0
      model2_w2(1,5) = 0.4023021459579468d0
      model2_w2(1,6) = 0.7079046964645386d0
      model2_w2(1,7) = -0.04976768046617508d0
      model2_w2(1,8) = -0.19026419520378113d0
      model2_w2(1,9) = 0.4177910387516022d0
      model2_w2(2,1) = 0.7661207914352417d0
      model2_w2(2,2) = 0.44955915212631226d0
      model2_w2(2,3) = -0.26563915610313416d0
      model2_w2(2,4) = 0.2510865330696106d0
      model2_w2(2,5) = 0.1926368772983551d0
      model2_w2(2,6) = 0.49608975648880005d0
      model2_w2(2,7) = 0.21073894202709198d0
      model2_w2(2,8) = 0.2505424916744232d0
      model2_w2(2,9) = 0.7457236051559448d0
      model2_w2(3,1) = 1.1917304992675781d0
      model2_w2(3,2) = 0.5425804257392883d0
      model2_w2(3,3) = 0.08659740537405014d0
      model2_w2(3,4) = 0.5912722945213318d0
      model2_w2(3,5) = 0.7418982982635498d0
      model2_w2(3,6) = 0.8062570095062256d0
      model2_w2(3,7) = 0.3856229782104492d0
      model2_w2(3,8) = -0.15058963000774384d0
      model2_w2(3,9) = 0.8813012838363647d0
      model2_w2(4,1) = 0.933408796787262d0
      model2_w2(4,2) = 0.5970777869224548d0
      model2_w2(4,3) = 0.3299091160297394d0
      model2_w2(4,4) = 0.6216821074485779d0
      model2_w2(4,5) = 0.11624865233898163d0
      model2_w2(4,6) = 0.5750903487205505d0
      model2_w2(4,7) = 0.140364408493042d0
      model2_w2(4,8) = 0.1027362048625946d0
      model2_w2(4,9) = 0.19989599287509918d0
      model2_w2(5,1) = 1.048079013824463d0
      model2_w2(5,2) = 0.2063864767551422d0
      model2_w2(5,3) = 0.31220585107803345d0
      model2_w2(5,4) = 0.32073041796684265d0
      model2_w2(5,5) = 0.6621811389923096d0
      model2_w2(5,6) = 1.0327953100204468d0
      model2_w2(5,7) = 0.118211530148983d0
      model2_w2(5,8) = -0.18730632960796356d0
      model2_w2(5,9) = 0.35686224699020386d0
      model2_w2(6,1) = -0.5288609862327576d0
      model2_w2(6,2) = -0.7520036697387695d0
      model2_w2(6,3) = 0.6953628063201904d0
      model2_w2(6,4) = -0.8564240336418152d0
      model2_w2(6,5) = -0.8888285160064697d0
      model2_w2(6,6) = -0.5004858374595642d0
      model2_w2(6,7) = 0.16498389840126038d0
      model2_w2(6,8) = -0.1697222888469696d0
      model2_w2(6,9) = -0.24329888820648193d0
      model2_w2(7,1) = -0.017113834619522095d0
      model2_w2(7,2) = -0.036143165081739426d0
      model2_w2(7,3) = -0.337891161441803d0
      model2_w2(7,4) = -0.1891702115535736d0
      model2_w2(7,5) = 0.0006740725366398692d0
      model2_w2(7,6) = 0.2886660099029541d0
      model2_w2(7,7) = 0.05017278343439102d0
      model2_w2(7,8) = -0.12533323466777802d0
      model2_w2(7,9) = -0.08338662981987d0
      model2_w2(8,1) = 0.7456281781196594d0
      model2_w2(8,2) = 0.785525381565094d0
      model2_w2(8,3) = 0.28756946325302124d0
      model2_w2(8,4) = 0.5062932372093201d0
      model2_w2(8,5) = 0.660944938659668d0
      model2_w2(8,6) = 0.9710549712181091d0
      model2_w2(8,7) = 0.2488168329000473d0
      model2_w2(8,8) = 0.10003805160522461d0
      model2_w2(8,9) = 0.6121833324432373d0
      model2_w2(9,1) = -0.1462239921092987d0
      model2_w2(9,2) = -0.16184525191783905d0
      model2_w2(9,3) = 0.22365184128284454d0
      model2_w2(9,4) = 0.007535104174166918d0
      model2_w2(9,5) = 0.16363926231861115d0
      model2_w2(9,6) = -0.1389022320508957d0
      model2_w2(9,7) = -0.3215830624103546d0
      model2_w2(9,8) = -0.22713100910186768d0
      model2_w2(9,9) = 0.25193294882774353d0
! model2_hidden_layers.0.bias
      model2_b2(1) = 0.10130389034748077d0
      model2_b2(2) = 0.05156270042061806d0
      model2_b2(3) = -0.47883325815200806d0
      model2_b2(4) = -0.2975935935974121d0
      model2_b2(5) = -0.47450965642929077d0
      model2_b2(6) = 0.5589735507965088d0
      model2_b2(7) = -0.2733721435070038d0
      model2_b2(8) = -0.4670000672340393d0
      model2_b2(9) = -0.10682208091020584d0
! model2_hidden_layers.1.weight
      model2_w3(1,1) = -0.33111801743507385d0
      model2_w3(1,2) = -0.40332117676734924d0
      model2_w3(1,3) = 0.08856477588415146d0
      model2_w3(1,4) = -0.2033807337284088d0
      model2_w3(1,5) = -0.16938860714435577d0
      model2_w3(1,6) = 0.16006311774253845d0
      model2_w3(1,7) = -0.07862603664398193d0
      model2_w3(1,8) = 0.17097754776477814d0
      model2_w3(1,9) = -0.23104023933410645d0
      model2_w3(2,1) = 0.6556527614593506d0
      model2_w3(2,2) = 0.2992216944694519d0
      model2_w3(2,3) = 0.6876199841499329d0
      model2_w3(2,4) = 0.4492623209953308d0
      model2_w3(2,5) = 0.8536486029624939d0
      model2_w3(2,6) = -1.116141676902771d0
      model2_w3(2,7) = -0.07514102756977081d0
      model2_w3(2,8) = 0.8261188268661499d0
      model2_w3(2,9) = -0.13407039642333984d0
      model2_w3(3,1) = 0.4501432180404663d0
      model2_w3(3,2) = 0.14855296909809113d0
      model2_w3(3,3) = 0.3894588053226471d0
      model2_w3(3,4) = 0.6113515496253967d0
      model2_w3(3,5) = 0.3743288516998291d0
      model2_w3(3,6) = 0.02550790086388588d0
      model2_w3(3,7) = 0.2486964911222458d0
      model2_w3(3,8) = 0.6961801648139954d0
      model2_w3(3,9) = -0.2738416790962219d0
      model2_w3(4,1) = 0.5847163796424866d0
      model2_w3(4,2) = 0.7087484002113342d0
      model2_w3(4,3) = 0.7383531332015991d0
      model2_w3(4,4) = 0.5273088812828064d0
      model2_w3(4,5) = 0.49061498045921326d0
      model2_w3(4,6) = 0.054917581379413605d0
      model2_w3(4,7) = -0.09394768625497818d0
      model2_w3(4,8) = 0.858488142490387d0
      model2_w3(4,9) = 0.2532843351364136d0
      model2_w3(5,1) = 0.22150930762290955d0
      model2_w3(5,2) = 0.6816686391830444d0
      model2_w3(5,3) = 0.5799810886383057d0
      model2_w3(5,4) = 0.27493688464164734d0
      model2_w3(5,5) = 0.5762967467308044d0
      model2_w3(5,6) = -1.2262462377548218d0
      model2_w3(5,7) = -0.25289589166641235d0
      model2_w3(5,8) = 0.8150562047958374d0
      model2_w3(5,9) = -0.3031548261642456d0
      model2_w3(6,1) = 0.590960681438446d0
      model2_w3(6,2) = 0.736510157585144d0
      model2_w3(6,3) = 0.5577355623245239d0
      model2_w3(6,4) = 0.504984974861145d0
      model2_w3(6,5) = 0.6078941822052002d0
      model2_w3(6,6) = -0.1984715461730957d0
      model2_w3(6,7) = 0.29049405455589294d0
      model2_w3(6,8) = 0.8797091245651245d0
      model2_w3(6,9) = 0.10875961184501648d0
      model2_w3(7,1) = 0.7946547865867615d0
      model2_w3(7,2) = 0.2610011696815491d0
      model2_w3(7,3) = 0.41832441091537476d0
      model2_w3(7,4) = 0.8399336934089661d0
      model2_w3(7,5) = 0.8129749894142151d0
      model2_w3(7,6) = -0.6516090035438538d0
      model2_w3(7,7) = -0.22024741768836975d0
      model2_w3(7,8) = 0.7925059795379639d0
      model2_w3(7,9) = 0.012541959062218666d0
      model2_w3(8,1) = 0.36063212156295776d0
      model2_w3(8,2) = 0.6020553708076477d0
      model2_w3(8,3) = 0.7182416319847107d0
      model2_w3(8,4) = 0.5243463516235352d0
      model2_w3(8,5) = 0.86110520362854d0
      model2_w3(8,6) = -0.8992149233818054d0
      model2_w3(8,7) = -0.278608500957489d0
      model2_w3(8,8) = 0.6877026557922363d0
      model2_w3(8,9) = 0.02778012864291668d0
      model2_w3(9,1) = -0.10946577042341232d0
      model2_w3(9,2) = 0.059555500745773315d0
      model2_w3(9,3) = -0.1606740802526474d0
      model2_w3(9,4) = -0.1778799295425415d0
      model2_w3(9,5) = -0.2619253695011139d0
      model2_w3(9,6) = 0.09339261054992676d0
      model2_w3(9,7) = -0.2762320637702942d0
      model2_w3(9,8) = 0.25332289934158325d0
      model2_w3(9,9) = 0.31127700209617615d0
! model2_hidden_layers.1.bias
      model2_b3(1) = -0.08781812340021133d0
      model2_b3(2) = -0.4259491562843323d0
      model2_b3(3) = -0.005332167260348797d0
      model2_b3(4) = 0.19711028039455414d0
      model2_b3(5) = -0.2233707755804062d0
      model2_b3(6) = -0.19228722155094147d0
      model2_b3(7) = -0.41030657291412354d0
      model2_b3(8) = -0.47689494490623474d0
      model2_b3(9) = -0.30707037448883057d0
! model2_hidden_layers.2.weight
      model2_w4(1,1) = -0.19294899702072144d0
      model2_w4(1,2) = 0.19450895488262177d0
      model2_w4(1,3) = 0.14762386679649353d0
      model2_w4(1,4) = -0.44317033886909485d0
      model2_w4(1,5) = -0.16463464498519897d0
      model2_w4(1,6) = 0.09094349294900894d0
      model2_w4(1,7) = 0.22414518892765045d0
      model2_w4(1,8) = 0.219558984041214d0
      model2_w4(1,9) = 0.023322701454162598d0
      model2_w4(2,1) = -0.01477153692394495d0
      model2_w4(2,2) = 0.4226253628730774d0
      model2_w4(2,3) = 0.8695992231369019d0
      model2_w4(2,4) = 0.5888765454292297d0
      model2_w4(2,5) = 0.2878888249397278d0
      model2_w4(2,6) = 0.33162838220596313d0
      model2_w4(2,7) = 0.7133751511573792d0
      model2_w4(2,8) = 0.9097638130187988d0
      model2_w4(2,9) = 0.06567268073558807d0
      model2_w4(3,1) = -0.03605484962463379d0
      model2_w4(3,2) = 0.580626368522644d0
      model2_w4(3,3) = 0.431291401386261d0
      model2_w4(3,4) = 0.7132905125617981d0
      model2_w4(3,5) = 0.7927988767623901d0
      model2_w4(3,6) = 0.7925674319267273d0
      model2_w4(3,7) = 0.8319947123527527d0
      model2_w4(3,8) = 0.5866861343383789d0
      model2_w4(3,9) = -0.1045619323849678d0
      model2_w4(4,1) = 0.0012697635684162378d0
      model2_w4(4,2) = -0.3569260835647583d0
      model2_w4(4,3) = 0.5040423274040222d0
      model2_w4(4,4) = 0.31482774019241333d0
      model2_w4(4,5) = -0.14697304368019104d0
      model2_w4(4,6) = -0.054387375712394714d0
      model2_w4(4,7) = -0.010388708673417568d0
      model2_w4(4,8) = -0.3924458622932434d0
      model2_w4(4,9) = -0.22149351239204407d0
      model2_w4(5,1) = 0.19780319929122925d0
      model2_w4(5,2) = -0.2970767021179199d0
      model2_w4(5,3) = 0.1338966339826584d0
      model2_w4(5,4) = 0.1565050333738327d0
      model2_w4(5,5) = -0.4941866397857666d0
      model2_w4(5,6) = 0.21002580225467682d0
      model2_w4(5,7) = 0.11238598078489304d0
      model2_w4(5,8) = 0.006405622232705355d0
      model2_w4(5,9) = -0.034540172666311264d0
      model2_w4(6,1) = 0.1942819505929947d0
      model2_w4(6,2) = -0.1541672646999359d0
      model2_w4(6,3) = 0.14596037566661835d0
      model2_w4(6,4) = 0.11709430813789368d0
      model2_w4(6,5) = -0.389045774936676d0
      model2_w4(6,6) = 0.3036547899246216d0
      model2_w4(6,7) = 0.0002834589686244726d0
      model2_w4(6,8) = -0.27296537160873413d0
      model2_w4(6,9) = -0.13489791750907898d0
      model2_w4(7,1) = 0.27629706263542175d0
      model2_w4(7,2) = -0.3017255961894989d0
      model2_w4(7,3) = 0.029836803674697876d0
      model2_w4(7,4) = -0.03050863742828369d0
      model2_w4(7,5) = 0.09540104866027832d0
      model2_w4(7,6) = -0.21047541499137878d0
      model2_w4(7,7) = -0.03675535321235657d0
      model2_w4(7,8) = -0.29306459426879883d0
      model2_w4(7,9) = 0.016597986221313477d0
      model2_w4(8,1) = -0.21649746596813202d0
      model2_w4(8,2) = 0.09574475139379501d0
      model2_w4(8,3) = -0.014507870189845562d0
      model2_w4(8,4) = 0.2429826706647873d0
      model2_w4(8,5) = -0.15957759320735931d0
      model2_w4(8,6) = -0.32979586720466614d0
      model2_w4(8,7) = -0.7246158719062805d0
      model2_w4(8,8) = 0.11408710479736328d0
      model2_w4(8,9) = -0.13239838182926178d0
      model2_w4(9,1) = -0.03705844655632973d0
      model2_w4(9,2) = 0.461826354265213d0
      model2_w4(9,3) = 0.37049421668052673d0
      model2_w4(9,4) = 0.5552497506141663d0
      model2_w4(9,5) = 0.7688655853271484d0
      model2_w4(9,6) = 0.6529259085655212d0
      model2_w4(9,7) = 0.4962442219257355d0
      model2_w4(9,8) = 0.5072665214538574d0
      model2_w4(9,9) = -0.28248700499534607d0
! model2_hidden_layers.2.bias
      model2_b4(1) = -1.0281121730804443d0
      model2_b4(2) = -0.05672615021467209d0
      model2_b4(3) = -0.36002859473228455d0
      model2_b4(4) = 0.8491845726966858d0
      model2_b4(5) = 0.5120474696159363d0
      model2_b4(6) = 0.9201098680496216d0
      model2_b4(7) = -0.10167643427848816d0
      model2_b4(8) = 0.5781031847000122d0
      model2_b4(9) = -0.06068945676088333d0
! model2_output_layer.weight
      model2_w5(1,1) = -0.7129983901977539d0
      model2_w5(1,2) = 0.7851832509040833d0
      model2_w5(1,3) = 0.8461012244224548d0
      model2_w5(1,4) = -0.8082910776138306d0
      model2_w5(1,5) = -0.946358323097229d0
      model2_w5(1,6) = -0.7422341704368591d0
      model2_w5(1,7) = -0.1441485583782196d0
      model2_w5(1,8) = -0.44310104846954346d0
      model2_w5(1,9) = 0.7595077753067017d0
! model2_output_layer.bias
      model2_b5(1) = -0.12732231616973877d0
      
! model3_input_layer.weight
      model3_w1(1,1) = 0.39382505416870117d0
      model3_w1(1,2) = -0.8003703355789185d0
      model3_w1(1,3) = 0.5154222846031189d0
      model3_w1(1,4) = -0.882439911365509d0
      model3_w1(1,5) = -0.8975973129272461d0
      model3_w1(1,6) = -0.634815514087677d0
      model3_w1(2,1) = -0.3199419379234314d0
      model3_w1(2,2) = -0.37021130323410034d0
      model3_w1(2,3) = 0.05901569128036499d0
      model3_w1(2,4) = -0.3514840006828308d0
      model3_w1(2,5) = -0.13150331377983093d0
      model3_w1(2,6) = 0.401561439037323d0
      model3_w1(3,1) = -0.39428743720054626d0
      model3_w1(3,2) = -0.3352356553077698d0
      model3_w1(3,3) = 0.04567950963973999d0
      model3_w1(3,4) = -0.25664761662483215d0
      model3_w1(3,5) = 0.22791844606399536d0
      model3_w1(3,6) = 0.29092735052108765d0
      model3_w1(4,1) = 0.6245424747467041d0
      model3_w1(4,2) = -0.28520411252975464d0
      model3_w1(4,3) = 0.3708266317844391d0
      model3_w1(4,4) = -0.3542988896369934d0
      model3_w1(4,5) = -0.04253166541457176d0
      model3_w1(4,6) = -0.02053970843553543d0
      model3_w1(5,1) = -0.16514885425567627d0
      model3_w1(5,2) = -0.25762540102005005d0
      model3_w1(5,3) = 0.019917398691177368d0
      model3_w1(5,4) = 0.20647621154785156d0
      model3_w1(5,5) = -0.27091509103775024d0
      model3_w1(5,6) = -0.21956278383731842d0
      model3_w1(6,1) = -0.39379334449768066d0
      model3_w1(6,2) = -0.11341920495033264d0
      model3_w1(6,3) = -0.15642353892326355d0
      model3_w1(6,4) = 0.1710374355316162d0
      model3_w1(6,5) = 0.19269609451293945d0
      model3_w1(6,6) = -0.23100195825099945d0
      model3_w1(7,1) = 0.3808276951313019d0
      model3_w1(7,2) = -0.28501370549201965d0
      model3_w1(7,3) = 0.6057112812995911d0
      model3_w1(7,4) = -0.16901230812072754d0
      model3_w1(7,5) = -0.42636391520500183d0
      model3_w1(7,6) = -0.3029794991016388d0
      model3_w1(8,1) = 0.47141632437705994d0
      model3_w1(8,2) = -0.038526877760887146d0
      model3_w1(8,3) = 0.0668405070900917d0
      model3_w1(8,4) = -0.5109586715698242d0
      model3_w1(8,5) = -0.11817701905965805d0
      model3_w1(8,6) = -0.08832429349422455d0
      model3_w1(9,1) = 0.49611973762512207d0
      model3_w1(9,2) = 0.9833371639251709d0
      model3_w1(9,3) = -0.050048355013132095d0
      model3_w1(9,4) = -0.5950713753700256d0
      model3_w1(9,5) = 0.03365696221590042d0
      model3_w1(9,6) = -0.146449014544487d0
! model3_input_layer.bias
      model3_b1(1) = -0.11267126351594925d0
      model3_b1(2) = -0.08579346537590027d0
      model3_b1(3) = 0.07743561267852783d0
      model3_b1(4) = -0.14873673021793365d0
      model3_b1(5) = 0.07828927040100098d0
      model3_b1(6) = -0.04775556921958923d0
      model3_b1(7) = -0.36340636014938354d0
      model3_b1(8) = -0.02693515084683895d0
      model3_b1(9) = 0.3753713071346283d0
! model3_hidden_layers.0.weight
      model3_w2(1,1) = -0.027557268738746643d0
      model3_w2(1,2) = 0.14242973923683167d0
      model3_w2(1,3) = -0.31596601009368896d0
      model3_w2(1,4) = -0.1597696989774704d0
      model3_w2(1,5) = -0.10956975817680359d0
      model3_w2(1,6) = 0.308328777551651d0
      model3_w2(1,7) = -0.14387574791908264d0
      model3_w2(1,8) = -0.34148678183555603d0
      model3_w2(1,9) = -0.003074798034504056d0
      model3_w2(2,1) = 0.8480839729309082d0
      model3_w2(2,2) = 0.10829845070838928d0
      model3_w2(2,3) = 0.20409664511680603d0
      model3_w2(2,4) = 0.3186114430427551d0
      model3_w2(2,5) = 0.15894058346748352d0
      model3_w2(2,6) = -0.10910813510417938d0
      model3_w2(2,7) = 0.7491595149040222d0
      model3_w2(2,8) = 0.6604374051094055d0
      model3_w2(2,9) = -0.05950413644313812d0
      model3_w2(3,1) = 0.7790043950080872d0
      model3_w2(3,2) = 0.3210565149784088d0
      model3_w2(3,3) = 0.17829838395118713d0
      model3_w2(3,4) = 0.5900335907936096d0
      model3_w2(3,5) = 0.10112929344177246d0
      model3_w2(3,6) = 0.21418985724449158d0
      model3_w2(3,7) = 0.5722058415412903d0
      model3_w2(3,8) = 0.774316132068634d0
      model3_w2(3,9) = -0.017510579898953438d0
      model3_w2(4,1) = 0.5626162886619568d0
      model3_w2(4,2) = 0.010672718286514282d0
      model3_w2(4,3) = -0.01274251937866211d0
      model3_w2(4,4) = 0.3978639245033264d0
      model3_w2(4,5) = -0.16601762175559998d0
      model3_w2(4,6) = -0.31563252210617065d0
      model3_w2(4,7) = 0.1281566321849823d0
      model3_w2(4,8) = 0.3916783630847931d0
      model3_w2(4,9) = -0.2887091040611267d0
      model3_w2(5,1) = 0.42036178708076477d0
      model3_w2(5,2) = -0.3019464910030365d0
      model3_w2(5,3) = -0.12982690334320068d0
      model3_w2(5,4) = 0.6222044825553894d0
      model3_w2(5,5) = -0.06695511937141418d0
      model3_w2(5,6) = 0.25560644268989563d0
      model3_w2(5,7) = 0.9040420651435852d0
      model3_w2(5,8) = 0.054045420140028d0
      model3_w2(5,9) = -0.08564002811908722d0
      model3_w2(6,1) = -1.0255781412124634d0
      model3_w2(6,2) = -0.3224838972091675d0
      model3_w2(6,3) = -0.0669708251953125d0
      model3_w2(6,4) = -0.5107476711273193d0
      model3_w2(6,5) = -0.10537335276603699d0
      model3_w2(6,6) = -0.033004969358444214d0
      model3_w2(6,7) = -0.7965236902236938d0
      model3_w2(6,8) = 0.20153765380382538d0
      model3_w2(6,9) = 0.784473717212677d0
      model3_w2(7,1) = -0.23303084075450897d0
      model3_w2(7,2) = 0.12011754512786865d0
      model3_w2(7,3) = 0.07463085651397705d0
      model3_w2(7,4) = 0.1378507912158966d0
      model3_w2(7,5) = -0.17201361060142517d0
      model3_w2(7,6) = -0.2259117066860199d0
      model3_w2(7,7) = -0.20220419764518738d0
      model3_w2(7,8) = -0.11095874011516571d0
      model3_w2(7,9) = -0.18572232127189636d0
      model3_w2(8,1) = 0.8826467394828796d0
      model3_w2(8,2) = 0.03316149115562439d0
      model3_w2(8,3) = -0.2789592146873474d0
      model3_w2(8,4) = 0.6256909370422363d0
      model3_w2(8,5) = -0.0740814208984375d0
      model3_w2(8,6) = 0.26739224791526794d0
      model3_w2(8,7) = 0.6461932063102722d0
      model3_w2(8,8) = 0.6584683060646057d0
      model3_w2(8,9) = -0.1741122454404831d0
      model3_w2(9,1) = 0.6651522517204285d0
      model3_w2(9,2) = -0.01833641529083252d0
      model3_w2(9,3) = -0.18889805674552917d0
      model3_w2(9,4) = 0.46660593152046204d0
      model3_w2(9,5) = 0.2834419310092926d0
      model3_w2(9,6) = 0.061240553855895996d0
      model3_w2(9,7) = 0.2522091567516327d0
      model3_w2(9,8) = 0.6550155282020569d0
      model3_w2(9,9) = 0.33855316042900085d0
! model3_hidden_layers.0.bias
      model3_b2(1) = 0.040520694106817245d0
      model3_b2(2) = -0.15324559807777405d0
      model3_b2(3) = -0.35000020265579224d0
      model3_b2(4) = -0.3139207661151886d0
      model3_b2(5) = 0.042344655841588974d0
      model3_b2(6) = 0.9069126844406128d0
      model3_b2(7) = -0.06935358047485352d0
      model3_b2(8) = 0.056435342878103256d0
      model3_b2(9) = -0.1702459752559662d0
! model3_hidden_layers.1.weight
      model3_w3(1,1) = -0.11305186152458191d0
      model3_w3(1,2) = 0.7041432857513428d0
      model3_w3(1,3) = 0.5226888656616211d0
      model3_w3(1,4) = 0.8204628229141235d0
      model3_w3(1,5) = 0.23929135501384735d0
      model3_w3(1,6) = -1.3290590047836304d0
      model3_w3(1,7) = -0.17305287718772888d0
      model3_w3(1,8) = 0.7467140555381775d0
      model3_w3(1,9) = 0.38361209630966187d0
      model3_w3(2,1) = 0.1541009247303009d0
      model3_w3(2,2) = 0.5875342488288879d0
      model3_w3(2,3) = 0.4385916590690613d0
      model3_w3(2,4) = 0.5610694885253906d0
      model3_w3(2,5) = 0.7924045920372009d0
      model3_w3(2,6) = -1.3622311353683472d0
      model3_w3(2,7) = 0.11543333530426025d0
      model3_w3(2,8) = 0.3063354790210724d0
      model3_w3(2,9) = 0.768399715423584d0
      model3_w3(3,1) = 0.03936339169740677d0
      model3_w3(3,2) = 0.5555173754692078d0
      model3_w3(3,3) = 0.6505478024482727d0
      model3_w3(3,4) = 0.6683972477912903d0
      model3_w3(3,5) = 0.41307181119918823d0
      model3_w3(3,6) = -0.8337687253952026d0
      model3_w3(3,7) = -0.08890235424041748d0
      model3_w3(3,8) = 0.2861757278442383d0
      model3_w3(3,9) = 0.582526445388794d0
      model3_w3(4,1) = 0.32482048869132996d0
      model3_w3(4,2) = -0.02346956729888916d0
      model3_w3(4,3) = 0.006691426038742065d0
      model3_w3(4,4) = -0.3105703294277191d0
      model3_w3(4,5) = -0.22527730464935303d0
      model3_w3(4,6) = -0.2574014365673065d0
      model3_w3(4,7) = -0.0799383819103241d0
      model3_w3(4,8) = 0.028954654932022095d0
      model3_w3(4,9) = 0.07018482685089111d0
      model3_w3(5,1) = 0.26428157091140747d0
      model3_w3(5,2) = 0.2969857454299927d0
      model3_w3(5,3) = 0.1942031979560852d0
      model3_w3(5,4) = -0.15148530900478363d0
      model3_w3(5,5) = 0.6291660666465759d0
      model3_w3(5,6) = -0.3621244430541992d0
      model3_w3(5,7) = -0.10023681819438934d0
      model3_w3(5,8) = 0.6605210900306702d0
      model3_w3(5,9) = 0.047552309930324554d0
      model3_w3(6,1) = -0.2233159840106964d0
      model3_w3(6,2) = -1.5862306356430054d0
      model3_w3(6,3) = -1.6031744480133057d0
      model3_w3(6,4) = 0.3967794179916382d0
      model3_w3(6,5) = -1.6581534147262573d0
      model3_w3(6,6) = 0.8297855257987976d0
      model3_w3(6,7) = -0.27593788504600525d0
      model3_w3(6,8) = -1.4905930757522583d0
      model3_w3(6,9) = 0.6447314620018005d0
      model3_w3(7,1) = 0.24195948243141174d0
      model3_w3(7,2) = -0.22387127578258514d0
      model3_w3(7,3) = 0.01478835940361023d0
      model3_w3(7,4) = -0.18848960101604462d0
      model3_w3(7,5) = 0.028507977724075317d0
      model3_w3(7,6) = 0.08626750111579895d0
      model3_w3(7,7) = 0.03903472423553467d0
      model3_w3(7,8) = -0.17738819122314453d0
      model3_w3(7,9) = -0.08175361156463623d0
      model3_w3(8,1) = -0.22190535068511963d0
      model3_w3(8,2) = -0.41101211309432983d0
      model3_w3(8,3) = -0.3838559091091156d0
      model3_w3(8,4) = -0.30192530155181885d0
      model3_w3(8,5) = -0.5332066416740417d0
      model3_w3(8,6) = 0.12242769449949265d0
      model3_w3(8,7) = 0.23550477623939514d0
      model3_w3(8,8) = -0.35144010186195374d0
      model3_w3(8,9) = 0.2526337802410126d0
      model3_w3(9,1) = 0.27156320214271545d0
      model3_w3(9,2) = 0.7276595234870911d0
      model3_w3(9,3) = 0.9016612768173218d0
      model3_w3(9,4) = 0.2494274526834488d0
      model3_w3(9,5) = 0.7733023762702942d0
      model3_w3(9,6) = -1.3743542432785034d0
      model3_w3(9,7) = -0.16110846400260925d0
      model3_w3(9,8) = 0.8559775352478027d0
      model3_w3(9,9) = 0.48174697160720825d0
! model3_hidden_layers.1.bias
      model3_b3(1) = -0.5384800434112549d0
      model3_b3(2) = -0.1530228853225708d0
      model3_b3(3) = -0.10212203115224838d0
      model3_b3(4) = -0.18854014575481415d0
      model3_b3(5) = 0.8267837166786194d0
      model3_b3(6) = 0.4709189832210541d0
      model3_b3(7) = -0.2123948037624359d0
      model3_b3(8) = -0.3482804298400879d0
      model3_b3(9) = -0.0583437979221344d0
! model3_hidden_layers.2.weight
      model3_w4(1,1) = 0.8002325892448425d0
      model3_w4(1,2) = 0.6804463863372803d0
      model3_w4(1,3) = 0.6525092124938965d0
      model3_w4(1,4) = -0.06220629811286926d0
      model3_w4(1,5) = 0.48790064454078674d0
      model3_w4(1,6) = -0.3778882324695587d0
      model3_w4(1,7) = -0.2531033456325531d0
      model3_w4(1,8) = 0.24172964692115784d0
      model3_w4(1,9) = 0.8973469734191895d0
      model3_w4(2,1) = 0.17803189158439636d0
      model3_w4(2,2) = -0.49656474590301514d0
      model3_w4(2,3) = 0.3983599841594696d0
      model3_w4(2,4) = -0.1315757930278778d0
      model3_w4(2,5) = 0.2515559792518616d0
      model3_w4(2,6) = -0.05248567461967468d0
      model3_w4(2,7) = 0.059076935052871704d0
      model3_w4(2,8) = -0.07923507690429688d0
      model3_w4(2,9) = -0.3255130350589752d0
      model3_w4(3,1) = -0.02072567120194435d0
      model3_w4(3,2) = 0.0547696091234684d0
      model3_w4(3,3) = -0.4431057870388031d0
      model3_w4(3,4) = 0.30567631125450134d0
      model3_w4(3,5) = -0.0638478472828865d0
      model3_w4(3,6) = -0.006789282895624638d0
      model3_w4(3,7) = 0.1929301917552948d0
      model3_w4(3,8) = -0.28778350353240967d0
      model3_w4(3,9) = 0.09810391068458557d0
      model3_w4(4,1) = 0.4410848915576935d0
      model3_w4(4,2) = 0.9360699653625488d0
      model3_w4(4,3) = 0.22307723760604858d0
      model3_w4(4,4) = -0.24546897411346436d0
      model3_w4(4,5) = -0.4805653393268585d0
      model3_w4(4,6) = 1.1544145345687866d0
      model3_w4(4,7) = -0.17432042956352234d0
      model3_w4(4,8) = -0.45995140075683594d0
      model3_w4(4,9) = 0.4040580987930298d0
      model3_w4(5,1) = 0.40572497248649597d0
      model3_w4(5,2) = 0.912192165851593d0
      model3_w4(5,3) = 0.5699706673622131d0
      model3_w4(5,4) = -0.2749142348766327d0
      model3_w4(5,5) = 0.1903025507926941d0
      model3_w4(5,6) = -0.39240047335624695d0
      model3_w4(5,7) = 0.02894863486289978d0
      model3_w4(5,8) = 0.2120230495929718d0
      model3_w4(5,9) = 0.8152110576629639d0
      model3_w4(6,1) = 0.6820113062858582d0
      model3_w4(6,2) = 0.7970682978630066d0
      model3_w4(6,3) = 0.6607582569122314d0
      model3_w4(6,4) = -0.02519369125366211d0
      model3_w4(6,5) = 0.3330124318599701d0
      model3_w4(6,6) = -0.3335088789463043d0
      model3_w4(6,7) = 0.13863638043403625d0
      model3_w4(6,8) = 0.2920092046260834d0
      model3_w4(6,9) = 0.9505367279052734d0
      model3_w4(7,1) = -0.4675298035144806d0
      model3_w4(7,2) = -0.4584214687347412d0
      model3_w4(7,3) = -0.3349776566028595d0
      model3_w4(7,4) = 0.11463510990142822d0
      model3_w4(7,5) = -2.0529844760894775d0
      model3_w4(7,6) = 1.1556676626205444d0
      model3_w4(7,7) = 0.22028473019599915d0
      model3_w4(7,8) = -0.5699805021286011d0
      model3_w4(7,9) = -0.7043954730033875d0
      model3_w4(8,1) = 0.888165295124054d0
      model3_w4(8,2) = 0.4730943441390991d0
      model3_w4(8,3) = 0.2733260691165924d0
      model3_w4(8,4) = 0.3064750134944916d0
      model3_w4(8,5) = 0.34639498591423035d0
      model3_w4(8,6) = -0.1754341423511505d0
      model3_w4(8,7) = -0.14626693725585938d0
      model3_w4(8,8) = 0.3145257532596588d0
      model3_w4(8,9) = 0.42284074425697327d0
      model3_w4(9,1) = 0.5242375135421753d0
      model3_w4(9,2) = 0.46500658988952637d0
      model3_w4(9,3) = 0.6566844582557678d0
      model3_w4(9,4) = -0.3329896628856659d0
      model3_w4(9,5) = -0.6024048924446106d0
      model3_w4(9,6) = 1.0435634851455688d0
      model3_w4(9,7) = 0.0025639235973358154d0
      model3_w4(9,8) = -0.2141464501619339d0
      model3_w4(9,9) = 0.7414857149124146d0
! model3_hidden_layers.2.bias
      model3_b4(1) = -0.5397164225578308d0
      model3_b4(2) = -0.3061380088329315d0
      model3_b4(3) = -0.11915915459394455d0
      model3_b4(4) = 0.14241591095924377d0
      model3_b4(5) = -0.018851879984140396d0
      model3_b4(6) = -0.24035558104515076d0
      model3_b4(7) = 0.5000084638595581d0
      model3_b4(8) = -0.08159486204385757d0
      model3_b4(9) = 0.25765642523765564d0
! model3_output_layer.weight
      model3_w5(1,1) = 0.7091753482818604d0
      model3_w5(1,2) = 0.47526490688323975d0
      model3_w5(1,3) = -0.15678618848323822d0
      model3_w5(1,4) = 1.0205714702606201d0
      model3_w5(1,5) = 0.8969132304191589d0
      model3_w5(1,6) = 0.675391674041748d0
      model3_w5(1,7) = 1.4083540439605713d0
      model3_w5(1,8) = 0.8622642755508423d0
      model3_w5(1,9) = 0.8410110473632812d0
! model3_output_layer.bias
      model3_b5(1) = -0.22220422327518463d0
      
! model4_input_layer.weight
      model4_w1(1,1) = 0.5435094237327576d0
      model4_w1(1,2) = 0.748958945274353d0
      model4_w1(1,3) = 0.002373830880969763d0
      model4_w1(1,4) = 0.49984458088874817d0
      model4_w1(1,5) = -0.7819381356239319d0
      model4_w1(1,6) = -0.7590355277061462d0
      model4_w1(2,1) = 0.2631146013736725d0
      model4_w1(2,2) = 0.2595173716545105d0
      model4_w1(2,3) = 0.28163591027259827d0
      model4_w1(2,4) = 0.37489408254623413d0
      model4_w1(2,5) = -0.2467079609632492d0
      model4_w1(2,6) = -0.6070389151573181d0
      model4_w1(3,1) = -0.24550515413284302d0
      model4_w1(3,2) = -0.10756194591522217d0
      model4_w1(3,3) = -0.3507192134857178d0
      model4_w1(3,4) = -0.3661750555038452d0
      model4_w1(3,5) = 0.2825166583061218d0
      model4_w1(3,6) = -0.2988305985927582d0
      model4_w1(4,1) = -0.07430823147296906d0
      model4_w1(4,2) = 0.7356857657432556d0
      model4_w1(4,3) = -0.06943877041339874d0
      model4_w1(4,4) = 0.5422316789627075d0
      model4_w1(4,5) = -0.0826852098107338d0
      model4_w1(4,6) = -0.33049991726875305d0
      model4_w1(5,1) = -0.21332088112831116d0
      model4_w1(5,2) = 0.40795719623565674d0
      model4_w1(5,3) = 0.7402582168579102d0
      model4_w1(5,4) = -0.48479610681533813d0
      model4_w1(5,5) = 0.12454792857170105d0
      model4_w1(5,6) = 0.18993167579174042d0
      model4_w1(6,1) = 0.45113682746887207d0
      model4_w1(6,2) = 0.5381941795349121d0
      model4_w1(6,3) = -0.1408608853816986d0
      model4_w1(6,4) = 0.5275582671165466d0
      model4_w1(6,5) = -0.6446545124053955d0
      model4_w1(6,6) = -0.09172850102186203d0
      model4_w1(7,1) = 0.14125734567642212d0
      model4_w1(7,2) = 0.44522008299827576d0
      model4_w1(7,3) = 0.21062059700489044d0
      model4_w1(7,4) = -0.7775958776473999d0
      model4_w1(7,5) = 0.32352322340011597d0
      model4_w1(7,6) = -0.3325515389442444d0
      model4_w1(8,1) = 0.2780148386955261d0
      model4_w1(8,2) = 0.7207414507865906d0
      model4_w1(8,3) = -0.11539208143949509d0
      model4_w1(8,4) = 0.7508379220962524d0
      model4_w1(8,5) = -0.36949145793914795d0
      model4_w1(8,6) = -0.5821123123168945d0
      model4_w1(9,1) = 0.07687879353761673d0
      model4_w1(9,2) = -0.10091353952884674d0
      model4_w1(9,3) = 0.6788118481636047d0
      model4_w1(9,4) = -0.3073820471763611d0
      model4_w1(9,5) = 0.20593971014022827d0
      model4_w1(9,6) = 0.3351590633392334d0
! model4_input_layer.bias
      model4_b1(1) = 0.41315123438835144d0
      model4_b1(2) = 0.05366747826337814d0
      model4_b1(3) = -0.4040124714374542d0
      model4_b1(4) = -0.22460471093654633d0
      model4_b1(5) = 0.2720222771167755d0
      model4_b1(6) = -0.33563411235809326d0
      model4_b1(7) = 0.5545889735221863d0
      model4_b1(8) = 0.24843847751617432d0
      model4_b1(9) = 0.3326582908630371d0
! model4_hidden_layers.0.weight
      model4_w2(1,1) = 0.5848321914672852d0
      model4_w2(1,2) = 0.42036572098731995d0
      model4_w2(1,3) = -0.23030869662761688d0
      model4_w2(1,4) = 0.26099857687950134d0
      model4_w2(1,5) = -0.7025095820426941d0
      model4_w2(1,6) = 0.4277702867984772d0
      model4_w2(1,7) = -0.5990339517593384d0
      model4_w2(1,8) = 0.48155122995376587d0
      model4_w2(1,9) = -0.953666627407074d0
      model4_w2(2,1) = 0.2966752350330353d0
      model4_w2(2,2) = 0.3949729800224304d0
      model4_w2(2,3) = -0.2608194053173065d0
      model4_w2(2,4) = 0.17140598595142365d0
      model4_w2(2,5) = -0.9244350790977478d0
      model4_w2(2,6) = 0.5395352840423584d0
      model4_w2(2,7) = -0.37572869658470154d0
      model4_w2(2,8) = 0.39129209518432617d0
      model4_w2(2,9) = -0.5871572494506836d0
      model4_w2(3,1) = 0.11856348812580109d0
      model4_w2(3,2) = 0.47212842106819153d0
      model4_w2(3,3) = 0.04164683818817139d0
      model4_w2(3,4) = 0.38180458545684814d0
      model4_w2(3,5) = -0.5229833722114563d0
      model4_w2(3,6) = 0.5853810906410217d0
      model4_w2(3,7) = -0.7138933539390564d0
      model4_w2(3,8) = 0.6319562792778015d0
      model4_w2(3,9) = -0.6702192425727844d0
      model4_w2(4,1) = 0.3568652272224426d0
      model4_w2(4,2) = 0.3188243806362152d0
      model4_w2(4,3) = -0.31764936447143555d0
      model4_w2(4,4) = -0.3714851438999176d0
      model4_w2(4,5) = 0.5661567449569702d0
      model4_w2(4,6) = -0.19724927842617035d0
      model4_w2(4,7) = 0.462997704744339d0
      model4_w2(4,8) = -0.1493910551071167d0
      model4_w2(4,9) = 0.9436951279640198d0
      model4_w2(5,1) = -0.0779029130935669d0
      model4_w2(5,2) = -0.0333673357963562d0
      model4_w2(5,3) = -0.24022352695465088d0
      model4_w2(5,4) = -0.3982331454753876d0
      model4_w2(5,5) = 0.5769281983375549d0
      model4_w2(5,6) = 0.26303520798683167d0
      model4_w2(5,7) = 0.7743213772773743d0
      model4_w2(5,8) = 0.1317596286535263d0
      model4_w2(5,9) = 0.31992363929748535d0
      model4_w2(6,1) = 0.41612234711647034d0
      model4_w2(6,2) = 0.5231009721755981d0
      model4_w2(6,3) = -0.28617843985557556d0
      model4_w2(6,4) = 0.7981254458427429d0
      model4_w2(6,5) = -0.9394769668579102d0
      model4_w2(6,6) = 0.5106766223907471d0
      model4_w2(6,7) = -0.7985734343528748d0
      model4_w2(6,8) = 0.5482537746429443d0
      model4_w2(6,9) = -0.7141858339309692d0
      model4_w2(7,1) = -0.2336854636669159d0
      model4_w2(7,2) = 0.16899660229682922d0
      model4_w2(7,3) = -0.09804996848106384d0
      model4_w2(7,4) = 0.28558316826820374d0
      model4_w2(7,5) = -0.30989837646484375d0
      model4_w2(7,6) = -0.09836980700492859d0
      model4_w2(7,7) = 0.20754316449165344d0
      model4_w2(7,8) = -0.05161559581756592d0
      model4_w2(7,9) = -0.062308162450790405d0
      model4_w2(8,1) = 0.4182027578353882d0
      model4_w2(8,2) = -0.03415258228778839d0
      model4_w2(8,3) = 0.21568551659584045d0
      model4_w2(8,4) = -0.25534337759017944d0
      model4_w2(8,5) = 0.7807217836380005d0
      model4_w2(8,6) = -0.18331614136695862d0
      model4_w2(8,7) = 0.796384334564209d0
      model4_w2(8,8) = -0.16643135249614716d0
      model4_w2(8,9) = 0.7805476188659668d0
      model4_w2(9,1) = 0.43143099546432495d0
      model4_w2(9,2) = 0.4453035593032837d0
      model4_w2(9,3) = -0.2558598518371582d0
      model4_w2(9,4) = 0.3872497081756592d0
      model4_w2(9,5) = -1.040944218635559d0
      model4_w2(9,6) = 0.3181890547275543d0
      model4_w2(9,7) = -0.9743265509605408d0
      model4_w2(9,8) = 0.5266060829162598d0
      model4_w2(9,9) = -0.7114148139953613d0
! model4_hidden_layers.0.bias
      model4_b2(1) = 0.08122801780700684d0
      model4_b2(2) = 0.33237510919570923d0
      model4_b2(3) = 0.2458355873823166d0
      model4_b2(4) = 0.2183544933795929d0
      model4_b2(5) = 0.23391161859035492d0
      model4_b2(6) = -0.05571733042597771d0
      model4_b2(7) = -0.30256304144859314d0
      model4_b2(8) = 0.23800528049468994d0
      model4_b2(9) = 0.12744136154651642d0
! model4_hidden_layers.1.weight
      model4_w3(1,1) = 0.39174216985702515d0
      model4_w3(1,2) = 0.031161341816186905d0
      model4_w3(1,3) = 0.2666900157928467d0
      model4_w3(1,4) = -1.0900743007659912d0
      model4_w3(1,5) = -0.8392836451530457d0
      model4_w3(1,6) = 0.34437862038612366d0
      model4_w3(1,7) = -0.07414200901985168d0
      model4_w3(1,8) = -0.8806578516960144d0
      model4_w3(1,9) = 0.5745623111724854d0
      model4_w3(2,1) = -0.1300995647907257d0
      model4_w3(2,2) = -0.06981464475393295d0
      model4_w3(2,3) = 0.16408900916576385d0
      model4_w3(2,4) = 0.013466072268784046d0
      model4_w3(2,5) = 0.1480569690465927d0
      model4_w3(2,6) = -0.13735292851924896d0
      model4_w3(2,7) = 0.24632516503334045d0
      model4_w3(2,8) = -0.0943220928311348d0
      model4_w3(2,9) = 0.22353653609752655d0
      model4_w3(3,1) = 0.23804281651973724d0
      model4_w3(3,2) = 0.5242123603820801d0
      model4_w3(3,3) = 0.5731713175773621d0
      model4_w3(3,4) = -0.3971461057662964d0
      model4_w3(3,5) = -0.09248713403940201d0
      model4_w3(3,6) = 0.6463009715080261d0
      model4_w3(3,7) = 0.2017943561077118d0
      model4_w3(3,8) = -0.5760934948921204d0
      model4_w3(3,9) = 0.4404514729976654d0
      model4_w3(4,1) = 0.062333397567272186d0
      model4_w3(4,2) = 0.5486874580383301d0
      model4_w3(4,3) = 0.21206294000148773d0
      model4_w3(4,4) = -0.30538755655288696d0
      model4_w3(4,5) = -0.49495643377304077d0
      model4_w3(4,6) = 0.5930377840995789d0
      model4_w3(4,7) = 0.17209646105766296d0
      model4_w3(4,8) = -0.4380568861961365d0
      model4_w3(4,9) = 0.5074257850646973d0
      model4_w3(5,1) = -0.030546749010682106d0
      model4_w3(5,2) = 0.20476661622524261d0
      model4_w3(5,3) = 0.1996484398841858d0
      model4_w3(5,4) = 0.4934466481208801d0
      model4_w3(5,5) = 0.6351860761642456d0
      model4_w3(5,6) = -0.3506004512310028d0
      model4_w3(5,7) = 0.2584923803806305d0
      model4_w3(5,8) = 0.49582383036613464d0
      model4_w3(5,9) = -0.2544412314891815d0
      model4_w3(6,1) = 0.5640528202056885d0
      model4_w3(6,2) = 0.7114721536636353d0
      model4_w3(6,3) = 0.3910190463066101d0
      model4_w3(6,4) = -0.7922865748405457d0
      model4_w3(6,5) = -0.6672661304473877d0
      model4_w3(6,6) = 0.6951494812965393d0
      model4_w3(6,7) = -0.23406347632408142d0
      model4_w3(6,8) = -0.6926104426383972d0
      model4_w3(6,9) = 0.6649876236915588d0
      model4_w3(7,1) = 0.4987543523311615d0
      model4_w3(7,2) = 0.4769178032875061d0
      model4_w3(7,3) = 0.41424718499183655d0
      model4_w3(7,4) = -0.44857877492904663d0
      model4_w3(7,5) = -0.34245988726615906d0
      model4_w3(7,6) = 0.2728562355041504d0
      model4_w3(7,7) = 0.202224463224411d0
      model4_w3(7,8) = -0.8710115551948547d0
      model4_w3(7,9) = 0.7600855231285095d0
      model4_w3(8,1) = 0.31195923686027527d0
      model4_w3(8,2) = 0.29966241121292114d0
      model4_w3(8,3) = 0.3328818380832672d0
      model4_w3(8,4) = -0.2212703824043274d0
      model4_w3(8,5) = -0.3239545226097107d0
      model4_w3(8,6) = 0.377437561750412d0
      model4_w3(8,7) = -0.32840508222579956d0
      model4_w3(8,8) = -0.7144215106964111d0
      model4_w3(8,9) = 0.6196817755699158d0
      model4_w3(9,1) = 0.10252342373132706d0
      model4_w3(9,2) = -0.23340167105197906d0
      model4_w3(9,3) = 0.2169162482023239d0
      model4_w3(9,4) = 0.7320113778114319d0
      model4_w3(9,5) = 0.24359561502933502d0
      model4_w3(9,6) = -0.18718387186527252d0
      model4_w3(9,7) = -0.04605284333229065d0
      model4_w3(9,8) = 0.7453216910362244d0
      model4_w3(9,9) = 0.03938203305006027d0
! model4_hidden_layers.1.bias
      model4_b3(1) = -0.45470741391181946d0
      model4_b3(2) = -0.21161776781082153d0
      model4_b3(3) = 0.33440619707107544d0
      model4_b3(4) = -0.2698575258255005d0
      model4_b3(5) = 0.7021270990371704d0
      model4_b3(6) = 0.21521607041358948d0
      model4_b3(7) = 0.2618136703968048d0
      model4_b3(8) = 0.04159863293170929d0
      model4_b3(9) = 0.10269683599472046d0
! model4_hidden_layers.2.weight
      model4_w4(1,1) = -0.5313814878463745d0
      model4_w4(1,2) = -0.2340620756149292d0
      model4_w4(1,3) = 0.21378248929977417d0
      model4_w4(1,4) = -0.2422439157962799d0
      model4_w4(1,5) = 0.02581956796348095d0
      model4_w4(1,6) = -0.11798913031816483d0
      model4_w4(1,7) = 0.2334986925125122d0
      model4_w4(1,8) = 0.24119853973388672d0
      model4_w4(1,9) = -0.15413227677345276d0
      model4_w4(2,1) = 0.13365116715431213d0
      model4_w4(2,2) = -0.3355102241039276d0
      model4_w4(2,3) = -0.2949044406414032d0
      model4_w4(2,4) = -0.14710621535778046d0
      model4_w4(2,5) = -0.2908870577812195d0
      model4_w4(2,6) = -0.12725210189819336d0
      model4_w4(2,7) = -0.0002958912227768451d0
      model4_w4(2,8) = 0.22628039121627808d0
      model4_w4(2,9) = 0.2150261253118515d0
      model4_w4(3,1) = 0.33915504813194275d0
      model4_w4(3,2) = -0.2185562252998352d0
      model4_w4(3,3) = 0.45400291681289673d0
      model4_w4(3,4) = 0.23433759808540344d0
      model4_w4(3,5) = -0.9192517995834351d0
      model4_w4(3,6) = 0.6378875970840454d0
      model4_w4(3,7) = 0.7302374243736267d0
      model4_w4(3,8) = 0.348923921585083d0
      model4_w4(3,9) = -0.802047610282898d0
      model4_w4(4,1) = -0.2531765401363373d0
      model4_w4(4,2) = 0.32771244645118713d0
      model4_w4(4,3) = 0.012101918458938599d0
      model4_w4(4,4) = 0.2999139130115509d0
      model4_w4(4,5) = 0.045332759618759155d0
      model4_w4(4,6) = -0.30247995257377625d0
      model4_w4(4,7) = 0.044756025075912476d0
      model4_w4(4,8) = -0.24441945552825928d0
      model4_w4(4,9) = -0.30253052711486816d0
      model4_w4(5,1) = 0.16527129709720612d0
      model4_w4(5,2) = 0.3016798198223114d0
      model4_w4(5,3) = 0.4539889991283417d0
      model4_w4(5,4) = 0.31244346499443054d0
      model4_w4(5,5) = 0.5450723171234131d0
      model4_w4(5,6) = 0.4640461206436157d0
      model4_w4(5,7) = 0.41540297865867615d0
      model4_w4(5,8) = 0.5314103960990906d0
      model4_w4(5,9) = 0.5278537273406982d0
      model4_w4(6,1) = 0.5303247570991516d0
      model4_w4(6,2) = -0.06257451325654984d0
      model4_w4(6,3) = 0.7473086714744568d0
      model4_w4(6,4) = 0.6941357851028442d0
      model4_w4(6,5) = -0.7555753588676453d0
      model4_w4(6,6) = 0.5614686608314514d0
      model4_w4(6,7) = 0.35292214155197144d0
      model4_w4(6,8) = 0.42430615425109863d0
      model4_w4(6,9) = -0.06752002984285355d0
      model4_w4(7,1) = 0.8624584674835205d0
      model4_w4(7,2) = 0.1584264636039734d0
      model4_w4(7,3) = 0.5812022089958191d0
      model4_w4(7,4) = 0.45879796147346497d0
      model4_w4(7,5) = -1.0778076648712158d0
      model4_w4(7,6) = 0.37738558650016785d0
      model4_w4(7,7) = 0.345020592212677d0
      model4_w4(7,8) = 0.47156256437301636d0
      model4_w4(7,9) = -0.5622085332870483d0
      model4_w4(8,1) = -0.4320763349533081d0
      model4_w4(8,2) = -0.20494867861270905d0
      model4_w4(8,3) = 0.3346157968044281d0
      model4_w4(8,4) = -0.007178094238042831d0
      model4_w4(8,5) = 0.15969572961330414d0
      model4_w4(8,6) = -0.2630164325237274d0
      model4_w4(8,7) = 0.15691351890563965d0
      model4_w4(8,8) = 0.03449828922748566d0
      model4_w4(8,9) = 0.24323131144046783d0
      model4_w4(9,1) = -0.10486714541912079d0
      model4_w4(9,2) = 0.3106047213077545d0
      model4_w4(9,3) = -0.6221382021903992d0
      model4_w4(9,4) = 0.0034112439025193453d0
      model4_w4(9,5) = 0.10903827100992203d0
      model4_w4(9,6) = -0.02040020003914833d0
      model4_w4(9,7) = 0.4908692538738251d0
      model4_w4(9,8) = 0.0294374730437994d0
      model4_w4(9,9) = 0.28692877292633057d0
! model4_hidden_layers.2.bias
      model4_b4(1) = -0.23185335099697113d0
      model4_b4(2) = 0.14144587516784668d0
      model4_b4(3) = 0.32712599635124207d0
      model4_b4(4) = -0.28852665424346924d0
      model4_b4(5) = -0.06966987252235413d0
      model4_b4(6) = 0.3721974790096283d0
      model4_b4(7) = 0.1141163632273674d0
      model4_b4(8) = 0.08034434169530869d0
      model4_b4(9) = -0.3822675347328186d0
! model4_output_layer.weight
      model4_w5(1,1) = -0.8961248397827148d0
      model4_w5(1,2) = -0.03721010684967041d0
      model4_w5(1,3) = 0.6562749147415161d0
      model4_w5(1,4) = 0.004011392593383789d0
      model4_w5(1,5) = 0.3225967288017273d0
      model4_w5(1,6) = 0.6425795555114746d0
      model4_w5(1,7) = 0.4958047568798065d0
      model4_w5(1,8) = -0.40985727310180664d0
      model4_w5(1,9) = -0.5445885062217712d0
! model4_output_layer.bias
      model4_b5(1) = 0.16346128284931183d0
      
! model5_input_layer.weight
      model5_w1(1,1) = 0.274230033159256d0
      model5_w1(1,2) = 0.4021267294883728d0
      model5_w1(1,3) = -0.3027137219905853d0
      model5_w1(1,4) = 0.2173243761062622d0
      model5_w1(1,5) = -0.4920958876609802d0
      model5_w1(1,6) = 0.010031433776021004d0
      model5_w1(2,1) = 0.6646416783332825d0
      model5_w1(2,2) = 0.7947642207145691d0
      model5_w1(2,3) = 0.12133079767227173d0
      model5_w1(2,4) = -0.3774116635322571d0
      model5_w1(2,5) = 0.36057591438293457d0
      model5_w1(2,6) = -0.4651421010494232d0
      model5_w1(3,1) = 0.3385658264160156d0
      model5_w1(3,2) = 0.3178502917289734d0
      model5_w1(3,3) = 0.5076451301574707d0
      model5_w1(3,4) = -0.02677210420370102d0
      model5_w1(3,5) = 0.8594026565551758d0
      model5_w1(3,6) = -0.2714616358280182d0
      model5_w1(4,1) = 0.767267644405365d0
      model5_w1(4,2) = 0.10261502861976624d0
      model5_w1(4,3) = 0.6498327851295471d0
      model5_w1(4,4) = -0.5276255011558533d0
      model5_w1(4,5) = 0.8296555280685425d0
      model5_w1(4,6) = -0.6925686597824097d0
      model5_w1(5,1) = 0.12198834121227264d0
      model5_w1(5,2) = 0.4227922856807709d0
      model5_w1(5,3) = 0.09622661024332047d0
      model5_w1(5,4) = 0.42073923349380493d0
      model5_w1(5,5) = -0.7430739998817444d0
      model5_w1(5,6) = 0.55966717004776d0
      model5_w1(6,1) = -0.01500457338988781d0
      model5_w1(6,2) = -0.015879089012742043d0
      model5_w1(6,3) = 0.3486887514591217d0
      model5_w1(6,4) = -0.012383771128952503d0
      model5_w1(6,5) = -0.600693941116333d0
      model5_w1(6,6) = 0.27460435032844543d0
      model5_w1(7,1) = 0.6268417239189148d0
      model5_w1(7,2) = -0.21033518016338348d0
      model5_w1(7,3) = 0.21993893384933472d0
      model5_w1(7,4) = 0.03312631696462631d0
      model5_w1(7,5) = 0.7774891257286072d0
      model5_w1(7,6) = 0.03939241170883179d0
      model5_w1(8,1) = 0.1294574737548828d0
      model5_w1(8,2) = 0.4354468286037445d0
      model5_w1(8,3) = 0.30324020981788635d0
      model5_w1(8,4) = -0.5930847525596619d0
      model5_w1(8,5) = 0.7655346393585205d0
      model5_w1(8,6) = -0.06003199890255928d0
      model5_w1(9,1) = -0.16896487772464752d0
      model5_w1(9,2) = -0.36043643951416016d0
      model5_w1(9,3) = -0.11064508557319641d0
      model5_w1(9,4) = -0.3966793417930603d0
      model5_w1(9,5) = 0.13528496026992798d0
      model5_w1(9,6) = 0.15084856748580933d0
! model5_input_layer.bias
      model5_b1(1) = 0.5249643325805664d0
      model5_b1(2) = 0.7275267243385315d0
      model5_b1(3) = -0.04615715518593788d0
      model5_b1(4) = 0.0326477587223053d0
      model5_b1(5) = 0.699748158454895d0
      model5_b1(6) = 0.7197260856628418d0
      model5_b1(7) = -0.16839952766895294d0
      model5_b1(8) = 0.38625290989875793d0
      model5_b1(9) = 0.05300381779670715d0
! model5_hidden_layers.0.weight
      model5_w2(1,1) = -1.0298560857772827d0
      model5_w2(1,2) = 0.46510979533195496d0
      model5_w2(1,3) = 0.47394266724586487d0
      model5_w2(1,4) = 0.436125785112381d0
      model5_w2(1,5) = -0.568474531173706d0
      model5_w2(1,6) = -0.5628915429115295d0
      model5_w2(1,7) = 0.49203434586524963d0
      model5_w2(1,8) = 0.1457769125699997d0
      model5_w2(1,9) = -0.3169499337673187d0
      model5_w2(2,1) = -1.0298515558242798d0
      model5_w2(2,2) = 0.14693717658519745d0
      model5_w2(2,3) = 0.3117566406726837d0
      model5_w2(2,4) = 0.612298846244812d0
      model5_w2(2,5) = -0.9017335176467896d0
      model5_w2(2,6) = -1.096632957458496d0
      model5_w2(2,7) = 0.17326465249061584d0
      model5_w2(2,8) = 0.7532740235328674d0
      model5_w2(2,9) = 0.24156484007835388d0
      model5_w2(3,1) = -1.087114691734314d0
      model5_w2(3,2) = 0.635990560054779d0
      model5_w2(3,3) = 0.6952679753303528d0
      model5_w2(3,4) = 0.3244853615760803d0
      model5_w2(3,5) = -0.9500294923782349d0
      model5_w2(3,6) = -0.9102177023887634d0
      model5_w2(3,7) = 0.5794639587402344d0
      model5_w2(3,8) = 0.32015886902809143d0
      model5_w2(3,9) = 0.06818351149559021d0
      model5_w2(4,1) = -0.89304119348526d0
      model5_w2(4,2) = 0.5312696099281311d0
      model5_w2(4,3) = 0.3528144061565399d0
      model5_w2(4,4) = 0.4887097477912903d0
      model5_w2(4,5) = -0.8623242378234863d0
      model5_w2(4,6) = -0.9506406784057617d0
      model5_w2(4,7) = 0.5078803896903992d0
      model5_w2(4,8) = 0.7743059396743774d0
      model5_w2(4,9) = 0.1452912986278534d0
      model5_w2(5,1) = 1.0114874839782715d0
      model5_w2(5,2) = 0.15465916693210602d0
      model5_w2(5,3) = -0.06854815036058426d0
      model5_w2(5,4) = -0.05138441175222397d0
      model5_w2(5,5) = 0.6791250705718994d0
      model5_w2(5,6) = 0.7868356704711914d0
      model5_w2(5,7) = -0.3381480872631073d0
      model5_w2(5,8) = 0.25435271859169006d0
      model5_w2(5,9) = -0.28128230571746826d0
      model5_w2(6,1) = -0.8144897222518921d0
      model5_w2(6,2) = 0.6541216373443604d0
      model5_w2(6,3) = 0.22677507996559143d0
      model5_w2(6,4) = 0.6788366436958313d0
      model5_w2(6,5) = -0.8138154745101929d0
      model5_w2(6,6) = -0.626384437084198d0
      model5_w2(6,7) = 0.5292165279388428d0
      model5_w2(6,8) = 0.36432787775993347d0
      model5_w2(6,9) = 0.1251576840877533d0
      model5_w2(7,1) = 1.1556696891784668d0
      model5_w2(7,2) = 0.16277097165584564d0
      model5_w2(7,3) = 0.12998250126838684d0
      model5_w2(7,4) = 0.15016505122184753d0
      model5_w2(7,5) = 0.8171055316925049d0
      model5_w2(7,6) = 0.9719035029411316d0
      model5_w2(7,7) = -0.5172800421714783d0
      model5_w2(7,8) = -0.015751328319311142d0
      model5_w2(7,9) = 0.3289203941822052d0
      model5_w2(8,1) = 0.03176931291818619d0
      model5_w2(8,2) = 0.11599022895097733d0
      model5_w2(8,3) = -0.3168545961380005d0
      model5_w2(8,4) = -0.03081228956580162d0
      model5_w2(8,5) = -0.13772784173488617d0
      model5_w2(8,6) = -0.047575391829013824d0
      model5_w2(8,7) = -0.0760456845164299d0
      model5_w2(8,8) = -0.25730377435684204d0
      model5_w2(8,9) = -0.0200139582157135d0
      model5_w2(9,1) = 0.5486839413642883d0
      model5_w2(9,2) = 0.45291629433631897d0
      model5_w2(9,3) = -0.05665314570069313d0
      model5_w2(9,4) = -0.31008732318878174d0
      model5_w2(9,5) = 0.5038348436355591d0
      model5_w2(9,6) = 1.068010687828064d0
      model5_w2(9,7) = -0.12443063408136368d0
      model5_w2(9,8) = -0.06370207667350769d0
      model5_w2(9,9) = 0.1422746181488037d0
! model5_hidden_layers.0.bias
      model5_b2(1) = 0.4875529408454895d0
      model5_b2(2) = 0.17069070041179657d0
      model5_b2(3) = 0.29555702209472656d0
      model5_b2(4) = 0.298061728477478d0
      model5_b2(5) = 0.5615718364715576d0
      model5_b2(6) = 0.013665017671883106d0
      model5_b2(7) = 0.0678958147764206d0
      model5_b2(8) = -0.06124445050954819d0
      model5_b2(9) = 0.7398881316184998d0
! model5_hidden_layers.1.weight
      model5_w3(1,1) = 0.19172132015228271d0
      model5_w3(1,2) = -0.05150396749377251d0
      model5_w3(1,3) = -0.2903057038784027d0
      model5_w3(1,4) = 0.06817029416561127d0
      model5_w3(1,5) = 0.9597054123878479d0
      model5_w3(1,6) = 0.041655994951725006d0
      model5_w3(1,7) = 0.8400248289108276d0
      model5_w3(1,8) = 0.2721449136734009d0
      model5_w3(1,9) = 0.7906648516654968d0
      model5_w3(2,1) = 0.22855380177497864d0
      model5_w3(2,2) = -0.065830759704113d0
      model5_w3(2,3) = -0.23940999805927277d0
      model5_w3(2,4) = 0.2647605538368225d0
      model5_w3(2,5) = 0.832116425037384d0
      model5_w3(2,6) = -0.21598714590072632d0
      model5_w3(2,7) = 0.6080795526504517d0
      model5_w3(2,8) = 0.09225526452064514d0
      model5_w3(2,9) = 0.6090695858001709d0
      model5_w3(3,1) = -0.14917702972888947d0
      model5_w3(3,2) = -0.11698298156261444d0
      model5_w3(3,3) = -0.1551867425441742d0
      model5_w3(3,4) = -0.17239992320537567d0
      model5_w3(3,5) = -0.07462682574987411d0
      model5_w3(3,6) = -0.2134791910648346d0
      model5_w3(3,7) = -0.18161281943321228d0
      model5_w3(3,8) = -0.26860323548316956d0
      model5_w3(3,9) = -0.1324983388185501d0
      model5_w3(4,1) = 0.022839831188321114d0
      model5_w3(4,2) = -0.18965217471122742d0
      model5_w3(4,3) = -0.07428710162639618d0
      model5_w3(4,4) = 0.0889308974146843d0
      model5_w3(4,5) = -0.0410170815885067d0
      model5_w3(4,6) = 0.09490186721086502d0
      model5_w3(4,7) = -0.1510569006204605d0
      model5_w3(4,8) = -0.16823673248291016d0
      model5_w3(4,9) = 0.15379150211811066d0
      model5_w3(5,1) = 0.5077649354934692d0
      model5_w3(5,2) = 0.749265193939209d0
      model5_w3(5,3) = 0.2708509564399719d0
      model5_w3(5,4) = 0.777698814868927d0
      model5_w3(5,5) = -0.8074890971183777d0
      model5_w3(5,6) = 0.5884124636650085d0
      model5_w3(5,7) = -0.7641353607177734d0
      model5_w3(5,8) = -0.10244033485651016d0
      model5_w3(5,9) = -0.9797330498695374d0
      model5_w3(6,1) = -0.17813652753829956d0
      model5_w3(6,2) = -0.20901881158351898d0
      model5_w3(6,3) = -0.032267823815345764d0
      model5_w3(6,4) = -0.009162388741970062d0
      model5_w3(6,5) = 0.9027639031410217d0
      model5_w3(6,6) = 0.26188430190086365d0
      model5_w3(6,7) = 1.0594786405563354d0
      model5_w3(6,8) = -0.018678776919841766d0
      model5_w3(6,9) = 0.6876705884933472d0
      model5_w3(7,1) = 0.7526639699935913d0
      model5_w3(7,2) = 0.7411093711853027d0
      model5_w3(7,3) = 0.6853732466697693d0
      model5_w3(7,4) = 0.5671345591545105d0
      model5_w3(7,5) = -0.5782483220100403d0
      model5_w3(7,6) = 0.7851797938346863d0
      model5_w3(7,7) = -0.9627465009689331d0
      model5_w3(7,8) = 0.295524924993515d0
      model5_w3(7,9) = -0.9575275182723999d0
      model5_w3(8,1) = 0.0672699511051178d0
      model5_w3(8,2) = -0.29662904143333435d0
      model5_w3(8,3) = -0.1989375799894333d0
      model5_w3(8,4) = 0.13411343097686768d0
      model5_w3(8,5) = -0.31998157501220703d0
      model5_w3(8,6) = -0.07466128468513489d0
      model5_w3(8,7) = -0.04949352145195007d0
      model5_w3(8,8) = 0.14856722950935364d0
      model5_w3(8,9) = -0.0907738208770752d0
      model5_w3(9,1) = 0.18565741181373596d0
      model5_w3(9,2) = 0.866307258605957d0
      model5_w3(9,3) = 0.7843379378318787d0
      model5_w3(9,4) = 0.7175877690315247d0
      model5_w3(9,5) = -0.9039617776870728d0
      model5_w3(9,6) = 0.6957454085350037d0
      model5_w3(9,7) = -0.8329935073852539d0
      model5_w3(9,8) = 0.16405317187309265d0
      model5_w3(9,9) = -1.053915023803711d0
! model5_hidden_layers.1.bias
      model5_b3(1) = 0.3118484914302826d0
      model5_b3(2) = 0.1861804872751236d0
      model5_b3(3) = 0.35418108105659485d0
      model5_b3(4) = -0.19664381444454193d0
      model5_b3(5) = -0.05959884077310562d0
      model5_b3(6) = 0.5311719179153442d0
      model5_b3(7) = 0.3360762894153595d0
      model5_b3(8) = -0.33319002389907837d0
      model5_b3(9) = 0.19035418331623077d0
! model5_hidden_layers.2.weight
      model5_w4(1,1) = 0.2761152684688568d0
      model5_w4(1,2) = 0.520323634147644d0
      model5_w4(1,3) = -0.23158614337444305d0
      model5_w4(1,4) = -0.04797378182411194d0
      model5_w4(1,5) = -0.6576027870178223d0
      model5_w4(1,6) = 0.19989685714244843d0
      model5_w4(1,7) = -0.4703088700771332d0
      model5_w4(1,8) = -0.23096701502799988d0
      model5_w4(1,9) = -0.701331615447998d0
      model5_w4(2,1) = 0.29435035586357117d0
      model5_w4(2,2) = 0.09159509837627411d0
      model5_w4(2,3) = -0.11440778523683548d0
      model5_w4(2,4) = 0.12865102291107178d0
      model5_w4(2,5) = -0.034544143825769424d0
      model5_w4(2,6) = 0.7000460624694824d0
      model5_w4(2,7) = -0.2543598711490631d0
      model5_w4(2,8) = -0.21664485335350037d0
      model5_w4(2,9) = 0.2685823142528534d0
      model5_w4(3,1) = -0.6384822726249695d0
      model5_w4(3,2) = -0.33364957571029663d0
      model5_w4(3,3) = 0.08133071660995483d0
      model5_w4(3,4) = 0.0705464780330658d0
      model5_w4(3,5) = 0.267907977104187d0
      model5_w4(3,6) = -0.7213244438171387d0
      model5_w4(3,7) = 0.8235994577407837d0
      model5_w4(3,8) = 0.20643523335456848d0
      model5_w4(3,9) = 0.40217456221580505d0
      model5_w4(4,1) = -0.18636886775493622d0
      model5_w4(4,2) = 0.15790840983390808d0
      model5_w4(4,3) = 0.025533050298690796d0
      model5_w4(4,4) = -0.09658870100975037d0
      model5_w4(4,5) = 0.10926306247711182d0
      model5_w4(4,6) = -0.2979825437068939d0
      model5_w4(4,7) = -0.24322077631950378d0
      model5_w4(4,8) = -0.05004921555519104d0
      model5_w4(4,9) = -0.031951457262039185d0
      model5_w4(5,1) = -0.09362153708934784d0
      model5_w4(5,2) = 0.25506916642189026d0
      model5_w4(5,3) = -0.12739920616149902d0
      model5_w4(5,4) = -0.3000548779964447d0
      model5_w4(5,5) = 0.08832821249961853d0
      model5_w4(5,6) = -0.14940766990184784d0
      model5_w4(5,7) = -0.2986789643764496d0
      model5_w4(5,8) = -0.22733017802238464d0
      model5_w4(5,9) = 0.002132326364517212d0
      model5_w4(6,1) = 1.0344724655151367d0
      model5_w4(6,2) = 0.8203356266021729d0
      model5_w4(6,3) = -0.07241404056549072d0
      model5_w4(6,4) = 0.2725367248058319d0
      model5_w4(6,5) = -0.4547186493873596d0
      model5_w4(6,6) = 0.71480393409729d0
      model5_w4(6,7) = 0.09473508596420288d0
      model5_w4(6,8) = 0.1259211301803589d0
      model5_w4(6,9) = -0.4160495400428772d0
      model5_w4(7,1) = -0.7206463813781738d0
      model5_w4(7,2) = -0.6428290605545044d0
      model5_w4(7,3) = 0.32580482959747314d0
      model5_w4(7,4) = 0.21754080057144165d0
      model5_w4(7,5) = 0.6160624027252197d0
      model5_w4(7,6) = -0.6158462762832642d0
      model5_w4(7,7) = 0.8080867528915405d0
      model5_w4(7,8) = -0.21075384318828583d0
      model5_w4(7,9) = 0.8063221573829651d0
      model5_w4(8,1) = -0.0649695098400116d0
      model5_w4(8,2) = -0.17128083109855652d0
      model5_w4(8,3) = 0.06625863909721375d0
      model5_w4(8,4) = -0.24318647384643555d0
      model5_w4(8,5) = 0.09671583771705627d0
      model5_w4(8,6) = -0.06731465458869934d0
      model5_w4(8,7) = -0.26434773206710815d0
      model5_w4(8,8) = 0.10336479544639587d0
      model5_w4(8,9) = 0.0535026490688324d0
      model5_w4(9,1) = 0.18969109654426575d0
      model5_w4(9,2) = 0.5004432797431946d0
      model5_w4(9,3) = 0.14320553839206696d0
      model5_w4(9,4) = -0.2264983206987381d0
      model5_w4(9,5) = -0.26027941703796387d0
      model5_w4(9,6) = 0.8395125269889832d0
      model5_w4(9,7) = 0.28640925884246826d0
      model5_w4(9,8) = 0.3109501898288727d0
      model5_w4(9,9) = -0.12712745368480682d0
! model5_hidden_layers.2.bias
      model5_b4(1) = 0.06527981162071228d0
      model5_b4(2) = 0.34243571758270264d0
      model5_b4(3) = 0.34987249970436096d0
      model5_b4(4) = -0.011126965284347534d0
      model5_b4(5) = -0.2036632001399994d0
      model5_b4(6) = 0.4574716091156006d0
      model5_b4(7) = 0.12920281291007996d0
      model5_b4(8) = -0.24000191688537598d0
      model5_b4(9) = 0.35437288880348206d0
! model5_output_layer.weight
      model5_w5(1,1) = -0.9076324701309204d0
      model5_w5(1,2) = -0.5524426698684692d0
      model5_w5(1,3) = 0.5430824756622314d0
      model5_w5(1,4) = -0.07785782217979431d0
      model5_w5(1,5) = -0.08858180046081543d0
      model5_w5(1,6) = 1.0661778450012207d0
      model5_w5(1,7) = 0.8092851042747498d0
      model5_w5(1,8) = -0.005686163902282715d0
      model5_w5(1,9) = -0.8217713832855225d0
! model5_output_layer.bias
      model5_b5(1) = 0.4216831624507904d0
      
! model6_input_layer.weight
      model6_w1(1,1) = -0.3476015329360962d0
      model6_w1(1,2) = -0.28181779384613037d0
      model6_w1(1,3) = 0.07364100217819214d0
      model6_w1(1,4) = -0.022534579038619995d0
      model6_w1(1,5) = 0.15380859375d0
      model6_w1(1,6) = 0.13468271493911743d0
      model6_w1(2,1) = -0.30178284645080566d0
      model6_w1(2,2) = 0.07779350876808167d0
      model6_w1(2,3) = 0.04305320978164673d0
      model6_w1(2,4) = 0.2855098843574524d0
      model6_w1(2,5) = -0.2926194667816162d0
      model6_w1(2,6) = -0.19060134887695312d0
      model6_w1(3,1) = 0.08144449442625046d0
      model6_w1(3,2) = 0.1743149608373642d0
      model6_w1(3,3) = -0.26965785026550293d0
      model6_w1(3,4) = -0.12850768864154816d0
      model6_w1(3,5) = 0.08837825059890747d0
      model6_w1(3,6) = -0.32254117727279663d0
      model6_w1(4,1) = -0.30331289768218994d0
      model6_w1(4,2) = -0.3383430242538452d0
      model6_w1(4,3) = 0.05475801229476929d0
      model6_w1(4,4) = -0.21622198820114136d0
      model6_w1(4,5) = 0.3479512333869934d0
      model6_w1(4,6) = -0.00598570704460144d0
      model6_w1(5,1) = -0.059823259711265564d0
      model6_w1(5,2) = 0.2646050751209259d0
      model6_w1(5,3) = 0.07185878604650497d0
      model6_w1(5,4) = 0.11437205970287323d0
      model6_w1(5,5) = 0.18261589109897614d0
      model6_w1(5,6) = -0.49777090549468994d0
      model6_w1(6,1) = -0.35908013582229614d0
      model6_w1(6,2) = 0.21053844690322876d0
      model6_w1(6,3) = 0.04245258867740631d0
      model6_w1(6,4) = -0.16739395260810852d0
      model6_w1(6,5) = -0.23682773113250732d0
      model6_w1(6,6) = 0.07482350617647171d0
      model6_w1(7,1) = 0.02696326933801174d0
      model6_w1(7,2) = -0.23281444609165192d0
      model6_w1(7,3) = -0.11835326999425888d0
      model6_w1(7,4) = -0.3986535966396332d0
      model6_w1(7,5) = 0.321298211812973d0
      model6_w1(7,6) = 0.03266619145870209d0
      model6_w1(8,1) = 0.3422641456127167d0
      model6_w1(8,2) = 0.5431178212165833d0
      model6_w1(8,3) = 0.13624246418476105d0
      model6_w1(8,4) = 0.4874892234802246d0
      model6_w1(8,5) = -0.21971732378005981d0
      model6_w1(8,6) = 0.6410597562789917d0
      model6_w1(9,1) = 0.023684443905949593d0
      model6_w1(9,2) = 0.6246466040611267d0
      model6_w1(9,3) = 0.16253119707107544d0
      model6_w1(9,4) = -0.05028501898050308d0
      model6_w1(9,5) = -0.2556476294994354d0
      model6_w1(9,6) = 0.7427229285240173d0
! model6_input_layer.bias
      model6_b1(1) = -0.042239993810653687d0
      model6_b1(2) = -0.30461958050727844d0
      model6_b1(3) = -0.34310248494148254d0
      model6_b1(4) = -0.10202470421791077d0
      model6_b1(5) = 0.4233376383781433d0
      model6_b1(6) = -0.452224463224411d0
      model6_b1(7) = -0.05882270634174347d0
      model6_b1(8) = 0.28190290927886963d0
      model6_b1(9) = -0.25068533420562744d0
! model6_hidden_layers.0.weight
      model6_w2(1,1) = 0.19741597771644592d0
      model6_w2(1,2) = -0.32516440749168396d0
      model6_w2(1,3) = -0.23029746115207672d0
      model6_w2(1,4) = -0.19865676760673523d0
      model6_w2(1,5) = 0.39485853910446167d0
      model6_w2(1,6) = 0.27583590149879456d0
      model6_w2(1,7) = -0.38408759236335754d0
      model6_w2(1,8) = -0.015438421629369259d0
      model6_w2(1,9) = -0.09880005568265915d0
      model6_w2(2,1) = -0.06889820098876953d0
      model6_w2(2,2) = -0.07170847058296204d0
      model6_w2(2,3) = 0.014299348928034306d0
      model6_w2(2,4) = -0.04801180958747864d0
      model6_w2(2,5) = -0.6370754241943359d0
      model6_w2(2,6) = -0.18186745047569275d0
      model6_w2(2,7) = 0.2396346628665924d0
      model6_w2(2,8) = 0.040339916944503784d0
      model6_w2(2,9) = 0.47921961545944214d0
      model6_w2(3,1) = -0.07901182770729065d0
      model6_w2(3,2) = 0.14004549384117126d0
      model6_w2(3,3) = -0.32654649019241333d0
      model6_w2(3,4) = 0.2933987081050873d0
      model6_w2(3,5) = -0.28565484285354614d0
      model6_w2(3,6) = 0.06935310363769531d0
      model6_w2(3,7) = 0.23388054966926575d0
      model6_w2(3,8) = -0.05578291416168213d0
      model6_w2(3,9) = -0.2570198178291321d0
      model6_w2(4,1) = 0.10636588931083679d0
      model6_w2(4,2) = -0.009752422571182251d0
      model6_w2(4,3) = 0.11431974917650223d0
      model6_w2(4,4) = 0.26526662707328796d0
      model6_w2(4,5) = 0.1836547702550888d0
      model6_w2(4,6) = -0.2647102475166321d0
      model6_w2(4,7) = -0.20383226871490479d0
      model6_w2(4,8) = -0.010527808219194412d0
      model6_w2(4,9) = -0.11413663625717163d0
      model6_w2(5,1) = 0.30987706780433655d0
      model6_w2(5,2) = 0.17631646990776062d0
      model6_w2(5,3) = 0.258585125207901d0
      model6_w2(5,4) = 0.2665710747241974d0
      model6_w2(5,5) = -0.1415008306503296d0
      model6_w2(5,6) = -0.16474828124046326d0
      model6_w2(5,7) = -0.11601841449737549d0
      model6_w2(5,8) = -0.21526584029197693d0
      model6_w2(5,9) = -0.004546314477920532d0
      model6_w2(6,1) = -0.0016180574893951416d0
      model6_w2(6,2) = 0.29454323649406433d0
      model6_w2(6,3) = 0.04948166012763977d0
      model6_w2(6,4) = -0.20460176467895508d0
      model6_w2(6,5) = 0.2787933647632599d0
      model6_w2(6,6) = -0.19675669074058533d0
      model6_w2(6,7) = 0.3161090016365051d0
      model6_w2(6,8) = -0.32682278752326965d0
      model6_w2(6,9) = -0.1925942599773407d0
      model6_w2(7,1) = -0.1149449348449707d0
      model6_w2(7,2) = -0.05085781216621399d0
      model6_w2(7,3) = -0.2691577076911926d0
      model6_w2(7,4) = 0.07007443904876709d0
      model6_w2(7,5) = 0.6035893559455872d0
      model6_w2(7,6) = -0.17171397805213928d0
      model6_w2(7,7) = -0.06134718656539917d0
      model6_w2(7,8) = 0.23081660270690918d0
      model6_w2(7,9) = -0.24304309487342834d0
      model6_w2(8,1) = 0.28993406891822815d0
      model6_w2(8,2) = -0.2877475619316101d0
      model6_w2(8,3) = 0.004124730825424194d0
      model6_w2(8,4) = 0.21292075514793396d0
      model6_w2(8,5) = -0.2879670262336731d0
      model6_w2(8,6) = -0.2950197458267212d0
      model6_w2(8,7) = -0.011240214109420776d0
      model6_w2(8,8) = -0.12227694690227509d0
      model6_w2(8,9) = -0.24706777930259705d0
      model6_w2(9,1) = -0.2632444500923157d0
      model6_w2(9,2) = 0.18915703892707825d0
      model6_w2(9,3) = 0.2883448600769043d0
      model6_w2(9,4) = 0.0980992317199707d0
      model6_w2(9,5) = -0.21452844142913818d0
      model6_w2(9,6) = -0.17890247702598572d0
      model6_w2(9,7) = 0.04880715534090996d0
      model6_w2(9,8) = 0.21515904366970062d0
      model6_w2(9,9) = 0.526331901550293d0
! model6_hidden_layers.0.bias
      model6_b2(1) = 0.40956488251686096d0
      model6_b2(2) = 0.050219714641571045d0
      model6_b2(3) = -0.283030241727829d0
      model6_b2(4) = 0.42453116178512573d0
      model6_b2(5) = 0.0009319782257080078d0
      model6_b2(6) = -0.03685932233929634d0
      model6_b2(7) = -0.02323204278945923d0
      model6_b2(8) = -0.2979136109352112d0
      model6_b2(9) = -0.022847866639494896d0
! model6_hidden_layers.1.weight
      model6_w3(1,1) = -0.3799307346343994d0
      model6_w3(1,2) = 0.4283704161643982d0
      model6_w3(1,3) = -0.22099339962005615d0
      model6_w3(1,4) = -0.07521726191043854d0
      model6_w3(1,5) = 0.2903479039669037d0
      model6_w3(1,6) = 0.23268872499465942d0
      model6_w3(1,7) = -0.2921638488769531d0
      model6_w3(1,8) = -0.054812073707580566d0
      model6_w3(1,9) = 0.3546871840953827d0
      model6_w3(2,1) = -0.23676152527332306d0
      model6_w3(2,2) = 0.15994393825531006d0
      model6_w3(2,3) = -0.08142220973968506d0
      model6_w3(2,4) = -0.4200062155723572d0
      model6_w3(2,5) = -0.27597367763519287d0
      model6_w3(2,6) = 0.08108624815940857d0
      model6_w3(2,7) = -0.3799360692501068d0
      model6_w3(2,8) = -0.2812359035015106d0
      model6_w3(2,9) = 0.5494256019592285d0
      model6_w3(3,1) = -0.10702678561210632d0
      model6_w3(3,2) = 0.44093137979507446d0
      model6_w3(3,3) = 0.03768599033355713d0
      model6_w3(3,4) = 0.061596035957336426d0
      model6_w3(3,5) = -0.2245233952999115d0
      model6_w3(3,6) = 0.0603446327149868d0
      model6_w3(3,7) = -0.16781091690063477d0
      model6_w3(3,8) = 0.1958082616329193d0
      model6_w3(3,9) = 0.019178899005055428d0
      model6_w3(4,1) = -0.22872690856456757d0
      model6_w3(4,2) = 0.265376478433609d0
      model6_w3(4,3) = 0.011362582445144653d0
      model6_w3(4,4) = 0.10121104121208191d0
      model6_w3(4,5) = 0.012826919555664062d0
      model6_w3(4,6) = 0.15510809421539307d0
      model6_w3(4,7) = 0.09827625751495361d0
      model6_w3(4,8) = 0.08638215065002441d0
      model6_w3(4,9) = -0.30613812804222107d0
      model6_w3(5,1) = -0.2926948666572571d0
      model6_w3(5,2) = -0.04881568253040314d0
      model6_w3(5,3) = -0.277682363986969d0
      model6_w3(5,4) = -0.3712320625782013d0
      model6_w3(5,5) = -0.019307732582092285d0
      model6_w3(5,6) = 0.08566639572381973d0
      model6_w3(5,7) = 0.049865882843732834d0
      model6_w3(5,8) = -0.18304388225078583d0
      model6_w3(5,9) = -0.2788070738315582d0
      model6_w3(6,1) = -0.07602693885564804d0
      model6_w3(6,2) = -0.16416144371032715d0
      model6_w3(6,3) = 0.19906899333000183d0
      model6_w3(6,4) = -0.19307565689086914d0
      model6_w3(6,5) = 0.1259807050228119d0
      model6_w3(6,6) = 5.4210424423217773e-05
      model6_w3(6,7) = -0.04139783978462219d0
      model6_w3(6,8) = -0.3321627080440521d0
      model6_w3(6,9) = 0.13297785818576813d0
      model6_w3(7,1) = -0.29899662733078003d0
      model6_w3(7,2) = 0.4047123193740845d0
      model6_w3(7,3) = -0.002040952444076538d0
      model6_w3(7,4) = -0.2764589786529541d0
      model6_w3(7,5) = -0.1608128547668457d0
      model6_w3(7,6) = 0.2265317142009735d0
      model6_w3(7,7) = -0.07087072730064392d0
      model6_w3(7,8) = 0.24219533801078796d0
      model6_w3(7,9) = 0.504601001739502d0
      model6_w3(8,1) = 0.005879592150449753d0
      model6_w3(8,2) = 0.3441278040409088d0
      model6_w3(8,3) = 0.06106269359588623d0
      model6_w3(8,4) = -0.08793391287326813d0
      model6_w3(8,5) = 0.08541765809059143d0
      model6_w3(8,6) = -0.2538812756538391d0
      model6_w3(8,7) = -0.561246931552887d0
      model6_w3(8,8) = -0.052643150091171265d0
      model6_w3(8,9) = 0.19164001941680908d0
      model6_w3(9,1) = -0.15149164199829102d0
      model6_w3(9,2) = -0.06817708909511566d0
      model6_w3(9,3) = -0.14769887924194336d0
      model6_w3(9,4) = -0.09916270524263382d0
      model6_w3(9,5) = 0.1802603304386139d0
      model6_w3(9,6) = 0.26600319147109985d0
      model6_w3(9,7) = -0.2930203080177307d0
      model6_w3(9,8) = 0.1412193477153778d0
      model6_w3(9,9) = 0.25680166482925415d0
! model6_hidden_layers.1.bias
      model6_b3(1) = 0.3088127374649048d0
      model6_b3(2) = 0.11213581264019012d0
      model6_b3(3) = 0.3130018711090088d0
      model6_b3(4) = -0.2985643148422241d0
      model6_b3(5) = 0.22597794234752655d0
      model6_b3(6) = -0.0466424934566021d0
      model6_b3(7) = -0.1135701984167099d0
      model6_b3(8) = 0.2622096538543701d0
      model6_b3(9) = 0.1669013500213623d0
! model6_hidden_layers.2.weight
      model6_w4(1,1) = 0.2586227059364319d0
      model6_w4(1,2) = -0.0274825282394886d0
      model6_w4(1,3) = 0.41958072781562805d0
      model6_w4(1,4) = -0.24694018065929413d0
      model6_w4(1,5) = -0.1044778823852539d0
      model6_w4(1,6) = 0.047192662954330444d0
      model6_w4(1,7) = 0.10832423716783524d0
      model6_w4(1,8) = -0.08063898980617523d0
      model6_w4(1,9) = 0.33679383993148804d0
      model6_w4(2,1) = 0.22603870928287506d0
      model6_w4(2,2) = 0.21546122431755066d0
      model6_w4(2,3) = -0.10882333666086197d0
      model6_w4(2,4) = -0.2692098021507263d0
      model6_w4(2,5) = 0.24895235896110535d0
      model6_w4(2,6) = -0.14388540387153625d0
      model6_w4(2,7) = 0.23253154754638672d0
      model6_w4(2,8) = 0.20989106595516205d0
      model6_w4(2,9) = 0.392365038394928d0
      model6_w4(3,1) = -0.05568590760231018d0
      model6_w4(3,2) = 0.1412133276462555d0
      model6_w4(3,3) = -0.31910794973373413d0
      model6_w4(3,4) = -0.1468500792980194d0
      model6_w4(3,5) = 0.17309758067131042d0
      model6_w4(3,6) = 0.06885191798210144d0
      model6_w4(3,7) = -0.09515008330345154d0
      model6_w4(3,8) = -0.13850438594818115d0
      model6_w4(3,9) = -0.168204665184021d0
      model6_w4(4,1) = -0.28163817524909973d0
      model6_w4(4,2) = -0.04086434468626976d0
      model6_w4(4,3) = 0.054384563118219376d0
      model6_w4(4,4) = -0.011220455169677734d0
      model6_w4(4,5) = -0.026287470012903214d0
      model6_w4(4,6) = -0.3256872892379761d0
      model6_w4(4,7) = 0.09210068732500076d0
      model6_w4(4,8) = -0.17703083157539368d0
      model6_w4(4,9) = 0.01845310628414154d0
      model6_w4(5,1) = 0.402659147977829d0
      model6_w4(5,2) = 0.03468475490808487d0
      model6_w4(5,3) = 0.4693647027015686d0
      model6_w4(5,4) = -0.3278671205043793d0
      model6_w4(5,5) = 0.04696447029709816d0
      model6_w4(5,6) = -0.17312371730804443d0
      model6_w4(5,7) = 0.48645180463790894d0
      model6_w4(5,8) = 0.28437307476997375d0
      model6_w4(5,9) = 0.39969170093536377d0
      model6_w4(6,1) = 0.3873388469219208d0
      model6_w4(6,2) = 0.5300235152244568d0
      model6_w4(6,3) = -0.057009272277355194d0
      model6_w4(6,4) = 0.0732257068157196d0
      model6_w4(6,5) = 0.14461031556129456d0
      model6_w4(6,6) = -0.08158212155103683d0
      model6_w4(6,7) = 0.27092352509498596d0
      model6_w4(6,8) = 0.2800224721431732d0
      model6_w4(6,9) = 0.40525639057159424d0
      model6_w4(7,1) = 0.20811352133750916d0
      model6_w4(7,2) = 0.339577853679657d0
      model6_w4(7,3) = 0.3758852183818817d0
      model6_w4(7,4) = 0.01435193419456482d0
      model6_w4(7,5) = 0.013763883151113987d0
      model6_w4(7,6) = 0.026062235236167908d0
      model6_w4(7,7) = 0.4776577949523926d0
      model6_w4(7,8) = 0.3271467089653015d0
      model6_w4(7,9) = 0.36605119705200195d0
      model6_w4(8,1) = -0.04416784644126892d0
      model6_w4(8,2) = -0.3313276469707489d0
      model6_w4(8,3) = -0.28023621439933777d0
      model6_w4(8,4) = -0.19844043254852295d0
      model6_w4(8,5) = -0.20700065791606903d0
      model6_w4(8,6) = 0.154556006193161d0
      model6_w4(8,7) = -0.294715017080307d0
      model6_w4(8,8) = 0.04929348826408386d0
      model6_w4(8,9) = -0.026705384254455566d0
      model6_w4(9,1) = -0.11106209456920624d0
      model6_w4(9,2) = 0.018243879079818726d0
      model6_w4(9,3) = -0.2834893465042114d0
      model6_w4(9,4) = -0.12871870398521423d0
      model6_w4(9,5) = -0.06692513823509216d0
      model6_w4(9,6) = 0.2812276780605316d0
      model6_w4(9,7) = -0.2903895080089569d0
      model6_w4(9,8) = 0.0044480860233306885d0
      model6_w4(9,9) = -0.14214956760406494d0
! model6_hidden_layers.2.bias
      model6_b4(1) = -0.1262567937374115d0
      model6_b4(2) = 0.27204999327659607d0
      model6_b4(3) = -0.08754751086235046d0
      model6_b4(4) = 0.14659912884235382d0
      model6_b4(5) = 0.1740460991859436d0
      model6_b4(6) = -0.040832631289958954d0
      model6_b4(7) = -0.007703576702624559d0
      model6_b4(8) = -0.2518331706523895d0
      model6_b4(9) = -0.12824460864067078d0
! model6_output_layer.weight
      model6_w5(1,1) = 0.1465604305267334d0
      model6_w5(1,2) = 0.28401967883110046d0
      model6_w5(1,3) = 0.2688601315021515d0
      model6_w5(1,4) = -0.13296617567539215d0
      model6_w5(1,5) = 0.17895270884037018d0
      model6_w5(1,6) = 0.3627700209617615d0
      model6_w5(1,7) = 0.3805962800979614d0
      model6_w5(1,8) = -0.03884503245353699d0
      model6_w5(1,9) = 0.0029549598693847656d0
! model6_output_layer.bias
      model6_b5(1) = -0.12402419745922089d0


      
      
      
      
      B_sym(1)=DFGRD1(1, 1)**2+DFGRD1(1, 2)**2+DFGRD1(1, 3)**2
      B_sym(2)=DFGRD1(2, 1)**2+DFGRD1(2, 2)**2+DFGRD1(2, 3)**2
      B_sym(3)=DFGRD1(3, 3)**2+DFGRD1(3, 1)**2+DFGRD1(3, 2)**2
      B_sym(4)=DFGRD1(1, 1)*DFGRD1(2, 1)+DFGRD1(1, 2)*DFGRD1(2, 2)
     1       +DFGRD1(1, 3)*DFGRD1(2, 3)
      IF(NSHR.EQ.3) THEN
        B_sym(5)=DFGRD1(1, 1)*DFGRD1(3, 1)+DFGRD1(1, 2)*DFGRD1(3, 2)
     1         +DFGRD1(1, 3)*DFGRD1(3, 3)
        B_sym(6)=DFGRD1(2, 1)*DFGRD1(3, 1)+DFGRD1(2, 2)*DFGRD1(3, 2)
     1         +DFGRD1(2, 3)*DFGRD1(3, 3)
      END IF
      
      
      
      
      !B_sym1(1)=B_sym(1)+eps_d
      !B_sym1(2)=B_sym(2)
      !B_sym1(3)=B_sym(3)
      !B_sym1(4)=B_sym(4)
      !B_sym1(5)=B_sym(5)
      !B_sym1(6)=B_sym(6)
      !
      !B_sym2(1)=B_sym(1)
      !B_sym2(2)=B_sym(2)+eps_d
      !B_sym2(3)=B_sym(3)
      !B_sym2(4)=B_sym(4)
      !B_sym2(5)=B_sym(5)
      !B_sym2(6)=B_sym(6)
      !B_sym3(1)=B_sym(1)
      !B_sym3(2)=B_sym(2)
      !B_sym3(3)=B_sym(3)+eps_d
      !B_sym3(4)=B_sym(4)
      !B_sym3(5)=B_sym(5)
      !B_sym3(6)=B_sym(6)
      !B_sym4(1)=B_sym(1)
      !B_sym4(2)=B_sym(2)
      !B_sym4(3)=B_sym(3)
      !B_sym4(4)=B_sym(4)+eps_d
      !B_sym4(5)=B_sym(5)
      !B_sym4(6)=B_sym(6)
      !B_sym5(1)=B_sym(1)
      !B_sym5(2)=B_sym(2)
      !B_sym5(3)=B_sym(3)
      !B_sym5(4)=B_sym(4)
      !B_sym5(5)=B_sym(5)+eps_d
      !B_sym5(6)=B_sym(6)
      !B_sym6(1)=B_sym(1)
      !B_sym6(2)=B_sym(2)
      !B_sym6(3)=B_sym(3)
      !B_sym6(4)=B_sym(4)
      !B_sym6(5)=B_sym(5)
      !B_sym6(6)=B_sym(6)+eps_d
      
      ! reshape to all B's to 1x6
      
      
      
      print *, "B_sym="
      print *, B_sym(1), B_sym(2), B_sym(3) 
      print *, B_sym(4), B_sym(5), B_sym(6)
      print *, "sig11 model"
      sig11=0.
      sig22=0.
      sig33=0.
      sig12=0.
      sig13=0.
      sig23=0.
      ! testing derivatives
      call derivative(B_sym, model1_w1, model1_w2,
     * model1_w3, model1_w4, model1_w5,
     *  model1_b1, model1_b2, model1_b3, model1_b4,
     * model1_b5, C4row1)
      call derivative(B_sym, model2_w1, model2_w2,
     * model2_w3, model2_w4, model2_w5,
     * model2_b1, model2_b2, model2_b3, model2_b4,
     * model2_b5, C4row2)
      call derivative(B_sym, model3_w1, model3_w2,
     * model3_w3, model3_w4, model3_w5,
     * model3_b1, model3_b2, model3_b3, model3_b4,
     * model3_b5, C4row3)
      call derivative(B_sym, model4_w1, model4_w2,
     * model4_w3, model4_w4, model4_w5,
     * model4_b1, model4_b2, model4_b3, model4_b4,
     * model4_b5, C4row4)
      call derivative(B_sym, model5_w1, model5_w2,
     * model5_w3, model5_w4, model5_w5,
     * model5_b1, model5_b2, model5_b3, model5_b4,
     * model5_b5, C4row5)
      call derivative(B_sym, model6_w1, model6_w2,
     * model6_w3, model6_w4, model6_w5,
     * model6_b1, model6_b2, model6_b3, model6_b4,
     * model6_b5, C4row6)
      
      print *, "C4row1=", C4row1
      print *, "C4row2=", C4row2
      print *, "C4row3=", C4row3
      print *, "C4row4=", C4row4
      print *, "C4row5=", C4row5
      print *, "C4row6=", C4row6
      
      sig11=model2(B_sym, model1_w1, model1_w2, 
     * model1_w3, model1_w4, model1_w5,
     *  model1_b1, model1_b2, model1_b3, model1_b4,
     * model1_b5)
      !print *, "sig22 model"
      !print *, "sig11=", sig11
      !print *, "sig22=", sig22
      !print *, "sig33=", sig33
      !print *, "sig12=", sig12
      !print *, "sig13=", sig13
      !print *, "sig23=", sig23
      sig22=model2(B_sym, model2_w1, model2_w2,
     * model2_w3, model2_w4, model2_w5,
     * model2_b1, model2_b2, model2_b3, model2_b4, model2_b5)
      !print *, "sig33 model"
      !print *, "sig11=", sig11
      !print *, "sig22=", sig22
      !print *, "sig33=", sig33
      !print *, "sig12=", sig12
      !print *, "sig13=", sig13
      !print *, "sig23=", sig23
      sig33=model2(B_sym, model3_w1, model3_w2,
     * model3_w3, model3_w4, model3_w5,
     * model3_b1, model3_b2, model3_b3, model3_b4, model3_b5)
      !print *, "sig12 model"
      !print *, "sig11=", sig11
      !print *, "sig22=", sig22
      !print *, "sig33=", sig33
      !print *, "sig12=", sig12
      !print *, "sig13=", sig13
      !print *, "sig23=", sig23
      sig12=model2(B_sym, model4_w1, model4_w2,
     * model4_w3, model4_w4, model4_w5,
     * model4_b1, model4_b2, model4_b3, model4_b4, model4_b5)
      !print *, "sig13 model"
      !print *, "sig11=", sig11
      !print *, "sig22=", sig22
      !print *, "sig33=", sig33
      !print *, "sig12=", sig12
      !print *, "sig13=", sig13
      !print *, "sig23=", sig23
      sig13=model2(B_sym, model5_w1, model5_w2,
     * model5_w3, model5_w4, model5_w5,
     * model5_b1, model5_b2, model5_b3, model5_b4, model5_b5)
      !print *, "sig23 model"
      !print *, "sig11=", sig11
      !print *, "sig22=", sig22
      !print *, "sig33=", sig33
      !print *, "sig12=", sig12
      !print *, "sig13=", sig13
      !print *, "sig23=", sig23
      sig23=model2(B_sym, model6_w1, model6_w2,
     * model6_w3, model6_w4, model6_w5,
     * model6_b1, model6_b2, model6_b3, model6_b4, model6_b5)
      
      print *, "sig11=", sig11
      print *, "sig22=", sig22
      print *, "sig33=", sig33
      print *, "sig12=", sig12
      print *, "sig13=", sig13
      print *, "sig23=", sig23
      
      
      
    !  print *, "sig11_s model"
    !  sig11_1=model2(B_sym1, model1_w1, model1_w2,
    ! * model1_w3, model1_w4, model1_w5,
    ! *  model1_b1, model1_b2, model1_b3, model1_b4,
    ! * model1_b5)
    !  print *, 'sig11_1=',sig11_1
    !  
    !  sig11_2=model2(B_sym2, model1_w1, model1_w2,
    ! * model1_w3, model1_w4, model1_w5,
    ! * model1_b1, model1_b2, model1_b3, model1_b4,
    ! * model1_b5)
    !  print *, 'sig11_2=',sig11_2
    !  
    !  sig11_3=model2(B_sym3, model1_w1, model1_w2,
    ! * model1_w3, model1_w4, model1_w5,
    ! * model1_b1, model1_b2, model1_b3, model1_b4,
    ! * model1_b5)
    !  print *, 'sig11_3=',sig11_3
    !  
    !  sig11_4=model2(B_sym4, model1_w1, model1_w2,
    ! * model1_w3, model1_w4, model1_w5,
    ! * model1_b1, model1_b2, model1_b3, model1_b4,
    ! * model1_b5)
    !  print *, 'sig11_4=',sig11_4
    !  
    !  sig11_5=model2(B_sym5, model1_w1, model1_w2,
    ! * model1_w3, model1_w4, model1_w5,
    ! * model1_b1, model1_b2, model1_b3, model1_b4,
    ! * model1_b5)
    !  print *, 'sig11_5=',sig11_5
    !  
    !  sig11_6=model2(B_sym6, model1_w1, model1_w2,
    ! * model1_w3, model1_w4, model1_w5,
    ! * model1_b1, model1_b2, model1_b3, model1_b4,
    ! * model1_b5)
    !  print *, 'sig11_6=',sig11_6
    !  ! will write the symmetric part
    !  print *, "sig22_s model"
    !  sig22_2=model2(B_sym2, model2_w1, model2_w2,
    ! * model2_w3, model2_w4, model2_w5,
    ! * model2_b1, model2_b2, model2_b3, model2_b4, model2_b5)
    !  print *, 'sig22_2=',sig22_2
    !  
    !  sig22_3=model2(B_sym3, model2_w1, model2_w2,
    ! * model2_w3, model2_w4, model2_w5,
    ! * model2_b1, model2_b2, model2_b3, model2_b4, model2_b5)
    !  print *, 'sig22_3=',sig22_3
    !  
    !  sig22_4=model2(B_sym4, model2_w1, model2_w2,
    ! * model2_w3, model2_w4, model2_w5,
    ! * model2_b1, model2_b2, model2_b3, model2_b4, model2_b5)
    !  print *, 'sig22_4=',sig22_4
    !  
    ! 
    !  sig22_5=model2(B_sym5, model2_w1, model2_w2,
    ! * model2_w3, model2_w4, model2_w5,
    ! * model2_b1, model2_b2, model2_b3, model2_b4, model2_b5)
    !  print *, 'sig22_5=',sig22_5
    !  
    !  
    !  sig22_6=model2(B_sym6, model2_w1, model2_w2,
    ! * model2_w3, model2_w4, model2_w5,
    ! * model2_b1, model2_b2, model2_b3, model2_b4, model2_b5)
    !  print *, 'sig22_6=',sig22_6
    !  print *, "sig33_s model"
    !  sig33_3=model2(B_sym3, model3_w1, model3_w2,
    ! * model3_w3, model3_w4, model3_w5,
    ! * model3_b1, model3_b2, model3_b3, model3_b4, model3_b5)
    !  print *, 'sig33_3=',sig33_3
    !  
    !  sig33_4=model2(B_sym4, model3_w1, model3_w2,
    ! * model3_w3, model3_w4, model3_w5,
    ! * model3_b1, model3_b2, model3_b3, model3_b4, model3_b5)
    !  print *, 'sig33_4=',sig33_4
    !  print *, "sig33_5 model"
    !  sig33_5=model2(B_sym5, model3_w1, model3_w2,
    ! * model3_w3, model3_w4, model3_w5,
    ! * model3_b1, model3_b2, model3_b3, model3_b4, model3_b5)
    !  print *, 'sig33_5=',sig33_5
    !  
    !  sig33_6=model2(B_sym6, model3_w1, model3_w2,
    ! * model3_w3, model3_w4, model3_w5,
    ! * model3_b1, model3_b2, model3_b3, model3_b4, model3_b5)
    !  print *, 'sig33_6=',sig33_6
    !  print *, "sig12_s model"
    !  sig12_4=model2(B_sym4, model4_w1, model4_w2,
    ! * model4_w3, model4_w4, model4_w5,
    ! * model4_b1, model4_b2, model4_b3, model4_b4, model4_b5)
    !  print *, 'sig12_4=',sig12_4
    !  
    !  sig12_5=model2(B_sym5, model4_w1, model4_w2,
    ! * model4_w3, model4_w4, model4_w5,
    ! * model4_b1, model4_b2, model4_b3, model4_b4, model4_b5)
    !  print *, 'sig12_5=',sig12_5
    !  
    !  sig12_6=model2(B_sym6, model4_w1, model4_w2,
    ! * model4_w3, model4_w4, model4_w5,
    ! * model4_b1, model4_b2, model4_b3, model4_b4, model4_b5)
    !  print *, 'sig12_6=',sig12_6
    !  print *, "sig13s model"
    !  sig13_5=model2(B_sym5, model5_w1, model5_w2,
    ! * model5_w3, model5_w4, model5_w5,
    ! * model5_b1, model5_b2, model5_b3, model5_b4, model5_b5)
    !  print *, 'sig13_5=',sig13_5
    !  
    !  sig13_6=model2(B_sym6, model5_w1, model5_w2,
    ! * model5_w3, model5_w4, model5_w5,
    ! * model5_b1, model5_b2, model5_b3, model5_b4, model5_b5)
    !  print *, 'sig13_6=',sig13_6
    !  print *, "sig23_6 model"
    !  sig23_6=model2(B_sym6, model6_w1, model6_w2,
    ! * model6_w3, model6_w4, model6_w5,
    ! * model6_b1, model6_b2, model6_b3, model6_b4, model6_b5)
    !  print *, 'sig23_6=',sig23_6
      
      STRESS(1)=sig11
      STRESS(2)=sig22
      STRESS(3)=sig33
      STRESS(4)=sig12
      STRESS(5)=sig13
      STRESS(6)=sig23
      
      ! stiffness part
      DDSDDE(1,1) = C4row1(1)
      DDSDDE(1,2) = C4row1(2)
      DDSDDE(1,3) = C4row1(3)
      DDSDDE(1,4) = C4row1(4)
      DDSDDE(1,5) = C4row1(5)
      DDSDDE(1,6) = C4row1(6)
      !DDSDDE(2,1) = C4row2(1)
      DDSDDE(2,2) = C4row2(2)
      DDSDDE(2,3) = C4row2(3)
      DDSDDE(2,4) = C4row2(4)
      DDSDDE(2,5) = C4row2(5)
      DDSDDE(2,6) = C4row2(6)
      !DDSDDE(3,1) = C4row3(1)
      !DDSDDE(3,2) = C4row3(2)
      DDSDDE(3,3) = C4row3(3)
      DDSDDE(3,4) = C4row3(4)
      DDSDDE(3,5) = C4row3(5)
      DDSDDE(3,6) = C4row3(6)
      !DDSDDE(4,1) = C4row4(1)
      !DDSDDE(4,2) = C4row4(2)
      !DDSDDE(4,3) = C4row4(3)
      DDSDDE(4,4) = C4row4(4)
      DDSDDE(4,5) = C4row4(5)
      DDSDDE(4,6) = C4row4(6)
      !DDSDDE(5,1) = C4row5(1)
      !DDSDDE(5,2) = C4row5(2)
      !DDSDDE(5,3) = C4row5(3)
      !DDSDDE(5,4) = C4row5(4)
      DDSDDE(5,5) = C4row5(5)
      DDSDDE(5,6) = C4row5(6)
      !DDSDDE(6,1) = C4row6(1)
      !DDSDDE(6,2) = C4row6(2)
      !DDSDDE(6,3) = C4row6(3)
      !DDSDDE(6,4) = C4row6(4)
      !DDSDDE(6,5) = C4row6(5)
      DDSDDE(6,6) = C4row6(6)
      
      
      !DDSDDE(1,1)= (sig11_1-sig11)/eps_d
      !DDSDDE(1,2)= (sig11_2-sig11)/eps_d
      !DDSDDE(1,3)= (sig11_3-sig11)/eps_d
      !!DDSDDE(1,4)=0.
      !!DDSDDE(1,5)=0.
      !!DDSDDE(1,6)=0.
      !DDSDDE(1,4)= (sig11_4-sig11)/eps_d
      !DDSDDE(1,5)= (sig11_5-sig11)/eps_d
      !DDSDDE(1,6)= (sig11_6-sig11)/eps_d
      !
      !DDSDDE(2,2)= (sig22_2-sig22)/eps_d
      !DDSDDE(2,3)= (sig22_3-sig22)/eps_d
      !DDSDDE(2,4)= (sig22_4-sig22)/eps_d
      !DDSDDE(2,5)= (sig22_5-sig22)/eps_d
      !DDSDDE(2,6)= (sig22_6-sig22)/eps_d
      !!DDSDDE(2,4)=0.
      !!DDSDDE(2,5)=0.
      !!DDSDDE(2,6)=0.
      !
      !DDSDDE(3,3)= (sig33_3-sig33)/eps_d
      !DDSDDE(3,4)= (sig33_4-sig33)/eps_d
      !DDSDDE(3,5)= (sig33_5-sig33)/eps_d
      !DDSDDE(3,6)= (sig33_6-sig33)/eps_d
      !!DDSDDE(3,4)=0.
      !!DDSDDE(3,5)=0.
      !!DDSDDE(3,6)=0.
      !DDSDDE(4,4)= (sig12_4-sig12)/eps_d
      !DDSDDE(4,5)= (sig12_5-sig12)/eps_d
      !!DDSDDE(4,5)= 0.
      !DDSDDE(4,6)= (sig12_6-sig12)/eps_d
      !!DDSDDE(4,6)= 0.
      !DDSDDE(5,5)= (sig13_5-sig13)/eps_d
      !DDSDDE(5,6)= (sig13_6-sig13)/eps_d
      !!DDSDDE(5,6)= 0.
      !
      !DDSDDE(6,6)= (sig23_6-sig23)/eps_d
      
      
      
      
      DO K1=1, NTENS
        DO K2=1, K1-1
          DDSDDE(K1, K2)=DDSDDE(K2, K1)
        END DO
      END DO

      
      DO K1=1,NTENS
            print *,"STRESS(",K1,")=", STRESS(K1)
      END DO
      !DO K1=1, NTENS
      !  DO K2=1, NTENS
      !    print *,"DDSDDE(",K1,",", K2,")=", DDSDDE(K1, K2)
      !  END DO
      !END DO
      
      
      
      

C
      RETURN
      END
      
      function model1(X,w1,w2,w3,w4,w5,w6,w7,
     *            b1,b2,b3,b4,b5,b6,b7) 
        !implicit none
        integer, parameter::input_size = 6
        integer, parameter::output_size = 1
        integer, parameter::hidden_layers = 5
        integer, parameter::hidden_size = 9
        
        real(kind=8), dimension(input_size), intent(in) :: X
        real(kind=8), dimension(hidden_size,input_size), intent(in) :: w1
        real(kind=8), dimension(hidden_size,hidden_size), intent(in) :: w2,w3,
     *  w4,w5,w6
        real(kind=8), dimension(output_size,hidden_size), intent(in) :: w7
        real(kind=8), dimension(hidden_size), intent(in) :: b1,b2,b3,b4,b5,b6
        real(kind=8), dimension(output_size), intent(in) :: b7
        integer :: input_size_layer
        real(kind=8), dimension(9) :: h1, h2,h3,h4,h5,h6
        real(kind=8), dimension(1) :: h7
        real(kind=8) :: model1
        
        
        input_size_layer = input_size
        !print *, "model1 h1: shape(X)=", shape(X)
        !print *, "model1 h1: shape(w1)  =", shape(w1)
        !print *, "model1 h1: shape(b1) =", shape(b1)
        
        
        !print *, "before forward shape(X)=",shape(X)
        call forward(X, w1, b1,h1,input_size_layer,hidden_size)
        !print *, "model1 h1: shape(h1) =", shape(h1)
        !print *, "model1 h1:",h1
        call relu(h1,hidden_size)
        input_size_layer = size(h1)
        call forward(h1, w2, b2,h2,input_size_layer,hidden_size)
        call relu(h2,hidden_size)
        !print *, "model1 h2:",h2
        call forward(h2, w3, b3,h3,input_size_layer,hidden_size)
        call relu(h3,hidden_size)
        !print *, "model1 h3:",h3
        call forward(h3, w4, b4,h4,input_size_layer,hidden_size)
        call relu(h4,hidden_size)
        !print *, "model1 h4:",h4
        call forward(h4, w5, b5,h5,input_size_layer,hidden_size)
        call relu(h5,hidden_size)
        !print *, "model1 h5:",h5
        
        call forward(h5, w6, b6,h6,input_size_layer,hidden_size)
        call relu(h6,hidden_size)
        !print *, "model1 h6:",h6
        input_size_layer = size(h6)
        call forward(h6, w7, b7,h7,input_size_layer,output_size)
        !print *, "model1 y:",h7(1)
        model1 = h7(1)
        
      end function model1
      
            
      !similarly create function for model2
      function model2(X,w1,w2,w3,w4,w5,b1,b2,b3,b4,b5) 
        !implicit none
        integer, parameter::input_size = 6
        integer, parameter::output_size = 1
        integer, parameter::hidden_layers = 3
        integer, parameter::hidden_size = 9

        real(kind=8), dimension(input_size), intent(in) :: X
        real(kind=8), 
     *         dimension(hidden_size,input_size), intent(in) :: w1
        real(kind=8), 
     *         dimension(hidden_size,hidden_size), intent(in) :: w2,w3,
     *    w4
        real(kind=8), 
     *         dimension(output_size,hidden_size), intent(in) :: w5
        real(kind=8), dimension(hidden_size), intent(in) :: b1,b2,b3,b4
        real(kind=8), dimension(output_size), intent(in) :: b5
        integer :: input_size_layer
        real(kind=8), dimension(9) :: h1,h2,h3,h4
        real(kind=8), dimension(1) :: h5
        real(kind=8)::model2
        !print *, "model2: X=",X
        input_size_layer = input_size
        !print *, "model2 h1: shape(X)=", shape(X)
        !print *, "model2 h1: shape(w1)  =", shape(w1)
        !print *, "model2 h1: shape(b1) =", shape(b1)
        call forward(X, w1, b1,h1,input_size_layer,hidden_size)
        call relu(h1,hidden_size)

        input_size_layer = size(h1)
        call forward(h1, w2, b2,h2,input_size_layer,hidden_size)
        call relu(h2,hidden_size)
        !print *, "model2 h2:",h2
        call forward(h2, w3, b3,h3,input_size_layer,hidden_size)
        call relu(h3,hidden_size)
        !print *, "model2 h3:",h3
        call forward(h3, w4, b4,h4,input_size_layer,hidden_size)
        call relu(h4,hidden_size)
        !print *, "model2 h4:",h4
        input_size_layer = size(h4)
        call forward(h4, w5, b5,h5,input_size_layer,output_size)
        !print *, "model2 y:",h5(1)
        model2 = h5(1)
        
        end function model2
        
        
      subroutine forward(input, weight, bias, output,
     *      input_size,output_size)
        implicit none
        integer,intent(in):: input_size
        integer,intent(in):: output_size
        
      
        real(kind=8), dimension(input_size), intent(in) :: input
        real(kind=8), dimension(output_size,input_size), 
     *     intent(in) :: weight
      
        real(kind=8), dimension(output_size), intent(in) :: bias
        real(kind=8), dimension(output_size), intent(out) :: output
        integer :: i,j,k
        output = 0d0
        DO j=1, output_size
            DO k=1, input_size
                  output(j) = output(j) 
     *                     + input(k)*weight(j,k)
            end do
            output(j) = output(j) 
     *                     + bias(j)
        end do
        
        
        
        
        ! Perform matrix multiplication and add bias
        !print *, "input=", input
        
        !output = matmul(input,(transpose(weight))) + bias
      end subroutine forward
      
      subroutine relu(x,input_size)
        integer,intent(in):: input_size
        real(kind=8), dimension(input_size), intent(inout) :: x
        !print *, "shape(x)=", size(x)
        ! Apply ReLU activation element-wise
        do i = 1, input_size
          if (x(i) < 0.0) then
            x(i) = 0.0
          end if
        end do
      end subroutine relu
      
      subroutine relu2(x,y,input_size)
        integer,intent(in):: input_size
        real(kind=8), dimension(input_size), intent(in) :: x
        real(kind=8), dimension(input_size), intent(out) :: y
        !print *, "shape(x)=", size(x)
        ! Apply ReLU activation element-wise
        do i = 1, input_size
          if (x(i) < 0.0) then
            y(i) = 0.0
          else
            y(i) = x(i)
          end if
          
        end do
      end subroutine relu2
      
      ! subroutine of linearMultiply
      subroutine linearMultiply2(x,y,input_size,r)
        integer,intent(in):: input_size
        real(kind=8), dimension(1,input_size), intent(in) :: x,y
        real(kind=8),dimension(1,input_size),intent(out) :: r
        !print *, "shape(x)=", shape(x)
        !print *, "shape(y)=", shape(y)
        r = 0.0
        do i = 1, input_size
          !print *, "x(1,",i,")=",x(1,i)
          !print *, "y(1,",i,")=",y(1,i)
          r(1,i) = r(1,i)+ x(1,i)*y(1,i)
          !print *, "r(1,",i,")=", r(1,i)
        end do
        !print *, "linearMultiply=", r
      end subroutine linearMultiply2
      
          ! subroutine for matrix multiply
      subroutine matrixMultiply2(x,y,shape_x,shape_y,r)
        integer,dimension(2),intent(in):: shape_x,shape_y
        real(kind=8), dimension(shape_x(1),shape_x(2)), intent(in) :: x
        real(kind=8), dimension(shape_y(1),shape_y(2)), intent(in) :: y
        real(kind=8),dimension(shape_x(1),shape_y(2)),intent(out) :: r
        integer :: i,j,k
        DO i=1,shape_x(1)
          DO j=1,shape_y(2)
            r(i,j)=0.0
            DO k=1,shape_x(2)
              r(i,j)=r(i,j)+x(i,k)*y(k,j)
            end do
          end do
        end do
      end subroutine matrixMultiply2  
      
      
      
      subroutine derivative(x,w1,w2,w3,w4,w5,b1,b2,b3,b4,b5,y)
        integer, parameter::input_size = 6
        integer, parameter::output_size = 1
        integer, parameter::hidden_layers = 3
        integer, parameter::hidden_size = 9
        real(kind=8), dimension(input_size), intent(in) :: x
        real(kind=8), 
     *   dimension(hidden_size,input_size), intent(in) :: w1
        real(kind=8), 
     *    dimension(hidden_size,hidden_size), intent(in) :: w2,w3,w4
        real(kind=8), dimension(output_size,hidden_size),intent(in):: w5
        real(kind=8), dimension(hidden_size), intent(in) :: b1,b2,b3,b4
        real(kind=8), dimension(output_size), intent(in) :: b5
        real(kind=8), dimension(input_size), intent(out) :: y
        real(kind=8), dimension(hidden_size) :: y1,y2,y3,y4
        real(kind=8), 
     *   dimension(hidden_size):: y1Hat,y2Hat,y3Hat,y4Hat
        real(kind=8), dimension(1,hidden_size) :: derv_y4Hat_y4
        real(kind=8), dimension(1,hidden_size) :: derv_y5_y4Hat
        real(kind=8), dimension(hidden_size,hidden_size):: derv_y4_y3Hat
        real(kind=8), dimension(1,hidden_size) :: derv_y3Hat_y3
        real(kind=8), dimension(hidden_size,hidden_size):: derv_y3_y2Hat
        real(kind=8), dimension(1,hidden_size) :: derv_y2Hat_y2
        real(kind=8), dimension(hidden_size,hidden_size):: derv_y2_y1Hat
        real(kind=8), dimension(1,hidden_size) :: derv_y1Hat_y1
        real(kind=8), dimension(hidden_size,input_size):: derv_y1_x
        real(kind=8) :: linearMultiply
        real(kind=8),dimension(output_size):: matrixMultiply
        integer,dimension(2):: temp
        real(kind=8),
     *   dimension(1,hidden_size):: derv1,derv2,derv3,derv4,derv5,
     *       derv6,derv7,derv8,derv9,derv10
        real(kind=8),dimension(1,input_size):: derv
        integer:: input_size_layer
        
        !print *, "shape(x)=",shape(x)
        !print *, "shape(w1)=",shape(w1)
        !print *, "shape(b1)=",shape(b1)
        !print *, "shape(y1)=",shape(y1)
        call forward(x,w1,b1,y1,input_size,hidden_size)
        !print *, "y1=",y1
        call relu2(y1,y1Hat,hidden_size)
        !print *, "y1Hat=",y1Hat
        input_size_layer = size(y1Hat)
        call forward(y1Hat,w2,b2,y2,input_size_layer,hidden_size)
        !print *, "y2=",y2
        call relu2(y2,y2Hat,hidden_size)
        !print *, "y2Hat=",y2Hat
        call forward(y2Hat,w3,b3,y3,input_size_layer,hidden_size)
        !print *, "y3=",y3
        call relu2(y3,y3Hat,hidden_size)
        !print *, "y3Hat=",y3Hat
        call forward(y3Hat,w4,b4,y4,input_size_layer,hidden_size)
        !print *, "y4=",y4
        call relu2(y4,y4Hat,hidden_size)
        !print *, "y4Hat=",y4Hat
        
        
        derv_y5_y4Hat = w5
        !print *, "shape(derv_y5_y4Hat)=",shape(derv_y5_y4Hat)
        temp = shape(w5)
        call relu_derivative2(y4,derv_y4Hat_y4,temp(1),temp(2))
        !print *, "shape(derv_y4Hat_y4)=",shape(derv_y4Hat_y4)
        !print *, "derv_y4Hat_y4=",derv_y4Hat_y4
        derv_y4_y3Hat = w4
        !print *, "shape(derv_y4_y3Hat)=",shape(derv_y4_y3Hat)
        temp = shape(w4)
        call relu_derivative2(y3,derv_y3Hat_y3,temp(1),temp(2))
        !print *, "shape(derv_y3Hat_y3)=",shape(derv_y3Hat_y3)
        derv_y3_y2Hat = w3
        temp = shape(w3)
        !print *, "shape(derv_y3_y2Hat)=",shape(derv_y3_y2Hat)
        call relu_derivative2(y2,derv_y2Hat_y2,temp(1),temp(2))
        !print *, "shape(derv_y2Hat_y2)=",shape(derv_y2Hat_y2)
        derv_y2_y1Hat = w2
        temp = shape(w2)
        !print *, "shape(derv_y2_y1Hat)=",shape(derv_y2_y1Hat)
        call relu_derivative2(y1,derv_y1Hat_y1,temp(1),temp(2))
        !print *, "shape(derv_y1Hat_y1)=",shape(derv_y1Hat_y1)
        derv_y1_x = w1
        call linearMultiply2(derv_y5_y4Hat,derv_y4Hat_y4,9,derv1)
        !print *, "derv.shape=",shape(derv1)
        !print *, "derv1",derv1
        !derv = linearMultiply(derv_y5_y4Hat,derv_y4Hat_y4,hidden_size)
        call MatrixMultiply2(derv1,
     *    derv_y4_y3Hat,shape(derv1),shape(derv_y4_y3Hat),derv2)
        !derv2 = matmul(derv1,derv_y4_y3Hat)
        !print *, "derv.shape=",shape(derv2)
        !print *, "derv1",derv2
        !derv1 =matrixMultipl!y(derv,derv_y4_y3Hat,hidden_size,output_size)
        call linearMultiply2(derv2,derv_y3Hat_y3,9,derv3)
        !print *, "derv1",derv3
        call MatrixMultiply2(derv3,
     *    derv_y3_y2Hat,shape(derv3),shape(derv_y3_y2Hat),derv4)
        !derv3 = matmul(derv3,derv_y3_y2Hat)
        !print *, "derv4.shape=",shape(derv4)
        !print *, "derv4",derv4
        !derv3 =matrixMultiply(derv2,derv_y3_y2hat,hidden_size,output_size)
        call linearMultiply2(derv4,derv_y2Hat_y2,9,derv5)
        !print *, "derv5=",derv5
        call MatrixMultiply2(derv5,
     *    derv_y2_y1Hat, shape(derv5),shape(derv_y2_y1Hat),derv6) 
        !derv4 = matmul(derv4,derv_y2_y1Hat)
        !print *, "derv6=",derv6
        call linearMultiply2(derv6,derv_y1Hat_y1,9,derv7)
        !print *, "derv7=",derv7
        call MatrixMultiply2(derv7,
     *    derv_y1_x, shape(derv7),shape(derv_y1_x),derv)
        !derv = matmul(derv5,derv_y1_x)
        !print *, "derv=",derv
       
        ! derv4 = linearMultiply(derv3,derv_y2Hat_y2,hidden_size)
        !derv5 = matmul(derv4,derv_y2_y1Hat)
       ! print *, "derv5.shape=",shape(derv5)
        !derv5 =matrixMultiply(derv4,derv_y2_y1Hat,hidden_size,output_size)
        !derv6 = linearMultiply(derv5,derv_y1Hat_y1,hidden_size)
       ! derv7 = matmul(derv6,derv_y1_x)
        !print *, "derv7.shape=",shape(derv7)
        !derv7 =matrixMultiply(derv6,derv_y1_x,hidden_size,input_size)
        y = derv(1,:)
        
        
        
        
      end subroutine derivative
      
      subroutine relu_derivative(x, y,length)
          integer, intent(in) :: length
          real, intent(in) :: x(length)
          real, intent(out) :: y(length)
          integer :: i
          do i = 1, length
              if (x(i) > 0) then
                  y(i) = 1.0
              else
                  y(i) = 0.0
              end if
          end do
      end subroutine relu_derivative
      
      subroutine relu_derivative2(x, y,len_i,len_j)
          implicit none
          integer, intent(in) :: len_i, len_j
          real(kind=8),dimension(len_i,len_j), intent(in) :: x 
          real(kind=8),dimension(len_i,len_j), intent(out) :: y
          integer :: i, j
          y= 0.0
          !print *, "Relu Derv: shape(x)=",shape(x)
          do i = 1, len_i
              do j = 1, len_j
                  !print *,"x(",i,",",j,")=",x(i,j)  
                  if (x(i,j) > 0) then
                      y(i,j) = 1.0
                  else
                      y(i,j) = 0.0
                  end if
              end do
          end do
      end subroutine relu_derivative2