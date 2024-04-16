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
      integer, parameter::model1_hidden_layers = 5
      integer, parameter::model1_hidden_size = 9
      real, dimension(model1_hidden_size,model1_input_size)::model1_w1 ! weight matrix from input layer to first hidden layer
      real, dimension(1,model1_hidden_size)::model1_b1 ! bias vector for first hidden layer
      real, dimension(model1_hidden_size, model1_hidden_size)::model1_w2 ! weight matrix from hidden layer 1 to hidden layer 2
      real, dimension(1,model1_hidden_size)::model1_b2 ! bias vector for hidden layer 2
      real, dimension(model1_hidden_size, model1_hidden_size)::model1_w3 ! weight matrix from hidden layer 2 to hidden layer 3
      real, dimension(1,model1_hidden_size)::model1_b3 ! bias vector for hidden layer 3
      real, dimension(model1_hidden_size, model1_hidden_size)::model1_w4 ! weight matrix from hidden layer 3 to hidden layer 4
      real, dimension(1,model1_hidden_size)::model1_b4 ! bias vector for hidden layer 4
      real, dimension(model1_hidden_size, model1_hidden_size)::model1_w5 ! weight matrix from hidden layer 4 to hidden layer 5
      real, dimension(1,model1_hidden_size)::model1_b5 ! bias vector for hidden layer 5
      real, dimension(model1_hidden_size, model1_hidden_size)::model1_w6 ! weight matrix from hidden layer 5 to hidden layer 6
      real, dimension(1,model1_hidden_size)::model1_b6 ! bias vector for hidden layer 6
      real, dimension(model1_output_size,model1_hidden_size)::model1_w7 ! weight matrix from last hidden layer to output layer
      real, dimension(1,model1_output_size)::model1_b7 ! bias vector for output layer
      
      integer, parameter::model2_input_size = 6
      integer, parameter::model2_output_size = 1
      integer, parameter::model2_hidden_layers = 3
      integer, parameter::model2_hidden_size = 9
      real, dimension(model2_hidden_size,model2_input_size)::model2_w1 ! weight matrix from input layer to first hidden layer
      real, dimension(1,model2_hidden_size)::model2_b1 ! bias vector for first hidden layer
      real, dimension(model2_hidden_size, model2_hidden_size)::model2_w2 ! weight matrix from hidden layer 1 to hidden layer 2
      real, dimension(1,model2_hidden_size)::model2_b2 ! bias vector for hidden layer 2
      real, dimension(model2_hidden_size, model2_hidden_size)::model2_w3 ! weight matrix from hidden layer 2 to hidden layer 3
      real, dimension(1,model2_hidden_size)::model2_b3 ! bias vector for hidden layer 3
      real, dimension(model2_hidden_size, model2_hidden_size)::model2_w4 ! weight matrix from hidden layer 3 to hidden layer 4
      real, dimension(1,model2_hidden_size)::model2_b4 ! bias vector for hidden layer 4
      real, dimension(model2_output_size,model2_hidden_size)::model2_w5 ! weight matrix from last hidden layer to output layer
      real, dimension(1,model2_output_size)::model2_b5 ! bias vector for output layer
      
      integer, parameter::model3_input_size = 6
      integer, parameter::model3_output_size = 1
      integer, parameter::model3_hidden_layers = 5
      integer, parameter::model3_hidden_size = 9
      real, dimension(model3_hidden_size,model3_input_size)::model3_w1 ! weight matrix from input layer to first hidden layer
      real, dimension(1,model3_hidden_size)::model3_b1 ! bias vector for first hidden layer
      real, dimension(model3_hidden_size, model3_hidden_size)::model3_w2 ! weight matrix from hidden layer 1 to hidden layer 2
      real, dimension(1,model3_hidden_size)::model3_b2 ! bias vector for hidden layer 2
      real, dimension(model3_hidden_size, model3_hidden_size)::model3_w3 ! weight matrix from hidden layer 2 to hidden layer 3
      real, dimension(1,model3_hidden_size)::model3_b3 ! bias vector for hidden layer 3
      real, dimension(model3_hidden_size, model3_hidden_size)::model3_w4 ! weight matrix from hidden layer 3 to hidden layer 4
      real, dimension(1,model3_hidden_size)::model3_b4 ! bias vector for hidden layer 4
      real, dimension(model3_hidden_size, model3_hidden_size)::model3_w5 ! weight matrix from hidden layer 4 to hidden layer 5
      real, dimension(1,model3_hidden_size)::model3_b5 ! bias vector for hidden layer 5
      real, dimension(model3_hidden_size, model3_hidden_size)::model3_w6 ! weight matrix from hidden layer 5 to hidden layer 6
      real, dimension(1,model3_hidden_size)::model3_b6 ! bias vector for hidden layer 6
      real, dimension(model3_output_size,model3_hidden_size)::model3_w7 ! weight matrix from last hidden layer to output layer
      real, dimension(1,model3_output_size)::model3_b7 ! bias vector for output layer
      
      integer, parameter::model4_input_size = 6
      integer, parameter::model4_output_size = 1
      integer, parameter::model4_hidden_layers = 5
      integer, parameter::model4_hidden_size = 9
      real, dimension(model4_hidden_size,model4_input_size)::model4_w1 ! weight matrix from input layer to first hidden layer
      real, dimension(1,model4_hidden_size)::model4_b1 ! bias vector for first hidden layer
      real, dimension(model4_hidden_size, model4_hidden_size)::model4_w2 ! weight matrix from hidden layer 1 to hidden layer 2
      real, dimension(1,model4_hidden_size)::model4_b2 ! bias vector for hidden layer 2
      real, dimension(model4_hidden_size, model4_hidden_size)::model4_w3 ! weight matrix from hidden layer 2 to hidden layer 3
      real, dimension(1,model4_hidden_size)::model4_b3 ! bias vector for hidden layer 3
      real, dimension(model4_hidden_size, model4_hidden_size)::model4_w4 ! weight matrix from hidden layer 3 to hidden layer 4
      real, dimension(1,model4_hidden_size)::model4_b4 ! bias vector for hidden layer 4
      real, dimension(model4_hidden_size, model4_hidden_size)::model4_w5 ! weight matrix from hidden layer 4 to hidden layer 5
      real, dimension(1,model4_hidden_size)::model4_b5 ! bias vector for hidden layer 5
      real, dimension(model4_hidden_size, model4_hidden_size)::model4_w6 ! weight matrix from hidden layer 5 to hidden layer 6
      real, dimension(1,model4_hidden_size)::model4_b6 ! bias vector for hidden layer 6
      real, dimension(model4_output_size,model4_hidden_size)::model4_w7 ! weight matrix from last hidden layer to output layer
      real, dimension(1,model4_output_size)::model4_b7 ! bias vector for output layer
      
      integer, parameter::model5_input_size = 6
      integer, parameter::model5_output_size = 1
      integer, parameter::model5_hidden_layers = 5
      integer, parameter::model5_hidden_size = 9
      real, dimension(model5_hidden_size,model5_input_size)::model5_w1 ! weight matrix from input layer to first hidden layer
      real, dimension(1,model5_hidden_size)::model5_b1 ! bias vector for first hidden layer
      real, dimension(model5_hidden_size, model5_hidden_size)::model5_w2 ! weight matrix from hidden layer 1 to hidden layer 2
      real, dimension(1,model5_hidden_size)::model5_b2 ! bias vector for hidden layer 2
      real, dimension(model5_hidden_size, model5_hidden_size)::model5_w3 ! weight matrix from hidden layer 2 to hidden layer 3
      real, dimension(1,model5_hidden_size)::model5_b3 ! bias vector for hidden layer 3
      real, dimension(model5_hidden_size, model5_hidden_size)::model5_w4 ! weight matrix from hidden layer 3 to hidden layer 4
      real, dimension(1,model5_hidden_size)::model5_b4 ! bias vector for hidden layer 4
      real, dimension(model5_hidden_size, model5_hidden_size)::model5_w5 ! weight matrix from hidden layer 4 to hidden layer 5
      real, dimension(1,model5_hidden_size)::model5_b5 ! bias vector for hidden layer 5
      real, dimension(model5_hidden_size, model5_hidden_size)::model5_w6 ! weight matrix from hidden layer 5 to hidden layer 6
      real, dimension(1,model5_hidden_size)::model5_b6 ! bias vector for hidden layer 6
      real, dimension(model5_output_size,model5_hidden_size)::model5_w7 ! weight matrix from last hidden layer to output layer
      real, dimension(1,model5_output_size)::model5_b7 ! bias vector for output layer
      
      integer, parameter::model6_input_size = 6
      integer, parameter::model6_output_size = 1
      integer, parameter::model6_hidden_layers = 5
      integer, parameter::model6_hidden_size = 9
      real, dimension(model6_hidden_size,model6_input_size)::model6_w1 ! weight matrix from input layer to first hidden layer
      real, dimension(1,model6_hidden_size)::model6_b1 ! bias vector for first hidden layer
      real, dimension(model6_hidden_size, model6_hidden_size)::model6_w2 ! weight matrix from hidden layer 1 to hidden layer 2
      real, dimension(1,model6_hidden_size)::model6_b2 ! bias vector for hidden layer 2
      real, dimension(model6_hidden_size, model6_hidden_size)::model6_w3 ! weight matrix from hidden layer 2 to hidden layer 3
      real, dimension(1,model6_hidden_size)::model6_b3 ! bias vector for hidden layer 3
      real, dimension(model6_hidden_size, model6_hidden_size)::model6_w4 ! weight matrix from hidden layer 3 to hidden layer 4
      real, dimension(1,model6_hidden_size)::model6_b4 ! bias vector for hidden layer 4
      real, dimension(model6_hidden_size, model6_hidden_size)::model6_w5 ! weight matrix from hidden layer 4 to hidden layer 5
      real, dimension(1,model6_hidden_size)::model6_b5 ! bias vector for hidden layer 5
      real, dimension(model6_hidden_size, model6_hidden_size)::model6_w6 ! weight matrix from hidden layer 5 to hidden layer 6
      real, dimension(1,model6_hidden_size)::model6_b6 ! bias vector for hidden layer 6
      real, dimension(model6_output_size,model6_hidden_size)::model6_w7 ! weight matrix from last hidden layer to output layer
      real, dimension(1,model6_output_size)::model6_b7 ! bias vector for output layer
      
      real, DIMENSION(1,6)::B_sym
C
      PARAMETER(ZERO=0.D0, ONE=1.D0, TWO=2.D0, THREE=3.D0, FOUR=4.D0)
      
      real, PARAMETER::eps_d=1.e-5
      real, DIMENSION(1,6)::B_sym1, B_sym2, 
     *       B_sym3, B_sym4, B_sym5, B_sym6
      
      
      real, dimension(1,model1_output_size)::sig11, 
     *      sig22, sig33, sig12, sig13, sig23
      
      real, dimension(1,model1_output_size)::sig11_1, sig11_2, sig11_3,
     * sig11_4, sig11_5, sig11_6
      real, dimension(1,model2_output_size)::sig22_1, sig22_2, sig22_3,
     * sig22_4, sig22_5, sig22_6
      real, dimension(1,model3_output_size)::sig33_1, sig33_2, sig33_3,
     * sig33_4, sig33_5, sig33_6
      real, dimension(1,model4_output_size)::sig12_1, sig12_2, sig12_3,
     * sig12_4, sig12_5, sig12_6
      real, dimension(1,model5_output_size)::sig13_1, sig13_2, sig13_3,
     * sig13_4, sig13_5, sig13_6
      real, dimension(1,model6_output_size)::sig23_1, sig23_2, sig23_3,
     * sig23_4, sig23_5, sig23_6
      
      
      ! model1_input_layer.weight
      model1_w1(1,1) = 0.5708730220794678d0
      model1_w1(1,2) = 0.296874076128006d0
      model1_w1(1,3) = -0.19265501201152802d0
      model1_w1(1,4) = 0.3717806339263916d0
      model1_w1(1,5) = -0.24020107090473175d0
      model1_w1(1,6) = -1.212396264076233d0
      model1_w1(2,1) = -0.38724422454833984d0
      model1_w1(2,2) = 0.1596437692642212d0
      model1_w1(2,3) = -0.09333497285842896d0
      model1_w1(2,4) = -0.28220134973526d0
      model1_w1(2,5) = 0.1388879418373108d0
      model1_w1(2,6) = 0.05507686734199524d0
      model1_w1(3,1) = 0.7364110946655273d0
      model1_w1(3,2) = 0.2059098780155182d0
      model1_w1(3,3) = -0.5606481432914734d0
      model1_w1(3,4) = -0.18612705171108246d0
      model1_w1(3,5) = 0.09955371916294098d0
      model1_w1(3,6) = 0.7216070294380188d0
      model1_w1(4,1) = 0.5513116717338562d0
      model1_w1(4,2) = 0.22689326107501984d0
      model1_w1(4,3) = -0.11430196464061737d0
      model1_w1(4,4) = 0.23479819297790527d0
      model1_w1(4,5) = 0.4642467498779297d0
      model1_w1(4,6) = -0.7506508827209473d0
      model1_w1(5,1) = 0.8272923827171326d0
      model1_w1(5,2) = -0.4343664050102234d0
      model1_w1(5,3) = -0.06561248004436493d0
      model1_w1(5,4) = -0.27260011434555054d0
      model1_w1(5,5) = 0.04172897711396217d0
      model1_w1(5,6) = -1.282576322555542d0
      model1_w1(6,1) = 0.13022583723068237d0
      model1_w1(6,2) = -0.33830660581588745d0
      model1_w1(6,3) = -0.3897096514701843d0
      model1_w1(6,4) = 0.26411354541778564d0
      model1_w1(6,5) = 0.2625427842140198d0
      model1_w1(6,6) = -0.11380118131637573d0
      model1_w1(7,1) = -0.2926369905471802d0
      model1_w1(7,2) = 1.1798880100250244d0
      model1_w1(7,3) = 0.15684014558792114d0
      model1_w1(7,4) = 0.1679871678352356d0
      model1_w1(7,5) = 0.49330106377601624d0
      model1_w1(7,6) = -0.07796115428209305d0
      model1_w1(8,1) = 0.4715004861354828d0
      model1_w1(8,2) = 0.37506696581840515d0
      model1_w1(8,3) = -0.2691367268562317d0
      model1_w1(8,4) = -0.20971010625362396d0
      model1_w1(8,5) = 0.010099153034389019d0
      model1_w1(8,6) = -0.6326584815979004d0
      model1_w1(9,1) = 0.8419840931892395d0
      model1_w1(9,2) = -0.0690028965473175d0
      model1_w1(9,3) = -0.4881681799888611d0
      model1_w1(9,4) = 0.18016569316387177d0
      model1_w1(9,5) = 0.02925524115562439d0
      model1_w1(9,6) = -0.22504061460494995d0
! model1_input_layer.bias
      model1_b1(1,1) = 0.47549232840538025d0
      model1_b1(1,2) = -0.32454535365104675d0
      model1_b1(1,3) = 0.4639122188091278d0
      model1_b1(1,4) = 0.11366737633943558d0
      model1_b1(1,5) = 0.36114630103111267d0
      model1_b1(1,6) = -0.27486681938171387d0
      model1_b1(1,7) = 0.33075231313705444d0
      model1_b1(1,8) = 0.867797315120697d0
      model1_b1(1,9) = 0.16024775803089142d0
! model1_hidden_layers.0.weight
      model1_w2(1,1) = 0.4333462715148926d0
      model1_w2(1,2) = -0.14029249548912048d0
      model1_w2(1,3) = -0.8732380270957947d0
      model1_w2(1,4) = 0.6557230949401855d0
      model1_w2(1,5) = 0.14834371209144592d0
      model1_w2(1,6) = -0.2019951045513153d0
      model1_w2(1,7) = 0.4257764220237732d0
      model1_w2(1,8) = 0.24024131894111633d0
      model1_w2(1,9) = -0.7021380066871643d0
      model1_w2(2,1) = 0.6087055206298828d0
      model1_w2(2,2) = 0.2604045569896698d0
      model1_w2(2,3) = 0.30445733666419983d0
      model1_w2(2,4) = 0.5114172697067261d0
      model1_w2(2,5) = 0.6429568529129028d0
      model1_w2(2,6) = -0.23593971133232117d0
      model1_w2(2,7) = -1.1447222232818604d0
      model1_w2(2,8) = 0.1020699217915535d0
      model1_w2(2,9) = 0.5317041277885437d0
      model1_w2(3,1) = 0.7339848875999451d0
      model1_w2(3,2) = -0.08818301558494568d0
      model1_w2(3,3) = 0.8938527703285217d0
      model1_w2(3,4) = 0.5292338728904724d0
      model1_w2(3,5) = 0.4994967579841614d0
      model1_w2(3,6) = 0.3147808611392975d0
      model1_w2(3,7) = -0.37399953603744507d0
      model1_w2(3,8) = 0.9418402910232544d0
      model1_w2(3,9) = 0.34586429595947266d0
      model1_w2(4,1) = 0.6765978336334229d0
      model1_w2(4,2) = 0.022312194108963013d0
      model1_w2(4,3) = 0.7282277345657349d0
      model1_w2(4,4) = 0.40560299158096313d0
      model1_w2(4,5) = 0.5906367301940918d0
      model1_w2(4,6) = -0.1291676014661789d0
      model1_w2(4,7) = -1.165307641029358d0
      model1_w2(4,8) = 0.6438212394714355d0
      model1_w2(4,9) = 0.6979356408119202d0
      model1_w2(5,1) = 0.019669851288199425d0
      model1_w2(5,2) = 0.014763206243515015d0
      model1_w2(5,3) = -0.2928885221481323d0
      model1_w2(5,4) = 0.2591356635093689d0
      model1_w2(5,5) = -0.2866898477077484d0
      model1_w2(5,6) = -0.18616001307964325d0
      model1_w2(5,7) = -0.22547146677970886d0
      model1_w2(5,8) = 0.2088439017534256d0
      model1_w2(5,9) = -0.0708695650100708d0
      model1_w2(6,1) = 0.18123726546764374d0
      model1_w2(6,2) = 0.2190399467945099d0
      model1_w2(6,3) = -0.23917415738105774d0
      model1_w2(6,4) = -0.3471594452857971d0
      model1_w2(6,5) = -0.04653840884566307d0
      model1_w2(6,6) = -0.04987189173698425d0
      model1_w2(6,7) = -0.2201811820268631d0
      model1_w2(6,8) = 0.12996584177017212d0
      model1_w2(6,9) = -0.32904061675071716d0
      model1_w2(7,1) = 0.20387998223304749d0
      model1_w2(7,2) = 0.2657499611377716d0
      model1_w2(7,3) = 0.46624183654785156d0
      model1_w2(7,4) = -0.06516877561807632d0
      model1_w2(7,5) = 0.7708205580711365d0
      model1_w2(7,6) = 0.005811542272567749d0
      model1_w2(7,7) = -0.8765473961830139d0
      model1_w2(7,8) = 0.5438826680183411d0
      model1_w2(7,9) = 0.5088119506835938d0
      model1_w2(8,1) = 0.290800005197525d0
      model1_w2(8,2) = -0.18382517993450165d0
      model1_w2(8,3) = -0.3424682021141052d0
      model1_w2(8,4) = 0.3822537362575531d0
      model1_w2(8,5) = 0.24322780966758728d0
      model1_w2(8,6) = 0.2456413209438324d0
      model1_w2(8,7) = 0.3616553544998169d0
      model1_w2(8,8) = 0.6159040927886963d0
      model1_w2(8,9) = -0.2832071781158447d0
      model1_w2(9,1) = -0.2776246964931488d0
      model1_w2(9,2) = -0.2852957844734192d0
      model1_w2(9,3) = 0.1643056571483612d0
      model1_w2(9,4) = -0.1713992804288864d0
      model1_w2(9,5) = -0.18843524158000946d0
      model1_w2(9,6) = 0.21246913075447083d0
      model1_w2(9,7) = -0.1340492218732834d0
      model1_w2(9,8) = -0.025242537260055542d0
      model1_w2(9,9) = 0.12656626105308533d0
! model1_hidden_layers.0.bias
      model1_b2(1,1) = 0.4458763301372528d0
      model1_b2(1,2) = -0.12825609743595123d0
      model1_b2(1,3) = 0.6884883046150208d0
      model1_b2(1,4) = 0.5656911134719849d0
      model1_b2(1,5) = -0.16423626244068146d0
      model1_b2(1,6) = 0.27374234795570374d0
      model1_b2(1,7) = 0.12137995660305023d0
      model1_b2(1,8) = -0.08400547504425049d0
      model1_b2(1,9) = -0.33267292380332947d0
! model1_hidden_layers.1.weight
      model1_w3(1,1) = 0.7102617621421814d0
      model1_w3(1,2) = -0.7993353009223938d0
      model1_w3(1,3) = 0.3219507932662964d0
      model1_w3(1,4) = -0.1847507357597351d0
      model1_w3(1,5) = 0.012675595469772816d0
      model1_w3(1,6) = 0.10140907764434814d0
      model1_w3(1,7) = -0.3069400489330292d0
      model1_w3(1,8) = 0.4429132640361786d0
      model1_w3(1,9) = 0.11130475997924805d0
      model1_w3(2,1) = -1.113595962524414d0
      model1_w3(2,2) = 0.5686254501342773d0
      model1_w3(2,3) = 0.7501143217086792d0
      model1_w3(2,4) = 0.24874918162822723d0
      model1_w3(2,5) = 0.21068894863128662d0
      model1_w3(2,6) = 0.11631347239017487d0
      model1_w3(2,7) = 0.3364488482475281d0
      model1_w3(2,8) = -0.4495278596878052d0
      model1_w3(2,9) = -0.14144930243492126d0
      model1_w3(3,1) = -0.7697535753250122d0
      model1_w3(3,2) = 0.9138484597206116d0
      model1_w3(3,3) = 0.8123010396957397d0
      model1_w3(3,4) = 0.6189084053039551d0
      model1_w3(3,5) = 0.16113214194774628d0
      model1_w3(3,6) = -0.029181648045778275d0
      model1_w3(3,7) = 0.4570799767971039d0
      model1_w3(3,8) = -0.19687466323375702d0
      model1_w3(3,9) = 1.519918441772461e-06
      model1_w3(4,1) = -1.057836651802063d0
      model1_w3(4,2) = 0.5900413393974304d0
      model1_w3(4,3) = 0.30880773067474365d0
      model1_w3(4,4) = 0.8706649541854858d0
      model1_w3(4,5) = -0.03550605848431587d0
      model1_w3(4,6) = 0.3611569106578827d0
      model1_w3(4,7) = 0.5957608222961426d0
      model1_w3(4,8) = -0.5269845128059387d0
      model1_w3(4,9) = 0.09654513001441956d0
      model1_w3(5,1) = 1.0869507789611816d0
      model1_w3(5,2) = -0.6295535564422607d0
      model1_w3(5,3) = 0.25242990255355835d0
      model1_w3(5,4) = -0.16656623780727386d0
      model1_w3(5,5) = -0.07882286608219147d0
      model1_w3(5,6) = 0.14139065146446228d0
      model1_w3(5,7) = -0.1968136429786682d0
      model1_w3(5,8) = 0.8345522880554199d0
      model1_w3(5,9) = 0.1671527922153473d0
      model1_w3(6,1) = 1.4054292440414429d0
      model1_w3(6,2) = -0.28644004464149475d0
      model1_w3(6,3) = 0.32352229952812195d0
      model1_w3(6,4) = -0.29861724376678467d0
      model1_w3(6,5) = -0.2112942636013031d0
      model1_w3(6,6) = 0.18732649087905884d0
      model1_w3(6,7) = -0.693874180316925d0
      model1_w3(6,8) = 0.9636648893356323d0
      model1_w3(6,9) = 0.13844779133796692d0
      model1_w3(7,1) = -0.8566271066665649d0
      model1_w3(7,2) = 0.7632412910461426d0
      model1_w3(7,3) = 0.518107533454895d0
      model1_w3(7,4) = 0.6202042102813721d0
      model1_w3(7,5) = -0.0705418735742569d0
      model1_w3(7,6) = 0.14472393691539764d0
      model1_w3(7,7) = 0.7282719612121582d0
      model1_w3(7,8) = -0.5515934824943542d0
      model1_w3(7,9) = 0.12341150641441345d0
      model1_w3(8,1) = -0.7972953915596008d0
      model1_w3(8,2) = 0.48361510038375854d0
      model1_w3(8,3) = 0.6371060609817505d0
      model1_w3(8,4) = 0.27300775051116943d0
      model1_w3(8,5) = -0.041048839688301086d0
      model1_w3(8,6) = -0.0837019756436348d0
      model1_w3(8,7) = 0.7133356332778931d0
      model1_w3(8,8) = -0.00566799147054553d0
      model1_w3(8,9) = -0.038595050573349d0
      model1_w3(9,1) = -0.5419386029243469d0
      model1_w3(9,2) = 0.8763414025306702d0
      model1_w3(9,3) = 0.7428808808326721d0
      model1_w3(9,4) = 0.6340868473052979d0
      model1_w3(9,5) = 0.16541995108127594d0
      model1_w3(9,6) = -0.12949922680854797d0
      model1_w3(9,7) = 0.5089005827903748d0
      model1_w3(9,8) = -0.38314035534858704d0
      model1_w3(9,9) = -0.25687384605407715d0
! model1_hidden_layers.1.bias
      model1_b3(1,1) = -0.04732884094119072d0
      model1_b3(1,2) = 0.40327560901641846d0
      model1_b3(1,3) = 0.5535919666290283d0
      model1_b3(1,4) = 0.2795674800872803d0
      model1_b3(1,5) = 0.16155606508255005d0
      model1_b3(1,6) = 0.3786679804325104d0
      model1_b3(1,7) = 0.34324491024017334d0
      model1_b3(1,8) = 0.5925180315971375d0
      model1_b3(1,9) = 0.4781711995601654d0
! model1_hidden_layers.2.weight
      model1_w4(1,1) = -0.9642623066902161d0
      model1_w4(1,2) = 0.4179089069366455d0
      model1_w4(1,3) = 0.44851067662239075d0
      model1_w4(1,4) = 0.48903071880340576d0
      model1_w4(1,5) = -0.9941849112510681d0
      model1_w4(1,6) = -1.1878408193588257d0
      model1_w4(1,7) = 0.3394182622432709d0
      model1_w4(1,8) = 0.040215253829956055d0
      model1_w4(1,9) = 0.0772649273276329d0
      model1_w4(2,1) = -1.7533961534500122d0
      model1_w4(2,2) = 0.7949208617210388d0
      model1_w4(2,3) = 0.3760264217853546d0
      model1_w4(2,4) = 0.7401663064956665d0
      model1_w4(2,5) = -0.2912736237049103d0
      model1_w4(2,6) = -0.6186468005180359d0
      model1_w4(2,7) = 0.7366520166397095d0
      model1_w4(2,8) = 0.5546929836273193d0
      model1_w4(2,9) = 0.6499462723731995d0
      model1_w4(3,1) = -1.5480983257293701d0
      model1_w4(3,2) = 0.5595046281814575d0
      model1_w4(3,3) = 0.7118808031082153d0
      model1_w4(3,4) = 0.468540757894516d0
      model1_w4(3,5) = -0.5093170404434204d0
      model1_w4(3,6) = -0.5161328911781311d0
      model1_w4(3,7) = 0.7448580265045166d0
      model1_w4(3,8) = 0.7926362752914429d0
      model1_w4(3,9) = 0.620907723903656d0
      model1_w4(4,1) = -1.687849521636963d0
      model1_w4(4,2) = 0.40552040934562683d0
      model1_w4(4,3) = 0.4423161745071411d0
      model1_w4(4,4) = 0.6074065566062927d0
      model1_w4(4,5) = -1.1699309349060059d0
      model1_w4(4,6) = -1.481742024421692d0
      model1_w4(4,7) = 0.6390907764434814d0
      model1_w4(4,8) = 0.6360065340995789d0
      model1_w4(4,9) = 0.27089059352874756d0
      model1_w4(5,1) = -0.31929510831832886d0
      model1_w4(5,2) = 0.3876676559448242d0
      model1_w4(5,3) = 0.7615064978599548d0
      model1_w4(5,4) = 0.7496820688247681d0
      model1_w4(5,5) = -0.053261756896972656d0
      model1_w4(5,6) = 0.05224957317113876d0
      model1_w4(5,7) = 0.4293161928653717d0
      model1_w4(5,8) = 0.965556800365448d0
      model1_w4(5,9) = 1.0142898559570312d0
      model1_w4(6,1) = 0.07417508214712143d0
      model1_w4(6,2) = 0.020137032493948936d0
      model1_w4(6,3) = -0.17081589996814728d0
      model1_w4(6,4) = 0.12260175496339798d0
      model1_w4(6,5) = -0.038437068462371826d0
      model1_w4(6,6) = -0.25067728757858276d0
      model1_w4(6,7) = -0.21949371695518494d0
      model1_w4(6,8) = 0.06823289394378662d0
      model1_w4(6,9) = 0.10386228561401367d0
      model1_w4(7,1) = 0.8362196087837219d0
      model1_w4(7,2) = 0.18261484801769257d0
      model1_w4(7,3) = 0.06374379247426987d0
      model1_w4(7,4) = -0.20062430202960968d0
      model1_w4(7,5) = 1.261258840560913d0
      model1_w4(7,6) = 1.2647995948791504d0
      model1_w4(7,7) = -0.0747705027461052d0
      model1_w4(7,8) = -0.32586991786956787d0
      model1_w4(7,9) = -0.3171899914741516d0
      model1_w4(8,1) = -1.1366486549377441d0
      model1_w4(8,2) = 0.784542977809906d0
      model1_w4(8,3) = 0.8376007676124573d0
      model1_w4(8,4) = 0.5156993269920349d0
      model1_w4(8,5) = -0.7747506499290466d0
      model1_w4(8,6) = -0.9350093007087708d0
      model1_w4(8,7) = 0.6127182841300964d0
      model1_w4(8,8) = 0.4678395986557007d0
      model1_w4(8,9) = 0.4593496322631836d0
      model1_w4(9,1) = -0.6935150623321533d0
      model1_w4(9,2) = 0.41424378752708435d0
      model1_w4(9,3) = 0.3294745683670044d0
      model1_w4(9,4) = 0.42383453249931335d0
      model1_w4(9,5) = -0.1808495670557022d0
      model1_w4(9,6) = -0.938456118106842d0
      model1_w4(9,7) = 0.763489305973053d0
      model1_w4(9,8) = 0.5256944894790649d0
      model1_w4(9,9) = 0.2991580069065094d0
! model1_hidden_layers.2.bias
      model1_b4(1,1) = 0.09720579534769058d0
      model1_b4(1,2) = -0.1314428299665451d0
      model1_b4(1,3) = 0.02452849969267845d0
      model1_b4(1,4) = -0.43850553035736084d0
      model1_b4(1,5) = 0.7202531695365906d0
      model1_b4(1,6) = 0.11606022715568542d0
      model1_b4(1,7) = 0.2810879051685333d0
      model1_b4(1,8) = 0.18154607713222504d0
      model1_b4(1,9) = 0.08695316314697266d0
! model1_hidden_layers.3.weight
      model1_w5(1,1) = 0.10843989253044128d0
      model1_w5(1,2) = -0.20269295573234558d0
      model1_w5(1,3) = -0.2383023500442505d0
      model1_w5(1,4) = 0.2042529284954071d0
      model1_w5(1,5) = -0.2380537986755371d0
      model1_w5(1,6) = 0.15650996565818787d0
      model1_w5(1,7) = -0.17927324771881104d0
      model1_w5(1,8) = 0.2197505533695221d0
      model1_w5(1,9) = 0.10751417279243469d0
      model1_w5(2,1) = 0.3876243829727173d0
      model1_w5(2,2) = 0.7488009333610535d0
      model1_w5(2,3) = 0.452920138835907d0
      model1_w5(2,4) = 0.5326200723648071d0
      model1_w5(2,5) = 0.5393655300140381d0
      model1_w5(2,6) = 0.1888674795627594d0
      model1_w5(2,7) = -1.1214070320129395d0
      model1_w5(2,8) = 0.7548540830612183d0
      model1_w5(2,9) = 0.5577192902565002d0
      model1_w5(3,1) = 0.22382494807243347d0
      model1_w5(3,2) = 0.1524582952260971d0
      model1_w5(3,3) = 0.12289422005414963d0
      model1_w5(3,4) = -0.06928718835115433d0
      model1_w5(3,5) = 0.02743556722998619d0
      model1_w5(3,6) = -0.19732801616191864d0
      model1_w5(3,7) = -0.20513233542442322d0
      model1_w5(3,8) = -0.3774919807910919d0
      model1_w5(3,9) = -0.0006223751115612686d0
      model1_w5(4,1) = 0.46240246295928955d0
      model1_w5(4,2) = 0.7719236612319946d0
      model1_w5(4,3) = 0.8559908270835876d0
      model1_w5(4,4) = 0.5860627293586731d0
      model1_w5(4,5) = 1.0510179996490479d0
      model1_w5(4,6) = 0.016760794445872307d0
      model1_w5(4,7) = -0.07965540885925293d0
      model1_w5(4,8) = 0.6437051296234131d0
      model1_w5(4,9) = 0.8241767883300781d0
      model1_w5(5,1) = 0.5632852911949158d0
      model1_w5(5,2) = -0.19130223989486694d0
      model1_w5(5,3) = 0.17515958845615387d0
      model1_w5(5,4) = 0.2531568706035614d0
      model1_w5(5,5) = 0.2729189693927765d0
      model1_w5(5,6) = -0.10407690703868866d0
      model1_w5(5,7) = 0.027937738224864006d0
      model1_w5(5,8) = -0.4510730504989624d0
      model1_w5(5,9) = -0.16327597200870514d0
      model1_w5(6,1) = 0.8467310070991516d0
      model1_w5(6,2) = 0.741504430770874d0
      model1_w5(6,3) = 0.8459659218788147d0
      model1_w5(6,4) = 0.8519889712333679d0
      model1_w5(6,5) = 0.6519148945808411d0
      model1_w5(6,6) = -0.03468950837850571d0
      model1_w5(6,7) = -0.057584911584854126d0
      model1_w5(6,8) = 0.8676846027374268d0
      model1_w5(6,9) = 0.24647283554077148d0
      model1_w5(7,1) = 0.1387958824634552d0
      model1_w5(7,2) = -0.008165746927261353d0
      model1_w5(7,3) = 0.07114681601524353d0
      model1_w5(7,4) = -0.3315126597881317d0
      model1_w5(7,5) = -0.3140028119087219d0
      model1_w5(7,6) = 0.19701114296913147d0
      model1_w5(7,7) = -0.12247726321220398d0
      model1_w5(7,8) = -0.32049423456192017d0
      model1_w5(7,9) = -0.3102385997772217d0
      model1_w5(8,1) = -0.6510381698608398d0
      model1_w5(8,2) = 0.2635539472103119d0
      model1_w5(8,3) = 0.433964341878891d0
      model1_w5(8,4) = -0.3881585896015167d0
      model1_w5(8,5) = -0.31081512570381165d0
      model1_w5(8,6) = 0.22620877623558044d0
      model1_w5(8,7) = 1.1616779565811157d0
      model1_w5(8,8) = 0.11857062578201294d0
      model1_w5(8,9) = -0.03520578145980835d0
      model1_w5(9,1) = 0.09122039377689362d0
      model1_w5(9,2) = 0.047437429428100586d0
      model1_w5(9,3) = 0.2627785801887512d0
      model1_w5(9,4) = -0.07299039512872696d0
      model1_w5(9,5) = -0.30231034755706787d0
      model1_w5(9,6) = 0.3011145293712616d0
      model1_w5(9,7) = -0.22914902865886688d0
      model1_w5(9,8) = -0.16517654061317444d0
      model1_w5(9,9) = 0.22794482111930847d0
! model1_hidden_layers.3.bias
      model1_b5(1,1) = -0.3247557282447815d0
      model1_b5(1,2) = -0.29854676127433777d0
      model1_b5(1,3) = -0.21811066567897797d0
      model1_b5(1,4) = 1.0215187072753906d0
      model1_b5(1,5) = -0.37816092371940613d0
      model1_b5(1,6) = 0.7445167303085327d0
      model1_b5(1,7) = -0.05011752247810364d0
      model1_b5(1,8) = 0.4926255941390991d0
      model1_b5(1,9) = -0.30119121074676514d0
! model1_hidden_layers.4.weight
      model1_w6(1,1) = -0.14708420634269714d0
      model1_w6(1,2) = -0.07055047154426575d0
      model1_w6(1,3) = -0.20638780295848846d0
      model1_w6(1,4) = -0.08015719056129456d0
      model1_w6(1,5) = 0.14798545837402344d0
      model1_w6(1,6) = -0.12878768146038055d0
      model1_w6(1,7) = 0.045865267515182495d0
      model1_w6(1,8) = -0.260695219039917d0
      model1_w6(1,9) = -0.2165112942457199d0
      model1_w6(2,1) = 0.1921415627002716d0
      model1_w6(2,2) = 0.8389366269111633d0
      model1_w6(2,3) = -0.04010063409805298d0
      model1_w6(2,4) = 0.741913914680481d0
      model1_w6(2,5) = -0.010314212180674076d0
      model1_w6(2,6) = 0.5452962517738342d0
      model1_w6(2,7) = -0.22585853934288025d0
      model1_w6(2,8) = -0.612201988697052d0
      model1_w6(2,9) = -0.19526876509189606d0
      model1_w6(3,1) = -0.04135596752166748d0
      model1_w6(3,2) = 0.13100670278072357d0
      model1_w6(3,3) = 0.18858353793621063d0
      model1_w6(3,4) = -0.006092555820941925d0
      model1_w6(3,5) = -0.22942231595516205d0
      model1_w6(3,6) = -0.10455480217933655d0
      model1_w6(3,7) = -0.3092721402645111d0
      model1_w6(3,8) = 1.2349684238433838d0
      model1_w6(3,9) = -0.12456376850605011d0
      model1_w6(4,1) = -0.07594907283782959d0
      model1_w6(4,2) = -0.3329571485519409d0
      model1_w6(4,3) = -0.2196059226989746d0
      model1_w6(4,4) = -1.7604140043258667d0
      model1_w6(4,5) = -0.6195018291473389d0
      model1_w6(4,6) = -1.3168271780014038d0
      model1_w6(4,7) = 0.12897047400474548d0
      model1_w6(4,8) = 1.1752574443817139d0
      model1_w6(4,9) = 0.13495135307312012d0
      model1_w6(5,1) = -0.17768506705760956d0
      model1_w6(5,2) = -0.6694992780685425d0
      model1_w6(5,3) = -0.3265420198440552d0
      model1_w6(5,4) = -1.5405081510543823d0
      model1_w6(5,5) = -1.1284701824188232d0
      model1_w6(5,6) = -1.6085156202316284d0
      model1_w6(5,7) = 0.0640343725681305d0
      model1_w6(5,8) = 0.8991484642028809d0
      model1_w6(5,9) = 0.30167922377586365d0
      model1_w6(6,1) = 0.10226711630821228d0
      model1_w6(6,2) = 0.03820481523871422d0
      model1_w6(6,3) = 0.0642988309264183d0
      model1_w6(6,4) = 0.10627009719610214d0
      model1_w6(6,5) = -0.03938107565045357d0
      model1_w6(6,6) = -0.15358595550060272d0
      model1_w6(6,7) = -0.14762184023857117d0
      model1_w6(6,8) = -0.1844470500946045d0
      model1_w6(6,9) = -0.10928760468959808d0
      model1_w6(7,1) = -0.06221163272857666d0
      model1_w6(7,2) = 0.805444061756134d0
      model1_w6(7,3) = -0.26791271567344666d0
      model1_w6(7,4) = 0.2657296061515808d0
      model1_w6(7,5) = 0.19984181225299835d0
      model1_w6(7,6) = 0.5254884958267212d0
      model1_w6(7,7) = 0.12286496162414551d0
      model1_w6(7,8) = -1.4391734600067139d0
      model1_w6(7,9) = 0.08320976048707962d0
      model1_w6(8,1) = -0.257292240858078d0
      model1_w6(8,2) = 0.04862368851900101d0
      model1_w6(8,3) = -0.19310569763183594d0
      model1_w6(8,4) = 0.21808333694934845d0
      model1_w6(8,5) = -0.7328030467033386d0
      model1_w6(8,6) = -0.2732136845588684d0
      model1_w6(8,7) = 0.04175803065299988d0
      model1_w6(8,8) = 1.3003681898117065d0
      model1_w6(8,9) = -0.26791515946388245d0
      model1_w6(9,1) = 0.3233547508716583d0
      model1_w6(9,2) = 0.20313267409801483d0
      model1_w6(9,3) = -0.1657853126525879d0
      model1_w6(9,4) = -0.15080024302005768d0
      model1_w6(9,5) = -1.186536192893982d0
      model1_w6(9,6) = 0.031851738691329956d0
      model1_w6(9,7) = 0.09157371520996094d0
      model1_w6(9,8) = 1.1695685386657715d0
      model1_w6(9,9) = 0.2908729016780853d0
! model1_hidden_layers.4.bias
      model1_b6(1,1) = -0.03238677978515625d0
      model1_b6(1,2) = 0.5982076525688171d0
      model1_b6(1,3) = 0.39482754468917847d0
      model1_b6(1,4) = 0.6265862584114075d0
      model1_b6(1,5) = 0.525774359703064d0
      model1_b6(1,6) = -0.2630085051059723d0
      model1_b6(1,7) = -0.3282582759857178d0
      model1_b6(1,8) = 0.29461291432380676d0
      model1_b6(1,9) = 0.4078598618507385d0
! model1_output_layer.weight
      model1_w7(1,1) = 0.24630191922187805d0
      model1_w7(1,2) = 0.7110228538513184d0
      model1_w7(1,3) = -0.9830858707427979d0
      model1_w7(1,4) = -1.386780023574829d0
      model1_w7(1,5) = -1.3367729187011719d0
      model1_w7(1,6) = -0.1579604595899582d0
      model1_w7(1,7) = 0.6287941336631775d0
      model1_w7(1,8) = -1.1026654243469238d0
      model1_w7(1,9) = -1.0847141742706299d0
! model1_output_layer.bias
      model1_b7(1,1) = 0.28261539340019226d0
      
! model2_input_layer.weight
      model2_w1(1,1) = 0.32193031907081604d0
      model2_w1(1,2) = 0.3960241377353668d0
      model2_w1(1,3) = 0.3749643862247467d0
      model2_w1(1,4) = -0.11687841266393661d0
      model2_w1(1,5) = -0.4610370099544525d0
      model2_w1(1,6) = -0.653115451335907d0
      model2_w1(2,1) = 0.5196325778961182d0
      model2_w1(2,2) = -0.4532700479030609d0
      model2_w1(2,3) = 0.16801191866397858d0
      model2_w1(2,4) = -0.01864282786846161d0
      model2_w1(2,5) = -0.4805883467197418d0
      model2_w1(2,6) = -0.6187543869018555d0
      model2_w1(3,1) = -0.10760871320962906d0
      model2_w1(3,2) = -0.1651497483253479d0
      model2_w1(3,3) = -0.40068817138671875d0
      model2_w1(3,4) = 0.2808096408843994d0
      model2_w1(3,5) = -0.021683931350708008d0
      model2_w1(3,6) = 0.3465840816497803d0
      model2_w1(4,1) = -0.08292152732610703d0
      model2_w1(4,2) = 0.6562455892562866d0
      model2_w1(4,3) = 0.3613753616809845d0
      model2_w1(4,4) = -0.113151915371418d0
      model2_w1(4,5) = -0.47005969285964966d0
      model2_w1(4,6) = -0.1772131472826004d0
      model2_w1(5,1) = 0.16634927690029144d0
      model2_w1(5,2) = 0.5863516330718994d0
      model2_w1(5,3) = -0.0644671842455864d0
      model2_w1(5,4) = -0.11237972229719162d0
      model2_w1(5,5) = -0.7868658900260925d0
      model2_w1(5,6) = -1.058688998222351d0
      model2_w1(6,1) = 0.12437693029642105d0
      model2_w1(6,2) = -0.6660160422325134d0
      model2_w1(6,3) = 0.6532979607582092d0
      model2_w1(6,4) = -0.991607129573822d0
      model2_w1(6,5) = -0.42376232147216797d0
      model2_w1(6,6) = -0.025136278942227364d0
      model2_w1(7,1) = -0.2288404107093811d0
      model2_w1(7,2) = -0.2271905094385147d0
      model2_w1(7,3) = -0.35637882351875305d0
      model2_w1(7,4) = 0.3620421886444092d0
      model2_w1(7,5) = -0.1208680272102356d0
      model2_w1(7,6) = -0.02277153730392456d0
      model2_w1(8,1) = 0.39655575156211853d0
      model2_w1(8,2) = -0.4633123576641083d0
      model2_w1(8,3) = 0.3314105272293091d0
      model2_w1(8,4) = -0.6532965302467346d0
      model2_w1(8,5) = -1.0859051942825317d0
      model2_w1(8,6) = -0.5651305317878723d0
      model2_w1(9,1) = 0.8699139356613159d0
      model2_w1(9,2) = -0.7336575388908386d0
      model2_w1(9,3) = -0.6200912594795227d0
      model2_w1(9,4) = -0.7148450613021851d0
      model2_w1(9,5) = -0.6811404824256897d0
      model2_w1(9,6) = -0.665900468826294d0
! model2_input_layer.bias
      model2_b1(1,1) = -0.6662160754203796d0
      model2_b1(1,2) = -0.2341005653142929d0
      model2_b1(1,3) = 0.3674738109111786d0
      model2_b1(1,4) = -0.11428201198577881d0
      model2_b1(1,5) = -0.7078685164451599d0
      model2_b1(1,6) = -0.022404780611395836d0
      model2_b1(1,7) = -0.08613011240959167d0
      model2_b1(1,8) = -0.145091250538826d0
      model2_b1(1,9) = 0.1668958067893982d0
! model2_hidden_layers.0.weight
      model2_w2(1,1) = 0.23606599867343903d0
      model2_w2(1,2) = -0.380507230758667d0
      model2_w2(1,3) = 0.2862068712711334d0
      model2_w2(1,4) = 0.19602835178375244d0
      model2_w2(1,5) = 0.9070737361907959d0
      model2_w2(1,6) = 0.4046453833580017d0
      model2_w2(1,7) = -0.06744778156280518d0
      model2_w2(1,8) = 0.5370634198188782d0
      model2_w2(1,9) = 0.40022727847099304d0
      model2_w2(2,1) = 0.11284694820642471d0
      model2_w2(2,2) = 0.08955491334199905d0
      model2_w2(2,3) = 0.1122528612613678d0
      model2_w2(2,4) = 0.1907574087381363d0
      model2_w2(2,5) = 1.0003918409347534d0
      model2_w2(2,6) = 0.7102049589157104d0
      model2_w2(2,7) = -0.2450031042098999d0
      model2_w2(2,8) = 0.438229501247406d0
      model2_w2(2,9) = 0.959076464176178d0
      model2_w2(3,1) = 0.06119022145867348d0
      model2_w2(3,2) = 0.4192851185798645d0
      model2_w2(3,3) = -0.24097780883312225d0
      model2_w2(3,4) = 0.44964712858200073d0
      model2_w2(3,5) = 0.9833163022994995d0
      model2_w2(3,6) = 0.8148440718650818d0
      model2_w2(3,7) = -0.04115381836891174d0
      model2_w2(3,8) = 0.5657507181167603d0
      model2_w2(3,9) = 0.627782940864563d0
      model2_w2(4,1) = 0.5576700568199158d0
      model2_w2(4,2) = -0.042386628687381744d0
      model2_w2(4,3) = -0.3109440207481384d0
      model2_w2(4,4) = 0.46307992935180664d0
      model2_w2(4,5) = 0.7541642189025879d0
      model2_w2(4,6) = 0.5065889358520508d0
      model2_w2(4,7) = 0.09245166182518005d0
      model2_w2(4,8) = 0.2239779829978943d0
      model2_w2(4,9) = 0.5934054255485535d0
      model2_w2(5,1) = 0.28486111760139465d0
      model2_w2(5,2) = 0.042588599026203156d0
      model2_w2(5,3) = 0.08268973976373672d0
      model2_w2(5,4) = 0.3001512885093689d0
      model2_w2(5,5) = 1.1408556699752808d0
      model2_w2(5,6) = 0.28689301013946533d0
      model2_w2(5,7) = 0.05889737606048584d0
      model2_w2(5,8) = 0.587466835975647d0
      model2_w2(5,9) = 0.5145970582962036d0
      model2_w2(6,1) = 0.20488989353179932d0
      model2_w2(6,2) = 0.4717281758785248d0
      model2_w2(6,3) = 0.1254819631576538d0
      model2_w2(6,4) = 0.5590744018554688d0
      model2_w2(6,5) = 0.790102481842041d0
      model2_w2(6,6) = 0.5579062700271606d0
      model2_w2(6,7) = -0.2940416932106018d0
      model2_w2(6,8) = 0.42963290214538574d0
      model2_w2(6,9) = 0.5044662356376648d0
      model2_w2(7,1) = 0.3551024794578552d0
      model2_w2(7,2) = 0.46160346269607544d0
      model2_w2(7,3) = -0.272370845079422d0
      model2_w2(7,4) = 0.4770863354206085d0
      model2_w2(7,5) = 1.2061941623687744d0
      model2_w2(7,6) = 0.46025699377059937d0
      model2_w2(7,7) = -0.1898355931043625d0
      model2_w2(7,8) = 0.3492257595062256d0
      model2_w2(7,9) = 0.7515994310379028d0
      model2_w2(8,1) = 0.49268630146980286d0
      model2_w2(8,2) = 0.0757332295179367d0
      model2_w2(8,3) = -0.17526988685131073d0
      model2_w2(8,4) = 0.12567931413650513d0
      model2_w2(8,5) = 1.1836574077606201d0
      model2_w2(8,6) = 0.4996396601200104d0
      model2_w2(8,7) = -0.20892450213432312d0
      model2_w2(8,8) = 0.6474205255508423d0
      model2_w2(8,9) = 0.740432620048523d0
      model2_w2(9,1) = 0.4398357570171356d0
      model2_w2(9,2) = 0.3191207945346832d0
      model2_w2(9,3) = 0.2702120542526245d0
      model2_w2(9,4) = 0.040846191346645355d0
      model2_w2(9,5) = 0.7176291942596436d0
      model2_w2(9,6) = 0.6584148406982422d0
      model2_w2(9,7) = -0.17054566740989685d0
      model2_w2(9,8) = 0.7041199207305908d0
      model2_w2(9,9) = 0.49996212124824524d0
! model2_hidden_layers.0.bias
      model2_b2(1,1) = -0.39845460653305054d0
      model2_b2(1,2) = -0.4445953965187073d0
      model2_b2(1,3) = -0.5277683138847351d0
      model2_b2(1,4) = -0.19407659769058228d0
      model2_b2(1,5) = -0.09889217466115952d0
      model2_b2(1,6) = -0.5287895202636719d0
      model2_b2(1,7) = -0.0684714987874031d0
      model2_b2(1,8) = -0.26989248394966125d0
      model2_b2(1,9) = -0.48268887400627136d0
! model2_hidden_layers.1.weight
      model2_w3(1,1) = -0.24650828540325165d0
      model2_w3(1,2) = -0.19731950759887695d0
      model2_w3(1,3) = -0.10973648726940155d0
      model2_w3(1,4) = -0.16106268763542175d0
      model2_w3(1,5) = 0.13582658767700195d0
      model2_w3(1,6) = 0.15596958994865417d0
      model2_w3(1,7) = -0.3193301856517792d0
      model2_w3(1,8) = 0.07001113891601562d0
      model2_w3(1,9) = -0.10681092739105225d0
      model2_w3(2,1) = 0.3733842670917511d0
      model2_w3(2,2) = 0.8632975220680237d0
      model2_w3(2,3) = 0.9032677412033081d0
      model2_w3(2,4) = 0.6207300424575806d0
      model2_w3(2,5) = 0.2873000502586365d0
      model2_w3(2,6) = 0.8159258365631104d0
      model2_w3(2,7) = 0.6719578504562378d0
      model2_w3(2,8) = 0.6839820742607117d0
      model2_w3(2,9) = 0.7645495533943176d0
      model2_w3(3,1) = 0.10369344800710678d0
      model2_w3(3,2) = 0.20353128015995026d0
      model2_w3(3,3) = -0.040305543690919876d0
      model2_w3(3,4) = 0.08115676045417786d0
      model2_w3(3,5) = 0.3002915680408478d0
      model2_w3(3,6) = -0.3096012771129608d0
      model2_w3(3,7) = -0.1473412960767746d0
      model2_w3(3,8) = -0.22941699624061584d0
      model2_w3(3,9) = -0.2967684864997864d0
      model2_w3(4,1) = 0.5399515628814697d0
      model2_w3(4,2) = 0.3045080900192261d0
      model2_w3(4,3) = 0.9281384944915771d0
      model2_w3(4,4) = 0.6537741422653198d0
      model2_w3(4,5) = 0.6251211762428284d0
      model2_w3(4,6) = 0.7414333820343018d0
      model2_w3(4,7) = 0.7536960244178772d0
      model2_w3(4,8) = 0.5630459189414978d0
      model2_w3(4,9) = 0.28494760394096375d0
      model2_w3(5,1) = -0.2942151129245758d0
      model2_w3(5,2) = -0.07240575551986694d0
      model2_w3(5,3) = 0.10057573020458221d0
      model2_w3(5,4) = 0.13156546652317047d0
      model2_w3(5,5) = -0.40047937631607056d0
      model2_w3(5,6) = 0.03835871070623398d0
      model2_w3(5,7) = 0.19067461788654327d0
      model2_w3(5,8) = 0.18628202378749847d0
      model2_w3(5,9) = -0.22338703274726868d0
      model2_w3(6,1) = 0.08044323325157166d0
      model2_w3(6,2) = 0.31384900212287903d0
      model2_w3(6,3) = 0.06877146661281586d0
      model2_w3(6,4) = -0.12353292107582092d0
      model2_w3(6,5) = 0.08866539597511292d0
      model2_w3(6,6) = 0.028312159702181816d0
      model2_w3(6,7) = -0.19171246886253357d0
      model2_w3(6,8) = -0.4024372100830078d0
      model2_w3(6,9) = 0.15681052207946777d0
      model2_w3(7,1) = 0.12224999070167542d0
      model2_w3(7,2) = 0.375084787607193d0
      model2_w3(7,3) = 0.2705135941505432d0
      model2_w3(7,4) = -0.5312618017196655d0
      model2_w3(7,5) = -0.2797452509403229d0
      model2_w3(7,6) = 0.20050452649593353d0
      model2_w3(7,7) = -0.48919713497161865d0
      model2_w3(7,8) = 0.05431896448135376d0
      model2_w3(7,9) = 0.15307290852069855d0
      model2_w3(8,1) = 0.3976656198501587d0
      model2_w3(8,2) = 0.9320551753044128d0
      model2_w3(8,3) = 0.8523600697517395d0
      model2_w3(8,4) = 0.8412402272224426d0
      model2_w3(8,5) = 0.3512997627258301d0
      model2_w3(8,6) = 0.48552706837654114d0
      model2_w3(8,7) = 0.6199502944946289d0
      model2_w3(8,8) = 0.29486095905303955d0
      model2_w3(8,9) = 0.8727031946182251d0
      model2_w3(9,1) = 0.6904162764549255d0
      model2_w3(9,2) = 0.9583572745323181d0
      model2_w3(9,3) = 0.7776113748550415d0
      model2_w3(9,4) = 0.7821049094200134d0
      model2_w3(9,5) = 0.9146052598953247d0
      model2_w3(9,6) = 0.6422884464263916d0
      model2_w3(9,7) = 0.8692254424095154d0
      model2_w3(9,8) = 0.7152132391929626d0
      model2_w3(9,9) = 0.8820793032646179d0
! model2_hidden_layers.1.bias
      model2_b3(1,1) = -0.055333226919174194d0
      model2_b3(1,2) = -0.9952524900436401d0
      model2_b3(1,3) = -0.0560225248336792d0
      model2_b3(1,4) = -0.27835872769355774d0
      model2_b3(1,5) = -0.4017472565174103d0
      model2_b3(1,6) = 0.3695340156555176d0
      model2_b3(1,7) = 0.5772308111190796d0
      model2_b3(1,8) = -0.6891332864761353d0
      model2_b3(1,9) = 0.4970256984233856d0
! model2_hidden_layers.2.weight
      model2_w4(1,1) = 0.299429327249527d0
      model2_w4(1,2) = -0.6391595005989075d0
      model2_w4(1,3) = 0.30062761902809143d0
      model2_w4(1,4) = 0.11125369369983673d0
      model2_w4(1,5) = 0.0452863872051239d0
      model2_w4(1,6) = -0.4598318338394165d0
      model2_w4(1,7) = -1.3080650568008423d0
      model2_w4(1,8) = -0.28744426369667053d0
      model2_w4(1,9) = 0.4130128026008606d0
      model2_w4(2,1) = 0.0326920747756958d0
      model2_w4(2,2) = -0.13130903244018555d0
      model2_w4(2,3) = 0.32690587639808655d0
      model2_w4(2,4) = 0.01902708411216736d0
      model2_w4(2,5) = 0.2738301455974579d0
      model2_w4(2,6) = -0.2555198669433594d0
      model2_w4(2,7) = 0.21598675847053528d0
      model2_w4(2,8) = -0.02384006977081299d0
      model2_w4(2,9) = -0.2053995132446289d0
      model2_w4(3,1) = 0.24962303042411804d0
      model2_w4(3,2) = 0.3926328122615814d0
      model2_w4(3,3) = -0.1983252912759781d0
      model2_w4(3,4) = 1.0055913925170898d0
      model2_w4(3,5) = -0.14657241106033325d0
      model2_w4(3,6) = 1.4734086990356445d0
      model2_w4(3,7) = 1.5873115062713623d0
      model2_w4(3,8) = 0.7061631679534912d0
      model2_w4(3,9) = 0.6718250513076782d0
      model2_w4(4,1) = -0.0006981194019317627d0
      model2_w4(4,2) = 0.497391015291214d0
      model2_w4(4,3) = -0.33808133006095886d0
      model2_w4(4,4) = 0.9264422059059143d0
      model2_w4(4,5) = 0.061905719339847565d0
      model2_w4(4,6) = 1.5699652433395386d0
      model2_w4(4,7) = 1.7149665355682373d0
      model2_w4(4,8) = 0.7598471641540527d0
      model2_w4(4,9) = 0.6248980760574341d0
      model2_w4(5,1) = -0.14098688960075378d0
      model2_w4(5,2) = 1.0208300352096558d0
      model2_w4(5,3) = -0.26906707882881165d0
      model2_w4(5,4) = 0.6537373065948486d0
      model2_w4(5,5) = 0.06993577629327774d0
      model2_w4(5,6) = 1.2632986307144165d0
      model2_w4(5,7) = 1.3146296739578247d0
      model2_w4(5,8) = 0.6063238978385925d0
      model2_w4(5,9) = 0.7127450108528137d0
      model2_w4(6,1) = 0.11629292368888855d0
      model2_w4(6,2) = -0.1450256109237671d0
      model2_w4(6,3) = 0.2473377287387848d0
      model2_w4(6,4) = -0.3072788119316101d0
      model2_w4(6,5) = -0.3128672242164612d0
      model2_w4(6,6) = 0.030550843104720116d0
      model2_w4(6,7) = -0.07415083050727844d0
      model2_w4(6,8) = 0.10598530620336533d0
      model2_w4(6,9) = 0.18614454567432404d0
      model2_w4(7,1) = 0.165543794631958d0
      model2_w4(7,2) = 0.08601208031177521d0
      model2_w4(7,3) = -0.16810917854309082d0
      model2_w4(7,4) = -0.21926094591617584d0
      model2_w4(7,5) = -0.012371029704809189d0
      model2_w4(7,6) = 0.21730314195156097d0
      model2_w4(7,7) = 0.09613200277090073d0
      model2_w4(7,8) = -0.04926375672221184d0
      model2_w4(7,9) = 0.008556888438761234d0
      model2_w4(8,1) = 0.30595269799232483d0
      model2_w4(8,2) = 0.7749544382095337d0
      model2_w4(8,3) = 0.15985539555549622d0
      model2_w4(8,4) = 0.4303998649120331d0
      model2_w4(8,5) = -0.18040397763252258d0
      model2_w4(8,6) = 1.4281973838806152d0
      model2_w4(8,7) = 1.587752103805542d0
      model2_w4(8,8) = 0.9158552885055542d0
      model2_w4(8,9) = 0.9460280537605286d0
      model2_w4(9,1) = 0.14642652869224548d0
      model2_w4(9,2) = -0.2635779082775116d0
      model2_w4(9,3) = 0.24221590161323547d0
      model2_w4(9,4) = 0.09216329455375671d0
      model2_w4(9,5) = 0.25220027565956116d0
      model2_w4(9,6) = 0.20007410645484924d0
      model2_w4(9,7) = 0.2626921236515045d0
      model2_w4(9,8) = -0.16907092928886414d0
      model2_w4(9,9) = -0.1756104677915573d0
! model2_hidden_layers.2.bias
      model2_b4(1,1) = 1.7763638496398926d0
      model2_b4(1,2) = -0.23478016257286072d0
      model2_b4(1,3) = 0.224398672580719d0
      model2_b4(1,4) = 0.2027444839477539d0
      model2_b4(1,5) = 0.019202951341867447d0
      model2_b4(1,6) = -0.40978944301605225d0
      model2_b4(1,7) = -0.14031173288822174d0
      model2_b4(1,8) = 0.17037256062030792d0
      model2_b4(1,9) = -0.30122458934783936d0
! model2_output_layer.weight
      model2_w5(1,1) = -2.555863380432129d0
      model2_w5(1,2) = -0.22667138278484344d0
      model2_w5(1,3) = 0.6064371466636658d0
      model2_w5(1,4) = 0.5771418213844299d0
      model2_w5(1,5) = 0.7466092705726624d0
      model2_w5(1,6) = -0.15439879894256592d0
      model2_w5(1,7) = -0.27802249789237976d0
      model2_w5(1,8) = 0.6913106441497803d0
      model2_w5(1,9) = -0.04063403606414795d0
! model2_output_layer.bias
      model2_b5(1,1) = 0.1217830628156662d0

! model3_input_layer.weight
      model3_w1(1,1) = 0.3965331017971039d0
      model3_w1(1,2) = 0.34580087661743164d0
      model3_w1(1,3) = 0.12077218294143677d0
      model3_w1(1,4) = -0.6600552201271057d0
      model3_w1(1,5) = -0.353536456823349d0
      model3_w1(1,6) = -0.5123719573020935d0
      model3_w1(2,1) = -0.30970266461372375d0
      model3_w1(2,2) = -0.07294130325317383d0
      model3_w1(2,3) = -0.2968329191207886d0
      model3_w1(2,4) = -0.03926461935043335d0
      model3_w1(2,5) = -0.0515037477016449d0
      model3_w1(2,6) = 0.4044625759124756d0
      model3_w1(3,1) = -0.16539910435676575d0
      model3_w1(3,2) = -0.30572283267974854d0
      model3_w1(3,3) = -0.06121426820755005d0
      model3_w1(3,4) = 0.1893898844718933d0
      model3_w1(3,5) = 0.15148448944091797d0
      model3_w1(3,6) = -0.3872409462928772d0
      model3_w1(4,1) = 0.32589808106422424d0
      model3_w1(4,2) = 0.024440975859761238d0
      model3_w1(4,3) = 0.4068923592567444d0
      model3_w1(4,4) = -0.5656362771987915d0
      model3_w1(4,5) = -0.8022282719612122d0
      model3_w1(4,6) = -0.5750306248664856d0
      model3_w1(5,1) = -0.5030530691146851d0
      model3_w1(5,2) = -0.11084578931331635d0
      model3_w1(5,3) = 0.6700794100761414d0
      model3_w1(5,4) = -0.6889730095863342d0
      model3_w1(5,5) = -0.31362709403038025d0
      model3_w1(5,6) = -0.32988905906677246d0
      model3_w1(6,1) = 0.5170407295227051d0
      model3_w1(6,2) = -0.7419587969779968d0
      model3_w1(6,3) = 0.4606085419654846d0
      model3_w1(6,4) = -0.5270767211914062d0
      model3_w1(6,5) = -0.28102239966392517d0
      model3_w1(6,6) = -0.39440974593162537d0
      model3_w1(7,1) = 0.038769423961639404d0
      model3_w1(7,2) = -0.3275541067123413d0
      model3_w1(7,3) = -0.3958769738674164d0
      model3_w1(7,4) = -0.1531846523284912d0
      model3_w1(7,5) = 0.14398539066314697d0
      model3_w1(7,6) = 0.07745939493179321d0
      model3_w1(8,1) = 0.35903772711753845d0
      model3_w1(8,2) = -0.46169063448905945d0
      model3_w1(8,3) = 0.2383941411972046d0
      model3_w1(8,4) = -0.3195595443248749d0
      model3_w1(8,5) = -0.9868440628051758d0
      model3_w1(8,6) = -0.7297748327255249d0
      model3_w1(9,1) = 0.24298834800720215d0
      model3_w1(9,2) = 0.3884594142436981d0
      model3_w1(9,3) = -0.06604667007923126d0
      model3_w1(9,4) = -0.6473424434661865d0
      model3_w1(9,5) = -0.027258971706032753d0
      model3_w1(9,6) = -0.5420491695404053d0
! model3_input_layer.bias
      model3_b1(1,1) = -0.26269927620887756d0
      model3_b1(1,2) = -0.35931456089019775d0
      model3_b1(1,3) = -0.10890364646911621d0
      model3_b1(1,4) = -0.02926352247595787d0
      model3_b1(1,5) = -0.08584233373403549d0
      model3_b1(1,6) = -0.3409305214881897d0
      model3_b1(1,7) = -0.2810218334197998d0
      model3_b1(1,8) = -0.29358842968940735d0
      model3_b1(1,9) = 0.08682011812925339d0
! model3_hidden_layers.0.weight
      model3_w2(1,1) = 0.038590624928474426d0
      model3_w2(1,2) = -0.07410943508148193d0
      model3_w2(1,3) = 0.1653108298778534d0
      model3_w2(1,4) = -0.5280359983444214d0
      model3_w2(1,5) = -0.03102014772593975d0
      model3_w2(1,6) = 0.5243807435035706d0
      model3_w2(1,7) = 0.25442638993263245d0
      model3_w2(1,8) = -0.29988032579421997d0
      model3_w2(1,9) = -0.22282865643501282d0
      model3_w2(2,1) = -0.2284127175807953d0
      model3_w2(2,2) = 0.05945602059364319d0
      model3_w2(2,3) = 0.22079059481620789d0
      model3_w2(2,4) = -0.07576906681060791d0
      model3_w2(2,5) = -0.1349170207977295d0
      model3_w2(2,6) = -0.06967630982398987d0
      model3_w2(2,7) = -0.27897071838378906d0
      model3_w2(2,8) = -0.1762937307357788d0
      model3_w2(2,9) = 0.13434326648712158d0
      model3_w2(3,1) = 0.1742885708808899d0
      model3_w2(3,2) = 0.0851355791091919d0
      model3_w2(3,3) = 0.10030603408813477d0
      model3_w2(3,4) = 0.15100254118442535d0
      model3_w2(3,5) = 0.8187077045440674d0
      model3_w2(3,6) = 0.6215920448303223d0
      model3_w2(3,7) = 0.1124577522277832d0
      model3_w2(3,8) = 0.8804743885993958d0
      model3_w2(3,9) = -0.029086509719491005d0
      model3_w2(4,1) = 0.4488877058029175d0
      model3_w2(4,2) = 0.22777387499809265d0
      model3_w2(4,3) = -0.19967269897460938d0
      model3_w2(4,4) = 0.04003911092877388d0
      model3_w2(4,5) = 0.2159438133239746d0
      model3_w2(4,6) = 0.5081828236579895d0
      model3_w2(4,7) = -0.03406500816345215d0
      model3_w2(4,8) = 0.4390679597854614d0
      model3_w2(4,9) = -0.042944926768541336d0
      model3_w2(5,1) = 0.24832826852798462d0
      model3_w2(5,2) = 0.23220786452293396d0
      model3_w2(5,3) = 0.10488855838775635d0
      model3_w2(5,4) = -0.005754909478127956d0
      model3_w2(5,5) = 0.5377516746520996d0
      model3_w2(5,6) = 0.48757603764533997d0
      model3_w2(5,7) = -0.18099626898765564d0
      model3_w2(5,8) = 0.69315105676651d0
      model3_w2(5,9) = 0.4469684064388275d0
      model3_w2(6,1) = -0.29568615555763245d0
      model3_w2(6,2) = -0.09080962836742401d0
      model3_w2(6,3) = 0.20510700345039368d0
      model3_w2(6,4) = 0.2582944631576538d0
      model3_w2(6,5) = 0.3411140739917755d0
      model3_w2(6,6) = 0.5632268190383911d0
      model3_w2(6,7) = 0.23623505234718323d0
      model3_w2(6,8) = 0.6840859651565552d0
      model3_w2(6,9) = -0.6787874698638916d0
      model3_w2(7,1) = 0.3611849248409271d0
      model3_w2(7,2) = -0.17242448031902313d0
      model3_w2(7,3) = 0.13691803812980652d0
      model3_w2(7,4) = -0.34249141812324524d0
      model3_w2(7,5) = 0.3392697870731354d0
      model3_w2(7,6) = 0.857039749622345d0
      model3_w2(7,7) = -0.0717022716999054d0
      model3_w2(7,8) = 0.7039754390716553d0
      model3_w2(7,9) = 0.24117185175418854d0
      model3_w2(8,1) = 0.5522592067718506d0
      model3_w2(8,2) = -0.0431266725063324d0
      model3_w2(8,3) = -0.04156717658042908d0
      model3_w2(8,4) = 0.2502298355102539d0
      model3_w2(8,5) = 0.3251587748527527d0
      model3_w2(8,6) = 0.7518342137336731d0
      model3_w2(8,7) = 0.2258158028125763d0
      model3_w2(8,8) = 0.23598648607730865d0
      model3_w2(8,9) = 0.3044045567512512d0
      model3_w2(9,1) = -0.2017650306224823d0
      model3_w2(9,2) = 0.1478288471698761d0
      model3_w2(9,3) = -0.1669701784849167d0
      model3_w2(9,4) = 0.26480165123939514d0
      model3_w2(9,5) = -0.10522174835205078d0
      model3_w2(9,6) = -0.03881934657692909d0
      model3_w2(9,7) = -0.09079274535179138d0
      model3_w2(9,8) = -0.08251229673624039d0
      model3_w2(9,9) = 0.119231678545475d0
! model3_hidden_layers.0.bias
      model3_b2(1,1) = 0.08916836231946945d0
      model3_b2(1,2) = -0.16662363708019257d0
      model3_b2(1,3) = -0.009700463153421879d0
      model3_b2(1,4) = -0.04441434144973755d0
      model3_b2(1,5) = -0.3205036222934723d0
      model3_b2(1,6) = -0.5816043019294739d0
      model3_b2(1,7) = -0.12444189190864563d0
      model3_b2(1,8) = -0.06211769953370094d0
      model3_b2(1,9) = -0.38285061717033386d0
! model3_hidden_layers.1.weight
      model3_w3(1,1) = 1.1035676002502441d0
      model3_w3(1,2) = 0.2586258351802826d0
      model3_w3(1,3) = -1.1816725730895996d0
      model3_w3(1,4) = -1.2069858312606812d0
      model3_w3(1,5) = 0.034330494701862335d0
      model3_w3(1,6) = 0.1331387758255005d0
      model3_w3(1,7) = 0.1714256852865219d0
      model3_w3(1,8) = -1.2603271007537842d0
      model3_w3(1,9) = 0.027610570192337036d0
      model3_w3(2,1) = 0.8080838918685913d0
      model3_w3(2,2) = -0.21769456565380096d0
      model3_w3(2,3) = 0.3141091763973236d0
      model3_w3(2,4) = 0.4824103116989136d0
      model3_w3(2,5) = 0.6164878606796265d0
      model3_w3(2,6) = -0.8420034646987915d0
      model3_w3(2,7) = 0.6160451769828796d0
      model3_w3(2,8) = 0.42055797576904297d0
      model3_w3(2,9) = 0.20792748034000397d0
      model3_w3(3,1) = 1.0581860542297363d0
      model3_w3(3,2) = 0.2315492331981659d0
      model3_w3(3,3) = 0.41494041681289673d0
      model3_w3(3,4) = 0.0742395892739296d0
      model3_w3(3,5) = 0.7122228741645813d0
      model3_w3(3,6) = -0.4959607720375061d0
      model3_w3(3,7) = 0.6250494122505188d0
      model3_w3(3,8) = 0.5415272116661072d0
      model3_w3(3,9) = -0.29282277822494507d0
      model3_w3(4,1) = 1.1205906867980957d0
      model3_w3(4,2) = 0.2540692985057831d0
      model3_w3(4,3) = -1.1202890872955322d0
      model3_w3(4,4) = -0.7821868062019348d0
      model3_w3(4,5) = -0.010118638165295124d0
      model3_w3(4,6) = -0.08878335356712341d0
      model3_w3(4,7) = 0.20726589858531952d0
      model3_w3(4,8) = -1.1118921041488647d0
      model3_w3(4,9) = -0.2697550058364868d0
      model3_w3(5,1) = 0.3944779932498932d0
      model3_w3(5,2) = -0.2751712501049042d0
      model3_w3(5,3) = 0.720371663570404d0
      model3_w3(5,4) = 0.14145642518997192d0
      model3_w3(5,5) = 0.3388598561286926d0
      model3_w3(5,6) = -1.082446813583374d0
      model3_w3(5,7) = 0.20795978605747223d0
      model3_w3(5,8) = 0.3916471004486084d0
      model3_w3(5,9) = -0.2772822082042694d0
      model3_w3(6,1) = 1.1070865392684937d0
      model3_w3(6,2) = -0.12310592830181122d0
      model3_w3(6,3) = 0.4341767430305481d0
      model3_w3(6,4) = 0.1929013431072235d0
      model3_w3(6,5) = 0.6996837258338928d0
      model3_w3(6,6) = -0.6176921725273132d0
      model3_w3(6,7) = 0.5585916638374329d0
      model3_w3(6,8) = 0.24708323180675507d0
      model3_w3(6,9) = -0.1444854885339737d0
      model3_w3(7,1) = 1.1777769327163696d0
      model3_w3(7,2) = -0.12035326659679413d0
      model3_w3(7,3) = 0.14824721217155457d0
      model3_w3(7,4) = 0.6779726147651672d0
      model3_w3(7,5) = 0.6653963327407837d0
      model3_w3(7,6) = -0.918159008026123d0
      model3_w3(7,7) = 0.4046820104122162d0
      model3_w3(7,8) = 0.5886439681053162d0
      model3_w3(7,9) = 0.35417434573173523d0
      model3_w3(8,1) = 0.6496090292930603d0
      model3_w3(8,2) = 0.29102107882499695d0
      model3_w3(8,3) = 0.7302860617637634d0
      model3_w3(8,4) = 0.45360267162323d0
      model3_w3(8,5) = 0.6431860327720642d0
      model3_w3(8,6) = -0.7775320410728455d0
      model3_w3(8,7) = 0.2899567782878876d0
      model3_w3(8,8) = 0.5793638229370117d0
      model3_w3(8,9) = 0.24225887656211853d0
      model3_w3(9,1) = 0.7080591320991516d0
      model3_w3(9,2) = 0.08738985657691956d0
      model3_w3(9,3) = 0.5034322738647461d0
      model3_w3(9,4) = 0.412874311208725d0
      model3_w3(9,5) = 0.3759107291698456d0
      model3_w3(9,6) = -0.6827079653739929d0
      model3_w3(9,7) = 0.8428512215614319d0
      model3_w3(9,8) = 0.19162321090698242d0
      model3_w3(9,9) = -0.16484200954437256d0
! model3_hidden_layers.1.bias
      model3_b3(1,1) = 0.34798362851142883d0
      model3_b3(1,2) = -0.5122495889663696d0
      model3_b3(1,3) = -0.3361353874206543d0
      model3_b3(1,4) = 0.17329041659832d0
      model3_b3(1,5) = -0.02375776134431362d0
      model3_b3(1,6) = -0.0758385881781578d0
      model3_b3(1,7) = -0.30132749676704407d0
      model3_b3(1,8) = -0.6324198246002197d0
      model3_b3(1,9) = -0.32250040769577026d0
! model3_hidden_layers.2.weight
      model3_w4(1,1) = -0.1719409078359604d0
      model3_w4(1,2) = 0.6822872757911682d0
      model3_w4(1,3) = 0.6347280740737915d0
      model3_w4(1,4) = -0.1724901795387268d0
      model3_w4(1,5) = 0.25509539246559143d0
      model3_w4(1,6) = 0.14182139933109283d0
      model3_w4(1,7) = 0.3401033878326416d0
      model3_w4(1,8) = 0.40833377838134766d0
      model3_w4(1,9) = 0.7574889063835144d0
      model3_w4(2,1) = -0.7438011765480042d0
      model3_w4(2,2) = -0.6401572227478027d0
      model3_w4(2,3) = -0.15284229815006256d0
      model3_w4(2,4) = -0.5366922616958618d0
      model3_w4(2,5) = 0.327350378036499d0
      model3_w4(2,6) = -0.08296408504247665d0
      model3_w4(2,7) = -0.1910850554704666d0
      model3_w4(2,8) = -0.2778310179710388d0
      model3_w4(2,9) = -0.3368925154209137d0
      model3_w4(3,1) = 0.2705250084400177d0
      model3_w4(3,2) = -0.2350662648677826d0
      model3_w4(3,3) = -0.18265502154827118d0
      model3_w4(3,4) = -0.04695984721183777d0
      model3_w4(3,5) = -0.2706592082977295d0
      model3_w4(3,6) = -0.312910795211792d0
      model3_w4(3,7) = 0.1166466474533081d0
      model3_w4(3,8) = 0.11484742164611816d0
      model3_w4(3,9) = -0.08247837424278259d0
      model3_w4(4,1) = 0.06382811069488525d0
      model3_w4(4,2) = 0.33178797364234924d0
      model3_w4(4,3) = -0.12717458605766296d0
      model3_w4(4,4) = -0.022550106048583984d0
      model3_w4(4,5) = -0.2562006115913391d0
      model3_w4(4,6) = 0.023551613092422485d0
      model3_w4(4,7) = 0.10208937525749207d0
      model3_w4(4,8) = -0.20843105018138885d0
      model3_w4(4,9) = 0.09497857093811035d0
      model3_w4(5,1) = -0.3198263347148895d0
      model3_w4(5,2) = -0.2517523765563965d0
      model3_w4(5,3) = 0.2929746210575104d0
      model3_w4(5,4) = 0.25184622406959534d0
      model3_w4(5,5) = -0.15165770053863525d0
      model3_w4(5,6) = -0.18740805983543396d0
      model3_w4(5,7) = 0.17024275660514832d0
      model3_w4(5,8) = 0.03628140315413475d0
      model3_w4(5,9) = -0.012564185075461864d0
      model3_w4(6,1) = -0.15325269103050232d0
      model3_w4(6,2) = 0.4433118402957916d0
      model3_w4(6,3) = 0.7246395945549011d0
      model3_w4(6,4) = -0.08229078352451324d0
      model3_w4(6,5) = 0.7624821662902832d0
      model3_w4(6,6) = 0.535659670829773d0
      model3_w4(6,7) = 0.6731340885162354d0
      model3_w4(6,8) = 0.6155699491500854d0
      model3_w4(6,9) = 0.32481664419174194d0
      model3_w4(7,1) = 0.046918123960494995d0
      model3_w4(7,2) = 0.6102895736694336d0
      model3_w4(7,3) = 0.6959823966026306d0
      model3_w4(7,4) = 0.222621887922287d0
      model3_w4(7,5) = 0.4576645791530609d0
      model3_w4(7,6) = 0.5161308646202087d0
      model3_w4(7,7) = 0.7111837863922119d0
      model3_w4(7,8) = 0.4454410970211029d0
      model3_w4(7,9) = 0.7964776158332825d0
      model3_w4(8,1) = 0.17597058415412903d0
      model3_w4(8,2) = -0.02849023975431919d0
      model3_w4(8,3) = 0.015517139807343483d0
      model3_w4(8,4) = -0.07516941428184509d0
      model3_w4(8,5) = 0.22616097331047058d0
      model3_w4(8,6) = -0.1265551596879959d0
      model3_w4(8,7) = 0.32345476746559143d0
      model3_w4(8,8) = -0.21779318153858185d0
      model3_w4(8,9) = -0.4641016721725464d0
      model3_w4(9,1) = -0.8524943590164185d0
      model3_w4(9,2) = -0.5924831032752991d0
      model3_w4(9,3) = -0.5657125115394592d0
      model3_w4(9,4) = -1.1127363443374634d0
      model3_w4(9,5) = 0.7205765843391418d0
      model3_w4(9,6) = -0.21378931403160095d0
      model3_w4(9,7) = -0.025005722418427467d0
      model3_w4(9,8) = -0.30098554491996765d0
      model3_w4(9,9) = -0.967305064201355d0
! model3_hidden_layers.2.bias
      model3_b4(1,1) = -0.43546435236930847d0
      model3_b4(1,2) = 0.442771852016449d0
      model3_b4(1,3) = -0.25103646516799927d0
      model3_b4(1,4) = -0.1480250060558319d0
      model3_b4(1,5) = -0.2775166630744934d0
      model3_b4(1,6) = -0.06916350871324539d0
      model3_b4(1,7) = -0.6478758454322815d0
      model3_b4(1,8) = -0.08564923703670502d0
      model3_b4(1,9) = 0.6355218291282654d0
! model3_hidden_layers.3.weight
      model3_w5(1,1) = -0.21407334506511688d0
      model3_w5(1,2) = 0.2922620475292206d0
      model3_w5(1,3) = 0.23928818106651306d0
      model3_w5(1,4) = -0.13938944041728973d0
      model3_w5(1,5) = 0.012989312410354614d0
      model3_w5(1,6) = -0.27780982851982117d0
      model3_w5(1,7) = -0.1153169572353363d0
      model3_w5(1,8) = 0.10541081428527832d0
      model3_w5(1,9) = -0.2000008076429367d0
      model3_w5(2,1) = 0.22134052217006683d0
      model3_w5(2,2) = -0.4935835301876068d0
      model3_w5(2,3) = 0.12100860476493835d0
      model3_w5(2,4) = 0.22795209288597107d0
      model3_w5(2,5) = -0.015379239805042744d0
      model3_w5(2,6) = 0.41407668590545654d0
      model3_w5(2,7) = 0.44236546754837036d0
      model3_w5(2,8) = -0.19963109493255615d0
      model3_w5(2,9) = 0.12429448217153549d0
      model3_w5(3,1) = 0.6240683794021606d0
      model3_w5(3,2) = -0.5627152919769287d0
      model3_w5(3,3) = 0.10641929507255554d0
      model3_w5(3,4) = 0.08184781670570374d0
      model3_w5(3,5) = -0.18375948071479797d0
      model3_w5(3,6) = 0.6980221271514893d0
      model3_w5(3,7) = 0.6829242706298828d0
      model3_w5(3,8) = -0.045445315539836884d0
      model3_w5(3,9) = -0.2145877182483673d0
      model3_w5(4,1) = -0.24609415233135223d0
      model3_w5(4,2) = -0.03710368275642395d0
      model3_w5(4,3) = 0.13858771324157715d0
      model3_w5(4,4) = -0.3329029977321625d0
      model3_w5(4,5) = 0.3257997930049896d0
      model3_w5(4,6) = 0.08543047308921814d0
      model3_w5(4,7) = -0.27238065004348755d0
      model3_w5(4,8) = -0.16473719477653503d0
      model3_w5(4,9) = 0.07362321019172668d0
      model3_w5(5,1) = 0.21449534595012665d0
      model3_w5(5,2) = -0.5392571687698364d0
      model3_w5(5,3) = -0.24300694465637207d0
      model3_w5(5,4) = 0.13549944758415222d0
      model3_w5(5,5) = -0.07518209517002106d0
      model3_w5(5,6) = 0.6765382885932922d0
      model3_w5(5,7) = 0.5752255320549011d0
      model3_w5(5,8) = 0.06717859208583832d0
      model3_w5(5,9) = 0.014902116730809212d0
      model3_w5(6,1) = 0.5009231567382812d0
      model3_w5(6,2) = -0.32035383582115173d0
      model3_w5(6,3) = -0.17557796835899353d0
      model3_w5(6,4) = -0.12582282721996307d0
      model3_w5(6,5) = 0.27853623032569885d0
      model3_w5(6,6) = 0.6333416104316711d0
      model3_w5(6,7) = 0.6064570546150208d0
      model3_w5(6,8) = -0.0524936206638813d0
      model3_w5(6,9) = -0.03894050791859627d0
      model3_w5(7,1) = -0.7597731351852417d0
      model3_w5(7,2) = 0.16055236756801605d0
      model3_w5(7,3) = -0.2613620460033417d0
      model3_w5(7,4) = -0.13365814089775085d0
      model3_w5(7,5) = 0.2564615309238434d0
      model3_w5(7,6) = -0.3159765899181366d0
      model3_w5(7,7) = 0.3773125112056732d0
      model3_w5(7,8) = 0.3418563902378082d0
      model3_w5(7,9) = 0.3240465223789215d0
      model3_w5(8,1) = 0.32013002038002014d0
      model3_w5(8,2) = 0.23717942833900452d0
      model3_w5(8,3) = 0.16360560059547424d0
      model3_w5(8,4) = -0.3140546679496765d0
      model3_w5(8,5) = 0.29423853754997253d0
      model3_w5(8,6) = -0.07667842507362366d0
      model3_w5(8,7) = -0.21503846347332d0
      model3_w5(8,8) = 0.2207522690296173d0
      model3_w5(8,9) = 0.22801551222801208d0
      model3_w5(9,1) = 0.6752935647964478d0
      model3_w5(9,2) = 1.0858277082443237d0
      model3_w5(9,3) = -0.021234124898910522d0
      model3_w5(9,4) = 0.14185631275177002d0
      model3_w5(9,5) = 0.06283590197563171d0
      model3_w5(9,6) = 0.44687870144844055d0
      model3_w5(9,7) = 0.08817355334758759d0
      model3_w5(9,8) = 0.4366300404071808d0
      model3_w5(9,9) = 1.4631929397583008d0
! model3_hidden_layers.3.bias
      model3_b5(1,1) = -0.3261376619338989d0
      model3_b5(1,2) = -0.6288453340530396d0
      model3_b5(1,3) = -0.2900548577308655d0
      model3_b5(1,4) = -0.25148364901542664d0
      model3_b5(1,5) = -0.42573320865631104d0
      model3_b5(1,6) = -0.4355798363685608d0
      model3_b5(1,7) = -0.013155424036085606d0
      model3_b5(1,8) = -0.28702646493911743d0
      model3_b5(1,9) = -0.00822508055716753d0
! model3_hidden_layers.4.weight
      model3_w6(1,1) = -0.15920798480510712d0
      model3_w6(1,2) = -0.5190618634223938d0
      model3_w6(1,3) = -0.3652508556842804d0
      model3_w6(1,4) = 0.14643701910972595d0
      model3_w6(1,5) = -0.5812450647354126d0
      model3_w6(1,6) = -0.3015458285808563d0
      model3_w6(1,7) = 0.5024741888046265d0
      model3_w6(1,8) = 0.05534765124320984d0
      model3_w6(1,9) = 0.8953561186790466d0
      model3_w6(2,1) = 0.27287134528160095d0
      model3_w6(2,2) = 0.5121484994888306d0
      model3_w6(2,3) = 0.653676450252533d0
      model3_w6(2,4) = -0.09838823974132538d0
      model3_w6(2,5) = 0.5344873666763306d0
      model3_w6(2,6) = 0.15836477279663086d0
      model3_w6(2,7) = -0.29437127709388733d0
      model3_w6(2,8) = -0.3172357976436615d0
      model3_w6(2,9) = 0.6012988686561584d0
      model3_w6(3,1) = 0.06832382082939148d0
      model3_w6(3,2) = 0.31734761595726013d0
      model3_w6(3,3) = 0.5066268444061279d0
      model3_w6(3,4) = 0.24564573168754578d0
      model3_w6(3,5) = 0.4971086382865906d0
      model3_w6(3,6) = 0.6613784432411194d0
      model3_w6(3,7) = -0.4303669035434723d0
      model3_w6(3,8) = -0.04683449864387512d0
      model3_w6(3,9) = 0.3840373158454895d0
      model3_w6(4,1) = 0.13468989729881287d0
      model3_w6(4,2) = 0.3017323315143585d0
      model3_w6(4,3) = 0.6544817686080933d0
      model3_w6(4,4) = -0.16691768169403076d0
      model3_w6(4,5) = 0.5273803472518921d0
      model3_w6(4,6) = 0.5030015110969543d0
      model3_w6(4,7) = -0.897896409034729d0
      model3_w6(4,8) = -0.26218271255493164d0
      model3_w6(4,9) = 0.5012885928153992d0
      model3_w6(5,1) = 0.23880800604820251d0
      model3_w6(5,2) = -0.4497092664241791d0
      model3_w6(5,3) = -0.23867353796958923d0
      model3_w6(5,4) = 0.11315616965293884d0
      model3_w6(5,5) = -0.33912113308906555d0
      model3_w6(5,6) = -0.01774907484650612d0
      model3_w6(5,7) = 0.5517453551292419d0
      model3_w6(5,8) = -0.2748560309410095d0
      model3_w6(5,9) = 0.9640757441520691d0
      model3_w6(6,1) = 0.2305329144001007d0
      model3_w6(6,2) = 0.20609714090824127d0
      model3_w6(6,3) = 0.04393040016293526d0
      model3_w6(6,4) = 0.19650182127952576d0
      model3_w6(6,5) = -0.34391456842422485d0
      model3_w6(6,6) = 0.20541846752166748d0
      model3_w6(6,7) = -0.11690108478069305d0
      model3_w6(6,8) = -0.058076411485672d0
      model3_w6(6,9) = -0.14664043486118317d0
      model3_w6(7,1) = 0.2750517427921295d0
      model3_w6(7,2) = 0.24881166219711304d0
      model3_w6(7,3) = 0.19258564710617065d0
      model3_w6(7,4) = -0.0886152982711792d0
      model3_w6(7,5) = -0.043681222945451736d0
      model3_w6(7,6) = -0.3725549578666687d0
      model3_w6(7,7) = 0.06393900513648987d0
      model3_w6(7,8) = -0.005466490983963013d0
      model3_w6(7,9) = 0.12277968227863312d0
      model3_w6(8,1) = -0.12079775333404541d0
      model3_w6(8,2) = 0.2420235276222229d0
      model3_w6(8,3) = 0.8061710596084595d0
      model3_w6(8,4) = -0.3143995702266693d0
      model3_w6(8,5) = 0.388001412153244d0
      model3_w6(8,6) = 0.5556389093399048d0
      model3_w6(8,7) = -0.14095237851142883d0
      model3_w6(8,8) = -0.22829890251159668d0
      model3_w6(8,9) = 0.1486949920654297d0
      model3_w6(9,1) = -0.2079780548810959d0
      model3_w6(9,2) = -0.4909444749355316d0
      model3_w6(9,3) = -0.037299443036317825d0
      model3_w6(9,4) = 0.3100176751613617d0
      model3_w6(9,5) = 0.19608725607395172d0
      model3_w6(9,6) = -0.1454080492258072d0
      model3_w6(9,7) = -0.5659427642822266d0
      model3_w6(9,8) = 0.2985726296901703d0
      model3_w6(9,9) = -0.5436554551124573d0
! model3_hidden_layers.4.bias
      model3_b6(1,1) = -0.001517204800620675d0
      model3_b6(1,2) = -0.6857774257659912d0
      model3_b6(1,3) = -0.5615244507789612d0
      model3_b6(1,4) = -0.6117209196090698d0
      model3_b6(1,5) = -0.0013656039955094457d0
      model3_b6(1,6) = -0.0004093303869012743d0
      model3_b6(1,7) = -1.034940481185913d0
      model3_b6(1,8) = -0.35116785764694214d0
      model3_b6(1,9) = 2.2345123291015625d0
! model3_output_layer.weight
      model3_w7(1,1) = -0.7815183997154236d0
      model3_w7(1,2) = 0.6559582352638245d0
      model3_w7(1,3) = 0.3924822509288788d0
      model3_w7(1,4) = 0.6602423191070557d0
      model3_w7(1,5) = -1.1265747547149658d0
      model3_w7(1,6) = -0.26037612557411194d0
      model3_w7(1,7) = -1.4515421390533447d0
      model3_w7(1,8) = 0.4964098632335663d0
      model3_w7(1,9) = 2.124232769012451d0
! model3_output_layer.bias
      model3_b7(1,1) = 1.0840007066726685d0

! model4_input_layer.weight
      model4_w1(1,1) = 0.3557128608226776d0
      model4_w1(1,2) = 0.12669388949871063d0
      model4_w1(1,3) = -0.148206889629364d0
      model4_w1(1,4) = 0.8918097615242004d0
      model4_w1(1,5) = -0.49336493015289307d0
      model4_w1(1,6) = -0.3420172929763794d0
      model4_w1(2,1) = -0.07169713824987411d0
      model4_w1(2,2) = 0.45705312490463257d0
      model4_w1(2,3) = 0.032131094485521317d0
      model4_w1(2,4) = 0.8837084770202637d0
      model4_w1(2,5) = -0.15310218930244446d0
      model4_w1(2,6) = -0.059206679463386536d0
      model4_w1(3,1) = 0.24023739993572235d0
      model4_w1(3,2) = 0.39809176325798035d0
      model4_w1(3,3) = 0.5426313877105713d0
      model4_w1(3,4) = 0.10404906421899796d0
      model4_w1(3,5) = -0.4496234655380249d0
      model4_w1(3,6) = -0.3082142174243927d0
      model4_w1(4,1) = 0.6225636005401611d0
      model4_w1(4,2) = 0.4283047914505005d0
      model4_w1(4,3) = -0.04280628263950348d0
      model4_w1(4,4) = 0.20719560980796814d0
      model4_w1(4,5) = -0.39194154739379883d0
      model4_w1(4,6) = -0.640762984752655d0
      model4_w1(5,1) = 0.08499183505773544d0
      model4_w1(5,2) = 0.13780228793621063d0
      model4_w1(5,3) = 0.14773423969745636d0
      model4_w1(5,4) = 0.6432070732116699d0
      model4_w1(5,5) = -0.12822753190994263d0
      model4_w1(5,6) = -0.0867786779999733d0
      model4_w1(6,1) = -0.04173797369003296d0
      model4_w1(6,2) = 0.24098530411720276d0
      model4_w1(6,3) = 0.9145596623420715d0
      model4_w1(6,4) = -0.564102292060852d0
      model4_w1(6,5) = 0.15866786241531372d0
      model4_w1(6,6) = 0.2009923756122589d0
      model4_w1(7,1) = 0.5493726134300232d0
      model4_w1(7,2) = -0.18392008543014526d0
      model4_w1(7,3) = 0.6414381265640259d0
      model4_w1(7,4) = -0.8523254990577698d0
      model4_w1(7,5) = 0.43130820989608765d0
      model4_w1(7,6) = 0.3038424253463745d0
      model4_w1(8,1) = 0.5232687592506409d0
      model4_w1(8,2) = 0.014155395328998566d0
      model4_w1(8,3) = -0.17347636818885803d0
      model4_w1(8,4) = 0.7388902306556702d0
      model4_w1(8,5) = -0.21016182005405426d0
      model4_w1(8,6) = -0.22896157205104828d0
      model4_w1(9,1) = 0.18792282044887543d0
      model4_w1(9,2) = 0.582831084728241d0
      model4_w1(9,3) = -0.03499046713113785d0
      model4_w1(9,4) = 0.6524561643600464d0
      model4_w1(9,5) = -0.7682601809501648d0
      model4_w1(9,6) = -0.01650385931134224d0
! model4_input_layer.bias
      model4_b1(1,1) = -0.15796101093292236d0
      model4_b1(1,2) = 0.012286389246582985d0
      model4_b1(1,3) = 0.6740909218788147d0
      model4_b1(1,4) = -0.17900921404361725d0
      model4_b1(1,5) = 0.4647590219974518d0
      model4_b1(1,6) = 0.5480387210845947d0
      model4_b1(1,7) = 0.7681930065155029d0
      model4_b1(1,8) = 0.05782197043299675d0
      model4_b1(1,9) = -0.05828632414340973d0
! model4_hidden_layers.0.weight
      model4_w2(1,1) = 0.513546884059906d0
      model4_w2(1,2) = 0.468405544757843d0
      model4_w2(1,3) = -0.17355255782604218d0
      model4_w2(1,4) = 0.2934931814670563d0
      model4_w2(1,5) = 0.32765212655067444d0
      model4_w2(1,6) = -0.8801819682121277d0
      model4_w2(1,7) = -0.5522714853286743d0
      model4_w2(1,8) = 0.1513451784849167d0
      model4_w2(1,9) = 0.6282334327697754d0
      model4_w2(2,1) = 0.5780299305915833d0
      model4_w2(2,2) = 0.21140363812446594d0
      model4_w2(2,3) = 0.14526374638080597d0
      model4_w2(2,4) = 0.0627264678478241d0
      model4_w2(2,5) = 0.493264764547348d0
      model4_w2(2,6) = -0.4518764615058899d0
      model4_w2(2,7) = -0.6458889245986938d0
      model4_w2(2,8) = 0.4623895287513733d0
      model4_w2(2,9) = 0.19006577134132385d0
      model4_w2(3,1) = 0.6746425628662109d0
      model4_w2(3,2) = 0.2734617590904236d0
      model4_w2(3,3) = 0.34585461020469666d0
      model4_w2(3,4) = 0.44279780983924866d0
      model4_w2(3,5) = 0.9020498991012573d0
      model4_w2(3,6) = -0.3135697543621063d0
      model4_w2(3,7) = -0.2888576090335846d0
      model4_w2(3,8) = 0.689828634262085d0
      model4_w2(3,9) = 0.25172749161720276d0
      model4_w2(4,1) = 0.24705727398395538d0
      model4_w2(4,2) = 0.20804740488529205d0
      model4_w2(4,3) = 0.3125164210796356d0
      model4_w2(4,4) = 0.6812006235122681d0
      model4_w2(4,5) = 0.48182734847068787d0
      model4_w2(4,6) = -0.34530842304229736d0
      model4_w2(4,7) = -0.5819010734558105d0
      model4_w2(4,8) = 0.4469737708568573d0
      model4_w2(4,9) = 0.6569244861602783d0
      model4_w2(5,1) = 0.491171270608902d0
      model4_w2(5,2) = 0.47804203629493713d0
      model4_w2(5,3) = 0.3811964690685272d0
      model4_w2(5,4) = 0.40667903423309326d0
      model4_w2(5,5) = 0.7076382040977478d0
      model4_w2(5,6) = -0.4702533185482025d0
      model4_w2(5,7) = -0.7823545336723328d0
      model4_w2(5,8) = 0.15404343605041504d0
      model4_w2(5,9) = 0.5196211934089661d0
      model4_w2(6,1) = -0.39751115441322327d0
      model4_w2(6,2) = -0.029355686157941818d0
      model4_w2(6,3) = 0.4561152756214142d0
      model4_w2(6,4) = 0.16840937733650208d0
      model4_w2(6,5) = 0.21963997185230255d0
      model4_w2(6,6) = 0.5801358819007874d0
      model4_w2(6,7) = 0.5676742196083069d0
      model4_w2(6,8) = -0.04120359197258949d0
      model4_w2(6,9) = 0.022639937698841095d0
      model4_w2(7,1) = 0.2826274335384369d0
      model4_w2(7,2) = 0.6054876446723938d0
      model4_w2(7,3) = -0.07430596649646759d0
      model4_w2(7,4) = 0.454574853181839d0
      model4_w2(7,5) = 0.4083806872367859d0
      model4_w2(7,6) = -0.9077109694480896d0
      model4_w2(7,7) = -0.6206233501434326d0
      model4_w2(7,8) = 0.6381428241729736d0
      model4_w2(7,9) = 0.17677094042301178d0
      model4_w2(8,1) = 0.44507259130477905d0
      model4_w2(8,2) = 0.5369616746902466d0
      model4_w2(8,3) = -0.027232101187109947d0
      model4_w2(8,4) = 0.22176900506019592d0
      model4_w2(8,5) = 0.25861790776252747d0
      model4_w2(8,6) = -0.8388913869857788d0
      model4_w2(8,7) = -0.952803909778595d0
      model4_w2(8,8) = 0.15731410682201385d0
      model4_w2(8,9) = 0.5627108812332153d0
      model4_w2(9,1) = 0.6894404888153076d0
      model4_w2(9,2) = 0.3561451733112335d0
      model4_w2(9,3) = 0.03706684708595276d0
      model4_w2(9,4) = 0.6017959713935852d0
      model4_w2(9,5) = 0.1816461980342865d0
      model4_w2(9,6) = -0.9162867069244385d0
      model4_w2(9,7) = -0.4126548767089844d0
      model4_w2(9,8) = 0.6954171061515808d0
      model4_w2(9,9) = 0.3639107346534729d0
! model4_hidden_layers.0.bias
      model4_b2(1,1) = -0.17151036858558655d0
      model4_b2(1,2) = 0.005032335873693228d0
      model4_b2(1,3) = 0.48775652050971985d0
      model4_b2(1,4) = 0.16029050946235657d0
      model4_b2(1,5) = 0.15264873206615448d0
      model4_b2(1,6) = 0.3683323860168457d0
      model4_b2(1,7) = -0.07777340710163116d0
      model4_b2(1,8) = 0.034897126257419586d0
      model4_b2(1,9) = -0.30914974212646484d0
! model4_hidden_layers.1.weight
      model4_w3(1,1) = 0.36116230487823486d0
      model4_w3(1,2) = 0.36094698309898376d0
      model4_w3(1,3) = 0.18526645004749298d0
      model4_w3(1,4) = 0.08964257687330246d0
      model4_w3(1,5) = 0.4428683817386627d0
      model4_w3(1,6) = -1.0201663970947266d0
      model4_w3(1,7) = 0.5459685325622559d0
      model4_w3(1,8) = 0.5431535840034485d0
      model4_w3(1,9) = 0.504022479057312d0
      model4_w3(2,1) = -0.24868826568126678d0
      model4_w3(2,2) = 0.11162208765745163d0
      model4_w3(2,3) = 0.19905029237270355d0
      model4_w3(2,4) = 0.04443589970469475d0
      model4_w3(2,5) = 0.10820170491933823d0
      model4_w3(2,6) = 0.8847467303276062d0
      model4_w3(2,7) = -0.35166338086128235d0
      model4_w3(2,8) = -0.29809990525245667d0
      model4_w3(2,9) = 0.04451196640729904d0
      model4_w3(3,1) = 0.5267354249954224d0
      model4_w3(3,2) = 0.24904181063175201d0
      model4_w3(3,3) = 0.22960840165615082d0
      model4_w3(3,4) = 0.2298087775707245d0
      model4_w3(3,5) = 0.23979948461055756d0
      model4_w3(3,6) = -0.9188055396080017d0
      model4_w3(3,7) = 0.6606320142745972d0
      model4_w3(3,8) = 0.5052360892295837d0
      model4_w3(3,9) = 0.6841166019439697d0
      model4_w3(4,1) = 0.5351936221122742d0
      model4_w3(4,2) = 0.7073352932929993d0
      model4_w3(4,3) = 0.4514854848384857d0
      model4_w3(4,4) = 0.7378836870193481d0
      model4_w3(4,5) = 0.7262853980064392d0
      model4_w3(4,6) = -0.8068763613700867d0
      model4_w3(4,7) = 0.735575795173645d0
      model4_w3(4,8) = 0.6827141046524048d0
      model4_w3(4,9) = 0.618465781211853d0
      model4_w3(5,1) = -0.03767118602991104d0
      model4_w3(5,2) = 0.04634036868810654d0
      model4_w3(5,3) = 0.3517175614833832d0
      model4_w3(5,4) = 0.11535488069057465d0
      model4_w3(5,5) = 0.05754878744482994d0
      model4_w3(5,6) = 0.641591489315033d0
      model4_w3(5,7) = -0.35608717799186707d0
      model4_w3(5,8) = -0.14996238052845d0
      model4_w3(5,9) = -0.2690250873565674d0
      model4_w3(6,1) = 0.5033013820648193d0
      model4_w3(6,2) = 0.5077149271965027d0
      model4_w3(6,3) = 0.5683215260505676d0
      model4_w3(6,4) = 0.06927752494812012d0
      model4_w3(6,5) = 0.5818266272544861d0
      model4_w3(6,6) = -0.769873321056366d0
      model4_w3(6,7) = 0.6366482377052307d0
      model4_w3(6,8) = 0.5937358140945435d0
      model4_w3(6,9) = 0.6414343118667603d0
      model4_w3(7,1) = 0.3419102132320404d0
      model4_w3(7,2) = 0.2069520503282547d0
      model4_w3(7,3) = 0.26051265001296997d0
      model4_w3(7,4) = 0.5702694058418274d0
      model4_w3(7,5) = 0.14810998737812042d0
      model4_w3(7,6) = -0.8348793387413025d0
      model4_w3(7,7) = 0.7819703817367554d0
      model4_w3(7,8) = 0.36292967200279236d0
      model4_w3(7,9) = 0.39077022671699524d0
      model4_w3(8,1) = 0.687034010887146d0
      model4_w3(8,2) = 0.3525262475013733d0
      model4_w3(8,3) = 0.8431726098060608d0
      model4_w3(8,4) = 0.810204803943634d0
      model4_w3(8,5) = 0.46447184681892395d0
      model4_w3(8,6) = -0.31294897198677063d0
      model4_w3(8,7) = 0.4006775915622711d0
      model4_w3(8,8) = 0.307453453540802d0
      model4_w3(8,9) = 0.79095458984375d0
      model4_w3(9,1) = 0.014509771950542927d0
      model4_w3(9,2) = 0.10443583130836487d0
      model4_w3(9,3) = 0.3721143901348114d0
      model4_w3(9,4) = 0.19670961797237396d0
      model4_w3(9,5) = -0.07030884921550751d0
      model4_w3(9,6) = 0.5845282673835754d0
      model4_w3(9,7) = -0.2127482295036316d0
      model4_w3(9,8) = -0.3481827676296234d0
      model4_w3(9,9) = -0.16526246070861816d0
! model4_hidden_layers.1.bias
      model4_b3(1,1) = 0.0103698018938303d0
      model4_b3(1,2) = 0.5817030072212219d0
      model4_b3(1,3) = -0.0756816416978836d0
      model4_b3(1,4) = 0.20533402264118195d0
      model4_b3(1,5) = 0.38643863797187805d0
      model4_b3(1,6) = -0.00842411071062088d0
      model4_b3(1,7) = 0.23793746531009674d0
      model4_b3(1,8) = 0.3260308802127838d0
      model4_b3(1,9) = 0.25973787903785706d0
! model4_hidden_layers.2.weight
      model4_w4(1,1) = 0.6093971133232117d0
      model4_w4(1,2) = -0.5578951835632324d0
      model4_w4(1,3) = 0.634300947189331d0
      model4_w4(1,4) = 0.7489154934883118d0
      model4_w4(1,5) = -0.42787685990333557d0
      model4_w4(1,6) = 0.6777766942977905d0
      model4_w4(1,7) = 0.5675849318504333d0
      model4_w4(1,8) = 0.6860774755477905d0
      model4_w4(1,9) = -0.35131388902664185d0
      model4_w4(2,1) = -0.3555746376514435d0
      model4_w4(2,2) = -0.17842312157154083d0
      model4_w4(2,3) = -0.20563986897468567d0
      model4_w4(2,4) = -0.0875663235783577d0
      model4_w4(2,5) = 0.2107689529657364d0
      model4_w4(2,6) = 0.269588440656662d0
      model4_w4(2,7) = 0.1887652426958084d0
      model4_w4(2,8) = -0.25363293290138245d0
      model4_w4(2,9) = -0.1505199819803238d0
      model4_w4(3,1) = 0.24538980424404144d0
      model4_w4(3,2) = -0.6663480997085571d0
      model4_w4(3,3) = 0.6375658512115479d0
      model4_w4(3,4) = 0.7161962389945984d0
      model4_w4(3,5) = -0.5178898572921753d0
      model4_w4(3,6) = 0.40172967314720154d0
      model4_w4(3,7) = 0.8246991634368896d0
      model4_w4(3,8) = 0.5585984587669373d0
      model4_w4(3,9) = -0.27883636951446533d0
      model4_w4(4,1) = -0.12845340371131897d0
      model4_w4(4,2) = 0.6233738660812378d0
      model4_w4(4,3) = -0.3488064110279083d0
      model4_w4(4,4) = 0.33993998169898987d0
      model4_w4(4,5) = 0.7755700945854187d0
      model4_w4(4,6) = -0.24080441892147064d0
      model4_w4(4,7) = -0.28162944316864014d0
      model4_w4(4,8) = 0.1499175876379013d0
      model4_w4(4,9) = 0.4846312403678894d0
      model4_w4(5,1) = -0.325056791305542d0
      model4_w4(5,2) = -0.0684104859828949d0
      model4_w4(5,3) = -0.2940966486930847d0
      model4_w4(5,4) = -0.24211657047271729d0
      model4_w4(5,5) = -0.22999370098114014d0
      model4_w4(5,6) = 0.05757594108581543d0
      model4_w4(5,7) = -0.13470828533172607d0
      model4_w4(5,8) = 0.05214682221412659d0
      model4_w4(5,9) = 0.14040574431419373d0
      model4_w4(6,1) = -0.2872956097126007d0
      model4_w4(6,2) = -0.14549997448921204d0
      model4_w4(6,3) = -0.2260170876979828d0
      model4_w4(6,4) = 0.26425936818122864d0
      model4_w4(6,5) = 0.19454249739646912d0
      model4_w4(6,6) = -0.1424322873353958d0
      model4_w4(6,7) = 0.0043330141343176365d0
      model4_w4(6,8) = -0.13124962151050568d0
      model4_w4(6,9) = 0.01632734201848507d0
      model4_w4(7,1) = 0.26172932982444763d0
      model4_w4(7,2) = -0.3044966459274292d0
      model4_w4(7,3) = 0.07550826668739319d0
      model4_w4(7,4) = -0.028328537940979004d0
      model4_w4(7,5) = -0.24905554950237274d0
      model4_w4(7,6) = -0.2158539742231369d0
      model4_w4(7,7) = -0.27774497866630554d0
      model4_w4(7,8) = -0.3072821795940399d0
      model4_w4(7,9) = -0.12823009490966797d0
      model4_w4(8,1) = -0.14879612624645233d0
      model4_w4(8,2) = -0.3233727812767029d0
      model4_w4(8,3) = -0.2952159345149994d0
      model4_w4(8,4) = -0.1625312864780426d0
      model4_w4(8,5) = -0.154661163687706d0
      model4_w4(8,6) = -0.06643752008676529d0
      model4_w4(8,7) = -0.3092191219329834d0
      model4_w4(8,8) = -0.2223859280347824d0
      model4_w4(8,9) = -0.21640825271606445d0
      model4_w4(9,1) = -0.3602433204650879d0
      model4_w4(9,2) = 0.646629810333252d0
      model4_w4(9,3) = -0.12354647368192673d0
      model4_w4(9,4) = -0.12629927694797516d0
      model4_w4(9,5) = 0.695651113986969d0
      model4_w4(9,6) = -0.08159241825342178d0
      model4_w4(9,7) = 0.09108006954193115d0
      model4_w4(9,8) = 0.3540121912956238d0
      model4_w4(9,9) = 0.6867257356643677d0
! model4_hidden_layers.2.bias
      model4_b4(1,1) = -0.07974138855934143d0
      model4_b4(1,2) = 0.2449323982000351d0
      model4_b4(1,3) = 0.19265545904636383d0
      model4_b4(1,4) = 0.42144760489463806d0
      model4_b4(1,5) = -0.2547958493232727d0
      model4_b4(1,6) = -0.2791686952114105d0
      model4_b4(1,7) = -0.24539276957511902d0
      model4_b4(1,8) = 0.2731195092201233d0
      model4_b4(1,9) = 0.5896056890487671d0
! model4_hidden_layers.3.weight
      model4_w5(1,1) = -0.09536468982696533d0
      model4_w5(1,2) = -0.14297816157341003d0
      model4_w5(1,3) = 0.019423574209213257d0
      model4_w5(1,4) = 0.04060542583465576d0
      model4_w5(1,5) = 0.09154510498046875d0
      model4_w5(1,6) = 0.05090650916099548d0
      model4_w5(1,7) = -0.22523348033428192d0
      model4_w5(1,8) = -0.30211326479911804d0
      model4_w5(1,9) = -0.06811964511871338d0
      model4_w5(2,1) = 0.24827948212623596d0
      model4_w5(2,2) = -0.11264900118112564d0
      model4_w5(2,3) = 0.34054315090179443d0
      model4_w5(2,4) = -0.39171046018600464d0
      model4_w5(2,5) = -0.29136812686920166d0
      model4_w5(2,6) = -0.17884790897369385d0
      model4_w5(2,7) = 0.0812821090221405d0
      model4_w5(2,8) = 0.08617768436670303d0
      model4_w5(2,9) = -0.2820221483707428d0
      model4_w5(3,1) = 0.5188508629798889d0
      model4_w5(3,2) = -0.06352877616882324d0
      model4_w5(3,3) = 0.5763006806373596d0
      model4_w5(3,4) = -0.669622540473938d0
      model4_w5(3,5) = -0.31807905435562134d0
      model4_w5(3,6) = -0.017383337020874023d0
      model4_w5(3,7) = -0.14864611625671387d0
      model4_w5(3,8) = -0.04279613494873047d0
      model4_w5(3,9) = -0.6477779746055603d0
      model4_w5(4,1) = -0.044722557067871094d0
      model4_w5(4,2) = -0.04549410939216614d0
      model4_w5(4,3) = -0.10223345458507538d0
      model4_w5(4,4) = -0.19408583641052246d0
      model4_w5(4,5) = 0.2181946337223053d0
      model4_w5(4,6) = 0.3310665786266327d0
      model4_w5(4,7) = -0.27826616168022156d0
      model4_w5(4,8) = 0.05108889937400818d0
      model4_w5(4,9) = 0.09596690535545349d0
      model4_w5(5,1) = -0.12474589794874191d0
      model4_w5(5,2) = 0.09386888146400452d0
      model4_w5(5,3) = 0.007568416651338339d0
      model4_w5(5,4) = 0.1847088634967804d0
      model4_w5(5,5) = -0.1791924238204956d0
      model4_w5(5,6) = -0.10105668753385544d0
      model4_w5(5,7) = 0.14168334007263184d0
      model4_w5(5,8) = -0.06868946552276611d0
      model4_w5(5,9) = 0.6174142360687256d0
      model4_w5(6,1) = -0.24454160034656525d0
      model4_w5(6,2) = 0.06819398701190948d0
      model4_w5(6,3) = 0.18396244943141937d0
      model4_w5(6,4) = -0.3424403667449951d0
      model4_w5(6,5) = -0.0046551525592803955d0
      model4_w5(6,6) = 0.06552699208259583d0
      model4_w5(6,7) = -0.08983442187309265d0
      model4_w5(6,8) = 0.2515677511692047d0
      model4_w5(6,9) = 0.24908460676670074d0
      model4_w5(7,1) = 0.013093413785099983d0
      model4_w5(7,2) = 0.26788926124572754d0
      model4_w5(7,3) = -0.054692164063453674d0
      model4_w5(7,4) = 0.724250316619873d0
      model4_w5(7,5) = 0.022736310958862305d0
      model4_w5(7,6) = 0.28972771763801575d0
      model4_w5(7,7) = 0.235672265291214d0
      model4_w5(7,8) = 0.22894011437892914d0
      model4_w5(7,9) = 0.8385338187217712d0
      model4_w5(8,1) = -0.10744957625865936d0
      model4_w5(8,2) = 0.004915356636047363d0
      model4_w5(8,3) = -0.13802079856395721d0
      model4_w5(8,4) = -0.1777881383895874d0
      model4_w5(8,5) = 0.3119035065174103d0
      model4_w5(8,6) = 0.11675691604614258d0
      model4_w5(8,7) = 0.2760373651981354d0
      model4_w5(8,8) = 0.1353723108768463d0
      model4_w5(8,9) = -0.020510196685791016d0
      model4_w5(9,1) = -0.2140336036682129d0
      model4_w5(9,2) = 0.29342636466026306d0
      model4_w5(9,3) = 0.23990574479103088d0
      model4_w5(9,4) = -0.12415409088134766d0
      model4_w5(9,5) = 0.26977208256721497d0
      model4_w5(9,6) = 0.1965889036655426d0
      model4_w5(9,7) = 0.041721075773239136d0
      model4_w5(9,8) = 0.016023844480514526d0
      model4_w5(9,9) = -0.0190579891204834d0
! model4_hidden_layers.3.bias
      model4_b5(1,1) = -0.0024415552616119385d0
      model4_b5(1,2) = -0.16703686118125916d0
      model4_b5(1,3) = 0.08411518484354019d0
      model4_b5(1,4) = -0.29705938696861267d0
      model4_b5(1,5) = 0.05973438546061516d0
      model4_b5(1,6) = 0.13420328497886658d0
      model4_b5(1,7) = 0.5326845645904541d0
      model4_b5(1,8) = -0.17058798670768738d0
      model4_b5(1,9) = -0.05962911248207092d0
! model4_hidden_layers.4.weight
      model4_w6(1,1) = -0.20340077579021454d0
      model4_w6(1,2) = -0.30404409766197205d0
      model4_w6(1,3) = 0.15801948308944702d0
      model4_w6(1,4) = -0.1794157475233078d0
      model4_w6(1,5) = -0.48230189085006714d0
      model4_w6(1,6) = -0.12157391756772995d0
      model4_w6(1,7) = 0.6281740665435791d0
      model4_w6(1,8) = -0.2753894329071045d0
      model4_w6(1,9) = 0.09275361895561218d0
      model4_w6(2,1) = 0.1584111452102661d0
      model4_w6(2,2) = -0.20496800541877747d0
      model4_w6(2,3) = 0.09236955642700195d0
      model4_w6(2,4) = -0.023619800806045532d0
      model4_w6(2,5) = 0.026875406503677368d0
      model4_w6(2,6) = 0.0033768117427825928d0
      model4_w6(2,7) = -0.14217500388622284d0
      model4_w6(2,8) = -0.12115487456321716d0
      model4_w6(2,9) = -0.1828862875699997d0
      model4_w6(3,1) = -0.1698542833328247d0
      model4_w6(3,2) = -0.13552993535995483d0
      model4_w6(3,3) = -0.35236597061157227d0
      model4_w6(3,4) = -0.20668546855449677d0
      model4_w6(3,5) = 0.2545846104621887d0
      model4_w6(3,6) = -0.33236390352249146d0
      model4_w6(3,7) = 0.09999337792396545d0
      model4_w6(3,8) = -0.24946586787700653d0
      model4_w6(3,9) = 0.24637261033058167d0
      model4_w6(4,1) = -0.05426192283630371d0
      model4_w6(4,2) = -0.3644525408744812d0
      model4_w6(4,3) = 0.07619959861040115d0
      model4_w6(4,4) = 0.09293782711029053d0
      model4_w6(4,5) = 0.06794962286949158d0
      model4_w6(4,6) = 0.2902143895626068d0
      model4_w6(4,7) = -0.16127318143844604d0
      model4_w6(4,8) = 0.3095981776714325d0
      model4_w6(4,9) = -0.3299698531627655d0
      model4_w6(5,1) = -0.23420695960521698d0
      model4_w6(5,2) = -0.2245502918958664d0
      model4_w6(5,3) = 0.023989081382751465d0
      model4_w6(5,4) = -0.19728998839855194d0
      model4_w6(5,5) = 0.12883010506629944d0
      model4_w6(5,6) = 0.09759056568145752d0
      model4_w6(5,7) = -0.30467963218688965d0
      model4_w6(5,8) = 0.001759648323059082d0
      model4_w6(5,9) = -0.30763351917266846d0
      model4_w6(6,1) = -0.3286707401275635d0
      model4_w6(6,2) = 0.19731739163398743d0
      model4_w6(6,3) = -0.2615475058555603d0
      model4_w6(6,4) = 0.06507542729377747d0
      model4_w6(6,5) = 0.5027832984924316d0
      model4_w6(6,6) = 0.2877883315086365d0
      model4_w6(6,7) = 0.4787794053554535d0
      model4_w6(6,8) = -0.03648543357849121d0
      model4_w6(6,9) = 0.029381901025772095d0
      model4_w6(7,1) = -0.3283396363258362d0
      model4_w6(7,2) = -0.33055269718170166d0
      model4_w6(7,3) = 0.011852771043777466d0
      model4_w6(7,4) = -0.08772337436676025d0
      model4_w6(7,5) = -0.09078697860240936d0
      model4_w6(7,6) = 0.26800549030303955d0
      model4_w6(7,7) = 0.5110636353492737d0
      model4_w6(7,8) = -0.32528504729270935d0
      model4_w6(7,9) = -0.0386219322681427d0
      model4_w6(8,1) = -0.01315605640411377d0
      model4_w6(8,2) = 0.3261570334434509d0
      model4_w6(8,3) = 0.48799043893814087d0
      model4_w6(8,4) = 0.3252216875553131d0
      model4_w6(8,5) = 0.6192241907119751d0
      model4_w6(8,6) = -0.19695138931274414d0
      model4_w6(8,7) = -0.2966685891151428d0
      model4_w6(8,8) = -0.04986456036567688d0
      model4_w6(8,9) = -0.08708414435386658d0
      model4_w6(9,1) = -0.21006903052330017d0
      model4_w6(9,2) = 0.22378602623939514d0
      model4_w6(9,3) = 0.8483606576919556d0
      model4_w6(9,4) = -0.01825961470603943d0
      model4_w6(9,5) = 0.5979917049407959d0
      model4_w6(9,6) = 0.22492562234401703d0
      model4_w6(9,7) = -0.45036518573760986d0
      model4_w6(9,8) = 0.03890618681907654d0
      model4_w6(9,9) = -0.16619515419006348d0
! model4_hidden_layers.4.bias
      model4_b6(1,1) = 0.3647362291812897d0
      model4_b6(1,2) = -0.38789549469947815d0
      model4_b6(1,3) = -0.018529869616031647d0
      model4_b6(1,4) = 0.22403296828269958d0
      model4_b6(1,5) = -0.2912343740463257d0
      model4_b6(1,6) = 0.24266117811203003d0
      model4_b6(1,7) = 0.30341866612434387d0
      model4_b6(1,8) = -0.33897048234939575d0
      model4_b6(1,9) = 0.2026893049478531d0
! model4_output_layer.weight
      model4_w7(1,1) = -0.8167872428894043d0
      model4_w7(1,2) = -0.08391272276639938d0
      model4_w7(1,3) = -0.3104633390903473d0
      model4_w7(1,4) = -0.15429489314556122d0
      model4_w7(1,5) = -0.09895941615104675d0
      model4_w7(1,6) = 0.2874618172645569d0
      model4_w7(1,7) = 0.4171495735645294d0
      model4_w7(1,8) = 0.40042224526405334d0
      model4_w7(1,9) = 0.6126970052719116d0
! model4_output_layer.bias
      model4_b7(1,1) = -0.07999471575021744d0

! model5_input_layer.weight
      model5_w1(1,1) = 0.4242885708808899d0
      model5_w1(1,2) = 0.3584316074848175d0
      model5_w1(1,3) = 0.522861123085022d0
      model5_w1(1,4) = -0.5624366402626038d0
      model5_w1(1,5) = 0.4177962839603424d0
      model5_w1(1,6) = -0.41035139560699463d0
      model5_w1(2,1) = -0.2039913684129715d0
      model5_w1(2,2) = 0.29806914925575256d0
      model5_w1(2,3) = -0.3989512026309967d0
      model5_w1(2,4) = -0.302354633808136d0
      model5_w1(2,5) = -0.025594741106033325d0
      model5_w1(2,6) = 0.0408175028860569d0
      model5_w1(3,1) = 0.4494200348854065d0
      model5_w1(3,2) = -0.05405983701348305d0
      model5_w1(3,3) = 0.21521931886672974d0
      model5_w1(3,4) = -0.29707252979278564d0
      model5_w1(3,5) = 0.7478185892105103d0
      model5_w1(3,6) = -0.5671245455741882d0
      model5_w1(4,1) = 0.20599299669265747d0
      model5_w1(4,2) = 0.1731065958738327d0
      model5_w1(4,3) = -0.0996914729475975d0
      model5_w1(4,4) = -0.05498485267162323d0
      model5_w1(4,5) = -0.32587382197380066d0
      model5_w1(4,6) = 0.1796244978904724d0
      model5_w1(5,1) = 0.6301352977752686d0
      model5_w1(5,2) = -0.06686397641897202d0
      model5_w1(5,3) = 0.11431778222322464d0
      model5_w1(5,4) = -0.16238439083099365d0
      model5_w1(5,5) = 0.27715522050857544d0
      model5_w1(5,6) = -0.09754864126443863d0
      model5_w1(6,1) = -0.29087820649147034d0
      model5_w1(6,2) = 0.7413313388824463d0
      model5_w1(6,3) = 0.4797602891921997d0
      model5_w1(6,4) = 0.38729533553123474d0
      model5_w1(6,5) = -0.42959919571876526d0
      model5_w1(6,6) = -0.06617365777492523d0
      model5_w1(7,1) = 0.3004732131958008d0
      model5_w1(7,2) = -0.0526486299932003d0
      model5_w1(7,3) = 0.257258802652359d0
      model5_w1(7,4) = 0.6315434575080872d0
      model5_w1(7,5) = -0.620575487613678d0
      model5_w1(7,6) = 0.1471516191959381d0
      model5_w1(8,1) = 0.4017349183559418d0
      model5_w1(8,2) = 0.33310768008232117d0
      model5_w1(8,3) = -0.02710522711277008d0
      model5_w1(8,4) = 0.06415620446205139d0
      model5_w1(8,5) = -0.6283280253410339d0
      model5_w1(8,6) = 0.3104732036590576d0
      model5_w1(9,1) = 0.3074929118156433d0
      model5_w1(9,2) = 0.24969933927059174d0
      model5_w1(9,3) = 0.1621105819940567d0
      model5_w1(9,4) = -0.04403934255242348d0
      model5_w1(9,5) = 0.6085886359214783d0
      model5_w1(9,6) = -0.6641579866409302d0
! model5_input_layer.bias
      model5_b1(1,1) = 0.0601649135351181d0
      model5_b1(1,2) = -0.23559732735157013d0
      model5_b1(1,3) = 0.12969015538692474d0
      model5_b1(1,4) = 0.31881415843963623d0
      model5_b1(1,5) = 0.2106630653142929d0
      model5_b1(1,6) = 0.1542726308107376d0
      model5_b1(1,7) = 0.25198185443878174d0
      model5_b1(1,8) = 0.17190133035182953d0
      model5_b1(1,9) = -0.3275788128376007d0
! model5_hidden_layers.0.weight
      model5_w2(1,1) = 0.3626662790775299d0
      model5_w2(1,2) = 0.18329259753227234d0
      model5_w2(1,3) = 0.12198039889335632d0
      model5_w2(1,4) = -0.20347867906093597d0
      model5_w2(1,5) = 0.06681659072637558d0
      model5_w2(1,6) = -0.8126519322395325d0
      model5_w2(1,7) = -0.6781520843505859d0
      model5_w2(1,8) = -0.7301762700080872d0
      model5_w2(1,9) = 0.44621628522872925d0
      model5_w2(2,1) = 0.38638272881507874d0
      model5_w2(2,2) = 0.23738180100917816d0
      model5_w2(2,3) = 0.4334368109703064d0
      model5_w2(2,4) = 0.08846084028482437d0
      model5_w2(2,5) = 0.19534581899642944d0
      model5_w2(2,6) = 0.6590884923934937d0
      model5_w2(2,7) = 0.6023769378662109d0
      model5_w2(2,8) = 0.6187342405319214d0
      model5_w2(2,9) = 0.4767761528491974d0
      model5_w2(3,1) = -0.0019188225269317627d0
      model5_w2(3,2) = -0.3329692780971527d0
      model5_w2(3,3) = 0.5914719700813293d0
      model5_w2(3,4) = -0.5967652201652527d0
      model5_w2(3,5) = 0.09122776985168457d0
      model5_w2(3,6) = -0.8585219979286194d0
      model5_w2(3,7) = -0.6264269351959229d0
      model5_w2(3,8) = -0.22843694686889648d0
      model5_w2(3,9) = 0.40405359864234924d0
      model5_w2(4,1) = -0.15874747931957245d0
      model5_w2(4,2) = -0.11724305152893066d0
      model5_w2(4,3) = -0.2647243142127991d0
      model5_w2(4,4) = 0.20380273461341858d0
      model5_w2(4,5) = -0.08423559367656708d0
      model5_w2(4,6) = -0.260648250579834d0
      model5_w2(4,7) = 0.05142712593078613d0
      model5_w2(4,8) = -0.053297996520996094d0
      model5_w2(4,9) = -0.08574692904949188d0
      model5_w2(5,1) = 0.071879081428051d0
      model5_w2(5,2) = -0.21413369476795197d0
      model5_w2(5,3) = 0.09023687988519669d0
      model5_w2(5,4) = 0.5616200566291809d0
      model5_w2(5,5) = 0.0790330022573471d0
      model5_w2(5,6) = 0.5119626522064209d0
      model5_w2(5,7) = 0.4863605797290802d0
      model5_w2(5,8) = 0.8163169622421265d0
      model5_w2(5,9) = -0.46534058451652527d0
      model5_w2(6,1) = -0.1775638908147812d0
      model5_w2(6,2) = -0.15477392077445984d0
      model5_w2(6,3) = -0.23530052602291107d0
      model5_w2(6,4) = 0.6791645288467407d0
      model5_w2(6,5) = 0.2805747091770172d0
      model5_w2(6,6) = 0.5279445648193359d0
      model5_w2(6,7) = 0.5045602321624756d0
      model5_w2(6,8) = 0.6231765151023865d0
      model5_w2(6,9) = 0.0801273062825203d0
      model5_w2(7,1) = -0.23428142070770264d0
      model5_w2(7,2) = -0.12264668941497803d0
      model5_w2(7,3) = -0.18804171681404114d0
      model5_w2(7,4) = 0.15582150220870972d0
      model5_w2(7,5) = 0.22573159635066986d0
      model5_w2(7,6) = -0.309600830078125d0
      model5_w2(7,7) = -0.2872602641582489d0
      model5_w2(7,8) = 0.29140162467956543d0
      model5_w2(7,9) = -0.012503045611083508d0
      model5_w2(8,1) = 0.43093186616897583d0
      model5_w2(8,2) = 0.06694608181715012d0
      model5_w2(8,3) = 0.36540108919143677d0
      model5_w2(8,4) = -0.27031904458999634d0
      model5_w2(8,5) = 0.2782596945762634d0
      model5_w2(8,6) = -0.5080631971359253d0
      model5_w2(8,7) = -0.6721565127372742d0
      model5_w2(8,8) = -0.7223788499832153d0
      model5_w2(8,9) = 0.5381459593772888d0
      model5_w2(9,1) = 0.5527902245521545d0
      model5_w2(9,2) = 0.12529340386390686d0
      model5_w2(9,3) = 0.37534040212631226d0
      model5_w2(9,4) = -0.40821176767349243d0
      model5_w2(9,5) = 0.18326513469219208d0
      model5_w2(9,6) = -0.3201732337474823d0
      model5_w2(9,7) = -0.2464556246995926d0
      model5_w2(9,8) = -0.3875516951084137d0
      model5_w2(9,9) = 0.4274437725543976d0
! model5_hidden_layers.0.bias
      model5_b2(1,1) = 0.01848011277616024d0
      model5_b2(1,2) = 0.5098156332969666d0
      model5_b2(1,3) = 0.010795618407428265d0
      model5_b2(1,4) = -0.051767498254776d0
      model5_b2(1,5) = 0.45725688338279724d0
      model5_b2(1,6) = 0.5646161437034607d0
      model5_b2(1,7) = -0.07681692391633987d0
      model5_b2(1,8) = 0.1225230023264885d0
      model5_b2(1,9) = 0.0512089766561985d0
! model5_hidden_layers.1.weight
      model5_w3(1,1) = 0.625473141670227d0
      model5_w3(1,2) = 0.1809075027704239d0
      model5_w3(1,3) = 0.43290647864341736d0
      model5_w3(1,4) = 0.22234442830085754d0
      model5_w3(1,5) = -0.5953558683395386d0
      model5_w3(1,6) = -0.32490232586860657d0
      model5_w3(1,7) = 0.3070768415927887d0
      model5_w3(1,8) = 0.3921375274658203d0
      model5_w3(1,9) = -0.023203980177640915d0
      model5_w3(2,1) = 0.3951281011104584d0
      model5_w3(2,2) = -0.16102363169193268d0
      model5_w3(2,3) = 0.5496883392333984d0
      model5_w3(2,4) = 0.24486085772514343d0
      model5_w3(2,5) = -0.4344300925731659d0
      model5_w3(2,6) = -0.804053008556366d0
      model5_w3(2,7) = 0.28610819578170776d0
      model5_w3(2,8) = 0.6038145422935486d0
      model5_w3(2,9) = 0.40380120277404785d0
      model5_w3(3,1) = 0.3679112493991852d0
      model5_w3(3,2) = 0.021238861605525017d0
      model5_w3(3,3) = 0.609222412109375d0
      model5_w3(3,4) = -0.0753885805606842d0
      model5_w3(3,5) = -0.09882596135139465d0
      model5_w3(3,6) = -0.5287606120109558d0
      model5_w3(3,7) = -0.21924519538879395d0
      model5_w3(3,8) = 0.38394781947135925d0
      model5_w3(3,9) = 0.22302104532718658d0
      model5_w3(4,1) = 0.13103647530078888d0
      model5_w3(4,2) = 0.1564558446407318d0
      model5_w3(4,3) = 0.28128257393836975d0
      model5_w3(4,4) = 0.27843979001045227d0
      model5_w3(4,5) = -0.16315299272537231d0
      model5_w3(4,6) = -0.3122870922088623d0
      model5_w3(4,7) = -0.3231234550476074d0
      model5_w3(4,8) = 0.6892897486686707d0
      model5_w3(4,9) = 0.3944357931613922d0
      model5_w3(5,1) = 0.1917044222354889d0
      model5_w3(5,2) = -0.0798109769821167d0
      model5_w3(5,3) = 0.1706758439540863d0
      model5_w3(5,4) = 0.17711177468299866d0
      model5_w3(5,5) = -0.32419145107269287d0
      model5_w3(5,6) = -0.025892943143844604d0
      model5_w3(5,7) = 0.2540319263935089d0
      model5_w3(5,8) = -0.22673377394676208d0
      model5_w3(5,9) = -0.311093270778656d0
      model5_w3(6,1) = 0.45826196670532227d0
      model5_w3(6,2) = 0.04113362357020378d0
      model5_w3(6,3) = 0.6823562979698181d0
      model5_w3(6,4) = 0.04728415608406067d0
      model5_w3(6,5) = -0.5433330535888672d0
      model5_w3(6,6) = -0.6361510753631592d0
      model5_w3(6,7) = 0.04975765943527222d0
      model5_w3(6,8) = 0.6280995011329651d0
      model5_w3(6,9) = 0.3319494426250458d0
      model5_w3(7,1) = -0.4262442886829376d0
      model5_w3(7,2) = 0.5450958609580994d0
      model5_w3(7,3) = -0.19388447701931d0
      model5_w3(7,4) = -0.21670162677764893d0
      model5_w3(7,5) = 0.5262796878814697d0
      model5_w3(7,6) = 0.3999119699001312d0
      model5_w3(7,7) = 0.1774720400571823d0
      model5_w3(7,8) = -0.33406761288642883d0
      model5_w3(7,9) = 0.15145516395568848d0
      model5_w3(8,1) = 0.19527313113212585d0
      model5_w3(8,2) = -0.29242783784866333d0
      model5_w3(8,3) = 0.11838409304618835d0
      model5_w3(8,4) = -0.28340381383895874d0
      model5_w3(8,5) = -0.0052804648876190186d0
      model5_w3(8,6) = 0.09637901186943054d0
      model5_w3(8,7) = 0.23128744959831238d0
      model5_w3(8,8) = -0.30396708846092224d0
      model5_w3(8,9) = 0.19598552584648132d0
      model5_w3(9,1) = 0.7432720065116882d0
      model5_w3(9,2) = 0.4408818781375885d0
      model5_w3(9,3) = 0.10363446176052094d0
      model5_w3(9,4) = 0.2888762056827545d0
      model5_w3(9,5) = -0.39015501737594604d0
      model5_w3(9,6) = -0.5386423468589783d0
      model5_w3(9,7) = 0.22721286118030548d0
      model5_w3(9,8) = 0.6634103059768677d0
      model5_w3(9,9) = 0.47709769010543823d0
! model5_hidden_layers.1.bias
      model5_b3(1,1) = -0.3472803235054016d0
      model5_b3(1,2) = 0.31000959873199463d0
      model5_b3(1,3) = -0.14877480268478394d0
      model5_b3(1,4) = 0.29638680815696716d0
      model5_b3(1,5) = -0.21258744597434998d0
      model5_b3(1,6) = 0.2979280352592468d0
      model5_b3(1,7) = 0.3072584569454193d0
      model5_b3(1,8) = 0.04220843315124512d0
      model5_b3(1,9) = -0.09438024461269379d0
! model5_hidden_layers.2.weight
      model5_w4(1,1) = -0.2717766761779785d0
      model5_w4(1,2) = -0.12104968726634979d0
      model5_w4(1,3) = -0.27220624685287476d0
      model5_w4(1,4) = -0.6100464463233948d0
      model5_w4(1,5) = -0.0363941490650177d0
      model5_w4(1,6) = 0.08458130806684494d0
      model5_w4(1,7) = 0.7211542129516602d0
      model5_w4(1,8) = 0.23590520024299622d0
      model5_w4(1,9) = -0.6614614129066467d0
      model5_w4(2,1) = 0.1960015743970871d0
      model5_w4(2,2) = 0.2803735136985779d0
      model5_w4(2,3) = 0.23127391934394836d0
      model5_w4(2,4) = 0.44367504119873047d0
      model5_w4(2,5) = 0.017117470502853394d0
      model5_w4(2,6) = 0.6643661260604858d0
      model5_w4(2,7) = -0.4750024378299713d0
      model5_w4(2,8) = 0.20765510201454163d0
      model5_w4(2,9) = 0.1624891608953476d0
      model5_w4(3,1) = 0.35830673575401306d0
      model5_w4(3,2) = 0.7133585810661316d0
      model5_w4(3,3) = 0.2822917103767395d0
      model5_w4(3,4) = 0.262275755405426d0
      model5_w4(3,5) = -0.08551101386547089d0
      model5_w4(3,6) = 0.39395979046821594d0
      model5_w4(3,7) = -0.786716103553772d0
      model5_w4(3,8) = 0.26290860772132874d0
      model5_w4(3,9) = 0.5313305258750916d0
      model5_w4(4,1) = -0.04996892064809799d0
      model5_w4(4,2) = -0.4206244647502899d0
      model5_w4(4,3) = -0.3715575635433197d0
      model5_w4(4,4) = 0.32038015127182007d0
      model5_w4(4,5) = -0.23850969970226288d0
      model5_w4(4,6) = -0.11400189995765686d0
      model5_w4(4,7) = 0.8215169310569763d0
      model5_w4(4,8) = 0.06801068782806396d0
      model5_w4(4,9) = 0.3213222622871399d0
      model5_w4(5,1) = 0.5282498002052307d0
      model5_w4(5,2) = 0.6550719141960144d0
      model5_w4(5,3) = 0.2596127390861511d0
      model5_w4(5,4) = 0.4259531795978546d0
      model5_w4(5,5) = 0.12441703677177429d0
      model5_w4(5,6) = 0.6049628853797913d0
      model5_w4(5,7) = -0.18437308073043823d0
      model5_w4(5,8) = -0.04721030592918396d0
      model5_w4(5,9) = 0.24354618787765503d0
      model5_w4(6,1) = 0.013581722043454647d0
      model5_w4(6,2) = 0.7144235968589783d0
      model5_w4(6,3) = 0.6236740350723267d0
      model5_w4(6,4) = 0.38085076212882996d0
      model5_w4(6,5) = -0.1650555580854416d0
      model5_w4(6,6) = 0.568325936794281d0
      model5_w4(6,7) = -0.4659205377101898d0
      model5_w4(6,8) = -0.3047650456428528d0
      model5_w4(6,9) = 0.527368426322937d0
      model5_w4(7,1) = 0.47120922803878784d0
      model5_w4(7,2) = 0.3311869502067566d0
      model5_w4(7,3) = 0.6096357107162476d0
      model5_w4(7,4) = 0.030815446749329567d0
      model5_w4(7,5) = -0.041823893785476685d0
      model5_w4(7,6) = 0.45781320333480835d0
      model5_w4(7,7) = -0.339354544878006d0
      model5_w4(7,8) = -0.017278462648391724d0
      model5_w4(7,9) = 0.15134741365909576d0
      model5_w4(8,1) = 0.015246739611029625d0
      model5_w4(8,2) = 0.7201414704322815d0
      model5_w4(8,3) = 0.4358130097389221d0
      model5_w4(8,4) = 0.5607863664627075d0
      model5_w4(8,5) = -0.2845267057418823d0
      model5_w4(8,6) = 0.6665984988212585d0
      model5_w4(8,7) = -0.5450543761253357d0
      model5_w4(8,8) = -0.3302578926086426d0
      model5_w4(8,9) = 0.39062005281448364d0
      model5_w4(9,1) = 0.6080361008644104d0
      model5_w4(9,2) = 0.6169742941856384d0
      model5_w4(9,3) = 0.15999352931976318d0
      model5_w4(9,4) = 0.3230835199356079d0
      model5_w4(9,5) = -0.20109987258911133d0
      model5_w4(9,6) = 0.3695278763771057d0
      model5_w4(9,7) = -0.6928098797798157d0
      model5_w4(9,8) = 0.2938176691532135d0
      model5_w4(9,9) = 0.14227116107940674d0
! model5_hidden_layers.2.bias
      model5_b4(1,1) = 0.08634112775325775d0
      model5_b4(1,2) = -0.27702128887176514d0
      model5_b4(1,3) = 0.1321808397769928d0
      model5_b4(1,4) = 0.370273232460022d0
      model5_b4(1,5) = 0.40786418318748474d0
      model5_b4(1,6) = 0.2868974804878235d0
      model5_b4(1,7) = -0.1580931544303894d0
      model5_b4(1,8) = 0.2656031847000122d0
      model5_b4(1,9) = 0.2973078191280365d0
! model5_hidden_layers.3.weight
      model5_w5(1,1) = 1.0744128227233887d0
      model5_w5(1,2) = -0.12463115900754929d0
      model5_w5(1,3) = -0.07775726169347763d0
      model5_w5(1,4) = 0.6500926613807678d0
      model5_w5(1,5) = -0.014801658689975739d0
      model5_w5(1,6) = 0.023753875866532326d0
      model5_w5(1,7) = 0.04325450584292412d0
      model5_w5(1,8) = 0.024851972237229347d0
      model5_w5(1,9) = 0.07718430459499359d0
      model5_w5(2,1) = -0.29517167806625366d0
      model5_w5(2,2) = -0.30937469005584717d0
      model5_w5(2,3) = -0.21502093970775604d0
      model5_w5(2,4) = -0.31069228053092957d0
      model5_w5(2,5) = -0.10763494670391083d0
      model5_w5(2,6) = -0.044384926557540894d0
      model5_w5(2,7) = -0.1730736494064331d0
      model5_w5(2,8) = 0.2510797679424286d0
      model5_w5(2,9) = -0.12514269351959229d0
      model5_w5(3,1) = -0.02959647960960865d0
      model5_w5(3,2) = 0.3229130804538727d0
      model5_w5(3,3) = 0.4294269382953644d0
      model5_w5(3,4) = -0.3374614119529724d0
      model5_w5(3,5) = 0.14372165501117706d0
      model5_w5(3,6) = 0.39277705550193787d0
      model5_w5(3,7) = 0.12304971367120743d0
      model5_w5(3,8) = 0.37814924120903015d0
      model5_w5(3,9) = 0.49238651990890503d0
      model5_w5(4,1) = 0.11883561313152313d0
      model5_w5(4,2) = 0.14732642471790314d0
      model5_w5(4,3) = 0.33772143721580505d0
      model5_w5(4,4) = -0.43882229924201965d0
      model5_w5(4,5) = 0.5676984190940857d0
      model5_w5(4,6) = 0.5887876749038696d0
      model5_w5(4,7) = 0.16176025569438934d0
      model5_w5(4,8) = 0.08318810164928436d0
      model5_w5(4,9) = 0.13766421377658844d0
      model5_w5(5,1) = -0.7915964722633362d0
      model5_w5(5,2) = -0.3292910158634186d0
      model5_w5(5,3) = -0.1622139811515808d0
      model5_w5(5,4) = 0.6492807269096375d0
      model5_w5(5,5) = 0.2853807210922241d0
      model5_w5(5,6) = 0.0664563998579979d0
      model5_w5(5,7) = -0.09716614335775375d0
      model5_w5(5,8) = 0.03653698042035103d0
      model5_w5(5,9) = 0.0727691501379013d0
      model5_w5(6,1) = 0.2042548954486847d0
      model5_w5(6,2) = 0.3537617027759552d0
      model5_w5(6,3) = 0.6081494092941284d0
      model5_w5(6,4) = -0.7800989151000977d0
      model5_w5(6,5) = 0.5001170039176941d0
      model5_w5(6,6) = 0.3197616934776306d0
      model5_w5(6,7) = 0.5367494225502014d0
      model5_w5(6,8) = -0.03884768858551979d0
      model5_w5(6,9) = 0.45480138063430786d0
      model5_w5(7,1) = -0.24916960299015045d0
      model5_w5(7,2) = 0.41730472445487976d0
      model5_w5(7,3) = 0.4449698328971863d0
      model5_w5(7,4) = -0.4926259219646454d0
      model5_w5(7,5) = 0.5184924602508545d0
      model5_w5(7,6) = 0.4336484968662262d0
      model5_w5(7,7) = 0.23465211689472198d0
      model5_w5(7,8) = 0.5974019765853882d0
      model5_w5(7,9) = 0.5480093955993652d0
      model5_w5(8,1) = 0.14333570003509521d0
      model5_w5(8,2) = 0.33843669295310974d0
      model5_w5(8,3) = 0.6085711121559143d0
      model5_w5(8,4) = -0.6480618119239807d0
      model5_w5(8,5) = 0.6246376037597656d0
      model5_w5(8,6) = 0.4984504282474518d0
      model5_w5(8,7) = 0.5253548622131348d0
      model5_w5(8,8) = 0.53496915102005d0
      model5_w5(8,9) = 0.6165830492973328d0
      model5_w5(9,1) = 0.06895947456359863d0
      model5_w5(9,2) = 0.3331514298915863d0
      model5_w5(9,3) = -0.1437343806028366d0
      model5_w5(9,4) = -0.08215978741645813d0
      model5_w5(9,5) = -0.2873877286911011d0
      model5_w5(9,6) = -0.28620725870132446d0
      model5_w5(9,7) = -0.23660366237163544d0
      model5_w5(9,8) = -0.23901140689849854d0
      model5_w5(9,9) = 0.29461589455604553d0
! model5_hidden_layers.3.bias
      model5_b5(1,1) = 0.23712290823459625d0
      model5_b5(1,2) = 0.00532221794128418d0
      model5_b5(1,3) = 0.3266080319881439d0
      model5_b5(1,4) = 0.22045135498046875d0
      model5_b5(1,5) = 0.49845579266548157d0
      model5_b5(1,6) = -0.3712059557437897d0
      model5_b5(1,7) = -0.18267858028411865d0
      model5_b5(1,8) = 0.33279645442962646d0
      model5_b5(1,9) = -0.2078828513622284d0
! model5_hidden_layers.4.weight
      model5_w6(1,1) = 0.011233210563659668d0
      model5_w6(1,2) = -0.22962121665477753d0
      model5_w6(1,3) = 0.16941192746162415d0
      model5_w6(1,4) = -0.15572556853294373d0
      model5_w6(1,5) = -0.02171263098716736d0
      model5_w6(1,6) = -0.2819802165031433d0
      model5_w6(1,7) = -0.2667381465435028d0
      model5_w6(1,8) = -0.05101075768470764d0
      model5_w6(1,9) = 0.03201010823249817d0
      model5_w6(2,1) = -0.0543917641043663d0
      model5_w6(2,2) = -0.16686806082725525d0
      model5_w6(2,3) = 0.06101361662149429d0
      model5_w6(2,4) = -0.20647786557674408d0
      model5_w6(2,5) = 0.5026329755783081d0
      model5_w6(2,6) = -0.1709558516740799d0
      model5_w6(2,7) = 0.1798316389322281d0
      model5_w6(2,8) = -0.045179564505815506d0
      model5_w6(2,9) = 0.058762311935424805d0
      model5_w6(3,1) = -0.2732916474342346d0
      model5_w6(3,2) = 0.17910471558570862d0
      model5_w6(3,3) = -0.26913556456565857d0
      model5_w6(3,4) = 0.2833549678325653d0
      model5_w6(3,5) = -0.1388435810804367d0
      model5_w6(3,6) = 0.26369380950927734d0
      model5_w6(3,7) = -0.29040414094924927d0
      model5_w6(3,8) = 0.06858546286821365d0
      model5_w6(3,9) = -0.057090550661087036d0
      model5_w6(4,1) = 0.18443740904331207d0
      model5_w6(4,2) = 0.19934657216072083d0
      model5_w6(4,3) = 0.07796991616487503d0
      model5_w6(4,4) = -0.07937179505825043d0
      model5_w6(4,5) = 0.21146683394908905d0
      model5_w6(4,6) = 0.06175227090716362d0
      model5_w6(4,7) = 0.08993437141180038d0
      model5_w6(4,8) = -0.12707313895225525d0
      model5_w6(4,9) = -0.12981939315795898d0
      model5_w6(5,1) = -0.2524815499782562d0
      model5_w6(5,2) = 0.31530526280403137d0
      model5_w6(5,3) = 0.2960653007030487d0
      model5_w6(5,4) = -0.3038463294506073d0
      model5_w6(5,5) = 0.023266291245818138d0
      model5_w6(5,6) = -0.26180046796798706d0
      model5_w6(5,7) = -0.21302112936973572d0
      model5_w6(5,8) = -0.07711505144834518d0
      model5_w6(5,9) = -0.29675909876823425d0
      model5_w6(6,1) = 0.498748242855072d0
      model5_w6(6,2) = -0.2801644206047058d0
      model5_w6(6,3) = -0.4204867482185364d0
      model5_w6(6,4) = -0.22824449837207794d0
      model5_w6(6,5) = -0.27678173780441284d0
      model5_w6(6,6) = 0.159638911485672d0
      model5_w6(6,7) = -0.16656818985939026d0
      model5_w6(6,8) = -0.4313342869281769d0
      model5_w6(6,9) = -0.15576720237731934d0
      model5_w6(7,1) = -0.5610507130622864d0
      model5_w6(7,2) = 0.17692819237709045d0
      model5_w6(7,3) = 0.6774885654449463d0
      model5_w6(7,4) = 0.6066581606864929d0
      model5_w6(7,5) = -0.5748957395553589d0
      model5_w6(7,6) = 0.6817841529846191d0
      model5_w6(7,7) = 0.3802255690097809d0
      model5_w6(7,8) = 0.6479154825210571d0
      model5_w6(7,9) = 0.21563956141471863d0
      model5_w6(8,1) = 0.42229393124580383d0
      model5_w6(8,2) = 0.10495159029960632d0
      model5_w6(8,3) = -0.1242041364312172d0
      model5_w6(8,4) = 0.00039110920624807477d0
      model5_w6(8,5) = 0.978675365447998d0
      model5_w6(8,6) = -0.08278190344572067d0
      model5_w6(8,7) = -0.08495857566595078d0
      model5_w6(8,8) = -0.0029927168507128954d0
      model5_w6(8,9) = -0.29029494524002075d0
      model5_w6(9,1) = 0.0807395726442337d0
      model5_w6(9,2) = 0.0076240599155426025d0
      model5_w6(9,3) = 0.08584414422512054d0
      model5_w6(9,4) = 0.29606935381889343d0
      model5_w6(9,5) = 0.05767711624503136d0
      model5_w6(9,6) = -0.2415827214717865d0
      model5_w6(9,7) = -0.31607159972190857d0
      model5_w6(9,8) = 0.20570263266563416d0
      model5_w6(9,9) = 0.27256062626838684d0
! model5_hidden_layers.4.bias
      model5_b6(1,1) = -0.23664817214012146d0
      model5_b6(1,2) = 0.09768163412809372d0
      model5_b6(1,3) = 0.08112742751836777d0
      model5_b6(1,4) = 0.1164882555603981d0
      model5_b6(1,5) = 0.1991513967514038d0
      model5_b6(1,6) = -0.4773051142692566d0
      model5_b6(1,7) = 0.23624366521835327d0
      model5_b6(1,8) = 0.2307671755552292d0
      model5_b6(1,9) = 0.3573639690876007d0
! model5_output_layer.weight
      model5_w7(1,1) = 0.3128524124622345d0
      model5_w7(1,2) = 0.6621597409248352d0
      model5_w7(1,3) = -0.11751161515712738d0
      model5_w7(1,4) = -0.15894779562950134d0
      model5_w7(1,5) = -0.14810848236083984d0
      model5_w7(1,6) = -0.7897214889526367d0
      model5_w7(1,7) = 0.6270809769630432d0
      model5_w7(1,8) = 0.7968776822090149d0
      model5_w7(1,9) = -0.40409547090530396d0
! model5_output_layer.bias
      model5_b7(1,1) = 0.07820222526788712d0

! model6_input_layer.weight
      model6_w1(1,1) = -0.24757714569568634d0
      model6_w1(1,2) = -0.09534817188978195d0
      model6_w1(1,3) = 0.4622587263584137d0
      model6_w1(1,4) = 0.01490155328065157d0
      model6_w1(1,5) = 0.17758633196353912d0
      model6_w1(1,6) = 0.13645854592323303d0
      model6_w1(2,1) = -0.21843409538269043d0
      model6_w1(2,2) = -0.3757602274417877d0
      model6_w1(2,3) = -0.14771264791488647d0
      model6_w1(2,4) = 0.24073296785354614d0
      model6_w1(2,5) = -0.11330565810203552d0
      model6_w1(2,6) = -0.002962946891784668d0
      model6_w1(3,1) = 0.11793039739131927d0
      model6_w1(3,2) = 0.1456170529127121d0
      model6_w1(3,3) = -0.11403528600931168d0
      model6_w1(3,4) = -0.3251115381717682d0
      model6_w1(3,5) = -0.26688072085380554d0
      model6_w1(3,6) = 0.38918066024780273d0
      model6_w1(4,1) = 0.16183602809906006d0
      model6_w1(4,2) = 0.41665351390838623d0
      model6_w1(4,3) = 0.14392805099487305d0
      model6_w1(4,4) = -0.15671873092651367d0
      model6_w1(4,5) = -0.41998976469039917d0
      model6_w1(4,6) = 0.0501878559589386d0
      model6_w1(5,1) = -0.09869185835123062d0
      model6_w1(5,2) = 0.15012449026107788d0
      model6_w1(5,3) = 0.14525875449180603d0
      model6_w1(5,4) = 0.06657643616199493d0
      model6_w1(5,5) = -0.22164808213710785d0
      model6_w1(5,6) = 0.4110090434551239d0
      model6_w1(6,1) = 0.1883368045091629d0
      model6_w1(6,2) = -0.14842268824577332d0
      model6_w1(6,3) = 0.29139068722724915d0
      model6_w1(6,4) = -0.0480484701693058d0
      model6_w1(6,5) = 0.17959323525428772d0
      model6_w1(6,6) = -0.4247419834136963d0
      model6_w1(7,1) = -0.043152887374162674d0
      model6_w1(7,2) = 0.33682313561439514d0
      model6_w1(7,3) = -0.06594566255807877d0
      model6_w1(7,4) = -0.07006397843360901d0
      model6_w1(7,5) = -0.20388050377368927d0
      model6_w1(7,6) = 0.015086071565747261d0
      model6_w1(8,1) = -0.03992239385843277d0
      model6_w1(8,2) = 0.09650220721960068d0
      model6_w1(8,3) = 0.38047918677330017d0
      model6_w1(8,4) = -0.22244496643543243d0
      model6_w1(8,5) = 0.06192700192332268d0
      model6_w1(8,6) = 0.2785669267177582d0
      model6_w1(9,1) = -0.0024010837078094482d0
      model6_w1(9,2) = -0.3450119197368622d0
      model6_w1(9,3) = -0.36614206433296204d0
      model6_w1(9,4) = 0.33668822050094604d0
      model6_w1(9,5) = -0.30633601546287537d0
      model6_w1(9,6) = -0.004723191261291504d0
! model6_input_layer.bias
      model6_b1(1,1) = 0.13579946756362915d0
      model6_b1(1,2) = -0.1987905353307724d0
      model6_b1(1,3) = 0.37133461236953735d0
      model6_b1(1,4) = 0.04923436418175697d0
      model6_b1(1,5) = -0.03244154155254364d0
      model6_b1(1,6) = 0.5360963344573975d0
      model6_b1(1,7) = 0.09697849303483963d0
      model6_b1(1,8) = -0.07355428487062454d0
      model6_b1(1,9) = 0.18286234140396118d0
! model6_hidden_layers.0.weight
      model6_w2(1,1) = -0.18301060795783997d0
      model6_w2(1,2) = 0.09407103061676025d0
      model6_w2(1,3) = 0.2648034691810608d0
      model6_w2(1,4) = 0.026555070653557777d0
      model6_w2(1,5) = 0.0546560175716877d0
      model6_w2(1,6) = 0.1836240142583847d0
      model6_w2(1,7) = -0.16431884467601776d0
      model6_w2(1,8) = -0.207233726978302d0
      model6_w2(1,9) = -0.15464036166667938d0
      model6_w2(2,1) = 0.03831613063812256d0
      model6_w2(2,2) = 0.2919810712337494d0
      model6_w2(2,3) = -0.27512285113334656d0
      model6_w2(2,4) = 0.207302987575531d0
      model6_w2(2,5) = -0.25867319107055664d0
      model6_w2(2,6) = 0.3797481954097748d0
      model6_w2(2,7) = 0.1846453696489334d0
      model6_w2(2,8) = 0.17538893222808838d0
      model6_w2(2,9) = 0.2657078802585602d0
      model6_w2(3,1) = 0.4248248040676117d0
      model6_w2(3,2) = 0.2992466986179352d0
      model6_w2(3,3) = -0.08462447673082352d0
      model6_w2(3,4) = 0.3811209499835968d0
      model6_w2(3,5) = 0.4169841408729553d0
      model6_w2(3,6) = -0.054288797080516815d0
      model6_w2(3,7) = 0.21457280218601227d0
      model6_w2(3,8) = 0.5228802561759949d0
      model6_w2(3,9) = -0.10446104407310486d0
      model6_w2(4,1) = 0.08553575724363327d0
      model6_w2(4,2) = -0.07168683409690857d0
      model6_w2(4,3) = -0.10891427099704742d0
      model6_w2(4,4) = 0.03377636522054672d0
      model6_w2(4,5) = 0.5323290228843689d0
      model6_w2(4,6) = -0.41660937666893005d0
      model6_w2(4,7) = 0.08536038547754288d0
      model6_w2(4,8) = 0.32243436574935913d0
      model6_w2(4,9) = -0.1708703488111496d0
      model6_w2(5,1) = 0.3913376033306122d0
      model6_w2(5,2) = -0.1785353422164917d0
      model6_w2(5,3) = 0.5229411721229553d0
      model6_w2(5,4) = -0.048482637852430344d0
      model6_w2(5,5) = 0.33952999114990234d0
      model6_w2(5,6) = -0.4228304922580719d0
      model6_w2(5,7) = 0.32170286774635315d0
      model6_w2(5,8) = 0.3738589882850647d0
      model6_w2(5,9) = -0.28650230169296265d0
      model6_w2(6,1) = 0.33681055903434753d0
      model6_w2(6,2) = 0.03747764229774475d0
      model6_w2(6,3) = 0.522296667098999d0
      model6_w2(6,4) = 0.09484411776065826d0
      model6_w2(6,5) = 0.5609400272369385d0
      model6_w2(6,6) = -0.0969129279255867d0
      model6_w2(6,7) = -0.008884619921445847d0
      model6_w2(6,8) = 0.2711033523082733d0
      model6_w2(6,9) = 0.2923421561717987d0
      model6_w2(7,1) = 0.008231014013290405d0
      model6_w2(7,2) = -0.10251674056053162d0
      model6_w2(7,3) = 0.5572776794433594d0
      model6_w2(7,4) = 0.1306861937046051d0
      model6_w2(7,5) = 0.6164017915725708d0
      model6_w2(7,6) = -0.4099997282028198d0
      model6_w2(7,7) = 0.017922401428222656d0
      model6_w2(7,8) = 0.00922112911939621d0
      model6_w2(7,9) = -0.18325607478618622d0
      model6_w2(8,1) = 0.1806906908750534d0
      model6_w2(8,2) = 0.22264733910560608d0
      model6_w2(8,3) = 0.4588128924369812d0
      model6_w2(8,4) = 0.43527358770370483d0
      model6_w2(8,5) = 0.13810093700885773d0
      model6_w2(8,6) = -0.5441505908966064d0
      model6_w2(8,7) = -0.052110809832811356d0
      model6_w2(8,8) = 0.07166072726249695d0
      model6_w2(8,9) = 0.30841341614723206d0
      model6_w2(9,1) = 0.1236657127737999d0
      model6_w2(9,2) = 0.30714306235313416d0
      model6_w2(9,3) = -0.047792091965675354d0
      model6_w2(9,4) = 0.034964874386787415d0
      model6_w2(9,5) = 0.4016227722167969d0
      model6_w2(9,6) = -0.4374617040157318d0
      model6_w2(9,7) = 0.18607215583324432d0
      model6_w2(9,8) = 0.23299309611320496d0
      model6_w2(9,9) = -0.08250662684440613d0
! model6_hidden_layers.0.bias
      model6_b2(1,1) = 0.14465031027793884d0
      model6_b2(1,2) = 0.17293456196784973d0
      model6_b2(1,3) = 0.28859350085258484d0
      model6_b2(1,4) = 0.09378577023744583d0
      model6_b2(1,5) = 0.09127242118120193d0
      model6_b2(1,6) = 0.06495604664087296d0
      model6_b2(1,7) = 0.045903682708740234d0
      model6_b2(1,8) = -0.17443346977233887d0
      model6_b2(1,9) = 0.20526176691055298d0
! model6_hidden_layers.1.weight
      model6_w3(1,1) = -0.13723766803741455d0
      model6_w3(1,2) = -0.22800353169441223d0
      model6_w3(1,3) = -0.00837935134768486d0
      model6_w3(1,4) = 0.09590515494346619d0
      model6_w3(1,5) = 0.10179174691438675d0
      model6_w3(1,6) = 0.4083021581172943d0
      model6_w3(1,7) = 0.4684257507324219d0
      model6_w3(1,8) = 0.4560159146785736d0
      model6_w3(1,9) = 0.37419256567955017d0
      model6_w3(2,1) = -0.11150459200143814d0
      model6_w3(2,2) = 0.2657301127910614d0
      model6_w3(2,3) = 0.2136562317609787d0
      model6_w3(2,4) = 0.20635491609573364d0
      model6_w3(2,5) = -0.2273515909910202d0
      model6_w3(2,6) = -0.07738327234983444d0
      model6_w3(2,7) = -0.0824296697974205d0
      model6_w3(2,8) = -0.205056831240654d0
      model6_w3(2,9) = 0.1550999879837036d0
      model6_w3(3,1) = 0.2378343641757965d0
      model6_w3(3,2) = -0.321970671415329d0
      model6_w3(3,3) = 0.14853036403656006d0
      model6_w3(3,4) = 0.11536848545074463d0
      model6_w3(3,5) = -0.12617874145507812d0
      model6_w3(3,6) = -0.3246803283691406d0
      model6_w3(3,7) = 0.3311977684497833d0
      model6_w3(3,8) = 0.12633416056632996d0
      model6_w3(3,9) = -0.14815664291381836d0
      model6_w3(4,1) = -0.18621326982975006d0
      model6_w3(4,2) = -0.5000455379486084d0
      model6_w3(4,3) = 0.1604936271905899d0
      model6_w3(4,4) = 0.23910358548164368d0
      model6_w3(4,5) = 0.44384804368019104d0
      model6_w3(4,6) = 0.3444300889968872d0
      model6_w3(4,7) = 0.38047558069229126d0
      model6_w3(4,8) = 0.4813843071460724d0
      model6_w3(4,9) = 0.41260701417922974d0
      model6_w3(5,1) = -0.3019043505191803d0
      model6_w3(5,2) = 0.11429327726364136d0
      model6_w3(5,3) = -0.09493564069271088d0
      model6_w3(5,4) = 0.07405853271484375d0
      model6_w3(5,5) = 0.06576656550168991d0
      model6_w3(5,6) = -0.1749747097492218d0
      model6_w3(5,7) = 0.3082805871963501d0
      model6_w3(5,8) = 0.31319284439086914d0
      model6_w3(5,9) = 0.27132895588874817d0
      model6_w3(6,1) = 0.02132059447467327d0
      model6_w3(6,2) = -0.2459293156862259d0
      model6_w3(6,3) = -0.1920064240694046d0
      model6_w3(6,4) = -0.06023810803890228d0
      model6_w3(6,5) = 0.2986306846141815d0
      model6_w3(6,6) = -0.28338995575904846d0
      model6_w3(6,7) = -0.26237672567367554d0
      model6_w3(6,8) = -0.019293228164315224d0
      model6_w3(6,9) = -0.2716130316257477d0
      model6_w3(7,1) = -0.1257353276014328d0
      model6_w3(7,2) = -0.3327171504497528d0
      model6_w3(7,3) = -0.23440603911876678d0
      model6_w3(7,4) = 0.25446000695228577d0
      model6_w3(7,5) = -0.05849289894104004d0
      model6_w3(7,6) = -0.16656558215618134d0
      model6_w3(7,7) = 0.029165118932724d0
      model6_w3(7,8) = -0.009300321340560913d0
      model6_w3(7,9) = -0.028456926345825195d0
      model6_w3(8,1) = 0.20221209526062012d0
      model6_w3(8,2) = -0.22647909820079803d0
      model6_w3(8,3) = -0.3189811706542969d0
      model6_w3(8,4) = 0.10794831067323685d0
      model6_w3(8,5) = 0.2725222408771515d0
      model6_w3(8,6) = -0.23553471267223358d0
      model6_w3(8,7) = -0.18945258855819702d0
      model6_w3(8,8) = 0.06199706345796585d0
      model6_w3(8,9) = 0.002504973206669092d0
      model6_w3(9,1) = -0.11519573628902435d0
      model6_w3(9,2) = 0.4644562900066376d0
      model6_w3(9,3) = 0.08389423042535782d0
      model6_w3(9,4) = -0.05138399824500084d0
      model6_w3(9,5) = 0.018766548484563828d0
      model6_w3(9,6) = -0.14413081109523773d0
      model6_w3(9,7) = 0.04823268949985504d0
      model6_w3(9,8) = -0.2693938612937927d0
      model6_w3(9,9) = 0.2636082172393799d0
! model6_hidden_layers.1.bias
      model6_b3(1,1) = -0.25893154740333557d0
      model6_b3(1,2) = 0.10094558447599411d0
      model6_b3(1,3) = -0.2247399538755417d0
      model6_b3(1,4) = -0.056657761335372925d0
      model6_b3(1,5) = -0.24487841129302979d0
      model6_b3(1,6) = 0.12032116949558258d0
      model6_b3(1,7) = -0.27936145663261414d0
      model6_b3(1,8) = 0.11616960912942886d0
      model6_b3(1,9) = 0.061097726225852966d0
! model6_hidden_layers.2.weight
      model6_w4(1,1) = 0.41569167375564575d0
      model6_w4(1,2) = -0.4617600739002228d0
      model6_w4(1,3) = -0.30455538630485535d0
      model6_w4(1,4) = 0.5912012457847595d0
      model6_w4(1,5) = 0.23758460581302643d0
      model6_w4(1,6) = -0.304520845413208d0
      model6_w4(1,7) = -0.3186919093132019d0
      model6_w4(1,8) = -0.11605031043291092d0
      model6_w4(1,9) = -0.46503832936286926d0
      model6_w4(2,1) = -0.22814540565013885d0
      model6_w4(2,2) = -0.23462867736816406d0
      model6_w4(2,3) = -0.04442393779754639d0
      model6_w4(2,4) = 0.0035809576511383057d0
      model6_w4(2,5) = 0.2861655056476593d0
      model6_w4(2,6) = 0.1630200445652008d0
      model6_w4(2,7) = 0.31866154074668884d0
      model6_w4(2,8) = -0.09179091453552246d0
      model6_w4(2,9) = 0.14684292674064636d0
      model6_w4(3,1) = -0.26165664196014404d0
      model6_w4(3,2) = -0.22163638472557068d0
      model6_w4(3,3) = -0.25061091780662537d0
      model6_w4(3,4) = -0.18698041141033173d0
      model6_w4(3,5) = 0.3322964012622833d0
      model6_w4(3,6) = -0.21444901823997498d0
      model6_w4(3,7) = -0.2770629823207855d0
      model6_w4(3,8) = 0.15400448441505432d0
      model6_w4(3,9) = 0.29583409428596497d0
      model6_w4(4,1) = 0.4196007251739502d0
      model6_w4(4,2) = 0.07457812875509262d0
      model6_w4(4,3) = -0.04954305291175842d0
      model6_w4(4,4) = 0.033098943531513214d0
      model6_w4(4,5) = 0.11863596737384796d0
      model6_w4(4,6) = 0.28241220116615295d0
      model6_w4(4,7) = 0.10791739821434021d0
      model6_w4(4,8) = 0.10052096843719482d0
      model6_w4(4,9) = -0.3729403018951416d0
      model6_w4(5,1) = -0.32517093420028687d0
      model6_w4(5,2) = -0.23394839465618134d0
      model6_w4(5,3) = -0.31414473056793213d0
      model6_w4(5,4) = -0.052001357078552246d0
      model6_w4(5,5) = 0.3023339807987213d0
      model6_w4(5,6) = -0.12312862277030945d0
      model6_w4(5,7) = 0.08092787861824036d0
      model6_w4(5,8) = 0.10252177715301514d0
      model6_w4(5,9) = -0.2260942906141281d0
      model6_w4(6,1) = 0.3551346957683563d0
      model6_w4(6,2) = -0.5435223579406738d0
      model6_w4(6,3) = -0.31925690174102783d0
      model6_w4(6,4) = 0.1488165408372879d0
      model6_w4(6,5) = -0.10855130106210709d0
      model6_w4(6,6) = 0.1028459370136261d0
      model6_w4(6,7) = 0.08660122752189636d0
      model6_w4(6,8) = -0.19393882155418396d0
      model6_w4(6,9) = -0.331107497215271d0
      model6_w4(7,1) = 0.5253534317016602d0
      model6_w4(7,2) = -0.3432956337928772d0
      model6_w4(7,3) = -0.32425054907798767d0
      model6_w4(7,4) = 0.46013620495796204d0
      model6_w4(7,5) = 0.3680539131164551d0
      model6_w4(7,6) = 0.16669639945030212d0
      model6_w4(7,7) = -0.02686154842376709d0
      model6_w4(7,8) = -0.018389850854873657d0
      model6_w4(7,9) = -0.17684373259544373d0
      model6_w4(8,1) = 0.11373598128557205d0
      model6_w4(8,2) = -0.2717640697956085d0
      model6_w4(8,3) = -0.06716334819793701d0
      model6_w4(8,4) = -0.257011741399765d0
      model6_w4(8,5) = -0.18653985857963562d0
      model6_w4(8,6) = -0.07752369344234467d0
      model6_w4(8,7) = 0.28241172432899475d0
      model6_w4(8,8) = 0.25545626878738403d0
      model6_w4(8,9) = -0.24278154969215393d0
      model6_w4(9,1) = -0.08236563205718994d0
      model6_w4(9,2) = -0.24229268729686737d0
      model6_w4(9,3) = 0.21915647387504578d0
      model6_w4(9,4) = -0.2858545184135437d0
      model6_w4(9,5) = 0.23770824074745178d0
      model6_w4(9,6) = 0.30548980832099915d0
      model6_w4(9,7) = -0.22220496833324432d0
      model6_w4(9,8) = -0.28041934967041016d0
      model6_w4(9,9) = 0.2924250662326813d0
! model6_hidden_layers.2.bias
      model6_b4(1,1) = 0.11268366128206253d0
      model6_b4(1,2) = -0.1727575957775116d0
      model6_b4(1,3) = -0.1958095282316208d0
      model6_b4(1,4) = -0.013603540137410164d0
      model6_b4(1,5) = -0.01984727382659912d0
      model6_b4(1,6) = -0.11039608716964722d0
      model6_b4(1,7) = -0.3919738233089447d0
      model6_b4(1,8) = 0.15382802486419678d0
      model6_b4(1,9) = -0.27168190479278564d0
! model6_hidden_layers.3.weight
      model6_w5(1,1) = 0.34481900930404663d0
      model6_w5(1,2) = 0.07550540566444397d0
      model6_w5(1,3) = 0.2659294307231903d0
      model6_w5(1,4) = -0.14827096462249756d0
      model6_w5(1,5) = -0.2811260223388672d0
      model6_w5(1,6) = 0.06140146031975746d0
      model6_w5(1,7) = 0.17032186686992645d0
      model6_w5(1,8) = -0.032910265028476715d0
      model6_w5(1,9) = -0.02966606616973877d0
      model6_w5(2,1) = -0.16659465432167053d0
      model6_w5(2,2) = 0.19677212834358215d0
      model6_w5(2,3) = -0.10075327754020691d0
      model6_w5(2,4) = 0.36031803488731384d0
      model6_w5(2,5) = 0.32103994488716125d0
      model6_w5(2,6) = 0.19497337937355042d0
      model6_w5(2,7) = 0.3310372233390808d0
      model6_w5(2,8) = -0.0034255683422088623d0
      model6_w5(2,9) = -0.21888002753257751d0
      model6_w5(3,1) = 0.23214833438396454d0
      model6_w5(3,2) = 0.06145396828651428d0
      model6_w5(3,3) = 0.155857652425766d0
      model6_w5(3,4) = 0.1532827466726303d0
      model6_w5(3,5) = 0.0324174165725708d0
      model6_w5(3,6) = 0.39334508776664734d0
      model6_w5(3,7) = 0.3379741907119751d0
      model6_w5(3,8) = -0.075355663895607d0
      model6_w5(3,9) = -0.26972243189811707d0
      model6_w5(4,1) = 0.36215510964393616d0
      model6_w5(4,2) = 0.2703215181827545d0
      model6_w5(4,3) = -0.016408175230026245d0
      model6_w5(4,4) = 0.3238151967525482d0
      model6_w5(4,5) = 0.21199187636375427d0
      model6_w5(4,6) = 0.4257219731807709d0
      model6_w5(4,7) = 0.022071117535233498d0
      model6_w5(4,8) = -0.06917999684810638d0
      model6_w5(4,9) = 0.01870870590209961d0
      model6_w5(5,1) = 0.4161350429058075d0
      model6_w5(5,2) = -0.27420181035995483d0
      model6_w5(5,3) = 0.1309661567211151d0
      model6_w5(5,4) = 0.25645408034324646d0
      model6_w5(5,5) = -0.017411023378372192d0
      model6_w5(5,6) = 0.19757793843746185d0
      model6_w5(5,7) = 0.4729430675506592d0
      model6_w5(5,8) = -0.3373953700065613d0
      model6_w5(5,9) = 0.2893619239330292d0
      model6_w5(6,1) = -0.2525898814201355d0
      model6_w5(6,2) = 0.1162794828414917d0
      model6_w5(6,3) = -0.3319912552833557d0
      model6_w5(6,4) = -0.23717327415943146d0
      model6_w5(6,5) = -0.055797725915908813d0
      model6_w5(6,6) = 0.13120272755622864d0
      model6_w5(6,7) = -0.18213650584220886d0
      model6_w5(6,8) = -0.13534262776374817d0
      model6_w5(6,9) = -0.26395368576049805d0
      model6_w5(7,1) = 0.07698158174753189d0
      model6_w5(7,2) = 0.15156212449073792d0
      model6_w5(7,3) = 0.25166741013526917d0
      model6_w5(7,4) = 0.015406221151351929d0
      model6_w5(7,5) = 0.12022030353546143d0
      model6_w5(7,6) = -0.1975294053554535d0
      model6_w5(7,7) = -0.00428416533395648d0
      model6_w5(7,8) = -0.2523902356624603d0
      model6_w5(7,9) = -0.1308746039867401d0
      model6_w5(8,1) = -0.32413387298583984d0
      model6_w5(8,2) = -0.24581894278526306d0
      model6_w5(8,3) = 0.24857047200202942d0
      model6_w5(8,4) = -0.149745911359787d0
      model6_w5(8,5) = -0.08842127025127411d0
      model6_w5(8,6) = 0.285434752702713d0
      model6_w5(8,7) = 0.18023720383644104d0
      model6_w5(8,8) = -0.22522518038749695d0
      model6_w5(8,9) = -0.1311333179473877d0
      model6_w5(9,1) = -0.05398768559098244d0
      model6_w5(9,2) = -0.13253149390220642d0
      model6_w5(9,3) = -0.1183168888092041d0
      model6_w5(9,4) = 0.46379533410072327d0
      model6_w5(9,5) = -0.29911357164382935d0
      model6_w5(9,6) = 0.11943241208791733d0
      model6_w5(9,7) = 0.5547277927398682d0
      model6_w5(9,8) = 0.08278457820415497d0
      model6_w5(9,9) = 0.186616450548172d0
! model6_hidden_layers.3.bias
      model6_b5(1,1) = 0.2924706041812897d0
      model6_b5(1,2) = -0.020083438605070114d0
      model6_b5(1,3) = 0.10049082338809967d0
      model6_b5(1,4) = 0.13268668949604034d0
      model6_b5(1,5) = -0.2084580510854721d0
      model6_b5(1,6) = 0.22575047612190247d0
      model6_b5(1,7) = -0.1874326765537262d0
      model6_b5(1,8) = -0.11861038208007812d0
      model6_b5(1,9) = 0.20136728882789612d0
! model6_hidden_layers.4.weight
      model6_w6(1,1) = 0.4609149396419525d0
      model6_w6(1,2) = 0.23454636335372925d0
      model6_w6(1,3) = 0.14704951643943787d0
      model6_w6(1,4) = 0.5011268854141235d0
      model6_w6(1,5) = 0.41975194215774536d0
      model6_w6(1,6) = 0.23987790942192078d0
      model6_w6(1,7) = 0.2568645477294922d0
      model6_w6(1,8) = -0.33235931396484375d0
      model6_w6(1,9) = -0.0005345441750250757d0
      model6_w6(2,1) = 0.021589159965515137d0
      model6_w6(2,2) = 0.020135730504989624d0
      model6_w6(2,3) = -0.01013803482055664d0
      model6_w6(2,4) = -0.06137192249298096d0
      model6_w6(2,5) = -0.32572299242019653d0
      model6_w6(2,6) = -0.09964506328105927d0
      model6_w6(2,7) = 0.1242203414440155d0
      model6_w6(2,8) = -0.27708011865615845d0
      model6_w6(2,9) = -0.25846433639526367d0
      model6_w6(3,1) = 0.4020049273967743d0
      model6_w6(3,2) = 0.16619448363780975d0
      model6_w6(3,3) = 0.18069516122341156d0
      model6_w6(3,4) = 0.3300637900829315d0
      model6_w6(3,5) = 0.18345026671886444d0
      model6_w6(3,6) = -0.19915857911109924d0
      model6_w6(3,7) = 0.1832617223262787d0
      model6_w6(3,8) = 0.1786210834980011d0
      model6_w6(3,9) = 0.37264299392700195d0
      model6_w6(4,1) = -0.21430782973766327d0
      model6_w6(4,2) = 0.08688685297966003d0
      model6_w6(4,3) = -0.1520908772945404d0
      model6_w6(4,4) = 0.17740967869758606d0
      model6_w6(4,5) = 0.10435664653778076d0
      model6_w6(4,6) = -0.09882418811321259d0
      model6_w6(4,7) = 0.17173203825950623d0
      model6_w6(4,8) = 0.2502707540988922d0
      model6_w6(4,9) = 0.0036433935165405273d0
      model6_w6(5,1) = 0.05554339289665222d0
      model6_w6(5,2) = -0.06770849227905273d0
      model6_w6(5,3) = -0.07413280010223389d0
      model6_w6(5,4) = 0.32316967844963074d0
      model6_w6(5,5) = -0.14993023872375488d0
      model6_w6(5,6) = -0.09202377498149872d0
      model6_w6(5,7) = 0.28898242115974426d0
      model6_w6(5,8) = 0.25282302498817444d0
      model6_w6(5,9) = -0.15410423278808594d0
      model6_w6(6,1) = 0.1405574083328247d0
      model6_w6(6,2) = 0.03690573573112488d0
      model6_w6(6,3) = 0.27450719475746155d0
      model6_w6(6,4) = -0.191950723528862d0
      model6_w6(6,5) = -0.29123660922050476d0
      model6_w6(6,6) = 0.2742960751056671d0
      model6_w6(6,7) = -0.03983762860298157d0
      model6_w6(6,8) = -0.12283504009246826d0
      model6_w6(6,9) = -0.015143722295761108d0
      model6_w6(7,1) = -0.1545143872499466d0
      model6_w6(7,2) = 0.08740784227848053d0
      model6_w6(7,3) = 0.25758811831474304d0
      model6_w6(7,4) = -0.13912922143936157d0
      model6_w6(7,5) = -0.1047687977552414d0
      model6_w6(7,6) = 0.13182993233203888d0
      model6_w6(7,7) = -0.2565936744213104d0
      model6_w6(7,8) = 0.32873448729515076d0
      model6_w6(7,9) = 0.09325825423002243d0
      model6_w6(8,1) = 0.05084575340151787d0
      model6_w6(8,2) = -0.18714866042137146d0
      model6_w6(8,3) = 0.17727592587471008d0
      model6_w6(8,4) = -0.14540624618530273d0
      model6_w6(8,5) = 0.08561276644468307d0
      model6_w6(8,6) = -0.025835420936346054d0
      model6_w6(8,7) = -0.0446159653365612d0
      model6_w6(8,8) = 0.22194746136665344d0
      model6_w6(8,9) = -0.3409161865711212d0
      model6_w6(9,1) = -0.321549654006958d0
      model6_w6(9,2) = -0.0819418653845787d0
      model6_w6(9,3) = 0.113505519926548d0
      model6_w6(9,4) = 0.09768079221248627d0
      model6_w6(9,5) = 0.08539198338985443d0
      model6_w6(9,6) = -0.03896207734942436d0
      model6_w6(9,7) = 0.14431288838386536d0
      model6_w6(9,8) = 0.16151416301727295d0
      model6_w6(9,9) = -0.1826533079147339d0
! model6_hidden_layers.4.bias
      model6_b6(1,1) = 0.022937927395105362d0
      model6_b6(1,2) = -0.2614386975765228d0
      model6_b6(1,3) = 0.253235787153244d0
      model6_b6(1,4) = -0.14293000102043152d0
      model6_b6(1,5) = -0.16987645626068115d0
      model6_b6(1,6) = -0.13027426600456238d0
      model6_b6(1,7) = -0.04345931112766266d0
      model6_b6(1,8) = 0.03325262665748596d0
      model6_b6(1,9) = 0.24139590561389923d0
! model6_output_layer.weight
      model6_w7(1,1) = 0.351420134305954d0
      model6_w7(1,2) = -0.18784384429454803d0
      model6_w7(1,3) = 0.34648221731185913d0
      model6_w7(1,4) = -0.2789658010005951d0
      model6_w7(1,5) = 0.04812929034233093d0
      model6_w7(1,6) = -0.2827250361442566d0
      model6_w7(1,7) = -0.047939546406269073d0
      model6_w7(1,8) = -0.03274303302168846d0
      model6_w7(1,9) = -0.2918946444988251d0
! model6_output_layer.bias
      model6_b7(1,1) = -0.22669611871242523d0

      
      
      
      
      B_sym(1,1)=DFGRD1(1, 1)**2+DFGRD1(1, 2)**2+DFGRD1(1, 3)**2
      B_sym(1,2)=DFGRD1(2, 1)**2+DFGRD1(2, 2)**2+DFGRD1(2, 3)**2
      B_sym(1,3)=DFGRD1(3, 3)**2+DFGRD1(3, 1)**2+DFGRD1(3, 2)**2
      B_sym(1,4)=DFGRD1(1, 1)*DFGRD1(2, 1)+DFGRD1(1, 2)*DFGRD1(2, 2)
     1       +DFGRD1(1, 3)*DFGRD1(2, 3)
      IF(NSHR.EQ.3) THEN
        B_sym(1,5)=DFGRD1(1, 1)*DFGRD1(3, 1)+DFGRD1(1, 2)*DFGRD1(3, 2)
     1         +DFGRD1(1, 3)*DFGRD1(3, 3)
        B_sym(1,6)=DFGRD1(2, 1)*DFGRD1(3, 1)+DFGRD1(2, 2)*DFGRD1(3, 2)
     1         +DFGRD1(2, 3)*DFGRD1(3, 3)
      END IF
      
      
      
      
      B_sym1(1,1)=B_sym(1,1)+eps_d
      B_sym1(1,2)=B_sym(1,2)
      B_sym1(1,3)=B_sym(1,3)
      B_sym1(1,4)=B_sym(1,4)
      B_sym1(1,5)=B_sym(1,5)
      B_sym1(1,6)=B_sym(1,6)
      
      B_sym2(1,1)=B_sym(1,1)
      B_sym2(1,2)=B_sym(1,2)+eps_d
      B_sym2(1,3)=B_sym(1,3)
      B_sym2(1,4)=B_sym(1,4)
      B_sym2(1,5)=B_sym(1,5)
      B_sym2(1,6)=B_sym(1,6)
      B_sym3(1,1)=B_sym(1,1)
      B_sym3(1,2)=B_sym(1,2)
      B_sym3(1,3)=B_sym(1,3)+eps_d
      B_sym3(1,4)=B_sym(1,4)
      B_sym3(1,5)=B_sym(1,5)
      B_sym3(1,6)=B_sym(1,6)
      B_sym4(1,1)=B_sym(1,1)
      B_sym4(1,2)=B_sym(1,2)
      B_sym4(1,3)=B_sym(1,3)
      B_sym4(1,4)=B_sym(1,4)+eps_d
      B_sym4(1,5)=B_sym(1,5)
      B_sym4(1,6)=B_sym(1,6)
      B_sym5(1,1)=B_sym(1,1)
      B_sym5(1,2)=B_sym(1,2)
      B_sym5(1,3)=B_sym(1,3)
      B_sym5(1,4)=B_sym(1,4)
      B_sym5(1,5)=B_sym(1,5)+eps_d
      B_sym5(1,6)=B_sym(1,6)
      B_sym6(1,1)=B_sym(1,1)
      B_sym6(1,2)=B_sym(1,2)
      B_sym6(1,3)=B_sym(1,3)
      B_sym6(1,4)=B_sym(1,4)
      B_sym6(1,5)=B_sym(1,5)
      B_sym6(1,6)=B_sym(1,6)+eps_d
      
      ! reshape to all B's to 1x6
      
      
      
      print *, "B_sym="
      print *, B_sym(1,1), B_sym(1,2), B_sym(1,3) 
      print *, B_sym(1,4), B_sym(1,5), B_sym(1,6)
      print *, "sig11 model"
      sig11(1,1)=0.d0
      sig22(1,1)=0.d0
      sig33(1,1)=0.d0
      sig12(1,1)=0.d0
      sig13(1,1)=0.d0
      sig23(1,1)=0.d0
      sig11=model1(B_sym, model1_w1, model1_w2, 
     * model1_w3, model1_w4, model1_w5,
     * model1_w6, model1_w7, model1_b1, model1_b2, model1_b3, model1_b4,
     * model1_b5, model1_b6, model1_b7)
      print *, "sig22 model"
      print *, "sig11=", sig11
      print *, "sig22=", sig22
      print *, "sig33=", sig33
      print *, "sig12=", sig12
      print *, "sig13=", sig13
      print *, "sig23=", sig23
      sig22=model2(B_sym, model2_w1, model2_w2,
     * model2_w3, model2_w4, model2_w5,
     * model2_b1, model2_b2, model2_b3, model2_b4, model2_b5)
      print *, "sig33 model"
      print *, "sig11=", sig11
      print *, "sig22=", sig22
      print *, "sig33=", sig33
      print *, "sig12=", sig12
      print *, "sig13=", sig13
      print *, "sig23=", sig23
      sig33=model1(B_sym, model3_w1, model3_w2,
     * model3_w3, model3_w4, model3_w5,
     * model3_b1, model3_b2, model3_b3, model3_b4, model3_b5)
      print *, "sig12 model"
      print *, "sig11=", sig11
      print *, "sig22=", sig22
      print *, "sig33=", sig33
      print *, "sig12=", sig12
      print *, "sig13=", sig13
      print *, "sig23=", sig23
      sig12=model1(B_sym, model4_w1, model4_w2,
     * model4_w3, model4_w4, model4_w5,
     * model4_b1, model4_b2, model4_b3, model4_b4, model4_b5)
      print *, "sig13 model"
      print *, "sig11=", sig11
      print *, "sig22=", sig22
      print *, "sig33=", sig33
      print *, "sig12=", sig12
      print *, "sig13=", sig13
      print *, "sig23=", sig23
      sig13=model1(B_sym, model5_w1, model5_w2,
     * model5_w3, model5_w4, model5_w5,
     * model5_b1, model5_b2, model5_b3, model5_b4, model5_b5)
      print *, "sig23 model"
      print *, "sig11=", sig11
      print *, "sig22=", sig22
      print *, "sig33=", sig33
      print *, "sig12=", sig12
      print *, "sig13=", sig13
      print *, "sig23=", sig23
      sig23=model1(B_sym, model6_w1, model6_w2,
     * model6_w3, model6_w4, model6_w5,
     * model6_b1, model6_b2, model6_b3, model6_b4, model6_b5)
      
      print *, "sig11=", sig11
      print *, "sig22=", sig22
      print *, "sig33=", sig33
      print *, "sig12=", sig12
      print *, "sig13=", sig13
      print *, "sig23=", sig23
      
      
      
      print *, "sig11_1 model"
      sig11_1=model1(B_sym1, model1_w1, model1_w2,
     * model1_w3, model1_w4, model1_w5,
     * model1_w6, model1_w7, model1_b1, model1_b2, model1_b3, model1_b4,
     * model1_b5, model1_b6, model1_b7)
      print *, "sig11_2 model"
      sig11_2=model1(B_sym2, model1_w1, model1_w2,
     * model1_w3, model1_w4, model1_w5,
     * model1_w6, model1_w7, model1_b1, model1_b2, model1_b3, model1_b4,
     * model1_b5, model1_b6, model1_b7)
      print *, "sig11_3 model"
      sig11_3=model1(B_sym3, model1_w1, model1_w2,
     * model1_w3, model1_w4, model1_w5,
     * model1_w6, model1_w7, model1_b1, model1_b2, model1_b3, model1_b4,
     * model1_b5, model1_b6, model1_b7)
      print *, "sig11_4 model"
      sig11_4=model1(B_sym4, model1_w1, model1_w2,
     * model1_w3, model1_w4, model1_w5,
     * model1_w6, model1_w7, model1_b1, model1_b2, model1_b3, model1_b4,
     * model1_b5, model1_b6, model1_b7)
      print *, "sig11_5 model"
      sig11_5=model1(B_sym5, model1_w1, model1_w2,
     * model1_w3, model1_w4, model1_w5,
     * model1_w6, model1_w7, model1_b1, model1_b2, model1_b3, model1_b4,
     * model1_b5, model1_b6, model1_b7)
      print *, "sig11_6 model"
      sig11_6=model1(B_sym6, model1_w1, model1_w2,
     * model1_w3, model1_w4, model1_w5,
     * model1_w6, model1_w7, model1_b1, model1_b2, model1_b3, model1_b4,
     * model1_b5, model1_b6, model1_b7)
      ! will write the symmetric part
      print *, "sig22_2 model"
      sig22_2=model2(B_sym2, model2_w1, model2_w2,
     * model2_w3, model2_w4, model2_w5,
     * model2_b1, model2_b2, model2_b3, model2_b4, model2_b5)
      print *, "sig22_3 model"
      sig22_3=model2(B_sym3, model2_w1, model2_w2,
     * model2_w3, model2_w4, model2_w5,
     * model2_b1, model2_b2, model2_b3, model2_b4, model2_b5)
      print *, "sig22_4 model"
      sig22_4=model2(B_sym4, model2_w1, model2_w2,
     * model2_w3, model2_w4, model2_w5,
     * model2_b1, model2_b2, model2_b3, model2_b4, model2_b5)
      
      print *, "sig22_5 model"
      sig22_5=model2(B_sym5, model2_w1, model2_w2,
     * model2_w3, model2_w4, model2_w5,
     * model2_b1, model2_b2, model2_b3, model2_b4, model2_b5)
      
      print *, "sig22_6 model"
      sig22_6=model2(B_sym6, model2_w1, model2_w2,
     * model2_w3, model2_w4, model2_w5,
     * model2_b1, model2_b2, model2_b3, model2_b4, model2_b5)
      print *, "sig33_3 model"
      sig33_3=model1(B_sym3, model3_w1, model3_w2,
     * model3_w3, model3_w4, model3_w5,model3_w6, model3_w7,
     * model3_b1, model3_b2, model3_b3, model3_b4, model3_b5,
     * model3_b6, model3_b7)
      print *, "sig33_4 model"
      sig33_4=model1(B_sym4, model3_w1, model3_w2,
     * model3_w3, model3_w4, model3_w5,model3_w6, model3_w7,
     * model3_b1, model3_b2, model3_b3, model3_b4, model3_b5,
     * model3_b6, model3_b7)
      print *, "sig33_5 model"
      sig33_5=model1(B_sym5, model3_w1, model3_w2,
     * model3_w3, model3_w4, model3_w5,model3_w6, model3_w7,
     * model3_b1, model3_b2, model3_b3, model3_b4, model3_b5,
     * model3_b6, model3_b7)
      print *, "sig33_6 model"
      sig33_6=model1(B_sym6, model3_w1, model3_w2,
     * model3_w3, model3_w4, model3_w5,model3_w6, model3_w7,
     * model3_b1, model3_b2, model3_b3, model3_b4, model3_b5,
     * model3_b6, model3_b7)
      print *, "sig12_4 model"
      sig12_4=model1(B_sym4, model4_w1, model4_w2,
     * model4_w3, model4_w4, model4_w5,model4_w6, model4_w7,
     * model4_b1, model4_b2, model4_b3, model4_b4, model4_b5,
     * model4_b6, model4_b7)
      print *, "sig12_5 model"
      sig12_5=model1(B_sym5, model4_w1, model4_w2,
     * model4_w3, model4_w4, model4_w5,model4_w6, model4_w7,
     * model4_b1, model4_b2, model4_b3, model4_b4, model4_b5,
     * model4_b6, model4_b7)
      print *, "sig12_6 model"
      sig12_6=model1(B_sym6, model4_w1, model4_w2,
     * model4_w3, model4_w4, model4_w5,model4_w6, model4_w7,
     * model4_b1, model4_b2, model4_b3, model4_b4, model4_b5,
     * model4_b6, model4_b7)
      print *, "sig13_5 model"
      sig13_5=model1(B_sym5, model5_w1, model5_w2,
     * model5_w3, model5_w4, model5_w5,model5_w6, model5_w7,
     * model5_b1, model5_b2, model5_b3, model5_b4, model5_b5,
     * model5_b6, model5_b7)
      print *, "sig13_6 model"
      sig13_6=model1(B_sym6, model5_w1, model5_w2,
     * model5_w3, model5_w4, model5_w5,model5_w6, model5_w7,
     * model5_b1, model5_b2, model5_b3, model5_b4, model5_b5,
     * model5_b6, model5_b7)
      print *, "sig23_6 model"
      sig23_6=model1(B_sym6, model6_w1, model6_w2,
     * model6_w3, model6_w4, model6_w5,model6_w6, model6_w7,
     * model6_b1, model6_b2, model6_b3, model6_b4, model6_b5,
     * model6_b6, model6_b7)
      
      STRESS(1)=sig11(1,1)
      STRESS(2)=sig22(1,1)
      STRESS(3)=sig33(1,1)
      STRESS(4)=sig12(1,1)
      STRESS(5)=sig13(1,1)
      STRESS(6)=sig23(1,1)
      
      ! stiffness part
      DDSDDE(1,1)= (sig11_1(1,1)-sig11(1,1))/eps_d
      DDSDDE(1,2)= (sig11_2(1,1)-sig11(1,1))/eps_d
      DDSDDE(1,3)= (sig11_3(1,1)-sig11(1,1))/eps_d
      DDSDDE(1,4)= (sig11_4(1,1)-sig11(1,1))/eps_d
      DDSDDE(1,5)= (sig11_5(1,1)-sig11(1,1))/eps_d
      DDSDDE(1,6)= (sig11_6(1,1)-sig11(1,1))/eps_d
      
      DDSDDE(2,2)= (sig22_2(1,1)-sig22(1,1))/eps_d
      DDSDDE(3,3)= (sig33_3(1,1)-sig33(1,1))/eps_d
      
      
      DDSDDE(2,3)= (sig22_3(1,1)-sig22(1,1))/eps_d
      
      DDSDDE(2,4)= (sig22_4(1,1)-sig22(1,1))/eps_d
      DDSDDE(3,4)= (sig33_4(1,1)-sig33(1,1))/eps_d
      
      DDSDDE(2,5)= (sig22_5(1,1)-sig22(1,1))/eps_d
      DDSDDE(3,5)= (sig33_5(1,1)-sig33(1,1))/eps_d
      
      DDSDDE(2,6)= (sig22_6(1,1)-sig22(1,1))/eps_d
      DDSDDE(3,6)= (sig33_6(1,1)-sig33(1,1))/eps_d
      DDSDDE(4,4)= (sig12_4(1,1)-sig12(1,1))/eps_d
      DDSDDE(5,5)= (sig13_5(1,1)-sig13(1,1))/eps_d
      DDSDDE(6,6)= (sig23_6(1,1)-sig23(1,1))/eps_d
      DDSDDE(4,5)= (sig12_5(1,1)-sig12(1,1))/eps_d
      DDSDDE(4,6)= (sig12_6(1,1)-sig12(1,1))/eps_d
      DDSDDE(5,6)= (sig13_6(1,1)-sig13(1,1))/eps_d
      
      DO K1=1, NTENS
        DO K2=1, K1-1
          DDSDDE(K1, K2)=DDSDDE(K2, K1)
        END DO
      END DO
      
      DO K1=1,NTENS
            print *,"STRESS(",K1,")=", STRESS(K1)
      END DO
      DO K1=1, NTENS
        DO K2=1, NTENS
          print *,"DDSDDE(",K1,",", K2,")=", DDSDDE(K1, K2)
        END DO
      END DO
      
      
      
      

C
      RETURN
      END
      
      function model1(X,w1,w2,w3,w4,w5,w6,w7,
     *            b1,b2,b3,b4,b5,b6,b7) result(y)
        !implicit none
        integer, parameter::input_size = 6
        integer, parameter::output_size = 1
        integer, parameter::hidden_layers = 5
        integer, parameter::hidden_size = 9
        integer::i
        real, dimension(1,input_size), intent(in) :: X
        real, dimension(hidden_size,input_size), intent(in) :: w1
        real, dimension(hidden_size,hidden_size), intent(in) :: w2,w3,
     *  w4,w5,w6
        real, dimension(output_size,hidden_size), intent(in) :: w7
        real, dimension(1,hidden_size), intent(in) :: b1,b2,b3,b4,b5,b6
        real, dimension(1,output_size), intent(in) :: b7
        integer, dimension(2):: input_size_layer
        real, dimension(1,9) :: h1, h2
        !real, dimension(1,1),intent(out) :: y
        
        input_size_layer(1) = 1
        input_size_layer(2) = input_size
        print *, "model1 h1: shape(X)=", shape(X)
        print *, "model1 h1: shape(w1)  =", shape(w1)
        print *, "model1 h1: shape(b1) =", shape(b1)
        
        
        !print *, "before forward shape(X)=",shape(X)
        call forward(X, w1, b1,h1,input_size_layer,hidden_size)
        print *, "model1 h1: shape(h1) =", shape(h1)
        h1= relu(h1,hidden_size)
        input_size_layer = shape(h1)
        print *, "model1 h2: shape(h1) =", shape(h1)
        call forward(h1, w2, b2,h2,input_size_layer,hidden_size)
        h2= relu(h2,hidden_size)
        print *, "model1 h3: shape(h1) =", shape(h1)
        call forward(h2, w3, b3,h1,input_size_layer,hidden_size)
        h1= relu(h1,hidden_size)
        print *, "model1 h4: shape(h1) =", shape(h1)
        call forward(h1, w4, b4,h2,input_size_layer,hidden_size)
        h1= relu(h1,hidden_size)
        print *, "model1 h5: shape(h1) =", shape(h1)
        call forward(h2, w5, b5,h1,input_size_layer,hidden_size)
        h2= relu(h2,hidden_size)
        print *, "model1 h6: shape(h1) =", shape(h1)
        call forward(h2, w6, b6,h1,input_size_layer,hidden_size)
        h1= relu(h1,hidden_size)
        input_size_layer = shape(h1)
        call forward(h1, w7, b7,y,input_size_layer,output_size)
      end function model1
      
      !subroutine model1(X, w1,w2,w3,w4,w5,w6,w7,b1,b2,b3,b4,b5,b6,b7, y)
      !  implicit none
      !  integer, parameter::input_size = 6
      !  integer, parameter::output_size = 1
      !  integer, parameter::hidden_layers = 5
      !  integer, parameter::hidden_size = 9
      !  integer::i
      !  real, dimension(1,input_size), intent(in) :: X
      !  real, dimension(hidden_size,input_size), intent(in) :: w1
      !  real, dimension(hidden_size,hidden_size), intent(in) :: w2,w3,
     *!  w4,w5,w6
      !  real, dimension(output_size,hidden_size), intent(in) :: w7
      !  
      !  real, dimension(1,hidden_size), intent(in) :: b1,b2,b3,b4,b5,b6
      !  real, dimension(1,output_size), intent(in) :: b7
      !  real, dimension(1,output_size), intent(out) :: y
      !  
      !  real, dimension(1,9) :: h1,h2,h3,h4,h5,h6
      !  real, dimension(1,1) :: h7
      !  
      !  !real, dimension(:,:), allocatable :: h1,h2,h3,h4,h5,h6,h7
      !  integer, dimension(2):: input_size_layer
      !  
      !  
      !  !allocate(h1(1,9))
      !  !allocate(h2(1,9))
      !  !allocate(h3(1,9))
      !  !allocate(h4(1,9))
      !  !allocate(h5(1,9))
      !  !allocate(h6(1,9))
      !  !allocate(h7(1,1))
      !
      !  print *, "model1: X=", X
      !  input_size_layer(1) = 1
      !  input_size_layer(2) = input_size
      !  
      !  !print *, "before forward shape(X)=",shape(X)
      !  call forward(X, w1, b1, h1,input_size_layer,hidden_size)
      !  !print *, "shape(h1)=",shape(h1)
      !  !print *, "model1: h1=", h1
      !  call relu(h1,hidden_size)
      !  !print *, "after relu shape(h1)=",shape(h1)
      !  !print *, "model1: after Relu h1=", h1
      !  !! print weight and bias
      !  !DO i=1,hidden_size
      !  !  print *, "model1: w1(",i,")=", w1(i,:)
      !  !END DO
      !  !DO i=1,hidden_size
      !  !  print *, "model1: b1(",i,")=", b1(1,i)
      !  !END DO
      !  ! reshape h1 to 1x9
      !  !reshape(h1, (/1,9/))
      !  
      !  input_size_layer = shape(h1)
      !  
      !  call forward(h1, w2, b2, h2,input_size_layer,hidden_size)
      !  !print *, "model1: h2=", h2
      !
      !  input_size_layer = shape(h2)
      !  call relu(h2,hidden_size)
      !  !print *, "model1: after Relu h2=", h2
      !  ! print weight and bias
      !  !DO i=1,hidden_size
      !  !  print *, "model1:h2 w2(",i,")=", w2(i,:)
      !  !END DO
      !  !DO i=1,hidden_size
      !  !  print *, "model1:h2 b2(",i,")=", b2(1,i)
      !  !END DO
      !  call forward(h2, w3, b3, h3,input_size_layer,hidden_size)
      !  !print *, "model1: h3=", h3
      !  call relu(h3,hidden_size)
      !  !print *, "model1: after Relu h3=", h3
      !  ! print weight and bias
      !  !DO i=1,hidden_size
      !  !  print *, "model1:h3 w3(",i,")=", w3(i,:)
      !  !END DO
      !  !DO i=1,hidden_size
      !  !  print *, "model1:h3 b3(",i,")=", b3(1,i)
      !  !END DO
      !
      !  call forward(h3, w4, b4, h4,input_size_layer,hidden_size)
      !  !print *, "model1: h4=", h4
      !  call relu(h4,hidden_size)
      !  !print *, "model1: after Relu h4=", h4
      !
      !  call forward(h4, w5, b5, h5,input_size_layer,hidden_size)
      !  !print *, "model1: h5=", h5
      !  call relu(h5,hidden_size)
      !  !print *, "model1: after Relu h5=", h5
      !
      !  call forward(h5, w6, b6, h6,input_size_layer,hidden_size)
      !  !print *, "model1: h6=", h6
      !
      !  call relu(h6,hidden_size)
      !  !print *, "model1: after Relu h6=", h6
      !  
      !  input_size_layer=shape(h6)
      !  call forward(h6, w7, b7, h7,input_size_layer,output_size)
      !  !print *, "model1: h7=", h7
      !  call relu(h7,output_size)
      !  !print *, "model1: after Relu h7=", h7
      !  
      !  y = h7
      !  print *, "model1: y=", y
      !  
      !  !deallocate(h1)
      !  !deallocate(h2)
      !  !deallocate(h3)
      !  !deallocate(h4)
      !  !deallocate(h5)
      !  !deallocate(h6)
      !  !deallocate(h7)
      !end subroutine model1
      
      !similarly create function for model2
      function model2(X,w1,w2,w3,w4,w5,b1,b2,b3,b4,b5) result(y)
        !implicit none
        integer, parameter::input_size = 6
        integer, parameter::output_size = 1
        integer, parameter::hidden_layers = 3
        integer, parameter::hidden_size = 9
        integer::i
        real, dimension(1,input_size), intent(in) :: X
        real, dimension(hidden_size,input_size), intent(in) :: w1
        real, dimension(hidden_size,hidden_size), intent(in) :: w2,w3,
     *    w4
        real, dimension(output_size,hidden_size), intent(in) :: w5
        real, dimension(1,hidden_size), intent(in) :: b1,b2,b3,b4
        real, dimension(1,output_size), intent(in) :: b5
        integer, dimension(2):: input_size_layer
        real, dimension(1,9) :: h1,h2
        !real, dimension(1,1),intent(out) :: y
        input_size_layer(1) = 1
        input_size_layer(2) = input_size
        print *, "model2 h1: shape(X)=", shape(X)
        print *, "model2 h1: shape(w1)  =", shape(w1)
        print *, "model2 h1: shape(b1) =", shape(b1)
        print *, "model2 h1: shape(output)=", shape(output)
        call forward(X, w1, b1,input_size_layer,hidden_size)
        h1= relu(h1,hidden_size)
        input_size_layer = shape(h1)
        call forward(h1, w2, b2,h2,input_size_layer,hidden_size)
        h2= relu(h2,hidden_size)
        call forward(h2, w3, b3,h1,input_size_layer,hidden_size)
        h1= relu(h1,hidden_size)
        call forward(h1, w4, b4,h2,input_size_layer,hidden_size)
        h2= relu(h2,hidden_size)
        input_size_layer = shape(h2)
        call forward(h2, w5, b5,y,input_size_layer,output_size)
      end function model2
      !subroutine model2(X,w1,w2,w3,w4,w5,b1,b2,b3,b4,b5,y)
      !  implicit none
      !  integer, parameter::input_size = 6
      !  integer, parameter::output_size = 1
      !  integer, parameter::hidden_layers = 3
      !  integer, parameter::hidden_size = 9
      !  real, dimension(1,input_size), intent(in) :: X
      !  real, dimension(hidden_size,input_size), intent(in) :: w1
      !  real, dimension(hidden_size,hidden_size), intent(in) :: w2,w3,w4
      !  real, dimension(output_size,hidden_size), intent(in) :: w5
      !  real, dimension(1,hidden_size), intent(in) :: b1,b2,b3,b4
      !  real, dimension(1,output_size), intent(in) :: b5
      !  real, dimension(1,output_size), intent(out) :: y
      !  
      !  real, dimension(1,9) :: h1,h2,h3,h4
      !  real, dimension(1,1) :: h5
      !  !real, dimension(:,:), allocatable :: h1,h2,h3,h4,h5
      !  integer, dimension(2):: input_size_layer
      !  
      !  !allocate(h1(1,9))
      !  !allocate(h2(1,9))
      !  !allocate(h3(1,9))
      !  !allocate(h4(1,9))
      !  !allocate(h5(1,1))
      !  
      !  print *, "model2: X=", X
      !  
      !  input_size_layer(1) = 1
      !  input_size_layer(2) = input_size
      !  call forward(X, w1, b1, h1,input_size_layer,hidden_size)
      !  !print *, "model2: h1=", h1
      !  call relu(h1,hidden_size)
      !  input_size_layer=shape(h1)
      !  !print *, "model2: after Relu h1=", h1
      !  call forward(h1, w2, b2, h2,input_size_layer,hidden_size)
      !  call relu(h2,hidden_size)
      !  !print *, "model2: after Relu h2=", h2
      !  call forward(h2, w3, b3, h3,input_size_layer,hidden_size)
      !  call relu(h3,hidden_size)
      !  !print *, "model2: after Relu h3=", h3
      !  call forward(h3, w4, b4, h4,input_size_layer,hidden_size)
      !  call relu(h4,hidden_size)
      !  !print *, "model2: after Relu h4=", h4
      !  input_size_layer=shape(h4)
      !  call forward(h4, w5, b5, h5,input_size_layer,output_size)
      !  call relu(h5,output_size)
      !  !print *, "model2: after Relu h5=", h5
      !  
      !  y = h5
      !  
      !  print *, "model2: y=", y
      !  
      !  !deallocate(h1)
      !  !deallocate(h2)
      !  !deallocate(h3)
      !  !deallocate(h4)
      !  !deallocate(h5)
      !end subroutine model2
      
    !  function forward(input, weight, bias, 
    ! *        input_size, output_size) result(output)
    !   implicit none
    !   integer, intent(in) :: input_size(2), output_size(1)
    !   real, dimension(input_size(1),input_size(2)), intent(in) ::input
    !    real, dimension(output_size(1),input_size(2)), 
    ! *     intent(in) :: weight
    !   real, dimension(1,output_size(1)), intent(in) :: bias
    !   real, dimension(1,output_size(1)), intent(out) :: output
    !   print *, "forward: shape(weight)=", size(weight)
    !   print *, "forward: shape(bias)=", size(bias)
    !   print *, "forward: shape(input)=", size(input)
    !   output = matmul(input, transpose(weight)) + bias
    !  end function forward

      
      !change forward to function
      !function forward(input, weight, bias,
     *!        input_size, output_size) result(output)
      !  !implicit none
      !  integer, intent(in) :: input_size(2), output_size(1)
      !  real, dimension(input_size(1),input_size(2)), intent(in) ::input
      !  real, dimension(output_size(1),input_size(2)),
     *!     intent(in) :: weight
      !  real, dimension(1,output_size(1)), intent(in) :: bias
      !  real, dimension(1,output_size(1)) :: tempoutput
      !  print *, "forward: shape(weight)=", shape(weight)
      !  print *, "forward: shape(bias)  =", shape(bias)
      !  print *, "forward: shape(input) =", shape(input)
      !  
      !  tempoutput = matmul(input, transpose(weight)) + bias
      !  output = tempoutput(1,:)
      !end function forward
      subroutine forward(input, weight, bias, output,
     *      input_size,output_size)
        implicit none
        integer, dimension(2),intent(in):: input_size
        integer, dimension(1),intent(in):: output_size
        
      
        real, dimension(input_size(1),input_size(2)), intent(in) ::input
        real, dimension(output_size(1),input_size(2)), 
     *     intent(in) :: weight
      
        real, dimension(output_size(1)), intent(in) :: bias
        real, dimension(1,output_size(1)), intent(out) :: output
        integer :: i,j,k
      
      
        
        
        !print *, "forward: input_size=", input_size
        !print *, "forward: output_size=", output_size
        ! print size of weight bias and input
        !print *, "forward: shape(weight)=", shape(weight)
        !print *, "forward: shape(bias)=", shape(bias)
        !print *, "forward: shape(input)=", shape(input)
      
        output = 0d0
        
        DO i=1,input_size(1)
              DO j=1, output_size(1)
                    DO k=1, input_size(1)
                          output(i,j) = output(i,j) 
     *                     + input(i,k)*weight(j,k)
                    end do
                    output(i,j) = output(i,j) 
     *                     + bias(j)
              end do
        end do
        
        
        
        ! Perform matrix multiplication and add bias
        !print *, "input=", input
        
        !output = matmul(input,(transpose(weight))) + bias
      end subroutine forward
      !function relu(x,input_size) result(y)
      !  !implicit none
      !  integer, dimension(1),intent(in):: input_size
      !  real, dimension(1,input_size(1)), intent(in) :: x
      !  !real, dimension(1,input_size(1)), intent(out) :: y
      !  !print *, "shape(x)=", size(x)
      !  ! Apply ReLU activation element-wise
      !  real, dimension(1,input_size(1)) :: y
      !  y=x
      !  do i = 1, input_size(1)
      !    if (y(1,i) < 0.0) then
      !      y(1,i) = 0.0
      !    end if
      !  end do
      !  
      !end function relu
      subroutine relu(x,input_size)
        integer, dimension(1),intent(in):: input_size
        real, dimension(1,input_size(1)), intent(inout) :: x
        !print *, "shape(x)=", size(x)
        ! Apply ReLU activation element-wise
        do i = 1, input_size(1)
          if (x(1,i) < 0.0) then
            x(1,i) = 0.0
          end if
        end do
      end subroutine relu