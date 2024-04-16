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
      integer, parameter::input_size = 6
      integer, parameter::output_size = 6
      integer, parameter::hidden_layers = 2
      integer, parameter::hidden_size = 9
      real, dimension( hidden_size,input_size)::w1 ! weight matrix from input layer to first hidden layer
      real, dimension(hidden_size)::b1 ! bias vector for first hidden layer
      real, dimension(hidden_size, hidden_size)::w2 ! weight matrix from hidden layer 1 to hidden layer 2
      real, dimension(hidden_size)::b2 ! bias vector for hidden layer 2
      real, dimension(hidden_size, hidden_size)::w3 ! weight matrix from hidden layer 2 to hidden layer 3
      real, dimension(hidden_size)::b3 ! bias vector for hidden layer 3
      real, dimension(output_size,hidden_size )::w4 ! weight matrix from last hidden layer to output layer
      real, dimension(output_size)::b4 ! bias vector for output layer  
      real(kind=8), DIMENSION(6)::B_sym
C
      PARAMETER(ZERO=0.D0, ONE=1.D0, TWO=2.D0, THREE=3.D0, FOUR=4.D0)
      
      real(kind=8), PARAMETER::eps_d=0.001D0
      real(kind=8), DIMENSION(6)::B_sym1, B_sym2, 
     *       B_sym3, B_sym4, B_sym5, B_sym6
      
      real(kind=8),DIMENSION(6)::sig, X
      real(kind=8),DIMENSION(6,6)::C4ten2,C4ten2_2
      real(kind=8),DIMENSION(3,3,3,3)::C4ten4
      real(kind=8), dimension(hidden_size) :: h1,h2,h3
        real(kind=8), dimension(hidden_size) :: 
     *        h1dash,h2dash,h3dash
        real(kind=8), dimension(hidden_size)::h2Cdash,h3Cdash
        real(kind=8), dimension(1,hidden_size) :: temp_vec1,temp_vec2
     *  ,temp_vec3,temp_vec4,temp_vec5,temp_vec6,temp_vec7,temp_vec8
     *  ,temp_vec9   
           real(kind=8), dimension(1,output_size) ::temp_vec10
      integer::I,j1,k1
      
 
      
     
     
      
      
            
! model1_input_layer.weight
      ! model__input_layer.weight
      w1(1,1) = 3.407334089279175d0
      w1(1,2) = -0.4182116389274597d0
      w1(1,3) = -0.4993583858013153d0
      w1(1,4) = -0.13304957747459412d0
      w1(1,5) = -0.7931703925132751d0
      w1(1,6) = 0.26733145117759705d0
      w1(2,1) = 0.5032358169555664d0
      w1(2,2) = -1.7321547269821167d0
      w1(2,3) = -1.7718007564544678d0
      w1(2,4) = -0.8844786882400513d0
      w1(2,5) = -0.9059313535690308d0
      w1(2,6) = -0.6282860040664673d0
      w1(3,1) = -0.5753132700920105d0
      w1(3,2) = 0.4047350287437439d0
      w1(3,3) = -2.237666368484497d0
      w1(3,4) = -0.18837155401706696d0
      w1(3,5) = 3.1599111557006836d0
      w1(3,6) = -0.06372510641813278d0
      w1(4,1) = 1.365587592124939d0
      w1(4,2) = -1.1601035594940186d0
      w1(4,3) = -2.319704294204712d0
      w1(4,4) = -0.8013031482696533d0
      w1(4,5) = 1.9938615560531616d0
      w1(4,6) = -0.4849841594696045d0
      w1(5,1) = 0.8580678105354309d0
      w1(5,2) = -2.855193614959717d0
      w1(5,3) = 0.7624449133872986d0
      w1(5,4) = 0.16956372559070587d0
      w1(5,5) = 0.6573392152786255d0
      w1(5,6) = -407.580810546875d0
      w1(6,1) = 1.4904086589813232d0
      w1(6,2) = -0.31472867727279663d0
      w1(6,3) = -0.6160460114479065d0
      w1(6,4) = 1.716835379600525d0
      w1(6,5) = 1.498837947845459d0
      w1(6,6) = -350.02264404296875d0
      w1(7,1) = 2.6778595447540283d0
      w1(7,2) = -1.2927944660186768d0
      w1(7,3) = -0.057322416454553604d0
      w1(7,4) = -0.46671754121780396d0
      w1(7,5) = -2.3443117141723633d0
      w1(7,6) = -1.3006739616394043d0
      w1(8,1) = 1.997135877609253d0
      w1(8,2) = -2.810582160949707d0
      w1(8,3) = 1.2051196098327637d0
      w1(8,4) = 0.07865329086780548d0
      w1(8,5) = -366.534423828125d0
      w1(8,6) = -401.2370300292969d0
      w1(9,1) = -0.2512248754501343d0
      w1(9,2) = -3.3456387519836426d0
      w1(9,3) = -3.2859201431274414d0
      w1(9,4) = 1.9792659282684326d0
      w1(9,5) = -0.9718766212463379d0
      w1(9,6) = -1.2507286071777344d0
! model__input_layer.bias
      b1(1) = 0.8735653758049011d0
      b1(2) = 0.21007300913333893d0
      b1(3) = 0.6447025537490845d0
      b1(4) = 2.615753412246704d0
      b1(5) = -0.22908252477645874d0
      b1(6) = -1.078667402267456d0
      b1(7) = 1.0159462690353394d0
      b1(8) = -0.32115522027015686d0
      b1(9) = -0.36063188314437866d0
! model__hidden_layers.0.weight
      w2(1,1) = 1.1385537385940552d0
      w2(1,2) = 0.7523328065872192d0
      w2(1,3) = 1.0891200304031372d0
      w2(1,4) = -0.8318615555763245d0
      w2(1,5) = 1.8774296045303345d0
      w2(1,6) = -0.0937015637755394d0
      w2(1,7) = 1.9977134466171265d0
      w2(1,8) = 3.0380899906158447d0
      w2(1,9) = -9.604934692382812d0
      w2(2,1) = 0.12119719386100769d0
      w2(2,2) = -0.16697494685649872d0
      w2(2,3) = 0.24650457501411438d0
      w2(2,4) = -0.25735291838645935d0
      w2(2,5) = -0.12438949942588806d0
      w2(2,6) = -0.09440228343009949d0
      w2(2,7) = 0.28049013018608093d0
      w2(2,8) = -0.28131935000419617d0
      w2(2,9) = -0.011764436960220337d0
      w2(3,1) = 0.3079645335674286d0
      w2(3,2) = -0.5741782784461975d0
      w2(3,3) = 3.4605555534362793d0
      w2(3,4) = 1.1876733303070068d0
      w2(3,5) = 1.5103669166564941d0
      w2(3,6) = 0.32169678807258606d0
      w2(3,7) = 1.5316418409347534d0
      w2(3,8) = 1.4126684665679932d0
      w2(3,9) = 0.7611626982688904d0
      w2(4,1) = -0.30335792899131775d0
      w2(4,2) = 0.2644161283969879d0
      w2(4,3) = 0.01081123948097229d0
      w2(4,4) = -0.18054966628551483d0
      w2(4,5) = 0.13054239749908447d0
      w2(4,6) = -0.3286917209625244d0
      w2(4,7) = -0.05917230248451233d0
      w2(4,8) = -0.01457059383392334d0
      w2(4,9) = -0.11606495082378387d0
      w2(5,1) = 1.9402234554290771d0
      w2(5,2) = -0.3645310699939728d0
      w2(5,3) = -0.6485813856124878d0
      w2(5,4) = 0.8810017704963684d0
      w2(5,5) = 1.748274326324463d0
      w2(5,6) = 2.9455621242523193d0
      w2(5,7) = 2.749920606613159d0
      w2(5,8) = 3.5446584224700928d0
      w2(5,9) = 0.5823633670806885d0
      w2(6,1) = 0.18518486618995667d0
      w2(6,2) = -0.22875618934631348d0
      w2(6,3) = -0.08017241954803467d0
      w2(6,4) = -0.10907232761383057d0
      w2(6,5) = -0.07086539268493652d0
      w2(6,6) = -0.12925755977630615d0
      w2(6,7) = 0.002943366765975952d0
      w2(6,8) = 0.05601587891578674d0
      w2(6,9) = -0.17904019355773926d0
      w2(7,1) = 1.7701537609100342d0
      w2(7,2) = -3.670283079147339d0
      w2(7,3) = 1.0044249296188354d0
      w2(7,4) = -0.18567036092281342d0
      w2(7,5) = 1.1106715202331543d0
      w2(7,6) = 2.271850824356079d0
      w2(7,7) = 1.8966865539550781d0
      w2(7,8) = 0.47622233629226685d0
      w2(7,9) = -0.16616514325141907d0
      w2(8,1) = 0.02089778147637844d0
      w2(8,2) = 2.073923110961914d0
      w2(8,3) = 2.0516505241394043d0
      w2(8,4) = 0.22022660076618195d0
      w2(8,5) = -0.17315803468227386d0
      w2(8,6) = 1.7039581537246704d0
      w2(8,7) = 2.208906888961792d0
      w2(8,8) = 1.7657707929611206d0
      w2(8,9) = -5.589003562927246d0
      w2(9,1) = 0.8739873766899109d0
      w2(9,2) = 1.5373060703277588d0
      w2(9,3) = 1.2577712535858154d0
      w2(9,4) = 1.4478930234909058d0
      w2(9,5) = 3.0366010665893555d0
      w2(9,6) = -1.8282389640808105d0
      w2(9,7) = 1.9224021434783936d0
      w2(9,8) = 1.392270565032959d0
      w2(9,9) = 0.07862132042646408d0
! model__hidden_layers.0.bias
      b2(1) = -2.541232109069824d0
      b2(2) = -0.29943928122520447d0
      b2(3) = -2.0156359672546387d0
      b2(4) = -0.006906980182975531d0
      b2(5) = -1.5532371997833252d0
      b2(6) = -0.15573129057884216d0
      b2(7) = -3.291851758956909d0
      b2(8) = -5.033766746520996d0
      b2(9) = -0.04665927588939667d0
! model__hidden_layers.1.weight
      w3(1,1) = -3.14550518989563d0
      w3(1,2) = 5.966382026672363d0
      w3(1,3) = 0.883809506893158d0
      w3(1,4) = -3.1009044647216797d0
      w3(1,5) = -0.43706411123275757d0
      w3(1,6) = -4.154470920562744d0
      w3(1,7) = -0.5348560214042664d0
      w3(1,8) = 2.7290024757385254d0
      w3(1,9) = -0.6214843392372131d0
      w3(2,1) = 9.76736068725586d0
      w3(2,2) = -2.118011951446533d0
      w3(2,3) = 0.49615606665611267d0
      w3(2,4) = -2.6850974559783936d0
      w3(2,5) = 1.9968425035476685d0
      w3(2,6) = -0.9336773753166199d0
      w3(2,7) = -0.44959327578544617d0
      w3(2,8) = 0.7774703502655029d0
      w3(2,9) = 0.6771952509880066d0
      w3(3,1) = 6.475264072418213d0
      w3(3,2) = -1.6520453691482544d0
      w3(3,3) = -0.31405091285705566d0
      w3(3,4) = -1.4116824865341187d0
      w3(3,5) = 1.101027250289917d0
      w3(3,6) = -3.2300474643707275d0
      w3(3,7) = 1.6085753440856934d0
      w3(3,8) = 3.680652618408203d0
      w3(3,9) = -0.9384092092514038d0
      w3(4,1) = -1.9165598154067993d0
      w3(4,2) = 1.8822596073150635d0
      w3(4,3) = -0.860019326210022d0
      w3(4,4) = 2.771336793899536d0
      w3(4,5) = -0.706590473651886d0
      w3(4,6) = -3.695054531097412d0
      w3(4,7) = 2.5927276611328125d0
      w3(4,8) = -0.566609799861908d0
      w3(4,9) = 0.7212762236595154d0
      w3(5,1) = 5.786647796630859d0
      w3(5,2) = 1.0273350477218628d0
      w3(5,3) = 1.2349215745925903d0
      w3(5,4) = -1.0762310028076172d0
      w3(5,5) = 2.4391584396362305d0
      w3(5,6) = -11.933103561401367d0
      w3(5,7) = 0.0008325494127348065d0
      w3(5,8) = 5.213501930236816d0
      w3(5,9) = 0.29250019788742065d0
      w3(6,1) = 3.763784646987915d0
      w3(6,2) = -1.6028419733047485d0
      w3(6,3) = 2.258195638656616d0
      w3(6,4) = -2.661146640777588d0
      w3(6,5) = -0.06131453439593315d0
      w3(6,6) = 6.669304370880127d0
      w3(6,7) = -0.047758232802152634d0
      w3(6,8) = 2.9969053268432617d0
      w3(6,9) = 0.2603788673877716d0
      w3(7,1) = -0.016917914152145386d0
      w3(7,2) = -0.05855238437652588d0
      w3(7,3) = -0.24886667728424072d0
      w3(7,4) = -0.23636244237422943d0
      w3(7,5) = -0.27472466230392456d0
      w3(7,6) = 0.2277940809726715d0
      w3(7,7) = -0.001433090423233807d0
      w3(7,8) = 0.328764945268631d0
      w3(7,9) = 0.14996294677257538d0
      w3(8,1) = 2.8715944290161133d0
      w3(8,2) = -0.0994054526090622d0
      w3(8,3) = 2.8764116764068604d0
      w3(8,4) = 2.5365917682647705d0
      w3(8,5) = -0.2450849413871765d0
      w3(8,6) = -2.1205224990844727d0
      w3(8,7) = 2.1629538536071777d0
      w3(8,8) = 5.304382801055908d0
      w3(8,9) = -1.7176204919815063d0
      w3(9,1) = 0.34061145782470703d0
      w3(9,2) = -4.844893932342529d0
      w3(9,3) = 1.6842975616455078d0
      w3(9,4) = 4.820822715759277d0
      w3(9,5) = -3.2893826961517334d0
      w3(9,6) = -11.454957008361816d0
      w3(9,7) = 2.525775909423828d0
      w3(9,8) = 17.832002639770508d0
      w3(9,9) = 0.7062625288963318d0
! model__hidden_layers.1.bias
      b3(1) = 13.526472091674805d0
      b3(2) = -1.674777865409851d0
      b3(3) = 0.8847312927246094d0
      b3(4) = -0.30614954233169556d0
      b3(5) = -6.737293243408203d0
      b3(6) = -9.999174118041992d0
      b3(7) = -0.13825079798698425d0
      b3(8) = -13.129894256591797d0
      b3(9) = -0.27022916078567505d0
! model__output_layer.weight
      w4(1,1) = -3.9059083461761475d0
      w4(1,2) = 3.5101335048675537d0
      w4(1,3) = 2.7715682983398438d0
      w4(1,4) = -11.580759048461914d0
      w4(1,5) = 2.9649031162261963d0
      w4(1,6) = -0.308685302734375d0
      w4(1,7) = 1.297293782234192d0
      w4(1,8) = 4.698578357696533d0
      w4(1,9) = -1.7805887460708618d0
      w4(2,1) = 13.014608383178711d0
      w4(2,2) = 1.5054150819778442d0
      w4(2,3) = 2.531209707260132d0
      w4(2,4) = -1.0354290008544922d0
      w4(2,5) = -0.4955916404724121d0
      w4(2,6) = 1.6524032354354858d0
      w4(2,7) = 3.6080126762390137d0
      w4(2,8) = 0.5414814352989197d0
      w4(2,9) = -2.641152858734131d0
      w4(3,1) = 12.217340469360352d0
      w4(3,2) = 1.7344576120376587d0
      w4(3,3) = 0.46104127168655396d0
      w4(3,4) = -2.650474786758423d0
      w4(3,5) = 0.7457412481307983d0
      w4(3,6) = -4.113423824310303d0
      w4(3,7) = -0.6562127470970154d0
      w4(3,8) = 7.794531345367432d0
      w4(3,9) = -4.330392360687256d0
      w4(4,1) = -1.250789761543274d0
      w4(4,2) = -0.7125986218452454d0
      w4(4,3) = -0.7736987471580505d0
      w4(4,4) = -2.259345293045044d0
      w4(4,5) = 0.1652505248785019d0
      w4(4,6) = 6.010852813720703d0
      w4(4,7) = 4.116481304168701d0
      w4(4,8) = -4.597085952758789d0
      w4(4,9) = 0.5693235993385315d0
      w4(5,1) = 0.13509106636047363d0
      w4(5,2) = 0.6792765855789185d0
      w4(5,3) = -1.7798700332641602d0
      w4(5,4) = -1.0587148666381836d0
      w4(5,5) = -0.41459789872169495d0
      w4(5,6) = 0.13307803869247437d0
      w4(5,7) = 6.436569690704346d0
      w4(5,8) = 0.14433535933494568d0
      w4(5,9) = 0.5849194526672363d0
      w4(6,1) = 0.42283838987350464d0
      w4(6,2) = 0.7783148288726807d0
      w4(6,3) = -0.2773628830909729d0
      w4(6,4) = 0.33211570978164673d0
      w4(6,5) = -0.47028830647468567d0
      w4(6,6) = 0.28433987498283386d0
      w4(6,7) = -0.4176706075668335d0
      w4(6,8) = -0.11567149311304092d0
      w4(6,9) = 0.057874664664268494d0
! model__output_layer.bias
      b4(1) = 1.1536800861358643d0
      b4(2) = -2.2567758560180664d0
      b4(3) = 35.655696868896484d0
      b4(4) = 17.186620712280273d0
      b4(5) = -2.1931962966918945d0
      b4(6) = -5.648481845855713d0



      
      
      
      
      !B_sym(1)=DFGRD1(1, 1)**2+DFGRD1(1, 2)**2+DFGRD1(1, 3)**2
      !B_sym(2)=DFGRD1(2, 1)**2+DFGRD1(2, 2)**2+DFGRD1(2, 3)**2
      !B_sym(3)=DFGRD1(3, 3)**2+DFGRD1(3, 1)**2+DFGRD1(3, 2)**2
      !B_sym(4)=DFGRD1(1, 1)*DFGRD1(2, 1)+DFGRD1(1, 2)*DFGRD1(2, 2)
     1!       +DFGRD1(1, 3)*DFGRD1(2, 3)
      !IF(NSHR.EQ.3) THEN
      !  B_sym(5)=DFGRD1(1, 1)*DFGRD1(3, 1)+DFGRD1(1, 2)*DFGRD1(3, 2)
     1!         +DFGRD1(1, 3)*DFGRD1(3, 3)
      !  B_sym(6)=DFGRD1(2, 1)*DFGRD1(3, 1)+DFGRD1(2, 2)*DFGRD1(3, 2)
     1!         +DFGRD1(2, 3)*DFGRD1(3, 3)
      !END IF
      
      B_sym1(1)=DFGRD1(1, 1)**2+DFGRD1(1, 2)**2+DFGRD1(1, 3)**2
      B_sym1(2)=DFGRD1(2, 1)**2+DFGRD1(2, 2)**2+DFGRD1(2, 3)**2
      B_sym1(3)=DFGRD1(3, 3)**2+DFGRD1(3, 1)**2+DFGRD1(3, 2)**2
      B_sym1(4)=DFGRD1(1, 1)*DFGRD1(2, 1)+DFGRD1(1, 2)*DFGRD1(2, 2)
     1       +DFGRD1(1, 3)*DFGRD1(2, 3)
      IF(NSHR.EQ.3) THEN
        B_sym1(5)=DFGRD1(1, 1)*DFGRD1(3, 1)+DFGRD1(1, 2)*DFGRD1(3, 2)
     1         +DFGRD1(1, 3)*DFGRD1(3, 3)
        B_sym1(6)=DFGRD1(2, 1)*DFGRD1(3, 1)+DFGRD1(2, 2)*DFGRD1(3, 2)
     1         +DFGRD1(2, 3)*DFGRD1(3, 3)
      END IF
      
      
            
      
      B_sym(1)=DFGRD0(1, 1)**2+DFGRD0(1, 2)**2+DFGRD0(1, 3)**2
      B_sym(2)=DFGRD0(2, 1)**2+DFGRD0(2, 2)**2+DFGRD0(2, 3)**2
      B_sym(3)=DFGRD0(3, 3)**2+DFGRD0(3, 1)**2+DFGRD0(3, 2)**2
      B_sym(4)=DFGRD0(1, 1)*DFGRD0(2, 1)+DFGRD0(1, 2)*DFGRD0(2, 2)
     1       +DFGRD0(1, 3)*DFGRD0(2, 3)
      IF(NSHR.EQ.3) THEN
        B_sym(5)=DFGRD0(1, 1)*DFGRD0(3, 1)+DFGRD0(1, 2)*DFGRD0(3, 2)
     1         + DFGRD0(1, 3)*DFGRD0(3, 3)
        B_sym(6)=DFGRD0(2, 1)*DFGRD0(3, 1)+DFGRD0(2, 2)*DFGRD0(3, 2)
     1         + DFGRD0(2, 3)*DFGRD0(3, 3)
      END IF
      
      !DO K1=1,3
      !      DSTRAN(K1) = (B_sym1(K1)-one)*0.5D0
      !      DSTRAN(K1+3) = (B_sym1(K1))*0.5D0
      !end do
      !DO K1=1,6
      !      STRAN(K1) = STRAN(K1) + DSTRAN(K1)
      !end do
      
      
      
      
      print *, "Strain=",STRAN
      print *, "DStrain=",DSTRAN
      print *, "B_sym=",B_sym


            
      
      ! stress values
      

      !sig = model(STRAN, w1, w2,w3,w4,b1,b2,b3,b4)
      sig = 0.0
      print *, "shape(STRAN)=", shape(STRAN)
      print *, "shape(sig)=", shape(sig)

      ! call model1(STRAN, w1, w2,w3,w4,b1,b2,b3,b4,sig)
      
      !   call forward(STRAN, w1, b1,h1,6,9)
      X = STRAN + DSTRAN
      h1 = 0.0
      DO j1=1, 9
          DO k1=1, 6
                
                h1(j1) = h1(j1) 
     *                     + X(k1)*w1(j1,k1)
          end do
          
          h1(j1) = h1(j1) 
     *                     + b1(j1)
      end do
      
      call relu2(h1,h1dash,9)
        
      !   call forward(h1dash, w2, b2,h2,9,9)
        h2 = 0.0
      DO j1=1, 9
          DO k1=1, 9
                h2(j1) = h2(j1) 
     *                     + h1dash(k1)*w2(j1,k1)
          end do
          h2(j1) = h2(j1) 
     *                     + b2(j1)
      end do
        call relu2(h2,h2dash,9)
      
       
        h2Cdash = h2dash + h1dash
        
      !   call forward(h2Cdash, w3, b3,h3,9,9)
        h3 = 0.0
      DO j1=1, 9
          DO k1=1, 9
                h3(j1) = h3(j1) 
     *                     + h2Cdash(k1)*w3(j1,k1)
          end do
          h3(j1) = h3(j1) 
     *                     + b3(j1)
      end do
        call relu2(h3,h3dash,9)
      
           
        h3Cdash = h3dash + h2Cdash
          
      !   call forward(h3Cdash, w4, b4,sig,9,6)
      sig = 0.0
      DO j1=1, 6
          DO k1=1, 9
                sig(j1) = sig(j1)
     *                     + h3Cdash(k1)*w4(j1,k1)
          end do
          sig(j1) = sig(j1)
     *                     + b4(j1)
      end do
     
     


      ! testing derivatives

!       call derivative(STRAN+DSTRAN, w1, w2,
!      * w3, w4, 
!      *  b1, b2, b3, b4, C4ten2)

      DO I=1,output_size
            temp_vec1(1,:)= w4(I,:)
            call relu_derivative2(h3,temp_vec2,1,hidden_size)
            call linearMultiply2(temp_vec1,temp_vec2,hidden_size,temp_vec3)
            !call MatrixMultiply2(temp_vec3,w3,shape(temp_vec3),shape(w3)
     *      !     ,temp_vec4)
            temp_vec4 = matmul(temp_vec3,w3)
            temp_vec4 = temp_vec1 + temp_vec4
            call relu_derivative2(h2,temp_vec5,1,hidden_size)
            call linearMultiply2(temp_vec4,temp_vec5,hidden_size,temp_vec6)
            !call MatrixMultiply2(temp_vec6,w2,shape(temp_vec6),shape(w2)
     *      !     ,temp_vec7)
            temp_vec7 = matmul(temp_vec6,w2)
            
            temp_vec7 = temp_vec4 + temp_vec7
            
            call relu_derivative2(h1,temp_vec8,1,hidden_size)
            call linearMultiply2(temp_vec7,temp_vec8,hidden_size,temp_vec9)
            !call MatrixMultiply2(temp_vec9,w1,shape(temp_vec9),shape(w1)
     *      !     ,temp_vec10)
            temp_vec10 = matmul(temp_vec9,w1)
            C4ten2(I,:) =  temp_vec10(1,:)
      END DO
      
      
      
      
      STRESS=sig
      
      
      
    
      
      
      
      ! stiffness part
      C4ten2 = transpose(C4ten2)
      !call get_4thOrderC(stress,DFGRD1,C4ten2,C4ten4) 
      !call convert4th2second(C4ten4,C4ten2_2)
      DDSDDE = C4ten2
      
      


      
      
      
      


      
      do K1=1,3
            print *, "F(",K1,",:)=",DFGRD1(K1,:)
      end do
      DO K1=1,NTENS
            print *,"STRESS(",K1,")=", STRESS(K1)
      END DO
      DO K1=1, NTENS
        print *,"DDSDDE(",K1,",:)=", DDSDDE(K1, :)    
      END DO
      
      
      
      

C
      RETURN
      END
      
      real(kind=8) function krnocker(i,j)
            integer, intent(in)::i,j
            !real(kind=8)::krnocker
            if (i==j) then
              krnocker = 1.0
            else
              krnocker = 0.0
            end if
      return 
      end
      
      real(kind=8) function eye4(i,j,k,l)
            integer, intent(in)::i,j,k,l
            !real(kind=8)::eye4
            real(kind=8)::krnocker
            eye4 = krnocker(i,k)*krnocker(j,l)
      return
      end
      real(kind=8) function dBdF(F,i,j,k,l)
            real(kind=8),dimension(3,3),intent(in)::F
            integer, intent(in)::i,j,k,l
            real(kind=8)::eye4
            integer::m
            dBdF=0.0
            !print *, "eye4(",i,",",j,",",k,",",l,")=",eye4(i,j,k,l)
            do m=1,3
                  dBdF = dBdF + eye4(i,j,k,m)*F(l,m)+
     *                         F(i,m)*eye4(m,j,k,l)
            end do
            !print *, "dBdF(",i,",",j,",",k,",",l,")=",dBdF
      
      return      
      end
      
     
      
      subroutine model1(X,w_in1,w_in2,w_in3,
     *       w_in4,b_in1,b_in2,b_in3,b_in4,y)
        implicit none
        integer, parameter::input_size = 6
        integer, parameter::output_size = 6
        integer, parameter::hidden_size = 9

        real(kind=8), dimension(input_size), intent(in) :: X
        real(kind=8), dimension(hidden_size,input_size) ::
     *      w_in1
        real(kind=8),dimension(hidden_size,hidden_size),intent(in)::
     *      w_in2,w_in3
        real(kind=8), dimension(output_size,hidden_size), intent(in)::
     *      w_in4
        real(kind=8), dimension(hidden_size), intent(in) :: b_in1,b_in2,b_in3
        real(kind=8), dimension(output_size), intent(in) :: b_in4
        real(kind=8), dimension(output_size), intent(out) :: y
        real(kind=8), dimension(hidden_size) :: h1,h2,h3
        real(kind=8), dimension(hidden_size) :: 
     *        h1dash,h2dash,h3dash
        real(kind=8), dimension(hidden_size)::h2Cdash,h3Cdash
        

        print *, "b1=", b_in1
        print *, "X=", X
        print *, "shape(X)=", shape(X)
        print *, "shape(w1)= ", shape(w_in1)
            print *, "shape(w2)= ", shape(w_in2)
            print *, "shape(w3)= ", shape(w_in3)
            print *, "shape(w4)= ", shape(w_in4)
            print *, "shape(b1)= ", shape(b_in1)
            print *, "shape(b2)= ", shape(b_in2)
            print *, "shape(b3)= ", shape(b_in3)
            print *, "shape(b4)= ", shape(b_in4)
        print *, "loc(w1)=", loc(w_in1)
        print *, "loc(w2)=", loc(w_in2)
        print *, "loc(w3)=", loc(w_in3)
        print *, "loc(w4)=", loc(w_in4)
        print *, "loc(b1)=", loc(b_in1)
        print *, "loc(b2)=", loc(b_in2)
        print *, "loc(b3)=", loc(b_in3)
        print *, "loc(b4)=", loc(b_in4)
        print *, "w1=", w_in1
        print *, "b1=", b_in1
        print *, "w2=", w_in2
        print *, "b2=", b_in2
        print *, "w3=", w_in3
        print *, "b3=", b_in3
        print *, "w4=", w_in4
        print *, "b4=", b_in4
        !call forward(X, w_in1, b_in1,h1,input_size,hidden_size)
        !call relu2(h1,h1dash,size(h1))
        !print *, "h1dash=", h1dash
        !call forward(h1dash, w_in2, b_in2,h2,hidden_size,hidden_size)
        !call relu2(h2,h2dash,size(h2))
      !
        !print *, "h2dash=", h2dash
        !h2Cdash = h2dash + h1dash
        !print *, "h2Cdash=", h2Cdash
        !call forward(h2Cdash, w_in3, b_in3,h3,hidden_size,hidden_size)
        !call relu2(h3,h3dash,size(h3))
      !
        !    print *, "h3=", h3
        !h3Cdash = h3dash + h2dash
        !    print *, "h3=", h3
        !call forward(h3Cdash, w_in4, b_in4,y,hidden_size,output_size)
        y = 0.0
        print *, "y=", y
        
      end subroutine model1      
      
      
        
        
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
        implicit none
        integer,intent(in):: input_size
        real(kind=8), dimension(input_size), intent(in) :: x
        real(kind=8), dimension(input_size), intent(out) :: y
        integer::i
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
         integer :: i
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
      
      
      
      subroutine derivative(x,w1,w2,w3,w4,b1,b2,b3,b4,y)
        integer, parameter::input_size = 6
        integer, parameter::output_size = 6
        integer, parameter::hidden_layers = 2
        integer, parameter::hidden_size = 9
        real(kind=8), dimension(input_size), intent(in) :: x
        real(kind=8), 
     *   dimension(hidden_size,input_size), intent(in) :: w1
        real(kind=8), 
     *    dimension(hidden_size,hidden_size), intent(in) :: w2,w3
        real(kind=8), dimension(output_size,hidden_size),intent(in):: w4
        real(kind=8), dimension(hidden_size), intent(in) :: b1,b2,b3
        real(kind=8), dimension(output_size), intent(in) :: b4
        real(kind=8), dimension(output_size,output_size), intent(out)::y
        real(kind=8), dimension(hidden_size) :: y1,y2,y3
        real(kind=8), 
     *   dimension(hidden_size):: y1Hat,y2Hat,y3Hat
        real(kind=8), dimension(1,hidden_size) :: temp_vec1,temp_vec2
     *  ,temp_vec3,temp_vec4,temp_vec5,temp_vec6,temp_vec7,temp_vec8
     *  ,temp_vec9   
        real(kind=8), dimension(1,output_size) ::temp_vec10
        
        integer:: i
        
        !print *, "shape(x)=",shape(x)
        !print *, "shape(w1)=",shape(w1)
        !print *, "shape(b1)=",shape(b1)
        !print *, "shape(y1)=",shape(y1)
        call forward(x,w1,b1,y1,input_size,hidden_size)
        !print *, "y1=",y1
        call relu2(y1,y1Hat,hidden_size)
        !print *, "y1Hat=",y1Hat
        call forward(y1Hat,w2,b2,y2,hidden_size,hidden_size)
        !print *, "y2=",y2
        call relu2(y2,y2Hat,hidden_size)
        y2Hat = y2Hat + y1Hat
        !print *, "y2Hat=",y2Hat
        call forward(y2Hat,w3,b3,y3,hidden_size,hidden_size)
        !print *, "y3=",y3
        call relu2(y3,y3Hat,hidden_size)
        y3Hat = y3Hat + y2Hat
        !print *, "y3Hat=",y3Hat
        call forward(y3Hat,w4,b4,y4,output_size,hidden_size)
        
        DO I=1,output_size
            temp_vec1(1,:)= w4(I,:)
            call relu_derivative2(y3,temp_vec2,1,hidden_size)
            call linearMultiply2(temp_vec1,temp_vec2,hidden_size,temp_vec3)
            call MatrixMultiply2(temp_vec3,w3,shape(temp_vec3),shape(w3)
     *           ,temp_vec4)
            temp_vec4 = temp_vec1 + temp_vec4
            call relu_derivative2(y2,temp_vec5,1,hidden_size)
            call linearMultiply2(temp_vec4,temp_vec5,hidden_size,temp_vec6)
            call MatrixMultiply2(temp_vec6,w2,shape(temp_vec6),shape(w2)
     *           ,temp_vec7)
            temp_vec7 = temp_vec4 + temp_vec7
            call relu_derivative2(y1,temp_vec8,1,hidden_size)
            call linearMultiply2(temp_vec7,temp_vec8,hidden_size,temp_vec9)
            call MatrixMultiply2(temp_vec9,w1,shape(temp_vec9),shape(w1)
     *           ,temp_vec10)
            y(I,:) =  temp_vec10(I,:)
        END DO
        
        
        
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

      subroutine convert4th2second(ten4,ten2)
            real(kind=8),dimension(3,3,3,3),intent(in)::ten4
            real(kind=8),dimension(6,6),intent(out)::ten2
            ten2 = 0.0
            ten2(1,1) = ten4(1,1,1,1)
            ten2(1,2) = ten4(1,1,2,2)
            ten2(1,3) = ten4(1,1,3,3)
            ten2(1,4) = ten4(1,1,1,2)
            ten2(1,5) = ten4(1,1,1,3)
            ten2(1,6) = ten4(1,1,2,3)
            
            ten2(2,1) = ten4(2,2,1,1)
            ten2(2,2) = ten4(2,2,2,2)
            ten2(2,3) = ten4(2,2,3,3)
            ten2(2,4) = ten4(2,2,1,2)
            ten2(2,5) = ten4(2,2,1,3)
            ten2(2,6) = ten4(2,2,2,3)
            
            ten2(3,1) = ten4(3,3,1,1)
            ten2(3,2) = ten4(3,3,2,2)
            ten2(3,3) = ten4(3,3,3,3)
            ten2(3,4) = ten4(3,3,1,2)
            ten2(3,5) = ten4(3,3,1,3)
            ten2(3,6) = ten4(3,3,2,3)
            
            ten2(4,1) = ten4(1,2,1,1)
            ten2(4,2) = ten4(1,2,2,2)
            ten2(4,3) = ten4(1,2,3,3)
            ten2(4,4) = ten4(1,2,1,2)
            ten2(4,5) = ten4(1,2,1,3)
            ten2(4,6) = ten4(1,2,2,3)
            
            ten2(5,1) = ten4(1,3,1,1)
            ten2(5,2) = ten4(1,3,2,2)
            ten2(5,3) = ten4(1,3,3,3)
            ten2(5,4) = ten4(1,3,1,2)
            ten2(5,5) = ten4(1,3,1,3)
            ten2(5,6) = ten4(1,3,2,3)
            
            ten2(6,1) = ten4(2,3,1,1)
            ten2(6,2) = ten4(2,3,2,2)
            ten2(6,3) = ten4(2,3,3,3)
            ten2(6,4) = ten4(2,3,1,2)
            ten2(6,5) = ten4(2,3,1,3)
            ten2(6,6) = ten4(2,3,2,3)
         end subroutine convert4th2second
         subroutine get_4thOrderC(sigma,F,C4,output)
           real(kind=8),dimension(6),intent(in)::sigma
           real(kind=8),dimension(6,6),intent(in)::C4
           real(kind=8),dimension(3,3),intent(in)::F
           real(kind=8),dimension(3,3,3,3),intent(out)::output
           real(kind=8),dimension(3,3)::sigmaT
           real(kind=8),dimension(3,3,3,3)::term1
           real(kind=8),dimension(3,3,3,3)::dsigmadF,dsigmadB
           integer::i,j,k,l,a
           real(kind=8):: krnocker
           sigMat(1,1) = sigma(1)
           sigMat(2,2) = sigma(2)
           sigMat(3,3) = sigma(3)
           sigMat(1,2) = sigma(4)
           sigMat(1,3) = sigma(5)
           sigMat(2,3) = sigma(6)
           sigMat(2,1) = sigMat(1,2)
           sigMat(3,1) = sigMat(1,3)
           sigMat(3,2) = sigMat(2,3)
           call ten2fourthOrderArray(C4,dsigmadB)
           call stressProdTerm1(dsigmadB,F,dsigmadF)
           output = 0.0
           do i=1,3
                 do j=1,3
                       do k=1,3
                             do l=1,3
                                   output(i,j,k,l)=0.0
         !print *, "dsigmadF(",i,",",j,",",k,",",l,")=",dsigmadF(i,j,k,l)
         !print *, "dsigmadB(",i,",",j,",",k,",",l,")=",dsigmadB(i,j,k,l)
                                   do a=1,3
                                         output(i,j,k,l) = output(i,j,k,l)+
     *                                 sigMat(i,j)*krnocker(k,l)+
     *                                 0.5d0 *(dsigmadF(i,j,k,a)*F(l,a)+
     *                                         dsigmadF(i,j,l,a)*F(k,a))
                                   end do
                             end do
                       end do
                 end do
           end do                              
           
         end subroutine get_4thOrderC
         
         
         subroutine stressProdTerm1(dsigmadB,F,output)
            real(kind=8),dimension(3,3,3,3),intent(in)::dsigmadB
            real(kind=8),dimension(3,3),intent(in)::F
            real(kind=8),dimension(3,3,3,3),intent(out)::output
            integer::i,j,alpha,beta,k,a
            real(kind=8)::dBdF
            output = 0.0
            do i=1,3
                  do j=1,3
                        do k=1,3
                              do a=1,3
                              output(i,j,k,a)=0.0      
                                    do alpha=1,3
                                          do beta=1,3
                                                output(i,j,k,a)=
     *                                        output(i,j,k,a)+
     *                                        dsigmadB(i,j,alpha,beta)
     *                                        *dBdF(F,alpha,beta,k,a)
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do
   
         end subroutine stressProdTerm1
         
         
         subroutine ten2fourthOrderArray(C4,output)
               real(kind=8),dimension(6,6),intent(in)::C4
               real(kind=8),dimension(3,3,3,3),intent(out)::output
               output=0.0
               output(1,1,1,1)=C4(1,1)
               output(1,1,2,2)=C4(1,2)
               output(1,1,3,3)=C4(1,3)
               output(1,1,1,2)=C4(1,4)
               output(1,1,1,3)=C4(1,5)
               output(1,1,2,3)=C4(1,6)
               output(2,2,1,1)=C4(2,1)
               output(2,2,2,2)=C4(2,2)
               output(2,2,3,3)=C4(2,3)
               output(2,2,1,2)=C4(2,4)
               output(2,2,1,3)=C4(2,5)
               output(2,2,2,3)=C4(2,6)
               
               output(3,3,1,1)=C4(3,1)
               output(3,3,2,2)=C4(3,2)
               output(3,3,3,3)=C4(3,3)
               output(3,3,1,2)=C4(3,4)
               output(3,3,1,3)=C4(3,5)
               output(3,3,2,3)=C4(3,6)
               
               output(1,2,1,1)=C4(4,1)
               output(1,2,2,2)=C4(4,2)
               output(1,2,3,3)=C4(4,3)
               output(1,2,1,2)=C4(4,4)
               output(1,2,1,3)=C4(4,5)
               output(1,2,2,3)=C4(4,6)
               
               output(1,3,1,1)=C4(5,1)
               output(1,3,2,2)=C4(5,2)
               output(1,3,3,3)=C4(5,3)
               output(1,3,1,2)=C4(5,4)
               output(1,3,1,3)=C4(5,5)
               output(1,3,2,3)=C4(5,6)
               
               output(2,3,1,1)=C4(6,1)
               output(2,3,2,2)=C4(6,2)
               output(2,3,3,3)=C4(6,3)
               output(2,3,1,2)=C4(6,4)
               output(2,3,1,3)=C4(6,5)
               output(2,3,2,3)=C4(6,6)
               
       
   ! Rest of the 45 components
   
               output(3,2,1,3) = output(2,3,1,3)
               output(3,1,3,1) = output(1,3,1,3)
               output(2,2,2,1) = output(2,2,1,2)
               output(2,2,3,2) = output(2,2,2,3)
               output(3,2,2,2) = output(2,3,2,2)
               output(2,3,3,2) = output(2,3,2,3)
               output(3,1,1,2) = output(1,3,1,2)
               output(2,1,2,1) = output(1,2,1,2)
               output(2,1,3,3) = output(1,2,3,3)
               output(3,2,1,2) = output(2,3,1,2)
               output(3,2,3,2) = output(2,3,2,3)
               output(3,3,2,1) = output(3,3,1,2)
               output(2,1,3,2) = output(1,2,2,3)
               output(1,2,2,1) = output(1,2,1,2)
               output(3,1,1,1) = output(1,3,1,1)
               output(3,2,3,3) = output(2,3,3,3)
               output(3,1,1,3) = output(1,3,1,3)
               output(2,1,1,2) = output(1,2,1,2)
               output(1,3,3,2) = output(1,3,2,3)
               output(1,2,3,2) = output(1,2,2,3)
               output(3,2,2,3) = output(2,3,2,3)
               output(2,3,2,1) = output(2,3,1,2)
               output(3,1,2,1) = output(1,3,1,2)
               output(2,3,3,1) = output(2,3,1,3)
               output(2,1,1,3) = output(1,2,1,3)
               output(1,1,2,1) = output(1,1,1,2)
               output(1,2,3,1) = output(1,2,1,3)
               output(3,3,3,2) = output(3,3,2,3)
               output(1,3,2,1) = output(1,3,1,2)
               output(2,2,3,1) = output(2,2,1,3)
               output(2,1,1,1) = output(1,2,1,1)
               output(3,2,1,1) = output(2,3,1,1)
               output(1,1,3,1) = output(1,1,1,3)
               output(3,1,2,3) = output(1,3,2,3)
               output(2,1,2,3) = output(1,2,2,3)
               output(2,1,3,1) = output(1,2,1,3)
               output(3,3,3,1) = output(3,3,1,3)
               output(3,1,3,2) = output(1,3,2,3)
               output(3,1,3,3) = output(1,3,3,3)
               output(3,1,2,2) = output(1,3,2,2)
               output(1,1,3,2) = output(1,1,2,3)
               output(2,1,2,2) = output(1,2,2,2)
               output(1,3,3,1) = output(1,3,1,3)
               output(3,2,2,1) = output(2,3,1,2)
               output(3,2,3,1) = output(2,3,1,3)
               
         end subroutine ten2fourthOrderArray