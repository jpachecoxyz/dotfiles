;; Auto-generated file: DO NOT EDIT -*- mode: emacs-lisp -*-
((48
  . #("#include <Wire.h>\n//#include <TinyWireM.h>\n//#include <TinyRTClib.h>\n#include <RTClib.h>\n\n// Instanciar el objeto del reloj (TinyRTCLib usa RTC_DS1307 por defecto)\nRTC_DS1307 rtc;\n\n// Variables para el c�lculo del caudal y consumo\nconst int pinPot = A0;\nfloat totalLitros = 0.0;\nunsigned long previoMillis = 0;\nconst unsigned long intervaloMuestreo = 1000; // 1 segundo en milisegundos\n\n// Variable para controlar el reporte por minuto\nint ultimoMinuto = -1;\n\nvoid setup() {\n  Serial.begin(9600);\n  Wire.begin();\n  \n  // Inicializar el RTC\n  if (!rtc.begin()) {\n    Serial.println(\"�No se pudo encontrar el m�dulo RTC!\");\n    while (1);\n  }\n\n  // Si el RTC perdi� la energ�a (por ejemplo, cambio de bater�a), se pone en hora\n  if (!rtc.isrunning()) {\n    Serial.println(\"El RTC NO est� corriendo. Configurando hora de compilaci�n...\");\n    // Sincroniza el RTC con la fecha y hora en la que se compilo el sketch\n    rtc.adjust(DateTime(__DATE__, __TIME__));\n  }\n  \n  Serial.println(\"Sistema inicializado. Esperando al siguiente minuto para reportar...\");\n}\n\nvoid loop() {\n  unsigned long actualMillis = millis();\n\n  // 1. C�LCULO DE CONSUMO SEGUNDO A SEGUNDO (No bloqueante)\n  if (actualMillis - previoMillis >= intervaloMuestreo) {\n    previoMillis = actualMillis;\n\n    int lecturaADC = analogRead(pinPot);\n    \n    // Simulaci�n de caudal (Ejemplo: de 0.0 a 50.0 Litros por Minuto basado en el potenci�metro)\n    // Ajusta los l�mites (0.0 y 50.0) seg�n las especificaciones de tu sensor real\n    float caudalLPM = map(lecturaADC, 0, 1023, 0, 500) / 10.0; \n\n    // Si el caudal es muy bajo, lo consideramos cero (ruido)\n    if (caudalLPM < 0.5) caudalLPM = 0.0;\n\n    // Como medimos cada 1 segundo, sumamos la fracci�n de litros consumidos en este segundo:\n    // Litros en 1 segundo = Caudal (L/min) / 60 segundos\n    totalLitros += (caudalLPM / 60.0);\n  }\n\n  // 2. CONTROL DEL TIEMPO Y REPORTE CADA MINUTO\n  DateTime ahora = rtc.now();\n\n  // Verificamos si cambi� el minuto actual respecto al �ltimo reporte\n  if (ahora.minute() != ultimoMinuto) {\n    ultimoMinuto = ahora.minute(); // Actualizamos el registro del �ltimo minuto\n\n    // Formatear la salida en consola tal como lo solicitaste\n    Serial.print(\"A las \");\n    \n    // A�adir un cero a la izquierda si la hora es menor a 10 (ej: 09:05)\n    if (ahora.hour() < 10) Serial.print(\"0\");\n    Serial.print(ahora.hour(), DEC);\n    Serial.print(\":\");\n    \n    if (ahora.minute() < 10) Serial.print(\"0\");\n    Serial.print(ahora.minute(), DEC);\n    \n    Serial.print(\" has consumido \");\n    Serial.print(totalLitros, 2); // Muestra el consumo con 2 decimales\n    Serial.println(\" Litros\");\n  }\n}\n"
      0 1
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-preprocessor-face fontified t c-in-sws t
                    c-is-sws t)
      1 8
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-preprocessor-face fontified t c-in-sws t)
      8 9
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    c-in-sws t)
      9 10
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t c-in-sws t
                    category c-<-as-paren-syntax)
      10 16
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t c-in-sws t)
      16 17
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t c-in-sws t
                    category c->-as-paren-syntax)
      17 18
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    c-in-sws t)
      18 19
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    c-in-sws t c-is-sws t)
      19 20
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    c-in-sws t)
      20 43
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t c-in-sws t)
      43 45
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    c-in-sws t)
      45 69
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t c-in-sws t)
      69 70
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-preprocessor-face fontified t c-in-sws t
                    c-is-sws t)
      70 77
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-preprocessor-face fontified t c-in-sws t)
      77 78
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    c-in-sws t)
      78 79
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t c-in-sws t
                    category c-<-as-paren-syntax)
      79 87
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t c-in-sws t)
      87 88
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t c-in-sws t
                    category c->-as-paren-syntax)
      88 89
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    c-in-sws t)
      89 90
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    c-is-sws t c-in-sws t)
      90 91
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    c-in-sws t c-is-sws t)
      91 93
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    c-in-sws t)
      93 164
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t c-in-sws t)
      164 165
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t c-is-sws t)
      165 173
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      173 174
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face c-type c-decl-id-start
                    fontified t)
      174 175
      (yank-handler (evil-yank-line-handler nil t) fontified t) 175
      178
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      178 181
      (yank-handler (evil-yank-line-handler nil t) fontified t) 181
      182
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    c-is-sws t c-in-sws t)
      182 184
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    c-in-sws t)
      184 231
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t c-in-sws t)
      231 232
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t c-is-sws t)
      232 236
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      236 237
      (yank-handler (evil-yank-line-handler nil t) fontified t) 237
      239
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      239 240
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face c-type c-decl-id-start
                    fontified t)
      240 241
      (yank-handler (evil-yank-line-handler nil t) fontified t) 241
      247
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      247 254
      (yank-handler (evil-yank-line-handler nil t) fontified t) 254
      258
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      258 259
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face c-type c-decl-id-start
                    fontified t)
      259 260
      (yank-handler (evil-yank-line-handler nil t) fontified t) 260
      271
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      271 279
      (yank-handler (evil-yank-line-handler nil t) fontified t) 279
      291
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      291 292
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face c-type c-decl-id-start
                    fontified t)
      292 293
      (yank-handler (evil-yank-line-handler nil t) fontified t) 293
      305
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      305 311
      (yank-handler (evil-yank-line-handler nil t) fontified t) 311
      316
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      316 317
      (yank-handler (evil-yank-line-handler nil t) fontified t) 317
      329
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      329 330
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face c-type c-decl-id-start
                    fontified t)
      330 331
      (yank-handler (evil-yank-line-handler nil t) fontified t) 331
      348
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      348 356
      (yank-handler (evil-yank-line-handler nil t) fontified t) 356
      357
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    c-is-sws t c-in-sws t)
      357 358
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    c-is-sws t c-in-sws t)
      358 360
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    c-in-sws t)
      360 386
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t c-in-sws t)
      386 387
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    c-in-sws t)
      387 390
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    c-in-sws t)
      390 436
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t c-in-sws t)
      436 437
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t c-is-sws t)
      437 438
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      438 439
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face c-type c-decl-id-start
                    fontified t)
      439 440
      (yank-handler (evil-yank-line-handler nil t) fontified t) 440
      452
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      452 460
      (yank-handler (evil-yank-line-handler nil t) fontified t) 460
      463
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      463 464
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face c-type c-decl-id-start
                    fontified t)
      464 465
      (yank-handler (evil-yank-line-handler nil t) fontified t) 465
      470
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      470 471
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      471 472
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      472 473
      (yank-handler (evil-yank-line-handler nil t) fontified t) 473
      474
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      474 477
      (yank-handler (evil-yank-line-handler nil t) fontified t) 477
      483
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      483 484
      (yank-handler (evil-yank-line-handler nil t) fontified t) 484
      489
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      489 490
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      490 494
      (yank-handler (evil-yank-line-handler nil t) fontified t) 494
      495
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      495 504
      (yank-handler (evil-yank-line-handler nil t) fontified t) 504
      509
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      509 510
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      510 511
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      511 516
      (yank-handler (evil-yank-line-handler nil t) fontified t) 516
      518
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    c-is-sws t c-in-sws t)
      518 519
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    c-is-sws t c-in-sws t)
      519 521
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    c-in-sws t)
      521 540
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t c-in-sws t)
      540 542
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    c-in-sws t c-is-sws t)
      542 543
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t c-is-sws t)
      543 544
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      544 545
      (yank-handler (evil-yank-line-handler nil t) fontified t) 545
      546
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      546 547
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-negation-char-face fontified t)
      547 551
      (yank-handler (evil-yank-line-handler nil t) fontified t) 551
      556
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      556 557
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      557 558
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      558 559
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      559 560
      (yank-handler (evil-yank-line-handler nil t) fontified t) 560
      561
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      561 566
      (yank-handler (evil-yank-line-handler nil t) fontified t) 566
      572
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      572 573
      (yank-handler (evil-yank-line-handler nil t) fontified t) 573
      580
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      580 581
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      581 619
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      619 620
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      620 626
      (yank-handler (evil-yank-line-handler nil t) fontified t) 626
      631
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      631 632
      (yank-handler (evil-yank-line-handler nil t) fontified t) 632
      633
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      633 634
      (yank-handler (evil-yank-line-handler nil t) fontified t) 634
      635
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      635 639
      (yank-handler (evil-yank-line-handler nil t) fontified t) 639
      640
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      640 642
      (yank-handler (evil-yank-line-handler nil t) fontified t) 642
      644
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    c-is-sws t c-in-sws t)
      644 645
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    c-is-sws t c-in-sws t)
      645 647
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    c-in-sws t)
      647 725
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t c-in-sws t)
      725 727
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    c-in-sws t c-is-sws t)
      727 728
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t c-is-sws t)
      728 729
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      729 730
      (yank-handler (evil-yank-line-handler nil t) fontified t) 730
      731
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      731 732
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-negation-char-face fontified t)
      732 745
      (yank-handler (evil-yank-line-handler nil t) fontified t) 745
      746
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      746 747
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      747 748
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      748 749
      (yank-handler (evil-yank-line-handler nil t) fontified t) 749
      750
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      750 755
      (yank-handler (evil-yank-line-handler nil t) fontified t) 755
      761
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      761 762
      (yank-handler (evil-yank-line-handler nil t) fontified t) 762
      769
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      769 770
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      770 833
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      833 834
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      834 836
      (yank-handler (evil-yank-line-handler nil t) fontified t) 836
      840
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    c-is-sws t c-in-sws t)
      840 841
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    c-is-sws t c-in-sws t)
      841 843
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    c-in-sws t)
      843 912
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t c-in-sws t)
      912 916
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    c-in-sws t c-is-sws t)
      916 917
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    c-is-sws t)
      917 926
      (yank-handler (evil-yank-line-handler nil t) fontified t) 926
      927
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      927 935
      (yank-handler (evil-yank-line-handler nil t) fontified t) 935
      936
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      936 954
      (yank-handler (evil-yank-line-handler nil t) fontified t) 954
      955
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      955 956
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      956 960
      (yank-handler (evil-yank-line-handler nil t) fontified t) 960
      961
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      961 967
      (yank-handler (evil-yank-line-handler nil t) fontified t) 967
      973
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      973 974
      (yank-handler (evil-yank-line-handler nil t) fontified t) 974
      981
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      981 982
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      982 1052
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1052 1053
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1053 1055
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1055
      1056
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1056 1058
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1058
      1061
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1061 1062
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face c-type c-decl-id-start
                    fontified t)
      1062 1063
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1063
      1067
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1067 1068
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1068 1069
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1069 1070
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1070
      1071
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1071 1074
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1074
      1086
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1086 1087
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face c-type c-decl-id-start
                    fontified t)
      1087 1088
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1088
      1100
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1100 1103
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1103
      1109
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1109 1110
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1110 1111
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1111 1114
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1114
      1116
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    c-is-sws t c-in-sws t)
      1116 1117
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    c-is-sws t c-in-sws t)
      1117 1119
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    c-in-sws t)
      1119 1175
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t c-in-sws t)
      1175 1177
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    c-in-sws t c-is-sws t)
      1177 1178
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t c-is-sws t)
      1178 1179
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1179 1180
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1180
      1181
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1181 1229
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1229
      1230
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1230 1231
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1231
      1232
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1232 1271
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1271
      1273
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1273 1274
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face c-type c-decl-id-start
                    fontified t)
      1274 1275
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1275
      1285
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1285 1288
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1288
      1298
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1298 1299
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1299 1305
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1305
      1306
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1306 1313
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1313
      1317
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    c-is-sws t c-in-sws t)
      1317 1318
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    c-is-sws t c-in-sws t)
      1318 1320
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    c-in-sws t)
      1320 1411
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t c-in-sws t)
      1411 1415
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    c-in-sws t)
      1415 1418
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    c-in-sws t)
      1418 1495
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t c-in-sws t)
      1495 1499
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    c-in-sws t c-is-sws t)
      1499 1500
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t c-is-sws t)
      1500 1503
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified nil)
      1503 1504
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face c-type c-decl-id-start
                    fontified nil)
      1504 1505
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1505
      1514
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified nil)
      1514 1517
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1517
      1520
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified nil)
      1520 1521
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified nil)
      1521 1548
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1548
      1549
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified nil)
      1549 1560
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1560
      1564
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-is-sws t c-in-sws t)
      1564 1565
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-is-sws t c-in-sws t face
                    font-lock-comment-delimiter-face)
      1565 1567
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-in-sws t face font-lock-comment-delimiter-face)
      1567 1622
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-in-sws t face font-lock-comment-face)
      1622 1626
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-in-sws t c-is-sws t)
      1626 1627
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-type-face c-is-sws t)
      1627 1628
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-type-face)
      1628 1629
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1629
      1630
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      1630 1645
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1645
      1646
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      1646 1665
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1665
      1669
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-is-sws t c-in-sws t)
      1669 1670
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-is-sws t c-in-sws t face
                    font-lock-comment-delimiter-face)
      1670 1672
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-in-sws t face font-lock-comment-delimiter-face)
      1672 1759
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-in-sws t face font-lock-comment-face)
      1759 1763
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-in-sws t)
      1763 1766
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-in-sws t face font-lock-comment-delimiter-face)
      1766 1817
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-in-sws t face font-lock-comment-face)
      1817 1821
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-in-sws t c-is-sws t)
      1821 1822
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-is-sws t)
      1822 1836
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1836
      1837
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      1837 1853
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1853
      1854
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      1854 1858
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1858
      1859
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-function-name-face)
      1859 1861
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1861
      1863
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-is-sws t c-in-sws t)
      1863 1864
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-is-sws t c-in-sws t face
                    font-lock-comment-delimiter-face)
      1864 1866
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-in-sws t face font-lock-comment-delimiter-face)
      1866 1910
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-in-sws t face font-lock-comment-face)
      1910 1912
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-in-sws t c-is-sws t)
      1912 1913
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-type-face c-is-sws t)
      1913 1919
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-type-face)
      1919 1920
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-type-face c-type c-decl-id-start)
      1920 1921
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1921
      1926
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      1926 1936
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1936
      1937
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-function-name-face)
      1937 1938
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-function-name-face)
      1938 1941
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1941
      1943
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-is-sws t c-in-sws t)
      1943 1944
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-is-sws t c-in-sws t face
                    font-lock-comment-delimiter-face)
      1944 1946
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-in-sws t face font-lock-comment-delimiter-face)
      1946 2012
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-in-sws t face font-lock-comment-face)
      2012 2014
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-in-sws t c-is-sws t)
      2014 2015
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-type-face c-is-sws t)
      2015 2016
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-type-face)
      2016 2017
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2017
      2018
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-function-name-face)
      2018 2030
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2030
      2031
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2031 2032
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2032 2048
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2048
      2049
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-function-name-face)
      2049 2050
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2050
      2051
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-function-name-face)
      2051 2083
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2083
      2084
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2084 2085
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2085 2087
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2087
      2090
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-comment-delimiter-face)
      2090 2133
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-comment-face)
      2133 2138
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2138
      2141
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-comment-delimiter-face)
      2141 2196
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-comment-face)
      2196 2200
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2200
      2206
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-keyword-face)
      2206 2207
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2207
      2212
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-keyword-face)
      2212 2213
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2213 2221
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-string-face)
      2221 2222
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2222 2229
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2229
      2233
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-is-sws t c-in-sws t)
      2233 2234
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-is-sws t c-in-sws t face
                    font-lock-comment-delimiter-face)
      2234 2236
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-in-sws t face font-lock-comment-delimiter-face)
      2236 2303
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-in-sws t face font-lock-comment-face)
      2303 2307
      (yank-handler (evil-yank-line-handler nil t) fontified nil
                    c-in-sws t c-is-sws t)
      2307 2308
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-type-face c-is-sws t)
      2308 2309
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-type-face)
      2309 2310
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2310
      2311
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2311 2321
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2321
      2322
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-constant-face)
      2322 2323
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-constant-face)
      2323 2328
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2328
      2329
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2329 2330
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2330
      2336
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-keyword-face)
      2336 2337
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2337
      2342
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-keyword-face)
      2342 2343
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2343 2346
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-string-face)
      2346 2347
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2347 2353
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2353
      2359
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-keyword-face)
      2359 2360
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2360
      2365
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-keyword-face)
      2365 2366
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2366 2376
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2376
      2377
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-constant-face)
      2377 2378
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-constant-face)
      2378 2383
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2383
      2384
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2384 2390
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2390
      2396
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-keyword-face)
      2396 2397
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2397
      2402
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-keyword-face)
      2402 2403
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2403 2406
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-string-face)
      2406 2407
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2407 2418
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2418
      2420
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-type-face)
      2420 2421
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2421
      2422
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2422 2434
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2434
      2435
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-constant-face)
      2435 2436
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-constant-face)
      2436 2441
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2441
      2442
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2442 2443
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2443
      2449
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-keyword-face)
      2449 2450
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2450
      2455
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-keyword-face)
      2455 2456
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2456 2459
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-string-face)
      2459 2460
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2460 2466
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2466
      2472
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-keyword-face)
      2472 2473
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2473
      2478
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-keyword-face)
      2478 2479
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2479 2491
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2491
      2492
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-constant-face)
      2492 2493
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-constant-face)
      2493 2498
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2498
      2499
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2499 2510
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2510
      2516
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-keyword-face)
      2516 2517
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2517
      2522
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-keyword-face)
      2522 2523
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2523 2540
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-string-face)
      2540 2541
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2541 2547
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2547
      2553
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-keyword-face)
      2553 2554
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2554
      2559
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-keyword-face)
      2559 2560
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2560 2574
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2574
      2575
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2575 2577
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2577
      2580
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-comment-delimiter-face)
      2580 2615
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-comment-face)
      2615 2619
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2619
      2625
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-keyword-face)
      2625 2626
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2626
      2633
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-keyword-face)
      2633 2634
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2634 2643
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-string-face)
      2643 2644
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      2644 2648
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2648
      2649
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-function-name-face)
      2649 2650
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 2650
      2651
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-type-face)
      2651 2652
      (yank-handler (evil-yank-line-handler nil t) fontified nil))))
