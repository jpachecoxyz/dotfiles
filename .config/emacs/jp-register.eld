;; Auto-generated file: DO NOT EDIT -*- mode: emacs-lisp -*-
((48
  . #("\n* Definicion y conceptos:\nLa comprension de listas, del ingles list comprehensions, es una funcionalidad que nos permite crear listas avanzadas en una misma linea de codigo. Esto se ve mucho mejor en la practica, asi que a lo largo de esta leccion vamos a trabajar distintos ejemplos.\n\n* Uso general de listas:\nEn el siguiente ejemplo, elevaremos al cuadrado una lista de numeros como lo hariamos de forma convencional:\n\n#+begin_src python :results output :exports both\nnums_squared = []\nnums = [1, 2, 3, 4, 5]\nfor n in nums:\n    square = n**2\n    nums_squared.append(square)\n\nprint(nums_squared)\n#+end_src\nEn el ejemplo vemos que funciona pero, son 5 lineas de codigo que por su puesto se puede mejorar.\n\n* List comprehensions:\n** Estructura:\nLa lista la podemos visualizar de la siguiente manera:\n\n- [  {parte 1} {parte 2} {parte 3}  ]\n\n- Parte 1 :: La operacion final, en nuestro ejemplo sera la =elevacion al cuadrado=.\n- Parte 2 :: La operacion que usaremos, en nuestro ejemplo es el =for=.\n- Parte 3 :: Lista original\n\nPara entender mejor la estructura como =tip=, es bueno empezar a escribir la =lista comprensiva= al reves, osea: =parte 3 -> parte 2 -> parte 1=\n\n#+BEGIN_SRC python :results output :exports both\nnums = [1, 2, 3, 4, 5]\nnum_squared = [n**2 for n in nums]\n\nprint(num_squared)\n#+END_SRC\n\n** Ejemplo 2\nEn el siguiente ejemplo =Capitalizaremos= la primer letra de cada palabra en la lista de =tv_shows=.\n\n#+BEGIN_SRC python :results output :exports both\ntv_shows = [ \"friends\", \"PARKS AND RECREATION\", \"the Office\", \"30 rock\", \"modern FAMILY\"]\ntv_shows_cap = []\nfor show in tv_shows:\n    show_cap = show.title()\n    tv_shows_cap.append(show_cap)\nprint (tv_shows_cap)\n#+END_SRC\n\nAhora lo haremos con la lista comprensiva:\n\n#+BEGIN_SRC python :results output :exports both\ntv_shows = [ \"friends\", \"PARKS AND RECREATION\", \"the Office\", \"30 rock\", \"modern FAMILY\"]\ntv_shows_cap = [show.title() for show in tv_shows] \nprint(tv_shows_cap)\n#+END_SRC\n"
      0 1 (yank-handler (evil-yank-line-handler nil t) fontified t) 1
      3
      (yank-handler (evil-yank-line-handler nil t) face org-level-1
                    fontified t)
      3 26
      (yank-handler (evil-yank-line-handler nil t) face org-level-1
                    fontified t)
      26 27 (yank-handler (evil-yank-line-handler nil t) fontified t)
      27 286 (yank-handler (evil-yank-line-handler nil t) fontified t)
      286 287
      (yank-handler (evil-yank-line-handler nil t) fontified t) 287
      289
      (yank-handler (evil-yank-line-handler nil t) face org-level-1
                    fontified t)
      289 311
      (yank-handler (evil-yank-line-handler nil t) face org-level-1
                    fontified t)
      311 312
      (yank-handler (evil-yank-line-handler nil t) fontified t) 312
      421 (yank-handler (evil-yank-line-handler nil t) fontified t)
      421 422
      (yank-handler (evil-yank-line-handler nil t) fontified t) 422
      471
      (yank-handler (evil-yank-line-handler nil t) font-lock-multiline
                    t font-lock-fontified t fontified t)
      471 489
      (yank-handler (evil-yank-line-handler nil t) font-lock-multiline
                    t font-lock-fontified t fontified t)
      489 512
      (yank-handler (evil-yank-line-handler nil t) font-lock-multiline
                    t font-lock-fontified t fontified t)
      512 527
      (yank-handler (evil-yank-line-handler nil t) font-lock-multiline
                    t font-lock-fontified t fontified t)
      527 545
      (yank-handler (evil-yank-line-handler nil t) font-lock-multiline
                    t font-lock-fontified t fontified t)
      545 577
      (yank-handler (evil-yank-line-handler nil t) font-lock-multiline
                    t font-lock-fontified t fontified t)
      577 578
      (yank-handler (evil-yank-line-handler nil t) font-lock-multiline
                    t font-lock-fontified t fontified t)
      578 598
      (yank-handler (evil-yank-line-handler nil t) font-lock-multiline
                    t font-lock-fontified t fontified t)
      598 607
      (yank-handler (evil-yank-line-handler nil t) font-lock-multiline
                    t font-lock-fontified t fontified t)
      607 608
      (yank-handler (evil-yank-line-handler nil t) fontified t) 608
      706 (yank-handler (evil-yank-line-handler nil t) fontified t)
      706 707
      (yank-handler (evil-yank-line-handler nil t) fontified t) 707
      709
      (yank-handler (evil-yank-line-handler nil t) face org-level-1
                    fontified t)
      709 729
      (yank-handler (evil-yank-line-handler nil t) face org-level-1
                    fontified t)
      729 730
      (yank-handler (evil-yank-line-handler nil t) fontified t) 730
      731
      (yank-handler (evil-yank-line-handler nil t) face org-level-2
                    fontified t)
      731 733
      (yank-handler (evil-yank-line-handler nil t) face org-level-2
                    fontified t)
      733 744
      (yank-handler (evil-yank-line-handler nil t) face org-level-2
                    fontified t)
      744 745
      (yank-handler (evil-yank-line-handler nil t) fontified t) 745
      800 (yank-handler (evil-yank-line-handler nil t) fontified t)
      800 801
      (yank-handler (evil-yank-line-handler nil t) fontified t) 801
      839 (yank-handler (evil-yank-line-handler nil t) fontified t)
      839 840
      (yank-handler (evil-yank-line-handler nil t) fontified t) 840
      842 (yank-handler (evil-yank-line-handler nil t) fontified t)
      842 852
      (yank-handler (evil-yank-line-handler nil t) face (org-list-dt)
                    fontified t)
      852 900
      (yank-handler (evil-yank-line-handler nil t) fontified t) 900
      901
      (yank-handler (evil-yank-line-handler nil t) invisible t
                    org-emphasis t font-lock-multiline t fontified t)
      901 910
      (yank-handler (evil-yank-line-handler nil t) org-emphasis t
                    font-lock-multiline t face (org-verbatim)
                    fontified t)
      910 922
      (yank-handler (evil-yank-line-handler nil t) org-emphasis t
                    font-lock-multiline t face (org-verbatim)
                    fontified t)
      922 923
      (yank-handler (evil-yank-line-handler nil t) invisible t
                    org-emphasis t font-lock-multiline t
                    rear-nonsticky
                    (mouse-face highlight keymap invisible intangible
                                help-echo org-linked-text htmlize-link)
                    fontified t)
      923 925
      (yank-handler (evil-yank-line-handler nil t) fontified t) 925
      927 (yank-handler (evil-yank-line-handler nil t) fontified t)
      927 937
      (yank-handler (evil-yank-line-handler nil t) face (org-list-dt)
                    fontified t)
      937 990
      (yank-handler (evil-yank-line-handler nil t) fontified t) 990
      991
      (yank-handler (evil-yank-line-handler nil t) invisible t
                    org-emphasis t font-lock-multiline t fontified t)
      991 994
      (yank-handler (evil-yank-line-handler nil t) org-emphasis t
                    font-lock-multiline t face (org-verbatim)
                    fontified t)
      994 995
      (yank-handler (evil-yank-line-handler nil t) rear-nonsticky
                    (mouse-face highlight keymap invisible intangible
                                help-echo org-linked-text htmlize-link)
                    invisible t org-emphasis t font-lock-multiline t
                    fontified t)
      995 997
      (yank-handler (evil-yank-line-handler nil t) fontified t) 997
      999 (yank-handler (evil-yank-line-handler nil t) fontified t)
      999 1004
      (yank-handler (evil-yank-line-handler nil t) face (org-list-dt)
                    fontified t)
      1004 1009
      (yank-handler (evil-yank-line-handler nil t) face (org-list-dt)
                    fontified t)
      1009 1025
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1025
      1026 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1026 1065
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1065
      1066
      (yank-handler (evil-yank-line-handler nil t) invisible t
                    org-emphasis t font-lock-multiline t fontified t)
      1066 1069
      (yank-handler (evil-yank-line-handler nil t) org-emphasis t
                    font-lock-multiline t face (org-verbatim)
                    fontified t)
      1069 1070
      (yank-handler (evil-yank-line-handler nil t) rear-nonsticky
                    (mouse-face highlight keymap invisible intangible
                                help-echo org-linked-text htmlize-link)
                    invisible t org-emphasis t font-lock-multiline t
                    fontified t)
      1070 1103
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1103
      1104
      (yank-handler (evil-yank-line-handler nil t) invisible t
                    org-emphasis t font-lock-multiline t fontified t)
      1104 1109
      (yank-handler (evil-yank-line-handler nil t) org-emphasis t
                    font-lock-multiline t face (org-verbatim)
                    fontified t)
      1109 1121
      (yank-handler (evil-yank-line-handler nil t) org-emphasis t
                    font-lock-multiline t face (org-verbatim)
                    fontified t)
      1121 1122
      (yank-handler (evil-yank-line-handler nil t) rear-nonsticky
                    (mouse-face highlight keymap invisible intangible
                                help-echo org-linked-text htmlize-link)
                    invisible t org-emphasis t font-lock-multiline t
                    fontified t)
      1122 1139
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1139
      1140
      (yank-handler (evil-yank-line-handler nil t) invisible t
                    org-emphasis t font-lock-multiline t fontified t)
      1140 1145
      (yank-handler (evil-yank-line-handler nil t) org-emphasis t
                    font-lock-multiline t face (org-verbatim)
                    fontified t)
      1145 1169
      (yank-handler (evil-yank-line-handler nil t) org-emphasis t
                    font-lock-multiline t face (org-verbatim)
                    fontified t)
      1169 1170
      (yank-handler (evil-yank-line-handler nil t) rear-nonsticky
                    (mouse-face highlight keymap invisible intangible
                                help-echo org-linked-text htmlize-link)
                    invisible t org-emphasis t font-lock-multiline t
                    fontified t)
      1170 1171
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1171
      1172 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1172 1220
      (yank-handler (evil-yank-line-handler nil t) face
                    org-block-begin-line font-lock-multiline t
                    font-lock-fontified t fontified t)
      1220 1221
      (yank-handler (evil-yank-line-handler nil t) face
                    org-block-begin-line font-lock-multiline t
                    font-lock-fontified t fontified t)
      1221 1225
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-variable-name-face org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1225 1226
      (yank-handler (evil-yank-line-handler nil t) face (org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1226 1227
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-operator-face org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1227 1228
      (yank-handler (evil-yank-line-handler nil t) face (org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1228 1229
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-type-face org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1229 1242
      (yank-handler (evil-yank-line-handler nil t) face (org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1242 1243
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-type-face org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1243 1244
      (yank-handler (evil-yank-line-handler nil t) face (org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1244 1247
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-variable-name-face org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1247 1255
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-variable-name-face org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1255 1256
      (yank-handler (evil-yank-line-handler nil t) face (org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1256 1257
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-operator-face org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1257 1258
      (yank-handler (evil-yank-line-handler nil t) face (org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1258 1259
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-type-face org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1259 1260
      (yank-handler (evil-yank-line-handler nil t) face (org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1260 1262
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-operator-face org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1262 1264
      (yank-handler (evil-yank-line-handler nil t) face (org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1264 1267
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-keyword-face org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1267 1270
      (yank-handler (evil-yank-line-handler nil t) face (org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1270 1272
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-keyword-face org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1272 1277
      (yank-handler (evil-yank-line-handler nil t) face (org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1277 1278
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-type-face org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1278 1279
      (yank-handler (evil-yank-line-handler nil t) face (org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1279 1280
      (yank-handler (evil-yank-line-handler nil t) face (org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1280 1285
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-builtin-face org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1285 1286
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-type-face org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1286 1294
      (yank-handler (evil-yank-line-handler nil t) face (org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1294 1297
      (yank-handler (evil-yank-line-handler nil t) face (org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1297 1298
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-type-face org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1298 1299
      (yank-handler (evil-yank-line-handler nil t) face (org-block)
                    font-lock-multiline t src-block t
                    font-lock-fontified t fontified t)
      1299 1308
      (yank-handler (evil-yank-line-handler nil t) face
                    org-block-end-line font-lock-multiline t
                    font-lock-fontified t fontified t)
      1308 1309
      (yank-handler (evil-yank-line-handler nil t) face
                    org-block-end-line fontified t)
      1309 1310
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1310
      1311
      (yank-handler (evil-yank-line-handler nil t) face org-level-2
                    fontified t)
      1311 1313
      (yank-handler (evil-yank-line-handler nil t) face org-level-2
                    fontified t)
      1313 1322
      (yank-handler (evil-yank-line-handler nil t) face org-level-2
                    fontified t)
      1322 1323
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1323
      1347 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1347 1348
      (yank-handler (evil-yank-line-handler nil t) invisible t
                    org-emphasis t font-lock-multiline t fontified t)
      1348 1363
      (yank-handler (evil-yank-line-handler nil t) org-emphasis t
                    font-lock-multiline t face (org-verbatim)
                    fontified t)
      1363 1364
      (yank-handler (evil-yank-line-handler nil t) rear-nonsticky
                    (mouse-face highlight keymap invisible intangible
                                help-echo org-linked-text htmlize-link)
                    invisible t org-emphasis t font-lock-multiline t
                    fontified t)
      1364 1412
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1412
      1413
      (yank-handler (evil-yank-line-handler nil t) invisible t
                    org-emphasis t font-lock-multiline t fontified t)
      1413 1415
      (yank-handler (evil-yank-line-handler nil t) org-emphasis t
                    font-lock-multiline t face (org-verbatim)
                    fontified t)
      1415 1421
      (yank-handler (evil-yank-line-handler nil t) org-emphasis t
                    font-lock-multiline t face (org-verbatim)
                    fontified t)
      1421 1422
      (yank-handler (evil-yank-line-handler nil t) rear-nonsticky
                    (mouse-face highlight keymap invisible intangible
                                help-echo org-linked-text htmlize-link)
                    invisible t org-emphasis t font-lock-multiline t
                    fontified t)
      1422 1424
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1424
      1425 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1425 1473
      (yank-handler (evil-yank-line-handler nil t) face
                    org-block-begin-line font-lock-multiline t
                    font-lock-fontified t fontified t)
      1473 1474
      (yank-handler (evil-yank-line-handler nil t) face
                    org-block-begin-line font-lock-multiline t
                    font-lock-fontified t fontified t)
      1474 1482
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-variable-name-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1482 1483
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1483 1484
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-operator-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1484 1485
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1485 1486
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-type-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1486 1487
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1487 1496
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-string-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1496 1498
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1498 1520
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-string-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1520 1522
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1522 1534
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-string-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1534 1536
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1536 1545
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-string-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1545 1547
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1547 1562
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-string-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1562 1563
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-type-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1563 1564
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1564 1576
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-variable-name-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1576 1577
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1577 1578
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-operator-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1578 1579
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1579 1581
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-type-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1581 1582
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1582 1585
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-keyword-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1585 1591
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1591 1593
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-keyword-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1593 1604
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1604 1608
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1608 1616
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-variable-name-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1616 1617
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1617 1618
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-operator-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1618 1629
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1629 1631
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-type-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1631 1632
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1632 1638
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1638 1655
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1655 1656
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-type-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1656 1664
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1664 1665
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-type-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1665 1666
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1666 1671
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-builtin-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1671 1672
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1672 1673
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-type-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1673 1685
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1685 1686
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-type-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1686 1687
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1687 1696
      (yank-handler (evil-yank-line-handler nil t) face
                    org-block-end-line font-lock-multiline t
                    font-lock-fontified t fontified t)
      1696 1697
      (yank-handler (evil-yank-line-handler nil t) face
                    org-block-end-line fontified t)
      1697 1698
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1698
      1741 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1741 1742
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1742
      1790
      (yank-handler (evil-yank-line-handler nil t) face
                    org-block-begin-line font-lock-multiline t
                    font-lock-fontified t fontified t)
      1790 1791
      (yank-handler (evil-yank-line-handler nil t) face
                    org-block-begin-line font-lock-multiline t
                    font-lock-fontified t fontified t)
      1791 1799
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-variable-name-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1799 1800
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1800 1801
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-operator-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1801 1802
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1802 1803
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-type-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1803 1804
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1804 1813
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-string-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1813 1815
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1815 1837
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-string-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1837 1839
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1839 1851
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-string-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1851 1853
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1853 1862
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-string-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1862 1864
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1864 1879
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-string-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1879 1880
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-type-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1880 1881
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1881 1893
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-variable-name-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1893 1894
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1894 1895
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-operator-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1895 1896
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1896 1897
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-type-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1897 1907
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1907 1909
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-function-name-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1909 1910
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1910 1913
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-keyword-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1913 1919
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1919 1921
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-keyword-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1921 1930
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1930 1931
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-type-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1931 1933
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1933 1938
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-builtin-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1938 1939
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-type-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1939 1951
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1951 1952
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (font-lock-type-face org-block)
                    font-lock-multiline t font-lock-fontified t
                    fontified t)
      1952 1953
      (yank-handler (evil-yank-line-handler nil t) src-block t face
                    (org-block) font-lock-multiline t
                    font-lock-fontified t fontified t)
      1953 1962
      (yank-handler (evil-yank-line-handler nil t) face
                    org-block-end-line font-lock-multiline t
                    font-lock-fontified t fontified t)
      1962 1963
      (yank-handler (evil-yank-line-handler nil t) face
                    org-block-end-line fontified t))))
