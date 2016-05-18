
(* This file was auto-generated based on "src/reason_parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
      "<SYNTAX ERROR>\n"
    | 2 ->
      "<SYNTAX ERROR>\n"
    | 2590 ->
      "<SYNTAX ERROR>\n"
    | 568 ->
      "Expecting one of the following:\n  - an identifier to access a member of an object\n  - \"[\" + expression + \"]\" to access an element of a list\n  - \"(\" + expression + \")\"\n  - \"{\" + expression + \"}\"\n"
    | 569 ->
      "Expecting an expression\n"
    | 2279 ->
      "Expecting one of the following:\n  - an infix operation to connect two expressions\n  - \")\" to close the block\n"
    | 2281 ->
      "Expecting an expression\n"
    | 2282 ->
      "Expecting one of the following:\n  - an infix operation to connect two expressions\n  - \"}\" to close the block\n"
    | 2284 ->
      "Expecting an expression\n"
    | 2285 ->
      "Expecting one of the following:\n  - an infix operation to connect two expressions\n  - \"}\" to close the block\n"
    | 1182 ->
      "Expecting an expression\n"
    | 566 ->
      "Expecting an identifier\n"
    | 1115 ->
      "Expecting a structure item\n"
    | 2595 ->
      "Invalid token\n"
    | 1236 ->
      "Expecting an expression\n"
    | 1237 ->
      "Expecting one of the following:\n  - The continuation of the previous expression\n  - \":\" to start the next expression\n"
    | 1238 ->
      "Expecting an expression\n"
    | 1197 ->
      "Expecting an expression\n"
    | 1203 ->
      "Expecting an expression\n"
    | 1205 ->
      "Expecting an expression\n"
    | 1199 ->
      "Expecting an expression\n"
    | 1207 ->
      "Expecting an expression\n"
    | 1209 ->
      "Expecting an expression\n"
    | 1211 ->
      "Expecting an expression\n"
    | 15 ->
      "Expecting one of the following:\n  - \")\" to form a unit value \"()\"\n  - \"module\" to start a module expression\n  - an expression\n  - an operator to denote the prefix form of an operator\n"
    | 1213 ->
      "Expecting an expression\n"
    | 1219 ->
      "Expecting an expression\n"
    | 1221 ->
      "Expecting an expression\n"
    | 2396 ->
      "Expecting \"]\"\n"
    | 400 ->
      "Expecting an attributed id\n"
    | 2503 ->
      "Expecting \"]\"\n"
    | 171 ->
      "Expecting an attribute id\n"
    | 1184 ->
      "Expecting an expression\n"
    | 1201 ->
      "Expecting an expression\n"
    | 1215 ->
      "Expecting an expression\n"
    | 1217 ->
      "Expecting an expression\n"
    | 1223 ->
      "Expecting an expression\n"
    | 1225 ->
      "Expecting an expression\n"
    | 848 ->
      "<SYNTAX ERROR>\n"
    | 849 ->
      "<SYNTAX ERROR>\n"
    | 2187 ->
      "<SYNTAX ERROR>\n"
    | 850 ->
      "<SYNTAX ERROR>\n"
    | 852 ->
      "<SYNTAX ERROR>\n"
    | 2183 ->
      "<SYNTAX ERROR>\n"
    | 2185 ->
      "<SYNTAX ERROR>\n"
    | 2190 ->
      "<SYNTAX ERROR>\n"
    | 2191 ->
      "<SYNTAX ERROR>\n"
    | 2195 ->
      "<SYNTAX ERROR>\n"
    | 2205 ->
      "<SYNTAX ERROR>\n"
    | 2210 ->
      "<SYNTAX ERROR>\n"
    | 1842 ->
      "<SYNTAX ERROR>\n"
    | 1233 ->
      "<SYNTAX ERROR>\n"
    | 1227 ->
      "<SYNTAX ERROR>\n"
    | 1229 ->
      "<SYNTAX ERROR>\n"
    | 1231 ->
      "<SYNTAX ERROR>\n"
    | 76 ->
      "<SYNTAX ERROR>\n"
    | 948 ->
      "<SYNTAX ERROR>\n"
    | 949 ->
      "<SYNTAX ERROR>\n"
    | 93 ->
      "Expecting one of the following:\n  - \"=\" to start the body of the type declaration\n  - \"constraint\" to add constraints to the type declaration\n  - \";\" to finish type declaratoin\n  - \"+=\" to form a string type extension\n  - \"and\" to declare another type\n"
    | 80 ->
      "<SYNTAX ERROR>\n"
    | 2575 ->
      "<SYNTAX ERROR>\n"
    | 2579 ->
      "<SYNTAX ERROR>\n"
    | 2576 ->
      "<SYNTAX ERROR>\n"
    | 2577 ->
      "<SYNTAX ERROR>\n"
    | 84 ->
      "<SYNTAX ERROR>\n"
    | 86 ->
      "<SYNTAX ERROR>\n"
    | 88 ->
      "<SYNTAX ERROR>\n"
    | 90 ->
      "<SYNTAX ERROR>\n"
    | 1125 ->
      "<SYNTAX ERROR>\n"
    | 94 ->
      "<SYNTAX ERROR>\n"
    | 2562 ->
      "<SYNTAX ERROR>\n"
    | 2563 ->
      "<SYNTAX ERROR>\n"
    | 2550 ->
      "<SYNTAX ERROR>\n"
    | 2564 ->
      "<SYNTAX ERROR>\n"
    | 2568 ->
      "<SYNTAX ERROR>\n"
    | 2569 ->
      "<SYNTAX ERROR>\n"
    | 2542 ->
      "<SYNTAX ERROR>\n"
    | 96 ->
      "<SYNTAX ERROR>\n"
    | 2553 ->
      "<SYNTAX ERROR>\n"
    | 2540 ->
      "<SYNTAX ERROR>\n"
    | 2541 ->
      "<SYNTAX ERROR>\n"
    | 2558 ->
      "Expecting at least one type field definition in the form of:\n  <field name> : <type>\n"
    | 483 ->
      "Expecting a type field definition in the form of:\n  <field name> : <type>\n"
    | 484 ->
      "Expecting \":\"\n"
    | 485 ->
      "Expecting a type name describing this field\n"
    | 2559 ->
      "Expecting one of the following:\n  - \",\" to finish current type field\n  - \"}\" to finish type definition\n"
    | 499 ->
      "Expecting one of the following:\n  - another type field definition\n  - \"}\" to finish entire type definition\n"
    | 1130 ->
      "<SYNTAX ERROR>\n"
    | 2547 ->
      "<SYNTAX ERROR>\n"
    | 970 ->
      "<SYNTAX ERROR>\n"
    | 971 ->
      "<SYNTAX ERROR>\n"
    | 972 ->
      "<SYNTAX ERROR>\n"
    | 1126 ->
      "<SYNTAX ERROR>\n"
    | 1128 ->
      "<SYNTAX ERROR>\n"
    | 173 ->
      "<SYNTAX ERROR>\n"
    | 2498 ->
      "<SYNTAX ERROR>\n"
    | 2497 ->
      "<SYNTAX ERROR>\n"
    | 2500 ->
      "<SYNTAX ERROR>\n"
    | 2439 ->
      "<SYNTAX ERROR>\n"
    | 2440 ->
      "<SYNTAX ERROR>\n"
    | 2441 ->
      "Expecting a sequence item\n"
    | 1872 ->
      "Expecting one of the following:\n  - \"|\" to open the next pattern\n  - \"=>\" to start the body of the matched pattern\n  - \"when\" to start a contitional guard for the previous pattern\n"
    | 2472 ->
      "Expecting the body of the matched pattern\n"
    | 2501 ->
      "Expecting one of the following:\n  - \"}\" to finish the block\n  - \"|\" to start another pattern matching case\n"
    | 2455 ->
      "<SYNTAX ERROR>\n"
    | 2442 ->
      "<SYNTAX ERROR>\n"
    | 2443 ->
      "<SYNTAX ERROR>\n"
    | 2446 ->
      "<SYNTAX ERROR>\n"
    | 2447 ->
      "<SYNTAX ERROR>\n"
    | 2444 ->
      "<SYNTAX ERROR>\n"
    | 2465 ->
      "<SYNTAX ERROR>\n"
    | 2466 ->
      "<SYNTAX ERROR>\n"
    | 2469 ->
      "<SYNTAX ERROR>\n"
    | 2468 ->
      "<SYNTAX ERROR>\n"
    | 1871 ->
      "Expecting a match case\n"
    | 887 ->
      "<SYNTAX ERROR>\n"
    | 113 ->
      "<SYNTAX ERROR>\n"
    | 114 ->
      "<SYNTAX ERROR>\n"
    | 888 ->
      "<SYNTAX ERROR>\n"
    | 1846 ->
      "<SYNTAX ERROR>\n"
    | 1849 ->
      "<SYNTAX ERROR>\n"
    | 1863 ->
      "<SYNTAX ERROR>\n"
    | 1851 ->
      "<SYNTAX ERROR>\n"
    | 1852 ->
      "<SYNTAX ERROR>\n"
    | 1855 ->
      "<SYNTAX ERROR>\n"
    | 1857 ->
      "<SYNTAX ERROR>\n"
    | 1858 ->
      "<SYNTAX ERROR>\n"
    | 1860 ->
      "<SYNTAX ERROR>\n"
    | 178 ->
      "<SYNTAX ERROR>\n"
    | 2481 ->
      "<SYNTAX ERROR>\n"
    | 2482 ->
      "<SYNTAX ERROR>\n"
    | 1114 ->
      "Incomplete module item, forgetting a \";\"?\n"
    | 1169 ->
      "<SYNTAX ERROR>\n"
    | 1172 ->
      "<SYNTAX ERROR>\n"
    | 6 ->
      "<SYNTAX ERROR>\n"
    | 1194 ->
      "<SYNTAX ERROR>\n"
    | 392 ->
      "<SYNTAX ERROR>\n"
    | 393 ->
      "<SYNTAX ERROR>\n"
    | 340 ->
      "<SYNTAX ERROR>\n"
    | 396 ->
      "<SYNTAX ERROR>\n"
    | 398 ->
      "<SYNTAX ERROR>\n"
    | 7 ->
      "<SYNTAX ERROR>\n"
    | 402 ->
      "<SYNTAX ERROR>\n"
    | 403 ->
      "<SYNTAX ERROR>\n"
    | 405 ->
      "<SYNTAX ERROR>\n"
    | 885 ->
      "<SYNTAX ERROR>\n"
    | 534 ->
      "<SYNTAX ERROR>\n"
    | 16 ->
      "<SYNTAX ERROR>\n"
    | 2587 ->
      "<SYNTAX ERROR>\n"
    | 605 ->
      "<SYNTAX ERROR>\n"
    | 606 ->
      "<SYNTAX ERROR>\n"
    | 2234 ->
      "<SYNTAX ERROR>\n"
    | 2236 ->
      "<SYNTAX ERROR>\n"
    | 2237 ->
      "<SYNTAX ERROR>\n"
    | 2239 ->
      "<SYNTAX ERROR>\n"
    | 2240 ->
      "<SYNTAX ERROR>\n"
    | 1373 ->
      "<SYNTAX ERROR>\n"
    | 602 ->
      "<SYNTAX ERROR>\n"
    | 1431 ->
      "<SYNTAX ERROR>\n"
    | 1432 ->
      "<SYNTAX ERROR>\n"
    | 1433 ->
      "<SYNTAX ERROR>\n"
    | 1388 ->
      "<SYNTAX ERROR>\n"
    | 1394 ->
      "<SYNTAX ERROR>\n"
    | 1396 ->
      "<SYNTAX ERROR>\n"
    | 1390 ->
      "<SYNTAX ERROR>\n"
    | 1398 ->
      "<SYNTAX ERROR>\n"
    | 1400 ->
      "<SYNTAX ERROR>\n"
    | 1402 ->
      "<SYNTAX ERROR>\n"
    | 194 ->
      "<SYNTAX ERROR>\n"
    | 1404 ->
      "<SYNTAX ERROR>\n"
    | 1410 ->
      "<SYNTAX ERROR>\n"
    | 1412 ->
      "<SYNTAX ERROR>\n"
    | 2399 ->
      "<SYNTAX ERROR>\n"
    | 336 ->
      "<SYNTAX ERROR>\n"
    | 1375 ->
      "<SYNTAX ERROR>\n"
    | 1392 ->
      "<SYNTAX ERROR>\n"
    | 1406 ->
      "<SYNTAX ERROR>\n"
    | 1408 ->
      "<SYNTAX ERROR>\n"
    | 1414 ->
      "<SYNTAX ERROR>\n"
    | 1416 ->
      "<SYNTAX ERROR>\n"
    | 898 ->
      "<SYNTAX ERROR>\n"
    | 899 ->
      "<SYNTAX ERROR>\n"
    | 1786 ->
      "<SYNTAX ERROR>\n"
    | 900 ->
      "<SYNTAX ERROR>\n"
    | 901 ->
      "<SYNTAX ERROR>\n"
    | 1779 ->
      "<SYNTAX ERROR>\n"
    | 1781 ->
      "<SYNTAX ERROR>\n"
    | 1789 ->
      "<SYNTAX ERROR>\n"
    | 1791 ->
      "<SYNTAX ERROR>\n"
    | 1806 ->
      "<SYNTAX ERROR>\n"
    | 1818 ->
      "<SYNTAX ERROR>\n"
    | 1823 ->
      "<SYNTAX ERROR>\n"
    | 2332 ->
      "<SYNTAX ERROR>\n"
    | 2337 ->
      "<SYNTAX ERROR>\n"
    | 2334 ->
      "<SYNTAX ERROR>\n"
    | 1253 ->
      "<SYNTAX ERROR>\n"
    | 2331 ->
      "<SYNTAX ERROR>\n"
    | 1424 ->
      "<SYNTAX ERROR>\n"
    | 1451 ->
      "<SYNTAX ERROR>\n"
    | 1272 ->
      "<SYNTAX ERROR>\n"
    | 1418 ->
      "<SYNTAX ERROR>\n"
    | 1420 ->
      "<SYNTAX ERROR>\n"
    | 1422 ->
      "<SYNTAX ERROR>\n"
    | 176 ->
      "<SYNTAX ERROR>\n"
    | 2487 ->
      "<SYNTAX ERROR>\n"
    | 2486 ->
      "<SYNTAX ERROR>\n"
    | 2489 ->
      "<SYNTAX ERROR>\n"
    | 1363 ->
      "<SYNTAX ERROR>\n"
    | 1364 ->
      "<SYNTAX ERROR>\n"
    | 1426 ->
      "<SYNTAX ERROR>\n"
    | 1429 ->
      "<SYNTAX ERROR>\n"
    | 1447 ->
      "<SYNTAX ERROR>\n"
    | 1435 ->
      "<SYNTAX ERROR>\n"
    | 1436 ->
      "<SYNTAX ERROR>\n"
    | 1439 ->
      "<SYNTAX ERROR>\n"
    | 1441 ->
      "<SYNTAX ERROR>\n"
    | 1442 ->
      "<SYNTAX ERROR>\n"
    | 1444 ->
      "<SYNTAX ERROR>\n"
    | 183 ->
      "<SYNTAX ERROR>\n"
    | 2433 ->
      "<SYNTAX ERROR>\n"
    | 2434 ->
      "<SYNTAX ERROR>\n"
    | 1304 ->
      "<SYNTAX ERROR>\n"
    | 1312 ->
      "<SYNTAX ERROR>\n"
    | 788 ->
      "<SYNTAX ERROR>\n"
    | 197 ->
      "<SYNTAX ERROR>\n"
    | 414 ->
      "<SYNTAX ERROR>\n"
    | 415 ->
      "<SYNTAX ERROR>\n"
    | 187 ->
      "<SYNTAX ERROR>\n"
    | 189 ->
      "<SYNTAX ERROR>\n"
    | 190 ->
      "<SYNTAX ERROR>\n"
    | 535 ->
      "<SYNTAX ERROR>\n"
    | 2318 ->
      "<SYNTAX ERROR>\n"
    | 2320 ->
      "<SYNTAX ERROR>\n"
    | 2322 ->
      "<SYNTAX ERROR>\n"
    | 1783 ->
      "<SYNTAX ERROR>\n"
    | 1784 ->
      "<SYNTAX ERROR>\n"
    | 413 ->
      "Expecting one of the following:\n  - \")\" to form a unit value \"()\"\n  - \"module\" to start a module expression\n  - an expression\n  - an operator to denote the prefix form of an operator\n"
    | 2376 ->
      "<SYNTAX ERROR>\n"
    | 716 ->
      "<SYNTAX ERROR>\n"
    | 416 ->
      "Expecting a module expression\n"
    | 2363 ->
      "<SYNTAX ERROR>\n"
    | 2365 ->
      "<SYNTAX ERROR>\n"
    | 2367 ->
      "<SYNTAX ERROR>\n"
    | 2369 ->
      "<SYNTAX ERROR>\n"
    | 2370 ->
      "<SYNTAX ERROR>\n"
    | 2371 ->
      "<SYNTAX ERROR>\n"
    | 2372 ->
      "<SYNTAX ERROR>\n"
    | 2373 ->
      "<SYNTAX ERROR>\n"
    | 2374 ->
      "<SYNTAX ERROR>\n"
    | 1371 ->
      "<SYNTAX ERROR>\n"
    | 2421 ->
      "<SYNTAX ERROR>\n"
    | 199 ->
      "<SYNTAX ERROR>\n"
    | 551 ->
      "<SYNTAX ERROR>\n"
    | 2291 ->
      "<SYNTAX ERROR>\n"
    | 552 ->
      "<SYNTAX ERROR>\n"
    | 1813 ->
      "<SYNTAX ERROR>\n"
    | 1812 ->
      "<SYNTAX ERROR>\n"
    | 1807 ->
      "<SYNTAX ERROR>\n"
    | 1808 ->
      "<SYNTAX ERROR>\n"
    | 570 ->
      "<SYNTAX ERROR>\n"
    | 579 ->
      "<SYNTAX ERROR>\n"
    | 589 ->
      "<SYNTAX ERROR>\n"
    | 607 ->
      "<SYNTAX ERROR>\n"
    | 608 ->
      "<SYNTAX ERROR>\n"
    | 610 ->
      "<SYNTAX ERROR>\n"
    | 611 ->
      "<SYNTAX ERROR>\n"
    | 2231 ->
      "<SYNTAX ERROR>\n"
    | 2217 ->
      "<SYNTAX ERROR>\n"
    | 616 ->
      "<SYNTAX ERROR>\n"
    | 617 ->
      "<SYNTAX ERROR>\n"
    | 618 ->
      "<SYNTAX ERROR>\n"
    | 619 ->
      "<SYNTAX ERROR>\n"
    | 2215 ->
      "<SYNTAX ERROR>\n"
    | 2216 ->
      "<SYNTAX ERROR>\n"
    | 612 ->
      "<SYNTAX ERROR>\n"
    | 613 ->
      "<SYNTAX ERROR>\n"
    | 614 ->
      "<SYNTAX ERROR>\n"
    | 615 ->
      "<SYNTAX ERROR>\n"
    | 2224 ->
      "<SYNTAX ERROR>\n"
    | 2225 ->
      "<SYNTAX ERROR>\n"
    | 2226 ->
      "<SYNTAX ERROR>\n"
    | 2227 ->
      "<SYNTAX ERROR>\n"
    | 2228 ->
      "<SYNTAX ERROR>\n"
    | 2229 ->
      "<SYNTAX ERROR>\n"
    | 889 ->
      "<SYNTAX ERROR>\n"
    | 890 ->
      "<SYNTAX ERROR>\n"
    | 891 ->
      "<SYNTAX ERROR>\n"
    | 892 ->
      "<SYNTAX ERROR>\n"
    | 893 ->
      "<SYNTAX ERROR>\n"
    | 894 ->
      "<SYNTAX ERROR>\n"
    | 2324 ->
      "<SYNTAX ERROR>\n"
    | 2325 ->
      "<SYNTAX ERROR>\n"
    | 2326 ->
      "<SYNTAX ERROR>\n"
    | 2327 ->
      "<SYNTAX ERROR>\n"
    | 2328 ->
      "<SYNTAX ERROR>\n"
    | 2329 ->
      "<SYNTAX ERROR>\n"
    | 1785 ->
      "<SYNTAX ERROR>\n"
    | 597 ->
      "<SYNTAX ERROR>\n"
    | 1359 ->
      "<SYNTAX ERROR>\n"
    | 1180 ->
      "<SYNTAX ERROR>\n"
    | 903 ->
      "Incomplete let binding\n"
    | 1282 ->
      "<SYNTAX ERROR>\n"
    | 1288 ->
      "<SYNTAX ERROR>\n"
    | 1283 ->
      "<SYNTAX ERROR>\n"
    | 1284 ->
      "<SYNTAX ERROR>\n"
    | 1285 ->
      "<SYNTAX ERROR>\n"
    | 1287 ->
      "<SYNTAX ERROR>\n"
    | 1156 ->
      "<SYNTAX ERROR>\n"
    | 904 ->
      "<SYNTAX ERROR>\n"
    | 905 ->
      "<SYNTAX ERROR>\n"
    | 1761 ->
      "<SYNTAX ERROR>\n"
    | 1762 ->
      "<SYNTAX ERROR>\n"
    | 1763 ->
      "<SYNTAX ERROR>\n"
    | 1764 ->
      "<SYNTAX ERROR>\n"
    | 1768 ->
      "<SYNTAX ERROR>\n"
    | 1765 ->
      "<SYNTAX ERROR>\n"
    | 1766 ->
      "<SYNTAX ERROR>\n"
    | 1767 ->
      "<SYNTAX ERROR>\n"
    | 906 ->
      "<SYNTAX ERROR>\n"
    | 907 ->
      "<SYNTAX ERROR>\n"
    | 916 ->
      "<SYNTAX ERROR>\n"
    | 1754 ->
      "<SYNTAX ERROR>\n"
    | 1755 ->
      "<SYNTAX ERROR>\n"
    | 1756 ->
      "<SYNTAX ERROR>\n"
    | 1772 ->
      "<SYNTAX ERROR>\n"
    | 1134 ->
      "<SYNTAX ERROR>\n"
    | 1135 ->
      "<SYNTAX ERROR>\n"
    | 1157 ->
      "<SYNTAX ERROR>\n"
    | 1278 ->
      "Defining a function?\nExpecting one of the following:\n  - \"=>\" to start the function body\n  - an identifier to add an function parameter\n  - \":\" to specify the return type\n"
    | 1246 ->
      "<SYNTAX ERROR>\n"
    | 1162 ->
      "<SYNTAX ERROR>\n"
    | 1163 ->
      "<SYNTAX ERROR>\n"
    | 1164 ->
      "<SYNTAX ERROR>\n"
    | 1165 ->
      "<SYNTAX ERROR>\n"
    | 1166 ->
      "Expecting an expression as function body\n"
    | 1240 ->
      "<SYNTAX ERROR>\n"
    | 1241 ->
      "Defining a function?\nExpecting \"=>\" to start the function body\n"
    | 1242 ->
      "<SYNTAX ERROR>\n"
    | 1243 ->
      "<SYNTAX ERROR>\n"
    | 1158 ->
      "<SYNTAX ERROR>\n"
    | 1159 ->
      "<SYNTAX ERROR>\n"
    | 1160 ->
      "<SYNTAX ERROR>\n"
    | 1161 ->
      "<SYNTAX ERROR>\n"
    | 1251 ->
      "<SYNTAX ERROR>\n"
    | 1274 ->
      "<SYNTAX ERROR>\n"
    | 1275 ->
      "<SYNTAX ERROR>\n"
    | 1255 ->
      "<SYNTAX ERROR>\n"
    | 1256 ->
      "<SYNTAX ERROR>\n"
    | 1257 ->
      "<SYNTAX ERROR>\n"
    | 1260 ->
      "<SYNTAX ERROR>\n"
    | 1261 ->
      "<SYNTAX ERROR>\n"
    | 1262 ->
      "<SYNTAX ERROR>\n"
    | 1265 ->
      "<SYNTAX ERROR>\n"
    | 1266 ->
      "<SYNTAX ERROR>\n"
    | 1267 ->
      "<SYNTAX ERROR>\n"
    | 1268 ->
      "<SYNTAX ERROR>\n"
    | 1264 ->
      "<SYNTAX ERROR>\n"
    | 1504 ->
      "<SYNTAX ERROR>\n"
    | 70 ->
      "<SYNTAX ERROR>\n"
    | 1705 ->
      "<SYNTAX ERROR>\n"
    | 201 ->
      "<SYNTAX ERROR>\n"
    | 2419 ->
      "<SYNTAX ERROR>\n"
    | 2420 ->
      "<SYNTAX ERROR>\n"
    | 2418 ->
      "<SYNTAX ERROR>\n"
    | 71 ->
      "<SYNTAX ERROR>\n"
    | 1055 ->
      "<SYNTAX ERROR>\n"
    | 1028 ->
      "<SYNTAX ERROR>\n"
    | 2585 ->
      "<SYNTAX ERROR>\n"
    | 18 ->
      "<SYNTAX ERROR>\n"
    | 174 ->
      "<SYNTAX ERROR>\n"
    | 2493 ->
      "<SYNTAX ERROR>\n"
    | 1792 ->
      "<SYNTAX ERROR>\n"
    | 1795 ->
      "<SYNTAX ERROR>\n"
    | 1797 ->
      "<SYNTAX ERROR>\n"
    | 1800 ->
      "Expecting a type name\n"
    | 186 ->
      "Expecting an expression\n"
    | 1385 ->
      "Expecting an expression\n"
    | 1361 ->
      "Expecting an expression\n"
    | 596 ->
      "<SYNTAX ERROR>\n"
    | 1703 ->
      "Expecting \"]\" to finish current floating attribute\n"
    | 1030 ->
      "<SYNTAX ERROR>\n"
    | 177 ->
      "Expecting one of the following:\n  - an list item\n  - \"]\" to finish this list\n"
    | 2199 ->
      "Expecting one of the following:\n  - \",\" to separate two items in a list\n  - \"]\" to finish this list\n"
    | 2200 ->
      "<SYNTAX ERROR>\n"
    | 2196 ->
      "<SYNTAX ERROR>\n"
    | 2197 ->
      "<SYNTAX ERROR>\n"
    | 179 ->
      "<SYNTAX ERROR>\n"
    | 180 ->
      "<SYNTAX ERROR>\n"
    | 573 ->
      "<SYNTAX ERROR>\n"
    | 181 ->
      "<SYNTAX ERROR>\n"
    | 2475 ->
      "<SYNTAX ERROR>\n"
    | 184 ->
      "<SYNTAX ERROR>\n"
    | 1340 ->
      "<SYNTAX ERROR>\n"
    | 1341 ->
      "<SYNTAX ERROR>\n"
    | 1342 ->
      "<SYNTAX ERROR>\n"
    | 1343 ->
      "<SYNTAX ERROR>\n"
    | 1344 ->
      "<SYNTAX ERROR>\n"
    | 1345 ->
      "<SYNTAX ERROR>\n"
    | 1350 ->
      "<SYNTAX ERROR>\n"
    | 1351 ->
      "<SYNTAX ERROR>\n"
    | 1352 ->
      "<SYNTAX ERROR>\n"
    | 1353 ->
      "<SYNTAX ERROR>\n"
    | 1354 ->
      "<SYNTAX ERROR>\n"
    | 1357 ->
      "<SYNTAX ERROR>\n"
    | 1358 ->
      "<SYNTAX ERROR>\n"
    | 1450 ->
      "<SYNTAX ERROR>\n"
    | 1452 ->
      "<SYNTAX ERROR>\n"
    | 1453 ->
      "<SYNTAX ERROR>\n"
    | 1454 ->
      "<SYNTAX ERROR>\n"
    | 1349 ->
      "<SYNTAX ERROR>\n"
    | 2344 ->
      "<SYNTAX ERROR>\n"
    | 532 ->
      "<SYNTAX ERROR>\n"
    | 2274 ->
      "<SYNTAX ERROR>\n"
    | 2251 ->
      "<SYNTAX ERROR>\n"
    | 1455 ->
      "<SYNTAX ERROR>\n"
    | 1457 ->
      "<SYNTAX ERROR>\n"
    | 1458 ->
      "<SYNTAX ERROR>\n"
    | 1459 ->
      "<SYNTAX ERROR>\n"
    | 1460 ->
      "<SYNTAX ERROR>\n"
    | 1101 ->
      "<SYNTAX ERROR>\n"
    | 1103 ->
      "<SYNTAX ERROR>\n"
    | 1104 ->
      "<SYNTAX ERROR>\n"
    | 1462 ->
      "<SYNTAX ERROR>\n"
    | 1463 ->
      "<SYNTAX ERROR>\n"
    | 1464 ->
      "<SYNTAX ERROR>\n"
    | 1465 ->
      "<SYNTAX ERROR>\n"
    | 1468 ->
      "<SYNTAX ERROR>\n"
    | 1479 ->
      "<SYNTAX ERROR>\n"
    | 1469 ->
      "<SYNTAX ERROR>\n"
    | 1470 ->
      "<SYNTAX ERROR>\n"
    | 1471 ->
      "<SYNTAX ERROR>\n"
    | 1472 ->
      "<SYNTAX ERROR>\n"
    | 1473 ->
      "<SYNTAX ERROR>\n"
    | 1539 ->
      "<SYNTAX ERROR>\n"
    | 1483 ->
      "<SYNTAX ERROR>\n"
    | 1484 ->
      "<SYNTAX ERROR>\n"
    | 1485 ->
      "<SYNTAX ERROR>\n"
    | 1492 ->
      "<SYNTAX ERROR>\n"
    | 1493 ->
      "<SYNTAX ERROR>\n"
    | 1494 ->
      "<SYNTAX ERROR>\n"
    | 1486 ->
      "<SYNTAX ERROR>\n"
    | 1488 ->
      "<SYNTAX ERROR>\n"
    | 1489 ->
      "<SYNTAX ERROR>\n"
    | 1490 ->
      "<SYNTAX ERROR>\n"
    | 1491 ->
      "<SYNTAX ERROR>\n"
    | 1456 ->
      "<SYNTAX ERROR>\n"
    | 1840 ->
      "<SYNTAX ERROR>\n"
    | 1831 ->
      "<SYNTAX ERROR>\n"
    | 1829 ->
      "<SYNTAX ERROR>\n"
    | 1832 ->
      "<SYNTAX ERROR>\n"
    | 1833 ->
      "<SYNTAX ERROR>\n"
    | 1843 ->
      "<SYNTAX ERROR>\n"
    | 1844 ->
      "<SYNTAX ERROR>\n"
    | 581 ->
      "<SYNTAX ERROR>\n"
    | 2056 ->
      "<SYNTAX ERROR>\n"
    | 2062 ->
      "<SYNTAX ERROR>\n"
    | 2057 ->
      "<SYNTAX ERROR>\n"
    | 2058 ->
      "<SYNTAX ERROR>\n"
    | 2059 ->
      "<SYNTAX ERROR>\n"
    | 2061 ->
      "<SYNTAX ERROR>\n"
    | 2026 ->
      "<SYNTAX ERROR>\n"
    | 583 ->
      "<SYNTAX ERROR>\n"
    | 587 ->
      "<SYNTAX ERROR>\n"
    | 588 ->
      "<SYNTAX ERROR>\n"
    | 584 ->
      "<SYNTAX ERROR>\n"
    | 2261 ->
      "<SYNTAX ERROR>\n"
    | 2262 ->
      "<SYNTAX ERROR>\n"
    | 2265 ->
      "<SYNTAX ERROR>\n"
    | 2264 ->
      "<SYNTAX ERROR>\n"
    | 2027 ->
      "<SYNTAX ERROR>\n"
    | 2052 ->
      "<SYNTAX ERROR>\n"
    | 1475 ->
      "<SYNTAX ERROR>\n"
    | 1476 ->
      "<SYNTAX ERROR>\n"
    | 1477 ->
      "<SYNTAX ERROR>\n"
    | 1478 ->
      "<SYNTAX ERROR>\n"
    | 2028 ->
      "<SYNTAX ERROR>\n"
    | 2029 ->
      "<SYNTAX ERROR>\n"
    | 2030 ->
      "<SYNTAX ERROR>\n"
    | 2031 ->
      "<SYNTAX ERROR>\n"
    | 2033 ->
      "<SYNTAX ERROR>\n"
    | 2048 ->
      "<SYNTAX ERROR>\n"
    | 2049 ->
      "<SYNTAX ERROR>\n"
    | 2035 ->
      "<SYNTAX ERROR>\n"
    | 2036 ->
      "<SYNTAX ERROR>\n"
    | 2038 ->
      "<SYNTAX ERROR>\n"
    | 2039 ->
      "<SYNTAX ERROR>\n"
    | 2040 ->
      "<SYNTAX ERROR>\n"
    | 2043 ->
      "<SYNTAX ERROR>\n"
    | 2044 ->
      "<SYNTAX ERROR>\n"
    | 2045 ->
      "<SYNTAX ERROR>\n"
    | 2046 ->
      "<SYNTAX ERROR>\n"
    | 2042 ->
      "<SYNTAX ERROR>\n"
    | 2424 ->
      "Expecting \"}\" to finish the block\n"
    | 2158 ->
      "<SYNTAX ERROR>\n"
    | 1679 ->
      "<SYNTAX ERROR>\n"
    | 1111 ->
      "<SYNTAX ERROR>\n"
    | 1501 ->
      "<SYNTAX ERROR>\n"
    | 1498 ->
      "<SYNTAX ERROR>\n"
    | 1517 ->
      "<SYNTAX ERROR>\n"
    | 1518 ->
      "<SYNTAX ERROR>\n"
    | 1521 ->
      "<SYNTAX ERROR>\n"
    | 1153 ->
      "<SYNTAX ERROR>\n"
    | 1550 ->
      "<SYNTAX ERROR>\n"
    | 1553 ->
      "<SYNTAX ERROR>\n"
    | 1575 ->
      "<SYNTAX ERROR>\n"
    | 1554 ->
      "<SYNTAX ERROR>\n"
    | 1560 ->
      "<SYNTAX ERROR>\n"
    | 1561 ->
      "<SYNTAX ERROR>\n"
    | 1565 ->
      "<SYNTAX ERROR>\n"
    | 1566 ->
      "<SYNTAX ERROR>\n"
    | 1567 ->
      "<SYNTAX ERROR>\n"
    | 1556 ->
      "<SYNTAX ERROR>\n"
    | 1574 ->
      "<SYNTAX ERROR>\n"
    | 1571 ->
      "<SYNTAX ERROR>\n"
    | 1572 ->
      "<SYNTAX ERROR>\n"
    | 1573 ->
      "<SYNTAX ERROR>\n"
    | 1580 ->
      "<SYNTAX ERROR>\n"
    | 1581 ->
      "<SYNTAX ERROR>\n"
    | 1582 ->
      "<SYNTAX ERROR>\n"
    | 1302 ->
      "<SYNTAX ERROR>\n"
    | 1314 ->
      "<SYNTAX ERROR>\n"
    | 1540 ->
      "<SYNTAX ERROR>\n"
    | 1523 ->
      "<SYNTAX ERROR>\n"
    | 1524 ->
      "<SYNTAX ERROR>\n"
    | 1154 ->
      "<SYNTAX ERROR>\n"
    | 1336 ->
      "<SYNTAX ERROR>\n"
    | 1544 ->
      "<SYNTAX ERROR>\n"
    | 1155 ->
      "<SYNTAX ERROR>\n"
    | 1332 ->
      "<SYNTAX ERROR>\n"
    | 1333 ->
      "<SYNTAX ERROR>\n"
    | 1292 ->
      "<SYNTAX ERROR>\n"
    | 1294 ->
      "<SYNTAX ERROR>\n"
    | 1295 ->
      "<SYNTAX ERROR>\n"
    | 1320 ->
      "<SYNTAX ERROR>\n"
    | 1296 ->
      "<SYNTAX ERROR>\n"
    | 1297 ->
      "<SYNTAX ERROR>\n"
    | 1298 ->
      "<SYNTAX ERROR>\n"
    | 1522 ->
      "<SYNTAX ERROR>\n"
    | 1824 ->
      "<SYNTAX ERROR>\n"
    | 1825 ->
      "<SYNTAX ERROR>\n"
    | 1826 ->
      "<SYNTAX ERROR>\n"
    | 1528 ->
      "<SYNTAX ERROR>\n"
    | 1529 ->
      "<SYNTAX ERROR>\n"
    | 1530 ->
      "<SYNTAX ERROR>\n"
    | 1327 ->
      "<SYNTAX ERROR>\n"
    | 1328 ->
      "<SYNTAX ERROR>\n"
    | 1339 ->
      "<SYNTAX ERROR>\n"
    | 555 ->
      "<SYNTAX ERROR>\n"
    | 1032 ->
      "<SYNTAX ERROR>\n"
    | 909 ->
      "<SYNTAX ERROR>\n"
    | 853 ->
      "<SYNTAX ERROR>\n"
    | 854 ->
      "<SYNTAX ERROR>\n"
    | 1886 ->
      "<SYNTAX ERROR>\n"
    | 1888 ->
      "<SYNTAX ERROR>\n"
    | 1891 ->
      "<SYNTAX ERROR>\n"
    | 2176 ->
      "<SYNTAX ERROR>\n"
    | 902 ->
      "<SYNTAX ERROR>\n"
    | 1777 ->
      "<SYNTAX ERROR>\n"
    | 409 ->
      "<SYNTAX ERROR>\n"
    | 410 ->
      "<SYNTAX ERROR>\n"
    | 2384 ->
      "<SYNTAX ERROR>\n"
    | 2386 ->
      "<SYNTAX ERROR>\n"
    | 1889 ->
      "<SYNTAX ERROR>\n"
    | 2388 ->
      "<SYNTAX ERROR>\n"
    | 1894 ->
      "<SYNTAX ERROR>\n"
    | 1895 ->
      "<SYNTAX ERROR>\n"
    | 2390 ->
      "<SYNTAX ERROR>\n"
    | 1997 ->
      "<SYNTAX ERROR>\n"
    | 1972 ->
      "<SYNTAX ERROR>\n"
    | 1973 ->
      "<SYNTAX ERROR>\n"
    | 1974 ->
      "<SYNTAX ERROR>\n"
    | 1976 ->
      "<SYNTAX ERROR>\n"
    | 1979 ->
      "<SYNTAX ERROR>\n"
    | 1986 ->
      "<SYNTAX ERROR>\n"
    | 1989 ->
      "<SYNTAX ERROR>\n"
    | 1990 ->
      "<SYNTAX ERROR>\n"
    | 2179 ->
      "<SYNTAX ERROR>\n"
    | 2180 ->
      "<SYNTAX ERROR>\n"
    | 548 ->
      "<SYNTAX ERROR>\n"
    | 549 ->
      "<SYNTAX ERROR>\n"
    | 2295 ->
      "<SYNTAX ERROR>\n"
    | 2297 ->
      "<SYNTAX ERROR>\n"
    | 1977 ->
      "<SYNTAX ERROR>\n"
    | 2299 ->
      "<SYNTAX ERROR>\n"
    | 1982 ->
      "<SYNTAX ERROR>\n"
    | 1983 ->
      "<SYNTAX ERROR>\n"
    | 2301 ->
      "<SYNTAX ERROR>\n"
    | 1992 ->
      "<SYNTAX ERROR>\n"
    | 1993 ->
      "<SYNTAX ERROR>\n"
    | 1898 ->
      "<SYNTAX ERROR>\n"
    | 1967 ->
      "<SYNTAX ERROR>\n"
    | 1968 ->
      "<SYNTAX ERROR>\n"
    | 1969 ->
      "<SYNTAX ERROR>\n"
    | 1971 ->
      "<SYNTAX ERROR>\n"
    | 417 ->
      "<SYNTAX ERROR>\n"
    | 2128 ->
      "<SYNTAX ERROR>\n"
    | 420 ->
      "<SYNTAX ERROR>\n"
    | 434 ->
      "<SYNTAX ERROR>\n"
    | 423 ->
      "<SYNTAX ERROR>\n"
    | 2348 ->
      "<SYNTAX ERROR>\n"
    | 2352 ->
      "<SYNTAX ERROR>\n"
    | 2349 ->
      "<SYNTAX ERROR>\n"
    | 2350 ->
      "<SYNTAX ERROR>\n"
    | 425 ->
      "<SYNTAX ERROR>\n"
    | 427 ->
      "<SYNTAX ERROR>\n"
    | 429 ->
      "<SYNTAX ERROR>\n"
    | 431 ->
      "<SYNTAX ERROR>\n"
    | 2135 ->
      "<SYNTAX ERROR>\n"
    | 435 ->
      "<SYNTAX ERROR>\n"
    | 504 ->
      "<SYNTAX ERROR>\n"
    | 505 ->
      "<SYNTAX ERROR>\n"
    | 506 ->
      "<SYNTAX ERROR>\n"
    | 510 ->
      "<SYNTAX ERROR>\n"
    | 511 ->
      "<SYNTAX ERROR>\n"
    | 436 ->
      "<SYNTAX ERROR>\n"
    | 473 ->
      "<SYNTAX ERROR>\n"
    | 443 ->
      "<SYNTAX ERROR>\n"
    | 476 ->
      "<SYNTAX ERROR>\n"
    | 438 ->
      "<SYNTAX ERROR>\n"
    | 439 ->
      "<SYNTAX ERROR>\n"
    | 481 ->
      "<SYNTAX ERROR>\n"
    | 498 ->
      "<SYNTAX ERROR>\n"
    | 517 ->
      "<SYNTAX ERROR>\n"
    | 518 ->
      "<SYNTAX ERROR>\n"
    | 519 ->
      "<SYNTAX ERROR>\n"
    | 520 ->
      "<SYNTAX ERROR>\n"
    | 2136 ->
      "<SYNTAX ERROR>\n"
    | 2138 ->
      "<SYNTAX ERROR>\n"
    | 2127 ->
      "<SYNTAX ERROR>\n"
    | 1899 ->
      "<SYNTAX ERROR>\n"
    | 1900 ->
      "<SYNTAX ERROR>\n"
    | 1903 ->
      "<SYNTAX ERROR>\n"
    | 1904 ->
      "<SYNTAX ERROR>\n"
    | 1906 ->
      "<SYNTAX ERROR>\n"
    | 1963 ->
      "<SYNTAX ERROR>\n"
    | 1964 ->
      "<SYNTAX ERROR>\n"
    | 1965 ->
      "<SYNTAX ERROR>\n"
    | 2013 ->
      "<SYNTAX ERROR>\n"
    | 2014 ->
      "<SYNTAX ERROR>\n"
    | 2015 ->
      "<SYNTAX ERROR>\n"
    | 2016 ->
      "<SYNTAX ERROR>\n"
    | 2017 ->
      "<SYNTAX ERROR>\n"
    | 2018 ->
      "<SYNTAX ERROR>\n"
    | 2019 ->
      "<SYNTAX ERROR>\n"
    | 1966 ->
      "<SYNTAX ERROR>\n"
    | 2004 ->
      "<SYNTAX ERROR>\n"
    | 2005 ->
      "<SYNTAX ERROR>\n"
    | 2006 ->
      "<SYNTAX ERROR>\n"
    | 2007 ->
      "<SYNTAX ERROR>\n"
    | 2008 ->
      "<SYNTAX ERROR>\n"
    | 2021 ->
      "<SYNTAX ERROR>\n"
    | 2142 ->
      "<SYNTAX ERROR>\n"
    | 2143 ->
      "<SYNTAX ERROR>\n"
    | 2066 ->
      "<SYNTAX ERROR>\n"
    | 2069 ->
      "<SYNTAX ERROR>\n"
    | 2070 ->
      "<SYNTAX ERROR>\n"
    | 2071 ->
      "<SYNTAX ERROR>\n"
    | 2072 ->
      "<SYNTAX ERROR>\n"
    | 2073 ->
      "<SYNTAX ERROR>\n"
    | 2074 ->
      "<SYNTAX ERROR>\n"
    | 2078 ->
      "<SYNTAX ERROR>\n"
    | 2083 ->
      "<SYNTAX ERROR>\n"
    | 447 ->
      "<SYNTAX ERROR>\n"
    | 448 ->
      "<SYNTAX ERROR>\n"
    | 449 ->
      "<SYNTAX ERROR>\n"
    | 2084 ->
      "<SYNTAX ERROR>\n"
    | 2085 ->
      "<SYNTAX ERROR>\n"
    | 2086 ->
      "<SYNTAX ERROR>\n"
    | 463 ->
      "<SYNTAX ERROR>\n"
    | 445 ->
      "<SYNTAX ERROR>\n"
    | 2091 ->
      "<SYNTAX ERROR>\n"
    | 2100 ->
      "<SYNTAX ERROR>\n"
    | 2092 ->
      "<SYNTAX ERROR>\n"
    | 2093 ->
      "<SYNTAX ERROR>\n"
    | 2095 ->
      "<SYNTAX ERROR>\n"
    | 2096 ->
      "<SYNTAX ERROR>\n"
    | 2097 ->
      "<SYNTAX ERROR>\n"
    | 2147 ->
      "<SYNTAX ERROR>\n"
    | 2148 ->
      "<SYNTAX ERROR>\n"
    | 2102 ->
      "<SYNTAX ERROR>\n"
    | 2111 ->
      "<SYNTAX ERROR>\n"
    | 2112 ->
      "<SYNTAX ERROR>\n"
    | 2113 ->
      "<SYNTAX ERROR>\n"
    | 2114 ->
      "<SYNTAX ERROR>\n"
    | 2115 ->
      "<SYNTAX ERROR>\n"
    | 2116 ->
      "<SYNTAX ERROR>\n"
    | 2117 ->
      "<SYNTAX ERROR>\n"
    | 2103 ->
      "<SYNTAX ERROR>\n"
    | 2104 ->
      "<SYNTAX ERROR>\n"
    | 2152 ->
      "<SYNTAX ERROR>\n"
    | 2153 ->
      "<SYNTAX ERROR>\n"
    | 2105 ->
      "<SYNTAX ERROR>\n"
    | 2106 ->
      "<SYNTAX ERROR>\n"
    | 2107 ->
      "<SYNTAX ERROR>\n"
    | 2108 ->
      "<SYNTAX ERROR>\n"
    | 536 ->
      "<SYNTAX ERROR>\n"
    | 537 ->
      "<SYNTAX ERROR>\n"
    | 541 ->
      "<SYNTAX ERROR>\n"
    | 542 ->
      "<SYNTAX ERROR>\n"
    | 2313 ->
      "<SYNTAX ERROR>\n"
    | 2315 ->
      "<SYNTAX ERROR>\n"
    | 2316 ->
      "<SYNTAX ERROR>\n"
    | 2317 ->
      "<SYNTAX ERROR>\n"
    | 855 ->
      "<SYNTAX ERROR>\n"
    | 856 ->
      "<SYNTAX ERROR>\n"
    | 858 ->
      "<SYNTAX ERROR>\n"
    | 859 ->
      "<SYNTAX ERROR>\n"
    | 1867 ->
      "<SYNTAX ERROR>\n"
    | 864 ->
      "<SYNTAX ERROR>\n"
    | 865 ->
      "<SYNTAX ERROR>\n"
    | 866 ->
      "<SYNTAX ERROR>\n"
    | 867 ->
      "<SYNTAX ERROR>\n"
    | 868 ->
      "<SYNTAX ERROR>\n"
    | 1866 ->
      "<SYNTAX ERROR>\n"
    | 860 ->
      "<SYNTAX ERROR>\n"
    | 861 ->
      "<SYNTAX ERROR>\n"
    | 862 ->
      "<SYNTAX ERROR>\n"
    | 863 ->
      "<SYNTAX ERROR>\n"
    | 1883 ->
      "<SYNTAX ERROR>\n"
    | 621 ->
      "<SYNTAX ERROR>\n"
    | 842 ->
      "<SYNTAX ERROR>\n"
    | 844 ->
      "<SYNTAX ERROR>\n"
    | 845 ->
      "<SYNTAX ERROR>\n"
    | 1876 ->
      "<SYNTAX ERROR>\n"
    | 1877 ->
      "<SYNTAX ERROR>\n"
    | 1878 ->
      "<SYNTAX ERROR>\n"
    | 1879 ->
      "<SYNTAX ERROR>\n"
    | 1880 ->
      "<SYNTAX ERROR>\n"
    | 1881 ->
      "<SYNTAX ERROR>\n"
    | 869 ->
      "<SYNTAX ERROR>\n"
    | 870 ->
      "<SYNTAX ERROR>\n"
    | 871 ->
      "<SYNTAX ERROR>\n"
    | 872 ->
      "<SYNTAX ERROR>\n"
    | 875 ->
      "<SYNTAX ERROR>\n"
    | 876 ->
      "<SYNTAX ERROR>\n"
    | 1035 ->
      "<SYNTAX ERROR>\n"
    | 1036 ->
      "<SYNTAX ERROR>\n"
    | 1037 ->
      "<SYNTAX ERROR>\n"
    | 1038 ->
      "<SYNTAX ERROR>\n"
    | 1039 ->
      "<SYNTAX ERROR>\n"
    | 1040 ->
      "<SYNTAX ERROR>\n"
    | 1044 ->
      "<SYNTAX ERROR>\n"
    | 1049 ->
      "<SYNTAX ERROR>\n"
    | 938 ->
      "<SYNTAX ERROR>\n"
    | 939 ->
      "<SYNTAX ERROR>\n"
    | 940 ->
      "<SYNTAX ERROR>\n"
    | 1050 ->
      "<SYNTAX ERROR>\n"
    | 1051 ->
      "<SYNTAX ERROR>\n"
    | 1052 ->
      "<SYNTAX ERROR>\n"
    | 942 ->
      "<SYNTAX ERROR>\n"
    | 930 ->
      "<SYNTAX ERROR>\n"
    | 1057 ->
      "<SYNTAX ERROR>\n"
    | 1146 ->
      "<SYNTAX ERROR>\n"
    | 1059 ->
      "<SYNTAX ERROR>\n"
    | 1060 ->
      "<SYNTAX ERROR>\n"
    | 1062 ->
      "<SYNTAX ERROR>\n"
    | 1065 ->
      "<SYNTAX ERROR>\n"
    | 1698 ->
      "<SYNTAX ERROR>\n"
    | 1139 ->
      "<SYNTAX ERROR>\n"
    | 1140 ->
      "<SYNTAX ERROR>\n"
    | 1148 ->
      "<SYNTAX ERROR>\n"
    | 1657 ->
      "<SYNTAX ERROR>\n"
    | 1658 ->
      "<SYNTAX ERROR>\n"
    | 1659 ->
      "<SYNTAX ERROR>\n"
    | 1660 ->
      "<SYNTAX ERROR>\n"
    | 1621 ->
      "<SYNTAX ERROR>\n"
    | 1622 ->
      "<SYNTAX ERROR>\n"
    | 1661 ->
      "<SYNTAX ERROR>\n"
    | 1662 ->
      "<SYNTAX ERROR>\n"
    | 1664 ->
      "<SYNTAX ERROR>\n"
    | 1626 ->
      "<SYNTAX ERROR>\n"
    | 1627 ->
      "<SYNTAX ERROR>\n"
    | 1668 ->
      "<SYNTAX ERROR>\n"
    | 1665 ->
      "<SYNTAX ERROR>\n"
    | 1666 ->
      "<SYNTAX ERROR>\n"
    | 1067 ->
      "<SYNTAX ERROR>\n"
    | 1075 ->
      "<SYNTAX ERROR>\n"
    | 1076 ->
      "<SYNTAX ERROR>\n"
    | 1077 ->
      "<SYNTAX ERROR>\n"
    | 1078 ->
      "<SYNTAX ERROR>\n"
    | 1079 ->
      "<SYNTAX ERROR>\n"
    | 1081 ->
      "<SYNTAX ERROR>\n"
    | 1082 ->
      "<SYNTAX ERROR>\n"
    | 1083 ->
      "<SYNTAX ERROR>\n"
    | 1084 ->
      "<SYNTAX ERROR>\n"
    | 1090 ->
      "<SYNTAX ERROR>\n"
    | 1091 ->
      "<SYNTAX ERROR>\n"
    | 1093 ->
      "<SYNTAX ERROR>\n"
    | 1094 ->
      "<SYNTAX ERROR>\n"
    | 1098 ->
      "<SYNTAX ERROR>\n"
    | 1096 ->
      "<SYNTAX ERROR>\n"
    | 1099 ->
      "<SYNTAX ERROR>\n"
    | 1100 ->
      "<SYNTAX ERROR>\n"
    | 1071 ->
      "<SYNTAX ERROR>\n"
    | 1681 ->
      "<SYNTAX ERROR>\n"
    | 1682 ->
      "<SYNTAX ERROR>\n"
    | 1685 ->
      "<SYNTAX ERROR>\n"
    | 1068 ->
      "<SYNTAX ERROR>\n"
    | 1069 ->
      "<SYNTAX ERROR>\n"
    | 1074 ->
      "<SYNTAX ERROR>\n"
    | 1649 ->
      "<SYNTAX ERROR>\n"
    | 1149 ->
      "<SYNTAX ERROR>\n"
    | 1150 ->
      "<SYNTAX ERROR>\n"
    | 1151 ->
      "<SYNTAX ERROR>\n"
    | 1152 ->
      "<SYNTAX ERROR>\n"
    | 1587 ->
      "<SYNTAX ERROR>\n"
    | 1590 ->
      "<SYNTAX ERROR>\n"
    | 1608 ->
      "<SYNTAX ERROR>\n"
    | 1609 ->
      "<SYNTAX ERROR>\n"
    | 1144 ->
      "<SYNTAX ERROR>\n"
    | 1145 ->
      "<SYNTAX ERROR>\n"
    | 1618 ->
      "<SYNTAX ERROR>\n"
    | 1594 ->
      "<SYNTAX ERROR>\n"
    | 1598 ->
      "<SYNTAX ERROR>\n"
    | 1600 ->
      "<SYNTAX ERROR>\n"
    | 1601 ->
      "<SYNTAX ERROR>\n"
    | 1611 ->
      "<SYNTAX ERROR>\n"
    | 1602 ->
      "<SYNTAX ERROR>\n"
    | 1603 ->
      "<SYNTAX ERROR>\n"
    | 1604 ->
      "<SYNTAX ERROR>\n"
    | 1619 ->
      "<SYNTAX ERROR>\n"
    | 1636 ->
      "<SYNTAX ERROR>\n"
    | 1620 ->
      "<SYNTAX ERROR>\n"
    | 1645 ->
      "<SYNTAX ERROR>\n"
    | 1628 ->
      "<SYNTAX ERROR>\n"
    | 1646 ->
      "<SYNTAX ERROR>\n"
    | 1647 ->
      "<SYNTAX ERROR>\n"
    | 1635 ->
      "<SYNTAX ERROR>\n"
    | 1632 ->
      "<SYNTAX ERROR>\n"
    | 1633 ->
      "<SYNTAX ERROR>\n"
    | 1634 ->
      "<SYNTAX ERROR>\n"
    | 1641 ->
      "<SYNTAX ERROR>\n"
    | 1642 ->
      "<SYNTAX ERROR>\n"
    | 1643 ->
      "<SYNTAX ERROR>\n"
    | 562 ->
      "<SYNTAX ERROR>\n"
    | 140 ->
      "<SYNTAX ERROR>\n"
    | 883 ->
      "<SYNTAX ERROR>\n"
    | 2598 ->
      "<SYNTAX ERROR>\n"
    | 924 ->
      "<SYNTAX ERROR>\n"
    | 919 ->
      "<SYNTAX ERROR>\n"
    | 2600 ->
      "<SYNTAX ERROR>\n"
    | 1735 ->
      "<SYNTAX ERROR>\n"
    | 925 ->
      "<SYNTAX ERROR>\n"
    | 935 ->
      "<SYNTAX ERROR>\n"
    | 927 ->
      "<SYNTAX ERROR>\n"
    | 928 ->
      "<SYNTAX ERROR>\n"
    | 2599 ->
      "<SYNTAX ERROR>\n"
    | 951 ->
      "<SYNTAX ERROR>\n"
    | 952 ->
      "<SYNTAX ERROR>\n"
    | 954 ->
      "<SYNTAX ERROR>\n"
    | 998 ->
      "<SYNTAX ERROR>\n"
    | 999 ->
      "<SYNTAX ERROR>\n"
    | 1000 ->
      "<SYNTAX ERROR>\n"
    | 1001 ->
      "<SYNTAX ERROR>\n"
    | 1002 ->
      "<SYNTAX ERROR>\n"
    | 1003 ->
      "<SYNTAX ERROR>\n"
    | 1004 ->
      "<SYNTAX ERROR>\n"
    | 1005 ->
      "<SYNTAX ERROR>\n"
    | 1011 ->
      "<SYNTAX ERROR>\n"
    | 1013 ->
      "<SYNTAX ERROR>\n"
    | 1006 ->
      "<SYNTAX ERROR>\n"
    | 1007 ->
      "<SYNTAX ERROR>\n"
    | 1018 ->
      "<SYNTAX ERROR>\n"
    | 1019 ->
      "<SYNTAX ERROR>\n"
    | 1020 ->
      "<SYNTAX ERROR>\n"
    | 1021 ->
      "<SYNTAX ERROR>\n"
    | 1736 ->
      "<SYNTAX ERROR>\n"
    | 1737 ->
      "<SYNTAX ERROR>\n"
    | 787 ->
      "<SYNTAX ERROR>\n"
    | 1024 ->
      "<SYNTAX ERROR>\n"
    | 1025 ->
      "<SYNTAX ERROR>\n"
    | 1707 ->
      "<SYNTAX ERROR>\n"
    | 959 ->
      "<SYNTAX ERROR>\n"
    | 960 ->
      "<SYNTAX ERROR>\n"
    | 962 ->
      "<SYNTAX ERROR>\n"
    | 963 ->
      "<SYNTAX ERROR>\n"
    | 969 ->
      "<SYNTAX ERROR>\n"
    | 967 ->
      "<SYNTAX ERROR>\n"
    | 965 ->
      "<SYNTAX ERROR>\n"
    | 982 ->
      "<SYNTAX ERROR>\n"
    | 983 ->
      "<SYNTAX ERROR>\n"
    | 975 ->
      "<SYNTAX ERROR>\n"
    | 976 ->
      "<SYNTAX ERROR>\n"
    | 980 ->
      "<SYNTAX ERROR>\n"
    | 981 ->
      "<SYNTAX ERROR>\n"
    | 979 ->
      "<SYNTAX ERROR>\n"
    | 977 ->
      "<SYNTAX ERROR>\n"
    | 978 ->
      "<SYNTAX ERROR>\n"
    | 106 ->
      "<SYNTAX ERROR>\n"
    | 263 ->
      "<SYNTAX ERROR>\n"
    | 986 ->
      "<SYNTAX ERROR>\n"
    | 987 ->
      "<SYNTAX ERROR>\n"
    | 264 ->
      "<SYNTAX ERROR>\n"
    | 265 ->
      "<SYNTAX ERROR>\n"
    | 406 ->
      "<SYNTAX ERROR>\n"
    | 407 ->
      "<SYNTAX ERROR>\n"
    | 408 ->
      "<SYNTAX ERROR>\n"
    | 2392 ->
      "<SYNTAX ERROR>\n"
    | 543 ->
      "<SYNTAX ERROR>\n"
    | 2307 ->
      "<SYNTAX ERROR>\n"
    | 2308 ->
      "<SYNTAX ERROR>\n"
    | 2309 ->
      "<SYNTAX ERROR>\n"
    | 2310 ->
      "<SYNTAX ERROR>\n"
    | 2311 ->
      "<SYNTAX ERROR>\n"
    | 2312 ->
      "<SYNTAX ERROR>\n"
    | 1917 ->
      "<SYNTAX ERROR>\n"
    | 1918 ->
      "<SYNTAX ERROR>\n"
    | 1920 ->
      "<SYNTAX ERROR>\n"
    | 1925 ->
      "<SYNTAX ERROR>\n"
    | 1923 ->
      "<SYNTAX ERROR>\n"
    | 1921 ->
      "<SYNTAX ERROR>\n"
    | 1939 ->
      "<SYNTAX ERROR>\n"
    | 1940 ->
      "<SYNTAX ERROR>\n"
    | 1926 ->
      "<SYNTAX ERROR>\n"
    | 1927 ->
      "<SYNTAX ERROR>\n"
    | 1937 ->
      "<SYNTAX ERROR>\n"
    | 1938 ->
      "<SYNTAX ERROR>\n"
    | 1936 ->
      "<SYNTAX ERROR>\n"
    | 1928 ->
      "<SYNTAX ERROR>\n"
    | 1930 ->
      "<SYNTAX ERROR>\n"
    | 1931 ->
      "<SYNTAX ERROR>\n"
    | 1932 ->
      "<SYNTAX ERROR>\n"
    | 1934 ->
      "<SYNTAX ERROR>\n"
    | 544 ->
      "<SYNTAX ERROR>\n"
    | 638 ->
      "<SYNTAX ERROR>\n"
    | 1944 ->
      "<SYNTAX ERROR>\n"
    | 1945 ->
      "<SYNTAX ERROR>\n"
    | 639 ->
      "<SYNTAX ERROR>\n"
    | 640 ->
      "<SYNTAX ERROR>\n"
    | 545 ->
      "<SYNTAX ERROR>\n"
    | 546 ->
      "<SYNTAX ERROR>\n"
    | 547 ->
      "<SYNTAX ERROR>\n"
    | 2303 ->
      "<SYNTAX ERROR>\n"
    | 1907 ->
      "<SYNTAX ERROR>\n"
    | 1954 ->
      "<SYNTAX ERROR>\n"
    | 1955 ->
      "<SYNTAX ERROR>\n"
    | 1956 ->
      "<SYNTAX ERROR>\n"
    | 1957 ->
      "<SYNTAX ERROR>\n"
    | 1958 ->
      "<SYNTAX ERROR>\n"
    | 1959 ->
      "<SYNTAX ERROR>\n"
    | 1915 ->
      "<SYNTAX ERROR>\n"
    | 2304 ->
      "<SYNTAX ERROR>\n"
    | 1908 ->
      "<SYNTAX ERROR>\n"
    | 917 ->
      "<SYNTAX ERROR>\n"
    | 1729 ->
      "<SYNTAX ERROR>\n"
    | 1728 ->
      "<SYNTAX ERROR>\n"
    | 1710 ->
      "<SYNTAX ERROR>\n"
    | 1711 ->
      "<SYNTAX ERROR>\n"
    | 1712 ->
      "<SYNTAX ERROR>\n"
    | 1713 ->
      "<SYNTAX ERROR>\n"
    | 1714 ->
      "<SYNTAX ERROR>\n"
    | 1717 ->
      "<SYNTAX ERROR>\n"
    | 937 ->
      "<SYNTAX ERROR>\n"
    | 1720 ->
      "<SYNTAX ERROR>\n"
    | 1721 ->
      "<SYNTAX ERROR>\n"
    | 1741 ->
      "<SYNTAX ERROR>\n"
    | 1723 ->
      "<SYNTAX ERROR>\n"
    | 1648 ->
      "<SYNTAX ERROR>\n"
    | 1724 ->
      "<SYNTAX ERROR>\n"
    | 1742 ->
      "<SYNTAX ERROR>\n"
    | 1743 ->
      "<SYNTAX ERROR>\n"
    | 2605 ->
      "<SYNTAX ERROR>\n"
    | 2607 ->
      "<SYNTAX ERROR>\n"
    | 275 ->
      "<SYNTAX ERROR>\n"
    | 279 ->
      "<SYNTAX ERROR>\n"
    | 280 ->
      "<SYNTAX ERROR>\n"
    | 156 ->
      "<SYNTAX ERROR>\n"
    | 157 ->
      "<SYNTAX ERROR>\n"
    | 158 ->
      "<SYNTAX ERROR>\n"
    | 160 ->
      "<SYNTAX ERROR>\n"
    | 162 ->
      "<SYNTAX ERROR>\n"
    | 98 ->
      "<SYNTAX ERROR>\n"
    | 9 ->
      "<SYNTAX ERROR>\n"
    | 10 ->
      "<SYNTAX ERROR>\n"
    | 2539 ->
      "<SYNTAX ERROR>\n"
    | 102 ->
      "<SYNTAX ERROR>\n"
    | 104 ->
      "<SYNTAX ERROR>\n"
    | 2537 ->
      "Expecting one of the following:\n  - \",\" to start the type in the tuple\n  - \")\" to finish the tuple type definition\n"
    | 273 ->
      "<SYNTAX ERROR>\n"
    | 105 ->
      "<SYNTAX ERROR>\n"
    | 110 ->
      "<SYNTAX ERROR>\n"
    | 111 ->
      "<SYNTAX ERROR>\n"
    | 116 ->
      "<SYNTAX ERROR>\n"
    | 117 ->
      "<SYNTAX ERROR>\n"
    | 2534 ->
      "<SYNTAX ERROR>\n"
    | 2535 ->
      "<SYNTAX ERROR>\n"
    | 109 ->
      "<SYNTAX ERROR>\n"
    | 107 ->
      "<SYNTAX ERROR>\n"
    | 154 ->
      "<SYNTAX ERROR>\n"
    | 100 ->
      "<SYNTAX ERROR>\n"
    | 119 ->
      "<SYNTAX ERROR>\n"
    | 2526 ->
      "<SYNTAX ERROR>\n"
    | 2527 ->
      "<SYNTAX ERROR>\n"
    | 2528 ->
      "<SYNTAX ERROR>\n"
    | 2530 ->
      "<SYNTAX ERROR>\n"
    | 123 ->
      "<SYNTAX ERROR>\n"
    | 128 ->
      "<SYNTAX ERROR>\n"
    | 129 ->
      "<SYNTAX ERROR>\n"
    | 290 ->
      "<SYNTAX ERROR>\n"
    | 130 ->
      "<SYNTAX ERROR>\n"
    | 487 ->
      "<SYNTAX ERROR>\n"
    | 486 ->
      "<SYNTAX ERROR>\n"
    | 489 ->
      "<SYNTAX ERROR>\n"
    | 291 ->
      "<SYNTAX ERROR>\n"
    | 126 ->
      "<SYNTAX ERROR>\n"
    | 132 ->
      "<SYNTAX ERROR>\n"
    | 2519 ->
      "<SYNTAX ERROR>\n"
    | 2521 ->
      "<SYNTAX ERROR>\n"
    | 2522 ->
      "<SYNTAX ERROR>\n"
    | 146 ->
      "<SYNTAX ERROR>\n"
    | 134 ->
      "<SYNTAX ERROR>\n"
    | 135 ->
      "<SYNTAX ERROR>\n"
    | 2517 ->
      "<SYNTAX ERROR>\n"
    | 137 ->
      "<SYNTAX ERROR>\n"
    | 138 ->
      "<SYNTAX ERROR>\n"
    | 2513 ->
      "<SYNTAX ERROR>\n"
    | 2514 ->
      "<SYNTAX ERROR>\n"
    | 2515 ->
      "<SYNTAX ERROR>\n"
    | 139 ->
      "<SYNTAX ERROR>\n"
    | 144 ->
      "<SYNTAX ERROR>\n"
    | 150 ->
      "<SYNTAX ERROR>\n"
    | 2511 ->
      "<SYNTAX ERROR>\n"
    | 2507 ->
      "<SYNTAX ERROR>\n"
    | 152 ->
      "<SYNTAX ERROR>\n"
    | 2609 ->
      "<SYNTAX ERROR>\n"
    | 2611 ->
      "<SYNTAX ERROR>\n"
    | 2613 ->
      "<SYNTAX ERROR>\n"
    | 2614 ->
      "<SYNTAX ERROR>\n"
    | 782 ->
      "<SYNTAX ERROR>\n"
    | 784 ->
      "<SYNTAX ERROR>\n"
    | 795 ->
      "<SYNTAX ERROR>\n"
    | 785 ->
      "<SYNTAX ERROR>\n"
    | 777 ->
      "<SYNTAX ERROR>\n"
    | 622 ->
      "<SYNTAX ERROR>\n"
    | 586 ->
      "<SYNTAX ERROR>\n"
    | 758 ->
      "<SYNTAX ERROR>\n"
    | 203 ->
      "<SYNTAX ERROR>\n"
    | 205 ->
      "<SYNTAX ERROR>\n"
    | 211 ->
      "<SYNTAX ERROR>\n"
    | 217 ->
      "<SYNTAX ERROR>\n"
    | 802 ->
      "<SYNTAX ERROR>\n"
    | 803 ->
      "<SYNTAX ERROR>\n"
    | 832 ->
      "<SYNTAX ERROR>\n"
    | 793 ->
      "<SYNTAX ERROR>\n"
    | 721 ->
      "<SYNTAX ERROR>\n"
    | 708 ->
      "<SYNTAX ERROR>\n"
    | 710 ->
      "<SYNTAX ERROR>\n"
    | 837 ->
      "<SYNTAX ERROR>\n"
    | 454 ->
      "<SYNTAX ERROR>\n"
    | 458 ->
      "<SYNTAX ERROR>\n"
    | 459 ->
      "<SYNTAX ERROR>\n"
    | 252 ->
      "<SYNTAX ERROR>\n"
    | 320 ->
      "<SYNTAX ERROR>\n"
    | 256 ->
      "<SYNTAX ERROR>\n"
    | 259 ->
      "<SYNTAX ERROR>\n"
    | 271 ->
      "<SYNTAX ERROR>\n"
    | 260 ->
      "<SYNTAX ERROR>\n"
    | 261 ->
      "<SYNTAX ERROR>\n"
    | 332 ->
      "<SYNTAX ERROR>\n"
    | 254 ->
      "<SYNTAX ERROR>\n"
    | 327 ->
      "<SYNTAX ERROR>\n"
    | 328 ->
      "<SYNTAX ERROR>\n"
    | 329 ->
      "<SYNTAX ERROR>\n"
    | 330 ->
      "<SYNTAX ERROR>\n"
    | 2403 ->
      "<SYNTAX ERROR>\n"
    | 287 ->
      "<SYNTAX ERROR>\n"
    | 288 ->
      "<SYNTAX ERROR>\n"
    | 294 ->
      "<SYNTAX ERROR>\n"
    | 296 ->
      "<SYNTAX ERROR>\n"
    | 298 ->
      "<SYNTAX ERROR>\n"
    | 299 ->
      "<SYNTAX ERROR>\n"
    | 295 ->
      "<SYNTAX ERROR>\n"
    | 304 ->
      "<SYNTAX ERROR>\n"
    | 307 ->
      "<SYNTAX ERROR>\n"
    | 306 ->
      "<SYNTAX ERROR>\n"
    | 309 ->
      "<SYNTAX ERROR>\n"
    | 315 ->
      "<SYNTAX ERROR>\n"
    | 316 ->
      "<SYNTAX ERROR>\n"
    | 317 ->
      "<SYNTAX ERROR>\n"
    | 310 ->
      "<SYNTAX ERROR>\n"
    | 311 ->
      "<SYNTAX ERROR>\n"
    | 313 ->
      "<SYNTAX ERROR>\n"
    | 728 ->
      "<SYNTAX ERROR>\n"
    | 711 ->
      "<SYNTAX ERROR>\n"
    | 713 ->
      "<SYNTAX ERROR>\n"
    | 698 ->
      "<SYNTAX ERROR>\n"
    | 667 ->
      "<SYNTAX ERROR>\n"
    | 688 ->
      "<SYNTAX ERROR>\n"
    | 679 ->
      "<SYNTAX ERROR>\n"
    | 219 ->
      "<SYNTAX ERROR>\n"
    | 222 ->
      "<SYNTAX ERROR>\n"
    | 223 ->
      "<SYNTAX ERROR>\n"
    | 229 ->
      "<SYNTAX ERROR>\n"
    | 237 ->
      "<SYNTAX ERROR>\n"
    | 238 ->
      "<SYNTAX ERROR>\n"
    | 240 ->
      "<SYNTAX ERROR>\n"
    | 247 ->
      "<SYNTAX ERROR>\n"
    | 248 ->
      "<SYNTAX ERROR>\n"
    | 249 ->
      "<SYNTAX ERROR>\n"
    | 250 ->
      "<SYNTAX ERROR>\n"
    | 2407 ->
      "<SYNTAX ERROR>\n"
    | 2408 ->
      "<SYNTAX ERROR>\n"
    | 241 ->
      "<SYNTAX ERROR>\n"
    | 246 ->
      "<SYNTAX ERROR>\n"
    | 624 ->
      "<SYNTAX ERROR>\n"
    | 630 ->
      "<SYNTAX ERROR>\n"
    | 719 ->
      "<SYNTAX ERROR>\n"
    | 808 ->
      "<SYNTAX ERROR>\n"
    | 631 ->
      "<SYNTAX ERROR>\n"
    | 632 ->
      "<SYNTAX ERROR>\n"
    | 634 ->
      "<SYNTAX ERROR>\n"
    | 824 ->
      "<SYNTAX ERROR>\n"
    | 825 ->
      "<SYNTAX ERROR>\n"
    | 826 ->
      "<SYNTAX ERROR>\n"
    | 827 ->
      "<SYNTAX ERROR>\n"
    | 828 ->
      "<SYNTAX ERROR>\n"
    | 829 ->
      "<SYNTAX ERROR>\n"
    | 646 ->
      "<SYNTAX ERROR>\n"
    | 821 ->
      "<SYNTAX ERROR>\n"
    | 649 ->
      "<SYNTAX ERROR>\n"
    | 816 ->
      "<SYNTAX ERROR>\n"
    | 650 ->
      "<SYNTAX ERROR>\n"
    | 655 ->
      "<SYNTAX ERROR>\n"
    | 666 ->
      "<SYNTAX ERROR>\n"
    | 674 ->
      "<SYNTAX ERROR>\n"
    | 682 ->
      "<SYNTAX ERROR>\n"
    | 2410 ->
      "<SYNTAX ERROR>\n"
    | 2411 ->
      "<SYNTAX ERROR>\n"
    | 2412 ->
      "<SYNTAX ERROR>\n"
    | 2413 ->
      "<SYNTAX ERROR>\n"
    | 2414 ->
      "<SYNTAX ERROR>\n"
    | 2415 ->
      "<SYNTAX ERROR>\n"
    | 722 ->
      "<SYNTAX ERROR>\n"
    | 733 ->
      "<SYNTAX ERROR>\n"
    | 736 ->
      "<SYNTAX ERROR>\n"
    | 726 ->
      "<SYNTAX ERROR>\n"
    | 727 ->
      "<SYNTAX ERROR>\n"
    | 647 ->
      "<SYNTAX ERROR>\n"
    | 648 ->
      "<SYNTAX ERROR>\n"
    | 737 ->
      "<SYNTAX ERROR>\n"
    | 740 ->
      "<SYNTAX ERROR>\n"
    | 748 ->
      "<SYNTAX ERROR>\n"
    | 744 ->
      "<SYNTAX ERROR>\n"
    | 747 ->
      "<SYNTAX ERROR>\n"
    | 745 ->
      "<SYNTAX ERROR>\n"
    | 746 ->
      "<SYNTAX ERROR>\n"
    | 749 ->
      "<SYNTAX ERROR>\n"
    | 652 ->
      "<SYNTAX ERROR>\n"
    | 653 ->
      "<SYNTAX ERROR>\n"
    | 664 ->
      "<SYNTAX ERROR>\n"
    | 659 ->
      "<SYNTAX ERROR>\n"
    | 660 ->
      "<SYNTAX ERROR>\n"
    | 750 ->
      "<SYNTAX ERROR>\n"
    | 665 ->
      "<SYNTAX ERROR>\n"
    | 814 ->
      "<SYNTAX ERROR>\n"
    | 753 ->
      "<SYNTAX ERROR>\n"
    | 769 ->
      "<SYNTAX ERROR>\n"
    | 771 ->
      "<SYNTAX ERROR>\n"
    | 2617 ->
      "<SYNTAX ERROR>\n"
    | 2631 ->
      "<SYNTAX ERROR>\n"
    | 2618 ->
      "<SYNTAX ERROR>\n"
    | 2619 ->
      "<SYNTAX ERROR>\n"
    | 2625 ->
      "<SYNTAX ERROR>\n"
    | 2626 ->
      "<SYNTAX ERROR>\n"
    | 2629 ->
      "<SYNTAX ERROR>\n"
    | 2635 ->
      "<SYNTAX ERROR>\n"
    | 2642 ->
      "<SYNTAX ERROR>\n"
    | 2641 ->
      "<SYNTAX ERROR>\n"
    | 2638 ->
      "<SYNTAX ERROR>\n"
    | 2639 ->
      "<SYNTAX ERROR>\n"
    | _ ->
      raise Not_found
