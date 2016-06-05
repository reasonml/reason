
(* This file was auto-generated based on "src/reason_parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "<SYNTAX ERROR>\n"
    | 2 ->
        "<SYNTAX ERROR>\n"
    | 2608 ->
        "<SYNTAX ERROR>\n"
    | 574 ->
        "Expecting one of the following:\n  - an identifier to access a member of an object\n  - \"[\" + expression + \"]\" to access an element of a list\n  - \"(\" + expression + \")\"\n  - \"{\" + expression + \"}\"\n"
    | 575 ->
        "Expecting an expression\n"
    | 2292 ->
        "Expecting one of the following:\n  - an infix operation to connect two expressions\n  - \")\" to close the block\n"
    | 2294 ->
        "Expecting an expression\n"
    | 2295 ->
        "Expecting one of the following:\n  - an infix operation to connect two expressions\n  - \"}\" to close the block\n"
    | 2297 ->
        "Expecting an expression\n"
    | 2298 ->
        "Expecting one of the following:\n  - an infix operation to connect two expressions\n  - \"}\" to close the block\n"
    | 1189 ->
        "Expecting an expression\n"
    | 572 ->
        "Expecting an identifier\n"
    | 1122 ->
        "Expecting a structure item\n"
    | 2613 ->
        "Invalid token\n"
    | 1243 ->
        "Expecting an expression\n"
    | 1244 ->
        "Expecting one of the following:\n  - The continuation of the previous expression\n  - \":\" to start the next expression\n"
    | 1245 ->
        "Expecting an expression\n"
    | 1204 ->
        "Expecting an expression\n"
    | 1210 ->
        "Expecting an expression\n"
    | 1212 ->
        "Expecting an expression\n"
    | 1206 ->
        "Expecting an expression\n"
    | 1214 ->
        "Expecting an expression\n"
    | 1216 ->
        "Expecting an expression\n"
    | 1218 ->
        "Expecting an expression\n"
    | 15 ->
        "Expecting one of the following:\n  - \")\" to form a unit value \"()\"\n  - \"module\" to start a module expression\n  - an expression\n  - an operator to denote the prefix form of an operator\n"
    | 1220 ->
        "Expecting an expression\n"
    | 1226 ->
        "Expecting an expression\n"
    | 1228 ->
        "Expecting an expression\n"
    | 2409 ->
        "Expecting \"]\"\n"
    | 403 ->
        "Expecting an attributed id\n"
    | 2516 ->
        "Expecting \"]\"\n"
    | 161 ->
        "Expecting an attribute id\n"
    | 1191 ->
        "Expecting an expression\n"
    | 1208 ->
        "Expecting an expression\n"
    | 1222 ->
        "Expecting an expression\n"
    | 1224 ->
        "Expecting an expression\n"
    | 1230 ->
        "Expecting an expression\n"
    | 1232 ->
        "Expecting an expression\n"
    | 854 ->
        "<SYNTAX ERROR>\n"
    | 855 ->
        "<SYNTAX ERROR>\n"
    | 2200 ->
        "<SYNTAX ERROR>\n"
    | 856 ->
        "<SYNTAX ERROR>\n"
    | 858 ->
        "<SYNTAX ERROR>\n"
    | 2196 ->
        "<SYNTAX ERROR>\n"
    | 2198 ->
        "<SYNTAX ERROR>\n"
    | 2203 ->
        "<SYNTAX ERROR>\n"
    | 2204 ->
        "<SYNTAX ERROR>\n"
    | 2208 ->
        "<SYNTAX ERROR>\n"
    | 2218 ->
        "<SYNTAX ERROR>\n"
    | 2223 ->
        "<SYNTAX ERROR>\n"
    | 1848 ->
        "<SYNTAX ERROR>\n"
    | 1240 ->
        "<SYNTAX ERROR>\n"
    | 1234 ->
        "<SYNTAX ERROR>\n"
    | 1236 ->
        "<SYNTAX ERROR>\n"
    | 1238 ->
        "<SYNTAX ERROR>\n"
    | 75 ->
        "<SYNTAX ERROR>\n"
    | 955 ->
        "<SYNTAX ERROR>\n"
    | 956 ->
        "<SYNTAX ERROR>\n"
    | 104 ->
        "Expecting one of the following:\n  - \"=\" to start the body of the type declaration\n  - \"constraint\" to add constraints to the type declaration\n  - \";\" to finish type declaratoin\n  - \"+=\" to form a string type extension\n  - \"and\" to declare another type\n"
    | 91 ->
        "<SYNTAX ERROR>\n"
    | 2593 ->
        "<SYNTAX ERROR>\n"
    | 2597 ->
        "<SYNTAX ERROR>\n"
    | 2594 ->
        "<SYNTAX ERROR>\n"
    | 2595 ->
        "<SYNTAX ERROR>\n"
    | 95 ->
        "<SYNTAX ERROR>\n"
    | 97 ->
        "<SYNTAX ERROR>\n"
    | 99 ->
        "<SYNTAX ERROR>\n"
    | 101 ->
        "<SYNTAX ERROR>\n"
    | 1132 ->
        "<SYNTAX ERROR>\n"
    | 105 ->
        "<SYNTAX ERROR>\n"
    | 2579 ->
        "<SYNTAX ERROR>\n"
    | 2580 ->
        "<SYNTAX ERROR>\n"
    | 2567 ->
        "<SYNTAX ERROR>\n"
    | 2582 ->
        "<SYNTAX ERROR>\n"
    | 2586 ->
        "<SYNTAX ERROR>\n"
    | 2587 ->
        "<SYNTAX ERROR>\n"
    | 2559 ->
        "<SYNTAX ERROR>\n"
    | 107 ->
        "<SYNTAX ERROR>\n"
    | 2557 ->
        "<SYNTAX ERROR>\n"
    | 2558 ->
        "<SYNTAX ERROR>\n"
    | 2575 ->
        "Expecting at least one type field definition in the form of:\n  <field name> : <type>\n"
    | 489 ->
        "Expecting a type field definition in the form of:\n  <field name> : <type>\n"
    | 490 ->
        "Expecting \":\"\n"
    | 491 ->
        "Expecting a type name describing this field\n"
    | 2576 ->
        "Expecting one of the following:\n  - \",\" to finish current type field\n  - \"}\" to finish type definition\n"
    | 505 ->
        "Expecting one of the following:\n  - another type field definition\n  - \"}\" to finish entire type definition\n"
    | 1137 ->
        "<SYNTAX ERROR>\n"
    | 2564 ->
        "<SYNTAX ERROR>\n"
    | 977 ->
        "<SYNTAX ERROR>\n"
    | 978 ->
        "<SYNTAX ERROR>\n"
    | 979 ->
        "<SYNTAX ERROR>\n"
    | 1133 ->
        "<SYNTAX ERROR>\n"
    | 1135 ->
        "<SYNTAX ERROR>\n"
    | 163 ->
        "<SYNTAX ERROR>\n"
    | 2511 ->
        "<SYNTAX ERROR>\n"
    | 2510 ->
        "<SYNTAX ERROR>\n"
    | 2513 ->
        "<SYNTAX ERROR>\n"
    | 2452 ->
        "<SYNTAX ERROR>\n"
    | 2453 ->
        "<SYNTAX ERROR>\n"
    | 2454 ->
        "Expecting a sequence item\n"
    | 1878 ->
        "Expecting one of the following:\n  - \"|\" to open the next pattern\n  - \"=>\" to start the body of the matched pattern\n  - \"when\" to start a contitional guard for the previous pattern\n"
    | 2485 ->
        "Expecting the body of the matched pattern\n"
    | 2514 ->
        "Expecting one of the following:\n  - \"}\" to finish the block\n  - \"|\" to start another pattern matching case\n"
    | 2468 ->
        "<SYNTAX ERROR>\n"
    | 2455 ->
        "<SYNTAX ERROR>\n"
    | 2456 ->
        "<SYNTAX ERROR>\n"
    | 2459 ->
        "<SYNTAX ERROR>\n"
    | 2460 ->
        "<SYNTAX ERROR>\n"
    | 2457 ->
        "<SYNTAX ERROR>\n"
    | 2478 ->
        "<SYNTAX ERROR>\n"
    | 2479 ->
        "<SYNTAX ERROR>\n"
    | 2482 ->
        "<SYNTAX ERROR>\n"
    | 2481 ->
        "<SYNTAX ERROR>\n"
    | 1877 ->
        "Expecting a match case\n"
    | 893 ->
        "<SYNTAX ERROR>\n"
    | 124 ->
        "<SYNTAX ERROR>\n"
    | 125 ->
        "<SYNTAX ERROR>\n"
    | 894 ->
        "<SYNTAX ERROR>\n"
    | 1852 ->
        "<SYNTAX ERROR>\n"
    | 1855 ->
        "<SYNTAX ERROR>\n"
    | 1869 ->
        "<SYNTAX ERROR>\n"
    | 1857 ->
        "<SYNTAX ERROR>\n"
    | 1858 ->
        "<SYNTAX ERROR>\n"
    | 1861 ->
        "<SYNTAX ERROR>\n"
    | 1863 ->
        "<SYNTAX ERROR>\n"
    | 1864 ->
        "<SYNTAX ERROR>\n"
    | 1866 ->
        "<SYNTAX ERROR>\n"
    | 168 ->
        "<SYNTAX ERROR>\n"
    | 2494 ->
        "<SYNTAX ERROR>\n"
    | 2495 ->
        "<SYNTAX ERROR>\n"
    | 1121 ->
        "Incomplete module item, forgetting a \";\"?\n"
    | 1176 ->
        "<SYNTAX ERROR>\n"
    | 1179 ->
        "<SYNTAX ERROR>\n"
    | 6 ->
        "<SYNTAX ERROR>\n"
    | 1201 ->
        "<SYNTAX ERROR>\n"
    | 395 ->
        "<SYNTAX ERROR>\n"
    | 396 ->
        "<SYNTAX ERROR>\n"
    | 344 ->
        "<SYNTAX ERROR>\n"
    | 399 ->
        "<SYNTAX ERROR>\n"
    | 401 ->
        "<SYNTAX ERROR>\n"
    | 7 ->
        "<SYNTAX ERROR>\n"
    | 405 ->
        "<SYNTAX ERROR>\n"
    | 406 ->
        "<SYNTAX ERROR>\n"
    | 408 ->
        "<SYNTAX ERROR>\n"
    | 891 ->
        "<SYNTAX ERROR>\n"
    | 541 ->
        "<SYNTAX ERROR>\n"
    | 16 ->
        "<SYNTAX ERROR>\n"
    | 2605 ->
        "<SYNTAX ERROR>\n"
    | 611 ->
        "<SYNTAX ERROR>\n"
    | 612 ->
        "<SYNTAX ERROR>\n"
    | 2247 ->
        "<SYNTAX ERROR>\n"
    | 2249 ->
        "<SYNTAX ERROR>\n"
    | 2250 ->
        "<SYNTAX ERROR>\n"
    | 2252 ->
        "<SYNTAX ERROR>\n"
    | 2253 ->
        "<SYNTAX ERROR>\n"
    | 1379 ->
        "<SYNTAX ERROR>\n"
    | 608 ->
        "<SYNTAX ERROR>\n"
    | 1437 ->
        "<SYNTAX ERROR>\n"
    | 1438 ->
        "<SYNTAX ERROR>\n"
    | 1439 ->
        "<SYNTAX ERROR>\n"
    | 1394 ->
        "<SYNTAX ERROR>\n"
    | 1400 ->
        "<SYNTAX ERROR>\n"
    | 1402 ->
        "<SYNTAX ERROR>\n"
    | 1396 ->
        "<SYNTAX ERROR>\n"
    | 1404 ->
        "<SYNTAX ERROR>\n"
    | 1406 ->
        "<SYNTAX ERROR>\n"
    | 1408 ->
        "<SYNTAX ERROR>\n"
    | 184 ->
        "<SYNTAX ERROR>\n"
    | 1410 ->
        "<SYNTAX ERROR>\n"
    | 1416 ->
        "<SYNTAX ERROR>\n"
    | 1418 ->
        "<SYNTAX ERROR>\n"
    | 2412 ->
        "<SYNTAX ERROR>\n"
    | 340 ->
        "<SYNTAX ERROR>\n"
    | 1381 ->
        "<SYNTAX ERROR>\n"
    | 1398 ->
        "<SYNTAX ERROR>\n"
    | 1412 ->
        "<SYNTAX ERROR>\n"
    | 1414 ->
        "<SYNTAX ERROR>\n"
    | 1420 ->
        "<SYNTAX ERROR>\n"
    | 1422 ->
        "<SYNTAX ERROR>\n"
    | 904 ->
        "<SYNTAX ERROR>\n"
    | 905 ->
        "<SYNTAX ERROR>\n"
    | 1792 ->
        "<SYNTAX ERROR>\n"
    | 906 ->
        "<SYNTAX ERROR>\n"
    | 907 ->
        "<SYNTAX ERROR>\n"
    | 1785 ->
        "<SYNTAX ERROR>\n"
    | 1787 ->
        "<SYNTAX ERROR>\n"
    | 1795 ->
        "<SYNTAX ERROR>\n"
    | 1797 ->
        "<SYNTAX ERROR>\n"
    | 1812 ->
        "<SYNTAX ERROR>\n"
    | 1824 ->
        "<SYNTAX ERROR>\n"
    | 1829 ->
        "<SYNTAX ERROR>\n"
    | 2345 ->
        "<SYNTAX ERROR>\n"
    | 2350 ->
        "<SYNTAX ERROR>\n"
    | 2347 ->
        "<SYNTAX ERROR>\n"
    | 1259 ->
        "<SYNTAX ERROR>\n"
    | 2344 ->
        "<SYNTAX ERROR>\n"
    | 1430 ->
        "<SYNTAX ERROR>\n"
    | 1457 ->
        "<SYNTAX ERROR>\n"
    | 1278 ->
        "<SYNTAX ERROR>\n"
    | 1424 ->
        "<SYNTAX ERROR>\n"
    | 1426 ->
        "<SYNTAX ERROR>\n"
    | 1428 ->
        "<SYNTAX ERROR>\n"
    | 166 ->
        "<SYNTAX ERROR>\n"
    | 2500 ->
        "<SYNTAX ERROR>\n"
    | 2499 ->
        "<SYNTAX ERROR>\n"
    | 2502 ->
        "<SYNTAX ERROR>\n"
    | 1369 ->
        "<SYNTAX ERROR>\n"
    | 1370 ->
        "<SYNTAX ERROR>\n"
    | 1432 ->
        "<SYNTAX ERROR>\n"
    | 1435 ->
        "<SYNTAX ERROR>\n"
    | 1453 ->
        "<SYNTAX ERROR>\n"
    | 1441 ->
        "<SYNTAX ERROR>\n"
    | 1442 ->
        "<SYNTAX ERROR>\n"
    | 1445 ->
        "<SYNTAX ERROR>\n"
    | 1447 ->
        "<SYNTAX ERROR>\n"
    | 1448 ->
        "<SYNTAX ERROR>\n"
    | 1450 ->
        "<SYNTAX ERROR>\n"
    | 173 ->
        "<SYNTAX ERROR>\n"
    | 2446 ->
        "<SYNTAX ERROR>\n"
    | 2447 ->
        "<SYNTAX ERROR>\n"
    | 1310 ->
        "<SYNTAX ERROR>\n"
    | 1318 ->
        "<SYNTAX ERROR>\n"
    | 794 ->
        "<SYNTAX ERROR>\n"
    | 187 ->
        "<SYNTAX ERROR>\n"
    | 416 ->
        "<SYNTAX ERROR>\n"
    | 417 ->
        "<SYNTAX ERROR>\n"
    | 177 ->
        "<SYNTAX ERROR>\n"
    | 179 ->
        "<SYNTAX ERROR>\n"
    | 180 ->
        "<SYNTAX ERROR>\n"
    | 542 ->
        "<SYNTAX ERROR>\n"
    | 2331 ->
        "<SYNTAX ERROR>\n"
    | 2333 ->
        "<SYNTAX ERROR>\n"
    | 2335 ->
        "<SYNTAX ERROR>\n"
    | 1789 ->
        "<SYNTAX ERROR>\n"
    | 1790 ->
        "<SYNTAX ERROR>\n"
    | 415 ->
        "Expecting one of the following:\n  - \")\" to form a unit value \"()\"\n  - \"module\" to start a module expression\n  - an expression\n  - an operator to denote the prefix form of an operator\n"
    | 2389 ->
        "<SYNTAX ERROR>\n"
    | 722 ->
        "<SYNTAX ERROR>\n"
    | 418 ->
        "Expecting a module expression\n"
    | 2376 ->
        "<SYNTAX ERROR>\n"
    | 2378 ->
        "<SYNTAX ERROR>\n"
    | 2380 ->
        "<SYNTAX ERROR>\n"
    | 2382 ->
        "<SYNTAX ERROR>\n"
    | 2383 ->
        "<SYNTAX ERROR>\n"
    | 2384 ->
        "<SYNTAX ERROR>\n"
    | 2385 ->
        "<SYNTAX ERROR>\n"
    | 2386 ->
        "<SYNTAX ERROR>\n"
    | 2387 ->
        "<SYNTAX ERROR>\n"
    | 1377 ->
        "<SYNTAX ERROR>\n"
    | 2434 ->
        "<SYNTAX ERROR>\n"
    | 189 ->
        "<SYNTAX ERROR>\n"
    | 557 ->
        "<SYNTAX ERROR>\n"
    | 2304 ->
        "<SYNTAX ERROR>\n"
    | 558 ->
        "<SYNTAX ERROR>\n"
    | 1819 ->
        "<SYNTAX ERROR>\n"
    | 1818 ->
        "<SYNTAX ERROR>\n"
    | 1813 ->
        "<SYNTAX ERROR>\n"
    | 1814 ->
        "<SYNTAX ERROR>\n"
    | 576 ->
        "<SYNTAX ERROR>\n"
    | 585 ->
        "<SYNTAX ERROR>\n"
    | 595 ->
        "<SYNTAX ERROR>\n"
    | 613 ->
        "<SYNTAX ERROR>\n"
    | 614 ->
        "<SYNTAX ERROR>\n"
    | 616 ->
        "<SYNTAX ERROR>\n"
    | 617 ->
        "<SYNTAX ERROR>\n"
    | 2244 ->
        "<SYNTAX ERROR>\n"
    | 2230 ->
        "<SYNTAX ERROR>\n"
    | 622 ->
        "<SYNTAX ERROR>\n"
    | 623 ->
        "<SYNTAX ERROR>\n"
    | 624 ->
        "<SYNTAX ERROR>\n"
    | 625 ->
        "<SYNTAX ERROR>\n"
    | 2228 ->
        "<SYNTAX ERROR>\n"
    | 2229 ->
        "<SYNTAX ERROR>\n"
    | 618 ->
        "<SYNTAX ERROR>\n"
    | 619 ->
        "<SYNTAX ERROR>\n"
    | 620 ->
        "<SYNTAX ERROR>\n"
    | 621 ->
        "<SYNTAX ERROR>\n"
    | 2237 ->
        "<SYNTAX ERROR>\n"
    | 2238 ->
        "<SYNTAX ERROR>\n"
    | 2239 ->
        "<SYNTAX ERROR>\n"
    | 2240 ->
        "<SYNTAX ERROR>\n"
    | 2241 ->
        "<SYNTAX ERROR>\n"
    | 2242 ->
        "<SYNTAX ERROR>\n"
    | 895 ->
        "<SYNTAX ERROR>\n"
    | 896 ->
        "<SYNTAX ERROR>\n"
    | 897 ->
        "<SYNTAX ERROR>\n"
    | 898 ->
        "<SYNTAX ERROR>\n"
    | 899 ->
        "<SYNTAX ERROR>\n"
    | 900 ->
        "<SYNTAX ERROR>\n"
    | 2337 ->
        "<SYNTAX ERROR>\n"
    | 2338 ->
        "<SYNTAX ERROR>\n"
    | 2339 ->
        "<SYNTAX ERROR>\n"
    | 2340 ->
        "<SYNTAX ERROR>\n"
    | 2341 ->
        "<SYNTAX ERROR>\n"
    | 2342 ->
        "<SYNTAX ERROR>\n"
    | 1791 ->
        "<SYNTAX ERROR>\n"
    | 603 ->
        "<SYNTAX ERROR>\n"
    | 1365 ->
        "<SYNTAX ERROR>\n"
    | 1187 ->
        "<SYNTAX ERROR>\n"
    | 909 ->
        "Incomplete let binding\n"
    | 1288 ->
        "<SYNTAX ERROR>\n"
    | 1294 ->
        "<SYNTAX ERROR>\n"
    | 1289 ->
        "<SYNTAX ERROR>\n"
    | 1290 ->
        "<SYNTAX ERROR>\n"
    | 1291 ->
        "<SYNTAX ERROR>\n"
    | 1293 ->
        "<SYNTAX ERROR>\n"
    | 1163 ->
        "<SYNTAX ERROR>\n"
    | 910 ->
        "<SYNTAX ERROR>\n"
    | 911 ->
        "<SYNTAX ERROR>\n"
    | 1767 ->
        "<SYNTAX ERROR>\n"
    | 1768 ->
        "<SYNTAX ERROR>\n"
    | 1769 ->
        "<SYNTAX ERROR>\n"
    | 1770 ->
        "<SYNTAX ERROR>\n"
    | 1774 ->
        "<SYNTAX ERROR>\n"
    | 1771 ->
        "<SYNTAX ERROR>\n"
    | 1772 ->
        "<SYNTAX ERROR>\n"
    | 1773 ->
        "<SYNTAX ERROR>\n"
    | 912 ->
        "<SYNTAX ERROR>\n"
    | 913 ->
        "<SYNTAX ERROR>\n"
    | 922 ->
        "<SYNTAX ERROR>\n"
    | 1760 ->
        "<SYNTAX ERROR>\n"
    | 1761 ->
        "<SYNTAX ERROR>\n"
    | 1762 ->
        "<SYNTAX ERROR>\n"
    | 1778 ->
        "<SYNTAX ERROR>\n"
    | 1141 ->
        "<SYNTAX ERROR>\n"
    | 1142 ->
        "<SYNTAX ERROR>\n"
    | 1164 ->
        "<SYNTAX ERROR>\n"
    | 1284 ->
        "Defining a function?\nExpecting one of the following:\n  - \"=>\" to start the function body\n  - an identifier to add a function parameter\n  - \":\" to specify the return type\n"
    | 1252 ->
        "<SYNTAX ERROR>\n"
    | 1169 ->
        "<SYNTAX ERROR>\n"
    | 1170 ->
        "<SYNTAX ERROR>\n"
    | 1171 ->
        "<SYNTAX ERROR>\n"
    | 1172 ->
        "<SYNTAX ERROR>\n"
    | 1173 ->
        "Expecting an expression as function body\n"
    | 1247 ->
        "<SYNTAX ERROR>\n"
    | 1248 ->
        "Defining a function?\nExpecting \"=>\" to start the function body\n"
    | 1249 ->
        "<SYNTAX ERROR>\n"
    | 1250 ->
        "<SYNTAX ERROR>\n"
    | 1165 ->
        "<SYNTAX ERROR>\n"
    | 1166 ->
        "<SYNTAX ERROR>\n"
    | 1167 ->
        "<SYNTAX ERROR>\n"
    | 1168 ->
        "<SYNTAX ERROR>\n"
    | 1257 ->
        "<SYNTAX ERROR>\n"
    | 1280 ->
        "<SYNTAX ERROR>\n"
    | 1281 ->
        "<SYNTAX ERROR>\n"
    | 1261 ->
        "<SYNTAX ERROR>\n"
    | 1262 ->
        "<SYNTAX ERROR>\n"
    | 1263 ->
        "<SYNTAX ERROR>\n"
    | 1266 ->
        "<SYNTAX ERROR>\n"
    | 1267 ->
        "<SYNTAX ERROR>\n"
    | 1268 ->
        "<SYNTAX ERROR>\n"
    | 1271 ->
        "<SYNTAX ERROR>\n"
    | 1272 ->
        "<SYNTAX ERROR>\n"
    | 1273 ->
        "<SYNTAX ERROR>\n"
    | 1274 ->
        "<SYNTAX ERROR>\n"
    | 1270 ->
        "<SYNTAX ERROR>\n"
    | 1510 ->
        "<SYNTAX ERROR>\n"
    | 69 ->
        "<SYNTAX ERROR>\n"
    | 1711 ->
        "<SYNTAX ERROR>\n"
    | 191 ->
        "<SYNTAX ERROR>\n"
    | 2432 ->
        "<SYNTAX ERROR>\n"
    | 2433 ->
        "<SYNTAX ERROR>\n"
    | 2431 ->
        "<SYNTAX ERROR>\n"
    | 70 ->
        "<SYNTAX ERROR>\n"
    | 1062 ->
        "<SYNTAX ERROR>\n"
    | 1035 ->
        "<SYNTAX ERROR>\n"
    | 2603 ->
        "<SYNTAX ERROR>\n"
    | 18 ->
        "<SYNTAX ERROR>\n"
    | 164 ->
        "<SYNTAX ERROR>\n"
    | 2506 ->
        "<SYNTAX ERROR>\n"
    | 1798 ->
        "<SYNTAX ERROR>\n"
    | 1801 ->
        "<SYNTAX ERROR>\n"
    | 1803 ->
        "<SYNTAX ERROR>\n"
    | 1806 ->
        "Expecting a type name\n"
    | 176 ->
        "Expecting an expression\n"
    | 1391 ->
        "Expecting an expression\n"
    | 1367 ->
        "Expecting an expression\n"
    | 602 ->
        "<SYNTAX ERROR>\n"
    | 1709 ->
        "Expecting \"]\" to finish current floating attribute\n"
    | 1037 ->
        "<SYNTAX ERROR>\n"
    | 167 ->
        "Expecting one of the following:\n  - an list item\n  - \"]\" to finish this list\n"
    | 2212 ->
        "Expecting one of the following:\n  - \",\" to separate two items in a list\n  - \"]\" to finish this list\n"
    | 2213 ->
        "<SYNTAX ERROR>\n"
    | 2209 ->
        "<SYNTAX ERROR>\n"
    | 2210 ->
        "<SYNTAX ERROR>\n"
    | 169 ->
        "<SYNTAX ERROR>\n"
    | 170 ->
        "<SYNTAX ERROR>\n"
    | 579 ->
        "<SYNTAX ERROR>\n"
    | 171 ->
        "<SYNTAX ERROR>\n"
    | 2488 ->
        "<SYNTAX ERROR>\n"
    | 174 ->
        "<SYNTAX ERROR>\n"
    | 1346 ->
        "<SYNTAX ERROR>\n"
    | 1347 ->
        "<SYNTAX ERROR>\n"
    | 1348 ->
        "<SYNTAX ERROR>\n"
    | 1349 ->
        "<SYNTAX ERROR>\n"
    | 1350 ->
        "<SYNTAX ERROR>\n"
    | 1351 ->
        "<SYNTAX ERROR>\n"
    | 1356 ->
        "<SYNTAX ERROR>\n"
    | 1357 ->
        "<SYNTAX ERROR>\n"
    | 1358 ->
        "<SYNTAX ERROR>\n"
    | 1359 ->
        "<SYNTAX ERROR>\n"
    | 1360 ->
        "<SYNTAX ERROR>\n"
    | 1363 ->
        "<SYNTAX ERROR>\n"
    | 1364 ->
        "<SYNTAX ERROR>\n"
    | 1456 ->
        "<SYNTAX ERROR>\n"
    | 1458 ->
        "<SYNTAX ERROR>\n"
    | 1459 ->
        "<SYNTAX ERROR>\n"
    | 1460 ->
        "<SYNTAX ERROR>\n"
    | 1355 ->
        "<SYNTAX ERROR>\n"
    | 2357 ->
        "<SYNTAX ERROR>\n"
    | 539 ->
        "<SYNTAX ERROR>\n"
    | 2287 ->
        "<SYNTAX ERROR>\n"
    | 2264 ->
        "<SYNTAX ERROR>\n"
    | 1461 ->
        "<SYNTAX ERROR>\n"
    | 1463 ->
        "<SYNTAX ERROR>\n"
    | 1464 ->
        "<SYNTAX ERROR>\n"
    | 1465 ->
        "<SYNTAX ERROR>\n"
    | 1466 ->
        "<SYNTAX ERROR>\n"
    | 1108 ->
        "<SYNTAX ERROR>\n"
    | 1110 ->
        "<SYNTAX ERROR>\n"
    | 1111 ->
        "<SYNTAX ERROR>\n"
    | 1468 ->
        "<SYNTAX ERROR>\n"
    | 1469 ->
        "<SYNTAX ERROR>\n"
    | 1470 ->
        "<SYNTAX ERROR>\n"
    | 1471 ->
        "<SYNTAX ERROR>\n"
    | 1474 ->
        "<SYNTAX ERROR>\n"
    | 1485 ->
        "<SYNTAX ERROR>\n"
    | 1475 ->
        "<SYNTAX ERROR>\n"
    | 1476 ->
        "<SYNTAX ERROR>\n"
    | 1477 ->
        "<SYNTAX ERROR>\n"
    | 1478 ->
        "<SYNTAX ERROR>\n"
    | 1479 ->
        "<SYNTAX ERROR>\n"
    | 1545 ->
        "<SYNTAX ERROR>\n"
    | 1489 ->
        "<SYNTAX ERROR>\n"
    | 1490 ->
        "<SYNTAX ERROR>\n"
    | 1491 ->
        "<SYNTAX ERROR>\n"
    | 1498 ->
        "<SYNTAX ERROR>\n"
    | 1499 ->
        "<SYNTAX ERROR>\n"
    | 1500 ->
        "<SYNTAX ERROR>\n"
    | 1492 ->
        "<SYNTAX ERROR>\n"
    | 1494 ->
        "<SYNTAX ERROR>\n"
    | 1495 ->
        "<SYNTAX ERROR>\n"
    | 1496 ->
        "<SYNTAX ERROR>\n"
    | 1497 ->
        "<SYNTAX ERROR>\n"
    | 1462 ->
        "<SYNTAX ERROR>\n"
    | 1846 ->
        "<SYNTAX ERROR>\n"
    | 1837 ->
        "<SYNTAX ERROR>\n"
    | 1835 ->
        "<SYNTAX ERROR>\n"
    | 1838 ->
        "<SYNTAX ERROR>\n"
    | 1839 ->
        "<SYNTAX ERROR>\n"
    | 1849 ->
        "<SYNTAX ERROR>\n"
    | 1850 ->
        "<SYNTAX ERROR>\n"
    | 587 ->
        "<SYNTAX ERROR>\n"
    | 2069 ->
        "<SYNTAX ERROR>\n"
    | 2075 ->
        "<SYNTAX ERROR>\n"
    | 2070 ->
        "<SYNTAX ERROR>\n"
    | 2071 ->
        "<SYNTAX ERROR>\n"
    | 2072 ->
        "<SYNTAX ERROR>\n"
    | 2074 ->
        "<SYNTAX ERROR>\n"
    | 2039 ->
        "<SYNTAX ERROR>\n"
    | 589 ->
        "<SYNTAX ERROR>\n"
    | 593 ->
        "<SYNTAX ERROR>\n"
    | 594 ->
        "<SYNTAX ERROR>\n"
    | 590 ->
        "<SYNTAX ERROR>\n"
    | 2274 ->
        "<SYNTAX ERROR>\n"
    | 2275 ->
        "<SYNTAX ERROR>\n"
    | 2278 ->
        "<SYNTAX ERROR>\n"
    | 2277 ->
        "<SYNTAX ERROR>\n"
    | 2040 ->
        "<SYNTAX ERROR>\n"
    | 2065 ->
        "<SYNTAX ERROR>\n"
    | 1481 ->
        "<SYNTAX ERROR>\n"
    | 1482 ->
        "<SYNTAX ERROR>\n"
    | 1483 ->
        "<SYNTAX ERROR>\n"
    | 1484 ->
        "<SYNTAX ERROR>\n"
    | 2041 ->
        "<SYNTAX ERROR>\n"
    | 2042 ->
        "<SYNTAX ERROR>\n"
    | 2043 ->
        "<SYNTAX ERROR>\n"
    | 2044 ->
        "<SYNTAX ERROR>\n"
    | 2046 ->
        "<SYNTAX ERROR>\n"
    | 2061 ->
        "<SYNTAX ERROR>\n"
    | 2062 ->
        "<SYNTAX ERROR>\n"
    | 2048 ->
        "<SYNTAX ERROR>\n"
    | 2049 ->
        "<SYNTAX ERROR>\n"
    | 2051 ->
        "<SYNTAX ERROR>\n"
    | 2052 ->
        "<SYNTAX ERROR>\n"
    | 2053 ->
        "<SYNTAX ERROR>\n"
    | 2056 ->
        "<SYNTAX ERROR>\n"
    | 2057 ->
        "<SYNTAX ERROR>\n"
    | 2058 ->
        "<SYNTAX ERROR>\n"
    | 2059 ->
        "<SYNTAX ERROR>\n"
    | 2055 ->
        "<SYNTAX ERROR>\n"
    | 2437 ->
        "Expecting \"}\" to finish the block\n"
    | 2171 ->
        "<SYNTAX ERROR>\n"
    | 1685 ->
        "<SYNTAX ERROR>\n"
    | 1118 ->
        "<SYNTAX ERROR>\n"
    | 1507 ->
        "<SYNTAX ERROR>\n"
    | 1504 ->
        "<SYNTAX ERROR>\n"
    | 1523 ->
        "<SYNTAX ERROR>\n"
    | 1524 ->
        "<SYNTAX ERROR>\n"
    | 1527 ->
        "<SYNTAX ERROR>\n"
    | 1160 ->
        "<SYNTAX ERROR>\n"
    | 1556 ->
        "<SYNTAX ERROR>\n"
    | 1559 ->
        "<SYNTAX ERROR>\n"
    | 1581 ->
        "<SYNTAX ERROR>\n"
    | 1560 ->
        "<SYNTAX ERROR>\n"
    | 1566 ->
        "<SYNTAX ERROR>\n"
    | 1567 ->
        "<SYNTAX ERROR>\n"
    | 1571 ->
        "<SYNTAX ERROR>\n"
    | 1572 ->
        "<SYNTAX ERROR>\n"
    | 1573 ->
        "<SYNTAX ERROR>\n"
    | 1562 ->
        "<SYNTAX ERROR>\n"
    | 1580 ->
        "<SYNTAX ERROR>\n"
    | 1577 ->
        "<SYNTAX ERROR>\n"
    | 1578 ->
        "<SYNTAX ERROR>\n"
    | 1579 ->
        "<SYNTAX ERROR>\n"
    | 1586 ->
        "<SYNTAX ERROR>\n"
    | 1587 ->
        "<SYNTAX ERROR>\n"
    | 1588 ->
        "<SYNTAX ERROR>\n"
    | 1308 ->
        "<SYNTAX ERROR>\n"
    | 1320 ->
        "<SYNTAX ERROR>\n"
    | 1546 ->
        "<SYNTAX ERROR>\n"
    | 1529 ->
        "<SYNTAX ERROR>\n"
    | 1530 ->
        "<SYNTAX ERROR>\n"
    | 1161 ->
        "<SYNTAX ERROR>\n"
    | 1342 ->
        "<SYNTAX ERROR>\n"
    | 1550 ->
        "<SYNTAX ERROR>\n"
    | 1162 ->
        "<SYNTAX ERROR>\n"
    | 1338 ->
        "<SYNTAX ERROR>\n"
    | 1339 ->
        "<SYNTAX ERROR>\n"
    | 1298 ->
        "<SYNTAX ERROR>\n"
    | 1300 ->
        "<SYNTAX ERROR>\n"
    | 1301 ->
        "<SYNTAX ERROR>\n"
    | 1326 ->
        "<SYNTAX ERROR>\n"
    | 1302 ->
        "<SYNTAX ERROR>\n"
    | 1303 ->
        "<SYNTAX ERROR>\n"
    | 1304 ->
        "<SYNTAX ERROR>\n"
    | 1528 ->
        "<SYNTAX ERROR>\n"
    | 1830 ->
        "<SYNTAX ERROR>\n"
    | 1831 ->
        "<SYNTAX ERROR>\n"
    | 1832 ->
        "<SYNTAX ERROR>\n"
    | 1534 ->
        "<SYNTAX ERROR>\n"
    | 1535 ->
        "<SYNTAX ERROR>\n"
    | 1536 ->
        "<SYNTAX ERROR>\n"
    | 1333 ->
        "<SYNTAX ERROR>\n"
    | 1334 ->
        "<SYNTAX ERROR>\n"
    | 1345 ->
        "<SYNTAX ERROR>\n"
    | 561 ->
        "<SYNTAX ERROR>\n"
    | 1039 ->
        "<SYNTAX ERROR>\n"
    | 915 ->
        "<SYNTAX ERROR>\n"
    | 859 ->
        "<SYNTAX ERROR>\n"
    | 860 ->
        "<SYNTAX ERROR>\n"
    | 1892 ->
        "<SYNTAX ERROR>\n"
    | 1894 ->
        "<SYNTAX ERROR>\n"
    | 1897 ->
        "<SYNTAX ERROR>\n"
    | 2189 ->
        "<SYNTAX ERROR>\n"
    | 908 ->
        "<SYNTAX ERROR>\n"
    | 1783 ->
        "<SYNTAX ERROR>\n"
    | 411 ->
        "<SYNTAX ERROR>\n"
    | 412 ->
        "<SYNTAX ERROR>\n"
    | 2397 ->
        "<SYNTAX ERROR>\n"
    | 2399 ->
        "<SYNTAX ERROR>\n"
    | 1895 ->
        "<SYNTAX ERROR>\n"
    | 2401 ->
        "<SYNTAX ERROR>\n"
    | 1900 ->
        "<SYNTAX ERROR>\n"
    | 1901 ->
        "<SYNTAX ERROR>\n"
    | 2403 ->
        "<SYNTAX ERROR>\n"
    | 2010 ->
        "<SYNTAX ERROR>\n"
    | 1985 ->
        "<SYNTAX ERROR>\n"
    | 1986 ->
        "<SYNTAX ERROR>\n"
    | 1987 ->
        "<SYNTAX ERROR>\n"
    | 1989 ->
        "<SYNTAX ERROR>\n"
    | 1992 ->
        "<SYNTAX ERROR>\n"
    | 1999 ->
        "<SYNTAX ERROR>\n"
    | 2002 ->
        "<SYNTAX ERROR>\n"
    | 2003 ->
        "<SYNTAX ERROR>\n"
    | 2192 ->
        "<SYNTAX ERROR>\n"
    | 2193 ->
        "<SYNTAX ERROR>\n"
    | 554 ->
        "<SYNTAX ERROR>\n"
    | 555 ->
        "<SYNTAX ERROR>\n"
    | 2308 ->
        "<SYNTAX ERROR>\n"
    | 2310 ->
        "<SYNTAX ERROR>\n"
    | 1990 ->
        "<SYNTAX ERROR>\n"
    | 2312 ->
        "<SYNTAX ERROR>\n"
    | 1995 ->
        "<SYNTAX ERROR>\n"
    | 1996 ->
        "<SYNTAX ERROR>\n"
    | 2314 ->
        "<SYNTAX ERROR>\n"
    | 2005 ->
        "<SYNTAX ERROR>\n"
    | 2006 ->
        "<SYNTAX ERROR>\n"
    | 1904 ->
        "<SYNTAX ERROR>\n"
    | 1980 ->
        "<SYNTAX ERROR>\n"
    | 1981 ->
        "<SYNTAX ERROR>\n"
    | 1982 ->
        "<SYNTAX ERROR>\n"
    | 1984 ->
        "<SYNTAX ERROR>\n"
    | 419 ->
        "<SYNTAX ERROR>\n"
    | 2141 ->
        "<SYNTAX ERROR>\n"
    | 422 ->
        "<SYNTAX ERROR>\n"
    | 436 ->
        "<SYNTAX ERROR>\n"
    | 425 ->
        "<SYNTAX ERROR>\n"
    | 2361 ->
        "<SYNTAX ERROR>\n"
    | 2365 ->
        "<SYNTAX ERROR>\n"
    | 2362 ->
        "<SYNTAX ERROR>\n"
    | 2363 ->
        "<SYNTAX ERROR>\n"
    | 427 ->
        "<SYNTAX ERROR>\n"
    | 429 ->
        "<SYNTAX ERROR>\n"
    | 431 ->
        "<SYNTAX ERROR>\n"
    | 433 ->
        "<SYNTAX ERROR>\n"
    | 2148 ->
        "<SYNTAX ERROR>\n"
    | 437 ->
        "<SYNTAX ERROR>\n"
    | 510 ->
        "<SYNTAX ERROR>\n"
    | 511 ->
        "<SYNTAX ERROR>\n"
    | 513 ->
        "<SYNTAX ERROR>\n"
    | 517 ->
        "<SYNTAX ERROR>\n"
    | 518 ->
        "<SYNTAX ERROR>\n"
    | 438 ->
        "<SYNTAX ERROR>\n"
    | 479 ->
        "<SYNTAX ERROR>\n"
    | 468 ->
        "<SYNTAX ERROR>\n"
    | 463 ->
        "<SYNTAX ERROR>\n"
    | 464 ->
        "<SYNTAX ERROR>\n"
    | 487 ->
        "<SYNTAX ERROR>\n"
    | 504 ->
        "<SYNTAX ERROR>\n"
    | 524 ->
        "<SYNTAX ERROR>\n"
    | 525 ->
        "<SYNTAX ERROR>\n"
    | 526 ->
        "<SYNTAX ERROR>\n"
    | 527 ->
        "<SYNTAX ERROR>\n"
    | 2149 ->
        "<SYNTAX ERROR>\n"
    | 2151 ->
        "<SYNTAX ERROR>\n"
    | 2140 ->
        "<SYNTAX ERROR>\n"
    | 1905 ->
        "<SYNTAX ERROR>\n"
    | 1906 ->
        "<SYNTAX ERROR>\n"
    | 1909 ->
        "<SYNTAX ERROR>\n"
    | 1910 ->
        "<SYNTAX ERROR>\n"
    | 1912 ->
        "<SYNTAX ERROR>\n"
    | 1976 ->
        "<SYNTAX ERROR>\n"
    | 1977 ->
        "<SYNTAX ERROR>\n"
    | 1978 ->
        "<SYNTAX ERROR>\n"
    | 2026 ->
        "<SYNTAX ERROR>\n"
    | 2027 ->
        "<SYNTAX ERROR>\n"
    | 2028 ->
        "<SYNTAX ERROR>\n"
    | 2029 ->
        "<SYNTAX ERROR>\n"
    | 2030 ->
        "<SYNTAX ERROR>\n"
    | 2031 ->
        "<SYNTAX ERROR>\n"
    | 2032 ->
        "<SYNTAX ERROR>\n"
    | 1979 ->
        "<SYNTAX ERROR>\n"
    | 2017 ->
        "<SYNTAX ERROR>\n"
    | 2018 ->
        "<SYNTAX ERROR>\n"
    | 2019 ->
        "<SYNTAX ERROR>\n"
    | 2020 ->
        "<SYNTAX ERROR>\n"
    | 2021 ->
        "<SYNTAX ERROR>\n"
    | 2034 ->
        "<SYNTAX ERROR>\n"
    | 2155 ->
        "<SYNTAX ERROR>\n"
    | 2156 ->
        "<SYNTAX ERROR>\n"
    | 2079 ->
        "<SYNTAX ERROR>\n"
    | 2082 ->
        "<SYNTAX ERROR>\n"
    | 2083 ->
        "<SYNTAX ERROR>\n"
    | 2084 ->
        "<SYNTAX ERROR>\n"
    | 2085 ->
        "<SYNTAX ERROR>\n"
    | 2086 ->
        "<SYNTAX ERROR>\n"
    | 2087 ->
        "<SYNTAX ERROR>\n"
    | 2091 ->
        "<SYNTAX ERROR>\n"
    | 2096 ->
        "<SYNTAX ERROR>\n"
    | 455 ->
        "<SYNTAX ERROR>\n"
    | 456 ->
        "<SYNTAX ERROR>\n"
    | 2097 ->
        "<SYNTAX ERROR>\n"
    | 2098 ->
        "<SYNTAX ERROR>\n"
    | 2099 ->
        "<SYNTAX ERROR>\n"
    | 441 ->
        "<SYNTAX ERROR>\n"
    | 470 ->
        "<SYNTAX ERROR>\n"
    | 2104 ->
        "<SYNTAX ERROR>\n"
    | 2113 ->
        "<SYNTAX ERROR>\n"
    | 2105 ->
        "<SYNTAX ERROR>\n"
    | 2106 ->
        "<SYNTAX ERROR>\n"
    | 2108 ->
        "<SYNTAX ERROR>\n"
    | 2109 ->
        "<SYNTAX ERROR>\n"
    | 2110 ->
        "<SYNTAX ERROR>\n"
    | 2160 ->
        "<SYNTAX ERROR>\n"
    | 2161 ->
        "<SYNTAX ERROR>\n"
    | 2115 ->
        "<SYNTAX ERROR>\n"
    | 2124 ->
        "<SYNTAX ERROR>\n"
    | 2125 ->
        "<SYNTAX ERROR>\n"
    | 2126 ->
        "<SYNTAX ERROR>\n"
    | 2127 ->
        "<SYNTAX ERROR>\n"
    | 2128 ->
        "<SYNTAX ERROR>\n"
    | 2129 ->
        "<SYNTAX ERROR>\n"
    | 2130 ->
        "<SYNTAX ERROR>\n"
    | 2116 ->
        "<SYNTAX ERROR>\n"
    | 2117 ->
        "<SYNTAX ERROR>\n"
    | 2165 ->
        "<SYNTAX ERROR>\n"
    | 2166 ->
        "<SYNTAX ERROR>\n"
    | 2118 ->
        "<SYNTAX ERROR>\n"
    | 2119 ->
        "<SYNTAX ERROR>\n"
    | 2120 ->
        "<SYNTAX ERROR>\n"
    | 2121 ->
        "<SYNTAX ERROR>\n"
    | 543 ->
        "<SYNTAX ERROR>\n"
    | 544 ->
        "<SYNTAX ERROR>\n"
    | 548 ->
        "<SYNTAX ERROR>\n"
    | 549 ->
        "<SYNTAX ERROR>\n"
    | 2326 ->
        "<SYNTAX ERROR>\n"
    | 2328 ->
        "<SYNTAX ERROR>\n"
    | 2329 ->
        "<SYNTAX ERROR>\n"
    | 2330 ->
        "<SYNTAX ERROR>\n"
    | 861 ->
        "<SYNTAX ERROR>\n"
    | 862 ->
        "<SYNTAX ERROR>\n"
    | 864 ->
        "<SYNTAX ERROR>\n"
    | 865 ->
        "<SYNTAX ERROR>\n"
    | 1873 ->
        "<SYNTAX ERROR>\n"
    | 870 ->
        "<SYNTAX ERROR>\n"
    | 871 ->
        "<SYNTAX ERROR>\n"
    | 872 ->
        "<SYNTAX ERROR>\n"
    | 873 ->
        "<SYNTAX ERROR>\n"
    | 874 ->
        "<SYNTAX ERROR>\n"
    | 1872 ->
        "<SYNTAX ERROR>\n"
    | 866 ->
        "<SYNTAX ERROR>\n"
    | 867 ->
        "<SYNTAX ERROR>\n"
    | 868 ->
        "<SYNTAX ERROR>\n"
    | 869 ->
        "<SYNTAX ERROR>\n"
    | 1889 ->
        "<SYNTAX ERROR>\n"
    | 627 ->
        "<SYNTAX ERROR>\n"
    | 848 ->
        "<SYNTAX ERROR>\n"
    | 850 ->
        "<SYNTAX ERROR>\n"
    | 851 ->
        "<SYNTAX ERROR>\n"
    | 1882 ->
        "<SYNTAX ERROR>\n"
    | 1883 ->
        "<SYNTAX ERROR>\n"
    | 1884 ->
        "<SYNTAX ERROR>\n"
    | 1885 ->
        "<SYNTAX ERROR>\n"
    | 1886 ->
        "<SYNTAX ERROR>\n"
    | 1887 ->
        "<SYNTAX ERROR>\n"
    | 875 ->
        "<SYNTAX ERROR>\n"
    | 876 ->
        "<SYNTAX ERROR>\n"
    | 877 ->
        "<SYNTAX ERROR>\n"
    | 878 ->
        "<SYNTAX ERROR>\n"
    | 881 ->
        "<SYNTAX ERROR>\n"
    | 882 ->
        "<SYNTAX ERROR>\n"
    | 1042 ->
        "<SYNTAX ERROR>\n"
    | 1043 ->
        "<SYNTAX ERROR>\n"
    | 1044 ->
        "<SYNTAX ERROR>\n"
    | 1045 ->
        "<SYNTAX ERROR>\n"
    | 1046 ->
        "<SYNTAX ERROR>\n"
    | 1047 ->
        "<SYNTAX ERROR>\n"
    | 1051 ->
        "<SYNTAX ERROR>\n"
    | 1056 ->
        "<SYNTAX ERROR>\n"
    | 948 ->
        "<SYNTAX ERROR>\n"
    | 949 ->
        "<SYNTAX ERROR>\n"
    | 1057 ->
        "<SYNTAX ERROR>\n"
    | 1058 ->
        "<SYNTAX ERROR>\n"
    | 1059 ->
        "<SYNTAX ERROR>\n"
    | 946 ->
        "<SYNTAX ERROR>\n"
    | 937 ->
        "<SYNTAX ERROR>\n"
    | 1064 ->
        "<SYNTAX ERROR>\n"
    | 1153 ->
        "<SYNTAX ERROR>\n"
    | 1066 ->
        "<SYNTAX ERROR>\n"
    | 1067 ->
        "<SYNTAX ERROR>\n"
    | 1069 ->
        "<SYNTAX ERROR>\n"
    | 1072 ->
        "<SYNTAX ERROR>\n"
    | 1704 ->
        "<SYNTAX ERROR>\n"
    | 1146 ->
        "<SYNTAX ERROR>\n"
    | 1147 ->
        "<SYNTAX ERROR>\n"
    | 1155 ->
        "<SYNTAX ERROR>\n"
    | 1663 ->
        "<SYNTAX ERROR>\n"
    | 1664 ->
        "<SYNTAX ERROR>\n"
    | 1665 ->
        "<SYNTAX ERROR>\n"
    | 1666 ->
        "<SYNTAX ERROR>\n"
    | 1627 ->
        "<SYNTAX ERROR>\n"
    | 1628 ->
        "<SYNTAX ERROR>\n"
    | 1667 ->
        "<SYNTAX ERROR>\n"
    | 1668 ->
        "<SYNTAX ERROR>\n"
    | 1670 ->
        "<SYNTAX ERROR>\n"
    | 1632 ->
        "<SYNTAX ERROR>\n"
    | 1633 ->
        "<SYNTAX ERROR>\n"
    | 1674 ->
        "<SYNTAX ERROR>\n"
    | 1671 ->
        "<SYNTAX ERROR>\n"
    | 1672 ->
        "<SYNTAX ERROR>\n"
    | 1074 ->
        "<SYNTAX ERROR>\n"
    | 1082 ->
        "<SYNTAX ERROR>\n"
    | 1083 ->
        "<SYNTAX ERROR>\n"
    | 1084 ->
        "<SYNTAX ERROR>\n"
    | 1085 ->
        "<SYNTAX ERROR>\n"
    | 1086 ->
        "<SYNTAX ERROR>\n"
    | 1088 ->
        "<SYNTAX ERROR>\n"
    | 1089 ->
        "<SYNTAX ERROR>\n"
    | 1090 ->
        "<SYNTAX ERROR>\n"
    | 1091 ->
        "<SYNTAX ERROR>\n"
    | 1097 ->
        "<SYNTAX ERROR>\n"
    | 1098 ->
        "<SYNTAX ERROR>\n"
    | 1100 ->
        "<SYNTAX ERROR>\n"
    | 1101 ->
        "<SYNTAX ERROR>\n"
    | 1105 ->
        "<SYNTAX ERROR>\n"
    | 1103 ->
        "<SYNTAX ERROR>\n"
    | 1106 ->
        "<SYNTAX ERROR>\n"
    | 1107 ->
        "<SYNTAX ERROR>\n"
    | 1078 ->
        "<SYNTAX ERROR>\n"
    | 1687 ->
        "<SYNTAX ERROR>\n"
    | 1688 ->
        "<SYNTAX ERROR>\n"
    | 1691 ->
        "<SYNTAX ERROR>\n"
    | 1075 ->
        "<SYNTAX ERROR>\n"
    | 1076 ->
        "<SYNTAX ERROR>\n"
    | 1081 ->
        "<SYNTAX ERROR>\n"
    | 1655 ->
        "<SYNTAX ERROR>\n"
    | 1156 ->
        "<SYNTAX ERROR>\n"
    | 1157 ->
        "<SYNTAX ERROR>\n"
    | 1158 ->
        "<SYNTAX ERROR>\n"
    | 1159 ->
        "<SYNTAX ERROR>\n"
    | 1593 ->
        "<SYNTAX ERROR>\n"
    | 1596 ->
        "<SYNTAX ERROR>\n"
    | 1614 ->
        "<SYNTAX ERROR>\n"
    | 1615 ->
        "<SYNTAX ERROR>\n"
    | 1151 ->
        "<SYNTAX ERROR>\n"
    | 1152 ->
        "<SYNTAX ERROR>\n"
    | 1624 ->
        "<SYNTAX ERROR>\n"
    | 1600 ->
        "<SYNTAX ERROR>\n"
    | 1604 ->
        "<SYNTAX ERROR>\n"
    | 1606 ->
        "<SYNTAX ERROR>\n"
    | 1607 ->
        "<SYNTAX ERROR>\n"
    | 1617 ->
        "<SYNTAX ERROR>\n"
    | 1608 ->
        "<SYNTAX ERROR>\n"
    | 1609 ->
        "<SYNTAX ERROR>\n"
    | 1610 ->
        "<SYNTAX ERROR>\n"
    | 1625 ->
        "<SYNTAX ERROR>\n"
    | 1642 ->
        "<SYNTAX ERROR>\n"
    | 1626 ->
        "<SYNTAX ERROR>\n"
    | 1651 ->
        "<SYNTAX ERROR>\n"
    | 1634 ->
        "<SYNTAX ERROR>\n"
    | 1652 ->
        "<SYNTAX ERROR>\n"
    | 1653 ->
        "<SYNTAX ERROR>\n"
    | 1641 ->
        "<SYNTAX ERROR>\n"
    | 1638 ->
        "<SYNTAX ERROR>\n"
    | 1639 ->
        "<SYNTAX ERROR>\n"
    | 1640 ->
        "<SYNTAX ERROR>\n"
    | 1647 ->
        "<SYNTAX ERROR>\n"
    | 1648 ->
        "<SYNTAX ERROR>\n"
    | 1649 ->
        "<SYNTAX ERROR>\n"
    | 568 ->
        "<SYNTAX ERROR>\n"
    | 151 ->
        "<SYNTAX ERROR>\n"
    | 889 ->
        "<SYNTAX ERROR>\n"
    | 2616 ->
        "<SYNTAX ERROR>\n"
    | 930 ->
        "<SYNTAX ERROR>\n"
    | 925 ->
        "<SYNTAX ERROR>\n"
    | 2618 ->
        "<SYNTAX ERROR>\n"
    | 1741 ->
        "<SYNTAX ERROR>\n"
    | 931 ->
        "<SYNTAX ERROR>\n"
    | 942 ->
        "<SYNTAX ERROR>\n"
    | 933 ->
        "<SYNTAX ERROR>\n"
    | 934 ->
        "<SYNTAX ERROR>\n"
    | 2617 ->
        "<SYNTAX ERROR>\n"
    | 958 ->
        "<SYNTAX ERROR>\n"
    | 959 ->
        "<SYNTAX ERROR>\n"
    | 961 ->
        "<SYNTAX ERROR>\n"
    | 1005 ->
        "<SYNTAX ERROR>\n"
    | 1006 ->
        "<SYNTAX ERROR>\n"
    | 1007 ->
        "<SYNTAX ERROR>\n"
    | 1008 ->
        "<SYNTAX ERROR>\n"
    | 1009 ->
        "<SYNTAX ERROR>\n"
    | 1010 ->
        "<SYNTAX ERROR>\n"
    | 1011 ->
        "<SYNTAX ERROR>\n"
    | 1012 ->
        "<SYNTAX ERROR>\n"
    | 1018 ->
        "<SYNTAX ERROR>\n"
    | 1020 ->
        "<SYNTAX ERROR>\n"
    | 1013 ->
        "<SYNTAX ERROR>\n"
    | 1014 ->
        "<SYNTAX ERROR>\n"
    | 1025 ->
        "<SYNTAX ERROR>\n"
    | 1026 ->
        "<SYNTAX ERROR>\n"
    | 1027 ->
        "<SYNTAX ERROR>\n"
    | 1028 ->
        "<SYNTAX ERROR>\n"
    | 1742 ->
        "<SYNTAX ERROR>\n"
    | 1743 ->
        "<SYNTAX ERROR>\n"
    | 793 ->
        "<SYNTAX ERROR>\n"
    | 1031 ->
        "<SYNTAX ERROR>\n"
    | 1032 ->
        "<SYNTAX ERROR>\n"
    | 1713 ->
        "<SYNTAX ERROR>\n"
    | 966 ->
        "<SYNTAX ERROR>\n"
    | 967 ->
        "<SYNTAX ERROR>\n"
    | 969 ->
        "<SYNTAX ERROR>\n"
    | 970 ->
        "<SYNTAX ERROR>\n"
    | 976 ->
        "<SYNTAX ERROR>\n"
    | 974 ->
        "<SYNTAX ERROR>\n"
    | 972 ->
        "<SYNTAX ERROR>\n"
    | 989 ->
        "<SYNTAX ERROR>\n"
    | 990 ->
        "<SYNTAX ERROR>\n"
    | 982 ->
        "<SYNTAX ERROR>\n"
    | 983 ->
        "<SYNTAX ERROR>\n"
    | 987 ->
        "<SYNTAX ERROR>\n"
    | 76 ->
        "<SYNTAX ERROR>\n"
    | 986 ->
        "<SYNTAX ERROR>\n"
    | 984 ->
        "<SYNTAX ERROR>\n"
    | 117 ->
        "<SYNTAX ERROR>\n"
    | 256 ->
        "<SYNTAX ERROR>\n"
    | 993 ->
        "<SYNTAX ERROR>\n"
    | 994 ->
        "<SYNTAX ERROR>\n"
    | 257 ->
        "<SYNTAX ERROR>\n"
    | 258 ->
        "<SYNTAX ERROR>\n"
    | 409 ->
        "<SYNTAX ERROR>\n"
    | 410 ->
        "<SYNTAX ERROR>\n"
    | 2405 ->
        "<SYNTAX ERROR>\n"
    | 550 ->
        "<SYNTAX ERROR>\n"
    | 2320 ->
        "<SYNTAX ERROR>\n"
    | 2321 ->
        "<SYNTAX ERROR>\n"
    | 2322 ->
        "<SYNTAX ERROR>\n"
    | 2323 ->
        "<SYNTAX ERROR>\n"
    | 2324 ->
        "<SYNTAX ERROR>\n"
    | 2325 ->
        "<SYNTAX ERROR>\n"
    | 1923 ->
        "<SYNTAX ERROR>\n"
    | 1924 ->
        "<SYNTAX ERROR>\n"
    | 1926 ->
        "<SYNTAX ERROR>\n"
    | 1931 ->
        "<SYNTAX ERROR>\n"
    | 1929 ->
        "<SYNTAX ERROR>\n"
    | 1927 ->
        "<SYNTAX ERROR>\n"
    | 1952 ->
        "<SYNTAX ERROR>\n"
    | 1953 ->
        "<SYNTAX ERROR>\n"
    | 1932 ->
        "<SYNTAX ERROR>\n"
    | 1933 ->
        "<SYNTAX ERROR>\n"
    | 1950 ->
        "<SYNTAX ERROR>\n"
    | 1935 ->
        "<SYNTAX ERROR>\n"
    | 1949 ->
        "<SYNTAX ERROR>\n"
    | 1934 ->
        "<SYNTAX ERROR>\n"
    | 1936 ->
        "<SYNTAX ERROR>\n"
    | 1937 ->
        "<SYNTAX ERROR>\n"
    | 1940 ->
        "<SYNTAX ERROR>\n"
    | 551 ->
        "<SYNTAX ERROR>\n"
    | 644 ->
        "<SYNTAX ERROR>\n"
    | 1957 ->
        "<SYNTAX ERROR>\n"
    | 1958 ->
        "<SYNTAX ERROR>\n"
    | 645 ->
        "<SYNTAX ERROR>\n"
    | 646 ->
        "<SYNTAX ERROR>\n"
    | 552 ->
        "<SYNTAX ERROR>\n"
    | 553 ->
        "<SYNTAX ERROR>\n"
    | 2316 ->
        "<SYNTAX ERROR>\n"
    | 1913 ->
        "<SYNTAX ERROR>\n"
    | 1967 ->
        "<SYNTAX ERROR>\n"
    | 1968 ->
        "<SYNTAX ERROR>\n"
    | 1969 ->
        "<SYNTAX ERROR>\n"
    | 1970 ->
        "<SYNTAX ERROR>\n"
    | 1971 ->
        "<SYNTAX ERROR>\n"
    | 1972 ->
        "<SYNTAX ERROR>\n"
    | 1921 ->
        "<SYNTAX ERROR>\n"
    | 2317 ->
        "<SYNTAX ERROR>\n"
    | 1914 ->
        "<SYNTAX ERROR>\n"
    | 923 ->
        "<SYNTAX ERROR>\n"
    | 1735 ->
        "<SYNTAX ERROR>\n"
    | 1734 ->
        "<SYNTAX ERROR>\n"
    | 1716 ->
        "<SYNTAX ERROR>\n"
    | 1717 ->
        "<SYNTAX ERROR>\n"
    | 1718 ->
        "<SYNTAX ERROR>\n"
    | 1719 ->
        "<SYNTAX ERROR>\n"
    | 1720 ->
        "<SYNTAX ERROR>\n"
    | 1723 ->
        "<SYNTAX ERROR>\n"
    | 945 ->
        "<SYNTAX ERROR>\n"
    | 1726 ->
        "<SYNTAX ERROR>\n"
    | 1727 ->
        "<SYNTAX ERROR>\n"
    | 1747 ->
        "<SYNTAX ERROR>\n"
    | 1729 ->
        "<SYNTAX ERROR>\n"
    | 1654 ->
        "<SYNTAX ERROR>\n"
    | 1730 ->
        "<SYNTAX ERROR>\n"
    | 1748 ->
        "<SYNTAX ERROR>\n"
    | 1749 ->
        "<SYNTAX ERROR>\n"
    | 2623 ->
        "<SYNTAX ERROR>\n"
    | 2625 ->
        "<SYNTAX ERROR>\n"
    | 279 ->
        "<SYNTAX ERROR>\n"
    | 283 ->
        "<SYNTAX ERROR>\n"
    | 284 ->
        "<SYNTAX ERROR>\n"
    | 195 ->
        "<SYNTAX ERROR>\n"
    | 77 ->
        "<SYNTAX ERROR>\n"
    | 78 ->
        "<SYNTAX ERROR>\n"
    | 80 ->
        "<SYNTAX ERROR>\n"
    | 196 ->
        "<SYNTAX ERROR>\n"
    | 113 ->
        "<SYNTAX ERROR>\n"
    | 9 ->
        "<SYNTAX ERROR>\n"
    | 10 ->
        "<SYNTAX ERROR>\n"
    | 2554 ->
        "<SYNTAX ERROR>\n"
    | 110 ->
        "<SYNTAX ERROR>\n"
    | 2552 ->
        "Expecting one of the following:\n  - \",\" to start the type in the tuple\n  - \")\" to finish the tuple type definition\n"
    | 277 ->
        "<SYNTAX ERROR>\n"
    | 116 ->
        "<SYNTAX ERROR>\n"
    | 121 ->
        "<SYNTAX ERROR>\n"
    | 122 ->
        "<SYNTAX ERROR>\n"
    | 127 ->
        "<SYNTAX ERROR>\n"
    | 128 ->
        "<SYNTAX ERROR>\n"
    | 2549 ->
        "<SYNTAX ERROR>\n"
    | 2550 ->
        "<SYNTAX ERROR>\n"
    | 120 ->
        "<SYNTAX ERROR>\n"
    | 118 ->
        "<SYNTAX ERROR>\n"
    | 262 ->
        "<SYNTAX ERROR>\n"
    | 108 ->
        "<SYNTAX ERROR>\n"
    | 130 ->
        "<SYNTAX ERROR>\n"
    | 2541 ->
        "<SYNTAX ERROR>\n"
    | 2542 ->
        "<SYNTAX ERROR>\n"
    | 2543 ->
        "<SYNTAX ERROR>\n"
    | 2545 ->
        "<SYNTAX ERROR>\n"
    | 134 ->
        "<SYNTAX ERROR>\n"
    | 139 ->
        "<SYNTAX ERROR>\n"
    | 140 ->
        "<SYNTAX ERROR>\n"
    | 294 ->
        "<SYNTAX ERROR>\n"
    | 141 ->
        "<SYNTAX ERROR>\n"
    | 493 ->
        "<SYNTAX ERROR>\n"
    | 492 ->
        "<SYNTAX ERROR>\n"
    | 495 ->
        "<SYNTAX ERROR>\n"
    | 295 ->
        "<SYNTAX ERROR>\n"
    | 137 ->
        "<SYNTAX ERROR>\n"
    | 143 ->
        "<SYNTAX ERROR>\n"
    | 2534 ->
        "<SYNTAX ERROR>\n"
    | 2536 ->
        "<SYNTAX ERROR>\n"
    | 2537 ->
        "<SYNTAX ERROR>\n"
    | 157 ->
        "<SYNTAX ERROR>\n"
    | 145 ->
        "<SYNTAX ERROR>\n"
    | 146 ->
        "<SYNTAX ERROR>\n"
    | 2532 ->
        "<SYNTAX ERROR>\n"
    | 148 ->
        "<SYNTAX ERROR>\n"
    | 149 ->
        "<SYNTAX ERROR>\n"
    | 2528 ->
        "<SYNTAX ERROR>\n"
    | 2529 ->
        "<SYNTAX ERROR>\n"
    | 2530 ->
        "<SYNTAX ERROR>\n"
    | 150 ->
        "<SYNTAX ERROR>\n"
    | 155 ->
        "<SYNTAX ERROR>\n"
    | 2526 ->
        "<SYNTAX ERROR>\n"
    | 2522 ->
        "<SYNTAX ERROR>\n"
    | 2519 ->
        "<SYNTAX ERROR>\n"
    | 2627 ->
        "<SYNTAX ERROR>\n"
    | 2629 ->
        "<SYNTAX ERROR>\n"
    | 2631 ->
        "<SYNTAX ERROR>\n"
    | 2632 ->
        "<SYNTAX ERROR>\n"
    | 788 ->
        "<SYNTAX ERROR>\n"
    | 790 ->
        "<SYNTAX ERROR>\n"
    | 801 ->
        "<SYNTAX ERROR>\n"
    | 791 ->
        "<SYNTAX ERROR>\n"
    | 783 ->
        "<SYNTAX ERROR>\n"
    | 628 ->
        "<SYNTAX ERROR>\n"
    | 592 ->
        "<SYNTAX ERROR>\n"
    | 764 ->
        "<SYNTAX ERROR>\n"
    | 193 ->
        "<SYNTAX ERROR>\n"
    | 198 ->
        "<SYNTAX ERROR>\n"
    | 204 ->
        "<SYNTAX ERROR>\n"
    | 210 ->
        "<SYNTAX ERROR>\n"
    | 808 ->
        "<SYNTAX ERROR>\n"
    | 809 ->
        "<SYNTAX ERROR>\n"
    | 838 ->
        "<SYNTAX ERROR>\n"
    | 799 ->
        "<SYNTAX ERROR>\n"
    | 727 ->
        "<SYNTAX ERROR>\n"
    | 714 ->
        "<SYNTAX ERROR>\n"
    | 716 ->
        "<SYNTAX ERROR>\n"
    | 843 ->
        "<SYNTAX ERROR>\n"
    | 446 ->
        "<SYNTAX ERROR>\n"
    | 450 ->
        "<SYNTAX ERROR>\n"
    | 451 ->
        "<SYNTAX ERROR>\n"
    | 245 ->
        "<SYNTAX ERROR>\n"
    | 324 ->
        "<SYNTAX ERROR>\n"
    | 249 ->
        "<SYNTAX ERROR>\n"
    | 275 ->
        "<SYNTAX ERROR>\n"
    | 253 ->
        "<SYNTAX ERROR>\n"
    | 254 ->
        "<SYNTAX ERROR>\n"
    | 336 ->
        "<SYNTAX ERROR>\n"
    | 247 ->
        "<SYNTAX ERROR>\n"
    | 331 ->
        "<SYNTAX ERROR>\n"
    | 332 ->
        "<SYNTAX ERROR>\n"
    | 333 ->
        "<SYNTAX ERROR>\n"
    | 334 ->
        "<SYNTAX ERROR>\n"
    | 2416 ->
        "<SYNTAX ERROR>\n"
    | 291 ->
        "<SYNTAX ERROR>\n"
    | 292 ->
        "<SYNTAX ERROR>\n"
    | 298 ->
        "<SYNTAX ERROR>\n"
    | 300 ->
        "<SYNTAX ERROR>\n"
    | 302 ->
        "<SYNTAX ERROR>\n"
    | 303 ->
        "<SYNTAX ERROR>\n"
    | 299 ->
        "<SYNTAX ERROR>\n"
    | 308 ->
        "<SYNTAX ERROR>\n"
    | 311 ->
        "<SYNTAX ERROR>\n"
    | 310 ->
        "<SYNTAX ERROR>\n"
    | 313 ->
        "<SYNTAX ERROR>\n"
    | 319 ->
        "<SYNTAX ERROR>\n"
    | 320 ->
        "<SYNTAX ERROR>\n"
    | 321 ->
        "<SYNTAX ERROR>\n"
    | 314 ->
        "<SYNTAX ERROR>\n"
    | 315 ->
        "<SYNTAX ERROR>\n"
    | 317 ->
        "<SYNTAX ERROR>\n"
    | 734 ->
        "<SYNTAX ERROR>\n"
    | 717 ->
        "<SYNTAX ERROR>\n"
    | 719 ->
        "<SYNTAX ERROR>\n"
    | 704 ->
        "<SYNTAX ERROR>\n"
    | 673 ->
        "<SYNTAX ERROR>\n"
    | 694 ->
        "<SYNTAX ERROR>\n"
    | 685 ->
        "<SYNTAX ERROR>\n"
    | 212 ->
        "<SYNTAX ERROR>\n"
    | 215 ->
        "<SYNTAX ERROR>\n"
    | 216 ->
        "<SYNTAX ERROR>\n"
    | 222 ->
        "<SYNTAX ERROR>\n"
    | 230 ->
        "<SYNTAX ERROR>\n"
    | 231 ->
        "<SYNTAX ERROR>\n"
    | 233 ->
        "<SYNTAX ERROR>\n"
    | 240 ->
        "<SYNTAX ERROR>\n"
    | 241 ->
        "<SYNTAX ERROR>\n"
    | 242 ->
        "<SYNTAX ERROR>\n"
    | 243 ->
        "<SYNTAX ERROR>\n"
    | 2420 ->
        "<SYNTAX ERROR>\n"
    | 2421 ->
        "<SYNTAX ERROR>\n"
    | 234 ->
        "<SYNTAX ERROR>\n"
    | 239 ->
        "<SYNTAX ERROR>\n"
    | 630 ->
        "<SYNTAX ERROR>\n"
    | 636 ->
        "<SYNTAX ERROR>\n"
    | 725 ->
        "<SYNTAX ERROR>\n"
    | 814 ->
        "<SYNTAX ERROR>\n"
    | 637 ->
        "<SYNTAX ERROR>\n"
    | 638 ->
        "<SYNTAX ERROR>\n"
    | 640 ->
        "<SYNTAX ERROR>\n"
    | 830 ->
        "<SYNTAX ERROR>\n"
    | 831 ->
        "<SYNTAX ERROR>\n"
    | 832 ->
        "<SYNTAX ERROR>\n"
    | 833 ->
        "<SYNTAX ERROR>\n"
    | 834 ->
        "<SYNTAX ERROR>\n"
    | 835 ->
        "<SYNTAX ERROR>\n"
    | 652 ->
        "<SYNTAX ERROR>\n"
    | 827 ->
        "<SYNTAX ERROR>\n"
    | 655 ->
        "<SYNTAX ERROR>\n"
    | 822 ->
        "<SYNTAX ERROR>\n"
    | 656 ->
        "<SYNTAX ERROR>\n"
    | 661 ->
        "<SYNTAX ERROR>\n"
    | 672 ->
        "<SYNTAX ERROR>\n"
    | 680 ->
        "<SYNTAX ERROR>\n"
    | 688 ->
        "<SYNTAX ERROR>\n"
    | 2423 ->
        "<SYNTAX ERROR>\n"
    | 2424 ->
        "<SYNTAX ERROR>\n"
    | 2425 ->
        "<SYNTAX ERROR>\n"
    | 2426 ->
        "<SYNTAX ERROR>\n"
    | 2427 ->
        "<SYNTAX ERROR>\n"
    | 2428 ->
        "<SYNTAX ERROR>\n"
    | 728 ->
        "<SYNTAX ERROR>\n"
    | 739 ->
        "<SYNTAX ERROR>\n"
    | 742 ->
        "<SYNTAX ERROR>\n"
    | 732 ->
        "<SYNTAX ERROR>\n"
    | 733 ->
        "<SYNTAX ERROR>\n"
    | 653 ->
        "<SYNTAX ERROR>\n"
    | 654 ->
        "<SYNTAX ERROR>\n"
    | 743 ->
        "<SYNTAX ERROR>\n"
    | 746 ->
        "<SYNTAX ERROR>\n"
    | 754 ->
        "<SYNTAX ERROR>\n"
    | 750 ->
        "<SYNTAX ERROR>\n"
    | 753 ->
        "<SYNTAX ERROR>\n"
    | 751 ->
        "Expecting a valid list identifier\n"
    | 752 ->
        "<SYNTAX ERROR>\n"
    | 755 ->
        "<SYNTAX ERROR>\n"
    | 658 ->
        "<SYNTAX ERROR>\n"
    | 659 ->
        "<SYNTAX ERROR>\n"
    | 670 ->
        "<SYNTAX ERROR>\n"
    | 665 ->
        "<SYNTAX ERROR>\n"
    | 666 ->
        "<SYNTAX ERROR>\n"
    | 756 ->
        "<SYNTAX ERROR>\n"
    | 671 ->
        "<SYNTAX ERROR>\n"
    | 820 ->
        "<SYNTAX ERROR>\n"
    | 759 ->
        "<SYNTAX ERROR>\n"
    | 775 ->
        "<SYNTAX ERROR>\n"
    | 777 ->
        "<SYNTAX ERROR>\n"
    | 2635 ->
        "<SYNTAX ERROR>\n"
    | 2649 ->
        "<SYNTAX ERROR>\n"
    | 2636 ->
        "<SYNTAX ERROR>\n"
    | 2637 ->
        "<SYNTAX ERROR>\n"
    | 2643 ->
        "<SYNTAX ERROR>\n"
    | 2644 ->
        "<SYNTAX ERROR>\n"
    | 2647 ->
        "<SYNTAX ERROR>\n"
    | 2652 ->
        "<SYNTAX ERROR>\n"
    | 2659 ->
        "<SYNTAX ERROR>\n"
    | 2658 ->
        "<SYNTAX ERROR>\n"
    | 2655 ->
        "<SYNTAX ERROR>\n"
    | 2656 ->
        "<SYNTAX ERROR>\n"
    | _ ->
        raise Not_found
