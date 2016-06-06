
(* This file was auto-generated based on "src/reason_parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "<SYNTAX ERROR>\n"
    | 2 ->
        "<SYNTAX ERROR>\n"
    | 2695 ->
        "<SYNTAX ERROR>\n"
    | 590 ->
        "Expecting one of the following:\n  - an identifier to access a member of an object\n  - \"[\" + expression + \"]\" to access an element of a list\n  - \"(\" + expression + \")\"\n  - \"{\" + expression + \"}\"\n"
    | 591 ->
        "Expecting an expression\n"
    | 2356 ->
        "Expecting one of the following:\n  - an infix operation to connect two expressions\n  - \")\" to close the block\n"
    | 2358 ->
        "Expecting an expression\n"
    | 2359 ->
        "Expecting one of the following:\n  - an infix operation to connect two expressions\n  - \"}\" to close the block\n"
    | 2361 ->
        "Expecting an expression\n"
    | 2362 ->
        "Expecting one of the following:\n  - an infix operation to connect two expressions\n  - \"}\" to close the block\n"
    | 1228 ->
        "Expecting an expression\n"
    | 588 ->
        "Expecting an identifier\n"
    | 1161 ->
        "Expecting a structure item\n"
    | 2700 ->
        "Invalid token\n"
    | 1282 ->
        "Expecting an expression\n"
    | 1283 ->
        "Expecting one of the following:\n  - The continuation of the previous expression\n  - \":\" to start the next expression\n"
    | 1284 ->
        "Expecting an expression\n"
    | 1243 ->
        "Expecting an expression\n"
    | 1249 ->
        "Expecting an expression\n"
    | 1251 ->
        "Expecting an expression\n"
    | 1245 ->
        "Expecting an expression\n"
    | 1253 ->
        "Expecting an expression\n"
    | 1255 ->
        "Expecting an expression\n"
    | 1257 ->
        "Expecting an expression\n"
    | 15 ->
        "Expecting one of the following:\n  - \")\" to form a unit value \"()\"\n  - \"module\" to start a module expression\n  - an expression\n  - an operator to denote the prefix form of an operator\n"
    | 1259 ->
        "Expecting an expression\n"
    | 1265 ->
        "Expecting an expression\n"
    | 1267 ->
        "Expecting an expression\n"
    | 2473 ->
        "Expecting \"]\"\n"
    | 403 ->
        "Expecting an attributed id\n"
    | 2580 ->
        "Expecting \"]\"\n"
    | 161 ->
        "Expecting an attribute id\n"
    | 1230 ->
        "Expecting an expression\n"
    | 1247 ->
        "Expecting an expression\n"
    | 1261 ->
        "Expecting an expression\n"
    | 1263 ->
        "Expecting an expression\n"
    | 1269 ->
        "Expecting an expression\n"
    | 1271 ->
        "Expecting an expression\n"
    | 870 ->
        "<SYNTAX ERROR>\n"
    | 871 ->
        "<SYNTAX ERROR>\n"
    | 2264 ->
        "<SYNTAX ERROR>\n"
    | 872 ->
        "<SYNTAX ERROR>\n"
    | 874 ->
        "<SYNTAX ERROR>\n"
    | 2260 ->
        "<SYNTAX ERROR>\n"
    | 2262 ->
        "<SYNTAX ERROR>\n"
    | 2267 ->
        "<SYNTAX ERROR>\n"
    | 2268 ->
        "<SYNTAX ERROR>\n"
    | 2272 ->
        "<SYNTAX ERROR>\n"
    | 2282 ->
        "<SYNTAX ERROR>\n"
    | 2287 ->
        "<SYNTAX ERROR>\n"
    | 1887 ->
        "<SYNTAX ERROR>\n"
    | 1279 ->
        "<SYNTAX ERROR>\n"
    | 1273 ->
        "<SYNTAX ERROR>\n"
    | 1275 ->
        "<SYNTAX ERROR>\n"
    | 1277 ->
        "<SYNTAX ERROR>\n"
    | 75 ->
        "<SYNTAX ERROR>\n"
    | 977 ->
        "<SYNTAX ERROR>\n"
    | 978 ->
        "<SYNTAX ERROR>\n"
    | 104 ->
        "Expecting one of the following:\n  - \"=\" to start the body of the type declaration\n  - \"constraint\" to add constraints to the type declaration\n  - \";\" to finish type declaratoin\n  - \"+=\" to form a string type extension\n  - \"and\" to declare another type\n"
    | 91 ->
        "<SYNTAX ERROR>\n"
    | 2680 ->
        "<SYNTAX ERROR>\n"
    | 2684 ->
        "<SYNTAX ERROR>\n"
    | 2681 ->
        "<SYNTAX ERROR>\n"
    | 2682 ->
        "<SYNTAX ERROR>\n"
    | 95 ->
        "<SYNTAX ERROR>\n"
    | 97 ->
        "<SYNTAX ERROR>\n"
    | 99 ->
        "<SYNTAX ERROR>\n"
    | 101 ->
        "<SYNTAX ERROR>\n"
    | 1171 ->
        "<SYNTAX ERROR>\n"
    | 105 ->
        "<SYNTAX ERROR>\n"
    | 2665 ->
        "<SYNTAX ERROR>\n"
    | 2666 ->
        "<SYNTAX ERROR>\n"
    | 2656 ->
        "<SYNTAX ERROR>\n"
    | 2668 ->
        "<SYNTAX ERROR>\n"
    | 2673 ->
        "<SYNTAX ERROR>\n"
    | 2674 ->
        "<SYNTAX ERROR>\n"
    | 2635 ->
        "<SYNTAX ERROR>\n"
    | 107 ->
        "<SYNTAX ERROR>\n"
    | 2624 ->
        "<SYNTAX ERROR>\n"
    | 2625 ->
        "<SYNTAX ERROR>\n"
    | 2661 ->
        "Expecting at least one type field definition in the form of:\n  <field name> : <type>\n"
    | 504 ->
        "Expecting a type field definition in the form of:\n  <field name> : <type>\n"
    | 505 ->
        "Expecting \":\"\n"
    | 506 ->
        "Expecting a type name describing this field\n"
    | 2662 ->
        "Expecting one of the following:\n  - \",\" to finish current type field\n  - \"}\" to finish type definition\n"
    | 520 ->
        "Expecting one of the following:\n  - another type field definition\n  - \"}\" to finish entire type definition\n"
    | 1176 ->
        "<SYNTAX ERROR>\n"
    | 2653 ->
        "<SYNTAX ERROR>\n"
    | 999 ->
        "<SYNTAX ERROR>\n"
    | 1000 ->
        "<SYNTAX ERROR>\n"
    | 1001 ->
        "<SYNTAX ERROR>\n"
    | 1172 ->
        "<SYNTAX ERROR>\n"
    | 1174 ->
        "<SYNTAX ERROR>\n"
    | 163 ->
        "<SYNTAX ERROR>\n"
    | 2575 ->
        "<SYNTAX ERROR>\n"
    | 2574 ->
        "<SYNTAX ERROR>\n"
    | 2577 ->
        "<SYNTAX ERROR>\n"
    | 2516 ->
        "<SYNTAX ERROR>\n"
    | 2517 ->
        "<SYNTAX ERROR>\n"
    | 2518 ->
        "Expecting a sequence item\n"
    | 1917 ->
        "Expecting one of the following:\n  - \"|\" to open the next pattern\n  - \"=>\" to start the body of the matched pattern\n  - \"when\" to start a contitional guard for the previous pattern\n"
    | 2549 ->
        "Expecting the body of the matched pattern\n"
    | 2578 ->
        "Expecting one of the following:\n  - \"}\" to finish the block\n  - \"|\" to start another pattern matching case\n"
    | 2532 ->
        "<SYNTAX ERROR>\n"
    | 2519 ->
        "<SYNTAX ERROR>\n"
    | 2520 ->
        "<SYNTAX ERROR>\n"
    | 2523 ->
        "<SYNTAX ERROR>\n"
    | 2524 ->
        "<SYNTAX ERROR>\n"
    | 2521 ->
        "<SYNTAX ERROR>\n"
    | 2542 ->
        "<SYNTAX ERROR>\n"
    | 2543 ->
        "<SYNTAX ERROR>\n"
    | 2546 ->
        "<SYNTAX ERROR>\n"
    | 2545 ->
        "<SYNTAX ERROR>\n"
    | 1916 ->
        "Expecting a match case\n"
    | 909 ->
        "<SYNTAX ERROR>\n"
    | 124 ->
        "<SYNTAX ERROR>\n"
    | 125 ->
        "<SYNTAX ERROR>\n"
    | 910 ->
        "<SYNTAX ERROR>\n"
    | 1891 ->
        "<SYNTAX ERROR>\n"
    | 1894 ->
        "<SYNTAX ERROR>\n"
    | 1908 ->
        "<SYNTAX ERROR>\n"
    | 1896 ->
        "<SYNTAX ERROR>\n"
    | 1897 ->
        "<SYNTAX ERROR>\n"
    | 1900 ->
        "<SYNTAX ERROR>\n"
    | 1902 ->
        "<SYNTAX ERROR>\n"
    | 1903 ->
        "<SYNTAX ERROR>\n"
    | 1905 ->
        "<SYNTAX ERROR>\n"
    | 168 ->
        "<SYNTAX ERROR>\n"
    | 2558 ->
        "<SYNTAX ERROR>\n"
    | 2559 ->
        "<SYNTAX ERROR>\n"
    | 1160 ->
        "Incomplete module item, forgetting a \";\"?\n"
    | 1215 ->
        "<SYNTAX ERROR>\n"
    | 1218 ->
        "<SYNTAX ERROR>\n"
    | 6 ->
        "<SYNTAX ERROR>\n"
    | 1240 ->
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
    | 907 ->
        "<SYNTAX ERROR>\n"
    | 557 ->
        "<SYNTAX ERROR>\n"
    | 16 ->
        "<SYNTAX ERROR>\n"
    | 2692 ->
        "<SYNTAX ERROR>\n"
    | 627 ->
        "<SYNTAX ERROR>\n"
    | 628 ->
        "<SYNTAX ERROR>\n"
    | 2311 ->
        "<SYNTAX ERROR>\n"
    | 2313 ->
        "<SYNTAX ERROR>\n"
    | 2314 ->
        "<SYNTAX ERROR>\n"
    | 2316 ->
        "<SYNTAX ERROR>\n"
    | 2317 ->
        "<SYNTAX ERROR>\n"
    | 1418 ->
        "<SYNTAX ERROR>\n"
    | 624 ->
        "<SYNTAX ERROR>\n"
    | 1476 ->
        "<SYNTAX ERROR>\n"
    | 1477 ->
        "<SYNTAX ERROR>\n"
    | 1478 ->
        "<SYNTAX ERROR>\n"
    | 1433 ->
        "<SYNTAX ERROR>\n"
    | 1439 ->
        "<SYNTAX ERROR>\n"
    | 1441 ->
        "<SYNTAX ERROR>\n"
    | 1435 ->
        "<SYNTAX ERROR>\n"
    | 1443 ->
        "<SYNTAX ERROR>\n"
    | 1445 ->
        "<SYNTAX ERROR>\n"
    | 1447 ->
        "<SYNTAX ERROR>\n"
    | 184 ->
        "<SYNTAX ERROR>\n"
    | 1449 ->
        "<SYNTAX ERROR>\n"
    | 1455 ->
        "<SYNTAX ERROR>\n"
    | 1457 ->
        "<SYNTAX ERROR>\n"
    | 2476 ->
        "<SYNTAX ERROR>\n"
    | 340 ->
        "<SYNTAX ERROR>\n"
    | 1420 ->
        "<SYNTAX ERROR>\n"
    | 1437 ->
        "<SYNTAX ERROR>\n"
    | 1451 ->
        "<SYNTAX ERROR>\n"
    | 1453 ->
        "<SYNTAX ERROR>\n"
    | 1459 ->
        "<SYNTAX ERROR>\n"
    | 1461 ->
        "<SYNTAX ERROR>\n"
    | 920 ->
        "<SYNTAX ERROR>\n"
    | 921 ->
        "<SYNTAX ERROR>\n"
    | 1831 ->
        "<SYNTAX ERROR>\n"
    | 922 ->
        "<SYNTAX ERROR>\n"
    | 923 ->
        "<SYNTAX ERROR>\n"
    | 1824 ->
        "<SYNTAX ERROR>\n"
    | 1826 ->
        "<SYNTAX ERROR>\n"
    | 1834 ->
        "<SYNTAX ERROR>\n"
    | 1836 ->
        "<SYNTAX ERROR>\n"
    | 1851 ->
        "<SYNTAX ERROR>\n"
    | 1863 ->
        "<SYNTAX ERROR>\n"
    | 1868 ->
        "<SYNTAX ERROR>\n"
    | 2409 ->
        "<SYNTAX ERROR>\n"
    | 2414 ->
        "<SYNTAX ERROR>\n"
    | 2411 ->
        "<SYNTAX ERROR>\n"
    | 1298 ->
        "<SYNTAX ERROR>\n"
    | 2408 ->
        "<SYNTAX ERROR>\n"
    | 1469 ->
        "<SYNTAX ERROR>\n"
    | 1496 ->
        "<SYNTAX ERROR>\n"
    | 1317 ->
        "<SYNTAX ERROR>\n"
    | 1463 ->
        "<SYNTAX ERROR>\n"
    | 1465 ->
        "<SYNTAX ERROR>\n"
    | 1467 ->
        "<SYNTAX ERROR>\n"
    | 166 ->
        "<SYNTAX ERROR>\n"
    | 2564 ->
        "<SYNTAX ERROR>\n"
    | 2563 ->
        "<SYNTAX ERROR>\n"
    | 2566 ->
        "<SYNTAX ERROR>\n"
    | 1408 ->
        "<SYNTAX ERROR>\n"
    | 1409 ->
        "<SYNTAX ERROR>\n"
    | 1471 ->
        "<SYNTAX ERROR>\n"
    | 1474 ->
        "<SYNTAX ERROR>\n"
    | 1492 ->
        "<SYNTAX ERROR>\n"
    | 1480 ->
        "<SYNTAX ERROR>\n"
    | 1481 ->
        "<SYNTAX ERROR>\n"
    | 1484 ->
        "<SYNTAX ERROR>\n"
    | 1486 ->
        "<SYNTAX ERROR>\n"
    | 1487 ->
        "<SYNTAX ERROR>\n"
    | 1489 ->
        "<SYNTAX ERROR>\n"
    | 173 ->
        "<SYNTAX ERROR>\n"
    | 2510 ->
        "<SYNTAX ERROR>\n"
    | 2511 ->
        "<SYNTAX ERROR>\n"
    | 1349 ->
        "<SYNTAX ERROR>\n"
    | 1357 ->
        "<SYNTAX ERROR>\n"
    | 810 ->
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
    | 558 ->
        "<SYNTAX ERROR>\n"
    | 2395 ->
        "<SYNTAX ERROR>\n"
    | 2397 ->
        "<SYNTAX ERROR>\n"
    | 2399 ->
        "<SYNTAX ERROR>\n"
    | 1828 ->
        "<SYNTAX ERROR>\n"
    | 1829 ->
        "<SYNTAX ERROR>\n"
    | 415 ->
        "Expecting one of the following:\n  - \")\" to form a unit value \"()\"\n  - \"module\" to start a module expression\n  - an expression\n  - an operator to denote the prefix form of an operator\n"
    | 2453 ->
        "<SYNTAX ERROR>\n"
    | 738 ->
        "<SYNTAX ERROR>\n"
    | 418 ->
        "Expecting a module expression\n"
    | 2440 ->
        "<SYNTAX ERROR>\n"
    | 2442 ->
        "<SYNTAX ERROR>\n"
    | 2444 ->
        "<SYNTAX ERROR>\n"
    | 2446 ->
        "<SYNTAX ERROR>\n"
    | 2447 ->
        "<SYNTAX ERROR>\n"
    | 2448 ->
        "<SYNTAX ERROR>\n"
    | 2449 ->
        "<SYNTAX ERROR>\n"
    | 2450 ->
        "<SYNTAX ERROR>\n"
    | 2451 ->
        "<SYNTAX ERROR>\n"
    | 1416 ->
        "<SYNTAX ERROR>\n"
    | 2498 ->
        "<SYNTAX ERROR>\n"
    | 189 ->
        "<SYNTAX ERROR>\n"
    | 573 ->
        "<SYNTAX ERROR>\n"
    | 2368 ->
        "<SYNTAX ERROR>\n"
    | 574 ->
        "<SYNTAX ERROR>\n"
    | 1858 ->
        "<SYNTAX ERROR>\n"
    | 1857 ->
        "<SYNTAX ERROR>\n"
    | 1852 ->
        "<SYNTAX ERROR>\n"
    | 1853 ->
        "<SYNTAX ERROR>\n"
    | 592 ->
        "<SYNTAX ERROR>\n"
    | 601 ->
        "<SYNTAX ERROR>\n"
    | 611 ->
        "<SYNTAX ERROR>\n"
    | 629 ->
        "<SYNTAX ERROR>\n"
    | 630 ->
        "<SYNTAX ERROR>\n"
    | 632 ->
        "<SYNTAX ERROR>\n"
    | 633 ->
        "<SYNTAX ERROR>\n"
    | 2308 ->
        "<SYNTAX ERROR>\n"
    | 2294 ->
        "<SYNTAX ERROR>\n"
    | 638 ->
        "<SYNTAX ERROR>\n"
    | 639 ->
        "<SYNTAX ERROR>\n"
    | 640 ->
        "<SYNTAX ERROR>\n"
    | 641 ->
        "<SYNTAX ERROR>\n"
    | 2292 ->
        "<SYNTAX ERROR>\n"
    | 2293 ->
        "<SYNTAX ERROR>\n"
    | 634 ->
        "<SYNTAX ERROR>\n"
    | 635 ->
        "<SYNTAX ERROR>\n"
    | 636 ->
        "<SYNTAX ERROR>\n"
    | 637 ->
        "<SYNTAX ERROR>\n"
    | 2301 ->
        "<SYNTAX ERROR>\n"
    | 2302 ->
        "<SYNTAX ERROR>\n"
    | 2303 ->
        "<SYNTAX ERROR>\n"
    | 2304 ->
        "<SYNTAX ERROR>\n"
    | 2305 ->
        "<SYNTAX ERROR>\n"
    | 2306 ->
        "<SYNTAX ERROR>\n"
    | 911 ->
        "<SYNTAX ERROR>\n"
    | 912 ->
        "<SYNTAX ERROR>\n"
    | 913 ->
        "<SYNTAX ERROR>\n"
    | 914 ->
        "<SYNTAX ERROR>\n"
    | 915 ->
        "<SYNTAX ERROR>\n"
    | 916 ->
        "<SYNTAX ERROR>\n"
    | 2401 ->
        "<SYNTAX ERROR>\n"
    | 2402 ->
        "<SYNTAX ERROR>\n"
    | 2403 ->
        "<SYNTAX ERROR>\n"
    | 2404 ->
        "<SYNTAX ERROR>\n"
    | 2405 ->
        "<SYNTAX ERROR>\n"
    | 2406 ->
        "<SYNTAX ERROR>\n"
    | 1830 ->
        "<SYNTAX ERROR>\n"
    | 619 ->
        "<SYNTAX ERROR>\n"
    | 1404 ->
        "<SYNTAX ERROR>\n"
    | 1226 ->
        "<SYNTAX ERROR>\n"
    | 925 ->
        "Incomplete let binding\n"
    | 1327 ->
        "<SYNTAX ERROR>\n"
    | 1333 ->
        "<SYNTAX ERROR>\n"
    | 1328 ->
        "<SYNTAX ERROR>\n"
    | 1329 ->
        "<SYNTAX ERROR>\n"
    | 1330 ->
        "<SYNTAX ERROR>\n"
    | 1332 ->
        "<SYNTAX ERROR>\n"
    | 1202 ->
        "<SYNTAX ERROR>\n"
    | 926 ->
        "<SYNTAX ERROR>\n"
    | 927 ->
        "<SYNTAX ERROR>\n"
    | 1806 ->
        "<SYNTAX ERROR>\n"
    | 1807 ->
        "<SYNTAX ERROR>\n"
    | 1808 ->
        "<SYNTAX ERROR>\n"
    | 1809 ->
        "<SYNTAX ERROR>\n"
    | 1813 ->
        "<SYNTAX ERROR>\n"
    | 1810 ->
        "<SYNTAX ERROR>\n"
    | 1811 ->
        "<SYNTAX ERROR>\n"
    | 1812 ->
        "<SYNTAX ERROR>\n"
    | 928 ->
        "<SYNTAX ERROR>\n"
    | 929 ->
        "<SYNTAX ERROR>\n"
    | 938 ->
        "<SYNTAX ERROR>\n"
    | 1799 ->
        "<SYNTAX ERROR>\n"
    | 1800 ->
        "<SYNTAX ERROR>\n"
    | 1801 ->
        "<SYNTAX ERROR>\n"
    | 1817 ->
        "<SYNTAX ERROR>\n"
    | 1180 ->
        "<SYNTAX ERROR>\n"
    | 1181 ->
        "<SYNTAX ERROR>\n"
    | 1203 ->
        "<SYNTAX ERROR>\n"
    | 1323 ->
        "Defining a function?\nExpecting one of the following:\n  - \"=>\" to start the function body\n  - an identifier to add a function parameter\n  - \":\" to specify the return type\n"
    | 1291 ->
        "<SYNTAX ERROR>\n"
    | 1208 ->
        "<SYNTAX ERROR>\n"
    | 1209 ->
        "<SYNTAX ERROR>\n"
    | 1210 ->
        "<SYNTAX ERROR>\n"
    | 1211 ->
        "<SYNTAX ERROR>\n"
    | 1212 ->
        "Expecting an expression as function body\n"
    | 1286 ->
        "<SYNTAX ERROR>\n"
    | 1287 ->
        "Defining a function?\nExpecting \"=>\" to start the function body\n"
    | 1288 ->
        "<SYNTAX ERROR>\n"
    | 1289 ->
        "<SYNTAX ERROR>\n"
    | 1204 ->
        "<SYNTAX ERROR>\n"
    | 1205 ->
        "<SYNTAX ERROR>\n"
    | 1206 ->
        "<SYNTAX ERROR>\n"
    | 1207 ->
        "<SYNTAX ERROR>\n"
    | 1296 ->
        "<SYNTAX ERROR>\n"
    | 1319 ->
        "<SYNTAX ERROR>\n"
    | 1320 ->
        "<SYNTAX ERROR>\n"
    | 1300 ->
        "<SYNTAX ERROR>\n"
    | 1301 ->
        "<SYNTAX ERROR>\n"
    | 1302 ->
        "<SYNTAX ERROR>\n"
    | 1305 ->
        "<SYNTAX ERROR>\n"
    | 1306 ->
        "<SYNTAX ERROR>\n"
    | 1307 ->
        "<SYNTAX ERROR>\n"
    | 1310 ->
        "<SYNTAX ERROR>\n"
    | 1311 ->
        "<SYNTAX ERROR>\n"
    | 1312 ->
        "<SYNTAX ERROR>\n"
    | 1313 ->
        "<SYNTAX ERROR>\n"
    | 1309 ->
        "<SYNTAX ERROR>\n"
    | 1549 ->
        "<SYNTAX ERROR>\n"
    | 69 ->
        "<SYNTAX ERROR>\n"
    | 1750 ->
        "<SYNTAX ERROR>\n"
    | 191 ->
        "<SYNTAX ERROR>\n"
    | 2496 ->
        "<SYNTAX ERROR>\n"
    | 2497 ->
        "<SYNTAX ERROR>\n"
    | 2495 ->
        "<SYNTAX ERROR>\n"
    | 70 ->
        "<SYNTAX ERROR>\n"
    | 1101 ->
        "<SYNTAX ERROR>\n"
    | 1057 ->
        "<SYNTAX ERROR>\n"
    | 2690 ->
        "<SYNTAX ERROR>\n"
    | 18 ->
        "<SYNTAX ERROR>\n"
    | 164 ->
        "<SYNTAX ERROR>\n"
    | 2570 ->
        "<SYNTAX ERROR>\n"
    | 1837 ->
        "<SYNTAX ERROR>\n"
    | 1840 ->
        "<SYNTAX ERROR>\n"
    | 1842 ->
        "<SYNTAX ERROR>\n"
    | 1845 ->
        "Expecting a type name\n"
    | 176 ->
        "Expecting an expression\n"
    | 1430 ->
        "Expecting an expression\n"
    | 1406 ->
        "Expecting an expression\n"
    | 618 ->
        "<SYNTAX ERROR>\n"
    | 1748 ->
        "Expecting \"]\" to finish current floating attribute\n"
    | 1059 ->
        "<SYNTAX ERROR>\n"
    | 167 ->
        "Expecting one of the following:\n  - an list item\n  - \"]\" to finish this list\n"
    | 2276 ->
        "Expecting one of the following:\n  - \",\" to separate two items in a list\n  - \"]\" to finish this list\n"
    | 2277 ->
        "<SYNTAX ERROR>\n"
    | 2273 ->
        "<SYNTAX ERROR>\n"
    | 2274 ->
        "<SYNTAX ERROR>\n"
    | 169 ->
        "<SYNTAX ERROR>\n"
    | 170 ->
        "<SYNTAX ERROR>\n"
    | 595 ->
        "<SYNTAX ERROR>\n"
    | 171 ->
        "<SYNTAX ERROR>\n"
    | 2552 ->
        "<SYNTAX ERROR>\n"
    | 174 ->
        "<SYNTAX ERROR>\n"
    | 1385 ->
        "<SYNTAX ERROR>\n"
    | 1386 ->
        "<SYNTAX ERROR>\n"
    | 1387 ->
        "<SYNTAX ERROR>\n"
    | 1388 ->
        "<SYNTAX ERROR>\n"
    | 1389 ->
        "<SYNTAX ERROR>\n"
    | 1390 ->
        "<SYNTAX ERROR>\n"
    | 1395 ->
        "<SYNTAX ERROR>\n"
    | 1396 ->
        "<SYNTAX ERROR>\n"
    | 1397 ->
        "<SYNTAX ERROR>\n"
    | 1398 ->
        "<SYNTAX ERROR>\n"
    | 1399 ->
        "<SYNTAX ERROR>\n"
    | 1402 ->
        "<SYNTAX ERROR>\n"
    | 1403 ->
        "<SYNTAX ERROR>\n"
    | 1495 ->
        "<SYNTAX ERROR>\n"
    | 1497 ->
        "<SYNTAX ERROR>\n"
    | 1498 ->
        "<SYNTAX ERROR>\n"
    | 1499 ->
        "<SYNTAX ERROR>\n"
    | 1394 ->
        "<SYNTAX ERROR>\n"
    | 2421 ->
        "<SYNTAX ERROR>\n"
    | 555 ->
        "<SYNTAX ERROR>\n"
    | 2351 ->
        "<SYNTAX ERROR>\n"
    | 2328 ->
        "<SYNTAX ERROR>\n"
    | 1500 ->
        "<SYNTAX ERROR>\n"
    | 1502 ->
        "<SYNTAX ERROR>\n"
    | 1503 ->
        "<SYNTAX ERROR>\n"
    | 1504 ->
        "<SYNTAX ERROR>\n"
    | 1505 ->
        "<SYNTAX ERROR>\n"
    | 1147 ->
        "<SYNTAX ERROR>\n"
    | 1149 ->
        "<SYNTAX ERROR>\n"
    | 1150 ->
        "<SYNTAX ERROR>\n"
    | 1507 ->
        "<SYNTAX ERROR>\n"
    | 1508 ->
        "<SYNTAX ERROR>\n"
    | 1509 ->
        "<SYNTAX ERROR>\n"
    | 1510 ->
        "<SYNTAX ERROR>\n"
    | 1513 ->
        "<SYNTAX ERROR>\n"
    | 1524 ->
        "<SYNTAX ERROR>\n"
    | 1514 ->
        "<SYNTAX ERROR>\n"
    | 1515 ->
        "<SYNTAX ERROR>\n"
    | 1516 ->
        "<SYNTAX ERROR>\n"
    | 1517 ->
        "<SYNTAX ERROR>\n"
    | 1518 ->
        "<SYNTAX ERROR>\n"
    | 1584 ->
        "<SYNTAX ERROR>\n"
    | 1528 ->
        "<SYNTAX ERROR>\n"
    | 1529 ->
        "<SYNTAX ERROR>\n"
    | 1530 ->
        "<SYNTAX ERROR>\n"
    | 1537 ->
        "<SYNTAX ERROR>\n"
    | 1538 ->
        "<SYNTAX ERROR>\n"
    | 1539 ->
        "<SYNTAX ERROR>\n"
    | 1531 ->
        "<SYNTAX ERROR>\n"
    | 1533 ->
        "<SYNTAX ERROR>\n"
    | 1534 ->
        "<SYNTAX ERROR>\n"
    | 1535 ->
        "<SYNTAX ERROR>\n"
    | 1536 ->
        "<SYNTAX ERROR>\n"
    | 1501 ->
        "<SYNTAX ERROR>\n"
    | 1885 ->
        "<SYNTAX ERROR>\n"
    | 1876 ->
        "<SYNTAX ERROR>\n"
    | 1874 ->
        "<SYNTAX ERROR>\n"
    | 1877 ->
        "<SYNTAX ERROR>\n"
    | 1878 ->
        "<SYNTAX ERROR>\n"
    | 1888 ->
        "<SYNTAX ERROR>\n"
    | 1889 ->
        "<SYNTAX ERROR>\n"
    | 603 ->
        "<SYNTAX ERROR>\n"
    | 2108 ->
        "<SYNTAX ERROR>\n"
    | 2114 ->
        "<SYNTAX ERROR>\n"
    | 2109 ->
        "<SYNTAX ERROR>\n"
    | 2110 ->
        "<SYNTAX ERROR>\n"
    | 2111 ->
        "<SYNTAX ERROR>\n"
    | 2113 ->
        "<SYNTAX ERROR>\n"
    | 2078 ->
        "<SYNTAX ERROR>\n"
    | 605 ->
        "<SYNTAX ERROR>\n"
    | 609 ->
        "<SYNTAX ERROR>\n"
    | 610 ->
        "<SYNTAX ERROR>\n"
    | 606 ->
        "<SYNTAX ERROR>\n"
    | 2338 ->
        "<SYNTAX ERROR>\n"
    | 2339 ->
        "<SYNTAX ERROR>\n"
    | 2342 ->
        "<SYNTAX ERROR>\n"
    | 2341 ->
        "<SYNTAX ERROR>\n"
    | 2079 ->
        "<SYNTAX ERROR>\n"
    | 2104 ->
        "<SYNTAX ERROR>\n"
    | 1520 ->
        "<SYNTAX ERROR>\n"
    | 1521 ->
        "<SYNTAX ERROR>\n"
    | 1522 ->
        "<SYNTAX ERROR>\n"
    | 1523 ->
        "<SYNTAX ERROR>\n"
    | 2080 ->
        "<SYNTAX ERROR>\n"
    | 2081 ->
        "<SYNTAX ERROR>\n"
    | 2082 ->
        "<SYNTAX ERROR>\n"
    | 2083 ->
        "<SYNTAX ERROR>\n"
    | 2085 ->
        "<SYNTAX ERROR>\n"
    | 2100 ->
        "<SYNTAX ERROR>\n"
    | 2101 ->
        "<SYNTAX ERROR>\n"
    | 2087 ->
        "<SYNTAX ERROR>\n"
    | 2088 ->
        "<SYNTAX ERROR>\n"
    | 2090 ->
        "<SYNTAX ERROR>\n"
    | 2091 ->
        "<SYNTAX ERROR>\n"
    | 2092 ->
        "<SYNTAX ERROR>\n"
    | 2095 ->
        "<SYNTAX ERROR>\n"
    | 2096 ->
        "<SYNTAX ERROR>\n"
    | 2097 ->
        "<SYNTAX ERROR>\n"
    | 2098 ->
        "<SYNTAX ERROR>\n"
    | 2094 ->
        "<SYNTAX ERROR>\n"
    | 2501 ->
        "Expecting \"}\" to finish the block\n"
    | 2235 ->
        "<SYNTAX ERROR>\n"
    | 1724 ->
        "<SYNTAX ERROR>\n"
    | 1157 ->
        "<SYNTAX ERROR>\n"
    | 1546 ->
        "<SYNTAX ERROR>\n"
    | 1543 ->
        "<SYNTAX ERROR>\n"
    | 1562 ->
        "<SYNTAX ERROR>\n"
    | 1563 ->
        "<SYNTAX ERROR>\n"
    | 1566 ->
        "<SYNTAX ERROR>\n"
    | 1199 ->
        "<SYNTAX ERROR>\n"
    | 1595 ->
        "<SYNTAX ERROR>\n"
    | 1598 ->
        "<SYNTAX ERROR>\n"
    | 1620 ->
        "<SYNTAX ERROR>\n"
    | 1599 ->
        "<SYNTAX ERROR>\n"
    | 1605 ->
        "<SYNTAX ERROR>\n"
    | 1606 ->
        "<SYNTAX ERROR>\n"
    | 1610 ->
        "<SYNTAX ERROR>\n"
    | 1611 ->
        "<SYNTAX ERROR>\n"
    | 1612 ->
        "<SYNTAX ERROR>\n"
    | 1601 ->
        "<SYNTAX ERROR>\n"
    | 1619 ->
        "<SYNTAX ERROR>\n"
    | 1616 ->
        "<SYNTAX ERROR>\n"
    | 1617 ->
        "<SYNTAX ERROR>\n"
    | 1618 ->
        "<SYNTAX ERROR>\n"
    | 1625 ->
        "<SYNTAX ERROR>\n"
    | 1626 ->
        "<SYNTAX ERROR>\n"
    | 1627 ->
        "<SYNTAX ERROR>\n"
    | 1347 ->
        "<SYNTAX ERROR>\n"
    | 1359 ->
        "<SYNTAX ERROR>\n"
    | 1585 ->
        "<SYNTAX ERROR>\n"
    | 1568 ->
        "<SYNTAX ERROR>\n"
    | 1569 ->
        "<SYNTAX ERROR>\n"
    | 1200 ->
        "<SYNTAX ERROR>\n"
    | 1381 ->
        "<SYNTAX ERROR>\n"
    | 1589 ->
        "<SYNTAX ERROR>\n"
    | 1201 ->
        "<SYNTAX ERROR>\n"
    | 1377 ->
        "<SYNTAX ERROR>\n"
    | 1378 ->
        "<SYNTAX ERROR>\n"
    | 1337 ->
        "<SYNTAX ERROR>\n"
    | 1339 ->
        "<SYNTAX ERROR>\n"
    | 1340 ->
        "<SYNTAX ERROR>\n"
    | 1365 ->
        "<SYNTAX ERROR>\n"
    | 1341 ->
        "<SYNTAX ERROR>\n"
    | 1342 ->
        "<SYNTAX ERROR>\n"
    | 1343 ->
        "<SYNTAX ERROR>\n"
    | 1567 ->
        "<SYNTAX ERROR>\n"
    | 1869 ->
        "<SYNTAX ERROR>\n"
    | 1870 ->
        "<SYNTAX ERROR>\n"
    | 1871 ->
        "<SYNTAX ERROR>\n"
    | 1573 ->
        "<SYNTAX ERROR>\n"
    | 1574 ->
        "<SYNTAX ERROR>\n"
    | 1575 ->
        "<SYNTAX ERROR>\n"
    | 1372 ->
        "<SYNTAX ERROR>\n"
    | 1373 ->
        "<SYNTAX ERROR>\n"
    | 1384 ->
        "<SYNTAX ERROR>\n"
    | 577 ->
        "<SYNTAX ERROR>\n"
    | 1061 ->
        "<SYNTAX ERROR>\n"
    | 931 ->
        "<SYNTAX ERROR>\n"
    | 875 ->
        "<SYNTAX ERROR>\n"
    | 876 ->
        "<SYNTAX ERROR>\n"
    | 1931 ->
        "<SYNTAX ERROR>\n"
    | 1933 ->
        "<SYNTAX ERROR>\n"
    | 1936 ->
        "<SYNTAX ERROR>\n"
    | 2253 ->
        "<SYNTAX ERROR>\n"
    | 924 ->
        "<SYNTAX ERROR>\n"
    | 1822 ->
        "<SYNTAX ERROR>\n"
    | 411 ->
        "<SYNTAX ERROR>\n"
    | 412 ->
        "<SYNTAX ERROR>\n"
    | 2461 ->
        "<SYNTAX ERROR>\n"
    | 2463 ->
        "<SYNTAX ERROR>\n"
    | 1934 ->
        "<SYNTAX ERROR>\n"
    | 2465 ->
        "<SYNTAX ERROR>\n"
    | 1939 ->
        "<SYNTAX ERROR>\n"
    | 1940 ->
        "<SYNTAX ERROR>\n"
    | 2467 ->
        "<SYNTAX ERROR>\n"
    | 2049 ->
        "<SYNTAX ERROR>\n"
    | 2024 ->
        "<SYNTAX ERROR>\n"
    | 2025 ->
        "<SYNTAX ERROR>\n"
    | 2026 ->
        "<SYNTAX ERROR>\n"
    | 2028 ->
        "<SYNTAX ERROR>\n"
    | 2031 ->
        "<SYNTAX ERROR>\n"
    | 2038 ->
        "<SYNTAX ERROR>\n"
    | 2041 ->
        "<SYNTAX ERROR>\n"
    | 2042 ->
        "<SYNTAX ERROR>\n"
    | 2256 ->
        "<SYNTAX ERROR>\n"
    | 2257 ->
        "<SYNTAX ERROR>\n"
    | 570 ->
        "<SYNTAX ERROR>\n"
    | 571 ->
        "<SYNTAX ERROR>\n"
    | 2372 ->
        "<SYNTAX ERROR>\n"
    | 2374 ->
        "<SYNTAX ERROR>\n"
    | 2029 ->
        "<SYNTAX ERROR>\n"
    | 2376 ->
        "<SYNTAX ERROR>\n"
    | 2034 ->
        "<SYNTAX ERROR>\n"
    | 2035 ->
        "<SYNTAX ERROR>\n"
    | 2378 ->
        "<SYNTAX ERROR>\n"
    | 2044 ->
        "<SYNTAX ERROR>\n"
    | 2045 ->
        "<SYNTAX ERROR>\n"
    | 1943 ->
        "<SYNTAX ERROR>\n"
    | 2019 ->
        "<SYNTAX ERROR>\n"
    | 2020 ->
        "<SYNTAX ERROR>\n"
    | 2021 ->
        "<SYNTAX ERROR>\n"
    | 2023 ->
        "<SYNTAX ERROR>\n"
    | 419 ->
        "<SYNTAX ERROR>\n"
    | 2205 ->
        "<SYNTAX ERROR>\n"
    | 422 ->
        "<SYNTAX ERROR>\n"
    | 436 ->
        "<SYNTAX ERROR>\n"
    | 425 ->
        "<SYNTAX ERROR>\n"
    | 2425 ->
        "<SYNTAX ERROR>\n"
    | 2429 ->
        "<SYNTAX ERROR>\n"
    | 2426 ->
        "<SYNTAX ERROR>\n"
    | 2427 ->
        "<SYNTAX ERROR>\n"
    | 427 ->
        "<SYNTAX ERROR>\n"
    | 429 ->
        "<SYNTAX ERROR>\n"
    | 431 ->
        "<SYNTAX ERROR>\n"
    | 433 ->
        "<SYNTAX ERROR>\n"
    | 2212 ->
        "<SYNTAX ERROR>\n"
    | 437 ->
        "<SYNTAX ERROR>\n"
    | 525 ->
        "<SYNTAX ERROR>\n"
    | 526 ->
        "<SYNTAX ERROR>\n"
    | 528 ->
        "<SYNTAX ERROR>\n"
    | 533 ->
        "<SYNTAX ERROR>\n"
    | 534 ->
        "<SYNTAX ERROR>\n"
    | 438 ->
        "<SYNTAX ERROR>\n"
    | 497 ->
        "<SYNTAX ERROR>\n"
    | 476 ->
        "<SYNTAX ERROR>\n"
    | 465 ->
        "<SYNTAX ERROR>\n"
    | 466 ->
        "<SYNTAX ERROR>\n"
    | 502 ->
        "<SYNTAX ERROR>\n"
    | 519 ->
        "<SYNTAX ERROR>\n"
    | 540 ->
        "<SYNTAX ERROR>\n"
    | 541 ->
        "<SYNTAX ERROR>\n"
    | 542 ->
        "<SYNTAX ERROR>\n"
    | 543 ->
        "<SYNTAX ERROR>\n"
    | 2213 ->
        "<SYNTAX ERROR>\n"
    | 2215 ->
        "<SYNTAX ERROR>\n"
    | 2204 ->
        "<SYNTAX ERROR>\n"
    | 1944 ->
        "<SYNTAX ERROR>\n"
    | 1945 ->
        "<SYNTAX ERROR>\n"
    | 1948 ->
        "<SYNTAX ERROR>\n"
    | 1949 ->
        "<SYNTAX ERROR>\n"
    | 1951 ->
        "<SYNTAX ERROR>\n"
    | 2015 ->
        "<SYNTAX ERROR>\n"
    | 2016 ->
        "<SYNTAX ERROR>\n"
    | 2017 ->
        "<SYNTAX ERROR>\n"
    | 2065 ->
        "<SYNTAX ERROR>\n"
    | 2066 ->
        "<SYNTAX ERROR>\n"
    | 2067 ->
        "<SYNTAX ERROR>\n"
    | 2068 ->
        "<SYNTAX ERROR>\n"
    | 2069 ->
        "<SYNTAX ERROR>\n"
    | 2070 ->
        "<SYNTAX ERROR>\n"
    | 2071 ->
        "<SYNTAX ERROR>\n"
    | 2018 ->
        "<SYNTAX ERROR>\n"
    | 2056 ->
        "<SYNTAX ERROR>\n"
    | 2057 ->
        "<SYNTAX ERROR>\n"
    | 2058 ->
        "<SYNTAX ERROR>\n"
    | 2059 ->
        "<SYNTAX ERROR>\n"
    | 2060 ->
        "<SYNTAX ERROR>\n"
    | 2073 ->
        "<SYNTAX ERROR>\n"
    | 2219 ->
        "<SYNTAX ERROR>\n"
    | 2220 ->
        "<SYNTAX ERROR>\n"
    | 2118 ->
        "<SYNTAX ERROR>\n"
    | 2121 ->
        "<SYNTAX ERROR>\n"
    | 2122 ->
        "<SYNTAX ERROR>\n"
    | 2123 ->
        "<SYNTAX ERROR>\n"
    | 2124 ->
        "<SYNTAX ERROR>\n"
    | 2125 ->
        "<SYNTAX ERROR>\n"
    | 2126 ->
        "<SYNTAX ERROR>\n"
    | 2130 ->
        "<SYNTAX ERROR>\n"
    | 2131 ->
        "<SYNTAX ERROR>\n"
    | 455 ->
        "<SYNTAX ERROR>\n"
    | 456 ->
        "<SYNTAX ERROR>\n"
    | 2132 ->
        "<SYNTAX ERROR>\n"
    | 2133 ->
        "<SYNTAX ERROR>\n"
    | 2134 ->
        "<SYNTAX ERROR>\n"
    | 441 ->
        "<SYNTAX ERROR>\n"
    | 2145 ->
        "<SYNTAX ERROR>\n"
    | 2168 ->
        "<SYNTAX ERROR>\n"
    | 2177 ->
        "<SYNTAX ERROR>\n"
    | 2169 ->
        "<SYNTAX ERROR>\n"
    | 2170 ->
        "<SYNTAX ERROR>\n"
    | 2172 ->
        "<SYNTAX ERROR>\n"
    | 2173 ->
        "<SYNTAX ERROR>\n"
    | 2174 ->
        "<SYNTAX ERROR>\n"
    | 2224 ->
        "<SYNTAX ERROR>\n"
    | 2225 ->
        "<SYNTAX ERROR>\n"
    | 2179 ->
        "<SYNTAX ERROR>\n"
    | 2188 ->
        "<SYNTAX ERROR>\n"
    | 2189 ->
        "<SYNTAX ERROR>\n"
    | 2190 ->
        "<SYNTAX ERROR>\n"
    | 2191 ->
        "<SYNTAX ERROR>\n"
    | 2192 ->
        "<SYNTAX ERROR>\n"
    | 2193 ->
        "<SYNTAX ERROR>\n"
    | 2194 ->
        "<SYNTAX ERROR>\n"
    | 2180 ->
        "<SYNTAX ERROR>\n"
    | 2181 ->
        "<SYNTAX ERROR>\n"
    | 2229 ->
        "<SYNTAX ERROR>\n"
    | 2230 ->
        "<SYNTAX ERROR>\n"
    | 2182 ->
        "<SYNTAX ERROR>\n"
    | 2183 ->
        "<SYNTAX ERROR>\n"
    | 2184 ->
        "<SYNTAX ERROR>\n"
    | 2185 ->
        "<SYNTAX ERROR>\n"
    | 559 ->
        "<SYNTAX ERROR>\n"
    | 560 ->
        "<SYNTAX ERROR>\n"
    | 564 ->
        "<SYNTAX ERROR>\n"
    | 565 ->
        "<SYNTAX ERROR>\n"
    | 2390 ->
        "<SYNTAX ERROR>\n"
    | 2392 ->
        "<SYNTAX ERROR>\n"
    | 2393 ->
        "<SYNTAX ERROR>\n"
    | 2394 ->
        "<SYNTAX ERROR>\n"
    | 877 ->
        "<SYNTAX ERROR>\n"
    | 878 ->
        "<SYNTAX ERROR>\n"
    | 880 ->
        "<SYNTAX ERROR>\n"
    | 881 ->
        "<SYNTAX ERROR>\n"
    | 1912 ->
        "<SYNTAX ERROR>\n"
    | 886 ->
        "<SYNTAX ERROR>\n"
    | 887 ->
        "<SYNTAX ERROR>\n"
    | 888 ->
        "<SYNTAX ERROR>\n"
    | 889 ->
        "<SYNTAX ERROR>\n"
    | 890 ->
        "<SYNTAX ERROR>\n"
    | 1911 ->
        "<SYNTAX ERROR>\n"
    | 882 ->
        "<SYNTAX ERROR>\n"
    | 883 ->
        "<SYNTAX ERROR>\n"
    | 884 ->
        "<SYNTAX ERROR>\n"
    | 885 ->
        "<SYNTAX ERROR>\n"
    | 1928 ->
        "<SYNTAX ERROR>\n"
    | 643 ->
        "<SYNTAX ERROR>\n"
    | 864 ->
        "<SYNTAX ERROR>\n"
    | 866 ->
        "<SYNTAX ERROR>\n"
    | 867 ->
        "<SYNTAX ERROR>\n"
    | 1921 ->
        "<SYNTAX ERROR>\n"
    | 1922 ->
        "<SYNTAX ERROR>\n"
    | 1923 ->
        "<SYNTAX ERROR>\n"
    | 1924 ->
        "<SYNTAX ERROR>\n"
    | 1925 ->
        "<SYNTAX ERROR>\n"
    | 1926 ->
        "<SYNTAX ERROR>\n"
    | 891 ->
        "<SYNTAX ERROR>\n"
    | 892 ->
        "<SYNTAX ERROR>\n"
    | 893 ->
        "<SYNTAX ERROR>\n"
    | 894 ->
        "<SYNTAX ERROR>\n"
    | 897 ->
        "<SYNTAX ERROR>\n"
    | 898 ->
        "<SYNTAX ERROR>\n"
    | 1064 ->
        "<SYNTAX ERROR>\n"
    | 1065 ->
        "<SYNTAX ERROR>\n"
    | 1066 ->
        "<SYNTAX ERROR>\n"
    | 1067 ->
        "<SYNTAX ERROR>\n"
    | 1068 ->
        "<SYNTAX ERROR>\n"
    | 1069 ->
        "<SYNTAX ERROR>\n"
    | 1073 ->
        "<SYNTAX ERROR>\n"
    | 1074 ->
        "<SYNTAX ERROR>\n"
    | 954 ->
        "<SYNTAX ERROR>\n"
    | 955 ->
        "<SYNTAX ERROR>\n"
    | 1075 ->
        "<SYNTAX ERROR>\n"
    | 1076 ->
        "<SYNTAX ERROR>\n"
    | 1077 ->
        "<SYNTAX ERROR>\n"
    | 952 ->
        "<SYNTAX ERROR>\n"
    | 1084 ->
        "<SYNTAX ERROR>\n"
    | 1103 ->
        "<SYNTAX ERROR>\n"
    | 1192 ->
        "<SYNTAX ERROR>\n"
    | 1105 ->
        "<SYNTAX ERROR>\n"
    | 1106 ->
        "<SYNTAX ERROR>\n"
    | 1108 ->
        "<SYNTAX ERROR>\n"
    | 1111 ->
        "<SYNTAX ERROR>\n"
    | 1743 ->
        "<SYNTAX ERROR>\n"
    | 1185 ->
        "<SYNTAX ERROR>\n"
    | 1186 ->
        "<SYNTAX ERROR>\n"
    | 1194 ->
        "<SYNTAX ERROR>\n"
    | 1702 ->
        "<SYNTAX ERROR>\n"
    | 1703 ->
        "<SYNTAX ERROR>\n"
    | 1704 ->
        "<SYNTAX ERROR>\n"
    | 1705 ->
        "<SYNTAX ERROR>\n"
    | 1666 ->
        "<SYNTAX ERROR>\n"
    | 1667 ->
        "<SYNTAX ERROR>\n"
    | 1706 ->
        "<SYNTAX ERROR>\n"
    | 1707 ->
        "<SYNTAX ERROR>\n"
    | 1709 ->
        "<SYNTAX ERROR>\n"
    | 1671 ->
        "<SYNTAX ERROR>\n"
    | 1672 ->
        "<SYNTAX ERROR>\n"
    | 1713 ->
        "<SYNTAX ERROR>\n"
    | 1710 ->
        "<SYNTAX ERROR>\n"
    | 1711 ->
        "<SYNTAX ERROR>\n"
    | 1113 ->
        "<SYNTAX ERROR>\n"
    | 1121 ->
        "<SYNTAX ERROR>\n"
    | 1122 ->
        "<SYNTAX ERROR>\n"
    | 1123 ->
        "<SYNTAX ERROR>\n"
    | 1124 ->
        "<SYNTAX ERROR>\n"
    | 1125 ->
        "<SYNTAX ERROR>\n"
    | 1127 ->
        "<SYNTAX ERROR>\n"
    | 1128 ->
        "<SYNTAX ERROR>\n"
    | 1129 ->
        "<SYNTAX ERROR>\n"
    | 1130 ->
        "<SYNTAX ERROR>\n"
    | 1136 ->
        "<SYNTAX ERROR>\n"
    | 1137 ->
        "<SYNTAX ERROR>\n"
    | 1139 ->
        "<SYNTAX ERROR>\n"
    | 1140 ->
        "<SYNTAX ERROR>\n"
    | 1144 ->
        "<SYNTAX ERROR>\n"
    | 1142 ->
        "<SYNTAX ERROR>\n"
    | 1145 ->
        "<SYNTAX ERROR>\n"
    | 1146 ->
        "<SYNTAX ERROR>\n"
    | 1117 ->
        "<SYNTAX ERROR>\n"
    | 1726 ->
        "<SYNTAX ERROR>\n"
    | 1727 ->
        "<SYNTAX ERROR>\n"
    | 1730 ->
        "<SYNTAX ERROR>\n"
    | 1114 ->
        "<SYNTAX ERROR>\n"
    | 1115 ->
        "<SYNTAX ERROR>\n"
    | 1120 ->
        "<SYNTAX ERROR>\n"
    | 1694 ->
        "<SYNTAX ERROR>\n"
    | 1195 ->
        "<SYNTAX ERROR>\n"
    | 1196 ->
        "<SYNTAX ERROR>\n"
    | 1197 ->
        "<SYNTAX ERROR>\n"
    | 1198 ->
        "<SYNTAX ERROR>\n"
    | 1632 ->
        "<SYNTAX ERROR>\n"
    | 1635 ->
        "<SYNTAX ERROR>\n"
    | 1653 ->
        "<SYNTAX ERROR>\n"
    | 1654 ->
        "<SYNTAX ERROR>\n"
    | 1190 ->
        "<SYNTAX ERROR>\n"
    | 1191 ->
        "<SYNTAX ERROR>\n"
    | 1663 ->
        "<SYNTAX ERROR>\n"
    | 1639 ->
        "<SYNTAX ERROR>\n"
    | 1643 ->
        "<SYNTAX ERROR>\n"
    | 1645 ->
        "<SYNTAX ERROR>\n"
    | 1646 ->
        "<SYNTAX ERROR>\n"
    | 1656 ->
        "<SYNTAX ERROR>\n"
    | 1647 ->
        "<SYNTAX ERROR>\n"
    | 1648 ->
        "<SYNTAX ERROR>\n"
    | 1649 ->
        "<SYNTAX ERROR>\n"
    | 1664 ->
        "<SYNTAX ERROR>\n"
    | 1681 ->
        "<SYNTAX ERROR>\n"
    | 1665 ->
        "<SYNTAX ERROR>\n"
    | 1690 ->
        "<SYNTAX ERROR>\n"
    | 1673 ->
        "<SYNTAX ERROR>\n"
    | 1691 ->
        "<SYNTAX ERROR>\n"
    | 1692 ->
        "<SYNTAX ERROR>\n"
    | 1680 ->
        "<SYNTAX ERROR>\n"
    | 1677 ->
        "<SYNTAX ERROR>\n"
    | 1678 ->
        "<SYNTAX ERROR>\n"
    | 1679 ->
        "<SYNTAX ERROR>\n"
    | 1686 ->
        "<SYNTAX ERROR>\n"
    | 1687 ->
        "<SYNTAX ERROR>\n"
    | 1688 ->
        "<SYNTAX ERROR>\n"
    | 584 ->
        "<SYNTAX ERROR>\n"
    | 151 ->
        "<SYNTAX ERROR>\n"
    | 905 ->
        "<SYNTAX ERROR>\n"
    | 2703 ->
        "<SYNTAX ERROR>\n"
    | 946 ->
        "<SYNTAX ERROR>\n"
    | 941 ->
        "<SYNTAX ERROR>\n"
    | 2705 ->
        "<SYNTAX ERROR>\n"
    | 1780 ->
        "<SYNTAX ERROR>\n"
    | 947 ->
        "<SYNTAX ERROR>\n"
    | 973 ->
        "<SYNTAX ERROR>\n"
    | 949 ->
        "<SYNTAX ERROR>\n"
    | 950 ->
        "<SYNTAX ERROR>\n"
    | 2704 ->
        "<SYNTAX ERROR>\n"
    | 980 ->
        "<SYNTAX ERROR>\n"
    | 981 ->
        "<SYNTAX ERROR>\n"
    | 983 ->
        "<SYNTAX ERROR>\n"
    | 1027 ->
        "<SYNTAX ERROR>\n"
    | 1028 ->
        "<SYNTAX ERROR>\n"
    | 1029 ->
        "<SYNTAX ERROR>\n"
    | 1030 ->
        "<SYNTAX ERROR>\n"
    | 1031 ->
        "<SYNTAX ERROR>\n"
    | 1032 ->
        "<SYNTAX ERROR>\n"
    | 1033 ->
        "<SYNTAX ERROR>\n"
    | 1034 ->
        "<SYNTAX ERROR>\n"
    | 1040 ->
        "<SYNTAX ERROR>\n"
    | 1042 ->
        "<SYNTAX ERROR>\n"
    | 1035 ->
        "<SYNTAX ERROR>\n"
    | 1036 ->
        "<SYNTAX ERROR>\n"
    | 1047 ->
        "<SYNTAX ERROR>\n"
    | 1048 ->
        "<SYNTAX ERROR>\n"
    | 1049 ->
        "<SYNTAX ERROR>\n"
    | 1050 ->
        "<SYNTAX ERROR>\n"
    | 1781 ->
        "<SYNTAX ERROR>\n"
    | 1782 ->
        "<SYNTAX ERROR>\n"
    | 809 ->
        "<SYNTAX ERROR>\n"
    | 1053 ->
        "<SYNTAX ERROR>\n"
    | 1054 ->
        "<SYNTAX ERROR>\n"
    | 1752 ->
        "<SYNTAX ERROR>\n"
    | 988 ->
        "<SYNTAX ERROR>\n"
    | 989 ->
        "<SYNTAX ERROR>\n"
    | 991 ->
        "<SYNTAX ERROR>\n"
    | 992 ->
        "<SYNTAX ERROR>\n"
    | 998 ->
        "<SYNTAX ERROR>\n"
    | 996 ->
        "<SYNTAX ERROR>\n"
    | 994 ->
        "<SYNTAX ERROR>\n"
    | 1011 ->
        "<SYNTAX ERROR>\n"
    | 1012 ->
        "<SYNTAX ERROR>\n"
    | 1004 ->
        "<SYNTAX ERROR>\n"
    | 1005 ->
        "<SYNTAX ERROR>\n"
    | 1009 ->
        "<SYNTAX ERROR>\n"
    | 76 ->
        "<SYNTAX ERROR>\n"
    | 1008 ->
        "<SYNTAX ERROR>\n"
    | 1006 ->
        "<SYNTAX ERROR>\n"
    | 117 ->
        "<SYNTAX ERROR>\n"
    | 256 ->
        "<SYNTAX ERROR>\n"
    | 1015 ->
        "<SYNTAX ERROR>\n"
    | 1016 ->
        "<SYNTAX ERROR>\n"
    | 257 ->
        "<SYNTAX ERROR>\n"
    | 258 ->
        "<SYNTAX ERROR>\n"
    | 409 ->
        "<SYNTAX ERROR>\n"
    | 410 ->
        "<SYNTAX ERROR>\n"
    | 2469 ->
        "<SYNTAX ERROR>\n"
    | 566 ->
        "<SYNTAX ERROR>\n"
    | 2384 ->
        "<SYNTAX ERROR>\n"
    | 2385 ->
        "<SYNTAX ERROR>\n"
    | 2386 ->
        "<SYNTAX ERROR>\n"
    | 2387 ->
        "<SYNTAX ERROR>\n"
    | 2388 ->
        "<SYNTAX ERROR>\n"
    | 2389 ->
        "<SYNTAX ERROR>\n"
    | 1962 ->
        "<SYNTAX ERROR>\n"
    | 1963 ->
        "<SYNTAX ERROR>\n"
    | 1965 ->
        "<SYNTAX ERROR>\n"
    | 1970 ->
        "<SYNTAX ERROR>\n"
    | 1968 ->
        "<SYNTAX ERROR>\n"
    | 1966 ->
        "<SYNTAX ERROR>\n"
    | 1991 ->
        "<SYNTAX ERROR>\n"
    | 1992 ->
        "<SYNTAX ERROR>\n"
    | 1971 ->
        "<SYNTAX ERROR>\n"
    | 1972 ->
        "<SYNTAX ERROR>\n"
    | 1989 ->
        "<SYNTAX ERROR>\n"
    | 1974 ->
        "<SYNTAX ERROR>\n"
    | 1988 ->
        "<SYNTAX ERROR>\n"
    | 1973 ->
        "<SYNTAX ERROR>\n"
    | 1975 ->
        "<SYNTAX ERROR>\n"
    | 1976 ->
        "<SYNTAX ERROR>\n"
    | 1979 ->
        "<SYNTAX ERROR>\n"
    | 567 ->
        "<SYNTAX ERROR>\n"
    | 660 ->
        "<SYNTAX ERROR>\n"
    | 1996 ->
        "<SYNTAX ERROR>\n"
    | 1997 ->
        "<SYNTAX ERROR>\n"
    | 661 ->
        "<SYNTAX ERROR>\n"
    | 662 ->
        "<SYNTAX ERROR>\n"
    | 568 ->
        "<SYNTAX ERROR>\n"
    | 569 ->
        "<SYNTAX ERROR>\n"
    | 2380 ->
        "<SYNTAX ERROR>\n"
    | 1952 ->
        "<SYNTAX ERROR>\n"
    | 2006 ->
        "<SYNTAX ERROR>\n"
    | 2007 ->
        "<SYNTAX ERROR>\n"
    | 2008 ->
        "<SYNTAX ERROR>\n"
    | 2009 ->
        "<SYNTAX ERROR>\n"
    | 2010 ->
        "<SYNTAX ERROR>\n"
    | 2011 ->
        "<SYNTAX ERROR>\n"
    | 1960 ->
        "<SYNTAX ERROR>\n"
    | 2381 ->
        "<SYNTAX ERROR>\n"
    | 1953 ->
        "<SYNTAX ERROR>\n"
    | 939 ->
        "<SYNTAX ERROR>\n"
    | 1774 ->
        "<SYNTAX ERROR>\n"
    | 1773 ->
        "<SYNTAX ERROR>\n"
    | 1755 ->
        "<SYNTAX ERROR>\n"
    | 1756 ->
        "<SYNTAX ERROR>\n"
    | 1757 ->
        "<SYNTAX ERROR>\n"
    | 1758 ->
        "<SYNTAX ERROR>\n"
    | 1759 ->
        "<SYNTAX ERROR>\n"
    | 1762 ->
        "<SYNTAX ERROR>\n"
    | 951 ->
        "<SYNTAX ERROR>\n"
    | 1765 ->
        "<SYNTAX ERROR>\n"
    | 1766 ->
        "<SYNTAX ERROR>\n"
    | 1786 ->
        "<SYNTAX ERROR>\n"
    | 1768 ->
        "<SYNTAX ERROR>\n"
    | 1693 ->
        "<SYNTAX ERROR>\n"
    | 1769 ->
        "<SYNTAX ERROR>\n"
    | 1787 ->
        "<SYNTAX ERROR>\n"
    | 1788 ->
        "<SYNTAX ERROR>\n"
    | 2710 ->
        "<SYNTAX ERROR>\n"
    | 2712 ->
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
    | 2618 ->
        "<SYNTAX ERROR>\n"
    | 110 ->
        "<SYNTAX ERROR>\n"
    | 2616 ->
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
    | 2613 ->
        "<SYNTAX ERROR>\n"
    | 2614 ->
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
    | 2605 ->
        "<SYNTAX ERROR>\n"
    | 2606 ->
        "<SYNTAX ERROR>\n"
    | 2607 ->
        "<SYNTAX ERROR>\n"
    | 2609 ->
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
    | 508 ->
        "<SYNTAX ERROR>\n"
    | 507 ->
        "<SYNTAX ERROR>\n"
    | 510 ->
        "<SYNTAX ERROR>\n"
    | 295 ->
        "<SYNTAX ERROR>\n"
    | 137 ->
        "<SYNTAX ERROR>\n"
    | 143 ->
        "<SYNTAX ERROR>\n"
    | 2598 ->
        "<SYNTAX ERROR>\n"
    | 2600 ->
        "<SYNTAX ERROR>\n"
    | 2601 ->
        "<SYNTAX ERROR>\n"
    | 157 ->
        "<SYNTAX ERROR>\n"
    | 145 ->
        "<SYNTAX ERROR>\n"
    | 146 ->
        "<SYNTAX ERROR>\n"
    | 2596 ->
        "<SYNTAX ERROR>\n"
    | 148 ->
        "<SYNTAX ERROR>\n"
    | 149 ->
        "<SYNTAX ERROR>\n"
    | 2592 ->
        "<SYNTAX ERROR>\n"
    | 2593 ->
        "<SYNTAX ERROR>\n"
    | 2594 ->
        "<SYNTAX ERROR>\n"
    | 150 ->
        "<SYNTAX ERROR>\n"
    | 155 ->
        "<SYNTAX ERROR>\n"
    | 2590 ->
        "<SYNTAX ERROR>\n"
    | 2586 ->
        "<SYNTAX ERROR>\n"
    | 2583 ->
        "<SYNTAX ERROR>\n"
    | 2714 ->
        "<SYNTAX ERROR>\n"
    | 2716 ->
        "<SYNTAX ERROR>\n"
    | 2718 ->
        "<SYNTAX ERROR>\n"
    | 2719 ->
        "<SYNTAX ERROR>\n"
    | 804 ->
        "<SYNTAX ERROR>\n"
    | 806 ->
        "<SYNTAX ERROR>\n"
    | 817 ->
        "<SYNTAX ERROR>\n"
    | 807 ->
        "<SYNTAX ERROR>\n"
    | 799 ->
        "<SYNTAX ERROR>\n"
    | 644 ->
        "<SYNTAX ERROR>\n"
    | 608 ->
        "<SYNTAX ERROR>\n"
    | 780 ->
        "<SYNTAX ERROR>\n"
    | 193 ->
        "<SYNTAX ERROR>\n"
    | 198 ->
        "<SYNTAX ERROR>\n"
    | 204 ->
        "<SYNTAX ERROR>\n"
    | 210 ->
        "<SYNTAX ERROR>\n"
    | 824 ->
        "<SYNTAX ERROR>\n"
    | 825 ->
        "<SYNTAX ERROR>\n"
    | 854 ->
        "<SYNTAX ERROR>\n"
    | 815 ->
        "<SYNTAX ERROR>\n"
    | 743 ->
        "<SYNTAX ERROR>\n"
    | 730 ->
        "<SYNTAX ERROR>\n"
    | 732 ->
        "<SYNTAX ERROR>\n"
    | 859 ->
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
    | 2480 ->
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
    | 750 ->
        "<SYNTAX ERROR>\n"
    | 733 ->
        "<SYNTAX ERROR>\n"
    | 735 ->
        "<SYNTAX ERROR>\n"
    | 720 ->
        "<SYNTAX ERROR>\n"
    | 689 ->
        "<SYNTAX ERROR>\n"
    | 710 ->
        "<SYNTAX ERROR>\n"
    | 701 ->
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
    | 2484 ->
        "<SYNTAX ERROR>\n"
    | 2485 ->
        "<SYNTAX ERROR>\n"
    | 234 ->
        "<SYNTAX ERROR>\n"
    | 239 ->
        "<SYNTAX ERROR>\n"
    | 646 ->
        "<SYNTAX ERROR>\n"
    | 652 ->
        "<SYNTAX ERROR>\n"
    | 741 ->
        "<SYNTAX ERROR>\n"
    | 830 ->
        "<SYNTAX ERROR>\n"
    | 653 ->
        "<SYNTAX ERROR>\n"
    | 654 ->
        "<SYNTAX ERROR>\n"
    | 656 ->
        "<SYNTAX ERROR>\n"
    | 846 ->
        "<SYNTAX ERROR>\n"
    | 847 ->
        "<SYNTAX ERROR>\n"
    | 848 ->
        "<SYNTAX ERROR>\n"
    | 849 ->
        "<SYNTAX ERROR>\n"
    | 850 ->
        "<SYNTAX ERROR>\n"
    | 851 ->
        "<SYNTAX ERROR>\n"
    | 668 ->
        "<SYNTAX ERROR>\n"
    | 843 ->
        "<SYNTAX ERROR>\n"
    | 671 ->
        "<SYNTAX ERROR>\n"
    | 838 ->
        "<SYNTAX ERROR>\n"
    | 672 ->
        "<SYNTAX ERROR>\n"
    | 677 ->
        "<SYNTAX ERROR>\n"
    | 688 ->
        "<SYNTAX ERROR>\n"
    | 696 ->
        "<SYNTAX ERROR>\n"
    | 704 ->
        "<SYNTAX ERROR>\n"
    | 2487 ->
        "<SYNTAX ERROR>\n"
    | 2488 ->
        "<SYNTAX ERROR>\n"
    | 2489 ->
        "<SYNTAX ERROR>\n"
    | 2490 ->
        "<SYNTAX ERROR>\n"
    | 2491 ->
        "<SYNTAX ERROR>\n"
    | 2492 ->
        "<SYNTAX ERROR>\n"
    | 744 ->
        "<SYNTAX ERROR>\n"
    | 755 ->
        "<SYNTAX ERROR>\n"
    | 758 ->
        "<SYNTAX ERROR>\n"
    | 748 ->
        "<SYNTAX ERROR>\n"
    | 749 ->
        "<SYNTAX ERROR>\n"
    | 669 ->
        "<SYNTAX ERROR>\n"
    | 670 ->
        "<SYNTAX ERROR>\n"
    | 759 ->
        "<SYNTAX ERROR>\n"
    | 762 ->
        "<SYNTAX ERROR>\n"
    | 770 ->
        "<SYNTAX ERROR>\n"
    | 766 ->
        "<SYNTAX ERROR>\n"
    | 769 ->
        "<SYNTAX ERROR>\n"
    | 767 ->
        "Expecting a valid list identifier\n"
    | 768 ->
        "<SYNTAX ERROR>\n"
    | 771 ->
        "<SYNTAX ERROR>\n"
    | 674 ->
        "<SYNTAX ERROR>\n"
    | 675 ->
        "<SYNTAX ERROR>\n"
    | 686 ->
        "<SYNTAX ERROR>\n"
    | 681 ->
        "<SYNTAX ERROR>\n"
    | 682 ->
        "<SYNTAX ERROR>\n"
    | 772 ->
        "<SYNTAX ERROR>\n"
    | 687 ->
        "<SYNTAX ERROR>\n"
    | 836 ->
        "<SYNTAX ERROR>\n"
    | 775 ->
        "<SYNTAX ERROR>\n"
    | 791 ->
        "<SYNTAX ERROR>\n"
    | 793 ->
        "<SYNTAX ERROR>\n"
    | 2722 ->
        "<SYNTAX ERROR>\n"
    | 2736 ->
        "<SYNTAX ERROR>\n"
    | 2723 ->
        "<SYNTAX ERROR>\n"
    | 2724 ->
        "<SYNTAX ERROR>\n"
    | 2730 ->
        "<SYNTAX ERROR>\n"
    | 2731 ->
        "<SYNTAX ERROR>\n"
    | 2734 ->
        "<SYNTAX ERROR>\n"
    | 2739 ->
        "<SYNTAX ERROR>\n"
    | 2746 ->
        "<SYNTAX ERROR>\n"
    | 2745 ->
        "<SYNTAX ERROR>\n"
    | 2742 ->
        "<SYNTAX ERROR>\n"
    | 2743 ->
        "<SYNTAX ERROR>\n"
    | _ ->
        raise Not_found
