
(* This file was auto-generated based on "src/reason_parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "<SYNTAX ERROR>\n"
    | 2 ->
        "<SYNTAX ERROR>\n"
    | 2704 ->
        "<SYNTAX ERROR>\n"
    | 590 ->
        "Expecting one of the following:\n  - an identifier to access a member of an object\n  - \"[\" + expression + \"]\" to access an element of a list\n  - \"(\" + expression + \")\"\n  - \"{\" + expression + \"}\"\n"
    | 591 ->
        "Expecting an expression\n"
    | 2365 ->
        "Expecting one of the following:\n  - an infix operation to connect two expressions\n  - \")\" to close the block\n"
    | 2367 ->
        "Expecting an expression\n"
    | 2368 ->
        "Expecting one of the following:\n  - an infix operation to connect two expressions\n  - \"}\" to close the block\n"
    | 2370 ->
        "Expecting an expression\n"
    | 2371 ->
        "Expecting one of the following:\n  - an infix operation to connect two expressions\n  - \"}\" to close the block\n"
    | 1229 ->
        "Expecting an expression\n"
    | 588 ->
        "Expecting an identifier\n"
    | 1162 ->
        "Expecting a structure item\n"
    | 2709 ->
        "Invalid token\n"
    | 1283 ->
        "Expecting an expression\n"
    | 1284 ->
        "Expecting one of the following:\n  - The continuation of the previous expression\n  - \":\" to start the next expression\n"
    | 1285 ->
        "Expecting an expression\n"
    | 1244 ->
        "Expecting an expression\n"
    | 1250 ->
        "Expecting an expression\n"
    | 1252 ->
        "Expecting an expression\n"
    | 1246 ->
        "Expecting an expression\n"
    | 1254 ->
        "Expecting an expression\n"
    | 1256 ->
        "Expecting an expression\n"
    | 1258 ->
        "Expecting an expression\n"
    | 15 ->
        "Expecting one of the following:\n  - \")\" to form a unit value \"()\"\n  - \"module\" to start a module expression\n  - an expression\n  - an operator to denote the prefix form of an operator\n"
    | 1260 ->
        "Expecting an expression\n"
    | 1266 ->
        "Expecting an expression\n"
    | 1268 ->
        "Expecting an expression\n"
    | 2482 ->
        "Expecting \"]\"\n"
    | 403 ->
        "Expecting an attributed id\n"
    | 2589 ->
        "Expecting \"]\"\n"
    | 161 ->
        "Expecting an attribute id\n"
    | 1231 ->
        "Expecting an expression\n"
    | 1248 ->
        "Expecting an expression\n"
    | 1262 ->
        "Expecting an expression\n"
    | 1264 ->
        "Expecting an expression\n"
    | 1270 ->
        "Expecting an expression\n"
    | 1272 ->
        "Expecting an expression\n"
    | 871 ->
        "<SYNTAX ERROR>\n"
    | 872 ->
        "<SYNTAX ERROR>\n"
    | 2269 ->
        "<SYNTAX ERROR>\n"
    | 873 ->
        "<SYNTAX ERROR>\n"
    | 875 ->
        "<SYNTAX ERROR>\n"
    | 2265 ->
        "<SYNTAX ERROR>\n"
    | 2267 ->
        "<SYNTAX ERROR>\n"
    | 2272 ->
        "<SYNTAX ERROR>\n"
    | 2273 ->
        "<SYNTAX ERROR>\n"
    | 2277 ->
        "<SYNTAX ERROR>\n"
    | 2287 ->
        "<SYNTAX ERROR>\n"
    | 2292 ->
        "<SYNTAX ERROR>\n"
    | 1888 ->
        "<SYNTAX ERROR>\n"
    | 1280 ->
        "<SYNTAX ERROR>\n"
    | 1274 ->
        "<SYNTAX ERROR>\n"
    | 1276 ->
        "<SYNTAX ERROR>\n"
    | 1278 ->
        "<SYNTAX ERROR>\n"
    | 75 ->
        "<SYNTAX ERROR>\n"
    | 978 ->
        "<SYNTAX ERROR>\n"
    | 979 ->
        "<SYNTAX ERROR>\n"
    | 104 ->
        "Expecting one of the following:\n  - \"=\" to start the body of the type declaration\n  - \"constraint\" to add constraints to the type declaration\n  - \";\" to finish type declaratoin\n  - \"+=\" to form a string type extension\n  - \"and\" to declare another type\n"
    | 91 ->
        "<SYNTAX ERROR>\n"
    | 2689 ->
        "<SYNTAX ERROR>\n"
    | 2693 ->
        "<SYNTAX ERROR>\n"
    | 2690 ->
        "<SYNTAX ERROR>\n"
    | 2691 ->
        "<SYNTAX ERROR>\n"
    | 95 ->
        "<SYNTAX ERROR>\n"
    | 97 ->
        "<SYNTAX ERROR>\n"
    | 99 ->
        "<SYNTAX ERROR>\n"
    | 101 ->
        "<SYNTAX ERROR>\n"
    | 1172 ->
        "<SYNTAX ERROR>\n"
    | 105 ->
        "<SYNTAX ERROR>\n"
    | 2674 ->
        "<SYNTAX ERROR>\n"
    | 2675 ->
        "<SYNTAX ERROR>\n"
    | 2665 ->
        "<SYNTAX ERROR>\n"
    | 2677 ->
        "<SYNTAX ERROR>\n"
    | 2682 ->
        "<SYNTAX ERROR>\n"
    | 2683 ->
        "<SYNTAX ERROR>\n"
    | 2644 ->
        "<SYNTAX ERROR>\n"
    | 107 ->
        "<SYNTAX ERROR>\n"
    | 2633 ->
        "<SYNTAX ERROR>\n"
    | 2634 ->
        "<SYNTAX ERROR>\n"
    | 2670 ->
        "Expecting at least one type field definition in the form of:\n  <field name> : <type>\n"
    | 504 ->
        "Expecting a type field definition in the form of:\n  <field name> : <type>\n"
    | 505 ->
        "Expecting \":\"\n"
    | 506 ->
        "Expecting a type name describing this field\n"
    | 2671 ->
        "Expecting one of the following:\n  - \",\" to finish current type field\n  - \"}\" to finish type definition\n"
    | 520 ->
        "Expecting one of the following:\n  - another type field definition\n  - \"}\" to finish entire type definition\n"
    | 1177 ->
        "<SYNTAX ERROR>\n"
    | 2662 ->
        "<SYNTAX ERROR>\n"
    | 1000 ->
        "<SYNTAX ERROR>\n"
    | 1001 ->
        "<SYNTAX ERROR>\n"
    | 1002 ->
        "<SYNTAX ERROR>\n"
    | 1173 ->
        "<SYNTAX ERROR>\n"
    | 1175 ->
        "<SYNTAX ERROR>\n"
    | 163 ->
        "<SYNTAX ERROR>\n"
    | 2584 ->
        "<SYNTAX ERROR>\n"
    | 2583 ->
        "<SYNTAX ERROR>\n"
    | 2586 ->
        "<SYNTAX ERROR>\n"
    | 2525 ->
        "<SYNTAX ERROR>\n"
    | 2526 ->
        "<SYNTAX ERROR>\n"
    | 2527 ->
        "Expecting a sequence item\n"
    | 1922 ->
        "Expecting one of the following:\n  - \"|\" to open the next pattern\n  - \"=>\" to start the body of the matched pattern\n  - \"when\" to start a contitional guard for the previous pattern\n"
    | 2558 ->
        "Expecting the body of the matched pattern\n"
    | 2587 ->
        "Expecting one of the following:\n  - \"}\" to finish the block\n  - \"|\" to start another pattern matching case\n"
    | 2541 ->
        "<SYNTAX ERROR>\n"
    | 2528 ->
        "<SYNTAX ERROR>\n"
    | 2529 ->
        "<SYNTAX ERROR>\n"
    | 2532 ->
        "<SYNTAX ERROR>\n"
    | 2533 ->
        "<SYNTAX ERROR>\n"
    | 2530 ->
        "<SYNTAX ERROR>\n"
    | 2551 ->
        "<SYNTAX ERROR>\n"
    | 2552 ->
        "<SYNTAX ERROR>\n"
    | 2555 ->
        "<SYNTAX ERROR>\n"
    | 2554 ->
        "<SYNTAX ERROR>\n"
    | 1921 ->
        "Expecting a match case\n"
    | 910 ->
        "<SYNTAX ERROR>\n"
    | 124 ->
        "<SYNTAX ERROR>\n"
    | 125 ->
        "<SYNTAX ERROR>\n"
    | 911 ->
        "<SYNTAX ERROR>\n"
    | 1892 ->
        "<SYNTAX ERROR>\n"
    | 1895 ->
        "<SYNTAX ERROR>\n"
    | 1909 ->
        "<SYNTAX ERROR>\n"
    | 1897 ->
        "<SYNTAX ERROR>\n"
    | 1898 ->
        "<SYNTAX ERROR>\n"
    | 1901 ->
        "<SYNTAX ERROR>\n"
    | 1903 ->
        "<SYNTAX ERROR>\n"
    | 1904 ->
        "<SYNTAX ERROR>\n"
    | 1906 ->
        "<SYNTAX ERROR>\n"
    | 168 ->
        "<SYNTAX ERROR>\n"
    | 2567 ->
        "<SYNTAX ERROR>\n"
    | 2568 ->
        "<SYNTAX ERROR>\n"
    | 1161 ->
        "Incomplete module item, forgetting a \";\"?\n"
    | 1216 ->
        "<SYNTAX ERROR>\n"
    | 1219 ->
        "<SYNTAX ERROR>\n"
    | 6 ->
        "<SYNTAX ERROR>\n"
    | 1241 ->
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
    | 908 ->
        "<SYNTAX ERROR>\n"
    | 557 ->
        "<SYNTAX ERROR>\n"
    | 16 ->
        "<SYNTAX ERROR>\n"
    | 2701 ->
        "<SYNTAX ERROR>\n"
    | 627 ->
        "<SYNTAX ERROR>\n"
    | 628 ->
        "<SYNTAX ERROR>\n"
    | 2320 ->
        "<SYNTAX ERROR>\n"
    | 2322 ->
        "<SYNTAX ERROR>\n"
    | 2323 ->
        "<SYNTAX ERROR>\n"
    | 2325 ->
        "<SYNTAX ERROR>\n"
    | 2326 ->
        "<SYNTAX ERROR>\n"
    | 1419 ->
        "<SYNTAX ERROR>\n"
    | 624 ->
        "<SYNTAX ERROR>\n"
    | 1477 ->
        "<SYNTAX ERROR>\n"
    | 1478 ->
        "<SYNTAX ERROR>\n"
    | 1479 ->
        "<SYNTAX ERROR>\n"
    | 1434 ->
        "<SYNTAX ERROR>\n"
    | 1440 ->
        "<SYNTAX ERROR>\n"
    | 1442 ->
        "<SYNTAX ERROR>\n"
    | 1436 ->
        "<SYNTAX ERROR>\n"
    | 1444 ->
        "<SYNTAX ERROR>\n"
    | 1446 ->
        "<SYNTAX ERROR>\n"
    | 1448 ->
        "<SYNTAX ERROR>\n"
    | 184 ->
        "<SYNTAX ERROR>\n"
    | 1450 ->
        "<SYNTAX ERROR>\n"
    | 1456 ->
        "<SYNTAX ERROR>\n"
    | 1458 ->
        "<SYNTAX ERROR>\n"
    | 2485 ->
        "<SYNTAX ERROR>\n"
    | 340 ->
        "<SYNTAX ERROR>\n"
    | 1421 ->
        "<SYNTAX ERROR>\n"
    | 1438 ->
        "<SYNTAX ERROR>\n"
    | 1452 ->
        "<SYNTAX ERROR>\n"
    | 1454 ->
        "<SYNTAX ERROR>\n"
    | 1460 ->
        "<SYNTAX ERROR>\n"
    | 1462 ->
        "<SYNTAX ERROR>\n"
    | 921 ->
        "<SYNTAX ERROR>\n"
    | 922 ->
        "<SYNTAX ERROR>\n"
    | 1832 ->
        "<SYNTAX ERROR>\n"
    | 923 ->
        "<SYNTAX ERROR>\n"
    | 924 ->
        "<SYNTAX ERROR>\n"
    | 1825 ->
        "<SYNTAX ERROR>\n"
    | 1827 ->
        "<SYNTAX ERROR>\n"
    | 1835 ->
        "<SYNTAX ERROR>\n"
    | 1837 ->
        "<SYNTAX ERROR>\n"
    | 1852 ->
        "<SYNTAX ERROR>\n"
    | 1864 ->
        "<SYNTAX ERROR>\n"
    | 1869 ->
        "<SYNTAX ERROR>\n"
    | 2418 ->
        "<SYNTAX ERROR>\n"
    | 2423 ->
        "<SYNTAX ERROR>\n"
    | 2420 ->
        "<SYNTAX ERROR>\n"
    | 1299 ->
        "<SYNTAX ERROR>\n"
    | 2417 ->
        "<SYNTAX ERROR>\n"
    | 1470 ->
        "<SYNTAX ERROR>\n"
    | 1497 ->
        "<SYNTAX ERROR>\n"
    | 1318 ->
        "<SYNTAX ERROR>\n"
    | 1464 ->
        "<SYNTAX ERROR>\n"
    | 1466 ->
        "<SYNTAX ERROR>\n"
    | 1468 ->
        "<SYNTAX ERROR>\n"
    | 166 ->
        "<SYNTAX ERROR>\n"
    | 2573 ->
        "<SYNTAX ERROR>\n"
    | 2572 ->
        "<SYNTAX ERROR>\n"
    | 2575 ->
        "<SYNTAX ERROR>\n"
    | 1409 ->
        "<SYNTAX ERROR>\n"
    | 1410 ->
        "<SYNTAX ERROR>\n"
    | 1472 ->
        "<SYNTAX ERROR>\n"
    | 1475 ->
        "<SYNTAX ERROR>\n"
    | 1493 ->
        "<SYNTAX ERROR>\n"
    | 1481 ->
        "<SYNTAX ERROR>\n"
    | 1482 ->
        "<SYNTAX ERROR>\n"
    | 1485 ->
        "<SYNTAX ERROR>\n"
    | 1487 ->
        "<SYNTAX ERROR>\n"
    | 1488 ->
        "<SYNTAX ERROR>\n"
    | 1490 ->
        "<SYNTAX ERROR>\n"
    | 173 ->
        "<SYNTAX ERROR>\n"
    | 2519 ->
        "<SYNTAX ERROR>\n"
    | 2520 ->
        "<SYNTAX ERROR>\n"
    | 1350 ->
        "<SYNTAX ERROR>\n"
    | 1358 ->
        "<SYNTAX ERROR>\n"
    | 811 ->
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
    | 2404 ->
        "<SYNTAX ERROR>\n"
    | 2406 ->
        "<SYNTAX ERROR>\n"
    | 2408 ->
        "<SYNTAX ERROR>\n"
    | 1829 ->
        "<SYNTAX ERROR>\n"
    | 1830 ->
        "<SYNTAX ERROR>\n"
    | 415 ->
        "Expecting one of the following:\n  - \")\" to form a unit value \"()\"\n  - \"module\" to start a module expression\n  - an expression\n  - an operator to denote the prefix form of an operator\n"
    | 2462 ->
        "<SYNTAX ERROR>\n"
    | 739 ->
        "<SYNTAX ERROR>\n"
    | 418 ->
        "Expecting a module expression\n"
    | 2449 ->
        "<SYNTAX ERROR>\n"
    | 2451 ->
        "<SYNTAX ERROR>\n"
    | 2453 ->
        "<SYNTAX ERROR>\n"
    | 2455 ->
        "<SYNTAX ERROR>\n"
    | 2456 ->
        "<SYNTAX ERROR>\n"
    | 2457 ->
        "<SYNTAX ERROR>\n"
    | 2458 ->
        "<SYNTAX ERROR>\n"
    | 2459 ->
        "<SYNTAX ERROR>\n"
    | 2460 ->
        "<SYNTAX ERROR>\n"
    | 1417 ->
        "<SYNTAX ERROR>\n"
    | 2507 ->
        "<SYNTAX ERROR>\n"
    | 189 ->
        "<SYNTAX ERROR>\n"
    | 573 ->
        "<SYNTAX ERROR>\n"
    | 2377 ->
        "<SYNTAX ERROR>\n"
    | 574 ->
        "<SYNTAX ERROR>\n"
    | 1859 ->
        "<SYNTAX ERROR>\n"
    | 1858 ->
        "<SYNTAX ERROR>\n"
    | 1853 ->
        "<SYNTAX ERROR>\n"
    | 1854 ->
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
    | 2317 ->
        "<SYNTAX ERROR>\n"
    | 2303 ->
        "<SYNTAX ERROR>\n"
    | 638 ->
        "<SYNTAX ERROR>\n"
    | 639 ->
        "<SYNTAX ERROR>\n"
    | 640 ->
        "<SYNTAX ERROR>\n"
    | 641 ->
        "<SYNTAX ERROR>\n"
    | 2297 ->
        "<SYNTAX ERROR>\n"
    | 2298 ->
        "<SYNTAX ERROR>\n"
    | 634 ->
        "<SYNTAX ERROR>\n"
    | 635 ->
        "<SYNTAX ERROR>\n"
    | 636 ->
        "<SYNTAX ERROR>\n"
    | 637 ->
        "<SYNTAX ERROR>\n"
    | 2310 ->
        "<SYNTAX ERROR>\n"
    | 2311 ->
        "<SYNTAX ERROR>\n"
    | 2312 ->
        "<SYNTAX ERROR>\n"
    | 2313 ->
        "<SYNTAX ERROR>\n"
    | 2314 ->
        "<SYNTAX ERROR>\n"
    | 2315 ->
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
    | 917 ->
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
    | 1831 ->
        "<SYNTAX ERROR>\n"
    | 619 ->
        "<SYNTAX ERROR>\n"
    | 1405 ->
        "<SYNTAX ERROR>\n"
    | 1227 ->
        "<SYNTAX ERROR>\n"
    | 926 ->
        "Incomplete let binding\n"
    | 1328 ->
        "<SYNTAX ERROR>\n"
    | 1334 ->
        "<SYNTAX ERROR>\n"
    | 1329 ->
        "<SYNTAX ERROR>\n"
    | 1330 ->
        "<SYNTAX ERROR>\n"
    | 1331 ->
        "<SYNTAX ERROR>\n"
    | 1333 ->
        "<SYNTAX ERROR>\n"
    | 1203 ->
        "<SYNTAX ERROR>\n"
    | 927 ->
        "<SYNTAX ERROR>\n"
    | 928 ->
        "<SYNTAX ERROR>\n"
    | 1807 ->
        "<SYNTAX ERROR>\n"
    | 1808 ->
        "<SYNTAX ERROR>\n"
    | 1809 ->
        "<SYNTAX ERROR>\n"
    | 1810 ->
        "<SYNTAX ERROR>\n"
    | 1814 ->
        "<SYNTAX ERROR>\n"
    | 1811 ->
        "<SYNTAX ERROR>\n"
    | 1812 ->
        "<SYNTAX ERROR>\n"
    | 1813 ->
        "<SYNTAX ERROR>\n"
    | 929 ->
        "<SYNTAX ERROR>\n"
    | 930 ->
        "<SYNTAX ERROR>\n"
    | 939 ->
        "<SYNTAX ERROR>\n"
    | 1800 ->
        "<SYNTAX ERROR>\n"
    | 1801 ->
        "<SYNTAX ERROR>\n"
    | 1802 ->
        "<SYNTAX ERROR>\n"
    | 1818 ->
        "<SYNTAX ERROR>\n"
    | 1181 ->
        "<SYNTAX ERROR>\n"
    | 1182 ->
        "<SYNTAX ERROR>\n"
    | 1204 ->
        "<SYNTAX ERROR>\n"
    | 1324 ->
        "Defining a function?\nExpecting one of the following:\n  - \"=>\" to start the function body\n  - an identifier to add a function parameter\n  - \":\" to specify the return type\n"
    | 1292 ->
        "<SYNTAX ERROR>\n"
    | 1209 ->
        "<SYNTAX ERROR>\n"
    | 1210 ->
        "<SYNTAX ERROR>\n"
    | 1211 ->
        "<SYNTAX ERROR>\n"
    | 1212 ->
        "<SYNTAX ERROR>\n"
    | 1213 ->
        "Expecting an expression as function body\n"
    | 1287 ->
        "<SYNTAX ERROR>\n"
    | 1288 ->
        "Defining a function?\nExpecting \"=>\" to start the function body\n"
    | 1289 ->
        "<SYNTAX ERROR>\n"
    | 1290 ->
        "<SYNTAX ERROR>\n"
    | 1205 ->
        "<SYNTAX ERROR>\n"
    | 1206 ->
        "<SYNTAX ERROR>\n"
    | 1207 ->
        "<SYNTAX ERROR>\n"
    | 1208 ->
        "<SYNTAX ERROR>\n"
    | 1297 ->
        "<SYNTAX ERROR>\n"
    | 1320 ->
        "<SYNTAX ERROR>\n"
    | 1321 ->
        "<SYNTAX ERROR>\n"
    | 1301 ->
        "<SYNTAX ERROR>\n"
    | 1302 ->
        "<SYNTAX ERROR>\n"
    | 1303 ->
        "<SYNTAX ERROR>\n"
    | 1306 ->
        "<SYNTAX ERROR>\n"
    | 1307 ->
        "<SYNTAX ERROR>\n"
    | 1308 ->
        "<SYNTAX ERROR>\n"
    | 1311 ->
        "<SYNTAX ERROR>\n"
    | 1312 ->
        "<SYNTAX ERROR>\n"
    | 1313 ->
        "<SYNTAX ERROR>\n"
    | 1314 ->
        "<SYNTAX ERROR>\n"
    | 1310 ->
        "<SYNTAX ERROR>\n"
    | 1550 ->
        "<SYNTAX ERROR>\n"
    | 69 ->
        "<SYNTAX ERROR>\n"
    | 1751 ->
        "<SYNTAX ERROR>\n"
    | 191 ->
        "<SYNTAX ERROR>\n"
    | 2505 ->
        "<SYNTAX ERROR>\n"
    | 2506 ->
        "<SYNTAX ERROR>\n"
    | 2504 ->
        "<SYNTAX ERROR>\n"
    | 70 ->
        "<SYNTAX ERROR>\n"
    | 1102 ->
        "<SYNTAX ERROR>\n"
    | 1058 ->
        "<SYNTAX ERROR>\n"
    | 2699 ->
        "<SYNTAX ERROR>\n"
    | 18 ->
        "<SYNTAX ERROR>\n"
    | 164 ->
        "<SYNTAX ERROR>\n"
    | 2579 ->
        "<SYNTAX ERROR>\n"
    | 1838 ->
        "<SYNTAX ERROR>\n"
    | 1841 ->
        "<SYNTAX ERROR>\n"
    | 1843 ->
        "<SYNTAX ERROR>\n"
    | 1846 ->
        "Expecting a type name\n"
    | 176 ->
        "Expecting an expression\n"
    | 1431 ->
        "Expecting an expression\n"
    | 1407 ->
        "Expecting an expression\n"
    | 618 ->
        "<SYNTAX ERROR>\n"
    | 1749 ->
        "Expecting \"]\" to finish current floating attribute\n"
    | 1060 ->
        "<SYNTAX ERROR>\n"
    | 167 ->
        "Expecting one of the following:\n  - an list item\n  - \"]\" to finish this list\n"
    | 2281 ->
        "Expecting one of the following:\n  - \",\" to separate two items in a list\n  - \"]\" to finish this list\n"
    | 2282 ->
        "<SYNTAX ERROR>\n"
    | 2278 ->
        "<SYNTAX ERROR>\n"
    | 2279 ->
        "<SYNTAX ERROR>\n"
    | 169 ->
        "<SYNTAX ERROR>\n"
    | 170 ->
        "<SYNTAX ERROR>\n"
    | 595 ->
        "<SYNTAX ERROR>\n"
    | 171 ->
        "<SYNTAX ERROR>\n"
    | 2561 ->
        "<SYNTAX ERROR>\n"
    | 174 ->
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
    | 1391 ->
        "<SYNTAX ERROR>\n"
    | 1396 ->
        "<SYNTAX ERROR>\n"
    | 1397 ->
        "<SYNTAX ERROR>\n"
    | 1398 ->
        "<SYNTAX ERROR>\n"
    | 1399 ->
        "<SYNTAX ERROR>\n"
    | 1400 ->
        "<SYNTAX ERROR>\n"
    | 1403 ->
        "<SYNTAX ERROR>\n"
    | 1404 ->
        "<SYNTAX ERROR>\n"
    | 1496 ->
        "<SYNTAX ERROR>\n"
    | 1498 ->
        "<SYNTAX ERROR>\n"
    | 1499 ->
        "<SYNTAX ERROR>\n"
    | 1500 ->
        "<SYNTAX ERROR>\n"
    | 1395 ->
        "<SYNTAX ERROR>\n"
    | 2430 ->
        "<SYNTAX ERROR>\n"
    | 555 ->
        "<SYNTAX ERROR>\n"
    | 2360 ->
        "<SYNTAX ERROR>\n"
    | 2337 ->
        "<SYNTAX ERROR>\n"
    | 1501 ->
        "<SYNTAX ERROR>\n"
    | 1503 ->
        "<SYNTAX ERROR>\n"
    | 1504 ->
        "<SYNTAX ERROR>\n"
    | 1505 ->
        "<SYNTAX ERROR>\n"
    | 1506 ->
        "<SYNTAX ERROR>\n"
    | 1148 ->
        "<SYNTAX ERROR>\n"
    | 1150 ->
        "<SYNTAX ERROR>\n"
    | 1151 ->
        "<SYNTAX ERROR>\n"
    | 1508 ->
        "<SYNTAX ERROR>\n"
    | 1509 ->
        "<SYNTAX ERROR>\n"
    | 1510 ->
        "<SYNTAX ERROR>\n"
    | 1511 ->
        "<SYNTAX ERROR>\n"
    | 1514 ->
        "<SYNTAX ERROR>\n"
    | 1525 ->
        "<SYNTAX ERROR>\n"
    | 1515 ->
        "<SYNTAX ERROR>\n"
    | 1516 ->
        "<SYNTAX ERROR>\n"
    | 1517 ->
        "<SYNTAX ERROR>\n"
    | 1518 ->
        "<SYNTAX ERROR>\n"
    | 1519 ->
        "<SYNTAX ERROR>\n"
    | 1585 ->
        "<SYNTAX ERROR>\n"
    | 1529 ->
        "<SYNTAX ERROR>\n"
    | 1530 ->
        "<SYNTAX ERROR>\n"
    | 1531 ->
        "<SYNTAX ERROR>\n"
    | 1538 ->
        "<SYNTAX ERROR>\n"
    | 1539 ->
        "<SYNTAX ERROR>\n"
    | 1540 ->
        "<SYNTAX ERROR>\n"
    | 1532 ->
        "<SYNTAX ERROR>\n"
    | 1534 ->
        "<SYNTAX ERROR>\n"
    | 1535 ->
        "<SYNTAX ERROR>\n"
    | 1536 ->
        "<SYNTAX ERROR>\n"
    | 1537 ->
        "<SYNTAX ERROR>\n"
    | 1502 ->
        "<SYNTAX ERROR>\n"
    | 1886 ->
        "<SYNTAX ERROR>\n"
    | 1877 ->
        "<SYNTAX ERROR>\n"
    | 1875 ->
        "<SYNTAX ERROR>\n"
    | 1878 ->
        "<SYNTAX ERROR>\n"
    | 1879 ->
        "<SYNTAX ERROR>\n"
    | 1889 ->
        "<SYNTAX ERROR>\n"
    | 1890 ->
        "<SYNTAX ERROR>\n"
    | 603 ->
        "<SYNTAX ERROR>\n"
    | 2113 ->
        "<SYNTAX ERROR>\n"
    | 2119 ->
        "<SYNTAX ERROR>\n"
    | 2114 ->
        "<SYNTAX ERROR>\n"
    | 2115 ->
        "<SYNTAX ERROR>\n"
    | 2116 ->
        "<SYNTAX ERROR>\n"
    | 2118 ->
        "<SYNTAX ERROR>\n"
    | 2083 ->
        "<SYNTAX ERROR>\n"
    | 605 ->
        "<SYNTAX ERROR>\n"
    | 609 ->
        "<SYNTAX ERROR>\n"
    | 610 ->
        "<SYNTAX ERROR>\n"
    | 606 ->
        "<SYNTAX ERROR>\n"
    | 2347 ->
        "<SYNTAX ERROR>\n"
    | 2348 ->
        "<SYNTAX ERROR>\n"
    | 2351 ->
        "<SYNTAX ERROR>\n"
    | 2350 ->
        "<SYNTAX ERROR>\n"
    | 2084 ->
        "<SYNTAX ERROR>\n"
    | 2109 ->
        "<SYNTAX ERROR>\n"
    | 1521 ->
        "<SYNTAX ERROR>\n"
    | 1522 ->
        "<SYNTAX ERROR>\n"
    | 1523 ->
        "<SYNTAX ERROR>\n"
    | 1524 ->
        "<SYNTAX ERROR>\n"
    | 2085 ->
        "<SYNTAX ERROR>\n"
    | 2086 ->
        "<SYNTAX ERROR>\n"
    | 2087 ->
        "<SYNTAX ERROR>\n"
    | 2088 ->
        "<SYNTAX ERROR>\n"
    | 2090 ->
        "<SYNTAX ERROR>\n"
    | 2105 ->
        "<SYNTAX ERROR>\n"
    | 2106 ->
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
    | 2100 ->
        "<SYNTAX ERROR>\n"
    | 2101 ->
        "<SYNTAX ERROR>\n"
    | 2102 ->
        "<SYNTAX ERROR>\n"
    | 2103 ->
        "<SYNTAX ERROR>\n"
    | 2099 ->
        "<SYNTAX ERROR>\n"
    | 2510 ->
        "Expecting \"}\" to finish the block\n"
    | 2240 ->
        "<SYNTAX ERROR>\n"
    | 1725 ->
        "<SYNTAX ERROR>\n"
    | 1158 ->
        "<SYNTAX ERROR>\n"
    | 1547 ->
        "<SYNTAX ERROR>\n"
    | 1544 ->
        "<SYNTAX ERROR>\n"
    | 1563 ->
        "<SYNTAX ERROR>\n"
    | 1564 ->
        "<SYNTAX ERROR>\n"
    | 1567 ->
        "<SYNTAX ERROR>\n"
    | 1200 ->
        "<SYNTAX ERROR>\n"
    | 1596 ->
        "<SYNTAX ERROR>\n"
    | 1599 ->
        "<SYNTAX ERROR>\n"
    | 1621 ->
        "<SYNTAX ERROR>\n"
    | 1600 ->
        "<SYNTAX ERROR>\n"
    | 1606 ->
        "<SYNTAX ERROR>\n"
    | 1607 ->
        "<SYNTAX ERROR>\n"
    | 1611 ->
        "<SYNTAX ERROR>\n"
    | 1612 ->
        "<SYNTAX ERROR>\n"
    | 1613 ->
        "<SYNTAX ERROR>\n"
    | 1602 ->
        "<SYNTAX ERROR>\n"
    | 1620 ->
        "<SYNTAX ERROR>\n"
    | 1617 ->
        "<SYNTAX ERROR>\n"
    | 1618 ->
        "<SYNTAX ERROR>\n"
    | 1619 ->
        "<SYNTAX ERROR>\n"
    | 1626 ->
        "<SYNTAX ERROR>\n"
    | 1627 ->
        "<SYNTAX ERROR>\n"
    | 1628 ->
        "<SYNTAX ERROR>\n"
    | 1348 ->
        "<SYNTAX ERROR>\n"
    | 1360 ->
        "<SYNTAX ERROR>\n"
    | 1586 ->
        "<SYNTAX ERROR>\n"
    | 1569 ->
        "<SYNTAX ERROR>\n"
    | 1570 ->
        "<SYNTAX ERROR>\n"
    | 1201 ->
        "<SYNTAX ERROR>\n"
    | 1382 ->
        "<SYNTAX ERROR>\n"
    | 1590 ->
        "<SYNTAX ERROR>\n"
    | 1202 ->
        "<SYNTAX ERROR>\n"
    | 1378 ->
        "<SYNTAX ERROR>\n"
    | 1379 ->
        "<SYNTAX ERROR>\n"
    | 1338 ->
        "<SYNTAX ERROR>\n"
    | 1340 ->
        "<SYNTAX ERROR>\n"
    | 1341 ->
        "<SYNTAX ERROR>\n"
    | 1366 ->
        "<SYNTAX ERROR>\n"
    | 1342 ->
        "<SYNTAX ERROR>\n"
    | 1343 ->
        "<SYNTAX ERROR>\n"
    | 1344 ->
        "<SYNTAX ERROR>\n"
    | 1568 ->
        "<SYNTAX ERROR>\n"
    | 1870 ->
        "<SYNTAX ERROR>\n"
    | 1871 ->
        "<SYNTAX ERROR>\n"
    | 1872 ->
        "<SYNTAX ERROR>\n"
    | 1574 ->
        "<SYNTAX ERROR>\n"
    | 1575 ->
        "<SYNTAX ERROR>\n"
    | 1576 ->
        "<SYNTAX ERROR>\n"
    | 1373 ->
        "<SYNTAX ERROR>\n"
    | 1374 ->
        "<SYNTAX ERROR>\n"
    | 1385 ->
        "<SYNTAX ERROR>\n"
    | 577 ->
        "<SYNTAX ERROR>\n"
    | 1062 ->
        "<SYNTAX ERROR>\n"
    | 932 ->
        "<SYNTAX ERROR>\n"
    | 876 ->
        "<SYNTAX ERROR>\n"
    | 877 ->
        "<SYNTAX ERROR>\n"
    | 1936 ->
        "<SYNTAX ERROR>\n"
    | 1938 ->
        "<SYNTAX ERROR>\n"
    | 1941 ->
        "<SYNTAX ERROR>\n"
    | 2258 ->
        "<SYNTAX ERROR>\n"
    | 925 ->
        "<SYNTAX ERROR>\n"
    | 1823 ->
        "<SYNTAX ERROR>\n"
    | 411 ->
        "<SYNTAX ERROR>\n"
    | 412 ->
        "<SYNTAX ERROR>\n"
    | 2470 ->
        "<SYNTAX ERROR>\n"
    | 2472 ->
        "<SYNTAX ERROR>\n"
    | 1939 ->
        "<SYNTAX ERROR>\n"
    | 2474 ->
        "<SYNTAX ERROR>\n"
    | 1944 ->
        "<SYNTAX ERROR>\n"
    | 1945 ->
        "<SYNTAX ERROR>\n"
    | 2476 ->
        "<SYNTAX ERROR>\n"
    | 2054 ->
        "<SYNTAX ERROR>\n"
    | 2029 ->
        "<SYNTAX ERROR>\n"
    | 2030 ->
        "<SYNTAX ERROR>\n"
    | 2031 ->
        "<SYNTAX ERROR>\n"
    | 2033 ->
        "<SYNTAX ERROR>\n"
    | 2036 ->
        "<SYNTAX ERROR>\n"
    | 2043 ->
        "<SYNTAX ERROR>\n"
    | 2046 ->
        "<SYNTAX ERROR>\n"
    | 2047 ->
        "<SYNTAX ERROR>\n"
    | 2261 ->
        "<SYNTAX ERROR>\n"
    | 2262 ->
        "<SYNTAX ERROR>\n"
    | 570 ->
        "<SYNTAX ERROR>\n"
    | 571 ->
        "<SYNTAX ERROR>\n"
    | 2381 ->
        "<SYNTAX ERROR>\n"
    | 2383 ->
        "<SYNTAX ERROR>\n"
    | 2034 ->
        "<SYNTAX ERROR>\n"
    | 2385 ->
        "<SYNTAX ERROR>\n"
    | 2039 ->
        "<SYNTAX ERROR>\n"
    | 2040 ->
        "<SYNTAX ERROR>\n"
    | 2387 ->
        "<SYNTAX ERROR>\n"
    | 2049 ->
        "<SYNTAX ERROR>\n"
    | 2050 ->
        "<SYNTAX ERROR>\n"
    | 1948 ->
        "<SYNTAX ERROR>\n"
    | 2024 ->
        "<SYNTAX ERROR>\n"
    | 2025 ->
        "<SYNTAX ERROR>\n"
    | 2026 ->
        "<SYNTAX ERROR>\n"
    | 2028 ->
        "<SYNTAX ERROR>\n"
    | 419 ->
        "<SYNTAX ERROR>\n"
    | 2210 ->
        "<SYNTAX ERROR>\n"
    | 422 ->
        "<SYNTAX ERROR>\n"
    | 436 ->
        "<SYNTAX ERROR>\n"
    | 425 ->
        "<SYNTAX ERROR>\n"
    | 2434 ->
        "<SYNTAX ERROR>\n"
    | 2438 ->
        "<SYNTAX ERROR>\n"
    | 2435 ->
        "<SYNTAX ERROR>\n"
    | 2436 ->
        "<SYNTAX ERROR>\n"
    | 427 ->
        "<SYNTAX ERROR>\n"
    | 429 ->
        "<SYNTAX ERROR>\n"
    | 431 ->
        "<SYNTAX ERROR>\n"
    | 433 ->
        "<SYNTAX ERROR>\n"
    | 2217 ->
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
    | 2218 ->
        "<SYNTAX ERROR>\n"
    | 2220 ->
        "<SYNTAX ERROR>\n"
    | 2209 ->
        "<SYNTAX ERROR>\n"
    | 1949 ->
        "<SYNTAX ERROR>\n"
    | 1950 ->
        "<SYNTAX ERROR>\n"
    | 1953 ->
        "<SYNTAX ERROR>\n"
    | 1954 ->
        "<SYNTAX ERROR>\n"
    | 1956 ->
        "<SYNTAX ERROR>\n"
    | 2020 ->
        "<SYNTAX ERROR>\n"
    | 2021 ->
        "<SYNTAX ERROR>\n"
    | 2022 ->
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
    | 2075 ->
        "<SYNTAX ERROR>\n"
    | 2076 ->
        "<SYNTAX ERROR>\n"
    | 2023 ->
        "<SYNTAX ERROR>\n"
    | 2061 ->
        "<SYNTAX ERROR>\n"
    | 2062 ->
        "<SYNTAX ERROR>\n"
    | 2063 ->
        "<SYNTAX ERROR>\n"
    | 2064 ->
        "<SYNTAX ERROR>\n"
    | 2065 ->
        "<SYNTAX ERROR>\n"
    | 2078 ->
        "<SYNTAX ERROR>\n"
    | 2224 ->
        "<SYNTAX ERROR>\n"
    | 2225 ->
        "<SYNTAX ERROR>\n"
    | 2123 ->
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
    | 2131 ->
        "<SYNTAX ERROR>\n"
    | 2135 ->
        "<SYNTAX ERROR>\n"
    | 2136 ->
        "<SYNTAX ERROR>\n"
    | 455 ->
        "<SYNTAX ERROR>\n"
    | 456 ->
        "<SYNTAX ERROR>\n"
    | 2137 ->
        "<SYNTAX ERROR>\n"
    | 2138 ->
        "<SYNTAX ERROR>\n"
    | 2139 ->
        "<SYNTAX ERROR>\n"
    | 441 ->
        "<SYNTAX ERROR>\n"
    | 2150 ->
        "<SYNTAX ERROR>\n"
    | 2173 ->
        "<SYNTAX ERROR>\n"
    | 2182 ->
        "<SYNTAX ERROR>\n"
    | 2174 ->
        "<SYNTAX ERROR>\n"
    | 2175 ->
        "<SYNTAX ERROR>\n"
    | 2177 ->
        "<SYNTAX ERROR>\n"
    | 2178 ->
        "<SYNTAX ERROR>\n"
    | 2179 ->
        "<SYNTAX ERROR>\n"
    | 2229 ->
        "<SYNTAX ERROR>\n"
    | 2230 ->
        "<SYNTAX ERROR>\n"
    | 2184 ->
        "<SYNTAX ERROR>\n"
    | 2193 ->
        "<SYNTAX ERROR>\n"
    | 2194 ->
        "<SYNTAX ERROR>\n"
    | 2195 ->
        "<SYNTAX ERROR>\n"
    | 2196 ->
        "<SYNTAX ERROR>\n"
    | 2197 ->
        "<SYNTAX ERROR>\n"
    | 2198 ->
        "<SYNTAX ERROR>\n"
    | 2199 ->
        "<SYNTAX ERROR>\n"
    | 2185 ->
        "<SYNTAX ERROR>\n"
    | 2186 ->
        "<SYNTAX ERROR>\n"
    | 2234 ->
        "<SYNTAX ERROR>\n"
    | 2235 ->
        "<SYNTAX ERROR>\n"
    | 2187 ->
        "<SYNTAX ERROR>\n"
    | 2188 ->
        "<SYNTAX ERROR>\n"
    | 2189 ->
        "<SYNTAX ERROR>\n"
    | 2190 ->
        "<SYNTAX ERROR>\n"
    | 559 ->
        "<SYNTAX ERROR>\n"
    | 560 ->
        "<SYNTAX ERROR>\n"
    | 564 ->
        "<SYNTAX ERROR>\n"
    | 565 ->
        "<SYNTAX ERROR>\n"
    | 2399 ->
        "<SYNTAX ERROR>\n"
    | 2401 ->
        "<SYNTAX ERROR>\n"
    | 2402 ->
        "<SYNTAX ERROR>\n"
    | 2403 ->
        "<SYNTAX ERROR>\n"
    | 878 ->
        "<SYNTAX ERROR>\n"
    | 879 ->
        "<SYNTAX ERROR>\n"
    | 881 ->
        "<SYNTAX ERROR>\n"
    | 882 ->
        "<SYNTAX ERROR>\n"
    | 1917 ->
        "<SYNTAX ERROR>\n"
    | 887 ->
        "<SYNTAX ERROR>\n"
    | 888 ->
        "<SYNTAX ERROR>\n"
    | 889 ->
        "<SYNTAX ERROR>\n"
    | 890 ->
        "<SYNTAX ERROR>\n"
    | 891 ->
        "<SYNTAX ERROR>\n"
    | 1912 ->
        "<SYNTAX ERROR>\n"
    | 883 ->
        "<SYNTAX ERROR>\n"
    | 884 ->
        "<SYNTAX ERROR>\n"
    | 885 ->
        "<SYNTAX ERROR>\n"
    | 886 ->
        "<SYNTAX ERROR>\n"
    | 1933 ->
        "<SYNTAX ERROR>\n"
    | 643 ->
        "<SYNTAX ERROR>\n"
    | 865 ->
        "<SYNTAX ERROR>\n"
    | 867 ->
        "<SYNTAX ERROR>\n"
    | 868 ->
        "<SYNTAX ERROR>\n"
    | 1926 ->
        "<SYNTAX ERROR>\n"
    | 1927 ->
        "<SYNTAX ERROR>\n"
    | 1928 ->
        "<SYNTAX ERROR>\n"
    | 1929 ->
        "<SYNTAX ERROR>\n"
    | 1930 ->
        "<SYNTAX ERROR>\n"
    | 1931 ->
        "<SYNTAX ERROR>\n"
    | 892 ->
        "<SYNTAX ERROR>\n"
    | 893 ->
        "<SYNTAX ERROR>\n"
    | 894 ->
        "<SYNTAX ERROR>\n"
    | 895 ->
        "<SYNTAX ERROR>\n"
    | 898 ->
        "<SYNTAX ERROR>\n"
    | 899 ->
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
    | 1070 ->
        "<SYNTAX ERROR>\n"
    | 1074 ->
        "<SYNTAX ERROR>\n"
    | 1075 ->
        "<SYNTAX ERROR>\n"
    | 955 ->
        "<SYNTAX ERROR>\n"
    | 956 ->
        "<SYNTAX ERROR>\n"
    | 1076 ->
        "<SYNTAX ERROR>\n"
    | 1077 ->
        "<SYNTAX ERROR>\n"
    | 1078 ->
        "<SYNTAX ERROR>\n"
    | 953 ->
        "<SYNTAX ERROR>\n"
    | 1085 ->
        "<SYNTAX ERROR>\n"
    | 1104 ->
        "<SYNTAX ERROR>\n"
    | 1193 ->
        "<SYNTAX ERROR>\n"
    | 1106 ->
        "<SYNTAX ERROR>\n"
    | 1107 ->
        "<SYNTAX ERROR>\n"
    | 1109 ->
        "<SYNTAX ERROR>\n"
    | 1112 ->
        "<SYNTAX ERROR>\n"
    | 1744 ->
        "<SYNTAX ERROR>\n"
    | 1186 ->
        "<SYNTAX ERROR>\n"
    | 1187 ->
        "<SYNTAX ERROR>\n"
    | 1195 ->
        "<SYNTAX ERROR>\n"
    | 1703 ->
        "<SYNTAX ERROR>\n"
    | 1704 ->
        "<SYNTAX ERROR>\n"
    | 1705 ->
        "<SYNTAX ERROR>\n"
    | 1706 ->
        "<SYNTAX ERROR>\n"
    | 1667 ->
        "<SYNTAX ERROR>\n"
    | 1668 ->
        "<SYNTAX ERROR>\n"
    | 1707 ->
        "<SYNTAX ERROR>\n"
    | 1708 ->
        "<SYNTAX ERROR>\n"
    | 1710 ->
        "<SYNTAX ERROR>\n"
    | 1672 ->
        "<SYNTAX ERROR>\n"
    | 1673 ->
        "<SYNTAX ERROR>\n"
    | 1714 ->
        "<SYNTAX ERROR>\n"
    | 1711 ->
        "<SYNTAX ERROR>\n"
    | 1712 ->
        "<SYNTAX ERROR>\n"
    | 1114 ->
        "<SYNTAX ERROR>\n"
    | 1122 ->
        "<SYNTAX ERROR>\n"
    | 1123 ->
        "<SYNTAX ERROR>\n"
    | 1124 ->
        "<SYNTAX ERROR>\n"
    | 1125 ->
        "<SYNTAX ERROR>\n"
    | 1126 ->
        "<SYNTAX ERROR>\n"
    | 1128 ->
        "<SYNTAX ERROR>\n"
    | 1129 ->
        "<SYNTAX ERROR>\n"
    | 1130 ->
        "<SYNTAX ERROR>\n"
    | 1131 ->
        "<SYNTAX ERROR>\n"
    | 1137 ->
        "<SYNTAX ERROR>\n"
    | 1138 ->
        "<SYNTAX ERROR>\n"
    | 1140 ->
        "<SYNTAX ERROR>\n"
    | 1141 ->
        "<SYNTAX ERROR>\n"
    | 1145 ->
        "<SYNTAX ERROR>\n"
    | 1143 ->
        "<SYNTAX ERROR>\n"
    | 1146 ->
        "<SYNTAX ERROR>\n"
    | 1147 ->
        "<SYNTAX ERROR>\n"
    | 1118 ->
        "<SYNTAX ERROR>\n"
    | 1727 ->
        "<SYNTAX ERROR>\n"
    | 1728 ->
        "<SYNTAX ERROR>\n"
    | 1731 ->
        "<SYNTAX ERROR>\n"
    | 1115 ->
        "<SYNTAX ERROR>\n"
    | 1116 ->
        "<SYNTAX ERROR>\n"
    | 1121 ->
        "<SYNTAX ERROR>\n"
    | 1695 ->
        "<SYNTAX ERROR>\n"
    | 1196 ->
        "<SYNTAX ERROR>\n"
    | 1197 ->
        "<SYNTAX ERROR>\n"
    | 1198 ->
        "<SYNTAX ERROR>\n"
    | 1199 ->
        "<SYNTAX ERROR>\n"
    | 1633 ->
        "<SYNTAX ERROR>\n"
    | 1636 ->
        "<SYNTAX ERROR>\n"
    | 1654 ->
        "<SYNTAX ERROR>\n"
    | 1655 ->
        "<SYNTAX ERROR>\n"
    | 1191 ->
        "<SYNTAX ERROR>\n"
    | 1192 ->
        "<SYNTAX ERROR>\n"
    | 1664 ->
        "<SYNTAX ERROR>\n"
    | 1640 ->
        "<SYNTAX ERROR>\n"
    | 1644 ->
        "<SYNTAX ERROR>\n"
    | 1646 ->
        "<SYNTAX ERROR>\n"
    | 1647 ->
        "<SYNTAX ERROR>\n"
    | 1657 ->
        "<SYNTAX ERROR>\n"
    | 1648 ->
        "<SYNTAX ERROR>\n"
    | 1649 ->
        "<SYNTAX ERROR>\n"
    | 1650 ->
        "<SYNTAX ERROR>\n"
    | 1665 ->
        "<SYNTAX ERROR>\n"
    | 1682 ->
        "<SYNTAX ERROR>\n"
    | 1666 ->
        "<SYNTAX ERROR>\n"
    | 1691 ->
        "<SYNTAX ERROR>\n"
    | 1674 ->
        "<SYNTAX ERROR>\n"
    | 1692 ->
        "<SYNTAX ERROR>\n"
    | 1693 ->
        "<SYNTAX ERROR>\n"
    | 1681 ->
        "<SYNTAX ERROR>\n"
    | 1678 ->
        "<SYNTAX ERROR>\n"
    | 1679 ->
        "<SYNTAX ERROR>\n"
    | 1680 ->
        "<SYNTAX ERROR>\n"
    | 1687 ->
        "<SYNTAX ERROR>\n"
    | 1688 ->
        "<SYNTAX ERROR>\n"
    | 1689 ->
        "<SYNTAX ERROR>\n"
    | 584 ->
        "<SYNTAX ERROR>\n"
    | 151 ->
        "<SYNTAX ERROR>\n"
    | 906 ->
        "<SYNTAX ERROR>\n"
    | 2712 ->
        "<SYNTAX ERROR>\n"
    | 947 ->
        "<SYNTAX ERROR>\n"
    | 942 ->
        "<SYNTAX ERROR>\n"
    | 2714 ->
        "<SYNTAX ERROR>\n"
    | 1781 ->
        "<SYNTAX ERROR>\n"
    | 948 ->
        "<SYNTAX ERROR>\n"
    | 974 ->
        "<SYNTAX ERROR>\n"
    | 950 ->
        "<SYNTAX ERROR>\n"
    | 951 ->
        "<SYNTAX ERROR>\n"
    | 2713 ->
        "<SYNTAX ERROR>\n"
    | 981 ->
        "<SYNTAX ERROR>\n"
    | 982 ->
        "<SYNTAX ERROR>\n"
    | 984 ->
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
    | 1035 ->
        "<SYNTAX ERROR>\n"
    | 1041 ->
        "<SYNTAX ERROR>\n"
    | 1043 ->
        "<SYNTAX ERROR>\n"
    | 1036 ->
        "<SYNTAX ERROR>\n"
    | 1037 ->
        "<SYNTAX ERROR>\n"
    | 1048 ->
        "<SYNTAX ERROR>\n"
    | 1049 ->
        "<SYNTAX ERROR>\n"
    | 1050 ->
        "<SYNTAX ERROR>\n"
    | 1051 ->
        "<SYNTAX ERROR>\n"
    | 1782 ->
        "<SYNTAX ERROR>\n"
    | 1783 ->
        "<SYNTAX ERROR>\n"
    | 810 ->
        "<SYNTAX ERROR>\n"
    | 1054 ->
        "<SYNTAX ERROR>\n"
    | 1055 ->
        "<SYNTAX ERROR>\n"
    | 1753 ->
        "<SYNTAX ERROR>\n"
    | 989 ->
        "<SYNTAX ERROR>\n"
    | 990 ->
        "<SYNTAX ERROR>\n"
    | 992 ->
        "<SYNTAX ERROR>\n"
    | 993 ->
        "<SYNTAX ERROR>\n"
    | 999 ->
        "<SYNTAX ERROR>\n"
    | 997 ->
        "<SYNTAX ERROR>\n"
    | 995 ->
        "<SYNTAX ERROR>\n"
    | 1012 ->
        "<SYNTAX ERROR>\n"
    | 1013 ->
        "<SYNTAX ERROR>\n"
    | 1005 ->
        "<SYNTAX ERROR>\n"
    | 1006 ->
        "<SYNTAX ERROR>\n"
    | 1010 ->
        "<SYNTAX ERROR>\n"
    | 76 ->
        "<SYNTAX ERROR>\n"
    | 1009 ->
        "<SYNTAX ERROR>\n"
    | 1007 ->
        "<SYNTAX ERROR>\n"
    | 117 ->
        "<SYNTAX ERROR>\n"
    | 256 ->
        "<SYNTAX ERROR>\n"
    | 1016 ->
        "<SYNTAX ERROR>\n"
    | 1017 ->
        "<SYNTAX ERROR>\n"
    | 257 ->
        "<SYNTAX ERROR>\n"
    | 258 ->
        "<SYNTAX ERROR>\n"
    | 409 ->
        "<SYNTAX ERROR>\n"
    | 410 ->
        "<SYNTAX ERROR>\n"
    | 2478 ->
        "<SYNTAX ERROR>\n"
    | 566 ->
        "<SYNTAX ERROR>\n"
    | 2393 ->
        "<SYNTAX ERROR>\n"
    | 2394 ->
        "<SYNTAX ERROR>\n"
    | 2395 ->
        "<SYNTAX ERROR>\n"
    | 2396 ->
        "<SYNTAX ERROR>\n"
    | 2397 ->
        "<SYNTAX ERROR>\n"
    | 2398 ->
        "<SYNTAX ERROR>\n"
    | 1967 ->
        "<SYNTAX ERROR>\n"
    | 1968 ->
        "<SYNTAX ERROR>\n"
    | 1970 ->
        "<SYNTAX ERROR>\n"
    | 1975 ->
        "<SYNTAX ERROR>\n"
    | 1973 ->
        "<SYNTAX ERROR>\n"
    | 1971 ->
        "<SYNTAX ERROR>\n"
    | 1996 ->
        "<SYNTAX ERROR>\n"
    | 1997 ->
        "<SYNTAX ERROR>\n"
    | 1976 ->
        "<SYNTAX ERROR>\n"
    | 1977 ->
        "<SYNTAX ERROR>\n"
    | 1994 ->
        "<SYNTAX ERROR>\n"
    | 1979 ->
        "<SYNTAX ERROR>\n"
    | 1993 ->
        "<SYNTAX ERROR>\n"
    | 1978 ->
        "<SYNTAX ERROR>\n"
    | 1980 ->
        "<SYNTAX ERROR>\n"
    | 1981 ->
        "<SYNTAX ERROR>\n"
    | 1984 ->
        "<SYNTAX ERROR>\n"
    | 567 ->
        "<SYNTAX ERROR>\n"
    | 660 ->
        "<SYNTAX ERROR>\n"
    | 2001 ->
        "<SYNTAX ERROR>\n"
    | 2002 ->
        "<SYNTAX ERROR>\n"
    | 661 ->
        "<SYNTAX ERROR>\n"
    | 662 ->
        "<SYNTAX ERROR>\n"
    | 568 ->
        "<SYNTAX ERROR>\n"
    | 569 ->
        "<SYNTAX ERROR>\n"
    | 2389 ->
        "<SYNTAX ERROR>\n"
    | 1957 ->
        "<SYNTAX ERROR>\n"
    | 2011 ->
        "<SYNTAX ERROR>\n"
    | 2012 ->
        "<SYNTAX ERROR>\n"
    | 2013 ->
        "<SYNTAX ERROR>\n"
    | 2014 ->
        "<SYNTAX ERROR>\n"
    | 2015 ->
        "<SYNTAX ERROR>\n"
    | 2016 ->
        "<SYNTAX ERROR>\n"
    | 1965 ->
        "<SYNTAX ERROR>\n"
    | 2390 ->
        "<SYNTAX ERROR>\n"
    | 1958 ->
        "<SYNTAX ERROR>\n"
    | 940 ->
        "<SYNTAX ERROR>\n"
    | 1775 ->
        "<SYNTAX ERROR>\n"
    | 1774 ->
        "<SYNTAX ERROR>\n"
    | 1756 ->
        "<SYNTAX ERROR>\n"
    | 1757 ->
        "<SYNTAX ERROR>\n"
    | 1758 ->
        "<SYNTAX ERROR>\n"
    | 1759 ->
        "<SYNTAX ERROR>\n"
    | 1760 ->
        "<SYNTAX ERROR>\n"
    | 1763 ->
        "<SYNTAX ERROR>\n"
    | 952 ->
        "<SYNTAX ERROR>\n"
    | 1766 ->
        "<SYNTAX ERROR>\n"
    | 1767 ->
        "<SYNTAX ERROR>\n"
    | 1787 ->
        "<SYNTAX ERROR>\n"
    | 1769 ->
        "<SYNTAX ERROR>\n"
    | 1694 ->
        "<SYNTAX ERROR>\n"
    | 1770 ->
        "<SYNTAX ERROR>\n"
    | 1788 ->
        "<SYNTAX ERROR>\n"
    | 1789 ->
        "<SYNTAX ERROR>\n"
    | 2719 ->
        "<SYNTAX ERROR>\n"
    | 2721 ->
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
    | 2627 ->
        "<SYNTAX ERROR>\n"
    | 110 ->
        "<SYNTAX ERROR>\n"
    | 2625 ->
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
    | 2622 ->
        "<SYNTAX ERROR>\n"
    | 2623 ->
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
    | 2614 ->
        "<SYNTAX ERROR>\n"
    | 2615 ->
        "<SYNTAX ERROR>\n"
    | 2616 ->
        "<SYNTAX ERROR>\n"
    | 2618 ->
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
    | 2607 ->
        "<SYNTAX ERROR>\n"
    | 2609 ->
        "<SYNTAX ERROR>\n"
    | 2610 ->
        "<SYNTAX ERROR>\n"
    | 157 ->
        "<SYNTAX ERROR>\n"
    | 145 ->
        "<SYNTAX ERROR>\n"
    | 146 ->
        "<SYNTAX ERROR>\n"
    | 2605 ->
        "<SYNTAX ERROR>\n"
    | 148 ->
        "<SYNTAX ERROR>\n"
    | 149 ->
        "<SYNTAX ERROR>\n"
    | 2601 ->
        "<SYNTAX ERROR>\n"
    | 2602 ->
        "<SYNTAX ERROR>\n"
    | 2603 ->
        "<SYNTAX ERROR>\n"
    | 150 ->
        "<SYNTAX ERROR>\n"
    | 155 ->
        "<SYNTAX ERROR>\n"
    | 2599 ->
        "<SYNTAX ERROR>\n"
    | 2595 ->
        "<SYNTAX ERROR>\n"
    | 2592 ->
        "<SYNTAX ERROR>\n"
    | 2723 ->
        "<SYNTAX ERROR>\n"
    | 2725 ->
        "<SYNTAX ERROR>\n"
    | 2727 ->
        "<SYNTAX ERROR>\n"
    | 2728 ->
        "<SYNTAX ERROR>\n"
    | 805 ->
        "<SYNTAX ERROR>\n"
    | 807 ->
        "<SYNTAX ERROR>\n"
    | 818 ->
        "<SYNTAX ERROR>\n"
    | 808 ->
        "<SYNTAX ERROR>\n"
    | 800 ->
        "<SYNTAX ERROR>\n"
    | 644 ->
        "<SYNTAX ERROR>\n"
    | 608 ->
        "<SYNTAX ERROR>\n"
    | 781 ->
        "<SYNTAX ERROR>\n"
    | 193 ->
        "<SYNTAX ERROR>\n"
    | 198 ->
        "<SYNTAX ERROR>\n"
    | 204 ->
        "<SYNTAX ERROR>\n"
    | 210 ->
        "<SYNTAX ERROR>\n"
    | 825 ->
        "<SYNTAX ERROR>\n"
    | 826 ->
        "<SYNTAX ERROR>\n"
    | 855 ->
        "<SYNTAX ERROR>\n"
    | 816 ->
        "<SYNTAX ERROR>\n"
    | 744 ->
        "<SYNTAX ERROR>\n"
    | 731 ->
        "<SYNTAX ERROR>\n"
    | 733 ->
        "<SYNTAX ERROR>\n"
    | 860 ->
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
    | 2489 ->
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
    | 751 ->
        "<SYNTAX ERROR>\n"
    | 734 ->
        "<SYNTAX ERROR>\n"
    | 736 ->
        "<SYNTAX ERROR>\n"
    | 721 ->
        "<SYNTAX ERROR>\n"
    | 690 ->
        "<SYNTAX ERROR>\n"
    | 711 ->
        "<SYNTAX ERROR>\n"
    | 702 ->
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
    | 2493 ->
        "<SYNTAX ERROR>\n"
    | 2494 ->
        "<SYNTAX ERROR>\n"
    | 234 ->
        "<SYNTAX ERROR>\n"
    | 239 ->
        "<SYNTAX ERROR>\n"
    | 646 ->
        "<SYNTAX ERROR>\n"
    | 652 ->
        "<SYNTAX ERROR>\n"
    | 742 ->
        "<SYNTAX ERROR>\n"
    | 831 ->
        "<SYNTAX ERROR>\n"
    | 653 ->
        "<SYNTAX ERROR>\n"
    | 654 ->
        "<SYNTAX ERROR>\n"
    | 656 ->
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
    | 852 ->
        "<SYNTAX ERROR>\n"
    | 668 ->
        "<SYNTAX ERROR>\n"
    | 844 ->
        "<SYNTAX ERROR>\n"
    | 672 ->
        "<SYNTAX ERROR>\n"
    | 839 ->
        "<SYNTAX ERROR>\n"
    | 673 ->
        "<SYNTAX ERROR>\n"
    | 678 ->
        "<SYNTAX ERROR>\n"
    | 689 ->
        "<SYNTAX ERROR>\n"
    | 697 ->
        "<SYNTAX ERROR>\n"
    | 705 ->
        "<SYNTAX ERROR>\n"
    | 2496 ->
        "<SYNTAX ERROR>\n"
    | 2497 ->
        "<SYNTAX ERROR>\n"
    | 2498 ->
        "<SYNTAX ERROR>\n"
    | 2499 ->
        "<SYNTAX ERROR>\n"
    | 2500 ->
        "<SYNTAX ERROR>\n"
    | 2501 ->
        "<SYNTAX ERROR>\n"
    | 745 ->
        "<SYNTAX ERROR>\n"
    | 756 ->
        "<SYNTAX ERROR>\n"
    | 759 ->
        "<SYNTAX ERROR>\n"
    | 749 ->
        "<SYNTAX ERROR>\n"
    | 750 ->
        "<SYNTAX ERROR>\n"
    | 670 ->
        "<SYNTAX ERROR>\n"
    | 671 ->
        "<SYNTAX ERROR>\n"
    | 760 ->
        "<SYNTAX ERROR>\n"
    | 763 ->
        "<SYNTAX ERROR>\n"
    | 771 ->
        "<SYNTAX ERROR>\n"
    | 767 ->
        "<SYNTAX ERROR>\n"
    | 770 ->
        "<SYNTAX ERROR>\n"
    | 768 ->
        "Expecting a valid list identifier\n"
    | 769 ->
        "<SYNTAX ERROR>\n"
    | 772 ->
        "<SYNTAX ERROR>\n"
    | 675 ->
        "<SYNTAX ERROR>\n"
    | 676 ->
        "<SYNTAX ERROR>\n"
    | 687 ->
        "<SYNTAX ERROR>\n"
    | 682 ->
        "<SYNTAX ERROR>\n"
    | 683 ->
        "<SYNTAX ERROR>\n"
    | 773 ->
        "<SYNTAX ERROR>\n"
    | 688 ->
        "<SYNTAX ERROR>\n"
    | 837 ->
        "<SYNTAX ERROR>\n"
    | 776 ->
        "<SYNTAX ERROR>\n"
    | 792 ->
        "<SYNTAX ERROR>\n"
    | 794 ->
        "<SYNTAX ERROR>\n"
    | 2731 ->
        "<SYNTAX ERROR>\n"
    | 2745 ->
        "<SYNTAX ERROR>\n"
    | 2732 ->
        "<SYNTAX ERROR>\n"
    | 2733 ->
        "<SYNTAX ERROR>\n"
    | 2739 ->
        "<SYNTAX ERROR>\n"
    | 2740 ->
        "<SYNTAX ERROR>\n"
    | 2743 ->
        "<SYNTAX ERROR>\n"
    | 2748 ->
        "<SYNTAX ERROR>\n"
    | 2755 ->
        "<SYNTAX ERROR>\n"
    | 2754 ->
        "<SYNTAX ERROR>\n"
    | 2751 ->
        "<SYNTAX ERROR>\n"
    | 2752 ->
        "<SYNTAX ERROR>\n"
    | _ ->
        raise Not_found
