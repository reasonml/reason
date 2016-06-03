
(* This file was auto-generated based on "src/reason_parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "<SYNTAX ERROR>\n"
    | 2 ->
        "<SYNTAX ERROR>\n"
    | 2598 ->
        "<SYNTAX ERROR>\n"
    | 569 ->
        "Expecting one of the following:\n  - an identifier to access a member of an object\n  - \"[\" + expression + \"]\" to access an element of a list\n  - \"(\" + expression + \")\"\n  - \"{\" + expression + \"}\"\n"
    | 570 ->
        "Expecting an expression\n"
    | 2286 ->
        "Expecting one of the following:\n  - an infix operation to connect two expressions\n  - \")\" to close the block\n"
    | 2288 ->
        "Expecting an expression\n"
    | 2289 ->
        "Expecting one of the following:\n  - an infix operation to connect two expressions\n  - \"}\" to close the block\n"
    | 2291 ->
        "Expecting an expression\n"
    | 2292 ->
        "Expecting one of the following:\n  - an infix operation to connect two expressions\n  - \"}\" to close the block\n"
    | 1183 ->
        "Expecting an expression\n"
    | 567 ->
        "Expecting an identifier\n"
    | 1116 ->
        "Expecting a structure item\n"
    | 2603 ->
        "Invalid token\n"
    | 1237 ->
        "Expecting an expression\n"
    | 1238 ->
        "Expecting one of the following:\n  - The continuation of the previous expression\n  - \":\" to start the next expression\n"
    | 1239 ->
        "Expecting an expression\n"
    | 1198 ->
        "Expecting an expression\n"
    | 1204 ->
        "Expecting an expression\n"
    | 1206 ->
        "Expecting an expression\n"
    | 1200 ->
        "Expecting an expression\n"
    | 1208 ->
        "Expecting an expression\n"
    | 1210 ->
        "Expecting an expression\n"
    | 1212 ->
        "Expecting an expression\n"
    | 15 ->
        "Expecting one of the following:\n  - \")\" to form a unit value \"()\"\n  - \"module\" to start a module expression\n  - an expression\n  - an operator to denote the prefix form of an operator\n"
    | 1214 ->
        "Expecting an expression\n"
    | 1220 ->
        "Expecting an expression\n"
    | 1222 ->
        "Expecting an expression\n"
    | 2403 ->
        "Expecting \"]\"\n"
    | 403 ->
        "Expecting an attributed id\n"
    | 2510 ->
        "Expecting \"]\"\n"
    | 161 ->
        "Expecting an attribute id\n"
    | 1185 ->
        "Expecting an expression\n"
    | 1202 ->
        "Expecting an expression\n"
    | 1216 ->
        "Expecting an expression\n"
    | 1218 ->
        "Expecting an expression\n"
    | 1224 ->
        "Expecting an expression\n"
    | 1226 ->
        "Expecting an expression\n"
    | 849 ->
        "<SYNTAX ERROR>\n"
    | 850 ->
        "<SYNTAX ERROR>\n"
    | 2194 ->
        "<SYNTAX ERROR>\n"
    | 851 ->
        "<SYNTAX ERROR>\n"
    | 853 ->
        "<SYNTAX ERROR>\n"
    | 2190 ->
        "<SYNTAX ERROR>\n"
    | 2192 ->
        "<SYNTAX ERROR>\n"
    | 2197 ->
        "<SYNTAX ERROR>\n"
    | 2198 ->
        "<SYNTAX ERROR>\n"
    | 2202 ->
        "<SYNTAX ERROR>\n"
    | 2212 ->
        "<SYNTAX ERROR>\n"
    | 2217 ->
        "<SYNTAX ERROR>\n"
    | 1842 ->
        "<SYNTAX ERROR>\n"
    | 1234 ->
        "<SYNTAX ERROR>\n"
    | 1228 ->
        "<SYNTAX ERROR>\n"
    | 1230 ->
        "<SYNTAX ERROR>\n"
    | 1232 ->
        "<SYNTAX ERROR>\n"
    | 75 ->
        "<SYNTAX ERROR>\n"
    | 949 ->
        "<SYNTAX ERROR>\n"
    | 950 ->
        "<SYNTAX ERROR>\n"
    | 104 ->
        "Expecting one of the following:\n  - \"=\" to start the body of the type declaration\n  - \"constraint\" to add constraints to the type declaration\n  - \";\" to finish type declaratoin\n  - \"+=\" to form a string type extension\n  - \"and\" to declare another type\n"
    | 91 ->
        "<SYNTAX ERROR>\n"
    | 2583 ->
        "<SYNTAX ERROR>\n"
    | 2587 ->
        "<SYNTAX ERROR>\n"
    | 2584 ->
        "<SYNTAX ERROR>\n"
    | 2585 ->
        "<SYNTAX ERROR>\n"
    | 95 ->
        "<SYNTAX ERROR>\n"
    | 97 ->
        "<SYNTAX ERROR>\n"
    | 99 ->
        "<SYNTAX ERROR>\n"
    | 101 ->
        "<SYNTAX ERROR>\n"
    | 1126 ->
        "<SYNTAX ERROR>\n"
    | 105 ->
        "<SYNTAX ERROR>\n"
    | 2569 ->
        "<SYNTAX ERROR>\n"
    | 2570 ->
        "<SYNTAX ERROR>\n"
    | 2560 ->
        "<SYNTAX ERROR>\n"
    | 2572 ->
        "<SYNTAX ERROR>\n"
    | 2576 ->
        "<SYNTAX ERROR>\n"
    | 2577 ->
        "<SYNTAX ERROR>\n"
    | 2552 ->
        "<SYNTAX ERROR>\n"
    | 107 ->
        "<SYNTAX ERROR>\n"
    | 2551 ->
        "<SYNTAX ERROR>\n"
    | 115 ->
        "<SYNTAX ERROR>\n"
    | 2565 ->
        "Expecting at least one type field definition in the form of:\n  <field name> : <type>\n"
    | 484 ->
        "Expecting a type field definition in the form of:\n  <field name> : <type>\n"
    | 485 ->
        "Expecting \":\"\n"
    | 486 ->
        "Expecting a type name describing this field\n"
    | 2566 ->
        "Expecting one of the following:\n  - \",\" to finish current type field\n  - \"}\" to finish type definition\n"
    | 500 ->
        "Expecting one of the following:\n  - another type field definition\n  - \"}\" to finish entire type definition\n"
    | 1131 ->
        "<SYNTAX ERROR>\n"
    | 2557 ->
        "<SYNTAX ERROR>\n"
    | 971 ->
        "<SYNTAX ERROR>\n"
    | 972 ->
        "<SYNTAX ERROR>\n"
    | 973 ->
        "<SYNTAX ERROR>\n"
    | 1127 ->
        "<SYNTAX ERROR>\n"
    | 1129 ->
        "<SYNTAX ERROR>\n"
    | 163 ->
        "<SYNTAX ERROR>\n"
    | 2505 ->
        "<SYNTAX ERROR>\n"
    | 2504 ->
        "<SYNTAX ERROR>\n"
    | 2507 ->
        "<SYNTAX ERROR>\n"
    | 2446 ->
        "<SYNTAX ERROR>\n"
    | 2447 ->
        "<SYNTAX ERROR>\n"
    | 2448 ->
        "Expecting a sequence item\n"
    | 1872 ->
        "Expecting one of the following:\n  - \"|\" to open the next pattern\n  - \"=>\" to start the body of the matched pattern\n  - \"when\" to start a contitional guard for the previous pattern\n"
    | 2479 ->
        "Expecting the body of the matched pattern\n"
    | 2508 ->
        "Expecting one of the following:\n  - \"}\" to finish the block\n  - \"|\" to start another pattern matching case\n"
    | 2462 ->
        "<SYNTAX ERROR>\n"
    | 2449 ->
        "<SYNTAX ERROR>\n"
    | 2450 ->
        "<SYNTAX ERROR>\n"
    | 2453 ->
        "<SYNTAX ERROR>\n"
    | 2454 ->
        "<SYNTAX ERROR>\n"
    | 2451 ->
        "<SYNTAX ERROR>\n"
    | 2472 ->
        "<SYNTAX ERROR>\n"
    | 2473 ->
        "<SYNTAX ERROR>\n"
    | 2476 ->
        "<SYNTAX ERROR>\n"
    | 2475 ->
        "<SYNTAX ERROR>\n"
    | 1871 ->
        "Expecting a match case\n"
    | 888 ->
        "<SYNTAX ERROR>\n"
    | 124 ->
        "<SYNTAX ERROR>\n"
    | 125 ->
        "<SYNTAX ERROR>\n"
    | 889 ->
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
    | 168 ->
        "<SYNTAX ERROR>\n"
    | 2488 ->
        "<SYNTAX ERROR>\n"
    | 2489 ->
        "<SYNTAX ERROR>\n"
    | 1115 ->
        "Incomplete module item, forgetting a \";\"?\n"
    | 1170 ->
        "<SYNTAX ERROR>\n"
    | 1173 ->
        "<SYNTAX ERROR>\n"
    | 6 ->
        "<SYNTAX ERROR>\n"
    | 1195 ->
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
    | 886 ->
        "<SYNTAX ERROR>\n"
    | 536 ->
        "<SYNTAX ERROR>\n"
    | 16 ->
        "<SYNTAX ERROR>\n"
    | 2595 ->
        "<SYNTAX ERROR>\n"
    | 606 ->
        "<SYNTAX ERROR>\n"
    | 607 ->
        "<SYNTAX ERROR>\n"
    | 2241 ->
        "<SYNTAX ERROR>\n"
    | 2243 ->
        "<SYNTAX ERROR>\n"
    | 2244 ->
        "<SYNTAX ERROR>\n"
    | 2246 ->
        "<SYNTAX ERROR>\n"
    | 2247 ->
        "<SYNTAX ERROR>\n"
    | 1373 ->
        "<SYNTAX ERROR>\n"
    | 603 ->
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
    | 184 ->
        "<SYNTAX ERROR>\n"
    | 1404 ->
        "<SYNTAX ERROR>\n"
    | 1410 ->
        "<SYNTAX ERROR>\n"
    | 1412 ->
        "<SYNTAX ERROR>\n"
    | 2406 ->
        "<SYNTAX ERROR>\n"
    | 340 ->
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
    | 899 ->
        "<SYNTAX ERROR>\n"
    | 900 ->
        "<SYNTAX ERROR>\n"
    | 1786 ->
        "<SYNTAX ERROR>\n"
    | 901 ->
        "<SYNTAX ERROR>\n"
    | 902 ->
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
    | 2339 ->
        "<SYNTAX ERROR>\n"
    | 2344 ->
        "<SYNTAX ERROR>\n"
    | 2341 ->
        "<SYNTAX ERROR>\n"
    | 1253 ->
        "<SYNTAX ERROR>\n"
    | 2338 ->
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
    | 166 ->
        "<SYNTAX ERROR>\n"
    | 2494 ->
        "<SYNTAX ERROR>\n"
    | 2493 ->
        "<SYNTAX ERROR>\n"
    | 2496 ->
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
    | 173 ->
        "<SYNTAX ERROR>\n"
    | 2440 ->
        "<SYNTAX ERROR>\n"
    | 2441 ->
        "<SYNTAX ERROR>\n"
    | 1304 ->
        "<SYNTAX ERROR>\n"
    | 1312 ->
        "<SYNTAX ERROR>\n"
    | 789 ->
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
    | 537 ->
        "<SYNTAX ERROR>\n"
    | 2325 ->
        "<SYNTAX ERROR>\n"
    | 2327 ->
        "<SYNTAX ERROR>\n"
    | 2329 ->
        "<SYNTAX ERROR>\n"
    | 1783 ->
        "<SYNTAX ERROR>\n"
    | 1784 ->
        "<SYNTAX ERROR>\n"
    | 415 ->
        "Expecting one of the following:\n  - \")\" to form a unit value \"()\"\n  - \"module\" to start a module expression\n  - an expression\n  - an operator to denote the prefix form of an operator\n"
    | 2383 ->
        "<SYNTAX ERROR>\n"
    | 717 ->
        "<SYNTAX ERROR>\n"
    | 418 ->
        "Expecting a module expression\n"
    | 2370 ->
        "<SYNTAX ERROR>\n"
    | 2372 ->
        "<SYNTAX ERROR>\n"
    | 2374 ->
        "<SYNTAX ERROR>\n"
    | 2376 ->
        "<SYNTAX ERROR>\n"
    | 2377 ->
        "<SYNTAX ERROR>\n"
    | 2378 ->
        "<SYNTAX ERROR>\n"
    | 2379 ->
        "<SYNTAX ERROR>\n"
    | 2380 ->
        "<SYNTAX ERROR>\n"
    | 2381 ->
        "<SYNTAX ERROR>\n"
    | 1371 ->
        "<SYNTAX ERROR>\n"
    | 2428 ->
        "<SYNTAX ERROR>\n"
    | 189 ->
        "<SYNTAX ERROR>\n"
    | 552 ->
        "<SYNTAX ERROR>\n"
    | 2298 ->
        "<SYNTAX ERROR>\n"
    | 553 ->
        "<SYNTAX ERROR>\n"
    | 1813 ->
        "<SYNTAX ERROR>\n"
    | 1812 ->
        "<SYNTAX ERROR>\n"
    | 1807 ->
        "<SYNTAX ERROR>\n"
    | 1808 ->
        "<SYNTAX ERROR>\n"
    | 571 ->
        "<SYNTAX ERROR>\n"
    | 580 ->
        "<SYNTAX ERROR>\n"
    | 590 ->
        "<SYNTAX ERROR>\n"
    | 608 ->
        "<SYNTAX ERROR>\n"
    | 609 ->
        "<SYNTAX ERROR>\n"
    | 611 ->
        "<SYNTAX ERROR>\n"
    | 612 ->
        "<SYNTAX ERROR>\n"
    | 2238 ->
        "<SYNTAX ERROR>\n"
    | 2224 ->
        "<SYNTAX ERROR>\n"
    | 617 ->
        "<SYNTAX ERROR>\n"
    | 618 ->
        "<SYNTAX ERROR>\n"
    | 619 ->
        "<SYNTAX ERROR>\n"
    | 620 ->
        "<SYNTAX ERROR>\n"
    | 2222 ->
        "<SYNTAX ERROR>\n"
    | 2223 ->
        "<SYNTAX ERROR>\n"
    | 613 ->
        "<SYNTAX ERROR>\n"
    | 614 ->
        "<SYNTAX ERROR>\n"
    | 615 ->
        "<SYNTAX ERROR>\n"
    | 616 ->
        "<SYNTAX ERROR>\n"
    | 2231 ->
        "<SYNTAX ERROR>\n"
    | 2232 ->
        "<SYNTAX ERROR>\n"
    | 2233 ->
        "<SYNTAX ERROR>\n"
    | 2234 ->
        "<SYNTAX ERROR>\n"
    | 2235 ->
        "<SYNTAX ERROR>\n"
    | 2236 ->
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
    | 895 ->
        "<SYNTAX ERROR>\n"
    | 2331 ->
        "<SYNTAX ERROR>\n"
    | 2332 ->
        "<SYNTAX ERROR>\n"
    | 2333 ->
        "<SYNTAX ERROR>\n"
    | 2334 ->
        "<SYNTAX ERROR>\n"
    | 2335 ->
        "<SYNTAX ERROR>\n"
    | 2336 ->
        "<SYNTAX ERROR>\n"
    | 1785 ->
        "<SYNTAX ERROR>\n"
    | 598 ->
        "<SYNTAX ERROR>\n"
    | 1359 ->
        "<SYNTAX ERROR>\n"
    | 1181 ->
        "<SYNTAX ERROR>\n"
    | 904 ->
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
    | 1157 ->
        "<SYNTAX ERROR>\n"
    | 905 ->
        "<SYNTAX ERROR>\n"
    | 906 ->
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
    | 907 ->
        "<SYNTAX ERROR>\n"
    | 908 ->
        "<SYNTAX ERROR>\n"
    | 917 ->
        "<SYNTAX ERROR>\n"
    | 1754 ->
        "<SYNTAX ERROR>\n"
    | 1755 ->
        "<SYNTAX ERROR>\n"
    | 1756 ->
        "<SYNTAX ERROR>\n"
    | 1772 ->
        "<SYNTAX ERROR>\n"
    | 1135 ->
        "<SYNTAX ERROR>\n"
    | 1136 ->
        "<SYNTAX ERROR>\n"
    | 1158 ->
        "<SYNTAX ERROR>\n"
    | 1278 ->
        "Defining a function?\nExpecting one of the following:\n  - \"=>\" to start the function body\n  - an identifier to add a function parameter\n  - \":\" to specify the return type\n"
    | 1246 ->
        "<SYNTAX ERROR>\n"
    | 1163 ->
        "<SYNTAX ERROR>\n"
    | 1164 ->
        "<SYNTAX ERROR>\n"
    | 1165 ->
        "<SYNTAX ERROR>\n"
    | 1166 ->
        "<SYNTAX ERROR>\n"
    | 1167 ->
        "Expecting an expression as function body\n"
    | 1241 ->
        "<SYNTAX ERROR>\n"
    | 1242 ->
        "Defining a function?\nExpecting \"=>\" to start the function body\n"
    | 1243 ->
        "<SYNTAX ERROR>\n"
    | 1244 ->
        "<SYNTAX ERROR>\n"
    | 1159 ->
        "<SYNTAX ERROR>\n"
    | 1160 ->
        "<SYNTAX ERROR>\n"
    | 1161 ->
        "<SYNTAX ERROR>\n"
    | 1162 ->
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
    | 69 ->
        "<SYNTAX ERROR>\n"
    | 1705 ->
        "<SYNTAX ERROR>\n"
    | 191 ->
        "<SYNTAX ERROR>\n"
    | 2426 ->
        "<SYNTAX ERROR>\n"
    | 2427 ->
        "<SYNTAX ERROR>\n"
    | 2425 ->
        "<SYNTAX ERROR>\n"
    | 70 ->
        "<SYNTAX ERROR>\n"
    | 1056 ->
        "<SYNTAX ERROR>\n"
    | 1029 ->
        "<SYNTAX ERROR>\n"
    | 2593 ->
        "<SYNTAX ERROR>\n"
    | 18 ->
        "<SYNTAX ERROR>\n"
    | 164 ->
        "<SYNTAX ERROR>\n"
    | 2500 ->
        "<SYNTAX ERROR>\n"
    | 1792 ->
        "<SYNTAX ERROR>\n"
    | 1795 ->
        "<SYNTAX ERROR>\n"
    | 1797 ->
        "<SYNTAX ERROR>\n"
    | 1800 ->
        "Expecting a type name\n"
    | 176 ->
        "Expecting an expression\n"
    | 1385 ->
        "Expecting an expression\n"
    | 1361 ->
        "Expecting an expression\n"
    | 597 ->
        "<SYNTAX ERROR>\n"
    | 1703 ->
        "Expecting \"]\" to finish current floating attribute\n"
    | 1031 ->
        "<SYNTAX ERROR>\n"
    | 167 ->
        "Expecting one of the following:\n  - an list item\n  - \"]\" to finish this list\n"
    | 2206 ->
        "Expecting one of the following:\n  - \",\" to separate two items in a list\n  - \"]\" to finish this list\n"
    | 2207 ->
        "<SYNTAX ERROR>\n"
    | 2203 ->
        "<SYNTAX ERROR>\n"
    | 2204 ->
        "<SYNTAX ERROR>\n"
    | 169 ->
        "<SYNTAX ERROR>\n"
    | 170 ->
        "<SYNTAX ERROR>\n"
    | 574 ->
        "<SYNTAX ERROR>\n"
    | 171 ->
        "<SYNTAX ERROR>\n"
    | 2482 ->
        "<SYNTAX ERROR>\n"
    | 174 ->
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
    | 2351 ->
        "<SYNTAX ERROR>\n"
    | 534 ->
        "<SYNTAX ERROR>\n"
    | 2281 ->
        "<SYNTAX ERROR>\n"
    | 2258 ->
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
    | 1102 ->
        "<SYNTAX ERROR>\n"
    | 1104 ->
        "<SYNTAX ERROR>\n"
    | 1105 ->
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
    | 582 ->
        "<SYNTAX ERROR>\n"
    | 2063 ->
        "<SYNTAX ERROR>\n"
    | 2069 ->
        "<SYNTAX ERROR>\n"
    | 2064 ->
        "<SYNTAX ERROR>\n"
    | 2065 ->
        "<SYNTAX ERROR>\n"
    | 2066 ->
        "<SYNTAX ERROR>\n"
    | 2068 ->
        "<SYNTAX ERROR>\n"
    | 2033 ->
        "<SYNTAX ERROR>\n"
    | 584 ->
        "<SYNTAX ERROR>\n"
    | 588 ->
        "<SYNTAX ERROR>\n"
    | 589 ->
        "<SYNTAX ERROR>\n"
    | 585 ->
        "<SYNTAX ERROR>\n"
    | 2268 ->
        "<SYNTAX ERROR>\n"
    | 2269 ->
        "<SYNTAX ERROR>\n"
    | 2272 ->
        "<SYNTAX ERROR>\n"
    | 2271 ->
        "<SYNTAX ERROR>\n"
    | 2034 ->
        "<SYNTAX ERROR>\n"
    | 2059 ->
        "<SYNTAX ERROR>\n"
    | 1475 ->
        "<SYNTAX ERROR>\n"
    | 1476 ->
        "<SYNTAX ERROR>\n"
    | 1477 ->
        "<SYNTAX ERROR>\n"
    | 1478 ->
        "<SYNTAX ERROR>\n"
    | 2035 ->
        "<SYNTAX ERROR>\n"
    | 2036 ->
        "<SYNTAX ERROR>\n"
    | 2037 ->
        "<SYNTAX ERROR>\n"
    | 2038 ->
        "<SYNTAX ERROR>\n"
    | 2040 ->
        "<SYNTAX ERROR>\n"
    | 2055 ->
        "<SYNTAX ERROR>\n"
    | 2056 ->
        "<SYNTAX ERROR>\n"
    | 2042 ->
        "<SYNTAX ERROR>\n"
    | 2043 ->
        "<SYNTAX ERROR>\n"
    | 2045 ->
        "<SYNTAX ERROR>\n"
    | 2046 ->
        "<SYNTAX ERROR>\n"
    | 2047 ->
        "<SYNTAX ERROR>\n"
    | 2050 ->
        "<SYNTAX ERROR>\n"
    | 2051 ->
        "<SYNTAX ERROR>\n"
    | 2052 ->
        "<SYNTAX ERROR>\n"
    | 2053 ->
        "<SYNTAX ERROR>\n"
    | 2049 ->
        "<SYNTAX ERROR>\n"
    | 2431 ->
        "Expecting \"}\" to finish the block\n"
    | 2165 ->
        "<SYNTAX ERROR>\n"
    | 1679 ->
        "<SYNTAX ERROR>\n"
    | 1112 ->
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
    | 1154 ->
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
    | 1155 ->
        "<SYNTAX ERROR>\n"
    | 1336 ->
        "<SYNTAX ERROR>\n"
    | 1544 ->
        "<SYNTAX ERROR>\n"
    | 1156 ->
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
    | 556 ->
        "<SYNTAX ERROR>\n"
    | 1033 ->
        "<SYNTAX ERROR>\n"
    | 910 ->
        "<SYNTAX ERROR>\n"
    | 854 ->
        "<SYNTAX ERROR>\n"
    | 855 ->
        "<SYNTAX ERROR>\n"
    | 1886 ->
        "<SYNTAX ERROR>\n"
    | 1888 ->
        "<SYNTAX ERROR>\n"
    | 1891 ->
        "<SYNTAX ERROR>\n"
    | 2183 ->
        "<SYNTAX ERROR>\n"
    | 903 ->
        "<SYNTAX ERROR>\n"
    | 1777 ->
        "<SYNTAX ERROR>\n"
    | 411 ->
        "<SYNTAX ERROR>\n"
    | 412 ->
        "<SYNTAX ERROR>\n"
    | 2391 ->
        "<SYNTAX ERROR>\n"
    | 2393 ->
        "<SYNTAX ERROR>\n"
    | 1889 ->
        "<SYNTAX ERROR>\n"
    | 2395 ->
        "<SYNTAX ERROR>\n"
    | 1894 ->
        "<SYNTAX ERROR>\n"
    | 1895 ->
        "<SYNTAX ERROR>\n"
    | 2397 ->
        "<SYNTAX ERROR>\n"
    | 2004 ->
        "<SYNTAX ERROR>\n"
    | 1979 ->
        "<SYNTAX ERROR>\n"
    | 1980 ->
        "<SYNTAX ERROR>\n"
    | 1981 ->
        "<SYNTAX ERROR>\n"
    | 1983 ->
        "<SYNTAX ERROR>\n"
    | 1986 ->
        "<SYNTAX ERROR>\n"
    | 1993 ->
        "<SYNTAX ERROR>\n"
    | 1996 ->
        "<SYNTAX ERROR>\n"
    | 1997 ->
        "<SYNTAX ERROR>\n"
    | 2186 ->
        "<SYNTAX ERROR>\n"
    | 2187 ->
        "<SYNTAX ERROR>\n"
    | 549 ->
        "<SYNTAX ERROR>\n"
    | 550 ->
        "<SYNTAX ERROR>\n"
    | 2302 ->
        "<SYNTAX ERROR>\n"
    | 2304 ->
        "<SYNTAX ERROR>\n"
    | 1984 ->
        "<SYNTAX ERROR>\n"
    | 2306 ->
        "<SYNTAX ERROR>\n"
    | 1989 ->
        "<SYNTAX ERROR>\n"
    | 1990 ->
        "<SYNTAX ERROR>\n"
    | 2308 ->
        "<SYNTAX ERROR>\n"
    | 1999 ->
        "<SYNTAX ERROR>\n"
    | 2000 ->
        "<SYNTAX ERROR>\n"
    | 1898 ->
        "<SYNTAX ERROR>\n"
    | 1974 ->
        "<SYNTAX ERROR>\n"
    | 1975 ->
        "<SYNTAX ERROR>\n"
    | 1976 ->
        "<SYNTAX ERROR>\n"
    | 1978 ->
        "<SYNTAX ERROR>\n"
    | 419 ->
        "<SYNTAX ERROR>\n"
    | 2135 ->
        "<SYNTAX ERROR>\n"
    | 422 ->
        "<SYNTAX ERROR>\n"
    | 436 ->
        "<SYNTAX ERROR>\n"
    | 425 ->
        "<SYNTAX ERROR>\n"
    | 2355 ->
        "<SYNTAX ERROR>\n"
    | 2359 ->
        "<SYNTAX ERROR>\n"
    | 2356 ->
        "<SYNTAX ERROR>\n"
    | 2357 ->
        "<SYNTAX ERROR>\n"
    | 427 ->
        "<SYNTAX ERROR>\n"
    | 429 ->
        "<SYNTAX ERROR>\n"
    | 431 ->
        "<SYNTAX ERROR>\n"
    | 433 ->
        "<SYNTAX ERROR>\n"
    | 2142 ->
        "<SYNTAX ERROR>\n"
    | 437 ->
        "<SYNTAX ERROR>\n"
    | 505 ->
        "<SYNTAX ERROR>\n"
    | 506 ->
        "<SYNTAX ERROR>\n"
    | 508 ->
        "<SYNTAX ERROR>\n"
    | 512 ->
        "<SYNTAX ERROR>\n"
    | 513 ->
        "<SYNTAX ERROR>\n"
    | 438 ->
        "<SYNTAX ERROR>\n"
    | 477 ->
        "<SYNTAX ERROR>\n"
    | 463 ->
        "<SYNTAX ERROR>\n"
    | 462 ->
        "<SYNTAX ERROR>\n"
    | 252 ->
        "<SYNTAX ERROR>\n"
    | 482 ->
        "<SYNTAX ERROR>\n"
    | 499 ->
        "<SYNTAX ERROR>\n"
    | 519 ->
        "<SYNTAX ERROR>\n"
    | 520 ->
        "<SYNTAX ERROR>\n"
    | 521 ->
        "<SYNTAX ERROR>\n"
    | 522 ->
        "<SYNTAX ERROR>\n"
    | 2143 ->
        "<SYNTAX ERROR>\n"
    | 2145 ->
        "<SYNTAX ERROR>\n"
    | 2134 ->
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
    | 1970 ->
        "<SYNTAX ERROR>\n"
    | 1971 ->
        "<SYNTAX ERROR>\n"
    | 1972 ->
        "<SYNTAX ERROR>\n"
    | 2020 ->
        "<SYNTAX ERROR>\n"
    | 2021 ->
        "<SYNTAX ERROR>\n"
    | 2022 ->
        "<SYNTAX ERROR>\n"
    | 2023 ->
        "<SYNTAX ERROR>\n"
    | 2024 ->
        "<SYNTAX ERROR>\n"
    | 2025 ->
        "<SYNTAX ERROR>\n"
    | 2026 ->
        "<SYNTAX ERROR>\n"
    | 1973 ->
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
    | 2028 ->
        "<SYNTAX ERROR>\n"
    | 2149 ->
        "<SYNTAX ERROR>\n"
    | 2150 ->
        "<SYNTAX ERROR>\n"
    | 2073 ->
        "<SYNTAX ERROR>\n"
    | 2076 ->
        "<SYNTAX ERROR>\n"
    | 2077 ->
        "<SYNTAX ERROR>\n"
    | 2078 ->
        "<SYNTAX ERROR>\n"
    | 2079 ->
        "<SYNTAX ERROR>\n"
    | 2080 ->
        "<SYNTAX ERROR>\n"
    | 2081 ->
        "<SYNTAX ERROR>\n"
    | 2085 ->
        "<SYNTAX ERROR>\n"
    | 2090 ->
        "<SYNTAX ERROR>\n"
    | 455 ->
        "<SYNTAX ERROR>\n"
    | 456 ->
        "<SYNTAX ERROR>\n"
    | 2091 ->
        "<SYNTAX ERROR>\n"
    | 2092 ->
        "<SYNTAX ERROR>\n"
    | 2093 ->
        "<SYNTAX ERROR>\n"
    | 441 ->
        "<SYNTAX ERROR>\n"
    | 466 ->
        "<SYNTAX ERROR>\n"
    | 2098 ->
        "<SYNTAX ERROR>\n"
    | 2107 ->
        "<SYNTAX ERROR>\n"
    | 2099 ->
        "<SYNTAX ERROR>\n"
    | 2100 ->
        "<SYNTAX ERROR>\n"
    | 2102 ->
        "<SYNTAX ERROR>\n"
    | 2103 ->
        "<SYNTAX ERROR>\n"
    | 2104 ->
        "<SYNTAX ERROR>\n"
    | 2154 ->
        "<SYNTAX ERROR>\n"
    | 2155 ->
        "<SYNTAX ERROR>\n"
    | 2109 ->
        "<SYNTAX ERROR>\n"
    | 2118 ->
        "<SYNTAX ERROR>\n"
    | 2119 ->
        "<SYNTAX ERROR>\n"
    | 2120 ->
        "<SYNTAX ERROR>\n"
    | 2121 ->
        "<SYNTAX ERROR>\n"
    | 2122 ->
        "<SYNTAX ERROR>\n"
    | 2123 ->
        "<SYNTAX ERROR>\n"
    | 2124 ->
        "<SYNTAX ERROR>\n"
    | 2110 ->
        "<SYNTAX ERROR>\n"
    | 2111 ->
        "<SYNTAX ERROR>\n"
    | 2159 ->
        "<SYNTAX ERROR>\n"
    | 2160 ->
        "<SYNTAX ERROR>\n"
    | 2112 ->
        "<SYNTAX ERROR>\n"
    | 2113 ->
        "<SYNTAX ERROR>\n"
    | 2114 ->
        "<SYNTAX ERROR>\n"
    | 2115 ->
        "<SYNTAX ERROR>\n"
    | 538 ->
        "<SYNTAX ERROR>\n"
    | 539 ->
        "<SYNTAX ERROR>\n"
    | 543 ->
        "<SYNTAX ERROR>\n"
    | 544 ->
        "<SYNTAX ERROR>\n"
    | 2320 ->
        "<SYNTAX ERROR>\n"
    | 2322 ->
        "<SYNTAX ERROR>\n"
    | 2323 ->
        "<SYNTAX ERROR>\n"
    | 2324 ->
        "<SYNTAX ERROR>\n"
    | 856 ->
        "<SYNTAX ERROR>\n"
    | 857 ->
        "<SYNTAX ERROR>\n"
    | 859 ->
        "<SYNTAX ERROR>\n"
    | 860 ->
        "<SYNTAX ERROR>\n"
    | 1867 ->
        "<SYNTAX ERROR>\n"
    | 865 ->
        "<SYNTAX ERROR>\n"
    | 866 ->
        "<SYNTAX ERROR>\n"
    | 867 ->
        "<SYNTAX ERROR>\n"
    | 868 ->
        "<SYNTAX ERROR>\n"
    | 869 ->
        "<SYNTAX ERROR>\n"
    | 1866 ->
        "<SYNTAX ERROR>\n"
    | 861 ->
        "<SYNTAX ERROR>\n"
    | 862 ->
        "<SYNTAX ERROR>\n"
    | 863 ->
        "<SYNTAX ERROR>\n"
    | 864 ->
        "<SYNTAX ERROR>\n"
    | 1883 ->
        "<SYNTAX ERROR>\n"
    | 622 ->
        "<SYNTAX ERROR>\n"
    | 843 ->
        "<SYNTAX ERROR>\n"
    | 845 ->
        "<SYNTAX ERROR>\n"
    | 846 ->
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
    | 870 ->
        "<SYNTAX ERROR>\n"
    | 871 ->
        "<SYNTAX ERROR>\n"
    | 872 ->
        "<SYNTAX ERROR>\n"
    | 873 ->
        "<SYNTAX ERROR>\n"
    | 876 ->
        "<SYNTAX ERROR>\n"
    | 877 ->
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
    | 1041 ->
        "<SYNTAX ERROR>\n"
    | 1045 ->
        "<SYNTAX ERROR>\n"
    | 1050 ->
        "<SYNTAX ERROR>\n"
    | 942 ->
        "<SYNTAX ERROR>\n"
    | 943 ->
        "<SYNTAX ERROR>\n"
    | 1051 ->
        "<SYNTAX ERROR>\n"
    | 1052 ->
        "<SYNTAX ERROR>\n"
    | 1053 ->
        "<SYNTAX ERROR>\n"
    | 940 ->
        "<SYNTAX ERROR>\n"
    | 932 ->
        "<SYNTAX ERROR>\n"
    | 1058 ->
        "<SYNTAX ERROR>\n"
    | 1147 ->
        "<SYNTAX ERROR>\n"
    | 1060 ->
        "<SYNTAX ERROR>\n"
    | 1061 ->
        "<SYNTAX ERROR>\n"
    | 1063 ->
        "<SYNTAX ERROR>\n"
    | 1066 ->
        "<SYNTAX ERROR>\n"
    | 1698 ->
        "<SYNTAX ERROR>\n"
    | 1140 ->
        "<SYNTAX ERROR>\n"
    | 1141 ->
        "<SYNTAX ERROR>\n"
    | 1149 ->
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
    | 1068 ->
        "<SYNTAX ERROR>\n"
    | 1076 ->
        "<SYNTAX ERROR>\n"
    | 1077 ->
        "<SYNTAX ERROR>\n"
    | 1078 ->
        "<SYNTAX ERROR>\n"
    | 1079 ->
        "<SYNTAX ERROR>\n"
    | 1080 ->
        "<SYNTAX ERROR>\n"
    | 1082 ->
        "<SYNTAX ERROR>\n"
    | 1083 ->
        "<SYNTAX ERROR>\n"
    | 1084 ->
        "<SYNTAX ERROR>\n"
    | 1085 ->
        "<SYNTAX ERROR>\n"
    | 1091 ->
        "<SYNTAX ERROR>\n"
    | 1092 ->
        "<SYNTAX ERROR>\n"
    | 1094 ->
        "<SYNTAX ERROR>\n"
    | 1095 ->
        "<SYNTAX ERROR>\n"
    | 1099 ->
        "<SYNTAX ERROR>\n"
    | 1097 ->
        "<SYNTAX ERROR>\n"
    | 1100 ->
        "<SYNTAX ERROR>\n"
    | 1101 ->
        "<SYNTAX ERROR>\n"
    | 1072 ->
        "<SYNTAX ERROR>\n"
    | 1681 ->
        "<SYNTAX ERROR>\n"
    | 1682 ->
        "<SYNTAX ERROR>\n"
    | 1685 ->
        "<SYNTAX ERROR>\n"
    | 1069 ->
        "<SYNTAX ERROR>\n"
    | 1070 ->
        "<SYNTAX ERROR>\n"
    | 1075 ->
        "<SYNTAX ERROR>\n"
    | 1649 ->
        "<SYNTAX ERROR>\n"
    | 1150 ->
        "<SYNTAX ERROR>\n"
    | 1151 ->
        "<SYNTAX ERROR>\n"
    | 1152 ->
        "<SYNTAX ERROR>\n"
    | 1153 ->
        "<SYNTAX ERROR>\n"
    | 1587 ->
        "<SYNTAX ERROR>\n"
    | 1590 ->
        "<SYNTAX ERROR>\n"
    | 1608 ->
        "<SYNTAX ERROR>\n"
    | 1609 ->
        "<SYNTAX ERROR>\n"
    | 1145 ->
        "<SYNTAX ERROR>\n"
    | 1146 ->
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
    | 563 ->
        "<SYNTAX ERROR>\n"
    | 151 ->
        "<SYNTAX ERROR>\n"
    | 884 ->
        "<SYNTAX ERROR>\n"
    | 2606 ->
        "<SYNTAX ERROR>\n"
    | 925 ->
        "<SYNTAX ERROR>\n"
    | 920 ->
        "<SYNTAX ERROR>\n"
    | 2608 ->
        "<SYNTAX ERROR>\n"
    | 1735 ->
        "<SYNTAX ERROR>\n"
    | 926 ->
        "<SYNTAX ERROR>\n"
    | 937 ->
        "<SYNTAX ERROR>\n"
    | 928 ->
        "<SYNTAX ERROR>\n"
    | 929 ->
        "<SYNTAX ERROR>\n"
    | 2607 ->
        "<SYNTAX ERROR>\n"
    | 952 ->
        "<SYNTAX ERROR>\n"
    | 953 ->
        "<SYNTAX ERROR>\n"
    | 955 ->
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
    | 1006 ->
        "<SYNTAX ERROR>\n"
    | 1012 ->
        "<SYNTAX ERROR>\n"
    | 1014 ->
        "<SYNTAX ERROR>\n"
    | 1007 ->
        "<SYNTAX ERROR>\n"
    | 1008 ->
        "<SYNTAX ERROR>\n"
    | 1019 ->
        "<SYNTAX ERROR>\n"
    | 1020 ->
        "<SYNTAX ERROR>\n"
    | 1021 ->
        "<SYNTAX ERROR>\n"
    | 1022 ->
        "<SYNTAX ERROR>\n"
    | 1736 ->
        "<SYNTAX ERROR>\n"
    | 1737 ->
        "<SYNTAX ERROR>\n"
    | 788 ->
        "<SYNTAX ERROR>\n"
    | 1025 ->
        "<SYNTAX ERROR>\n"
    | 1026 ->
        "<SYNTAX ERROR>\n"
    | 1707 ->
        "<SYNTAX ERROR>\n"
    | 960 ->
        "<SYNTAX ERROR>\n"
    | 961 ->
        "<SYNTAX ERROR>\n"
    | 963 ->
        "<SYNTAX ERROR>\n"
    | 964 ->
        "<SYNTAX ERROR>\n"
    | 970 ->
        "<SYNTAX ERROR>\n"
    | 968 ->
        "<SYNTAX ERROR>\n"
    | 966 ->
        "<SYNTAX ERROR>\n"
    | 983 ->
        "<SYNTAX ERROR>\n"
    | 984 ->
        "<SYNTAX ERROR>\n"
    | 976 ->
        "<SYNTAX ERROR>\n"
    | 977 ->
        "<SYNTAX ERROR>\n"
    | 981 ->
        "<SYNTAX ERROR>\n"
    | 76 ->
        "<SYNTAX ERROR>\n"
    | 980 ->
        "<SYNTAX ERROR>\n"
    | 978 ->
        "<SYNTAX ERROR>\n"
    | 117 ->
        "<SYNTAX ERROR>\n"
    | 256 ->
        "<SYNTAX ERROR>\n"
    | 987 ->
        "<SYNTAX ERROR>\n"
    | 988 ->
        "<SYNTAX ERROR>\n"
    | 257 ->
        "<SYNTAX ERROR>\n"
    | 258 ->
        "<SYNTAX ERROR>\n"
    | 409 ->
        "<SYNTAX ERROR>\n"
    | 410 ->
        "<SYNTAX ERROR>\n"
    | 2399 ->
        "<SYNTAX ERROR>\n"
    | 545 ->
        "<SYNTAX ERROR>\n"
    | 2314 ->
        "<SYNTAX ERROR>\n"
    | 2315 ->
        "<SYNTAX ERROR>\n"
    | 2316 ->
        "<SYNTAX ERROR>\n"
    | 2317 ->
        "<SYNTAX ERROR>\n"
    | 2318 ->
        "<SYNTAX ERROR>\n"
    | 2319 ->
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
    | 1946 ->
        "<SYNTAX ERROR>\n"
    | 1947 ->
        "<SYNTAX ERROR>\n"
    | 1926 ->
        "<SYNTAX ERROR>\n"
    | 1927 ->
        "<SYNTAX ERROR>\n"
    | 1944 ->
        "<SYNTAX ERROR>\n"
    | 1929 ->
        "<SYNTAX ERROR>\n"
    | 1943 ->
        "<SYNTAX ERROR>\n"
    | 1928 ->
        "<SYNTAX ERROR>\n"
    | 1930 ->
        "<SYNTAX ERROR>\n"
    | 1931 ->
        "<SYNTAX ERROR>\n"
    | 1934 ->
        "<SYNTAX ERROR>\n"
    | 546 ->
        "<SYNTAX ERROR>\n"
    | 639 ->
        "<SYNTAX ERROR>\n"
    | 1951 ->
        "<SYNTAX ERROR>\n"
    | 1952 ->
        "<SYNTAX ERROR>\n"
    | 640 ->
        "<SYNTAX ERROR>\n"
    | 641 ->
        "<SYNTAX ERROR>\n"
    | 547 ->
        "<SYNTAX ERROR>\n"
    | 548 ->
        "<SYNTAX ERROR>\n"
    | 2310 ->
        "<SYNTAX ERROR>\n"
    | 1907 ->
        "<SYNTAX ERROR>\n"
    | 1961 ->
        "<SYNTAX ERROR>\n"
    | 1962 ->
        "<SYNTAX ERROR>\n"
    | 1963 ->
        "<SYNTAX ERROR>\n"
    | 1964 ->
        "<SYNTAX ERROR>\n"
    | 1965 ->
        "<SYNTAX ERROR>\n"
    | 1966 ->
        "<SYNTAX ERROR>\n"
    | 1915 ->
        "<SYNTAX ERROR>\n"
    | 2311 ->
        "<SYNTAX ERROR>\n"
    | 1908 ->
        "<SYNTAX ERROR>\n"
    | 918 ->
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
    | 939 ->
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
    | 2613 ->
        "<SYNTAX ERROR>\n"
    | 2615 ->
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
    | 2548 ->
        "<SYNTAX ERROR>\n"
    | 110 ->
        "<SYNTAX ERROR>\n"
    | 2546 ->
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
    | 2543 ->
        "<SYNTAX ERROR>\n"
    | 2544 ->
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
    | 2535 ->
        "<SYNTAX ERROR>\n"
    | 2536 ->
        "<SYNTAX ERROR>\n"
    | 2537 ->
        "<SYNTAX ERROR>\n"
    | 2539 ->
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
    | 488 ->
        "<SYNTAX ERROR>\n"
    | 487 ->
        "<SYNTAX ERROR>\n"
    | 490 ->
        "<SYNTAX ERROR>\n"
    | 295 ->
        "<SYNTAX ERROR>\n"
    | 137 ->
        "<SYNTAX ERROR>\n"
    | 143 ->
        "<SYNTAX ERROR>\n"
    | 2528 ->
        "<SYNTAX ERROR>\n"
    | 2530 ->
        "<SYNTAX ERROR>\n"
    | 2531 ->
        "<SYNTAX ERROR>\n"
    | 157 ->
        "<SYNTAX ERROR>\n"
    | 145 ->
        "<SYNTAX ERROR>\n"
    | 146 ->
        "<SYNTAX ERROR>\n"
    | 2526 ->
        "<SYNTAX ERROR>\n"
    | 148 ->
        "<SYNTAX ERROR>\n"
    | 149 ->
        "<SYNTAX ERROR>\n"
    | 2522 ->
        "<SYNTAX ERROR>\n"
    | 2523 ->
        "<SYNTAX ERROR>\n"
    | 2524 ->
        "<SYNTAX ERROR>\n"
    | 150 ->
        "<SYNTAX ERROR>\n"
    | 155 ->
        "<SYNTAX ERROR>\n"
    | 2520 ->
        "<SYNTAX ERROR>\n"
    | 2516 ->
        "<SYNTAX ERROR>\n"
    | 2513 ->
        "<SYNTAX ERROR>\n"
    | 2617 ->
        "<SYNTAX ERROR>\n"
    | 2619 ->
        "<SYNTAX ERROR>\n"
    | 2621 ->
        "<SYNTAX ERROR>\n"
    | 2622 ->
        "<SYNTAX ERROR>\n"
    | 783 ->
        "<SYNTAX ERROR>\n"
    | 785 ->
        "<SYNTAX ERROR>\n"
    | 796 ->
        "<SYNTAX ERROR>\n"
    | 786 ->
        "<SYNTAX ERROR>\n"
    | 778 ->
        "<SYNTAX ERROR>\n"
    | 623 ->
        "<SYNTAX ERROR>\n"
    | 587 ->
        "<SYNTAX ERROR>\n"
    | 759 ->
        "<SYNTAX ERROR>\n"
    | 193 ->
        "<SYNTAX ERROR>\n"
    | 198 ->
        "<SYNTAX ERROR>\n"
    | 204 ->
        "<SYNTAX ERROR>\n"
    | 210 ->
        "<SYNTAX ERROR>\n"
    | 803 ->
        "<SYNTAX ERROR>\n"
    | 804 ->
        "<SYNTAX ERROR>\n"
    | 833 ->
        "<SYNTAX ERROR>\n"
    | 794 ->
        "<SYNTAX ERROR>\n"
    | 722 ->
        "<SYNTAX ERROR>\n"
    | 709 ->
        "<SYNTAX ERROR>\n"
    | 711 ->
        "<SYNTAX ERROR>\n"
    | 838 ->
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
    | 2410 ->
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
    | 729 ->
        "<SYNTAX ERROR>\n"
    | 712 ->
        "<SYNTAX ERROR>\n"
    | 714 ->
        "<SYNTAX ERROR>\n"
    | 699 ->
        "<SYNTAX ERROR>\n"
    | 668 ->
        "<SYNTAX ERROR>\n"
    | 689 ->
        "<SYNTAX ERROR>\n"
    | 680 ->
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
    | 2414 ->
        "<SYNTAX ERROR>\n"
    | 2415 ->
        "<SYNTAX ERROR>\n"
    | 234 ->
        "<SYNTAX ERROR>\n"
    | 239 ->
        "<SYNTAX ERROR>\n"
    | 625 ->
        "<SYNTAX ERROR>\n"
    | 631 ->
        "<SYNTAX ERROR>\n"
    | 720 ->
        "<SYNTAX ERROR>\n"
    | 809 ->
        "<SYNTAX ERROR>\n"
    | 632 ->
        "<SYNTAX ERROR>\n"
    | 633 ->
        "<SYNTAX ERROR>\n"
    | 635 ->
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
    | 830 ->
        "<SYNTAX ERROR>\n"
    | 647 ->
        "<SYNTAX ERROR>\n"
    | 822 ->
        "<SYNTAX ERROR>\n"
    | 650 ->
        "<SYNTAX ERROR>\n"
    | 817 ->
        "<SYNTAX ERROR>\n"
    | 651 ->
        "<SYNTAX ERROR>\n"
    | 656 ->
        "<SYNTAX ERROR>\n"
    | 667 ->
        "<SYNTAX ERROR>\n"
    | 675 ->
        "<SYNTAX ERROR>\n"
    | 683 ->
        "<SYNTAX ERROR>\n"
    | 2417 ->
        "<SYNTAX ERROR>\n"
    | 2418 ->
        "<SYNTAX ERROR>\n"
    | 2419 ->
        "<SYNTAX ERROR>\n"
    | 2420 ->
        "<SYNTAX ERROR>\n"
    | 2421 ->
        "<SYNTAX ERROR>\n"
    | 2422 ->
        "<SYNTAX ERROR>\n"
    | 723 ->
        "<SYNTAX ERROR>\n"
    | 734 ->
        "<SYNTAX ERROR>\n"
    | 737 ->
        "<SYNTAX ERROR>\n"
    | 727 ->
        "<SYNTAX ERROR>\n"
    | 728 ->
        "<SYNTAX ERROR>\n"
    | 648 ->
        "<SYNTAX ERROR>\n"
    | 649 ->
        "<SYNTAX ERROR>\n"
    | 738 ->
        "<SYNTAX ERROR>\n"
    | 741 ->
        "<SYNTAX ERROR>\n"
    | 749 ->
        "<SYNTAX ERROR>\n"
    | 745 ->
        "<SYNTAX ERROR>\n"
    | 748 ->
        "<SYNTAX ERROR>\n"
    | 746 ->
        "Expecting a valid list identifier\n"
    | 747 ->
        "<SYNTAX ERROR>\n"
    | 750 ->
        "<SYNTAX ERROR>\n"
    | 653 ->
        "<SYNTAX ERROR>\n"
    | 654 ->
        "<SYNTAX ERROR>\n"
    | 665 ->
        "<SYNTAX ERROR>\n"
    | 660 ->
        "<SYNTAX ERROR>\n"
    | 661 ->
        "<SYNTAX ERROR>\n"
    | 751 ->
        "<SYNTAX ERROR>\n"
    | 666 ->
        "<SYNTAX ERROR>\n"
    | 815 ->
        "<SYNTAX ERROR>\n"
    | 754 ->
        "<SYNTAX ERROR>\n"
    | 770 ->
        "<SYNTAX ERROR>\n"
    | 772 ->
        "<SYNTAX ERROR>\n"
    | 2625 ->
        "<SYNTAX ERROR>\n"
    | 2639 ->
        "<SYNTAX ERROR>\n"
    | 2626 ->
        "<SYNTAX ERROR>\n"
    | 2627 ->
        "<SYNTAX ERROR>\n"
    | 2633 ->
        "<SYNTAX ERROR>\n"
    | 2634 ->
        "<SYNTAX ERROR>\n"
    | 2637 ->
        "<SYNTAX ERROR>\n"
    | 2642 ->
        "<SYNTAX ERROR>\n"
    | 2649 ->
        "<SYNTAX ERROR>\n"
    | 2648 ->
        "<SYNTAX ERROR>\n"
    | 2645 ->
        "<SYNTAX ERROR>\n"
    | 2646 ->
        "<SYNTAX ERROR>\n"
    | _ ->
        raise Not_found
