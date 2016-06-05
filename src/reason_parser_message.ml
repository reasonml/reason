
(* This file was auto-generated based on "src/reason_parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "<SYNTAX ERROR>\n"
    | 2 ->
        "<SYNTAX ERROR>\n"
    | 2616 ->
        "<SYNTAX ERROR>\n"
    | 578 ->
        "Expecting one of the following:\n  - an identifier to access a member of an object\n  - \"[\" + expression + \"]\" to access an element of a list\n  - \"(\" + expression + \")\"\n  - \"{\" + expression + \"}\"\n"
    | 579 ->
        "Expecting an expression\n"
    | 2295 ->
        "Expecting one of the following:\n  - an infix operation to connect two expressions\n  - \")\" to close the block\n"
    | 2297 ->
        "Expecting an expression\n"
    | 2298 ->
        "Expecting one of the following:\n  - an infix operation to connect two expressions\n  - \"}\" to close the block\n"
    | 2300 ->
        "Expecting an expression\n"
    | 2301 ->
        "Expecting one of the following:\n  - an infix operation to connect two expressions\n  - \"}\" to close the block\n"
    | 1192 ->
        "Expecting an expression\n"
    | 576 ->
        "Expecting an identifier\n"
    | 1125 ->
        "Expecting a structure item\n"
    | 2621 ->
        "Invalid token\n"
    | 1246 ->
        "Expecting an expression\n"
    | 1247 ->
        "Expecting one of the following:\n  - The continuation of the previous expression\n  - \":\" to start the next expression\n"
    | 1248 ->
        "Expecting an expression\n"
    | 1207 ->
        "Expecting an expression\n"
    | 1213 ->
        "Expecting an expression\n"
    | 1215 ->
        "Expecting an expression\n"
    | 1209 ->
        "Expecting an expression\n"
    | 1217 ->
        "Expecting an expression\n"
    | 1219 ->
        "Expecting an expression\n"
    | 1221 ->
        "Expecting an expression\n"
    | 15 ->
        "Expecting one of the following:\n  - \")\" to form a unit value \"()\"\n  - \"module\" to start a module expression\n  - an expression\n  - an operator to denote the prefix form of an operator\n"
    | 1223 ->
        "Expecting an expression\n"
    | 1229 ->
        "Expecting an expression\n"
    | 1231 ->
        "Expecting an expression\n"
    | 2412 ->
        "Expecting \"]\"\n"
    | 403 ->
        "Expecting an attributed id\n"
    | 2519 ->
        "Expecting \"]\"\n"
    | 161 ->
        "Expecting an attribute id\n"
    | 1194 ->
        "Expecting an expression\n"
    | 1211 ->
        "Expecting an expression\n"
    | 1225 ->
        "Expecting an expression\n"
    | 1227 ->
        "Expecting an expression\n"
    | 1233 ->
        "Expecting an expression\n"
    | 1235 ->
        "Expecting an expression\n"
    | 858 ->
        "<SYNTAX ERROR>\n"
    | 859 ->
        "<SYNTAX ERROR>\n"
    | 2203 ->
        "<SYNTAX ERROR>\n"
    | 860 ->
        "<SYNTAX ERROR>\n"
    | 862 ->
        "<SYNTAX ERROR>\n"
    | 2199 ->
        "<SYNTAX ERROR>\n"
    | 2201 ->
        "<SYNTAX ERROR>\n"
    | 2206 ->
        "<SYNTAX ERROR>\n"
    | 2207 ->
        "<SYNTAX ERROR>\n"
    | 2211 ->
        "<SYNTAX ERROR>\n"
    | 2221 ->
        "<SYNTAX ERROR>\n"
    | 2226 ->
        "<SYNTAX ERROR>\n"
    | 1851 ->
        "<SYNTAX ERROR>\n"
    | 1243 ->
        "<SYNTAX ERROR>\n"
    | 1237 ->
        "<SYNTAX ERROR>\n"
    | 1239 ->
        "<SYNTAX ERROR>\n"
    | 1241 ->
        "<SYNTAX ERROR>\n"
    | 75 ->
        "<SYNTAX ERROR>\n"
    | 958 ->
        "<SYNTAX ERROR>\n"
    | 959 ->
        "<SYNTAX ERROR>\n"
    | 104 ->
        "Expecting one of the following:\n  - \"=\" to start the body of the type declaration\n  - \"constraint\" to add constraints to the type declaration\n  - \";\" to finish type declaratoin\n  - \"+=\" to form a string type extension\n  - \"and\" to declare another type\n"
    | 91 ->
        "<SYNTAX ERROR>\n"
    | 2601 ->
        "<SYNTAX ERROR>\n"
    | 2605 ->
        "<SYNTAX ERROR>\n"
    | 2602 ->
        "<SYNTAX ERROR>\n"
    | 2603 ->
        "<SYNTAX ERROR>\n"
    | 95 ->
        "<SYNTAX ERROR>\n"
    | 97 ->
        "<SYNTAX ERROR>\n"
    | 99 ->
        "<SYNTAX ERROR>\n"
    | 101 ->
        "<SYNTAX ERROR>\n"
    | 1135 ->
        "<SYNTAX ERROR>\n"
    | 105 ->
        "<SYNTAX ERROR>\n"
    | 2586 ->
        "<SYNTAX ERROR>\n"
    | 2587 ->
        "<SYNTAX ERROR>\n"
    | 2574 ->
        "<SYNTAX ERROR>\n"
    | 2589 ->
        "<SYNTAX ERROR>\n"
    | 2594 ->
        "<SYNTAX ERROR>\n"
    | 2595 ->
        "<SYNTAX ERROR>\n"
    | 2566 ->
        "<SYNTAX ERROR>\n"
    | 107 ->
        "<SYNTAX ERROR>\n"
    | 2561 ->
        "<SYNTAX ERROR>\n"
    | 2562 ->
        "<SYNTAX ERROR>\n"
    | 2582 ->
        "Expecting at least one type field definition in the form of:\n  <field name> : <type>\n"
    | 492 ->
        "Expecting a type field definition in the form of:\n  <field name> : <type>\n"
    | 493 ->
        "Expecting \":\"\n"
    | 494 ->
        "Expecting a type name describing this field\n"
    | 2583 ->
        "Expecting one of the following:\n  - \",\" to finish current type field\n  - \"}\" to finish type definition\n"
    | 508 ->
        "Expecting one of the following:\n  - another type field definition\n  - \"}\" to finish entire type definition\n"
    | 1140 ->
        "<SYNTAX ERROR>\n"
    | 2571 ->
        "<SYNTAX ERROR>\n"
    | 980 ->
        "<SYNTAX ERROR>\n"
    | 981 ->
        "<SYNTAX ERROR>\n"
    | 982 ->
        "<SYNTAX ERROR>\n"
    | 1136 ->
        "<SYNTAX ERROR>\n"
    | 1138 ->
        "<SYNTAX ERROR>\n"
    | 163 ->
        "<SYNTAX ERROR>\n"
    | 2514 ->
        "<SYNTAX ERROR>\n"
    | 2513 ->
        "<SYNTAX ERROR>\n"
    | 2516 ->
        "<SYNTAX ERROR>\n"
    | 2455 ->
        "<SYNTAX ERROR>\n"
    | 2456 ->
        "<SYNTAX ERROR>\n"
    | 2457 ->
        "Expecting a sequence item\n"
    | 1881 ->
        "Expecting one of the following:\n  - \"|\" to open the next pattern\n  - \"=>\" to start the body of the matched pattern\n  - \"when\" to start a contitional guard for the previous pattern\n"
    | 2488 ->
        "Expecting the body of the matched pattern\n"
    | 2517 ->
        "Expecting one of the following:\n  - \"}\" to finish the block\n  - \"|\" to start another pattern matching case\n"
    | 2471 ->
        "<SYNTAX ERROR>\n"
    | 2458 ->
        "<SYNTAX ERROR>\n"
    | 2459 ->
        "<SYNTAX ERROR>\n"
    | 2462 ->
        "<SYNTAX ERROR>\n"
    | 2463 ->
        "<SYNTAX ERROR>\n"
    | 2460 ->
        "<SYNTAX ERROR>\n"
    | 2481 ->
        "<SYNTAX ERROR>\n"
    | 2482 ->
        "<SYNTAX ERROR>\n"
    | 2485 ->
        "<SYNTAX ERROR>\n"
    | 2484 ->
        "<SYNTAX ERROR>\n"
    | 1880 ->
        "Expecting a match case\n"
    | 897 ->
        "<SYNTAX ERROR>\n"
    | 124 ->
        "<SYNTAX ERROR>\n"
    | 125 ->
        "<SYNTAX ERROR>\n"
    | 898 ->
        "<SYNTAX ERROR>\n"
    | 1855 ->
        "<SYNTAX ERROR>\n"
    | 1858 ->
        "<SYNTAX ERROR>\n"
    | 1872 ->
        "<SYNTAX ERROR>\n"
    | 1860 ->
        "<SYNTAX ERROR>\n"
    | 1861 ->
        "<SYNTAX ERROR>\n"
    | 1864 ->
        "<SYNTAX ERROR>\n"
    | 1866 ->
        "<SYNTAX ERROR>\n"
    | 1867 ->
        "<SYNTAX ERROR>\n"
    | 1869 ->
        "<SYNTAX ERROR>\n"
    | 168 ->
        "<SYNTAX ERROR>\n"
    | 2497 ->
        "<SYNTAX ERROR>\n"
    | 2498 ->
        "<SYNTAX ERROR>\n"
    | 1124 ->
        "Incomplete module item, forgetting a \";\"?\n"
    | 1179 ->
        "<SYNTAX ERROR>\n"
    | 1182 ->
        "<SYNTAX ERROR>\n"
    | 6 ->
        "<SYNTAX ERROR>\n"
    | 1204 ->
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
    | 895 ->
        "<SYNTAX ERROR>\n"
    | 545 ->
        "<SYNTAX ERROR>\n"
    | 16 ->
        "<SYNTAX ERROR>\n"
    | 2613 ->
        "<SYNTAX ERROR>\n"
    | 615 ->
        "<SYNTAX ERROR>\n"
    | 616 ->
        "<SYNTAX ERROR>\n"
    | 2250 ->
        "<SYNTAX ERROR>\n"
    | 2252 ->
        "<SYNTAX ERROR>\n"
    | 2253 ->
        "<SYNTAX ERROR>\n"
    | 2255 ->
        "<SYNTAX ERROR>\n"
    | 2256 ->
        "<SYNTAX ERROR>\n"
    | 1382 ->
        "<SYNTAX ERROR>\n"
    | 612 ->
        "<SYNTAX ERROR>\n"
    | 1440 ->
        "<SYNTAX ERROR>\n"
    | 1441 ->
        "<SYNTAX ERROR>\n"
    | 1442 ->
        "<SYNTAX ERROR>\n"
    | 1397 ->
        "<SYNTAX ERROR>\n"
    | 1403 ->
        "<SYNTAX ERROR>\n"
    | 1405 ->
        "<SYNTAX ERROR>\n"
    | 1399 ->
        "<SYNTAX ERROR>\n"
    | 1407 ->
        "<SYNTAX ERROR>\n"
    | 1409 ->
        "<SYNTAX ERROR>\n"
    | 1411 ->
        "<SYNTAX ERROR>\n"
    | 184 ->
        "<SYNTAX ERROR>\n"
    | 1413 ->
        "<SYNTAX ERROR>\n"
    | 1419 ->
        "<SYNTAX ERROR>\n"
    | 1421 ->
        "<SYNTAX ERROR>\n"
    | 2415 ->
        "<SYNTAX ERROR>\n"
    | 340 ->
        "<SYNTAX ERROR>\n"
    | 1384 ->
        "<SYNTAX ERROR>\n"
    | 1401 ->
        "<SYNTAX ERROR>\n"
    | 1415 ->
        "<SYNTAX ERROR>\n"
    | 1417 ->
        "<SYNTAX ERROR>\n"
    | 1423 ->
        "<SYNTAX ERROR>\n"
    | 1425 ->
        "<SYNTAX ERROR>\n"
    | 908 ->
        "<SYNTAX ERROR>\n"
    | 909 ->
        "<SYNTAX ERROR>\n"
    | 1795 ->
        "<SYNTAX ERROR>\n"
    | 910 ->
        "<SYNTAX ERROR>\n"
    | 911 ->
        "<SYNTAX ERROR>\n"
    | 1788 ->
        "<SYNTAX ERROR>\n"
    | 1790 ->
        "<SYNTAX ERROR>\n"
    | 1798 ->
        "<SYNTAX ERROR>\n"
    | 1800 ->
        "<SYNTAX ERROR>\n"
    | 1815 ->
        "<SYNTAX ERROR>\n"
    | 1827 ->
        "<SYNTAX ERROR>\n"
    | 1832 ->
        "<SYNTAX ERROR>\n"
    | 2348 ->
        "<SYNTAX ERROR>\n"
    | 2353 ->
        "<SYNTAX ERROR>\n"
    | 2350 ->
        "<SYNTAX ERROR>\n"
    | 1262 ->
        "<SYNTAX ERROR>\n"
    | 2347 ->
        "<SYNTAX ERROR>\n"
    | 1433 ->
        "<SYNTAX ERROR>\n"
    | 1460 ->
        "<SYNTAX ERROR>\n"
    | 1281 ->
        "<SYNTAX ERROR>\n"
    | 1427 ->
        "<SYNTAX ERROR>\n"
    | 1429 ->
        "<SYNTAX ERROR>\n"
    | 1431 ->
        "<SYNTAX ERROR>\n"
    | 166 ->
        "<SYNTAX ERROR>\n"
    | 2503 ->
        "<SYNTAX ERROR>\n"
    | 2502 ->
        "<SYNTAX ERROR>\n"
    | 2505 ->
        "<SYNTAX ERROR>\n"
    | 1372 ->
        "<SYNTAX ERROR>\n"
    | 1373 ->
        "<SYNTAX ERROR>\n"
    | 1435 ->
        "<SYNTAX ERROR>\n"
    | 1438 ->
        "<SYNTAX ERROR>\n"
    | 1456 ->
        "<SYNTAX ERROR>\n"
    | 1444 ->
        "<SYNTAX ERROR>\n"
    | 1445 ->
        "<SYNTAX ERROR>\n"
    | 1448 ->
        "<SYNTAX ERROR>\n"
    | 1450 ->
        "<SYNTAX ERROR>\n"
    | 1451 ->
        "<SYNTAX ERROR>\n"
    | 1453 ->
        "<SYNTAX ERROR>\n"
    | 173 ->
        "<SYNTAX ERROR>\n"
    | 2449 ->
        "<SYNTAX ERROR>\n"
    | 2450 ->
        "<SYNTAX ERROR>\n"
    | 1313 ->
        "<SYNTAX ERROR>\n"
    | 1321 ->
        "<SYNTAX ERROR>\n"
    | 798 ->
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
    | 546 ->
        "<SYNTAX ERROR>\n"
    | 2334 ->
        "<SYNTAX ERROR>\n"
    | 2336 ->
        "<SYNTAX ERROR>\n"
    | 2338 ->
        "<SYNTAX ERROR>\n"
    | 1792 ->
        "<SYNTAX ERROR>\n"
    | 1793 ->
        "<SYNTAX ERROR>\n"
    | 415 ->
        "Expecting one of the following:\n  - \")\" to form a unit value \"()\"\n  - \"module\" to start a module expression\n  - an expression\n  - an operator to denote the prefix form of an operator\n"
    | 2392 ->
        "<SYNTAX ERROR>\n"
    | 726 ->
        "<SYNTAX ERROR>\n"
    | 418 ->
        "Expecting a module expression\n"
    | 2379 ->
        "<SYNTAX ERROR>\n"
    | 2381 ->
        "<SYNTAX ERROR>\n"
    | 2383 ->
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
    | 2390 ->
        "<SYNTAX ERROR>\n"
    | 1380 ->
        "<SYNTAX ERROR>\n"
    | 2437 ->
        "<SYNTAX ERROR>\n"
    | 189 ->
        "<SYNTAX ERROR>\n"
    | 561 ->
        "<SYNTAX ERROR>\n"
    | 2307 ->
        "<SYNTAX ERROR>\n"
    | 562 ->
        "<SYNTAX ERROR>\n"
    | 1822 ->
        "<SYNTAX ERROR>\n"
    | 1821 ->
        "<SYNTAX ERROR>\n"
    | 1816 ->
        "<SYNTAX ERROR>\n"
    | 1817 ->
        "<SYNTAX ERROR>\n"
    | 580 ->
        "<SYNTAX ERROR>\n"
    | 589 ->
        "<SYNTAX ERROR>\n"
    | 599 ->
        "<SYNTAX ERROR>\n"
    | 617 ->
        "<SYNTAX ERROR>\n"
    | 618 ->
        "<SYNTAX ERROR>\n"
    | 620 ->
        "<SYNTAX ERROR>\n"
    | 621 ->
        "<SYNTAX ERROR>\n"
    | 2247 ->
        "<SYNTAX ERROR>\n"
    | 2233 ->
        "<SYNTAX ERROR>\n"
    | 626 ->
        "<SYNTAX ERROR>\n"
    | 627 ->
        "<SYNTAX ERROR>\n"
    | 628 ->
        "<SYNTAX ERROR>\n"
    | 629 ->
        "<SYNTAX ERROR>\n"
    | 2231 ->
        "<SYNTAX ERROR>\n"
    | 2232 ->
        "<SYNTAX ERROR>\n"
    | 622 ->
        "<SYNTAX ERROR>\n"
    | 623 ->
        "<SYNTAX ERROR>\n"
    | 624 ->
        "<SYNTAX ERROR>\n"
    | 625 ->
        "<SYNTAX ERROR>\n"
    | 2240 ->
        "<SYNTAX ERROR>\n"
    | 2241 ->
        "<SYNTAX ERROR>\n"
    | 2242 ->
        "<SYNTAX ERROR>\n"
    | 2243 ->
        "<SYNTAX ERROR>\n"
    | 2244 ->
        "<SYNTAX ERROR>\n"
    | 2245 ->
        "<SYNTAX ERROR>\n"
    | 899 ->
        "<SYNTAX ERROR>\n"
    | 900 ->
        "<SYNTAX ERROR>\n"
    | 901 ->
        "<SYNTAX ERROR>\n"
    | 902 ->
        "<SYNTAX ERROR>\n"
    | 903 ->
        "<SYNTAX ERROR>\n"
    | 904 ->
        "<SYNTAX ERROR>\n"
    | 2340 ->
        "<SYNTAX ERROR>\n"
    | 2341 ->
        "<SYNTAX ERROR>\n"
    | 2342 ->
        "<SYNTAX ERROR>\n"
    | 2343 ->
        "<SYNTAX ERROR>\n"
    | 2344 ->
        "<SYNTAX ERROR>\n"
    | 2345 ->
        "<SYNTAX ERROR>\n"
    | 1794 ->
        "<SYNTAX ERROR>\n"
    | 607 ->
        "<SYNTAX ERROR>\n"
    | 1368 ->
        "<SYNTAX ERROR>\n"
    | 1190 ->
        "<SYNTAX ERROR>\n"
    | 913 ->
        "Incomplete let binding\n"
    | 1291 ->
        "<SYNTAX ERROR>\n"
    | 1297 ->
        "<SYNTAX ERROR>\n"
    | 1292 ->
        "<SYNTAX ERROR>\n"
    | 1293 ->
        "<SYNTAX ERROR>\n"
    | 1294 ->
        "<SYNTAX ERROR>\n"
    | 1296 ->
        "<SYNTAX ERROR>\n"
    | 1166 ->
        "<SYNTAX ERROR>\n"
    | 914 ->
        "<SYNTAX ERROR>\n"
    | 915 ->
        "<SYNTAX ERROR>\n"
    | 1770 ->
        "<SYNTAX ERROR>\n"
    | 1771 ->
        "<SYNTAX ERROR>\n"
    | 1772 ->
        "<SYNTAX ERROR>\n"
    | 1773 ->
        "<SYNTAX ERROR>\n"
    | 1777 ->
        "<SYNTAX ERROR>\n"
    | 1774 ->
        "<SYNTAX ERROR>\n"
    | 1775 ->
        "<SYNTAX ERROR>\n"
    | 1776 ->
        "<SYNTAX ERROR>\n"
    | 916 ->
        "<SYNTAX ERROR>\n"
    | 917 ->
        "<SYNTAX ERROR>\n"
    | 926 ->
        "<SYNTAX ERROR>\n"
    | 1763 ->
        "<SYNTAX ERROR>\n"
    | 1764 ->
        "<SYNTAX ERROR>\n"
    | 1765 ->
        "<SYNTAX ERROR>\n"
    | 1781 ->
        "<SYNTAX ERROR>\n"
    | 1144 ->
        "<SYNTAX ERROR>\n"
    | 1145 ->
        "<SYNTAX ERROR>\n"
    | 1167 ->
        "<SYNTAX ERROR>\n"
    | 1287 ->
        "Defining a function?\nExpecting one of the following:\n  - \"=>\" to start the function body\n  - an identifier to add a function parameter\n  - \":\" to specify the return type\n"
    | 1255 ->
        "<SYNTAX ERROR>\n"
    | 1172 ->
        "<SYNTAX ERROR>\n"
    | 1173 ->
        "<SYNTAX ERROR>\n"
    | 1174 ->
        "<SYNTAX ERROR>\n"
    | 1175 ->
        "<SYNTAX ERROR>\n"
    | 1176 ->
        "Expecting an expression as function body\n"
    | 1250 ->
        "<SYNTAX ERROR>\n"
    | 1251 ->
        "Defining a function?\nExpecting \"=>\" to start the function body\n"
    | 1252 ->
        "<SYNTAX ERROR>\n"
    | 1253 ->
        "<SYNTAX ERROR>\n"
    | 1168 ->
        "<SYNTAX ERROR>\n"
    | 1169 ->
        "<SYNTAX ERROR>\n"
    | 1170 ->
        "<SYNTAX ERROR>\n"
    | 1171 ->
        "<SYNTAX ERROR>\n"
    | 1260 ->
        "<SYNTAX ERROR>\n"
    | 1283 ->
        "<SYNTAX ERROR>\n"
    | 1284 ->
        "<SYNTAX ERROR>\n"
    | 1264 ->
        "<SYNTAX ERROR>\n"
    | 1265 ->
        "<SYNTAX ERROR>\n"
    | 1266 ->
        "<SYNTAX ERROR>\n"
    | 1269 ->
        "<SYNTAX ERROR>\n"
    | 1270 ->
        "<SYNTAX ERROR>\n"
    | 1271 ->
        "<SYNTAX ERROR>\n"
    | 1274 ->
        "<SYNTAX ERROR>\n"
    | 1275 ->
        "<SYNTAX ERROR>\n"
    | 1276 ->
        "<SYNTAX ERROR>\n"
    | 1277 ->
        "<SYNTAX ERROR>\n"
    | 1273 ->
        "<SYNTAX ERROR>\n"
    | 1513 ->
        "<SYNTAX ERROR>\n"
    | 69 ->
        "<SYNTAX ERROR>\n"
    | 1714 ->
        "<SYNTAX ERROR>\n"
    | 191 ->
        "<SYNTAX ERROR>\n"
    | 2435 ->
        "<SYNTAX ERROR>\n"
    | 2436 ->
        "<SYNTAX ERROR>\n"
    | 2434 ->
        "<SYNTAX ERROR>\n"
    | 70 ->
        "<SYNTAX ERROR>\n"
    | 1065 ->
        "<SYNTAX ERROR>\n"
    | 1038 ->
        "<SYNTAX ERROR>\n"
    | 2611 ->
        "<SYNTAX ERROR>\n"
    | 18 ->
        "<SYNTAX ERROR>\n"
    | 164 ->
        "<SYNTAX ERROR>\n"
    | 2509 ->
        "<SYNTAX ERROR>\n"
    | 1801 ->
        "<SYNTAX ERROR>\n"
    | 1804 ->
        "<SYNTAX ERROR>\n"
    | 1806 ->
        "<SYNTAX ERROR>\n"
    | 1809 ->
        "Expecting a type name\n"
    | 176 ->
        "Expecting an expression\n"
    | 1394 ->
        "Expecting an expression\n"
    | 1370 ->
        "Expecting an expression\n"
    | 606 ->
        "<SYNTAX ERROR>\n"
    | 1712 ->
        "Expecting \"]\" to finish current floating attribute\n"
    | 1040 ->
        "<SYNTAX ERROR>\n"
    | 167 ->
        "Expecting one of the following:\n  - an list item\n  - \"]\" to finish this list\n"
    | 2215 ->
        "Expecting one of the following:\n  - \",\" to separate two items in a list\n  - \"]\" to finish this list\n"
    | 2216 ->
        "<SYNTAX ERROR>\n"
    | 2212 ->
        "<SYNTAX ERROR>\n"
    | 2213 ->
        "<SYNTAX ERROR>\n"
    | 169 ->
        "<SYNTAX ERROR>\n"
    | 170 ->
        "<SYNTAX ERROR>\n"
    | 583 ->
        "<SYNTAX ERROR>\n"
    | 171 ->
        "<SYNTAX ERROR>\n"
    | 2491 ->
        "<SYNTAX ERROR>\n"
    | 174 ->
        "<SYNTAX ERROR>\n"
    | 1349 ->
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
    | 1359 ->
        "<SYNTAX ERROR>\n"
    | 1360 ->
        "<SYNTAX ERROR>\n"
    | 1361 ->
        "<SYNTAX ERROR>\n"
    | 1362 ->
        "<SYNTAX ERROR>\n"
    | 1363 ->
        "<SYNTAX ERROR>\n"
    | 1366 ->
        "<SYNTAX ERROR>\n"
    | 1367 ->
        "<SYNTAX ERROR>\n"
    | 1459 ->
        "<SYNTAX ERROR>\n"
    | 1461 ->
        "<SYNTAX ERROR>\n"
    | 1462 ->
        "<SYNTAX ERROR>\n"
    | 1463 ->
        "<SYNTAX ERROR>\n"
    | 1358 ->
        "<SYNTAX ERROR>\n"
    | 2360 ->
        "<SYNTAX ERROR>\n"
    | 543 ->
        "<SYNTAX ERROR>\n"
    | 2290 ->
        "<SYNTAX ERROR>\n"
    | 2267 ->
        "<SYNTAX ERROR>\n"
    | 1464 ->
        "<SYNTAX ERROR>\n"
    | 1466 ->
        "<SYNTAX ERROR>\n"
    | 1467 ->
        "<SYNTAX ERROR>\n"
    | 1468 ->
        "<SYNTAX ERROR>\n"
    | 1469 ->
        "<SYNTAX ERROR>\n"
    | 1111 ->
        "<SYNTAX ERROR>\n"
    | 1113 ->
        "<SYNTAX ERROR>\n"
    | 1114 ->
        "<SYNTAX ERROR>\n"
    | 1471 ->
        "<SYNTAX ERROR>\n"
    | 1472 ->
        "<SYNTAX ERROR>\n"
    | 1473 ->
        "<SYNTAX ERROR>\n"
    | 1474 ->
        "<SYNTAX ERROR>\n"
    | 1477 ->
        "<SYNTAX ERROR>\n"
    | 1488 ->
        "<SYNTAX ERROR>\n"
    | 1478 ->
        "<SYNTAX ERROR>\n"
    | 1479 ->
        "<SYNTAX ERROR>\n"
    | 1480 ->
        "<SYNTAX ERROR>\n"
    | 1481 ->
        "<SYNTAX ERROR>\n"
    | 1482 ->
        "<SYNTAX ERROR>\n"
    | 1548 ->
        "<SYNTAX ERROR>\n"
    | 1492 ->
        "<SYNTAX ERROR>\n"
    | 1493 ->
        "<SYNTAX ERROR>\n"
    | 1494 ->
        "<SYNTAX ERROR>\n"
    | 1501 ->
        "<SYNTAX ERROR>\n"
    | 1502 ->
        "<SYNTAX ERROR>\n"
    | 1503 ->
        "<SYNTAX ERROR>\n"
    | 1495 ->
        "<SYNTAX ERROR>\n"
    | 1497 ->
        "<SYNTAX ERROR>\n"
    | 1498 ->
        "<SYNTAX ERROR>\n"
    | 1499 ->
        "<SYNTAX ERROR>\n"
    | 1500 ->
        "<SYNTAX ERROR>\n"
    | 1465 ->
        "<SYNTAX ERROR>\n"
    | 1849 ->
        "<SYNTAX ERROR>\n"
    | 1840 ->
        "<SYNTAX ERROR>\n"
    | 1838 ->
        "<SYNTAX ERROR>\n"
    | 1841 ->
        "<SYNTAX ERROR>\n"
    | 1842 ->
        "<SYNTAX ERROR>\n"
    | 1852 ->
        "<SYNTAX ERROR>\n"
    | 1853 ->
        "<SYNTAX ERROR>\n"
    | 591 ->
        "<SYNTAX ERROR>\n"
    | 2072 ->
        "<SYNTAX ERROR>\n"
    | 2078 ->
        "<SYNTAX ERROR>\n"
    | 2073 ->
        "<SYNTAX ERROR>\n"
    | 2074 ->
        "<SYNTAX ERROR>\n"
    | 2075 ->
        "<SYNTAX ERROR>\n"
    | 2077 ->
        "<SYNTAX ERROR>\n"
    | 2042 ->
        "<SYNTAX ERROR>\n"
    | 593 ->
        "<SYNTAX ERROR>\n"
    | 597 ->
        "<SYNTAX ERROR>\n"
    | 598 ->
        "<SYNTAX ERROR>\n"
    | 594 ->
        "<SYNTAX ERROR>\n"
    | 2277 ->
        "<SYNTAX ERROR>\n"
    | 2278 ->
        "<SYNTAX ERROR>\n"
    | 2281 ->
        "<SYNTAX ERROR>\n"
    | 2280 ->
        "<SYNTAX ERROR>\n"
    | 2043 ->
        "<SYNTAX ERROR>\n"
    | 2068 ->
        "<SYNTAX ERROR>\n"
    | 1484 ->
        "<SYNTAX ERROR>\n"
    | 1485 ->
        "<SYNTAX ERROR>\n"
    | 1486 ->
        "<SYNTAX ERROR>\n"
    | 1487 ->
        "<SYNTAX ERROR>\n"
    | 2044 ->
        "<SYNTAX ERROR>\n"
    | 2045 ->
        "<SYNTAX ERROR>\n"
    | 2046 ->
        "<SYNTAX ERROR>\n"
    | 2047 ->
        "<SYNTAX ERROR>\n"
    | 2049 ->
        "<SYNTAX ERROR>\n"
    | 2064 ->
        "<SYNTAX ERROR>\n"
    | 2065 ->
        "<SYNTAX ERROR>\n"
    | 2051 ->
        "<SYNTAX ERROR>\n"
    | 2052 ->
        "<SYNTAX ERROR>\n"
    | 2054 ->
        "<SYNTAX ERROR>\n"
    | 2055 ->
        "<SYNTAX ERROR>\n"
    | 2056 ->
        "<SYNTAX ERROR>\n"
    | 2059 ->
        "<SYNTAX ERROR>\n"
    | 2060 ->
        "<SYNTAX ERROR>\n"
    | 2061 ->
        "<SYNTAX ERROR>\n"
    | 2062 ->
        "<SYNTAX ERROR>\n"
    | 2058 ->
        "<SYNTAX ERROR>\n"
    | 2440 ->
        "Expecting \"}\" to finish the block\n"
    | 2174 ->
        "<SYNTAX ERROR>\n"
    | 1688 ->
        "<SYNTAX ERROR>\n"
    | 1121 ->
        "<SYNTAX ERROR>\n"
    | 1510 ->
        "<SYNTAX ERROR>\n"
    | 1507 ->
        "<SYNTAX ERROR>\n"
    | 1526 ->
        "<SYNTAX ERROR>\n"
    | 1527 ->
        "<SYNTAX ERROR>\n"
    | 1530 ->
        "<SYNTAX ERROR>\n"
    | 1163 ->
        "<SYNTAX ERROR>\n"
    | 1559 ->
        "<SYNTAX ERROR>\n"
    | 1562 ->
        "<SYNTAX ERROR>\n"
    | 1584 ->
        "<SYNTAX ERROR>\n"
    | 1563 ->
        "<SYNTAX ERROR>\n"
    | 1569 ->
        "<SYNTAX ERROR>\n"
    | 1570 ->
        "<SYNTAX ERROR>\n"
    | 1574 ->
        "<SYNTAX ERROR>\n"
    | 1575 ->
        "<SYNTAX ERROR>\n"
    | 1576 ->
        "<SYNTAX ERROR>\n"
    | 1565 ->
        "<SYNTAX ERROR>\n"
    | 1583 ->
        "<SYNTAX ERROR>\n"
    | 1580 ->
        "<SYNTAX ERROR>\n"
    | 1581 ->
        "<SYNTAX ERROR>\n"
    | 1582 ->
        "<SYNTAX ERROR>\n"
    | 1589 ->
        "<SYNTAX ERROR>\n"
    | 1590 ->
        "<SYNTAX ERROR>\n"
    | 1591 ->
        "<SYNTAX ERROR>\n"
    | 1311 ->
        "<SYNTAX ERROR>\n"
    | 1323 ->
        "<SYNTAX ERROR>\n"
    | 1549 ->
        "<SYNTAX ERROR>\n"
    | 1532 ->
        "<SYNTAX ERROR>\n"
    | 1533 ->
        "<SYNTAX ERROR>\n"
    | 1164 ->
        "<SYNTAX ERROR>\n"
    | 1345 ->
        "<SYNTAX ERROR>\n"
    | 1553 ->
        "<SYNTAX ERROR>\n"
    | 1165 ->
        "<SYNTAX ERROR>\n"
    | 1341 ->
        "<SYNTAX ERROR>\n"
    | 1342 ->
        "<SYNTAX ERROR>\n"
    | 1301 ->
        "<SYNTAX ERROR>\n"
    | 1303 ->
        "<SYNTAX ERROR>\n"
    | 1304 ->
        "<SYNTAX ERROR>\n"
    | 1329 ->
        "<SYNTAX ERROR>\n"
    | 1305 ->
        "<SYNTAX ERROR>\n"
    | 1306 ->
        "<SYNTAX ERROR>\n"
    | 1307 ->
        "<SYNTAX ERROR>\n"
    | 1531 ->
        "<SYNTAX ERROR>\n"
    | 1833 ->
        "<SYNTAX ERROR>\n"
    | 1834 ->
        "<SYNTAX ERROR>\n"
    | 1835 ->
        "<SYNTAX ERROR>\n"
    | 1537 ->
        "<SYNTAX ERROR>\n"
    | 1538 ->
        "<SYNTAX ERROR>\n"
    | 1539 ->
        "<SYNTAX ERROR>\n"
    | 1336 ->
        "<SYNTAX ERROR>\n"
    | 1337 ->
        "<SYNTAX ERROR>\n"
    | 1348 ->
        "<SYNTAX ERROR>\n"
    | 565 ->
        "<SYNTAX ERROR>\n"
    | 1042 ->
        "<SYNTAX ERROR>\n"
    | 919 ->
        "<SYNTAX ERROR>\n"
    | 863 ->
        "<SYNTAX ERROR>\n"
    | 864 ->
        "<SYNTAX ERROR>\n"
    | 1895 ->
        "<SYNTAX ERROR>\n"
    | 1897 ->
        "<SYNTAX ERROR>\n"
    | 1900 ->
        "<SYNTAX ERROR>\n"
    | 2192 ->
        "<SYNTAX ERROR>\n"
    | 912 ->
        "<SYNTAX ERROR>\n"
    | 1786 ->
        "<SYNTAX ERROR>\n"
    | 411 ->
        "<SYNTAX ERROR>\n"
    | 412 ->
        "<SYNTAX ERROR>\n"
    | 2400 ->
        "<SYNTAX ERROR>\n"
    | 2402 ->
        "<SYNTAX ERROR>\n"
    | 1898 ->
        "<SYNTAX ERROR>\n"
    | 2404 ->
        "<SYNTAX ERROR>\n"
    | 1903 ->
        "<SYNTAX ERROR>\n"
    | 1904 ->
        "<SYNTAX ERROR>\n"
    | 2406 ->
        "<SYNTAX ERROR>\n"
    | 2013 ->
        "<SYNTAX ERROR>\n"
    | 1988 ->
        "<SYNTAX ERROR>\n"
    | 1989 ->
        "<SYNTAX ERROR>\n"
    | 1990 ->
        "<SYNTAX ERROR>\n"
    | 1992 ->
        "<SYNTAX ERROR>\n"
    | 1995 ->
        "<SYNTAX ERROR>\n"
    | 2002 ->
        "<SYNTAX ERROR>\n"
    | 2005 ->
        "<SYNTAX ERROR>\n"
    | 2006 ->
        "<SYNTAX ERROR>\n"
    | 2195 ->
        "<SYNTAX ERROR>\n"
    | 2196 ->
        "<SYNTAX ERROR>\n"
    | 558 ->
        "<SYNTAX ERROR>\n"
    | 559 ->
        "<SYNTAX ERROR>\n"
    | 2311 ->
        "<SYNTAX ERROR>\n"
    | 2313 ->
        "<SYNTAX ERROR>\n"
    | 1993 ->
        "<SYNTAX ERROR>\n"
    | 2315 ->
        "<SYNTAX ERROR>\n"
    | 1998 ->
        "<SYNTAX ERROR>\n"
    | 1999 ->
        "<SYNTAX ERROR>\n"
    | 2317 ->
        "<SYNTAX ERROR>\n"
    | 2008 ->
        "<SYNTAX ERROR>\n"
    | 2009 ->
        "<SYNTAX ERROR>\n"
    | 1907 ->
        "<SYNTAX ERROR>\n"
    | 1983 ->
        "<SYNTAX ERROR>\n"
    | 1984 ->
        "<SYNTAX ERROR>\n"
    | 1985 ->
        "<SYNTAX ERROR>\n"
    | 1987 ->
        "<SYNTAX ERROR>\n"
    | 419 ->
        "<SYNTAX ERROR>\n"
    | 2144 ->
        "<SYNTAX ERROR>\n"
    | 422 ->
        "<SYNTAX ERROR>\n"
    | 436 ->
        "<SYNTAX ERROR>\n"
    | 425 ->
        "<SYNTAX ERROR>\n"
    | 2364 ->
        "<SYNTAX ERROR>\n"
    | 2368 ->
        "<SYNTAX ERROR>\n"
    | 2365 ->
        "<SYNTAX ERROR>\n"
    | 2366 ->
        "<SYNTAX ERROR>\n"
    | 427 ->
        "<SYNTAX ERROR>\n"
    | 429 ->
        "<SYNTAX ERROR>\n"
    | 431 ->
        "<SYNTAX ERROR>\n"
    | 433 ->
        "<SYNTAX ERROR>\n"
    | 2151 ->
        "<SYNTAX ERROR>\n"
    | 437 ->
        "<SYNTAX ERROR>\n"
    | 513 ->
        "<SYNTAX ERROR>\n"
    | 514 ->
        "<SYNTAX ERROR>\n"
    | 516 ->
        "<SYNTAX ERROR>\n"
    | 521 ->
        "<SYNTAX ERROR>\n"
    | 522 ->
        "<SYNTAX ERROR>\n"
    | 438 ->
        "<SYNTAX ERROR>\n"
    | 482 ->
        "<SYNTAX ERROR>\n"
    | 468 ->
        "<SYNTAX ERROR>\n"
    | 463 ->
        "<SYNTAX ERROR>\n"
    | 464 ->
        "<SYNTAX ERROR>\n"
    | 490 ->
        "<SYNTAX ERROR>\n"
    | 507 ->
        "<SYNTAX ERROR>\n"
    | 528 ->
        "<SYNTAX ERROR>\n"
    | 529 ->
        "<SYNTAX ERROR>\n"
    | 530 ->
        "<SYNTAX ERROR>\n"
    | 531 ->
        "<SYNTAX ERROR>\n"
    | 2152 ->
        "<SYNTAX ERROR>\n"
    | 2154 ->
        "<SYNTAX ERROR>\n"
    | 2143 ->
        "<SYNTAX ERROR>\n"
    | 1908 ->
        "<SYNTAX ERROR>\n"
    | 1909 ->
        "<SYNTAX ERROR>\n"
    | 1912 ->
        "<SYNTAX ERROR>\n"
    | 1913 ->
        "<SYNTAX ERROR>\n"
    | 1915 ->
        "<SYNTAX ERROR>\n"
    | 1979 ->
        "<SYNTAX ERROR>\n"
    | 1980 ->
        "<SYNTAX ERROR>\n"
    | 1981 ->
        "<SYNTAX ERROR>\n"
    | 2029 ->
        "<SYNTAX ERROR>\n"
    | 2030 ->
        "<SYNTAX ERROR>\n"
    | 2031 ->
        "<SYNTAX ERROR>\n"
    | 2032 ->
        "<SYNTAX ERROR>\n"
    | 2033 ->
        "<SYNTAX ERROR>\n"
    | 2034 ->
        "<SYNTAX ERROR>\n"
    | 2035 ->
        "<SYNTAX ERROR>\n"
    | 1982 ->
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
    | 2037 ->
        "<SYNTAX ERROR>\n"
    | 2158 ->
        "<SYNTAX ERROR>\n"
    | 2159 ->
        "<SYNTAX ERROR>\n"
    | 2082 ->
        "<SYNTAX ERROR>\n"
    | 2085 ->
        "<SYNTAX ERROR>\n"
    | 2086 ->
        "<SYNTAX ERROR>\n"
    | 2087 ->
        "<SYNTAX ERROR>\n"
    | 2088 ->
        "<SYNTAX ERROR>\n"
    | 2089 ->
        "<SYNTAX ERROR>\n"
    | 2090 ->
        "<SYNTAX ERROR>\n"
    | 2094 ->
        "<SYNTAX ERROR>\n"
    | 2099 ->
        "<SYNTAX ERROR>\n"
    | 455 ->
        "<SYNTAX ERROR>\n"
    | 456 ->
        "<SYNTAX ERROR>\n"
    | 2100 ->
        "<SYNTAX ERROR>\n"
    | 2101 ->
        "<SYNTAX ERROR>\n"
    | 2102 ->
        "<SYNTAX ERROR>\n"
    | 441 ->
        "<SYNTAX ERROR>\n"
    | 471 ->
        "<SYNTAX ERROR>\n"
    | 2107 ->
        "<SYNTAX ERROR>\n"
    | 2116 ->
        "<SYNTAX ERROR>\n"
    | 2108 ->
        "<SYNTAX ERROR>\n"
    | 2109 ->
        "<SYNTAX ERROR>\n"
    | 2111 ->
        "<SYNTAX ERROR>\n"
    | 2112 ->
        "<SYNTAX ERROR>\n"
    | 2113 ->
        "<SYNTAX ERROR>\n"
    | 2163 ->
        "<SYNTAX ERROR>\n"
    | 2164 ->
        "<SYNTAX ERROR>\n"
    | 2118 ->
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
    | 2132 ->
        "<SYNTAX ERROR>\n"
    | 2133 ->
        "<SYNTAX ERROR>\n"
    | 2119 ->
        "<SYNTAX ERROR>\n"
    | 2120 ->
        "<SYNTAX ERROR>\n"
    | 2168 ->
        "<SYNTAX ERROR>\n"
    | 2169 ->
        "<SYNTAX ERROR>\n"
    | 2121 ->
        "<SYNTAX ERROR>\n"
    | 2122 ->
        "<SYNTAX ERROR>\n"
    | 2123 ->
        "<SYNTAX ERROR>\n"
    | 2124 ->
        "<SYNTAX ERROR>\n"
    | 547 ->
        "<SYNTAX ERROR>\n"
    | 548 ->
        "<SYNTAX ERROR>\n"
    | 552 ->
        "<SYNTAX ERROR>\n"
    | 553 ->
        "<SYNTAX ERROR>\n"
    | 2329 ->
        "<SYNTAX ERROR>\n"
    | 2331 ->
        "<SYNTAX ERROR>\n"
    | 2332 ->
        "<SYNTAX ERROR>\n"
    | 2333 ->
        "<SYNTAX ERROR>\n"
    | 865 ->
        "<SYNTAX ERROR>\n"
    | 866 ->
        "<SYNTAX ERROR>\n"
    | 868 ->
        "<SYNTAX ERROR>\n"
    | 869 ->
        "<SYNTAX ERROR>\n"
    | 1876 ->
        "<SYNTAX ERROR>\n"
    | 874 ->
        "<SYNTAX ERROR>\n"
    | 875 ->
        "<SYNTAX ERROR>\n"
    | 876 ->
        "<SYNTAX ERROR>\n"
    | 877 ->
        "<SYNTAX ERROR>\n"
    | 878 ->
        "<SYNTAX ERROR>\n"
    | 1875 ->
        "<SYNTAX ERROR>\n"
    | 870 ->
        "<SYNTAX ERROR>\n"
    | 871 ->
        "<SYNTAX ERROR>\n"
    | 872 ->
        "<SYNTAX ERROR>\n"
    | 873 ->
        "<SYNTAX ERROR>\n"
    | 1892 ->
        "<SYNTAX ERROR>\n"
    | 631 ->
        "<SYNTAX ERROR>\n"
    | 852 ->
        "<SYNTAX ERROR>\n"
    | 854 ->
        "<SYNTAX ERROR>\n"
    | 855 ->
        "<SYNTAX ERROR>\n"
    | 1885 ->
        "<SYNTAX ERROR>\n"
    | 1886 ->
        "<SYNTAX ERROR>\n"
    | 1887 ->
        "<SYNTAX ERROR>\n"
    | 1888 ->
        "<SYNTAX ERROR>\n"
    | 1889 ->
        "<SYNTAX ERROR>\n"
    | 1890 ->
        "<SYNTAX ERROR>\n"
    | 879 ->
        "<SYNTAX ERROR>\n"
    | 880 ->
        "<SYNTAX ERROR>\n"
    | 881 ->
        "<SYNTAX ERROR>\n"
    | 882 ->
        "<SYNTAX ERROR>\n"
    | 885 ->
        "<SYNTAX ERROR>\n"
    | 886 ->
        "<SYNTAX ERROR>\n"
    | 1045 ->
        "<SYNTAX ERROR>\n"
    | 1046 ->
        "<SYNTAX ERROR>\n"
    | 1047 ->
        "<SYNTAX ERROR>\n"
    | 1048 ->
        "<SYNTAX ERROR>\n"
    | 1049 ->
        "<SYNTAX ERROR>\n"
    | 1050 ->
        "<SYNTAX ERROR>\n"
    | 1054 ->
        "<SYNTAX ERROR>\n"
    | 1059 ->
        "<SYNTAX ERROR>\n"
    | 951 ->
        "<SYNTAX ERROR>\n"
    | 952 ->
        "<SYNTAX ERROR>\n"
    | 1060 ->
        "<SYNTAX ERROR>\n"
    | 1061 ->
        "<SYNTAX ERROR>\n"
    | 1062 ->
        "<SYNTAX ERROR>\n"
    | 949 ->
        "<SYNTAX ERROR>\n"
    | 941 ->
        "<SYNTAX ERROR>\n"
    | 1067 ->
        "<SYNTAX ERROR>\n"
    | 1156 ->
        "<SYNTAX ERROR>\n"
    | 1069 ->
        "<SYNTAX ERROR>\n"
    | 1070 ->
        "<SYNTAX ERROR>\n"
    | 1072 ->
        "<SYNTAX ERROR>\n"
    | 1075 ->
        "<SYNTAX ERROR>\n"
    | 1707 ->
        "<SYNTAX ERROR>\n"
    | 1149 ->
        "<SYNTAX ERROR>\n"
    | 1150 ->
        "<SYNTAX ERROR>\n"
    | 1158 ->
        "<SYNTAX ERROR>\n"
    | 1666 ->
        "<SYNTAX ERROR>\n"
    | 1667 ->
        "<SYNTAX ERROR>\n"
    | 1668 ->
        "<SYNTAX ERROR>\n"
    | 1669 ->
        "<SYNTAX ERROR>\n"
    | 1630 ->
        "<SYNTAX ERROR>\n"
    | 1631 ->
        "<SYNTAX ERROR>\n"
    | 1670 ->
        "<SYNTAX ERROR>\n"
    | 1671 ->
        "<SYNTAX ERROR>\n"
    | 1673 ->
        "<SYNTAX ERROR>\n"
    | 1635 ->
        "<SYNTAX ERROR>\n"
    | 1636 ->
        "<SYNTAX ERROR>\n"
    | 1677 ->
        "<SYNTAX ERROR>\n"
    | 1674 ->
        "<SYNTAX ERROR>\n"
    | 1675 ->
        "<SYNTAX ERROR>\n"
    | 1077 ->
        "<SYNTAX ERROR>\n"
    | 1085 ->
        "<SYNTAX ERROR>\n"
    | 1086 ->
        "<SYNTAX ERROR>\n"
    | 1087 ->
        "<SYNTAX ERROR>\n"
    | 1088 ->
        "<SYNTAX ERROR>\n"
    | 1089 ->
        "<SYNTAX ERROR>\n"
    | 1091 ->
        "<SYNTAX ERROR>\n"
    | 1092 ->
        "<SYNTAX ERROR>\n"
    | 1093 ->
        "<SYNTAX ERROR>\n"
    | 1094 ->
        "<SYNTAX ERROR>\n"
    | 1100 ->
        "<SYNTAX ERROR>\n"
    | 1101 ->
        "<SYNTAX ERROR>\n"
    | 1103 ->
        "<SYNTAX ERROR>\n"
    | 1104 ->
        "<SYNTAX ERROR>\n"
    | 1108 ->
        "<SYNTAX ERROR>\n"
    | 1106 ->
        "<SYNTAX ERROR>\n"
    | 1109 ->
        "<SYNTAX ERROR>\n"
    | 1110 ->
        "<SYNTAX ERROR>\n"
    | 1081 ->
        "<SYNTAX ERROR>\n"
    | 1690 ->
        "<SYNTAX ERROR>\n"
    | 1691 ->
        "<SYNTAX ERROR>\n"
    | 1694 ->
        "<SYNTAX ERROR>\n"
    | 1078 ->
        "<SYNTAX ERROR>\n"
    | 1079 ->
        "<SYNTAX ERROR>\n"
    | 1084 ->
        "<SYNTAX ERROR>\n"
    | 1658 ->
        "<SYNTAX ERROR>\n"
    | 1159 ->
        "<SYNTAX ERROR>\n"
    | 1160 ->
        "<SYNTAX ERROR>\n"
    | 1161 ->
        "<SYNTAX ERROR>\n"
    | 1162 ->
        "<SYNTAX ERROR>\n"
    | 1596 ->
        "<SYNTAX ERROR>\n"
    | 1599 ->
        "<SYNTAX ERROR>\n"
    | 1617 ->
        "<SYNTAX ERROR>\n"
    | 1618 ->
        "<SYNTAX ERROR>\n"
    | 1154 ->
        "<SYNTAX ERROR>\n"
    | 1155 ->
        "<SYNTAX ERROR>\n"
    | 1627 ->
        "<SYNTAX ERROR>\n"
    | 1603 ->
        "<SYNTAX ERROR>\n"
    | 1607 ->
        "<SYNTAX ERROR>\n"
    | 1609 ->
        "<SYNTAX ERROR>\n"
    | 1610 ->
        "<SYNTAX ERROR>\n"
    | 1620 ->
        "<SYNTAX ERROR>\n"
    | 1611 ->
        "<SYNTAX ERROR>\n"
    | 1612 ->
        "<SYNTAX ERROR>\n"
    | 1613 ->
        "<SYNTAX ERROR>\n"
    | 1628 ->
        "<SYNTAX ERROR>\n"
    | 1645 ->
        "<SYNTAX ERROR>\n"
    | 1629 ->
        "<SYNTAX ERROR>\n"
    | 1654 ->
        "<SYNTAX ERROR>\n"
    | 1637 ->
        "<SYNTAX ERROR>\n"
    | 1655 ->
        "<SYNTAX ERROR>\n"
    | 1656 ->
        "<SYNTAX ERROR>\n"
    | 1644 ->
        "<SYNTAX ERROR>\n"
    | 1641 ->
        "<SYNTAX ERROR>\n"
    | 1642 ->
        "<SYNTAX ERROR>\n"
    | 1643 ->
        "<SYNTAX ERROR>\n"
    | 1650 ->
        "<SYNTAX ERROR>\n"
    | 1651 ->
        "<SYNTAX ERROR>\n"
    | 1652 ->
        "<SYNTAX ERROR>\n"
    | 572 ->
        "<SYNTAX ERROR>\n"
    | 151 ->
        "<SYNTAX ERROR>\n"
    | 893 ->
        "<SYNTAX ERROR>\n"
    | 2624 ->
        "<SYNTAX ERROR>\n"
    | 934 ->
        "<SYNTAX ERROR>\n"
    | 929 ->
        "<SYNTAX ERROR>\n"
    | 2626 ->
        "<SYNTAX ERROR>\n"
    | 1744 ->
        "<SYNTAX ERROR>\n"
    | 935 ->
        "<SYNTAX ERROR>\n"
    | 946 ->
        "<SYNTAX ERROR>\n"
    | 937 ->
        "<SYNTAX ERROR>\n"
    | 938 ->
        "<SYNTAX ERROR>\n"
    | 2625 ->
        "<SYNTAX ERROR>\n"
    | 961 ->
        "<SYNTAX ERROR>\n"
    | 962 ->
        "<SYNTAX ERROR>\n"
    | 964 ->
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
    | 1013 ->
        "<SYNTAX ERROR>\n"
    | 1014 ->
        "<SYNTAX ERROR>\n"
    | 1015 ->
        "<SYNTAX ERROR>\n"
    | 1021 ->
        "<SYNTAX ERROR>\n"
    | 1023 ->
        "<SYNTAX ERROR>\n"
    | 1016 ->
        "<SYNTAX ERROR>\n"
    | 1017 ->
        "<SYNTAX ERROR>\n"
    | 1028 ->
        "<SYNTAX ERROR>\n"
    | 1029 ->
        "<SYNTAX ERROR>\n"
    | 1030 ->
        "<SYNTAX ERROR>\n"
    | 1031 ->
        "<SYNTAX ERROR>\n"
    | 1745 ->
        "<SYNTAX ERROR>\n"
    | 1746 ->
        "<SYNTAX ERROR>\n"
    | 797 ->
        "<SYNTAX ERROR>\n"
    | 1034 ->
        "<SYNTAX ERROR>\n"
    | 1035 ->
        "<SYNTAX ERROR>\n"
    | 1716 ->
        "<SYNTAX ERROR>\n"
    | 969 ->
        "<SYNTAX ERROR>\n"
    | 970 ->
        "<SYNTAX ERROR>\n"
    | 972 ->
        "<SYNTAX ERROR>\n"
    | 973 ->
        "<SYNTAX ERROR>\n"
    | 979 ->
        "<SYNTAX ERROR>\n"
    | 977 ->
        "<SYNTAX ERROR>\n"
    | 975 ->
        "<SYNTAX ERROR>\n"
    | 992 ->
        "<SYNTAX ERROR>\n"
    | 993 ->
        "<SYNTAX ERROR>\n"
    | 985 ->
        "<SYNTAX ERROR>\n"
    | 986 ->
        "<SYNTAX ERROR>\n"
    | 990 ->
        "<SYNTAX ERROR>\n"
    | 76 ->
        "<SYNTAX ERROR>\n"
    | 989 ->
        "<SYNTAX ERROR>\n"
    | 987 ->
        "<SYNTAX ERROR>\n"
    | 117 ->
        "<SYNTAX ERROR>\n"
    | 256 ->
        "<SYNTAX ERROR>\n"
    | 996 ->
        "<SYNTAX ERROR>\n"
    | 997 ->
        "<SYNTAX ERROR>\n"
    | 257 ->
        "<SYNTAX ERROR>\n"
    | 258 ->
        "<SYNTAX ERROR>\n"
    | 409 ->
        "<SYNTAX ERROR>\n"
    | 410 ->
        "<SYNTAX ERROR>\n"
    | 2408 ->
        "<SYNTAX ERROR>\n"
    | 554 ->
        "<SYNTAX ERROR>\n"
    | 2323 ->
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
    | 1926 ->
        "<SYNTAX ERROR>\n"
    | 1927 ->
        "<SYNTAX ERROR>\n"
    | 1929 ->
        "<SYNTAX ERROR>\n"
    | 1934 ->
        "<SYNTAX ERROR>\n"
    | 1932 ->
        "<SYNTAX ERROR>\n"
    | 1930 ->
        "<SYNTAX ERROR>\n"
    | 1955 ->
        "<SYNTAX ERROR>\n"
    | 1956 ->
        "<SYNTAX ERROR>\n"
    | 1935 ->
        "<SYNTAX ERROR>\n"
    | 1936 ->
        "<SYNTAX ERROR>\n"
    | 1953 ->
        "<SYNTAX ERROR>\n"
    | 1938 ->
        "<SYNTAX ERROR>\n"
    | 1952 ->
        "<SYNTAX ERROR>\n"
    | 1937 ->
        "<SYNTAX ERROR>\n"
    | 1939 ->
        "<SYNTAX ERROR>\n"
    | 1940 ->
        "<SYNTAX ERROR>\n"
    | 1943 ->
        "<SYNTAX ERROR>\n"
    | 555 ->
        "<SYNTAX ERROR>\n"
    | 648 ->
        "<SYNTAX ERROR>\n"
    | 1960 ->
        "<SYNTAX ERROR>\n"
    | 1961 ->
        "<SYNTAX ERROR>\n"
    | 649 ->
        "<SYNTAX ERROR>\n"
    | 650 ->
        "<SYNTAX ERROR>\n"
    | 556 ->
        "<SYNTAX ERROR>\n"
    | 557 ->
        "<SYNTAX ERROR>\n"
    | 2319 ->
        "<SYNTAX ERROR>\n"
    | 1916 ->
        "<SYNTAX ERROR>\n"
    | 1970 ->
        "<SYNTAX ERROR>\n"
    | 1971 ->
        "<SYNTAX ERROR>\n"
    | 1972 ->
        "<SYNTAX ERROR>\n"
    | 1973 ->
        "<SYNTAX ERROR>\n"
    | 1974 ->
        "<SYNTAX ERROR>\n"
    | 1975 ->
        "<SYNTAX ERROR>\n"
    | 1924 ->
        "<SYNTAX ERROR>\n"
    | 2320 ->
        "<SYNTAX ERROR>\n"
    | 1917 ->
        "<SYNTAX ERROR>\n"
    | 927 ->
        "<SYNTAX ERROR>\n"
    | 1738 ->
        "<SYNTAX ERROR>\n"
    | 1737 ->
        "<SYNTAX ERROR>\n"
    | 1719 ->
        "<SYNTAX ERROR>\n"
    | 1720 ->
        "<SYNTAX ERROR>\n"
    | 1721 ->
        "<SYNTAX ERROR>\n"
    | 1722 ->
        "<SYNTAX ERROR>\n"
    | 1723 ->
        "<SYNTAX ERROR>\n"
    | 1726 ->
        "<SYNTAX ERROR>\n"
    | 948 ->
        "<SYNTAX ERROR>\n"
    | 1729 ->
        "<SYNTAX ERROR>\n"
    | 1730 ->
        "<SYNTAX ERROR>\n"
    | 1750 ->
        "<SYNTAX ERROR>\n"
    | 1732 ->
        "<SYNTAX ERROR>\n"
    | 1657 ->
        "<SYNTAX ERROR>\n"
    | 1733 ->
        "<SYNTAX ERROR>\n"
    | 1751 ->
        "<SYNTAX ERROR>\n"
    | 1752 ->
        "<SYNTAX ERROR>\n"
    | 2631 ->
        "<SYNTAX ERROR>\n"
    | 2633 ->
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
    | 2557 ->
        "<SYNTAX ERROR>\n"
    | 110 ->
        "<SYNTAX ERROR>\n"
    | 2555 ->
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
    | 2552 ->
        "<SYNTAX ERROR>\n"
    | 2553 ->
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
    | 2544 ->
        "<SYNTAX ERROR>\n"
    | 2545 ->
        "<SYNTAX ERROR>\n"
    | 2546 ->
        "<SYNTAX ERROR>\n"
    | 2548 ->
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
    | 496 ->
        "<SYNTAX ERROR>\n"
    | 495 ->
        "<SYNTAX ERROR>\n"
    | 498 ->
        "<SYNTAX ERROR>\n"
    | 295 ->
        "<SYNTAX ERROR>\n"
    | 137 ->
        "<SYNTAX ERROR>\n"
    | 143 ->
        "<SYNTAX ERROR>\n"
    | 2537 ->
        "<SYNTAX ERROR>\n"
    | 2539 ->
        "<SYNTAX ERROR>\n"
    | 2540 ->
        "<SYNTAX ERROR>\n"
    | 157 ->
        "<SYNTAX ERROR>\n"
    | 145 ->
        "<SYNTAX ERROR>\n"
    | 146 ->
        "<SYNTAX ERROR>\n"
    | 2535 ->
        "<SYNTAX ERROR>\n"
    | 148 ->
        "<SYNTAX ERROR>\n"
    | 149 ->
        "<SYNTAX ERROR>\n"
    | 2531 ->
        "<SYNTAX ERROR>\n"
    | 2532 ->
        "<SYNTAX ERROR>\n"
    | 2533 ->
        "<SYNTAX ERROR>\n"
    | 150 ->
        "<SYNTAX ERROR>\n"
    | 155 ->
        "<SYNTAX ERROR>\n"
    | 2529 ->
        "<SYNTAX ERROR>\n"
    | 2525 ->
        "<SYNTAX ERROR>\n"
    | 2522 ->
        "<SYNTAX ERROR>\n"
    | 2635 ->
        "<SYNTAX ERROR>\n"
    | 2637 ->
        "<SYNTAX ERROR>\n"
    | 2639 ->
        "<SYNTAX ERROR>\n"
    | 2640 ->
        "<SYNTAX ERROR>\n"
    | 792 ->
        "<SYNTAX ERROR>\n"
    | 794 ->
        "<SYNTAX ERROR>\n"
    | 805 ->
        "<SYNTAX ERROR>\n"
    | 795 ->
        "<SYNTAX ERROR>\n"
    | 787 ->
        "<SYNTAX ERROR>\n"
    | 632 ->
        "<SYNTAX ERROR>\n"
    | 596 ->
        "<SYNTAX ERROR>\n"
    | 768 ->
        "<SYNTAX ERROR>\n"
    | 193 ->
        "<SYNTAX ERROR>\n"
    | 198 ->
        "<SYNTAX ERROR>\n"
    | 204 ->
        "<SYNTAX ERROR>\n"
    | 210 ->
        "<SYNTAX ERROR>\n"
    | 812 ->
        "<SYNTAX ERROR>\n"
    | 813 ->
        "<SYNTAX ERROR>\n"
    | 842 ->
        "<SYNTAX ERROR>\n"
    | 803 ->
        "<SYNTAX ERROR>\n"
    | 731 ->
        "<SYNTAX ERROR>\n"
    | 718 ->
        "<SYNTAX ERROR>\n"
    | 720 ->
        "<SYNTAX ERROR>\n"
    | 847 ->
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
    | 2419 ->
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
    | 738 ->
        "<SYNTAX ERROR>\n"
    | 721 ->
        "<SYNTAX ERROR>\n"
    | 723 ->
        "<SYNTAX ERROR>\n"
    | 708 ->
        "<SYNTAX ERROR>\n"
    | 677 ->
        "<SYNTAX ERROR>\n"
    | 698 ->
        "<SYNTAX ERROR>\n"
    | 689 ->
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
    | 2423 ->
        "<SYNTAX ERROR>\n"
    | 2424 ->
        "<SYNTAX ERROR>\n"
    | 234 ->
        "<SYNTAX ERROR>\n"
    | 239 ->
        "<SYNTAX ERROR>\n"
    | 634 ->
        "<SYNTAX ERROR>\n"
    | 640 ->
        "<SYNTAX ERROR>\n"
    | 729 ->
        "<SYNTAX ERROR>\n"
    | 818 ->
        "<SYNTAX ERROR>\n"
    | 641 ->
        "<SYNTAX ERROR>\n"
    | 642 ->
        "<SYNTAX ERROR>\n"
    | 644 ->
        "<SYNTAX ERROR>\n"
    | 834 ->
        "<SYNTAX ERROR>\n"
    | 835 ->
        "<SYNTAX ERROR>\n"
    | 836 ->
        "<SYNTAX ERROR>\n"
    | 837 ->
        "<SYNTAX ERROR>\n"
    | 838 ->
        "<SYNTAX ERROR>\n"
    | 839 ->
        "<SYNTAX ERROR>\n"
    | 656 ->
        "<SYNTAX ERROR>\n"
    | 831 ->
        "<SYNTAX ERROR>\n"
    | 659 ->
        "<SYNTAX ERROR>\n"
    | 826 ->
        "<SYNTAX ERROR>\n"
    | 660 ->
        "<SYNTAX ERROR>\n"
    | 665 ->
        "<SYNTAX ERROR>\n"
    | 676 ->
        "<SYNTAX ERROR>\n"
    | 684 ->
        "<SYNTAX ERROR>\n"
    | 692 ->
        "<SYNTAX ERROR>\n"
    | 2426 ->
        "<SYNTAX ERROR>\n"
    | 2427 ->
        "<SYNTAX ERROR>\n"
    | 2428 ->
        "<SYNTAX ERROR>\n"
    | 2429 ->
        "<SYNTAX ERROR>\n"
    | 2430 ->
        "<SYNTAX ERROR>\n"
    | 2431 ->
        "<SYNTAX ERROR>\n"
    | 732 ->
        "<SYNTAX ERROR>\n"
    | 743 ->
        "<SYNTAX ERROR>\n"
    | 746 ->
        "<SYNTAX ERROR>\n"
    | 736 ->
        "<SYNTAX ERROR>\n"
    | 737 ->
        "<SYNTAX ERROR>\n"
    | 657 ->
        "<SYNTAX ERROR>\n"
    | 658 ->
        "<SYNTAX ERROR>\n"
    | 747 ->
        "<SYNTAX ERROR>\n"
    | 750 ->
        "<SYNTAX ERROR>\n"
    | 758 ->
        "<SYNTAX ERROR>\n"
    | 754 ->
        "<SYNTAX ERROR>\n"
    | 757 ->
        "<SYNTAX ERROR>\n"
    | 755 ->
        "Expecting a valid list identifier\n"
    | 756 ->
        "<SYNTAX ERROR>\n"
    | 759 ->
        "<SYNTAX ERROR>\n"
    | 662 ->
        "<SYNTAX ERROR>\n"
    | 663 ->
        "<SYNTAX ERROR>\n"
    | 674 ->
        "<SYNTAX ERROR>\n"
    | 669 ->
        "<SYNTAX ERROR>\n"
    | 670 ->
        "<SYNTAX ERROR>\n"
    | 760 ->
        "<SYNTAX ERROR>\n"
    | 675 ->
        "<SYNTAX ERROR>\n"
    | 824 ->
        "<SYNTAX ERROR>\n"
    | 763 ->
        "<SYNTAX ERROR>\n"
    | 779 ->
        "<SYNTAX ERROR>\n"
    | 781 ->
        "<SYNTAX ERROR>\n"
    | 2643 ->
        "<SYNTAX ERROR>\n"
    | 2657 ->
        "<SYNTAX ERROR>\n"
    | 2644 ->
        "<SYNTAX ERROR>\n"
    | 2645 ->
        "<SYNTAX ERROR>\n"
    | 2651 ->
        "<SYNTAX ERROR>\n"
    | 2652 ->
        "<SYNTAX ERROR>\n"
    | 2655 ->
        "<SYNTAX ERROR>\n"
    | 2660 ->
        "<SYNTAX ERROR>\n"
    | 2667 ->
        "<SYNTAX ERROR>\n"
    | 2666 ->
        "<SYNTAX ERROR>\n"
    | 2663 ->
        "<SYNTAX ERROR>\n"
    | 2664 ->
        "<SYNTAX ERROR>\n"
    | _ ->
        raise Not_found
