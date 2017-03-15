
(* This file was auto-generated based on "src/reason_parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "<SYNTAX ERROR>\n"
    | 733 ->
        "Expecting one of the following:\n  - an identifier to access a member of an object\n  - \"[\" + expression + \"]\" to access an element of a list\n  - \"(\" + expression + \")\"\n  - \"{\" + expression + \"}\"\n"
    | 734 ->
        "Expecting an expression\n"
    | 2208 ->
        "Expecting one of the following:\n  - an infix operation to connect two expressions\n  - \")\" to close the block\n"
    | 2210 ->
        "Expecting an expression\n"
    | 2211 ->
        "Expecting one of the following:\n  - an infix operation to connect two expressions\n  - \"}\" to close the block\n"
    | 2213 ->
        "Expecting an expression\n"
    | 2214 ->
        "Expecting one of the following:\n  - an infix operation to connect two expressions\n  - \"}\" to close the block\n"
    | 890 ->
        "Expecting an expression\n"
    | 738 ->
        "Expecting an identifier\n"
    | 2074 ->
        "Expecting a structure item\n"
    | 2692 ->
        "Invalid token\n"
    | 907 ->
        "Expecting an expression\n"
    | 991 ->
        "Expecting one of the following:\n  - The continuation of the previous expression\n  - \":\" to start the next expression\n"
    | 992 ->
        "Expecting an expression\n"
    | 994 ->
        "Expecting an expression\n"
    | 1000 ->
        "Expecting an expression\n"
    | 1002 ->
        "Expecting an expression\n"
    | 996 ->
        "Expecting an expression\n"
    | 1004 ->
        "Expecting an expression\n"
    | 1006 ->
        "Expecting an expression\n"
    | 1008 ->
        "Expecting an expression\n"
    | 172 ->
        "Expecting one of the following:\n  - \")\" to form a unit value \"()\"\n  - \"module\" to start a module expression\n  - an expression\n  - an operator to denote the prefix form of an operator\n"
    | 178 ->
        "Expecting an expression\n"
    | 1010 ->
        "Expecting an expression\n"
    | 1016 ->
        "Expecting an expression\n"
    | 2461 ->
        "Expecting \"]\"\n"
    | 406 ->
        "Expecting an attributed id\n"
    | 2464 ->
        "Expecting \"]\"\n"
    | 345 ->
        "Expecting an attribute id\n"
    | 892 ->
        "Expecting an expression\n"
    | 998 ->
        "Expecting an expression\n"
    | 1012 ->
        "Expecting an expression\n"
    | 1014 ->
        "Expecting an expression\n"
    | 1018 ->
        "Expecting an expression\n"
    | 1020 ->
        "Expecting an expression\n"
    | 763 ->
        "<SYNTAX ERROR>\n"
    | 764 ->
        "<SYNTAX ERROR>\n"
    | 2128 ->
        "<SYNTAX ERROR>\n"
    | 765 ->
        "<SYNTAX ERROR>\n"
    | 2123 ->
        "<SYNTAX ERROR>\n"
    | 2124 ->
        "<SYNTAX ERROR>\n"
    | 2126 ->
        "<SYNTAX ERROR>\n"
    | 2140 ->
        "<SYNTAX ERROR>\n"
    | 2142 ->
        "<SYNTAX ERROR>\n"
    | 2148 ->
        "<SYNTAX ERROR>\n"
    | 2158 ->
        "<SYNTAX ERROR>\n"
    | 2163 ->
        "<SYNTAX ERROR>\n"
    | 2183 ->
        "<SYNTAX ERROR>\n"
    | 1030 ->
        "<SYNTAX ERROR>\n"
    | 1024 ->
        "<SYNTAX ERROR>\n"
    | 1026 ->
        "<SYNTAX ERROR>\n"
    | 1028 ->
        "<SYNTAX ERROR>\n"
    | 79 ->
        "<SYNTAX ERROR>\n"
    | 108 ->
        "Variant constructors need to begin with an uppercase character\n"
    | 2090 ->
        "<SYNTAX ERROR>\n"
    | 2655 ->
        "Expecting one of the following:\n  - \"=\" to start the body of the type declaration\n  - \"constraint\" to add constraints to the type declaration\n  - \";\" to finish type declaratoin\n  - \"+=\" to form a string type extension\n  - \"and\" to declare another type\n"
    | 82 ->
        "<SYNTAX ERROR>\n"
    | 2659 ->
        "<SYNTAX ERROR>\n"
    | 2663 ->
        "<SYNTAX ERROR>\n"
    | 2660 ->
        "<SYNTAX ERROR>\n"
    | 2661 ->
        "<SYNTAX ERROR>\n"
    | 86 ->
        "<SYNTAX ERROR>\n"
    | 88 ->
        "<SYNTAX ERROR>\n"
    | 90 ->
        "<SYNTAX ERROR>\n"
    | 92 ->
        "<SYNTAX ERROR>\n"
    | 1877 ->
        "<SYNTAX ERROR>\n"
    | 109 ->
        "<SYNTAX ERROR>\n"
    | 2637 ->
        "<SYNTAX ERROR>\n"
    | 2638 ->
        "<SYNTAX ERROR>\n"
    | 2633 ->
        "<SYNTAX ERROR>\n"
    | 2640 ->
        "<SYNTAX ERROR>\n"
    | 2646 ->
        "<SYNTAX ERROR>\n"
    | 2647 ->
        "<SYNTAX ERROR>\n"
    | 2608 ->
        "Variant constructors need to begin with an uppercase character\n"
    | 111 ->
        "<SYNTAX ERROR>\n"
    | 2593 ->
        "<SYNTAX ERROR>\n"
    | 2594 ->
        "<SYNTAX ERROR>\n"
    | 311 ->
        "Expecting at least one type field definition in the form of:\n  <field name> : <type>\n"
    | 159 ->
        "Expecting a type field definition in the form of:\n  <field name> : <type>\n"
    | 2547 ->
        "Expecting \":\"\n"
    | 2548 ->
        "Expecting a type name describing this field\n"
    | 324 ->
        "Expecting one of the following:\n  - \",\" to finish current type field\n  - \"}\" to finish type definition\n"
    | 1886 ->
        "<SYNTAX ERROR>\n"
    | 2630 ->
        "<SYNTAX ERROR>\n"
    | 1777 ->
        "<SYNTAX ERROR>\n"
    | 1778 ->
        "<SYNTAX ERROR>\n"
    | 1779 ->
        "<SYNTAX ERROR>\n"
    | 1878 ->
        "<SYNTAX ERROR>\n"
    | 1884 ->
        "<SYNTAX ERROR>\n"
    | 16 ->
        "<SYNTAX ERROR>\n"
    | 2681 ->
        "<SYNTAX ERROR>\n"
    | 2680 ->
        "<SYNTAX ERROR>\n"
    | 2676 ->
        "This is a reserved keyword. Consider using a different one. For BuckleScript, add an underscore at the end (http://bloomberg.github.io/bucklescript/Manual.html#_object_label_translation_convention).\n"
    | 2683 ->
        "<SYNTAX ERROR>\n"
    | 2374 ->
        "<SYNTAX ERROR>\n"
    | 2375 ->
        "<SYNTAX ERROR>\n"
    | 2376 ->
        "Expecting a sequence item\n"
    | 1087 ->
        "Expecting one of the following:\n  - \"|\" to open the next pattern\n  - \"=>\" to start the body of the matched pattern\n  - \"when\" to start a contitional guard for the previous pattern\n"
    | 2408 ->
        "Expecting the body of the matched pattern\n"
    | 2684 ->
        "Expecting one of the following:\n  - \"}\" to finish the block\n  - \"|\" to start another pattern matching case\n"
    | 2398 ->
        "<SYNTAX ERROR>\n"
    | 2377 ->
        "<SYNTAX ERROR>\n"
    | 2381 ->
        "<SYNTAX ERROR>\n"
    | 2384 ->
        "<SYNTAX ERROR>\n"
    | 2385 ->
        "<SYNTAX ERROR>\n"
    | 2382 ->
        "<SYNTAX ERROR>\n"
    | 2387 ->
        "<SYNTAX ERROR>\n"
    | 2388 ->
        "<SYNTAX ERROR>\n"
    | 2391 ->
        "<SYNTAX ERROR>\n"
    | 794 ->
        "<SYNTAX ERROR>\n"
    | 1086 ->
        "Expecting a match case\n"
    | 864 ->
        "<SYNTAX ERROR>\n"
    | 487 ->
        "<SYNTAX ERROR>\n"
    | 488 ->
        "<SYNTAX ERROR>\n"
    | 865 ->
        "<SYNTAX ERROR>\n"
    | 1032 ->
        "<SYNTAX ERROR>\n"
    | 1035 ->
        "<SYNTAX ERROR>\n"
    | 1049 ->
        "<SYNTAX ERROR>\n"
    | 1037 ->
        "<SYNTAX ERROR>\n"
    | 1038 ->
        "<SYNTAX ERROR>\n"
    | 1041 ->
        "<SYNTAX ERROR>\n"
    | 1043 ->
        "<SYNTAX ERROR>\n"
    | 1044 ->
        "<SYNTAX ERROR>\n"
    | 1046 ->
        "<SYNTAX ERROR>\n"
    | 164 ->
        "<SYNTAX ERROR>\n"
    | 2541 ->
        "<SYNTAX ERROR>\n"
    | 2542 ->
        "<SYNTAX ERROR>\n"
    | 2543 ->
        "The switch expression isn't closed.\n"
    | 1742 ->
        "Incomplete statement. Did you forget a \";\"?\n"
    | 871 ->
        "<SYNTAX ERROR>\n"
    | 870 ->
        "<SYNTAX ERROR>\n"
    | 398 ->
        "<SYNTAX ERROR>\n"
    | 399 ->
        "<SYNTAX ERROR>\n"
    | 347 ->
        "<SYNTAX ERROR>\n"
    | 402 ->
        "<SYNTAX ERROR>\n"
    | 404 ->
        "<SYNTAX ERROR>\n"
    | 1107 ->
        "<SYNTAX ERROR>\n"
    | 1120 ->
        "<SYNTAX ERROR>\n"
    | 1127 ->
        "<SYNTAX ERROR>\n"
    | 1106 ->
        "<SYNTAX ERROR>\n"
    | 1129 ->
        "<SYNTAX ERROR>\n"
    | 1132 ->
        "<SYNTAX ERROR>\n"
    | 558 ->
        "<SYNTAX ERROR>\n"
    | 176 ->
        "<SYNTAX ERROR>\n"
    | 190 ->
        "<SYNTAX ERROR>\n"
    | 191 ->
        "<SYNTAX ERROR>\n"
    | 170 ->
        "<SYNTAX ERROR>\n"
    | 114 ->
        "<SYNTAX ERROR>\n"
    | 115 ->
        "<SYNTAX ERROR>\n"
    | 2498 ->
        "<SYNTAX ERROR>\n"
    | 2500 ->
        "<SYNTAX ERROR>\n"
    | 801 ->
        "<SYNTAX ERROR>\n"
    | 802 ->
        "<SYNTAX ERROR>\n"
    | 188 ->
        "Expecting one of the following:\n  - \")\" to form a unit value \"()\"\n  - \"module\" to start a module expression\n  - an expression\n  - an operator to denote the prefix form of an operator\n"
    | 2509 ->
        "<SYNTAX ERROR>\n"
    | 194 ->
        "Expecting a module expression\n"
    | 2496 ->
        "<SYNTAX ERROR>\n"
    | 2502 ->
        "<SYNTAX ERROR>\n"
    | 2503 ->
        "<SYNTAX ERROR>\n"
    | 2504 ->
        "<SYNTAX ERROR>\n"
    | 2505 ->
        "<SYNTAX ERROR>\n"
    | 2506 ->
        "<SYNTAX ERROR>\n"
    | 2507 ->
        "<SYNTAX ERROR>\n"
    | 888 ->
        "<SYNTAX ERROR>\n"
    | 2486 ->
        "<SYNTAX ERROR>\n"
    | 198 ->
        "<SYNTAX ERROR>\n"
    | 429 ->
        "<SYNTAX ERROR>\n"
    | 2416 ->
        "<SYNTAX ERROR>\n"
    | 2153 ->
        "<SYNTAX ERROR>\n"
    | 2150 ->
        "<SYNTAX ERROR>\n"
    | 444 ->
        "<SYNTAX ERROR>\n"
    | 453 ->
        "<SYNTAX ERROR>\n"
    | 726 ->
        "<SYNTAX ERROR>\n"
    | 735 ->
        "<SYNTAX ERROR>\n"
    | 736 ->
        "<SYNTAX ERROR>\n"
    | 741 ->
        "<SYNTAX ERROR>\n"
    | 742 ->
        "<SYNTAX ERROR>\n"
    | 2205 ->
        "<SYNTAX ERROR>\n"
    | 2191 ->
        "<SYNTAX ERROR>\n"
    | 747 ->
        "<SYNTAX ERROR>\n"
    | 748 ->
        "<SYNTAX ERROR>\n"
    | 750 ->
        "<SYNTAX ERROR>\n"
    | 751 ->
        "<SYNTAX ERROR>\n"
    | 2186 ->
        "<SYNTAX ERROR>\n"
    | 743 ->
        "<SYNTAX ERROR>\n"
    | 744 ->
        "<SYNTAX ERROR>\n"
    | 746 ->
        "<SYNTAX ERROR>\n"
    | 2196 ->
        "<SYNTAX ERROR>\n"
    | 2197 ->
        "<SYNTAX ERROR>\n"
    | 2198 ->
        "<SYNTAX ERROR>\n"
    | 2199 ->
        "<SYNTAX ERROR>\n"
    | 2200 ->
        "<SYNTAX ERROR>\n"
    | 2201 ->
        "<SYNTAX ERROR>\n"
    | 752 ->
        "<SYNTAX ERROR>\n"
    | 754 ->
        "<SYNTAX ERROR>\n"
    | 755 ->
        "<SYNTAX ERROR>\n"
    | 758 ->
        "<SYNTAX ERROR>\n"
    | 1105 ->
        "<SYNTAX ERROR>\n"
    | 515 ->
        "<SYNTAX ERROR>\n"
    | 849 ->
        "<SYNTAX ERROR>\n"
    | 721 ->
        "Incomplete let binding\n"
    | 1907 ->
        "<SYNTAX ERROR>\n"
    | 1913 ->
        "<SYNTAX ERROR>\n"
    | 1908 ->
        "<SYNTAX ERROR>\n"
    | 1909 ->
        "<SYNTAX ERROR>\n"
    | 1910 ->
        "<SYNTAX ERROR>\n"
    | 1912 ->
        "<SYNTAX ERROR>\n"
    | 459 ->
        "<SYNTAX ERROR>\n"
    | 1747 ->
        "<SYNTAX ERROR>\n"
    | 1748 ->
        "<SYNTAX ERROR>\n"
    | 2058 ->
        "<SYNTAX ERROR>\n"
    | 2059 ->
        "<SYNTAX ERROR>\n"
    | 2060 ->
        "<SYNTAX ERROR>\n"
    | 2061 ->
        "<SYNTAX ERROR>\n"
    | 2065 ->
        "<SYNTAX ERROR>\n"
    | 2062 ->
        "<SYNTAX ERROR>\n"
    | 2063 ->
        "<SYNTAX ERROR>\n"
    | 2064 ->
        "<SYNTAX ERROR>\n"
    | 1749 ->
        "<SYNTAX ERROR>\n"
    | 1750 ->
        "<SYNTAX ERROR>\n"
    | 1759 ->
        "<SYNTAX ERROR>\n"
    | 2051 ->
        "<SYNTAX ERROR>\n"
    | 2052 ->
        "<SYNTAX ERROR>\n"
    | 2053 ->
        "<SYNTAX ERROR>\n"
    | 2069 ->
        "<SYNTAX ERROR>\n"
    | 1890 ->
        "<SYNTAX ERROR>\n"
    | 1891 ->
        "<SYNTAX ERROR>\n"
    | 696 ->
        "<SYNTAX ERROR>\n"
    | 2278 ->
        "Defining a function?\nExpecting one of the following:\n  - \"=>\" to start the function body\n  - an identifier to add a function parameter\n  - \":\" to specify the return type\n"
    | 1615 ->
        "<SYNTAX ERROR>\n"
    | 703 ->
        "<SYNTAX ERROR>\n"
    | 704 ->
        "<SYNTAX ERROR>\n"
    | 706 ->
        "<SYNTAX ERROR>\n"
    | 1595 ->
        "Expecting an expression as function body\n"
    | 1616 ->
        "<SYNTAX ERROR>\n"
    | 1617 ->
        "Defining a function?\nExpecting \"=>\" to start the function body\n"
    | 1618 ->
        "<SYNTAX ERROR>\n"
    | 1619 ->
        "<SYNTAX ERROR>\n"
    | 697 ->
        "<SYNTAX ERROR>\n"
    | 698 ->
        "<SYNTAX ERROR>\n"
    | 702 ->
        "<SYNTAX ERROR>\n"
    | 2274 ->
        "<SYNTAX ERROR>\n"
    | 2275 ->
        "<SYNTAX ERROR>\n"
    | 2261 ->
        "<SYNTAX ERROR>\n"
    | 2262 ->
        "<SYNTAX ERROR>\n"
    | 699 ->
        "<SYNTAX ERROR>\n"
    | 2264 ->
        "<SYNTAX ERROR>\n"
    | 2265 ->
        "<SYNTAX ERROR>\n"
    | 2266 ->
        "<SYNTAX ERROR>\n"
    | 2269 ->
        "<SYNTAX ERROR>\n"
    | 2270 ->
        "<SYNTAX ERROR>\n"
    | 2271 ->
        "<SYNTAX ERROR>\n"
    | 2272 ->
        "<SYNTAX ERROR>\n"
    | 2268 ->
        "<SYNTAX ERROR>\n"
    | 1906 ->
        "<SYNTAX ERROR>\n"
    | 73 ->
        "<SYNTAX ERROR>\n"
    | 2220 ->
        "<SYNTAX ERROR>\n"
    | 200 ->
        "<SYNTAX ERROR>\n"
    | 2484 ->
        "<SYNTAX ERROR>\n"
    | 2485 ->
        "<SYNTAX ERROR>\n"
    | 2483 ->
        "<SYNTAX ERROR>\n"
    | 74 ->
        "<SYNTAX ERROR>\n"
    | 1369 ->
        "<SYNTAX ERROR>\n"
    | 722 ->
        "<SYNTAX ERROR>\n"
    | 2143 ->
        "<SYNTAX ERROR>\n"
    | 1110 ->
        "<SYNTAX ERROR>\n"
    | 1112 ->
        "<SYNTAX ERROR>\n"
    | 1115 ->
        "Expecting a type name\n"
    | 169 ->
        "Expecting an expression\n"
    | 902 ->
        "Expecting an expression\n"
    | 862 ->
        "Expecting an expression\n"
    | 727 ->
        "<SYNTAX ERROR>\n"
    | 2218 ->
        "Expecting \"]\" to finish current floating attribute\n"
    | 724 ->
        "<SYNTAX ERROR>\n"
    | 430 ->
        "Expecting one of the following:\n  - an list item\n  - \"]\" to finish this list\n"
    | 2152 ->
        "Expecting one of the following:\n  - \",\" to separate two items in a list\n  - \"]\" to finish this list\n"
    | 2149 ->
        "<SYNTAX ERROR>\n"
    | 182 ->
        "<SYNTAX ERROR>\n"
    | 447 ->
        "<SYNTAX ERROR>\n"
    | 183 ->
        "<SYNTAX ERROR>\n"
    | 2518 ->
        "<SYNTAX ERROR>\n"
    | 1565 ->
        "<SYNTAX ERROR>\n"
    | 1566 ->
        "<SYNTAX ERROR>\n"
    | 1567 ->
        "<SYNTAX ERROR>\n"
    | 1568 ->
        "<SYNTAX ERROR>\n"
    | 1569 ->
        "<SYNTAX ERROR>\n"
    | 1570 ->
        "<SYNTAX ERROR>\n"
    | 1575 ->
        "<SYNTAX ERROR>\n"
    | 1576 ->
        "<SYNTAX ERROR>\n"
    | 1577 ->
        "<SYNTAX ERROR>\n"
    | 1578 ->
        "<SYNTAX ERROR>\n"
    | 1579 ->
        "<SYNTAX ERROR>\n"
    | 1582 ->
        "<SYNTAX ERROR>\n"
    | 1583 ->
        "<SYNTAX ERROR>\n"
    | 1584 ->
        "<SYNTAX ERROR>\n"
    | 1585 ->
        "<SYNTAX ERROR>\n"
    | 1586 ->
        "<SYNTAX ERROR>\n"
    | 1587 ->
        "<SYNTAX ERROR>\n"
    | 1574 ->
        "<SYNTAX ERROR>\n"
    | 2357 ->
        "<SYNTAX ERROR>\n"
    | 2336 ->
        "<SYNTAX ERROR>\n"
    | 1588 ->
        "<SYNTAX ERROR>\n"
    | 1410 ->
        "<SYNTAX ERROR>\n"
    | 1412 ->
        "<SYNTAX ERROR>\n"
    | 1415 ->
        "<SYNTAX ERROR>\n"
    | 1625 ->
        "<SYNTAX ERROR>\n"
    | 1590 ->
        "<SYNTAX ERROR>\n"
    | 1591 ->
        "<SYNTAX ERROR>\n"
    | 1592 ->
        "<SYNTAX ERROR>\n"
    | 1647 ->
        "<SYNTAX ERROR>\n"
    | 1597 ->
        "<SYNTAX ERROR>\n"
    | 1598 ->
        "<SYNTAX ERROR>\n"
    | 1599 ->
        "<SYNTAX ERROR>\n"
    | 1609 ->
        "<SYNTAX ERROR>\n"
    | 1610 ->
        "<SYNTAX ERROR>\n"
    | 1611 ->
        "<SYNTAX ERROR>\n"
    | 1600 ->
        "<SYNTAX ERROR>\n"
    | 1602 ->
        "<SYNTAX ERROR>\n"
    | 1603 ->
        "<SYNTAX ERROR>\n"
    | 1604 ->
        "<SYNTAX ERROR>\n"
    | 1605 ->
        "<SYNTAX ERROR>\n"
    | 1589 ->
        "<SYNTAX ERROR>\n"
    | 2181 ->
        "<SYNTAX ERROR>\n"
    | 2172 ->
        "<SYNTAX ERROR>\n"
    | 2170 ->
        "<SYNTAX ERROR>\n"
    | 2173 ->
        "<SYNTAX ERROR>\n"
    | 2174 ->
        "<SYNTAX ERROR>\n"
    | 2184 ->
        "<SYNTAX ERROR>\n"
    | 2185 ->
        "<SYNTAX ERROR>\n"
    | 456 ->
        "<SYNTAX ERROR>\n"
    | 2311 ->
        "<SYNTAX ERROR>\n"
    | 2314 ->
        "<SYNTAX ERROR>\n"
    | 2315 ->
        "<SYNTAX ERROR>\n"
    | 2312 ->
        "<SYNTAX ERROR>\n"
    | 2317 ->
        "<SYNTAX ERROR>\n"
    | 2318 ->
        "<SYNTAX ERROR>\n"
    | 2329 ->
        "<SYNTAX ERROR>\n"
    | 2301 ->
        "Expecting \"}\" to finish the block\n"
    | 1628 ->
        "<SYNTAX ERROR>\n"
    | 1629 ->
        "<SYNTAX ERROR>\n"
    | 1632 ->
        "<SYNTAX ERROR>\n"
    | 1473 ->
        "<SYNTAX ERROR>\n"
    | 1662 ->
        "<SYNTAX ERROR>\n"
    | 1665 ->
        "<SYNTAX ERROR>\n"
    | 1673 ->
        "<SYNTAX ERROR>\n"
    | 1666 ->
        "<SYNTAX ERROR>\n"
    | 1429 ->
        "<SYNTAX ERROR>\n"
    | 1430 ->
        "<SYNTAX ERROR>\n"
    | 1434 ->
        "<SYNTAX ERROR>\n"
    | 1435 ->
        "<SYNTAX ERROR>\n"
    | 1667 ->
        "<SYNTAX ERROR>\n"
    | 1381 ->
        "<SYNTAX ERROR>\n"
    | 1672 ->
        "<SYNTAX ERROR>\n"
    | 1669 ->
        "<SYNTAX ERROR>\n"
    | 1670 ->
        "<SYNTAX ERROR>\n"
    | 1671 ->
        "<SYNTAX ERROR>\n"
    | 1668 ->
        "<SYNTAX ERROR>\n"
    | 1678 ->
        "<SYNTAX ERROR>\n"
    | 1679 ->
        "<SYNTAX ERROR>\n"
    | 1542 ->
        "<SYNTAX ERROR>\n"
    | 1543 ->
        "<SYNTAX ERROR>\n"
    | 1648 ->
        "<SYNTAX ERROR>\n"
    | 1634 ->
        "<SYNTAX ERROR>\n"
    | 1635 ->
        "<SYNTAX ERROR>\n"
    | 1474 ->
        "<SYNTAX ERROR>\n"
    | 1561 ->
        "<SYNTAX ERROR>\n"
    | 1652 ->
        "<SYNTAX ERROR>\n"
    | 1475 ->
        "<SYNTAX ERROR>\n"
    | 1557 ->
        "<SYNTAX ERROR>\n"
    | 1558 ->
        "<SYNTAX ERROR>\n"
    | 1532 ->
        "<SYNTAX ERROR>\n"
    | 1534 ->
        "<SYNTAX ERROR>\n"
    | 1535 ->
        "<SYNTAX ERROR>\n"
    | 1545 ->
        "<SYNTAX ERROR>\n"
    | 1536 ->
        "<SYNTAX ERROR>\n"
    | 1537 ->
        "<SYNTAX ERROR>\n"
    | 1538 ->
        "<SYNTAX ERROR>\n"
    | 1633 ->
        "<SYNTAX ERROR>\n"
    | 2284 ->
        "<SYNTAX ERROR>\n"
    | 2285 ->
        "<SYNTAX ERROR>\n"
    | 2286 ->
        "<SYNTAX ERROR>\n"
    | 1639 ->
        "<SYNTAX ERROR>\n"
    | 1442 ->
        "<SYNTAX ERROR>\n"
    | 1443 ->
        "<SYNTAX ERROR>\n"
    | 1552 ->
        "<SYNTAX ERROR>\n"
    | 1553 ->
        "<SYNTAX ERROR>\n"
    | 1564 ->
        "<SYNTAX ERROR>\n"
    | 1149 ->
        "<SYNTAX ERROR>\n"
    | 1752 ->
        "<SYNTAX ERROR>\n"
    | 1155 ->
        "<SYNTAX ERROR>\n"
    | 1156 ->
        "<SYNTAX ERROR>\n"
    | 1157 ->
        "<SYNTAX ERROR>\n"
    | 1159 ->
        "<SYNTAX ERROR>\n"
    | 1162 ->
        "<SYNTAX ERROR>\n"
    | 1169 ->
        "<SYNTAX ERROR>\n"
    | 1148 ->
        "<SYNTAX ERROR>\n"
    | 2109 ->
        "<SYNTAX ERROR>\n"
    | 416 ->
        "<SYNTAX ERROR>\n"
    | 417 ->
        "<SYNTAX ERROR>\n"
    | 2449 ->
        "<SYNTAX ERROR>\n"
    | 2451 ->
        "<SYNTAX ERROR>\n"
    | 1160 ->
        "<SYNTAX ERROR>\n"
    | 2453 ->
        "<SYNTAX ERROR>\n"
    | 1165 ->
        "<SYNTAX ERROR>\n"
    | 1166 ->
        "<SYNTAX ERROR>\n"
    | 2455 ->
        "<SYNTAX ERROR>\n"
    | 1172 ->
        "<SYNTAX ERROR>\n"
    | 2104 ->
        "<SYNTAX ERROR>\n"
    | 1150 ->
        "<SYNTAX ERROR>\n"
    | 1151 ->
        "<SYNTAX ERROR>\n"
    | 1152 ->
        "<SYNTAX ERROR>\n"
    | 1154 ->
        "<SYNTAX ERROR>\n"
    | 408 ->
        "A module's name needs to begin with a upper-case letter\n"
    | 409 ->
        "<SYNTAX ERROR>\n"
    | 412 ->
        "<SYNTAX ERROR>\n"
    | 1304 ->
        "<SYNTAX ERROR>\n"
    | 1305 ->
        "<SYNTAX ERROR>\n"
    | 1306 ->
        "<SYNTAX ERROR>\n"
    | 1307 ->
        "<SYNTAX ERROR>\n"
    | 1308 ->
        "<SYNTAX ERROR>\n"
    | 1309 ->
        "<SYNTAX ERROR>\n"
    | 1315 ->
        "<SYNTAX ERROR>\n"
    | 1316 ->
        "<SYNTAX ERROR>\n"
    | 1326 ->
        "<SYNTAX ERROR>\n"
    | 1331 ->
        "<SYNTAX ERROR>\n"
    | 1317 ->
        "<SYNTAX ERROR>\n"
    | 1318 ->
        "<SYNTAX ERROR>\n"
    | 1319 ->
        "<SYNTAX ERROR>\n"
    | 1324 ->
        "<SYNTAX ERROR>\n"
    | 1339 ->
        "<SYNTAX ERROR>\n"
    | 1371 ->
        "<SYNTAX ERROR>\n"
    | 1467 ->
        "<SYNTAX ERROR>\n"
    | 1373 ->
        "<SYNTAX ERROR>\n"
    | 1374 ->
        "<SYNTAX ERROR>\n"
    | 1376 ->
        "<SYNTAX ERROR>\n"
    | 1379 ->
        "<SYNTAX ERROR>\n"
    | 1457 ->
        "<SYNTAX ERROR>\n"
    | 1895 ->
        "<SYNTAX ERROR>\n"
    | 1896 ->
        "<SYNTAX ERROR>\n"
    | 1469 ->
        "<SYNTAX ERROR>\n"
    | 1720 ->
        "<SYNTAX ERROR>\n"
    | 1721 ->
        "<SYNTAX ERROR>\n"
    | 1722 ->
        "<SYNTAX ERROR>\n"
    | 1723 ->
        "<SYNTAX ERROR>\n"
    | 1727 ->
        "<SYNTAX ERROR>\n"
    | 1728 ->
        "<SYNTAX ERROR>\n"
    | 1729 ->
        "<SYNTAX ERROR>\n"
    | 1472 ->
        "<SYNTAX ERROR>\n"
    | 1684 ->
        "<SYNTAX ERROR>\n"
    | 1900 ->
        "<SYNTAX ERROR>\n"
    | 1901 ->
        "<SYNTAX ERROR>\n"
    | 1685 ->
        "<SYNTAX ERROR>\n"
    | 1715 ->
        "<SYNTAX ERROR>\n"
    | 1716 ->
        "<SYNTAX ERROR>\n"
    | 1717 ->
        "<SYNTAX ERROR>\n"
    | 767 ->
        "<SYNTAX ERROR>\n"
    | 771 ->
        "<SYNTAX ERROR>\n"
    | 772 ->
        "<SYNTAX ERROR>\n"
    | 2118 ->
        "<SYNTAX ERROR>\n"
    | 707 ->
        "<SYNTAX ERROR>\n"
    | 708 ->
        "<SYNTAX ERROR>\n"
    | 710 ->
        "<SYNTAX ERROR>\n"
    | 711 ->
        "<SYNTAX ERROR>\n"
    | 753 ->
        "<SYNTAX ERROR>\n"
    | 759 ->
        "<SYNTAX ERROR>\n"
    | 1692 ->
        "<SYNTAX ERROR>\n"
    | 1693 ->
        "<SYNTAX ERROR>\n"
    | 1724 ->
        "<SYNTAX ERROR>\n"
    | 1725 ->
        "<SYNTAX ERROR>\n"
    | 1697 ->
        "<SYNTAX ERROR>\n"
    | 1698 ->
        "<SYNTAX ERROR>\n"
    | 1731 ->
        "<SYNTAX ERROR>\n"
    | 1688 ->
        "<SYNTAX ERROR>\n"
    | 1389 ->
        "<SYNTAX ERROR>\n"
    | 1390 ->
        "<SYNTAX ERROR>\n"
    | 1391 ->
        "<SYNTAX ERROR>\n"
    | 1392 ->
        "<SYNTAX ERROR>\n"
    | 1393 ->
        "<SYNTAX ERROR>\n"
    | 1395 ->
        "<SYNTAX ERROR>\n"
    | 1396 ->
        "<SYNTAX ERROR>\n"
    | 1397 ->
        "<SYNTAX ERROR>\n"
    | 1398 ->
        "<SYNTAX ERROR>\n"
    | 1402 ->
        "<SYNTAX ERROR>\n"
    | 1403 ->
        "<SYNTAX ERROR>\n"
    | 1405 ->
        "<SYNTAX ERROR>\n"
    | 1407 ->
        "<SYNTAX ERROR>\n"
    | 1423 ->
        "<SYNTAX ERROR>\n"
    | 1422 ->
        "<SYNTAX ERROR>\n"
    | 1408 ->
        "<SYNTAX ERROR>\n"
    | 1409 ->
        "<SYNTAX ERROR>\n"
    | 1689 ->
        "<SYNTAX ERROR>\n"
    | 1428 ->
        "<SYNTAX ERROR>\n"
    | 1436 ->
        "<SYNTAX ERROR>\n"
    | 1441 ->
        "<SYNTAX ERROR>\n"
    | 1382 ->
        "<SYNTAX ERROR>\n"
    | 1383 ->
        "<SYNTAX ERROR>\n"
    | 1388 ->
        "<SYNTAX ERROR>\n"
    | 1459 ->
        "<SYNTAX ERROR>\n"
    | 1470 ->
        "<SYNTAX ERROR>\n"
    | 1471 ->
        "<SYNTAX ERROR>\n"
    | 1707 ->
        "<SYNTAX ERROR>\n"
    | 1686 ->
        "<SYNTAX ERROR>\n"
    | 1706 ->
        "<SYNTAX ERROR>\n"
    | 1703 ->
        "<SYNTAX ERROR>\n"
    | 1704 ->
        "<SYNTAX ERROR>\n"
    | 1705 ->
        "<SYNTAX ERROR>\n"
    | 1702 ->
        "<SYNTAX ERROR>\n"
    | 1713 ->
        "<SYNTAX ERROR>\n"
    | 2695 ->
        "<SYNTAX ERROR>\n"
    | 2079 ->
        "<SYNTAX ERROR>\n"
    | 1177 ->
        "<SYNTAX ERROR>\n"
    | 2697 ->
        "<SYNTAX ERROR>\n"
    | 1296 ->
        "<SYNTAX ERROR>\n"
    | 2080 ->
        "<SYNTAX ERROR>\n"
    | 2085 ->
        "<SYNTAX ERROR>\n"
    | 2082 ->
        "<SYNTAX ERROR>\n"
    | 2083 ->
        "<SYNTAX ERROR>\n"
    | 2696 ->
        "<SYNTAX ERROR>\n"
    | 1824 ->
        "<SYNTAX ERROR>\n"
    | 1825 ->
        "<SYNTAX ERROR>\n"
    | 1827 ->
        "<SYNTAX ERROR>\n"
    | 1864 ->
        "<SYNTAX ERROR>\n"
    | 1990 ->
        "<SYNTAX ERROR>\n"
    | 1991 ->
        "<SYNTAX ERROR>\n"
    | 1992 ->
        "<SYNTAX ERROR>\n"
    | 1993 ->
        "<SYNTAX ERROR>\n"
    | 1994 ->
        "<SYNTAX ERROR>\n"
    | 1995 ->
        "<SYNTAX ERROR>\n"
    | 1996 ->
        "<SYNTAX ERROR>\n"
    | 2002 ->
        "<SYNTAX ERROR>\n"
    | 2004 ->
        "<SYNTAX ERROR>\n"
    | 1997 ->
        "<SYNTAX ERROR>\n"
    | 1998 ->
        "<SYNTAX ERROR>\n"
    | 2009 ->
        "<SYNTAX ERROR>\n"
    | 2010 ->
        "<SYNTAX ERROR>\n"
    | 2011 ->
        "<SYNTAX ERROR>\n"
    | 2012 ->
        "<SYNTAX ERROR>\n"
    | 2025 ->
        "<SYNTAX ERROR>\n"
    | 2026 ->
        "<SYNTAX ERROR>\n"
    | 624 ->
        "<SYNTAX ERROR>\n"
    | 1865 ->
        "<SYNTAX ERROR>\n"
    | 1866 ->
        "<SYNTAX ERROR>\n"
    | 1934 ->
        "<SYNTAX ERROR>\n"
    | 1832 ->
        "<SYNTAX ERROR>\n"
    | 1833 ->
        "<SYNTAX ERROR>\n"
    | 1835 ->
        "<SYNTAX ERROR>\n"
    | 1770 ->
        "<SYNTAX ERROR>\n"
    | 1840 ->
        "<SYNTAX ERROR>\n"
    | 1838 ->
        "<SYNTAX ERROR>\n"
    | 1836 ->
        "<SYNTAX ERROR>\n"
    | 1848 ->
        "<SYNTAX ERROR>\n"
    | 1849 ->
        "<SYNTAX ERROR>\n"
    | 1841 ->
        "<SYNTAX ERROR>\n"
    | 1842 ->
        "<SYNTAX ERROR>\n"
    | 1846 ->
        "<SYNTAX ERROR>\n"
    | 95 ->
        "<SYNTAX ERROR>\n"
    | 1845 ->
        "<SYNTAX ERROR>\n"
    | 1843 ->
        "<SYNTAX ERROR>\n"
    | 133 ->
        "<SYNTAX ERROR>\n"
    | 788 ->
        "<SYNTAX ERROR>\n"
    | 1852 ->
        "<SYNTAX ERROR>\n"
    | 1853 ->
        "<SYNTAX ERROR>\n"
    | 789 ->
        "<SYNTAX ERROR>\n"
    | 790 ->
        "<SYNTAX ERROR>\n"
    | 773 ->
        "<SYNTAX ERROR>\n"
    | 774 ->
        "<SYNTAX ERROR>\n"
    | 1760 ->
        "<SYNTAX ERROR>\n"
    | 1817 ->
        "<SYNTAX ERROR>\n"
    | 1818 ->
        "<SYNTAX ERROR>\n"
    | 2047 ->
        "<SYNTAX ERROR>\n"
    | 2048 ->
        "<SYNTAX ERROR>\n"
    | 2049 ->
        "<SYNTAX ERROR>\n"
    | 2050 ->
        "<SYNTAX ERROR>\n"
    | 1766 ->
        "<SYNTAX ERROR>\n"
    | 1767 ->
        "<SYNTAX ERROR>\n"
    | 1769 ->
        "<SYNTAX ERROR>\n"
    | 1776 ->
        "<SYNTAX ERROR>\n"
    | 1774 ->
        "<SYNTAX ERROR>\n"
    | 1772 ->
        "<SYNTAX ERROR>\n"
    | 1802 ->
        "<SYNTAX ERROR>\n"
    | 1803 ->
        "<SYNTAX ERROR>\n"
    | 1782 ->
        "<SYNTAX ERROR>\n"
    | 1783 ->
        "<SYNTAX ERROR>\n"
    | 1800 ->
        "<SYNTAX ERROR>\n"
    | 1785 ->
        "<SYNTAX ERROR>\n"
    | 1799 ->
        "<SYNTAX ERROR>\n"
    | 1784 ->
        "<SYNTAX ERROR>\n"
    | 1786 ->
        "<SYNTAX ERROR>\n"
    | 1787 ->
        "<SYNTAX ERROR>\n"
    | 1790 ->
        "<SYNTAX ERROR>\n"
    | 1174 ->
        "<SYNTAX ERROR>\n"
    | 473 ->
        "<SYNTAX ERROR>\n"
    | 1807 ->
        "<SYNTAX ERROR>\n"
    | 1808 ->
        "<SYNTAX ERROR>\n"
    | 474 ->
        "<SYNTAX ERROR>\n"
    | 475 ->
        "<SYNTAX ERROR>\n"
    | 413 ->
        "<SYNTAX ERROR>\n"
    | 414 ->
        "<SYNTAX ERROR>\n"
    | 1173 ->
        "<SYNTAX ERROR>\n"
    | 2098 ->
        "<SYNTAX ERROR>\n"
    | 2099 ->
        "<SYNTAX ERROR>\n"
    | 2100 ->
        "<SYNTAX ERROR>\n"
    | 2101 ->
        "<SYNTAX ERROR>\n"
    | 2102 ->
        "<SYNTAX ERROR>\n"
    | 2103 ->
        "<SYNTAX ERROR>\n"
    | 2095 ->
        "<SYNTAX ERROR>\n"
    | 1764 ->
        "<SYNTAX ERROR>\n"
    | 1175 ->
        "<SYNTAX ERROR>\n"
    | 1819 ->
        "<SYNTAX ERROR>\n"
    | 1986 ->
        "<SYNTAX ERROR>\n"
    | 1985 ->
        "<SYNTAX ERROR>\n"
    | 1937 ->
        "<SYNTAX ERROR>\n"
    | 1938 ->
        "<SYNTAX ERROR>\n"
    | 1939 ->
        "<SYNTAX ERROR>\n"
    | 1940 ->
        "<SYNTAX ERROR>\n"
    | 1941 ->
        "<SYNTAX ERROR>\n"
    | 1946 ->
        "<SYNTAX ERROR>\n"
    | 1947 ->
        "<SYNTAX ERROR>\n"
    | 1969 ->
        "<SYNTAX ERROR>\n"
    | 1978 ->
        "<SYNTAX ERROR>\n"
    | 1980 ->
        "<SYNTAX ERROR>\n"
    | 1458 ->
        "<SYNTAX ERROR>\n"
    | 1981 ->
        "<SYNTAX ERROR>\n"
    | 2035 ->
        "<SYNTAX ERROR>\n"
    | 2036 ->
        "<SYNTAX ERROR>\n"
    | 2702 ->
        "<SYNTAX ERROR>\n"
    | 2704 ->
        "<SYNTAX ERROR>\n"
    | 279 ->
        "<SYNTAX ERROR>\n"
    | 283 ->
        "<SYNTAX ERROR>\n"
    | 284 ->
        "<SYNTAX ERROR>\n"
    | 204 ->
        "<SYNTAX ERROR>\n"
    | 94 ->
        "<SYNTAX ERROR>\n"
    | 96 ->
        "<SYNTAX ERROR>\n"
    | 98 ->
        "<SYNTAX ERROR>\n"
    | 205 ->
        "<SYNTAX ERROR>\n"
    | 123 ->
        "<SYNTAX ERROR>\n"
    | 9 ->
        "<SYNTAX ERROR>\n"
    | 10 ->
        "<SYNTAX ERROR>\n"
    | 2578 ->
        "<SYNTAX ERROR>\n"
    | 127 ->
        "<SYNTAX ERROR>\n"
    | 1202 ->
        "Expecting one of the following:\n  - \",\" to start the type in the tuple\n  - \")\" to finish the tuple type definition\n"
    | 1204 ->
        "<SYNTAX ERROR>\n"
    | 132 ->
        "<SYNTAX ERROR>\n"
    | 137 ->
        "<SYNTAX ERROR>\n"
    | 138 ->
        "<SYNTAX ERROR>\n"
    | 143 ->
        "<SYNTAX ERROR>\n"
    | 144 ->
        "<SYNTAX ERROR>\n"
    | 2575 ->
        "<SYNTAX ERROR>\n"
    | 2576 ->
        "<SYNTAX ERROR>\n"
    | 136 ->
        "<SYNTAX ERROR>\n"
    | 134 ->
        "<SYNTAX ERROR>\n"
    | 265 ->
        "<SYNTAX ERROR>\n"
    | 125 ->
        "<SYNTAX ERROR>\n"
    | 154 ->
        "<SYNTAX ERROR>\n"
    | 2557 ->
        "<SYNTAX ERROR>\n"
    | 1505 ->
        "<SYNTAX ERROR>\n"
    | 1413 ->
        "<SYNTAX ERROR>\n"
    | 1606 ->
        "<SYNTAX ERROR>\n"
    | 1607 ->
        "<SYNTAX ERROR>\n"
    | 325 ->
        "<SYNTAX ERROR>\n"
    | 157 ->
        "<SYNTAX ERROR>\n"
    | 147 ->
        "<SYNTAX ERROR>\n"
    | 2562 ->
        "<SYNTAX ERROR>\n"
    | 2564 ->
        "<SYNTAX ERROR>\n"
    | 2565 ->
        "<SYNTAX ERROR>\n"
    | 255 ->
        "<SYNTAX ERROR>\n"
    | 149 ->
        "<SYNTAX ERROR>\n"
    | 150 ->
        "<SYNTAX ERROR>\n"
    | 2560 ->
        "<SYNTAX ERROR>\n"
    | 152 ->
        "<SYNTAX ERROR>\n"
    | 153 ->
        "<SYNTAX ERROR>\n"
    | 1231 ->
        "<SYNTAX ERROR>\n"
    | 1232 ->
        "<SYNTAX ERROR>\n"
    | 1233 ->
        "<SYNTAX ERROR>\n"
    | 1226 ->
        "<SYNTAX ERROR>\n"
    | 1227 ->
        "<SYNTAX ERROR>\n"
    | 1229 ->
        "<SYNTAX ERROR>\n"
    | 288 ->
        "<SYNTAX ERROR>\n"
    | 263 ->
        "<SYNTAX ERROR>\n"
    | 2706 ->
        "<SYNTAX ERROR>\n"
    | 2708 ->
        "<SYNTAX ERROR>\n"
    | 2710 ->
        "<SYNTAX ERROR>\n"
    | 2711 ->
        "<SYNTAX ERROR>\n"
    | 632 ->
        "<SYNTAX ERROR>\n"
    | 614 ->
        "<SYNTAX ERROR>\n"
    | 587 ->
        "<SYNTAX ERROR>\n"
    | 420 ->
        "<SYNTAX ERROR>\n"
    | 597 ->
        "<SYNTAX ERROR>\n"
    | 202 ->
        "<SYNTAX ERROR>\n"
    | 207 ->
        "<SYNTAX ERROR>\n"
    | 210 ->
        "<SYNTAX ERROR>\n"
    | 213 ->
        "<SYNTAX ERROR>\n"
    | 590 ->
        "<SYNTAX ERROR>\n"
    | 591 ->
        "<SYNTAX ERROR>\n"
    | 588 ->
        "<SYNTAX ERROR>\n"
    | 630 ->
        "<SYNTAX ERROR>\n"
    | 563 ->
        "<SYNTAX ERROR>\n"
    | 642 ->
        "<SYNTAX ERROR>\n"
    | 648 ->
        "<SYNTAX ERROR>\n"
    | 652 ->
        "<SYNTAX ERROR>\n"
    | 653 ->
        "<SYNTAX ERROR>\n"
    | 240 ->
        "<SYNTAX ERROR>\n"
    | 330 ->
        "<SYNTAX ERROR>\n"
    | 118 ->
        "<SYNTAX ERROR>\n"
    | 2582 ->
        "<SYNTAX ERROR>\n"
    | 2579 ->
        "<SYNTAX ERROR>\n"
    | 2580 ->
        "<SYNTAX ERROR>\n"
    | 341 ->
        "<SYNTAX ERROR>\n"
    | 112 ->
        "<SYNTAX ERROR>\n"
    | 313 ->
        "<SYNTAX ERROR>\n"
    | 242 ->
        "<SYNTAX ERROR>\n"
    | 248 ->
        "<SYNTAX ERROR>\n"
    | 250 ->
        "<SYNTAX ERROR>\n"
    | 251 ->
        "<SYNTAX ERROR>\n"
    | 243 ->
        "<SYNTAX ERROR>\n"
    | 296 ->
        "<SYNTAX ERROR>\n"
    | 299 ->
        "<SYNTAX ERROR>\n"
    | 298 ->
        "<SYNTAX ERROR>\n"
    | 301 ->
        "<SYNTAX ERROR>\n"
    | 307 ->
        "<SYNTAX ERROR>\n"
    | 308 ->
        "<SYNTAX ERROR>\n"
    | 309 ->
        "<SYNTAX ERROR>\n"
    | 302 ->
        "<SYNTAX ERROR>\n"
    | 303 ->
        "<SYNTAX ERROR>\n"
    | 305 ->
        "<SYNTAX ERROR>\n"
    | 575 ->
        "<SYNTAX ERROR>\n"
    | 540 ->
        "<SYNTAX ERROR>\n"
    | 502 ->
        "<SYNTAX ERROR>\n"
    | 530 ->
        "<SYNTAX ERROR>\n"
    | 521 ->
        "<SYNTAX ERROR>\n"
    | 215 ->
        "<SYNTAX ERROR>\n"
    | 218 ->
        "<SYNTAX ERROR>\n"
    | 219 ->
        "<SYNTAX ERROR>\n"
    | 224 ->
        "<SYNTAX ERROR>\n"
    | 227 ->
        "<SYNTAX ERROR>\n"
    | 228 ->
        "<SYNTAX ERROR>\n"
    | 230 ->
        "<SYNTAX ERROR>\n"
    | 236 ->
        "<SYNTAX ERROR>\n"
    | 237 ->
        "<SYNTAX ERROR>\n"
    | 238 ->
        "<SYNTAX ERROR>\n"
    | 239 ->
        "<SYNTAX ERROR>\n"
    | 2472 ->
        "<SYNTAX ERROR>\n"
    | 2473 ->
        "<SYNTAX ERROR>\n"
    | 231 ->
        "<SYNTAX ERROR>\n"
    | 235 ->
        "<SYNTAX ERROR>\n"
    | 479 ->
        "<SYNTAX ERROR>\n"
    | 465 ->
        "<SYNTAX ERROR>\n"
    | 561 ->
        "<SYNTAX ERROR>\n"
    | 664 ->
        "<SYNTAX ERROR>\n"
    | 466 ->
        "<SYNTAX ERROR>\n"
    | 467 ->
        "<SYNTAX ERROR>\n"
    | 469 ->
        "<SYNTAX ERROR>\n"
    | 683 ->
        "<SYNTAX ERROR>\n"
    | 684 ->
        "<SYNTAX ERROR>\n"
    | 685 ->
        "<SYNTAX ERROR>\n"
    | 686 ->
        "<SYNTAX ERROR>\n"
    | 687 ->
        "<SYNTAX ERROR>\n"
    | 688 ->
        "<SYNTAX ERROR>\n"
    | 482 ->
        "<SYNTAX ERROR>\n"
    | 679 ->
        "<SYNTAX ERROR>\n"
    | 483 ->
        "<SYNTAX ERROR>\n"
    | 672 ->
        "<SYNTAX ERROR>\n"
    | 485 ->
        "<SYNTAX ERROR>\n"
    | 490 ->
        "<SYNTAX ERROR>\n"
    | 501 ->
        "<SYNTAX ERROR>\n"
    | 513 ->
        "<SYNTAX ERROR>\n"
    | 2475 ->
        "<SYNTAX ERROR>\n"
    | 2476 ->
        "<SYNTAX ERROR>\n"
    | 2477 ->
        "<SYNTAX ERROR>\n"
    | 2478 ->
        "<SYNTAX ERROR>\n"
    | 2479 ->
        "<SYNTAX ERROR>\n"
    | 2480 ->
        "<SYNTAX ERROR>\n"
    | 460 ->
        "<SYNTAX ERROR>\n"
    | 693 ->
        "<SYNTAX ERROR>\n"
    | 682 ->
        "<SYNTAX ERROR>\n"
    | 677 ->
        "<SYNTAX ERROR>\n"
    | 678 ->
        "<SYNTAX ERROR>\n"
    | 461 ->
        "<SYNTAX ERROR>\n"
    | 462 ->
        "<SYNTAX ERROR>\n"
    | 564 ->
        "<SYNTAX ERROR>\n"
    | 568 ->
        "<SYNTAX ERROR>\n"
    | 581 ->
        "<SYNTAX ERROR>\n"
    | 572 ->
        "<SYNTAX ERROR>\n"
    | 580 ->
        "<SYNTAX ERROR>\n"
    | 573 ->
        "Expecting a valid list identifier\n"
    | 574 ->
        "<SYNTAX ERROR>\n"
    | 582 ->
        "<SYNTAX ERROR>\n"
    | 499 ->
        "<SYNTAX ERROR>\n"
    | 494 ->
        "<SYNTAX ERROR>\n"
    | 495 ->
        "<SYNTAX ERROR>\n"
    | 583 ->
        "<SYNTAX ERROR>\n"
    | 500 ->
        "<SYNTAX ERROR>\n"
    | 670 ->
        "<SYNTAX ERROR>\n"
    | 586 ->
        "<SYNTAX ERROR>\n"
    | 592 ->
        "<SYNTAX ERROR>\n"
    | 2714 ->
        "<SYNTAX ERROR>\n"
    | 2852 ->
        "<SYNTAX ERROR>\n"
    | 2756 ->
        "<SYNTAX ERROR>\n"
    | 2757 ->
        "<SYNTAX ERROR>\n"
    | 2763 ->
        "<SYNTAX ERROR>\n"
    | 922 ->
        "<SYNTAX ERROR>\n"
    | 2849 ->
        "<SYNTAX ERROR>\n"
    | 2890 ->
        "<SYNTAX ERROR>\n"
    | 2897 ->
        "<SYNTAX ERROR>\n"
    | 2896 ->
        "<SYNTAX ERROR>\n"
    | 2893 ->
        "<SYNTAX ERROR>\n"
    | 2894 ->
        "<SYNTAX ERROR>\n"
    | _ ->
        raise Not_found
