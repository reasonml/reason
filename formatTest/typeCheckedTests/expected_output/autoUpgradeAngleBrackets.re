/**
 * Even if you have an explicit v3.6 marker.
 * This whole file wil be auto-upaded to 3.8 becase something uses
 * angle brackets.
 */;
[@reason.version 3.8];
let watchThisIsOldStyle: list<int> = [1, 2];

let watchThisIsOldStylePoly = #hello;

/**
 * This will cause the whole file to be promoted.
 */
let x: list<int> = [1, 3];
