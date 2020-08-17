/**
 * Test auto-promotion based on feature inference even if no version
 * tag. By default you're using the old 3.7.
 */
let watchThisIsOldStyle : list(int) = [1, 2];

let watchThisIsOldStylePoly = `hello;

/**
 * This will cause the whole file to be promoted.
 */
let x : list<int> = [1, 3];
