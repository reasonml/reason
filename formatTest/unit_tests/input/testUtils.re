/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

let printSection s => {
  print_string "\n";
  print_string s;
  print_string "\n---------------------\n";
};

let printLn s => print_string (s ^ "\n");
