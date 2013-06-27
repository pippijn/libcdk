#!/usr/bin/perl

use common::sense;

my $nodes = do $ARGV[0];

say 'enum node_type : int {';
for (sort keys %$nodes) {
   say "  n_$_,";
}
say '  NODE_TYPES';
say '};';
say 'extern VISIBLE char const *const node_type_name[];';
