#!/usr/bin/perl

use common::sense;

my $nodes = do $ARGV[0];

for (sort keys %$nodes) {
   say "template<> struct node_type_enum<ast::$_> { static node_type const value = n_$_; };";
}
