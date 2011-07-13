#!/usr/bin/perl

use common::sense;

my $nodes = do $ARGV[0];

for (sort keys %$nodes) {
   say "  virtual void visit (::ast::$_ &n);"
}
