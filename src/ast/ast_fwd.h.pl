#!/usr/bin/perl

use common::sense;

my $nodes = do $ARGV[0];

say '#pragma once';
say 'namespace ast {';
for (sort keys %$nodes) {
   print <<EOF;
  struct $_;
EOF
}
say '}';
