#!/usr/bin/perl

use common::sense;

my $nodes = do $ARGV[0];

for (sort keys %$nodes) {
   say "void visitor::visit (::ast::$_ &n) { return visit (static_cast<ast::generic_node &> (n)); }"
}
