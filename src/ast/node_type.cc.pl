#!/usr/bin/perl

use common::sense;

my $nodes = do $ARGV[0];

say '#include "node_type.h"';
say 'char const *const node_type_name[] = {';
for (sort keys %$nodes) {
   say "  \"$_\","
}
say "};"
