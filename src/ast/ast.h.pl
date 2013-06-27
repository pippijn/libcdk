#!/usr/bin/perl

use common::sense;

my $nodes = do $ARGV[0];

say '#pragma once';
say '#include "cdk/ast/generic_node.h"';
say '#include "node_type.h"';
say 'namespace ast {';
for (sort keys %$nodes) {
   print <<EOF;
  struct VISIBLE $_
    : cdk::ast::generic_node
  {
    virtual void accept (cdk::ast::visitor &v);

    $_ (location const &loc)
      : generic_node (loc, n_$_)
    { }
  };

EOF
}
say '}';
say '#include "node_enum.h"';
