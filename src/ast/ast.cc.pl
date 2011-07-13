#!/usr/bin/perl

use common::sense;

my $nodes = do $ARGV[0];

say '#include "ast.h"';
say '#include "cdk/ast/visitor.h"';
say 'namespace ast {';
say 'using cdk::ast::visitor;';
for (sort keys %$nodes) {
  say "void ${_}::accept (visitor &v) { v.visit (*this); }";
}
say '}';
