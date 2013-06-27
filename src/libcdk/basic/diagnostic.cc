#include "cdk/basic/diagnostic.h"

BEGIN_BASIC_NAMESPACE

struct diagnostic::pimpl
{
  ast::node_ptr const node;
  diagnostic_kind const kind;
  std::string const message;
  std::string const note;

  pimpl (ast::node_ptr node, diagnostic_kind kind, std::string const &message, std::string const &note)
    : node (node)
    , kind (kind)
    , message (message)
    , note (note)
  {
  }
};

diagnostic::diagnostic (ast::node_ptr node, diagnostic_kind kind, std::string const &message, std::string const &note)
  : self (new pimpl (node, kind, message, note))
{
}

diagnostic::~diagnostic ()
{
}

diagnostic_kind
diagnostic::kind () const
{
  return self->kind;
}

ast::node_ptr const &
diagnostic::node () const
{
  return self->node;
}

std::string const &
diagnostic::message () const
{
  return self->message;
}

std::string const &
diagnostic::note () const
{
  return self->note;
}

END_BASIC_NAMESPACE
