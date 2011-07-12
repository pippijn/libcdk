#include "cdk/basic/diagnostics.h"

#include "cdk/basic/diagnostic.h"

#include <algorithm>

BEGIN_BASIC_NAMESPACE

struct diagnostics::pimpl
{
  typedef std::vector<diagnostic *> error_vec;

  ~pimpl ()
  {
    for (diagnostic *e : log)
      delete e;
  }

  error_vec log;
};

diagnostics::diagnostics ()
  : self (new pimpl)
{
}

diagnostics::~diagnostics ()
{
}

void
diagnostics::add (diagnostic *e)
{
  self->log.push_back (e);
}

void
diagnostics::add (ast::node_ptr const &node, diagnostic_kind kind, std::string const &message, std::string const &note)
{
  add<diagnostic> (node, kind, message, note);
}

bool
diagnostics::has_diagnostics () const
{
  return !self->log.empty ();
}

static bool
is_error_diag (diagnostic const *e)
{
  return is_error (e->kind ());
}

bool
diagnostics::has_errors () const
{
  return find_if (self->log.begin (), self->log.end (), is_error_diag) != self->log.end ();
}

size_t
diagnostics::count () const
{
  return self->log.size ();
}

size_t
diagnostics::error_count () const
{
  return count_if (self->log.begin (), self->log.end (), is_error_diag);
}

END_BASIC_NAMESPACE
