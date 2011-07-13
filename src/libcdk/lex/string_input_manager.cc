#include "cdk/lex/string_input_manager.h"

#include "cdk/lex/yyvtable.h"

#include <QString>

static QString const string_file = "<string>";

BEGIN_LEX_NAMESPACE

string_input_manager::string_input_manager (QString const &s)
  : str (s.trimmed ().toUtf8 ())
  , used (false)
{
}

string_input_manager::~string_input_manager ()
{
}

bool
string_input_manager::load (char const *&data, size_t &length)
{
  if (used)
    return false;

  data = str.data ();
  length = str.length ();

  used = true;

  return true;
}

QString
string_input_manager::current_file () const
{
  return string_file;
}

END_LEX_NAMESPACE
