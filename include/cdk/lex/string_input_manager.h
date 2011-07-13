#pragma once

#include "cdk/lex/input_manager.h"

#include <QByteArray>

BEGIN_LEX_NAMESPACE

struct VISIBLE string_input_manager
  : input_manager
{
  string_input_manager (QString const &s);
  ~string_input_manager ();

  virtual bool load (char const *&data, size_t &length);
  virtual QString current_file () const;

private:
  QByteArray str;
  bool used;
};

END_LEX_NAMESPACE
