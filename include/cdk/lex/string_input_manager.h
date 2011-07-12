#pragma once

#include "cdk/lex/input_manager.h"

#include <string>

BEGIN_LEX_NAMESPACE

struct VISIBLE string_input_manager
  : input_manager
{
  string_input_manager (std::string const &s);
  ~string_input_manager ();

  virtual bool load (char const *&data, size_t &length);
  virtual QString current_file () const;

private:
  std::string str;
  bool used;
};

END_LEX_NAMESPACE
