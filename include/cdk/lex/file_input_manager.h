#pragma once

#include "cdk/lex/input_manager.h"
#include "cdk/basic/fileref_vec.h"

class QFile;

BEGIN_LEX_NAMESPACE

struct VISIBLE file_input_manager
  : input_manager
{
  file_input_manager (basic::source_manager const &sm,
                      basic::fileref_vec const &files);
  ~file_input_manager ();

  virtual bool load (char const *&data, size_t &length);
  virtual QString current_file () const;

private:
  basic::source_manager const &sm;
  basic::fileref_vec::const_iterator it;
  basic::fileref_vec::const_iterator et;

  std::unique_ptr<QFile> fh;
};

END_LEX_NAMESPACE
