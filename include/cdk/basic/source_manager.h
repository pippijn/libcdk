#pragma once

#include "common.h"

#include <functional>

template<typename T>
class QList;
class QString;
class QFileInfo;

BEGIN_BASIC_NAMESPACE

struct VISIBLE source_manager
{
  typedef std::function<bool (QFileInfo)> find_fn;

  source_manager ();
  ~source_manager ();

  bool add (QString const &filename);
  bool add_recursive (QString const &dirname);

  fileref_vec files () const;
  fileref_vec find (find_fn rule) const;

  QString file_name (fileref const &ref) const;

  PIMPL;
};

END_BASIC_NAMESPACE
