#include "cdk/basic/source_manager.h"

#include "cdk/basic/fileref.h"
#include "cdk/basic/fileref_vec.h"
#include "cdk/util/grep.h"

#include <algorithm>

#include <QDebug>
#include <QDir>
#include <QFileInfo>
#include <QList>

BEGIN_BASIC_NAMESPACE

struct source_manager::pimpl
{
  struct file
  {
    QFileInfo info;
    size_t index;

    file (QFileInfo const &info, size_t index)
      : info (info)
      , index (index)
    {
    }

    bool operator < (file const &rhs) const
    {
      return info.absoluteFilePath () < rhs.info.absoluteFilePath ();
    }

    bool operator == (file const &rhs) const
    {
      return info == rhs.info;
    }
  };

  QList<file> files;

  bool add (QFileInfo const &info)
  {
    if (!info.exists ())
      return false;
    files.push_back (file (info, files.size ()));
    return true;
  }

  bool add_recursive (QFileInfo const &file)
  {
    QString name = file.fileName ();
    if (name == "." || name == "..")
      return false;

    if (file.isFile ())
      return add (file);

    if (file.isDir ())
      {
        QDir dir (file.absoluteFilePath ());
        for (QFileInfo info : dir.entryInfoList ())
          add_recursive (info);
        return true;
      }

    return false;
  }

  void compact ()
  {
    std::sort (files.begin (),
               files.end ());
    files.erase (std::unique (files.begin (),
                              files.end ()),
                 files.end ());
    for (file &f : files)
      f.index = files.indexOf (f);
  }

  static fileref_vec make_file_vec (QList<file> const &files)
  {
    fileref_vec vec;
    std::transform (files.begin (),
                    files.end (),
                    std::back_inserter (vec),
                    [] (file const &f) { return fileref { f.index }; });
    return vec;
  }
};


source_manager::source_manager ()
  : self (new pimpl)
{
}

source_manager::~source_manager ()
{
}


bool
source_manager::add (QString const &file)
{
  if (!self->add (file))
    return false;
  self->compact ();
  return true;
}

bool
source_manager::add_recursive (QString const &file)
{
  if (!self->add_recursive (file))
    return false;
  self->compact ();
  return true;
}

fileref_vec
source_manager::files () const
{
  return pimpl::make_file_vec (self->files);
}

fileref_vec
source_manager::find (find_fn rule) const
{
  QList<pimpl::file> found;
  std::copy_if (self->files.begin (),
                self->files.end (),
                std::back_inserter (found),
                [&rule] (pimpl::file const &f) { return rule (f.info); });
  return pimpl::make_file_vec (found);
}

QString
source_manager::file_name (fileref const &ref) const
{
  return self->files.at (ref.index).info.absoluteFilePath ();
}

END_BASIC_NAMESPACE
