#include "cdk/lex/file_input_manager.h"

#include "cdk/basic/source_manager.h"
#include "cdk/lex/yyvtable.h"
#include "cdk/parse/yyerror.h"

#include <stdexcept>

#include <QDebug>
#include <QFile>
#include <QString>

#include "yystate.h"

template<>
struct QTypeInfo<cdk::basic::fileref>
{
  enum {
    isPointer = false,
    isComplex = true,
    isStatic = true,
    isLarge = false,
    isDummy = false
  };
};

BEGIN_LEX_NAMESPACE

file_input_manager::file_input_manager (basic::source_manager const &sm,
                                        basic::fileref_vec const &files)
  : sm (sm)
  , it (files.begin ())
  , et (files.end ())
  , fh (NULL)
{
}

file_input_manager::~file_input_manager ()
{
  delete fh;
}

bool
file_input_manager::load (char const *&data, size_t &length)
{
  delete fh;
  fh = 0;

  if (it == et)
    return false;

  fh = new QFile (sm.file_name (*it));
  if (!fh->open (QIODevice::ReadOnly))
    throw std::runtime_error ("Could not open " + fh->fileName ().toStdString () + " for reading");
  ++it;

  qDebug () << "lexing" << fh->fileName ();

  length = fh->size ();
  data = reinterpret_cast<char const *> (fh->map (0, length));

  return 1;
}

QString
file_input_manager::current_file () const
{
  return fh->fileName ();
}

END_LEX_NAMESPACE
