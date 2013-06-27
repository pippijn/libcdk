#include "cdk/basic/source_manager.h"

#include "cdk/basic/fileref.h"

#include <QDebug>
#include <QFileInfo>

using namespace cdk::basic;

int
main ()
{
  source_manager sm;

  sm.add (__FILE__);
  sm.add (__FILE__);
  sm.add (__FILE__);
  sm.add (__FILE__);
  sm.add (__FILE__);

  fileref_vec found = sm.find ([] (QFileInfo s) { return s == QFileInfo (__FILE__); });
  for (fileref ref : found)
    qDebug () << sm.file_name (ref);

  sm.add_recursive ("../src/libcdk/basic");

  found = sm.files ();
  found = sm.find ([] (QFileInfo s) { return s.suffix () == "cc"; });
  for (fileref ref : found)
    qDebug () << sm.file_name (ref);

  return 0;
}
