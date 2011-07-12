#include <google/protobuf/stubs/common.h>

static struct cleanup
{
  ~cleanup () { google::protobuf::ShutdownProtobufLibrary (); }
} cleanup;
