#pragma once

#include "common.h"

#include "cdk/util/xenum.h"

BEGIN_BASIC_NAMESPACE

enum diagnostic_class
{
  DC_ERROR      = 1,
  DC_WARNING    = 2,
  DC_NOTE       = 3,
  DC_MASK       = 0x3,
};

#define DC_ERROR(Diag)   (Diag + DC_MASK + 1 | ::cdk::basic::DC_ERROR  )
#define DC_WARNING(Diag) (Diag + DC_MASK + 1 | ::cdk::basic::DC_WARNING)
#define DC_NOTE(Diag)    (Diag + DC_MASK + 1 | ::cdk::basic::DC_NOTE   )

enum diagnostic_kind_base
{
  err_file_not_found          = DC_ERROR   (0),
  err_unknown_argument        = DC_ERROR   (1),

  warn_unused_argument        = DC_WARNING (0),

  note_file_exists            = DC_NOTE    (0),
};

typedef util::xenum<diagnostic_kind_base> diagnostic_kind;

VISIBLE diagnostic_class classify (diagnostic_kind kind);

VISIBLE bool is_error (diagnostic_kind kind);
VISIBLE bool is_warning (diagnostic_kind kind);
VISIBLE bool is_note (diagnostic_kind kind);

END_BASIC_NAMESPACE
