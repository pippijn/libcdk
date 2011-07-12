#pragma once

#include <memory>

#define PIMPL                           \
  struct pimpl;                         \
protected:                              \
  std::unique_ptr<pimpl> self
