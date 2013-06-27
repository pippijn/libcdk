#pragma once

#include "common.h"

BEGIN_UTIL_NAMESPACE

template<typename T>
struct xenum
{
  typedef T type;
  template<typename O>
  struct compatible
  { static bool const value = false; };

  xenum (type value)
    : value (value)
  { }

  template<typename O>
  xenum (O value)
    : value (type (value))
  {
    static_assert (compatible<O>::value, "incompatible enumeration types");
  }

  operator type () const { return value; }

  type value;
};

#define XENUM(T, O)                     \
BEGIN_UTIL_NAMESPACE                    \
template<> template<>                   \
struct xenum<T>::compatible<O>          \
{ static bool const value = true; };    \
END_UTIL_NAMESPACE

END_UTIL_NAMESPACE
