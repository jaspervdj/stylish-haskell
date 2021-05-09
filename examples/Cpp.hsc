module Cpp where

#include <something.h>

data Foo = Foo
    { bar :: Int
#if 0
    , bazquux :: Int8
#else
    , bazquux :: Int16
#endif
    }

main :: IO ()
main = pure ()
