#!/usr/bin/env bash

# run 'stack-only' tests with some resolver

if [ "$#" -ne 1 ]; then
  echo "expected one arg, a resolver to use (e.g. 'lts-11')"
  exit 1
fi

export STACK_RESOLVER="$1"
stack --resolver="${STACK_RESOLVER}" test --flag assumpta-core:stack-based-tests

