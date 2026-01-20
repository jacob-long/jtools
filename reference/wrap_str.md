# `cat`, `message`, `warning`, and `stop` wrapped to fit the console's width.

These are convenience functions that format printed output to fit the
width of the user's console.

## Usage

``` r
wrap_str(..., sep = "")

cat_wrap(..., brk = "")

warn_wrap(..., brk = "\n", class = NULL, call. = FALSE)

stop_wrap(
  ...,
  brk = "\n",
  trace = rlang::trace_back(bottom = rlang::caller_env()),
  class = NULL,
  call = rlang::caller_env(),
  call. = FALSE
)

msg_wrap(..., class = NULL, brk = "\n")
```

## Arguments

- ...:

  Objects to print. For `stop_wrap()`, `warn_wrap()`, and `msg_wrap()`,
  any named objects are instead diverted to the `...` argument of
  [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html),
  [`rlang::warn()`](https://rlang.r-lib.org/reference/abort.html), and
  [`rlang::inform()`](https://rlang.r-lib.org/reference/abort.html),
  respectively.

- sep:

  Separator between `...`, Default: ”

- brk:

  What should the last character of the message/warning/error be?
  Default is `"\n"`, meaning the console output ends with a new line.

- class:

  Subclass of the condition.

- call.:

  Here for legacy reasons. It is ignored.

- trace:

  A `trace` object created by
  [`trace_back()`](https://rlang.r-lib.org/reference/trace_back.html).

- call:

  The actual calling environment to report in the error message. By
  default,
  [`rlang::caller_env()`](https://rlang.r-lib.org/reference/stack.html).

## Details

The point of these functions is to allow you to print
output/messages/warnings/errors to the console without having to figure
out where to place newline characters. These functions get the width of
the console from the `"width"` option, which in most editors adjusts
dynamically as you resize.

So instead of writing a warning like this:

    warning("I have to give you this very important message that may be too\n",
            "wide for your screen")

You can do it like this:

    warn_wrap("I have to give you this very important message that may be
              too wide for your screen")

And the function will automatically insert line breaks to fit the
console. As a note, it will also ignore any newlines you insert. This
means you can make your own fit your editor's screen and indent in the
middle of a string without that formatting being carried over into the
output.
