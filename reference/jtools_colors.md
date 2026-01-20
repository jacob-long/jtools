# Color palettes in `jtools` functions

`jtools` combines several options into the `colors` argument in plotting
functions.

## Details

The argument to `colors` in functions like `effect_plot`, `plot_coefs`,
and others is very flexible but may also cause confusion.

If you provide an argument of length 1, it is assumed that you are
naming a palette. `jtools` provides 6 color palettes design for
qualitative data. 4 of the 6 are based on Paul Tol's suggestions (see
references) and are meant to both optimize your ability to quickly
differentiate the colors and to be distinguishable to colorblind people.
These are called `"Qual1"`, `"Qual2"`, `"Qual3"`, `"CUD"`,
`"CUD Bright"`, and `"Rainbow"`. Each of the "Qual" schemes comes from
Paul Tol. "Rainbow" is Paul Tol's compromise rainbow color scheme that
is fairly differentiable for colorblind people and when rendered in
grayscale. `"CUD Bright"` is a brightened and reordered version of Okabe
and Ito's suggestions for 'Color Universal Design' while `"CUD"` is
their exact scheme (see references). `"CUD Bright"` is the default for
qualitative scales in `jtools` functions.

You may also provide any color palette supported by `RColorBrewer`. See
all of those options at
[`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html)'s
documentation. If you provide one of `RColorBrewer`'s sequential
palettes, like "Blues", `jtools` automatically requests one more color
than needed from `brewer.pal` and then drops the lightest color. My
experience is that those scales tend to give one color that is too light
to easily differentiate against a white background.

For **gradients**, you can use any of the `RColorBrewer` sequential
palette names and get comparable results on a continuous scale. There
are also some `jtools`-specific gradient schemes: `"blue"`, `"blue2"`,
`"green"`, `"red"`, `"purple"`, `"seagreen"`. If you want something a
little non-standard, I'd suggest taking a look at `"blue2"` or
`"seagreen"`.

Lastly, you may provide colors by name. This must be a vector of the
same length as whatever it is the colors will correspond to. The format
must be one understood by `ggplot2`'s manual scale functions. This
basically means it needs to be in hex format (e.g., "#000000") or one of
the many names R understands (e.g., "red"; use
[`colors()`](https://rdrr.io/r/grDevices/colors.html) to see all of
those options).

## References

Paul Tol's site is what is used to derive 4 of the 6 `jtools`-specific
qualitative palettes:
<https://web.archive.org/web/20200304220659/https://personal.sron.nl/~pault/>

Okabe and Ito's palette inspired "CUD Bright", though "CUD Bright" is
not exactly the same. "CUD" is the same. See
<https://web.archive.org/web/20190216090108/jfly.iam.u-tokyo.ac.jp/color/>
for more.
