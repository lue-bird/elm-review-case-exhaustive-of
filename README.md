# elm-review-case-exhaustive-of

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to REPLACEME.

## Provided rules

- [`Review.CaseExhaustiveOf`](https://package.elm-lang.org/packages/lue-bird/elm-review-case-exhaustive-of/1.0.0/Review-CaseExhaustiveOf) - Reports REPLACEME.

## Configuration

```elm
module ReviewConfig exposing (config)

import Review.CaseExhaustiveOf
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ Review.CaseExhaustiveOf.rule
    ]
```

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template lue-bird/elm-review-case-exhaustive-of/example
```
