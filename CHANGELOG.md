# 2.0.0

Added:
- `limit : Int -> HttpBuilder.RequestBuilder a -> HttpBuilder.RequestBuilder a`
- `toRequest : HttpBuilder.RequestBuilder a -> Http.Request a`

Changed:
- Expose the full `Filter` type:
```elm
type Filter
    = BEFORE String
    | Equal String String
    | GT String String
    | IN String (List String)
    | LIKE String String
    | LT String String
    | Max String String
    | Min String String
    | NOT String String
    | SINCE String
```

# 1.0.0

This is the very first version of the elm-kinto cient.
