# haskell-ddd-order-taking

## automated test using hspec and sensei

run `stack test` to run all tests.

run `stack exec sensei test/Spec.hs` to run all tests on any modifications to the codes.

## References

I used the following repositories and websites as references.

- https://github.com/swlaschin/DomainModelingMadeFunctional
  - official sample codes of the "Domain Modeling Made Functional" book
- https://github.com/nakaakist/haskell-ddd-order-taking
  - Haskell implementation of the "Domain Modeling Made Functional" book
- https://github.com/eckyputrady/haskell-scotty-realworld-example-app
  - web app by Scotty
- https://taylor.fausak.me/2018/03/16/record-fields-break-smart-constructors/
  - smart constructors in Haskell
- https://odone.io/posts/2019-09-02-merging-io-and-either-into-one-monad.html
  - EitherIO monad (= AsyncResult monad in the book)
