# haskell-ddd-order-taking

[![build](https://github.com/nakaakist/haskell-ddd-order-taking/actions/workflows/ci.yaml/badge.svg)](https://github.com/nakaakist/haskell-ddd-order-taking/actions/workflows/ci.yaml)

## About

This repository is Haskell implementation of the ["Domain Modeling Made Functional"](https://learning.oreilly.com/library/view/domain-modeling-made/9781680505481/) book.  
(in the book, sample codes are written in F#).

The "place order" workflow in the book is implemented.

## Run application

### start server

run `stack run start` to start an API server at `localhost:3000`.

### send requests

you can send POST requests to `/orders` endpoint.  
request body contains "unvalidated order", and response body contains "place order events."

#### via Postman

you can use Postman to send requests using out-of-the-box request templates.  
import the request collection file (`test-postman/collection.json`) and the environment variables file (`test-postman/variables.json`) to Postman.

#### via curl

example request is the following.

```bash
curl --location --request POST 'http://localhost:3000/orders' \
--header 'Content-Type: application/json' \
--data-raw '{
    "orderId": "test-order-id",
    "customerInfo": {
        "firstName": "test-first-name",
        "lastName": "test-last-name",
        "emailAddress": "test@example.com"
    },
    "shippingAddress": {
        "addressLine1": "test shipping street 1",
        "addressLine2": "test shipping street 2",
        "addressLine3": "test shipping street 3",
        "addressLine4": "test shipping street 4",
        "city": "test shipping city",
        "zipCode": "12345"
    },
    "billingAddress": {
        "addressLine1": "test billing street",
        "addressLine2": "",
        "addressLine3": "",
        "addressLine4": "",
        "city": "test billing city",
        "zipCode": "54321"
    },
    "orderLines": [
        {
            "orderLineId": "test-order-line-id1",
            "productCode": "W1234",
            "quantity": 1
        },
        {
            "orderLineId": "test-order-line-id2",
            "productCode": "G123",
            "quantity": 1.25
        }
    ]
}'
```

## Automated test using hspec and sensei

run `stack test` to run all tests.

run `stack exec sensei test/Spec.hs` to run all tests on any modifications to the codes.

## References

I referred to the following repositories and websites during development.

- https://github.com/swlaschin/DomainModelingMadeFunctional
  - official sample codes of the "Domain Modeling Made Functional" book
- https://github.com/bontaq/domain-modeling
  - Haskell implementation of the "Domain Modeling Made Functional" book
- https://github.com/eckyputrady/haskell-scotty-realworld-example-app
  - web app by Scotty
- https://taylor.fausak.me/2018/03/16/record-fields-break-smart-constructors/
  - smart constructors in Haskell
- https://odone.io/posts/2019-09-02-merging-io-and-either-into-one-monad.html
  - EitherIO monad (= AsyncResult monad in the book)
