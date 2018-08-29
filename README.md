# Exchange

Use
```
stack build
stack exec Exchange
```
to run the application.

Tests can be run with `stack test` as usual.

## Orderbook logic
As of now, orders are evaluated with partial filling; "all-or-none" or
"fill-or-kill" style ordering complicates things and needs work.

## Communicating with the exchange
By default, the exchange runs on `localhost:8080`. The exchange handles requests
via HTTP POSTs adhering to the following format, by request type:

### Limit
```
{ type     : "LIMIT"
, quantity : Int
, price    : Float
, dir      : String
, tid      : String
, ticker   : String
}
```

### Market
```
{ type     : "MARKET"
, quantity : Int
, dir      : String
, tid      : String
, ticker   : String
}
```

### Security registration
```
{ type   : "REGISTERS"
, ticker : String
}
```

### Trader registration
```
{ type   : "REGISTERT"
, trader : String
}
```

### Status
```
{ type : "STATUS" }
```

### Order cancellation
```
{ type     : "CANCEL"
, oid      : String
, tid      : String
, ticker   : String
}
```
