# Exchange

First build with
```
stack build
```
then use
```
stack exec Exchange
```
to run the application.

Tests can be run with
```
stack test
```
as usual.

## Orderbook logic
As of now, orders are evaluated with partial filling; "all-or-none" or
"fill-or-kill" style ordering complicates things and needs work.
