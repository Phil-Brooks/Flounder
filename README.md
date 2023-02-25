# Flounder

An attempt at a Chess Engine in F#. This was initially was just a conversion from the excellent [StockNemo](https://github.com/TheBlackPlague/StockNemo).

The code has then been significantly re-factored to be more in line with standard F# coding.

## Features

### Included

The engine includes:

- Null Move Pruning
- Late Move Pruning
- Reverse Futility Pruning
- Three-fold Repetition Pruning
- Mate Distance Pruning
- Material Draw Pruning
- Check Extension
- Quiescence Search with SEE Pruning

### Not Included

The following features were not included as they were tested and seemed to give no benefit:

- Futility Pruning
- Late Move Reduction

