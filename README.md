# convoluted

Dependently typed convolutional neural networks in pure Haskell.
Uses the [repa library](https://hackage.haskell.org/package/repa) for high-performance arrays,
with a static wrapper that ensures networks are valid at compile-time.

### Example
```haskell
type BatchSize = 100
type MNIST = Network (ZZ ::. BatchSize ::. 1 ::. 28 ::. 28)
                     '[ Convolution 5 1 13 13 16 16
                      , Pool
                      , ReLU
                      , Flatten
                      , FC 320 10
                      , MultiSoftMax '[10] ]
                     (ZZ ::. BatchSize ::. 10)

mnist = randomNetwork seed :: MNIST
```
see [visor](https://github.com/jonascarpay/visor) or [convoluted-mnist](https://github.com/jonascarpay/convoluted-mnist) for examples.

#### Thanks
I borrowed heavily from [Huw Campbell's grenade](https://github.com/HuwCampbell/grenade), and like the author of that library, from [Justin Le's blog posts on dependently typed neural networks](https://blog.jle.im/entry/practical-dependent-types-in-haskell-1.html).
