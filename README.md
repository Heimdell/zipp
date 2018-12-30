# Extensible zipper.

## Slogan

Traverse any datatype you want! Even an `Int`! No constraints on traversed object!*

_* You still need to provide `Lens`es, `Traversal`s or `Direction`s, though._

## Example

```haskell
    data Tree = Bin { _l, _r :: Tree, _naem :: String } | Nip { _naem :: String }
    
    makeLenses ''Tree
    
    data Dir = L | R deriving Show
    
    test = do
        let tree = Bin (Bin (Nip "lol") (Nip "kek") "lal") (Bin (Nip "foo") (Nip "bar") "qux") "all"
    
        let left  = fromTraversal L l
        let right = fromTraversal R r
     
        (tree', res) <- with' tree $ do
            go left
            go left
            change (naem .~ "HELLO")
            up
            up
            go right
            go right
            res <- peek naem
            return (res ++ "?")
      
        print res
```

The `tree'` should be a `tree` with `"lol"` replaced by `"HELLO"` and the `res` would be `"bar?"`.

## Documentation

You can do `stack haddock`.

## Capabilites

This is an implementation of iterator that can

1. `go` down the structure using a `Direction` (that might fail to deliver the destination).
2. Go `up` to the parent node.
3. Perform `change` on a node.

You run it `with` some object and an `Action`
to be performed. After its done with its actions, it reconstructs the object as a whole - also returning an action result.

It runs on top of any monad you like, and so do its handlers!
Just make sure you can `throwM` in it.

It also doesn't put any constrains on traversed object whatsoever.

You can traverse `Int` if you so choose, just find some `Direction`s or `Lens`es over it.

## Dependencies

It doesn't depend on [lens](http://hackage.haskell.org/package/lens) package, I used [microlens-platform](http://hackage.haskell.org/package/microlens-platform) instead.
