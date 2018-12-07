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
            res <- peek naem
            go right
            go right
            return res
      
        print res
```

The `tree'` should be a `tree` with `"lol"` replaced by `"HELLO"` and the `res` would be `"bar"`.

This example doesn't include `Handlers` for the sake of brevity. Basically, the are post-actions to
be done after you do something. They return a `Bool` and if its `False`, their effect on the object
is discarded (but not the effect on the additional state!).

## Capabilites

This is an implementation of iterator that can

1. `go` down the structure using a `Lens` (that might fail to deliver the destination).
2. Go `up` to the parent node.
3. Perform `change` on a node.
4. Do `plugState` to plug in some change on additional state.

Also, each of these `Action`s triggers a handler you provided.

You run it `with` some object, `Config`uration (additional state and `Handlers`) and an `Action`
to be performed. After its done with its actions, it reconstructs the object as a whole - also returning final form of additional state and an action result.

You can also run it using `with'`. In that case you don't have to provide
additional state (will be `()`) and handlers (will be inert).

It also runs on top of any monad you like, and so do its handlers!
Just make sure you can `throwM` in it.

It also doesn't put any constrains on traversed object whatsoever.

You can traverse `Int` if you so choose, just find some `Direction`s or `Lens`es over it.

## Handlers.

Handlers are used if you want to update your structure after changes in some non-trivial way.

1. `onUp` will fire if you go `up`, it will receive `Bool` argument specifying if there were any changes.
2. `onDown` will fire each time you `go` somewhere. It will also receive the name of the direction.
3. `onChange` will fire each time you call `change` - so you can post-update your structure.

## Dependencies

It doesn't depend on [lens](http://hackage.haskell.org/package/lens) package, I used [microlens-platform](http://hackage.haskell.org/package/microlens-platform) instead.
