To solve this, we must define a type for the list of functions that ensures they all have the same return type. Here's how you can do it by using a more specific type signature or by employing a type class:

**Solution 1: Specific Type Signature**
```haskell
myFunctions :: [Int -> Int]
myFunctions = [(+1), (*2)]

main :: IO ()
main = do
  print $ length myFunctions
```
This restricts the list to functions that take an `Int` and return an `Int`.  `length` will now work correctly.

**Solution 2:  Using a type class (more flexible)**
This approach is more flexible, allowing a wider variety of functions as long as they share a common behavior (like being numeric).

```haskell
class Num a => MyNum a where
  myFunc :: a -> a

instance MyNum Int where
  myFunc x = x + 1

instance MyNum Double where
  myFunc x = x * 2

myFunctions :: [MyNum a => a -> a]
myFunctions = [myFunc, myFunc]

main :: IO ()
main = do
  print $ length myFunctions
```
This solution uses a type class `MyNum` to abstract the function type.  This is more complex for a simple example, but is powerful when dealing with a wider range of numeric types.