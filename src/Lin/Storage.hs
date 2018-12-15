module Lin.Storage where 

import Prelude

import Data.Vector

import Foreign.Ptr 
import Foreign.Storable 
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array  

import Linear


newtype RowMajor a = RowMajor (M44 a) 

newtype ColMajor a = ColMajor (M44 a)

instance Storable a => Storable (ColMajor a) where 

  sizeOf (ColMajor m) = sizeOf (RowMajor m)

  alignment (ColMajor m) = alignment (RowMajor m)

  peekElemOff ptr offset 
    = do
        RowMajor m <- peekElemOff (castPtr ptr :: Ptr (RowMajor a)) offset
        pure $ ColMajor (transpose m)

  pokeElemOff ptr offset (ColMajor m) 
    = pokeElemOff (castPtr ptr :: Ptr (RowMajor a)) offset (RowMajor $ transpose m)

instance (Storable a) => Storable (RowMajor a) where 

  sizeOf (RowMajor m) = Prelude.sum (sizeOf <$> m)

  alignment (RowMajor (V4 r1 _ _ _)) = alignment r1

  peekElemOff ptr offset
    = let 
        rowPtr = castPtr ptr :: Ptr (V4 a) 
        start  = plusPtr ptr (fromIntegral $ 4 * offset)
      in 
        do
          r1 <- peekElemOff start 0
          r2 <- peekElemOff start 1
          r3 <- peekElemOff start 2
          r4 <- peekElemOff start 3
          pure $ RowMajor (V4 r1 r2 r3 r4)
        
  {- INLINE pokeElemOff -}
  pokeElemOff ptr offset rm@(RowMajor (V4 r1 r2 r3 r4))
    = let 
        rowPtr = castPtr (plusPtr ptr . fromIntegral $ offset * sizeOf rm) :: Ptr (V4 a) 
      in 
        do 
          pokeElemOff rowPtr 0 r1         
          pokeElemOff rowPtr 1 r2
          pokeElemOff rowPtr 2 r3
          pokeElemOff rowPtr 3 r4

storeTransformsByRow :: Storable a => Vector (RowMajor a)
                                   -> IO (Ptr (RowMajor a))
storeTransformsByRow v = 
  do 
    ptr <- callocBytes . Data.Vector.sum $ sizeOf <$> v 

    imapM_ (\i m -> pokeElemOff ptr i m) v 

    pure ptr