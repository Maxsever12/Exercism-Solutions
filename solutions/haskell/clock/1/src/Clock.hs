module Clock (addDelta, fromHourMin, toString) where

type Hour = Int
type Minute = Int
data Clock = Dummy Minute 
  deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = (Dummy ((hour*60 + min) `mod` 1440))

toString :: Clock -> String
toString (Dummy minute) = pad(mod(div minute 60)24) ++ ":" ++ pad(mod minute 60)
  where 
    pad x
      | x < 10 = "0" ++ show(x)
      | otherwise = show(x)

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Dummy minute) = (Dummy((hour*60 + min + minute) `mod` 1440))