
--BEGIN:GEOM1
data Geometry = Point     Float Float              -- x and y
              | Circle    Float Float Float        -- x, y, and radius
              | Rectangle Float Float Float Float  -- top, left, right, bottom
              | Composite [Geometry]               -- list of geometry objects
--END:GEOM1
  deriving (Show, Eq)

--BEGIN:GEOM2
area :: Geometry -> Float
area (Point x y)         = 0
area (Circle x y r)      = pi * r ^ 2
area (Rectangle t l r b) = (b - t) * (r - l)
area (Composite cs)      = sum [ area c | c <- cs ]
--END:GEOM2

main = do
  print $ area (Composite [Point 3 10, Circle 10 10 10, Rectangle 0 0 100 10])