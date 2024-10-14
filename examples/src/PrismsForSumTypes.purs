module PrismsForSumTypes where

{- Prisms are optics that "focus" on one case of a sum type. They can
   also be used for other kinds of case analysis, but sum types are most
   common. This introduction only discusses sum types.

   Use a Prism if you want to write code like this:

      preview prismForSolidFill $ Solid Color.white
      -- Just Color.white

      preview prismForSolidFill NoFill
      -- Nothing

      review prismForSolidFill Color.white
      -- Solid Color.white
-}

{-   If you want to try out examples, paste the following into the repl.

import PrismsForSumTypes
import Data.Lens
import Data.Lens.Prism
import Color as Color
import Data.Maybe

See `README.md` if you're wondering why code is formatted the way it is.
-}

import Prelude

import Color (Color)
import Color as Color
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens (Prism', is, isn't, nearly, only, preview, prism, prism', review)
import Data.Maybe (Maybe(..), maybe)

{- The types in question -}

newtype Percent = Percent Number
data Point = Point Number Number

data Fill -- think of a paint program filling a shape
  = Solid Color
  | LinearGradient Color Color Percent
  | RadialGradient Color Color Point
  | NoFill

{------ Some samples to work with ------}

fillBlackToWhite :: Fill
fillBlackToWhite = LinearGradient Color.black Color.white $ Percent 3.3

fillWhiteToBlack :: Fill
fillWhiteToBlack = LinearGradient Color.white Color.black $ Percent 3.3

fillRadial :: Fill
fillRadial = RadialGradient Color.white Color.black $ Point 1.0 3.4

{------ Making prisms with Maybe and `prism'` ------}

-- `prism'` (note the apostrophe) takes two functions. One is a data
-- constructor for the type in question. The other converts your
-- desired case to a `Just <wrapped values>` or `Nothing`.

solidFocus :: Prism' Fill Color
solidFocus = prism' constructor focus
  where
  constructor = Solid
  focus fill = case fill of
    Solid color -> Just color
    _ -> Nothing

-- In real life, you might abbreviate the above to this:

solidFocus' :: Prism' Fill Color
solidFocus' = prism' Solid case _ of
  Solid color -> Just color
  _ -> Nothing

-- ... but being painfully explicit is better for examples.

{------ Basic usage: `preview`, `review`, `is`, and `isn't` ------}

-- After building a prism, you focus in on a color with `preview`:

s1 :: Maybe Color
s1 = preview solidFocus (Solid Color.white)

-- (Just rgba 255 255 255 1.0)

s2 :: Maybe Color
s2 = preview solidFocus fillRadial

-- Nothing

-- ... or you can create a Fill from a color with `review`:

s3 :: Fill
s3 = review solidFocus Color.white

-- (Solid rgba 255 255 255 1.0)

-- ... or you can ask whether a given value matches the prism:

s4 :: Boolean
s4 = is solidFocus (Solid Color.white) :: Boolean

-- true

s5 :: Boolean
s5 = isn't solidFocus (Solid Color.white) :: Boolean

-- false

{------ Making prisms with Either and `prism` ------}

-- Since `LinearGradient` wraps two colors and a percent, they need to
-- be bundled together into a single value for `preview` to
-- return. I'll use a record:

type LinearInterchange =
  { color1 :: Color
  , color2 :: Color
  , percent :: Percent
  }

-- When making a prism with `prism` (no apostrophe), the "focus"
-- function returns either the selected value (as a `Right`) or the
-- entire argument (as a `Left`).

linearFocus :: Prism' Fill LinearInterchange
linearFocus = prism constructor focus
  where
  constructor { color1, color2, percent } =
    LinearGradient color1 color2 percent
  focus = case _ of
    LinearGradient color1 color2 percent ->
      Right { color1, color2, percent }
    otherCases ->
      Left otherCases

-- Even though made differently than `solidFocus`, `linearFocus` is
-- used the same way:

l1 :: String
l1 = preview linearFocus fillBlackToWhite # maybe "!" show

-- "{ color1: rgba 0 0 0 1.0, color2: rgba 255 255 255 1.0, percent: (3.3%) }"

l2 :: Fill
l2 = review linearFocus
  { color1: Color.black
  , color2: Color.white
  , percent: Percent 33.3
  }

{------ Use `only` to focus on specific values ------}

whiteToBlackFocus :: Prism' Fill Unit
whiteToBlackFocus = only fillWhiteToBlack

o1 :: Boolean
o1 = is whiteToBlackFocus fillWhiteToBlack :: Boolean

-- true

o2 :: Boolean
o2 = is whiteToBlackFocus fillBlackToWhite :: Boolean

-- false

o3 :: Boolean
o3 = is whiteToBlackFocus fillRadial :: Boolean

-- false

-- Note that `only` requires `Fill` to implement `Eq`.
-- It's the only prism constructor that does.

{------ Use `nearly` to focus on a sub-case ------}

-- `nearly` is typically used to look for a specific case (like other
-- prisms), but also accepts only values that are close to some target
-- value. It takes two values: a reference value, and a predicate that
-- determines whether the wrapped value(s) are close enough to the
-- reference. Note that the predicate takes the "whole" type (here,
-- `Fill`), not the unwrapped values inside the case you care about.

-- In this example, we want to focus on solid colors that are "bright
-- enough."

brightSolidFocus :: Prism' Fill Unit
brightSolidFocus = nearly (Solid referenceColor) predicate
  where
  referenceColor = Color.graytone 0.8
  predicate = case _ of
    Solid color ->
      Color.brightness color >= Color.brightness referenceColor
    _ ->
      false

-- Because a `nearly` prism focuses into `Unit`, you can get only two
-- values from `preview`:

n1 :: Maybe Unit
n1 = preview brightSolidFocus (Solid Color.white)

-- (Just unit)

n2 :: Maybe Unit
n2 = preview brightSolidFocus (Solid Color.black)

-- Nothing

n3 :: Maybe Unit
n3 = preview brightSolidFocus NoFill

--  Nothing

-- ... so you probably want to use `is` or `isn't`:

n4 :: Boolean
n4 = is brightSolidFocus (Solid Color.white) :: Boolean

-- true

-- You can recover the reference value with `review`:

n5 :: Fill
n5 = review brightSolidFocus unit

-- (Solid rgba 204 204 204 1.0)

{------ Eq and Show are always nice ------}

-- ... although Eq is only required for `only`.

derive instance Generic Percent _
derive instance Eq Percent

instance Show Percent where
  show (Percent f) = "(" <> show f <> "%)"

derive instance Generic Point _
derive instance Eq Point

instance Show Point where
  show (Point x y) = "(" <> show x <> ", " <> show y <> ")"

derive instance Generic Fill _
derive instance Eq Fill
instance Show Fill where
  show = genericShow
