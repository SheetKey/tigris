{-# LANGUAGE NamedFieldPuns #-}

module Tigris.Inventory.Types where


-- vector
import Data.Vector as V


type ItemId = Integer

type ItemWeight = Integer

type MaxItems = Integer

data ItemSize = S | M | L

data Item = Gun ItemSize ItemId ItemWeight 
          | Coin ItemId ItemWeight Integer

data ContainerType = SHolster
                   | MHolster
                   | LHolster
                   | Pouch
                   | MoneyPouch

data Container = Container
  { _id :: ItemId
  , containerType :: ContainerType
  , maxItems :: MaxItems
  , itemWeight :: ItemWeight
  , item :: (Maybe Item)
  }

type Inventory = V.Vector Container

newEmptyContainer :: ItemId -> ContainerType -> MaxItems -> ItemWeight -> Container
newEmptyContainer x y z j = Container x y z j Nothing

fillContainer :: Item -> Container -> Container
fillContainer cn@(Coin _ _ x) c@(Container {containerType = MoneyPouch, maxItems}) =
  if x <= maxItems then c { item = Just cn } else c
fillContainer _ c@(Container _ _ _ _ (Just _)) = c
fillContainer g@(Gun S _ _) c@(Container {containerType = SHolster}) = c { item = Just g }
fillContainer g@(Gun M _ _) c@(Container {containerType = MHolster}) = c { item = Just g }
fillContainer g@(Gun L _ _) c@(Container {containerType = LHolster}) = c { item = Just g }
fillContainer _ c = c

emptyContainer :: Container -> (Container, Maybe Item)
emptyContainer c@(Container { item }) = (c { item = Nothing }, item)

canContainerHold :: Item -> Container -> Bool
canContainerHold (Gun S _ _) (Container { containerType = SHolster, item = Nothing }) = True 
canContainerHold (Gun M _ _) (Container { containerType = MHolster, item = Nothing }) = True
canContainerHold (Gun L _ _) (Container { containerType = LHolster, item = Nothing }) = True
canContainerHold (Coin _ _ x) (Container { containerType = MoneyPouch, maxItems, item = Nothing }) =
  x <= maxItems
canContainerHold (Coin _ _ x) (Container { containerType = MoneyPouch, maxItems, item = Just (Coin _ _ y) }) =
  x + y <= maxItems
canContainerHold _ _ = False
  

itemType :: Item -> ContainerType
itemType (Gun S _ _) = SHolster
itemType (Gun M _ _) = MHolster
itemType (Gun L _ _) = LHolster
itemType (Coin _ _ _) = MoneyPouch

fillInventory :: Item -> Inventory -> (Inventory, Maybe Item)
fillInventory i inv =
  let
    ind = V.findIndices (canContainerHold i) inv
  in case ind V.!? 0 of
       Nothing -> (inv, Just i)
       Just x  -> (inv V.// [(x, fillContainer i (inv V.! x))], Nothing)
