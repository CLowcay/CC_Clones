{-
CC_Clones - Classic games reimplemented
© Callum Lowcay 2006-2011

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Common.Queue (
	Queue, empty, null, length, enqueue, enqueueMany, dequeue, head
) where

import Data.Sequence ((|>), (><), ViewL((:<)))
import Prelude hiding (null, length, head)
import qualified Data.Sequence as Seq

newtype Queue a = Queue {innerSeq :: Seq.Seq a}
	deriving (Eq, Ord, Read, Show)

empty :: Queue a
empty = Queue (Seq.empty)

null :: Queue a -> Bool
null q = Seq.null (innerSeq q)

length :: Queue a -> Int
length q = Seq.length (innerSeq q)

enqueue :: Queue a -> a -> Queue a
enqueue q x = Queue ((innerSeq q) |> x)

enqueueMany :: Queue a -> [a] -> Queue a
enqueueMany q xs = Queue ((Seq.fromList xs) >< (innerSeq q))

dequeue :: Queue a -> (a, Queue a)
dequeue q =
	case Seq.viewl (innerSeq q) of
		Seq.EmptyL -> error "attempt to dequeue an empty queue"
		x :< xs -> (x, Queue xs)

head :: Queue a -> a
head q =
	case Seq.viewl (innerSeq q) of
		Seq.EmptyL -> error "attempt to take the head of an empty queue"
		x :< _ -> (x)

