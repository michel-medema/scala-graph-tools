package nl.rug.ds.common

object Helper {
  /**
   * Returns a random element from a collection.
   */
  def selectRandom[T]( S: Iterable[T] ): Option[T] = {
    if ( S.nonEmpty ) {
      S.iterator.drop(util.Random.nextInt(S.size)).nextOption()
    } else {
      None
    }
  }

  /**
   * Returns all the unique pairs of the elements in a collection.
   *
   * For example, if the elements are [1, 2, 3], the result is [(1, 2), (1, 3), (2, 3)].
   */
  def pairs[T](collection: Iterable[T]): Iterator[(T, T)] = {
    collection.iterator.zipWithIndex.flatMap {
      case (elem1, idx1) => collection.iterator.drop( idx1 + 1 ).map( (elem1, _) )
    }
  }

   def combine[A, B]( v: Either[A, B], vs: Either[List[A], List[B]] ): Either[List[A], List[B]] = {
    vs match {
      case Left( ls: List[A] ) =>
        v match {
          case Left(a: A) => Left( ls.appended(a) )
          case _ => Left(ls)
        }
      case Right( ls: List[B] ) =>
        v match {
          case Left(a: A) => Left( List(a) )
          case Right( b: B ) => Right( ls.appended(b) )
        }
    }
  }

   def sequence[A, B](l: List[Either[A, B]]): Either[List[A], List[B]] = {
    l.foldRight( Right(List.empty): Either[List[A], List[B]] )( Helper.combine )
  }
}
