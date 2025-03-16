package task5

import org.junit.*
import org.junit.Assert.*
import Optionals.*

class OptionalTest:
  @Test def emptyOptionalShouldBeEmpty(): Unit =
    val empty = Optional.Empty()
    assertTrue(Optional.isEmpty(empty))

  @Test def nonEmptyOptionalShouldNotBeEmpty(): Unit =
    val nonEmpty = Optional.Maybe(0)
    assertFalse(Optional.isEmpty(nonEmpty))

  @Test def orElseShouldReturnDefaultWhenEmpty(): Unit =
    val nonEmpty = Optional.Maybe(0)
    assertEquals(0, Optional.orElse(nonEmpty, 1))

  @Test def orElseShouldReturnValueWhenNonEmpty(): Unit =
    val empty = Optional.Empty()
    assertEquals(1, Optional.orElse(empty, 1))

  /** Task 5 -- Look the behaviour of map operator */
  @Test def mapShouldReturnEmptyWhenEmpty(): Unit =
    val empty: Optional[Int] = Optional.Empty()
    val result = Optional.map(empty, _ + 1)
    assertTrue(Optional.isEmpty(result))

  @Test def mapShouldReturnTransformedValueWhenNonEmpty(): Unit =
    val nonEmpty = Optional.Maybe(0)
    val result = Optional.map(nonEmpty, _ + 1)
    assertEquals(1, Optional.orElse(result, 1))

  @Test def filterEmpty(): Unit =
    val empty: Optional[Int] = Optional.Empty()
    assertEquals(Optional.Empty(), Optional.filter(empty, _ > 2))

  @Test def filterNotEmpty(): Unit =
    val maybe: Optional[Int] = Optional.Maybe(5)
    assertEquals(Optional.Maybe(5), Optional.filter(maybe, _ > 2))

  @Test def filterWithMapNotEmpty(): Unit =
    val maybe: Optional[Int] = Optional.Maybe(5)
    assertEquals(Optional.Maybe(6), Optional.map(Optional.filter(maybe, _ > 2), _ + 1))

  @Test def filterWithMapEmpty(): Unit =
    val maybe: Optional[Int] = Optional.Maybe(5)
    assertEquals(Optional.Empty(), Optional.map(Optional.filter(maybe, _ > 6), _ + 1))

