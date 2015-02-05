package com.schoolshape.scalacheckplayjson

import org.scalacheck._
import Gen._
import Shrink.shrink
import Arbitrary._
import play.api.libs.json._
import org.scalatest.WordSpec
import org.scalatest.Tag
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import javax.script.{Invocable, ScriptEngineManager}
import java.io.FileReader

/**
 * Generator of Play2 json objects with a given maximum tree depth cutoff
 */
trait JsonGen {

  val defaultMaxPropertyLength = 4
  val defaultMaxStringLength = 8
  val defaultMaxArrayLength = 4
  val defaultMaxProperties = 4
  val defaultMaxDepth = 3
  val defaultPropertyGenerator = alphaChar

  /** generate either a JSONArray or a JSONObject */
  def jsValue(maxDepth: Int): Gen[JsValue] =
    if (maxDepth == 0)
      jsSimpleValue
    else
      frequency(
        1 -> jsArray(maxDepth - 1),
        1 -> jsObject(maxDepth - 1),
        1 -> jsSimpleValue
      )

  def jsSimpleValue: Gen[JsValue]      = oneOf(jsNumber, jsString, jsBoolean)
  def jsBoolean: Gen[JsBoolean]        = arbitrary[Boolean].map{JsBoolean(_)}
  def jsNumber: Gen[JsNumber]          = arbitrary[Int].map{JsNumber(_)}
  def jsString: Gen[JsString]          = jsStringN(defaultMaxStringLength) //alphaStr.map{JsString(_)}
  def jsStringN(n: Int): Gen[JsString] = listOfN(n, alphaNumChar)
    .map{ (charList: List[Char]) => JsString(charList.mkString) }

  def jsArray(maxDepth: Int): Gen[JsArray] = for {
    len  <- choose(0, defaultMaxArrayLength)
    vals <- values(len, maxDepth)
  } yield JsArray(vals)

  def jsArrayN(maxDepth: Int = defaultMaxDepth)(n: Int): Gen[JsArray] = values(n, maxDepth).map(JsArray(_))

  def jsObject(maxDepth: Int): Gen[JsObject] = for {
    size <- choose(0, defaultMaxProperties)
    ks   <- listOfN(size, jsObjectKey).map{ _.toSet.toSeq }
    vals <- values(size, maxDepth)
  } yield JsObject(ks zip vals)

  def jsObjectKey = for {
    len <- choose(1, defaultMaxPropertyLength)
    vals <- listOfN(len, defaultPropertyGenerator)
  } yield vals.mkString

  /** generate a list of keys to be used in the map of a JSONObject */
  def values(n: Int, maxDepth: Int) = listOfN(n, jsValue(maxDepth))
}

trait OpGen extends JsonGen {

  val maxLengthIncrease = 2

  /*
   * Generate random integers which sum to len
   */
  def slicesOf(len: Int): Gen[List[Int]] = for {
    rList <- listOfN(len + 1, choose(0, len))
  } yield {
    val sums  = rList.scanLeft(0)(_ + _) drop 1
    val valid = ((rList zip sums) takeWhile (_._2 <= len)).unzip._1
    if (valid.sum == len) valid else (len - valid.sum) :: valid
  }

  /**
   * Given a list of slice lengths, slices up another list according to those lengths
   * The sum of the second argument == length of the first argument
   */
  def sliceUp[T]: ((List[T], List[Int]) => List[List[T]]) = {
    case (Nil, Nil) => Nil
    case (xs, n :: ns) if xs.length >= n => (xs take n) :: sliceUp(xs drop n, ns)
    case _ => throw new IllegalArgumentException("Illegal slices")
  }


  def update: Gen[Update]    = jsValue(defaultMaxDepth).map(new Update(_))
  def replace: Gen[Replace]  = jsValue(defaultMaxDepth).map(new Replace(_))

  def keep(preLen: Int): Gen[Keep] = const(Keep(preLen))
  def change(preLen: Int, newGen: Int => Gen[JsValue]): Gen[Change] = for {
    postLen  <- choose(if (preLen == 0) 1 else 0, preLen + maxLengthIncrease) // Maximum increase length by 2
    toInsert <- newGen(postLen)
  } yield Change(preLen, toInsert)
  def modify(value: JsValue) = opFor(value).map(Modify(_))

  def stringOp(preLen: Int): Gen[ArrayOp] =
    if (preLen == 0)
      change(preLen, jsStringN)
    else
      oneOf(keep(preLen), change(preLen, jsStringN))

  /**
   * Generate an array operation
   */
  def arrayOp(slice: List[JsValue]): Gen[ArrayOp] = {
    val preLen   = slice.length
    val arrayGen = jsArrayN(defaultMaxDepth) _
    if (preLen == 0)
      change(preLen, arrayGen)
    else if (preLen == 1)
      frequency(
        4 -> modify(slice.head),
        1 -> change(preLen, arrayGen),
        1 -> keep(preLen)
      )
    else
      oneOf(keep(preLen), change(preLen, arrayGen))
  }

  /**
   * Generate a string array splice
   */
  def stringSplice(string: JsString): Gen[ArraySplice] = for {
    slices <- slicesOf(string.value.length)
    list   <- sequence[List, ArrayOp](slices map stringOp)
  } yield Splice(list: _*)


  /**
   * Generate an array splice op
   */
  def arraySplice(array: JsArray): Gen[ArraySplice] = for {
    sliceSizes <- slicesOf(array.value.length)
    slices = sliceUp(array.value.toList, sliceSizes)
    list   <- sequence[List, ArrayOp](slices map arrayOp)
  } yield Splice(list: _*)

  /**
   * Generate an op which acts on a Json object
   */
  def jsonOp(jsObject: JsObject): Gen[JsonOp] =
    for {
      // Take ops on a random selection of the old properties
      oldProps    <- someOf(jsObject.fields)
      oldKeyOps   <- sequence[List, (String, Op)](
        for { (key, value) <- oldProps } yield  opFor(value) map { (key, _) }
      )

      // Take a few new properies and execute set operations on them
      newPropsLen <- choose(0, jsObject.fields.length / 3 + 1)
      newKeys     <- listOfN(newPropsLen, jsObjectKey)
      newKeyOps   <- sequence[List, (String, Op)](
        newKeys.map{ key => setOp.map{ (key, _)} }
      )
    }
    yield JsonOp(oldKeyOps.toMap ++ newKeyOps.toMap)

  /**
   * Generate either update or replace
   */
  def setOp: Gen[Op] =
    for {
      obj <- jsValue(maxDepth = 2)
      (op: Op)  <- oneOf(Update(obj), Replace(obj))
    } yield op

  /**
   * Generate an op which is valid for application to the supplied JsValue
   */
  def opFor(v: JsValue): Gen[Op] = {

    // A generator which is only appropriate for this class of object
    val specificGen = v match {
      case o: JsObject  => jsonOp(o)
      case a: JsArray   => arraySplice(a)
      case s: JsString  => stringSplice(s)
      case _            => setOp
    }

    // A SetOp is appropriate for any object, return one 25% of the time
    // otherwise return the generator specific to this type
    frequency(
      1 -> setOp,
      4 -> specificGen
    )
  }

  def parallelOps: Gen[(JsValue, Op, Op)] =
    for {
      x <- jsValue(maxDepth = 2)
      a <- opFor(x)
      b <- opFor(x)
    }
    yield (x, a, b)

  def sequentialOps: Gen[(JsValue, Op, Op)] =
    for {
      x <- jsValue(maxDepth = 2)
      b <- opFor(x)
      a <- opFor(b * x)
    }
    yield (x, a, b)

  def anyJsonOp: Gen[(JsObject, JsonOp)] = for {
    x <- jsObject(maxDepth = 4)
    a <- opFor(x) suchThat (_.isInstanceOf[JsonOp])
  } yield (x, a.asInstanceOf[JsonOp])
}
