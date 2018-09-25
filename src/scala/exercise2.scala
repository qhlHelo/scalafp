import scala.annotation.tailrec
import scala.math._

/**
  * C2-函数式编程
  * c2.1 高阶函数
  * c2.2 多态函数
  *
  */
object exercise2 {
	def main(args: Array[String]): Unit = {
		// 高阶函数-HOF练习
		println("Fib example: " + fib(6))
		println(formatResult(-10, "absolute value", abs))
		println(formatResult(7, "fib value", fib))
		// 多态函数-类型抽象
		println("多态: " +isSorted(Array(1, 2, 3, 4, 5), (x: Int, y: Int) => x > y))
	}
	
	// c2.1 高阶函数-HOF练习
	/**
	  * 获取斐波那契数列的第n个数字
	  * 备注：斐波那契数列：0,1,1,2,3,5,8,13,21……
	  *
	  * @param n
	  * @return
	  */
	def fib(n: Int): Int = {
		@tailrec
		def loop(n: Int, acc1: Int, acc2: Int): Int = {
			if (n == 0) acc1
			else loop(n - 1, acc2, acc1 + acc2)
		}
		
		loop(n, 0, 1)
	}
	
	def formatAbs(n: Int) = {
		val msg = "Absolute value of %d is %d."
		msg.format(n, abs(n))
	}
	
	def formatFib(n: Int) = {
		val msg = "Factorial value of %d is %d."
		msg.format(n, fib(n))
	}
	
	/**
	  * formatResult是一个高阶函数（HOF），它接收一个函数f为参数，f接收Int并返回Int
	  *
	  * @param n
	  * @param name
	  * @param f
	  * @return
	  */
	def formatResult(n: Int, name: String, f: Int => Int) = {
		val msg = "Ths %s of %d is %d."
		msg.format(name, n, f(n))
	}
	
	// c2.2 多态函数-类型抽象
	/**
	  * 接收一个函数类型，对数组进行判断，符合则返回数组的索引
	  *
	  * @param arr
	  * @param p
	  * @tparam A
	  * @return
	  */
	def findFirst[A](arr: Array[A], p: A => Boolean) = {
		@tailrec
		def loop(n: Int): Int = {
			if (n >= arr.length) -1
			else if (p(arr(n))) n
			else loop(n + 1)
		}
		
		loop(0)
	}
	
	/**
	  * 监测Array[A]是否按照给定的比较函数排序
	  *
	  * @param arr
	  * @param ordered
	  * @tparam A
	  * @return
	  */
	def isSorted[A](arr: Array[A], ordered: (A, A) => Boolean): Boolean = {
		@tailrec
		def loop(n: Int): Boolean = {
			if (n >= arr.length) true
			// 如果按照给定的函数比较，则n+1，继续遍历
			else if (ordered(arr(n), arr(n + 1))) loop(n + 1)
			else false
		}
		
		loop(0)
	}
}
