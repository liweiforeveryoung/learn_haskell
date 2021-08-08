# 2021/6/19

##### 安装 ghci

在 ghci 环境下，按 tab ，ghci 会帮你联想。如果函数名字很长，只需要输入函数的前几个字符，然后按 tab 键，就可以把函数的完整名字联想出来了。

##### 基本运算

###### 加减乘除

```
ghci> 2 + 15
17
ghci> 49 * 100
4900
ghci> 1892 - 1472
420
ghci> 5 / 2
2.5
```

值得一提的是 5 / 2 居然可以得到一个小数。

处理负数时，必须将负数放在括号内

```
ghci> 50*(50-100)
-2500
ghci> 50*-50

<interactive>:6:3: error:
    ? Variable not in scope: (*-) :: t0 -> t1 -> t
    ? Perhaps you meant one of these:
        ‘*’ (imported from Prelude), ‘-’ (imported from Prelude),
        ‘*>’ (imported from Prelude)
ghci> 50*(-50)
-2500
```

###### 取余 mod

不能用 % 进行 mod

必须用专门的 mod 函数，且需要用 ` 包起来

```
ghci> 3 % 2

<interactive>:14:3: error:
    Variable not in scope: (%) :: t0 -> t1 -> t
ghci> 3 mod 2

<interactive>:15:1: error:
    ? Non type-variable argument
        in the constraint: Num ((a -> a -> a) -> t1 -> t2)
      (Use FlexibleContexts to permit this)
    ? When checking the inferred type
        it :: forall {a} {t1} {t2}.
              (Integral a, Num t1, Num ((a -> a -> a) -> t1 -> t2)) =>
              t2
ghci> 3 `mod` 2
1
```

###### bool 运算

True 和 False 的首字母必须大写。true 和 false 都是不合法的

```
ghci> True && False
False
ghci> True && True
True
ghci> False || True
True
ghci> not False		// not 一定得到小写的，一个字母都不能错
True
```

在 haskell 里，是没有 ! 的，只有 not

```
ghci> 5 == 5
True
ghci> 5 /= 5
False
ghci> 5 != 5

<interactive>:19:3: error:
    ? Variable not in scope: (!=) :: t0 -> t1 -> t
    ? Perhaps you meant one of these:
        ‘>=’ (imported from Prelude), ‘==’ (imported from Prelude),
        ‘/=’ (imported from Prelude)
```

haskell 里面的字符串是需要打双引号的

haskell 里面必须要求比较类型是相同的

```
ghci> "hello"
"hello"
ghci> 5 == "hello"

<interactive>:40:1: error:
    ? No instance for (Num String) arising from the literal ‘5’
    ? In the first argument of ‘(==)’, namely ‘5’
      In the expression: 5 == "hello"
      In an equation for ‘it’: it = 5 == "hello"
```

```
ghci> 5 + 4.0
9.0
```

5 + 4.0 是可行的，因为 5 既可以做被看做整数也可以被看做浮点数，但 4.0 则不能被看做整数。

##### 注释

```haskell
-- hello world
```

##### 函数

###### 中缀函数和前缀函数

中缀函数就是函数两边都有参数的函数，形式为：

```
arg1 func arg2
```

`*` 实际上就是一个中缀函数，例如`2 * 3`

前缀函数就是左边没有参数的函数，形式为

```
func arg1 [arg2] ...
```

###### 函數调用

在 Haskell 中，函数调用的形式是函数名，空格，空格分隔的参数表。

其他的编程语言中函数调用一般都会用括号把函数的参数列表都括起来，haskell 中没有括号。

```
ghci> min 9 10
9
```

###### 优先级

函数调用的优先级是高于加减乘除的优先级的

```
ghci> succ 9 * 10
100
ghci> (succ 9) * 10
100
ghci> succ(9*10)
91
```

###### 函数签名

```
函数名 [arg1] [arg2]... = 函数行为
```

###### 编写自己的函数

```haskell
-- baby.hs
doubleUs x y = x*2 + y*2
```

```
ghci> :l baby.hs
[1 of 1] Compiling Main             ( baby.hs, interpreted )
Ok, one module loaded.
ghci> :l baby.hs
[1 of 1] Compiling Main             ( baby.hs, interpreted )
Ok, one module loaded.
ghci> doubleUs 1 2
6
```

函数调用函数

Haskell 中的函数并没有顺序，所以先声明 `doubleUs` 还是先声明 `doubleMe` 都是同样的。

```haskell
-- baby.hs
doubleUs x y = doubleMe x + doubleMe y
doubleMe x = x * 2
```

```
ghci> :l baby.hs
[1 of 1] Compiling Main             ( baby.hs, interpreted )
Ok, one module loaded.
ghci> doubleUs 1 2
6
ghci> doubleMe 3
6
```

###### 表达式

表达式就是返回一个值的一段代码：5 是个表达式，它返回 5；`4+8` 是个表达式；`x+y` 也是个表达式，它返回 `x+y` 的结果。

###### if ... then ... else ...

```haskell
doubleSmailNumber x = if x < 100 then x else x*2
```

```
ghci> doubleSmailNumber 99
99
ghci> doubleSmailNumber 100
200
```

拐弯的

```haskell
doubleSmailNumber x = if x < 100 
    then x
    else x*2
```

`if`语句是一个表达式，它必须返回一个值，因此 `else` 是不可忽略的

###### 没有参数的函数

```haskell
hello = "hello world"
```

```
ghci> :l baby.hs 
[1 of 1] Compiling Main             ( baby.hs, interpreted )
Ok, one module loaded.
ghci> hello 
"hello world"
```

nice，输出了梦寐以求的 hello world

而且，为啥我的函数能不能为 main？

```
ghci> :l baby.hs 
[1 of 1] Compiling Main             ( baby.hs, interpreted )

baby.hs:4:1: error:
    ? Couldn't match type: [Char]
                     with: IO t0
      Expected: IO t0
        Actual: String
    ? In the expression: main
      When checking the type of the IO action ‘main’
  |
4 | main = "hello world"
  | ^
Failed, no modules loaded.
```

没有参数的函数通常被称作“定义”(或者“名字”)。

###### 函数的名字

函数的名字中可以带有`'`，它一个函数名的合法字符。通常，我们使用单引号来区分一个稍经修改但差别不大的函数。定义这样的函数也是可以的。

首字母大写的函数是不允许的。

##### 变量

###### 声明

```
variableName = variableValue
```

```
ghci> c = "cry"
ghci> c
"cry"
ghci> c = "happy"
ghci> c
"happy"
ghci> c = 1
ghci> c
1
```

如上，变量的值是可以变化的，甚至可能赋给它不同类型的值。（很像动态语言呢）

也可以用 let 来声明一个变量

```
ghci> let d = "down"
ghci> d
"down"
ghci> let d = "up"
ghci> d
"up"
ghci> d = "down"
ghci> d
"down"
ghci> d = 1
ghci> d
1
```

###### 理解

**<u>变量的声明方式和函数的声明方式是相同的，所以说，haskell 里面没有变量，只有函数！</u>**

haskell 里面没有变量

```
ghci> a = 1
ghci> a = 2
```

可以理解为将新值 2 赋给了 a 变量

也可以理解为原来的 a 消失了，产生了一个新的 a，新的 a 的值 2。（突然想起了《知致命魔术》）

##### List

在 Haskell 中，List 是一种单型别的数据结构，可以用来存储多个型别相同的元素。我们可以在里面装一组数字或者一组字符，但不能把字符和数字装在一起。

###### 声明方式

`[]` + `,`

```
ghci> lostNumbers = [4,8,15,16,23,48]  
ghci> lostNumbers 
[4,8,15,16,23,48]
```

###### 重新理解字符串 “hello”

字符串实际上就是一组字符的 List，"Hello" 只是 `['h','e','l','l','o']` 的语法糖而已。

```
ghci> h = ['h','e','l','l','o']
ghci> h
"hello"
```

###### 合并 List  `++`

```
ghci> [1,2]++[3,4]
[1,2,3,4]
ghci> ['h','e']++['l','l','o']
"hello"
ghci> ['h','e']++[1,2]        

<interactive>:83:13: error:
    ? No instance for (Num Char) arising from the literal ‘1’
    ? In the expression: 1
      In the second argument of ‘(++)’, namely ‘[1, 2]’
      In the expression: ['h', 'e'] ++ [1, 2]
```

###### 在 List 前面添加元素 `:`

```
ghci> 1:[2,3]
[1,2,3]
ghci> [2,3]:1

<interactive>:85:1: error:
    ? Non type-variable argument in the constraint: Num [[a]]
      (Use FlexibleContexts to permit this)
    ? When checking the inferred type
        it :: forall {a}. (Num a, Num [[a]]) => [[a]]
```

###### 重新理解 [1,2,3]

其实 [1,2,3] 也是 1:2:3:[] 的语法糖

```
ghci> [1,2,3]
[1,2,3]
ghci> 1:2:3:[]
[1,2,3]
```

这里其实是递归了的

```
1: (2:3:[])
1: (2: (3:[]))
1: (2: [3])
1: [2,3]
[1:2:3]
```

###### 按照索引从 List 中取元素 `!!`

```
ghci> "hello" !! 0  
'h'
ghci> "hello" !! 5
*** Exception: Prelude.!!: index too large
```

程序员的传统艺能了，索引从 0 开始算起

###### List 里面套 List

```haskell
ghci> ["he","llo"]
["he","llo"]
ghci> [[1,2],[3,4,5]]
[[1,2],[3,4,5]]
ghci> ["he",[1,2]]

<interactive>:98:8: error:
    ? No instance for (Num Char) arising from the literal ‘1’
    ? In the expression: 1
      In the expression: [1, 2]
      In the expression: ["he", [1, 2]]
```

列表里面的元素类型必须是相同的，长度可以不同。

###### 比较 List 的大小

当 List 内装有可比较的元素时，List 是可以比较大小的，> , < , >= , <= , ==  在他们身上是可以起作用的，它会先比较第一个元素，若它们的值相等，则比较下一个，以此类推。

```
ghci> [3,2] > [3,1]  
True  
ghci> [3,2,1] > [3,2]
True  
ghci> [3,1] == [3,1]  
True 
ghci> [3,1] >= [3,1]
True
ghci> [3,2] < [3,1]  
False
```

###### head / tail

head 返回一个 List 的头部，也就是 List 的首个元素。

tail 返回一个 List 的尾部，也就是 List 除去头部之后的部分。

```
ghci> head [5,4,3,2,1] 
5
ghci> tail [5,4,3,2,1] 
[4,3,2,1]
```

###### last / init

last 返回一个 List 的最后一个元素。

init 返回一个 List 除去最后一个元素的部分。

```
ghci> last [5,4,3,2,1] 
1
ghci> init [5,4,3,2,1] 
[5,4,3,2]
```

###### length

返回一个 List 的长度。

```
ghci> length [5,4,3,2,1]  
5
```

###### null

null 检查一个 List 是否为空。如果是，则返回 `True`，否则返回 `False`。应当避免使用 `xs==[]` 之类的语句来判断 List 是否为空，使用 null 会更好。

```
ghci> null [1,2,3]  
False  
ghci> null []  
True 
```

###### reverse 

将一个 List 反转

```
ghci> reverse [5,4,3,2,1]  
[1,2,3,4,5] 
```

###### take

返回一个 List 的前几个元素

```
ghci> take 3 [5,4,3,2,1]  
[5,4,3]  
ghci> take 1 [3,9,3]  
[3]  
ghci> take 5 [1,2]  
[1,2]  
ghci> take 0 [6,6,6] 
[] 
```

###### maximum / minimun

返回一个 List 中最大/最小 的那个元素

```
ghci> maximum [5,1,4]
5
ghci> minimum [5,1,4]
1
```

###### sum / product

返回 List 中所有元素的和/积

```haskell
ghci> sum [5,1,4]
10
ghci> product [5,1,4]
20
```

###### elem

中缀函数，判断一个元素是否在包含于一个 List

```haskell
ghci> 1 `elem` [1,2,3]
True
ghci> 4 `elem` [1,2,3]
False
```

elem 需要用 ` 包起来 

##### range

Range 是构造 List 方法之一，而其中的值必须是可枚举的，像 1、2、3、4...字符同样也可以枚举，字母表就是 `A..Z` 所有字符的枚举。而名字就不可以枚举了，`"john"` 后面是谁？我不知道。

###### [begin...end]

两边都是闭区间

```haskell
ghci> [1..20]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
ghci> ['a'..'z']
"abcdefghijklmnopqrstuvwxyz"
```

###### [begin,begin+step...end]

step 用来指定步长

```
ghci> [1,3..20]
[1,3,5,7,9,11,13,15,17,19]
// 步长为 3 - 1 = 2
```

```
ghci> [2,1..(-20)]
[2,1,0,-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20]
// 步长为 -1
```

```
ghci> [2,1..30]   
[]
```

###### [begin..] [begin,begin+step..]

生成无限的 List，用 ctrl + c 中断

```
ghci> [1..]
10652,10653,10654,10655,10656,10657,10658,10659,10660,10661,10662,10663,10664,10665,10666,10667,10668,10669,10670,10671,10672,10673,10674,10675,10676,10677,10678,10679,10680,10681,10682,10683,10684,10685,10686,10687,10688,10689,10690,10691,10692,10693,10694,10695,10696,10697,10698,10699,10700,10701,10702,10703,10704,10705,10706,10707,10708,10709,10710,10711,10712,10713,10714,10715,10716,10717,10718,Interrupted.
ghci> [1,3..]
9863,9865,9867,9869,9871,9873,9875,9877,9879,9881,9883,9885,9887,9889,9891,9893,9895,9897,9899,9901,9903,9905,9907,9909,9911,9913,9915,9917,9919,9921,9923,9925,9927,9929,9931,9933,9935,9937,9939,9941,9943,9945,9947,994Interrupted.
```

##### List Comprehension

学过数学的你对集合的 comprehension (Set Comprehension) 概念一定不会陌生。通过它，可以从既有的集合中按照规则产生一个新集合。前十个偶数的 set comprehension 可以表示为
$$
S = \{2*x \mid x \in N,x \leq 10 \}
$$
 list comprehension，它与 set comprehension 十分的相似，用它取前十个偶数轻而易举。这个 list comprehension 可以表示为：

```
ghci> [x*2 | x <- [1..10]]
[2,4,6,8,10,12,14,16,18,20]
ghci> [x*2|x<-[1..10]]    
[2,4,6,8,10,12,14,16,18,20]
```

###### 过滤 (filtering)

从一个 List 中筛选出符合特定限制条件的操作也可以称为过滤 (filtering)

给这个 comprehension 再添个限制条件 (predicate)

```
ghci> [x*2 | x <- [1..10], x*2 >= 12]
[12,14,16,18,20]
```

###### 从多个 List 中取元素

```
ghci> [x*y|x<-[1,2],y<-[3,4]]
[3,4,6,8]
```

###### Length'

```
ghci> length' array = sum [1 | _ <- array]
ghci> length' [1..10]
10
```

###### `_`

`_` 表示我们并不关心从 List 中取什么值，与其弄个永远不用的变量，不如直接一个 `_`。

###### 嵌套的 List comprehension

```
ghci> let ll = [[1..4],[5..8]]
ghci> ll
[[1,2,3,4],[5,6,7,8]]
ghci> oddll ll = [[elem | elem <- li,odd elem]|li <- ll]
ghci> oddll ll
[[1,3],[5,7]]
```

```
ghci> oddll ll = [elem | li <- ll,elem <- li,odd elem]
ghci> oddll ll
[1,3,5,7]
```

##### Tuple

用括号 (）括起来，List 是用 [] 括起来

 ```
(1,2)
 ```

###### Tuple && List

他两的关注点不同

List 只关注其中元素的类型

```
l1 = [1,2]
l2 = [1,2,3]
```

虽然 li 和 l2 里面的元素个数不相同，但是 l1 和 l2 的类型是相同的，因为他两的元素类型都是数字。

tuple 则关注 todo 

可以有单元素的 List ，但是不能有单元素的 tuple

###### pair 序对

序对特指只有两个元素的 tuple

###### fst / snd

返回 tuple 序对的首项 / 尾项

```
ghci> fst (1,2)
1
ghci> snd (1,2)
2
ghci> fst (1,2,3)

<interactive>:74:5: error:
    ? Couldn't match expected type: (a, b0)
                  with actual type: (a0, b1, c0)
    ? In the first argument of ‘fst’, namely ‘(1, 2, 3)’
      In the expression: fst (1, 2, 3)
      In an equation for ‘it’: it = fst (1, 2, 3)
    ? Relevant bindings include it :: a (bound at <interactive>:74:1)
ghci> snd (1,2,3)

<interactive>:75:5: error:
    ? Couldn't match expected type: (a0, b)
                  with actual type: (a1, b0, c0)
    ? In the first argument of ‘snd’, namely ‘(1, 2, 3)’
      In the expression: snd (1, 2, 3)
      In an equation for ‘it’: it = snd (1, 2, 3)
    ? Relevant bindings include it :: b (bound at <interactive>:75:1)
```

###### zip

它可以用来生成一组序对 (Pair) 的 List。它取两个 List，然后将它们交叉配对，形成一组序对的 List

```
ghci> zip [1,2] [3,4]
[(1,3),(2,4)]
ghci> zip [1,2] [3,4,5]
[(1,3),(2,4)]
ghci> zip [1,2,3] [3,4]
[(1,3),(2,4)]
```

# 2021 6 20

### type

#### :t 获取类型

```
ghci> :t 1
1 :: Num p => p
ghci> :t "hello"
"hello" :: String
ghci> :t ['h','e']
['h','e'] :: [Char]
ghci> ['h','e']
```

返回的格式为

```
对象 :: 对象的类型
```

凡是明确的类型，首字母都会大写

[Char] 和 String 是等价的

#### Haskell 里面的 type

+ Int 整数 ，Int 是有界的，也就是说它由上限和下限。对 32 位的机器而言，上限一般是 `2147483647`，下限是 `-2147483648`。
+ Integer 也是整数，但是是无界的
+ Float 表示单精度的浮点数
+ Double 表示双精度的浮点数
+ Bool 表示 bool 值，它只有两种值，True 和 False
+ Char 表示一个字符
+ Tuple 的型别取决于它的长度及其中项的型别。注意，空 Tuple 同样也是个型别，它只有一种值：`()`。

#### Type variables

```
ghci> :t head
head :: [a] -> a
```

a 是个型别变量，意味着 a 可以是任意的型别。**凡是明确的类型，首字母都会大写**

`head` 函数的型别声明里标明了它可以取任意型别的 List 并回传其中的第一个元素。

在命名上，型别变量使用多个字符是合法的，不过约定俗成，通常都是使用单个字符，如 `a`, `b` ,`c` ,`d`...

```
ghci> :t fst
fst :: (a, b) -> a
```

pair 里面的元素类型可以不相同

#### Type Classes

如果某一个 Type 属于某 Type Classs，那它一定实现了该 TypeClass 所描述的行为。在 Go 里面可以认为是 interface

如果 Type1 实现了 TypeClass1 所描述的行为，则 Type1 属于 TypeClass1。

##### 几个基本的 type class

###### Eq

这一 Typeclass 提供了判断相等性的接口，凡是可比较相等性的型别必属于 `Eq` class。

###### Ord

可比较大小的型别。除了函数以外，我们目前所谈到的所有型别都属于 `Ord` 类。`Ord` 包中包含了`<, >, <=, >=` 之类用于比较大小的函数。

如果一个 type 实现了 Ord，则它必然实现了 Eq

###### Show

type 的值可以用字符串表示

###### Read

type 的值可以由字符串转换而来

###### Enum

type 的值是可连续的

###### Bounded

type 的值是有上限和下限的

###### Num

type 的值是一个数字，包含所有的数字：实数和整数

###### Integral

整数，Int 和 Integer 属于此 TypeClass

###### Floating

浮点数，Float 和 Double 属于此 TypeClass

##### => 型别约束

看一下函数的类型

```
ghci> :t (==)
(==) :: Eq a => a -> a -> Bool
```

=> 左边的部分叫做型别约束。

```go
// 用 go 来描述
func equal(Eq a) func(Eq a) bool{
	
}
```

== 函数接收一个类型为 a 的参数，a 必须实现了 Eq 接口。返回一个函数 b 。函数 b 的参数也是 a 类型，并返回一个 Bool 值

###### 多个型别约束

只要将多个型别约束放到括号里用逗号隔开即可

```
ghci> :t fromIntegral
fromIntegral :: (Integral a, Num b) => a -> b
```

```
ghci> fromIntegral 100 ::Float
100.0
```



##### :: 型别注释

用来指定类型

```
ghci> :t read
read :: Read a => String -> a
ghci> read "4"
*** Exception: Prelude.read: no parse
ghci> read "4" :: Int
4
ghci> read "4" :: Float
4.0
ghci> read "[1,2,3,4]" :: [Int]
[1,2,3,4]
```

### Pattern matching

模式匹配通过检查数据的特定结构来检查其是否匹配，并按模式从中取得数据。

```haskell
hello7 :: (Integral a) => a -> String
hello7 7 = "hello"
hello7 x = "world"
```

```
ghci> hello7 7
"hello"
ghci> hello7 8
"world"
ghci> hello7 9
"world"
```

```haskell
printNum :: (Integral a) => a -> String
printNum 1 = "one"
printNum 2 = "two"
printNum balabala = "other nums"
```

可以省掉一颗  if-else 树。

###### 递归

```haskell
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

```
ghci> factorial 3
6
ghci> factorial 4
24
```

###### 留一个万能匹配的模式

```haskell
for12 :: (Integral a) => a -> String
for12 1 = "one"
for12 2 = "two"
```

```
ghci> for12 1
"one"
ghci> for12 2
"two"
ghci> for12 3
"*** Exception: baby.hs:(2,1)-(3,15): Non-exhaustive patterns in function for12
```

###### 常用的 pattern: x:xs

对 List 本身也可以使用模式匹配。你可以用 `[]` 或 `:` 来匹配它。因为 `[1,2,3]` 本质就是 `1:2:3:[]` 的语法糖。你也可以使用前一种形式，像 `x:xs` 这样的模式可以将 List 的头部绑定为 `x`，尾部绑定为 `xs`。如果这 List 只有一个元素，那么 `xs` 就是一个空 List。

这模式的应用非常广泛，尤其是递归函数。不过它只能匹配长度大于等于 1 的 List。

也有类似 `x:y:z:xs` 这样的形式。它只能匹配长度大于等于 3 的 List。

###### 利用 Pattern matching 实现 head

```haskell
head' :: [a] -> a
head' [] = error "empty list"
head' (x:xs) = x
```

注意 (x:xs) 得用 () 括起来

###### 匹配多个

```haskell
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' times e = e:replicate (times-1) e
```

###### as 模式 @

获取字符串的首字母

```haskell
firstChar :: String -> String
firstChar [] = []
firstChar (x:xs) = "first char of " ++ x:xs ++ " is " ++ show x
```

```
ghci> firstChar "hello"
"first char of hello is 'h'"
```

用 x:xs 来指代原始的字符串。

`as` 模式 : 就是将一个名字和 `@` 置于模式前，可以在按模式分割什么东西时仍保留对其整体的引用。

想一想 sql 里面的 as

```sql
select count(*) as all from tb_students;
```

所以 as 实际上就是为某个表达式取别名的意思

用上 as 模式之后，代码就变这样了：

```haskell
firstChar :: String -> String
firstChar [] = []
firstChar all@(x:xs) = "first char of " ++ all ++ " is " ++ show x
```

##### Guards

Guard 用来检查一个值的某项属性是否为真。其实和 if ... then ... else 的效果差不多

只不过 Guard 看起来更美观。

###### 例子：根据分数做出评价

```haskell
judge :: Int -> String
judge score
    | score < 60 = "not pass"
    | score <= 90 = "pass"
    | otherwise = "excellent"
```

guard 由跟在函数名及参数后面的竖线标志，通常他们都是靠右一个缩进排成一列。一个 guard 就是一个布尔表达式，如果为真，就使用其对应的函数体。如果为假，就送去见下一个 guard ...

```haskell
judge :: Int -> String
judge score
    | score <= 90 = "pass"
    | score < 60 = "not pass"
    | otherwise = "excellent"
```

交换顺序后可能就不对了

```
ghci> judge 50
"pass"
```

因为通过了第一个 guard，直接返回了，没有到第二个 guard 那儿。

###### 多个参数时使用 guard

```haskell
judge :: Int -> Int -> String
judge mathScore englishScore
    | mathScore + englishScore < 120 = "not pass"
    | mathScore + englishScore < 180 = "pass"
    | otherwise = "excellent"
```

```
ghci> judge 70 10
"not pass"
```

##### Where

用一个变量来代替表达式

```haskell
judge :: Int -> Int -> String
judge mathScore englishScore
    | sum < 120 = "not pass"
    | sum < 180 = "pass"
    | otherwise = "excellent"
    where sum = mathScore + englishScore
```

用一个变量来代替分数

```haskell
judge :: Int -> Int -> String
judge mathScore englishScore
    | sum < passScore = "not pass"
    | sum < excellentScore = "pass"
    | otherwise = "excellent"
    where 
        sum = mathScore + englishScore
        passScore = 120
        excellentScore = 180
```

> 注意：这几行代码的缩进必须是相同的
>
> ```
>         sum = mathScore + englishScore
>         passScore = 120
>         excellentScore = 180
> ```

###### Where 搭配 Pattern matching

```haskell
judge :: Int -> Int -> String
judge mathScore englishScore
    | sum < passScore = "not pass"
    | sum < excellentScore = "pass"
    | otherwise = "excellent"
    where 
        sum = mathScore + englishScore
        (passScore,excellentScore) = (120,180)
```

```haskell
-- 返回两个字符串的首字母
initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname  
```

###### 用 Where 命名函数

```haskell
sums :: [(Int, Int)] -> [Int]
sums couples = [sum' left right | (left,right) <- couples]
    where sum' arg1 arg2 = arg1 + arg2
```

这里用 where 命名了 sum' 函数

```
ghci> sums [(1,2),(3,4)]
[3,7]
```

##### Let

`let` 绑定与 `where` 绑定很相似。

`where` 绑定是在函数底部定义名字，对包括所有 guard 在内的整个函数可见。

`let` 绑定则是个表达式，允许你在任何位置定义局部变量，而对不同的 guard 不可见。

###### let [bindings] in [expressions]

`let` 的格式为 `let [bindings] in [expressions]`。在 `let` 中绑定的名字仅对 `in` 部分可见。`let` 里面定义的名字也得对齐到一列。

依据半径和高度求圆柱体表面积：

```haskell
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  
```

###### Let 配合 List Comprehension 

```haskell
sums :: [(Int, Int)] -> [Int]
sums couples = [sum' | (left,right) <- couples,let sum' = left + right]
```

此时`let` 中绑定的名字在输出函数及限制条件中都可见。

##### case

```
case expression of pattern -> result  
                   pattern -> result  
                   pattern -> result  
                   ...  
```

Pattern matching 其实是 case 的语法糖

```haskell
head' :: [a] -> a  
head' [] = error "No head for empty lists!"  
head' (x:_) = x  
```

```haskell
head' :: [a] -> a  
head' xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x  
```

函数参数的模式匹配只能在定义函数时使用，而 `case` 表达式可以用在任何地方。例如：

```haskell
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."  
```

### 递归

一个结合 Pattern matching 和 Guards 的例子

```haskell
take' :: Int -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs
```

### high order function 高阶函数

##### Curried functions 柯里化

```
ghci> max 4 5
5
```

上面的过程其实可以理解成分为两步进行

第一步：调用 `max 4` 得到一个新函数 `maxWith4`

第二步：调用 `maxWith4 5`得到结果

```
ghci> max 4 5
5
ghci> maxWith4 = max 4
ghci> maxWith4 5
5
ghci> maxWith4 3
4
```

> 实际上类似于 c++ 里面的 std::bind

再看看 max 的 type

```
ghci> :t max
max :: Ord a => a -> a -> a
```

```
a -> a -> a
```

它接收一个 a ，返回一个 a -> a

##### 高阶函数

何为高阶函数：函数可以作为参数，也可以作为返回值

例如：

```haskell
applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)  
```

```
ghci> applyTwice succ 8
10
```

##### 常用的高阶函数

###### map

**map** 取一个函数和 List 做参数，遍历该 List 的每个元素来调用该函数产生一个新的 List。 

```haskell
map :: (a -> b) -> [a] -> [b]  
map _ [] = []  
map f (x:xs) = f x : map f xs
```

```
ghci> map (+3) [1..3]
[4,5,6]
```

###### filter

**filter** 函数取一个限制条件和一个 List，回传该 List 中所有符合该条件的元素。

```haskell
filter :: (a -> Bool) -> [a] -> [a]  
filter _ [] = []  
filter p (x:xs)   
    | p x       = x : filter p xs  
    | otherwise = filter p xs 
```

```
ghci> filter (>3) [1,5,3,2,1,6,4,3,2,1]  
[5,6,4]
```

###### takeWhile

它取一个限制条件和 List 作参数，然后从头开始遍历这一 List，并回传符合限制条件的元素。 而一旦遇到不符合条件的元素，它就停止了。

```
ghci> takeWhile (<3) [1..10]
[1,2]
```

###### 利用 map 来得到函数的 List （难点）

```
ghci> let listOfFuns = map (*) [0..]  
ghci> (listOfFuns !! 4) 5  
20
```

`*` 是一个二元函数，用单个参数调用二元函数会回传一个一元函数。

所以 listOfFuns 是一个 List，其中的元素是一个个一元函数： `[(0*),(1*),(2*)..]`

接着取出其中 index = 4 的元素，即一元函数 (4*)

用 5 来调用它，最终得到 20

###### lambda

lambda 就是匿名函数。有些时候我们需要传给高阶函数一个函数，而这函数我们只会用这一次，这就弄个特定功能的 lambda。编写 lambda，就写个 `\` (因为它看起来像是希腊字母的 lambda -- 如果你斜视的厉害)，后面是用空格分隔的参数，`->` 后面就是函数体。通常我们都是用括号将其括起，要不然它就会占据整个右边部分。

```
ghci> filter (\x -> odd x && x > 50) [1..100]
[51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91,93,95,97,99]
```

###### foo a = bar b a && foo = bar b（柯里化）

```
foo a = bar b a

foo = bar b
```

两者是等价的

###### foldl

 List 的许多函数都有固定的模式，通常我们会将边界条件设置为空 List，再引入 `(x:xs)` 模式，对单个元素和余下的 List 做些事情。这一模式是如此常见，因此 Haskell 引入了一组函数来使之简化，也就是 `fold`。它们与map有点像，只是它们回传的是单个值。

```
ghci> :t foldl
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
```

> warning: t a 代表啥？元素类型为 a 的 List 么？

foldr 有三个参数

1. 一个二元函数，二元函数的一个参数是累加值，另一个参数是 List 中的元素，返回值将作为新的累加值
2. 一个初始值/累加值
3. 一个 List

```haskell
sum' :: Num a => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
```

```
sum' [1,2,3,4]
acc = 0 x = 1 => acc = 1
acc = 1 x = 2 => acc = 3
acc = 3 x = 3 => acc = 6
acc = 6 x = 4 => acc = 10       ret
```

###### foldr

foldr 行为与 foldl 类型，但是它是从右边开始遍历的

```
ghci> :t foldr
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
```

用 foldl 和 foldr 实现 map

```haskell
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldl (\acc x -> acc++[f x]) [] xs

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x:acc) [] xs
```

> 注意 foldl 和 foldr 分别是 `acc x` 和 `x acc`
>
> 在 foldl 中 ，acc 是二元函数的第一个参数
>
> 而在 foldr 中，acc 是二元函数的第二个参数

###### foldl1 && foldr1

他两的行为与 foldl 和 foldr 类似，只是你无需明确提供初始值。他们假定 List 的首个(或末尾)元素作为起始值，并从旁边的元素开始折叠。

```
ghci> :t foldl1
foldl1 :: Foldable t => (a -> a -> a) -> t a -> a
```

```
ghci> :t foldr1
foldr1 :: Foldable t => (a -> a -> a) -> t a -> a
```

```
ghci> foldl1 (-) [1..3]
-4
ghci> foldr1 (\x acc -> acc - x) [1..3]
0
```

###### scanl && scanr && scanl1 && scanr1

**scanl** 和 **scanr** 与 `foldl` 和 `foldr` 相似，只是它们会记录下累加值的所有状态到一个 List。

```
ghci> :t scanl
scanl :: (b -> a -> b) -> b -> [a] -> [b]
ghci> :t scanr
scanr :: (a -> b -> b) -> b -> [a] -> [b]
ghci> :t scanl1
scanl1 :: (a -> a -> a) -> [a] -> [a]
ghci> :t scanrl
scanr1 :: (a -> a -> a) -> [a] -> [a]
```

```
ghci> scanl (+) 0 [1..3]
[0,1,3,6]
ghci> scanr (+) 0 [1..3]
[6,5,3,0]
ghci> scanl1 (+) [1..3]
[1,3,6]
ghci> scanr1 (+) [1..3]
[6,5,3]
```

实际上最终得到的是一个记录 acc 状态的 List

###### 函数调用符: $

```
ghci> :t ($)
($) :: (a -> b) -> a -> b
```

什么鬼东西？这没啥意义的操作符？它只是个函数调用符罢了？好吧，不全是，但差不多。普通的函数调用符有最高的优先级，而 `$` 的优先级则最低。用空格的函数调用符是左结合的，如 `f a b c` 与 `((f a) b) c` 等价，而 `$` 则是右结合的。

听着不错。但有什么用？它可以减少我们代码中括号的数目。

```
sum (map sqrt [1..130])
sum $ map sqrt [1..130]
sqrt (3 + 4 + 9)
sqrt $ 3 + 4 + 9
```

除了减少括号外，`$` 还可以将数据作为函数使用。例如映射一个函数调用符到一组函数组成的 List：

```
ghci> map ($ 3) [(4+),(10*),(^2),sqrt]  
[7.0,30.0,9.0,1.7320508075688772]  
```

```
ghci> :t (3)
(3) :: Num p => p
ghci> :t ($ 3)
($ 3) :: Num a => (a -> b) -> b
```

###### Function Compostiton 函数组合: .

函数组合的数学定义：
$$
(f \circ g)(x)=f(g(x))
$$

```
ghci> :t (.)
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)  
```

它接收两个函数，然后生成一个新的函数。

f 的参数的类型与 g 的返回指的类型相同。

```
ghci> map (negate . abs) [-2..3]	// 先求绝对值，再取负
[-2,-1,0,-1,-2,-3]
```

含多个参数的函数该怎么办？我们可以使用不全调用使每个函数都只剩下一个参数。

```
sum (replicate 5 (max 6.7 8.9))

(sum . replicate 5 . max 6.7) 8.9

sum . replicate 5 . max 6.7 $ 8.9
```

### Modules

Haskell 中的模块是含有一组相关的函数，型别和型别类的组合。而 Haskell 进程的本质便是从主模块中引用其它模块并调用其中的函数来执行操作。

###### Preclude

目前为止我们谈及的所有函数,型别以及型别类都是 `Prelude` 模块的一部分，它缺省自动装载。

###### 导入模块 import

###### import Data.List

###### :m

**导入单个模块**

```
:m Data.List
```

**导入多个模块**

```
:m Data.List Data.Map Data.Set  
```

调用 `Data.Map` 模块的 `filter` 函数的话仅需 `M.filter` 就行了

```
import Data.List (nub，sort)  
```

**导入除去某函数之外的其它函数**

```
import Data.List hiding (nub) 
```

**避免命名冲突**

避免命名冲突还有个方法，便是 `qualified import`，`Data.Map` 模块提供一了一个按键索值的数据结构，它里面有几个和 `Prelude` 模块重名的函数。如 `filter` 和 `null`，装入 `Data.Map` 模块之后再调用 `filter`，Haskell 就不知道它究竟是哪个函数。如下便是解决的方法:

```haskell
import qualified Data.Map  
```

这样一来，再调用 `Data.Map` 中的 `filter` 函数，就必须得 `Data.Map.filter`，而 `filter` 依然是为我们熟悉喜爱的样子

**为模块取个别名**

```
import qualified Data.Map as M
```

调用 `Data.Map` 模块的 `filter` 函数的话仅需 `M.filter` 就行了

**函数手册**

https://hoogle.haskell.org/

##### Data.List

之前所使用的 map 和 filter，其实本来是 Data.List 模块里面的函数。不过 Prelude 模块为了方便起见，把它们导出了。因为这几个函数是直接引用自 `Data.List`，所以就无需使用 `qualified import`。

###### intersperse

```
ghci> import Data.List
ghci> :t intersperse
intersperse :: a -> [a] -> [a]
```

 取一个元素与 List 作参数，并将该元素置于 List 中每对元素的中间。

```
ghci> intersperse 0 [1..4]
[1,0,2,0,3,0,4]
```

###### intercalate

取两个 List 作参数。它会将第一个 List 交叉插入第二个 List 中间，并返回一个 List。

```
ghci> :t intercalate
intercalate :: [a] -> [[a]] -> [a]
```

```
ghci> intercalate [0,100] [[1],[2],[3]]
[1,0,100,2,0,100,3]
```

###### transpose 

反转一组 List 的 List

```
ghci> :t transpose
transpose :: [[a]] -> [[a]]
```

```
ghci> transpose [[1,2,3],[4,5,6]]
[[1,4],[2,5],[3,6]]
```

```
1 2 3	=>  1 4
4 5 6       2 5
            3 6
```

```
ghci> transpose [[1,2,3],[4,5]]
[[1,4],[2,5],[3]]
```

```
1 2 3	=>	 1 4
4 5			 2 5
			 3
```

假设有三个多项式

```
	    3x^2 + 5x + 9
10x^3 +             9
8x^3  + 5x^2  + x - 1
```

求他们的和，我们可以列三个 List

```
[0,3,5,9]
[10,0,0,9]
[8,5,1,-1]
```

通过如下方式取得结果

```
ghci> map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]
[18,8,6,17]
```

###### concat

把一组 List 连接成一个 List

```
ghci> :t concat
concat :: Foldable t => t [a] -> [a]
```

```
ghci> concat ["hello","world"]
"helloworld"
```

###### concatMap

实际上就是先调用 map，再调用 concat

```
ghci> :t concatMap
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
```

```
ghci> concatMap (replicate 4) [1..3]  
[1,1,1,1,2,2,2,2,3,3,3,3]
```

```
ghci> concat $ map (replicate 4) [1..3]
[1,1,1,1,2,2,2,2,3,3,3,3]
```

###### and

取一组 Bool 的 List 作参数。只有其中的值全为 `True` 的情况下才会返回 `True`

```
ghci> :t and
and :: Foldable t => t Bool -> Bool
```

```
ghci> and [True,True,False]
False
ghci> and [True,True,True]
True
```

###### or

一组 Bool  List 中若存在一个 `True` 它就返回 `True`

###### any / all

取一个限制条件和一组布林值 List 作参数，检查是否该 List 的某个元素或每个元素都符合该条件。

```
ghci> :t any
any :: Foldable t => (a -> Bool) -> t a -> Bool
ghci> :t all
all :: Foldable t => (a -> Bool) -> t a -> Bool
```

###### iterate

```
ghci> :t iterate
iterate :: (a -> a) -> a -> [a]
```

取一个函数和一个值作参数。它会用该值去调用该函数并用所得的结果再次调用该函数，产生一个无限的 List

```
ghci> take 10 $ iterate (*2) 1
[1,2,4,8,16,32,64,128,256,512]
```

###### splitAt

```
ghci> :t splitAt
splitAt :: Int -> [a] -> ([a], [a])
```

取一个 List 和数值作参数，将该 List 在特定的位置断开。返回一个包含两个 List 的二元组.

```
ghci> splitAt 3 "heyman"  
("hey","man")  
ghci> splitAt 100 "heyman"  
("heyman","")  
ghci> splitAt (-3) "heyman"  
("","heyman") 
```

###### dropWhile

当为 true 时就丢弃。一旦为 false，就返回余下的部分

```
ghci> :t dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
```

```
ghci> takeWhile (<=5) [1..10]
[1,2,3,4,5]
ghci> dropWhile (<=5) [1..10]
[6,7,8,9,10]
```

###### span

```
ghci> :t span
span :: (a -> Bool) -> [a] -> ([a], [a])
```

与 `takeWhile` 有点像，只是它返回两个 List。第一个 List 与同参数调用 `takeWhile` 所得的结果相同，第二个 List 就是原 List 中余下的部分。

> 个人感觉 span = takeWhile + dropWhile

```
ghci> takeWhile (>=5) [1..10]
[]
ghci> dropWhile (>=5) [1..10]
[1,2,3,4,5,6,7,8,9,10]
ghci> span (>=5) [1..10]
([],[1,2,3,4,5,6,7,8,9,10])
```

###### break

**span** 是在条件首次为 `False` 时断开 List，而 `break` 则是在条件首次为 `True` 时断开 `List`。`break p` 与 `span (not . p)` 是等价的。

```
ghci> :t span
span :: (a -> Bool) -> [a] -> ([a], [a])
```

```
ghci> span (<=5) [1..10]
([1,2,3,4,5],[6,7,8,9,10])
ghci> break (<=5) [1..10]
([],[1,2,3,4,5,6,7,8,9,10])
```

###### sort

排序

```
ghci> :t sort
sort :: Ord a => [a] -> [a]
```

```
ghci> sort [3,5,1]
[1,3,5]
```

###### group

```
ghci> :t group
group :: Eq a => [a] -> [[a]]
```

取一个 List 作参数，并将其中相邻并相等的元素各自归类，组成一个个子 List

```
ghci> group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]  
[[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]
```

###### inits && tails

作用和 init 与 tail 相似。它们会递归地调用自身直到什么都不剩

```
ghci> init [1..5]
[1,2,3,4]
ghci> inits [1..5]
[[],[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]
ghci> tail [1..5]
[2,3,4,5]
ghci> tails [1..5]
[[1,2,3,4,5],[2,3,4,5],[3,4,5],[4,5],[5],[]]
```

###### isInfixOf && isPrefixOf && **isSuffixOf** 

从一个 List 中搜索一个子 List，若该 List 包含子 List，则返回 `True`

```
ghci> :t isInfixOf
isInfixOf :: Eq a => [a] -> [a] -> Bool
```

**isPrefixOf** 与 **isSuffixOf** 分别检查一个 List 是否以某子 List 开头或者结尾

###### elem && notElem

检查一个 List 是否包含某元素

```
ghci> :t elem
elem :: (Foldable t, Eq a) => a -> t a -> Bool
```

###### partition

取一个限制条件和 List 作参数，返回两个 List，第一个 List 中包含所有符合条件的元素，而第二个 List 中包含余下的

```
ghci> :t partition
partition :: (a -> Bool) -> [a] -> ([a], [a])
```

```
ghci> partition (`elem` ['a'..'z']) ['A'..'z']
("abcdefghijklmnopqrstuvwxyz","ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`")
```

###### span && break && partition

`span` 和 `break` 会在遇到第一个符合或不符合条件的元素处断开，而 `partition` 则会遍历整个 List

###### find

查找

```
ghci> :t find
find :: Foldable t => (a -> Bool) -> t a -> Maybe a
```

```
ghci> find (>5) [1..10]
Just 6
ghci> find (>10) [1..10]
Nothing
```

> Maybe 是啥？

###### elemIndex && elemIndices

返回元素的索引

```
ghci> :t elemIndex  
elemIndex :: (Eq a) => a -> [a] -> Maybe Int  
ghci> 4 `elemIndex` [1,2,3,4,5,6]  
Just 3  
ghci> 10 `elemIndex` [1,2,3,4,5,6]  
Nothing
```

```
ghci> :t elemIndices
elemIndices :: Eq a => a -> [a] -> [Int]
ghci> 1 `elemIndices` [1,3,1,2]
[0,2]
```

###### findIndex && findIndices

与 `find` 相似，但它返回的是可能存在的首个符合该条件元素的索引。**findIndices** 会返回所有符合条件的索引

```
ghci> findIndex (==4) [5,3,2,1,6,4]  
Just 5  
ghci> findIndex (==7) [5,3,2,1,6,4]  
Nothing 
```

###### zip && zip3 ... zip7

```
ghci> zip [1..2] [12..14]
[(1,12),(2,13)]
ghci> zip3 [1..2] [12..14] [22..22]
[(1,12,22)]
```

以长度最短的那个为准

###### lines && unlines

分割行 / 合并行

```
ghci> :t lines
lines :: String -> [String]
ghci> lines "first line\nsecond line\nthird line"  
["first line","second line","third line"]
```

```
ghci> :t unlines
unlines :: [String] -> String
ghci> unlines ["first line","second line","third line"]
"first line\nsecond line\nthird line\n"
```

###### words && unwords

```
ghci> :t words
words :: String -> [String]
ghci> words "hello world"
["hello","world"]
```

```
ghci> :t unwords
unwords :: [String] -> String
ghci> unwords ["hello","world"]
"hello world"
```

###### nub

去重

```
ghci> :t nub
nub :: Eq a => [a] -> [a]
```

```
ghci> nub [1,1,1,2,2,3]
[1,2,3]
```

###### delete

取一个元素和 List 作参数，会删掉该 List 中首次出现的这一元素

```
ghci> :t delete
delete :: Eq a => a -> [a] -> [a]
ghci> delete 'l' "hello"
"helo"
```

###### 差集 \

它会从左边 List 中的元素扣除存在于右边 List 中的元素一次

```
ghci> :t (\\)
(\\) :: Eq a => [a] -> [a] -> [a]
```

```
ghci> [2,2,2] \\[2,2]
[2]
```

###### 并集 union

```
ghci> [1..10] `union` [5..15]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
```

###### 交集 intersect

```
ghci> [1..10] `intersect` [5..15]
[5,6,7,8,9,10]
```

###### insert

可以将一个元素插入一个可排序的 List，并将其置于首个大于等于它的元素之前

```
ghci> :t insert
insert :: Ord a => a -> [a] -> [a]
ghci> insert 3 [1,2,4,3,2,1]
[1,2,3,4,3,2,1]
```

###### on

```
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c  
f `on` g = \x y -> f (g x) (g y)
```

执行 `(==) `on` (> 0)` 得到的函数就与 `\x y -> (x > 0) == (y > 0)` 基本等价。`on` 与带 `By` 的函数在一起会非常好用，你可以这样写：

```
ghci> groupBy ((==) `on` (> 0)) values  
[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
```

```
ghci> let values = [-4.3，-2.4，-1.2，0.4，2.3，5.9，10.5，29.1，5.3，-2.4，-14.5，2.9，2.3]  
ghci> groupBy (\x y -> (x > 0) == (y > 0)) values  
[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
```

##### Data.Char

`Data.Char` 模块包含了一组用于处理字符的函数。由于字串的本质就是一组字符的 List，所以往往会在 `filter` 或是 `map` 字串时用到它。

###### isXXX

| 函数名        | 作用                                                      |
| ------------- | --------------------------------------------------------- |
| isControl     | 判断一个字符是否是控制字符                                |
| isSpace       | 判断一个字符是否是空格字符，包括空格，tab，换行符等       |
| isLower       | 判断一个字符是否为小写                                    |
| isUper        | 判断一个字符是否为大写                                    |
| isAlpha       | 判断一个字符是否为字母                                    |
| isAlphaNum    | 判断一个字符是否为字母或数字                              |
| isPrint       | 判断一个字符是否是可打印的                                |
| isDigit       | 判断一个字符是否为数字                                    |
| isOctDigit    | 判断一个字符是否为八进制数字                              |
| isHexDigit    | 判断一个字符是否为十六进制数字                            |
| isLetter      | 判断一个字符是否为字母                                    |
| isMark        | 判断是否为 unicode 注音字符，你如果是法国人就会经常用到的 |
| isNumber      | 判断一个字符是否为数字                                    |
| isPunctuation | 判断一个字符是否为标点符号                                |
| isSymbol      | 判断一个字符是否为货币符号                                |
| isSeperater   | 判断一个字符是否为 unicode 空格或分隔符                   |
| isAscii       | 判断一个字符是否在 unicode 字母表的前 128 位              |
| isLatin1      | 判断一个字符是否在 unicode 字母表的前 256 位              |
| isAsciiUpper  | 判断一个字符是否为大写的 ascii 字符                       |
| isAsciiLower  | 判断一个字符是否为小写的 ascii 字符                       |

以上所有判断函数的型别声明皆为 `Char -> Bool`

###### generalCategory

字符所在的类型

```
ghci> :t generalCategory
generalCategory :: Char -> GeneralCategory
```

GeneralCategory 和 Ordering 类型相似，也是一个枚举。它表示了一个字符可能所在的分类。

```
ghci> generalCategory ' '
Space
ghci> generalCategory 'A'
UppercaseLetter
```

###### toXXX

| 函数名     | 作用                                                         |
| ---------- | ------------------------------------------------------------ |
| toUpper    | 将一个字符转为大写字母，若该字符不是小写字母，就按原值返回   |
| toLower    | 将一个字符转为小写字母，若该字符不是大写字母，就按原值返回   |
| toTitle    | 将一个字符转为 title-case，对大多数字元而言，title-case 就是大写 |
| digittoInt | 将一个字符转为 Int 值，而这一字符必须得在 '1'..'9','a'..'f'或'A'..'F'的范围之内（十六进制） |
| intToDigit | 将数字转换为字符                                             |

```
ghci> digitToInt 'f'
15
ghci> digitToInt 'F'
15
ghci> intToDigit 15
'f'
```

###### ord && chr

将字符与其对应的数字相互转换

```
ghci> ord 'a'  
97  
ghci> chr 97  
'a'
```

##### Data.Map

二元组的 List 即为 Map

```
m = [(1,"one"),(2,"two")]
```

###### 自定义 findKey

```
findKey :: Eq k => k -> [(k,v)] -> v
findKey key kvs = snd . head . filter (\(k,v) -> k == key) $ kvs
```

> 其实最后的 kvs 都可以省掉的，毕竟有柯里化嘛

```
ghci> findKey 1 [(1,2),(3,4)]
2
ghci> findKey 2 [(1,2),(3,4)]
*** Exception: Prelude.head: empty list
```

因为对 [] 调用 head，会导致异常

所以就应该用 `Maybe` 型别。如果没找到相应的键，就返回 `Nothing`，而找到了就返回 `Just something`。

```
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v 
findKey key [] = Nothing
findKey key ((k,v):xs) = 
     if key == k then 
         Just v 
     else 
         findKey key xs
```

导入 Map 模块

```
import qualified Data.Map as Map
```

###### fromList

```
ghci> :t Map.fromList
Map.fromList :: Ord k => [(k, a)] -> Map k a
```

根据 List 生成一个 Map

```
ghci> Map.fromList [(1,2),(11,12)]
fromList [(1,2),(11,12)]
ghci> Map.fromList [(1,2),(11,12),(1,3)]
fromList [(1,3),(11,12)]
ghci> Map.fromList [(1,2),(11,12),(1,2)]
fromList [(1,2),(11,12)]
```

对于重复的 key ，会进行去重

###### empty

返回一个空 Map

```
ghci> Map.empty
fromList []
```

###### insert

往 Map 内插值

```
ghci> :t Map.insert
Map.insert :: Ord k => k -> a -> Map k a -> Map k a
```

```
ghci> Map.insert  1 2 Map.empty
fromList [(1,2)]
```

###### null

检查 Map 是否为空

```
ghci> Map.null Map.empty 
True 
```

###### size

返回 map 的 size

```
ghci> Map.size Map.empty 
0
```

###### singleton 

取一个键值对做参数,并返回一个只含有一个映射的 `map`

```
ghci> Map.singleton 3 9 
fromList [(3,9)] 
```

###### lookup 

查找，如果找到键对应的值。就返回 `Just something`，否则返回 `Nothing`。

###### member

是个判断函数，它取一个键与 `map` 做参数，并返回该键是否存在于该 `map`

```
ghci> Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)] 
True
```

###### map

```
ghci> Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)] 
fromList [(1,100),(2,400),(3,900)] 
```

###### filter

```
ghci> Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')] 
fromList [(2,'A'),(4,'B')]
```

###### toList

fromList 的反函数

```
ghci> Map.toList . Map.insert 9 2 $ Map.singleton 4 3 
[(4,3),(9,2)]
```

###### keys  && elems

各自返回一组由键或值组成的 List

```
ghci> keys $ Map.fromList [(1,2),(11,12)]
[1,11]
ghci> elems $ Map.fromList [(1,2),(11,12)]
[2,12]
```

###### fromListWith

```
ghci> :t fromListWith
fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Map k a
```

约定了 key 冲突时的处理方式

```
ghci> fromListWith (\x y -> x + y) [(1,1),(1,2)]
fromList [(1,3)]
```

###### insertWith 

作用与 fromListWith 类似，约定了插入冲突的处理方式

##### Data.Set

```
import qualified Data.Set as Set
```

###### fromList

```
ghci> :t Set.fromList
Set.fromList :: Ord a => [a] -> Set.Set a
```

> 注意 a 必须是 Ord ，而非 Eq

###### difference

得到存在于第一个集合但不在第二个集合的元素

###### union

两个集合的并集

###### null && size && member && empty && singleton && insert && delete

略

###### isSubsetOf && isProperSubsetOf

判断子集与真子集

子集(subset)：如果集合 A 中的元素都属于集合 B，那么 A 就是 B 的子集。

真子集(proper subset)：如果 A 中的元素都属于 B 且 B 的元素比 A 多，那 A 就是 B 的真子集。

```
ghci> Set.fromList [2,3,4] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]  
True  
ghci> Set.fromList [1,6] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
False
ghci> Set.fromList [1,2,3,4] `Set.isProperSubsetOf` Set.fromList [1,2,3,4,5]
True
ghci> Set.fromList [1,2,3,4,5] `Set.isProperSubsetOf` Set.fromList [1,2,3,4,5]  
False
```

###### map && filter

```
ghci> Set.filter odd $ Set.fromList [3,4,5,6,7,2,3,4]
fromList [3,5,7]
ghci> Set.map (+1) $ Set.fromList [3,4,5,6,7,2,3,4]
fromList [3,4,5,6,7,8]
```

##### 自己的模块

我们已经见识过了几个很酷的模块，但怎样才能构造自己的模块呢? 几乎所有的编程语言都允许你将代码分成多个文件，Haskell 也不例外。在编程时，将功能相近的函数和型别至于同一模块中会是个很好的习惯。这样一来，你就可以轻松地一个 `import` 来重用其中的函数.

接下来我们将构造一个由计算机几何图形体积和面积组成的模块，先从新建一个 `Geometry.hs` 的文件开始.

在模块的开头定义模块的名称，如果文件名叫做 `Geometry.hs` 那它的名字就得是 `Geometry`。在声明出它含有的函数名之后就可以编写函数的实现啦，就这样写:

```
module Geometry  
( sphereVolume  
, sphereArea  
, cubeVolume  
, cubeArea  
, cuboidArea  
, cuboidVolume  
) where
```

如你所见，我们提供了对球体,立方体和立方体的面积和体积的解法。继续进发，定义函数体:

```
module Geometry  ( sphereVolume  , sphereArea  , cubeVolume  , cubeArea  , cuboidArea  , cuboidVolume  ) where  
sphereVolume :: Float -> Float  sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)  
sphereArea :: Float -> Float  sphereArea radius = 4 * pi * (radius ^ 2)  
cubeVolume :: Float -> Float  cubeVolume side = cuboidVolume side side side  
cubeArea :: Float -> Float  cubeArea side = cuboidArea side side side  
cuboidVolume :: Float -> Float -> Float -> Float  cuboidVolume a b c = rectangleArea a b * c  
cuboidArea :: Float -> Float -> Float -> Float  cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  
rectangleArea :: Float -> Float -> Float  rectangleArea a b = a * b
```

![img](https://gblobscdn.gitbook.com/assets%2F-LjUmp_4rLaEvLYs4D0h%2F-LjUmptMINNvnlhyXusA%2F-LjUmqdPk0mUwejnvmEw%2Fmaking_modules.png?alt=media)

标准的几何公式。有几个地方需要注意一下，由于立方体只是长方体的特殊形式，所以在求它面积和体积的时候我们就将它当作是边长相等的长方体。在这里还定义了一个 `helper`函数，`rectangleArea` 它可以通过长方体的两条边计算出长方体的面积。它仅仅是简单的相乘而已，份量不大。但请注意我们可以在这一模块中调用这个函数，而它不会被导出! 因为我们这个模块只与三维图形打交道.

当构造一个模块的时候，我们通常只会导出那些行为相近的函数，而其内部的实现则是隐蔽的。如果有人用到了 `Geometry` 模块，就不需要关心它的内部实现是如何。我们作为编写者，完全可以随意修改这些函数甚至将其删掉，没有人会注意到里面的变动，因为我们并不把它们导出.

要使用我们的模块，只需:



```
import Geometry
```

将 `Geometry.hs` 文件至于用到它的进程文件的同一目录之下.

模块也可以按照分层的结构来组织，每个模块都可以含有多个子模块。而子模块还可以有自己的子模块。我们可以把 `Geometry` 分成三个子模块，而一个模块对应各自的图形对象.

首先，建立一个 `Geometry` 文件夹，注意首字母要大写，在里面新建三个文件

如下就是各个文件的内容:

Sphere.hs



```
module Geometry.Sphere  ( volume  , area  ) where  
volume :: Float -> Float  volume radius = (4.0 / 3.0) * pi * (radius ^ 3)  
area :: Float -> Float  area radius = 4 * pi * (radius ^ 2)
```

Cuboid.hs



```
module Geometry.Cuboid  ( volume  , area  ) where  
volume :: Float -> Float -> Float -> Float  volume a b c = rectangleArea a b * c  
area :: Float -> Float -> Float -> Float  area a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  
rectangleArea :: Float -> Float -> Float  rectangleArea a b = a * b
```

Cube.hs



```
module Geometry.Cube  ( volume  , area  ) where  
import qualified Geometry.Cuboid as Cuboid  
volume :: Float -> Float  volume side = Cuboid.volume side side side  
area :: Float -> Float  area side = Cuboid.area side side side
```

好的! 先是 `Geometry.Sphere`。注意，我们将它置于 `Geometry` 文件夹之中并将它的名字定为 `Geometry.Sphere`。对 Cuboid 也是同样，也注意下，在三个模块中我们定义了许多名称相同的函数，因为所在模块不同，所以不会产生命名冲突。若要在 `Geometry.Cube` 使用 `Geometry.Cuboid` 中的函数，就不能直接 `import Geometry.Cuboid`，而必须得 `qualified import`。因为它们中间的函数名完全相同.



```
import Geometry.Sphere
```

然后，调用 `area` 和 `volume`，就可以得到球体的面积和体积，而若要用到两个或更多此类模块，就必须得 `qualified import` 来避免重名。所以就得这样写:



```
import qualified Geometry.Sphere as Sphere  
import qualified Geometry.Cuboid as Cuboid  
import qualified Geometry.Cube as Cube
```

然后就可以调用 `Sphere.area`，`Sphere.volume`，`Cuboid.area` 了，而每个函数都只计算其对应物体的面积和体积.

以后你若发现自己的代码体积庞大且函数众多，就应该试着找找目的相近的函数能否装入各自的模块，也方便日后的重用.

# type

##### 型别名和构造子

Bool 的定义

```haskell
data Bool = False | True
```

*data* 表示我们要定义一个新的型别

`=` 的左端标明型别的名称即 `Bool`，`=` 的右端就是*值构造子* (*Value Constructor*)，它们明确了该型别可能的值。

`|` 读作"或"，所以可以这样阅读该声明：`Bool` 型别的值可以是 `True` 或 `False`。

**型别名和值构造子的首字母必大写**

##### 项

```
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
```

`Circle` 的值构造子有三个项，都是 Float。可见我们在定义值构造子时，可以在后面跟几个型别表示它包含值的型别。在这里，前两项表示圆心的坐标，尾项表示半径。`Rectangle` 的值构造子取四个 `Float` 项，前两项表示其左上角的坐标，后两项表示右下角的坐标。

谈到「项」 (field)，其实应为「参数」 (parameters)。值构造子的本质是个函数，可以返回一个型别的值。

```haskell
ghci> :t Circle
Circle :: Float -> Float -> Float -> Shape
ghci> :t Rectangle
Rectangle :: Float -> Float -> Float -> Float -> Shape
```

```haskell
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

值得一提的是，它的型别声明表示了该函数取一个 `Shape` 值并返回一个 `Float` 值。写 `Circle -> Float` 是不可以的，因为 `Circle` 并非型别，真正的型别应该是 `Shape`。这与不能写`True->False` 的道理是一样的。

##### deriving

```
Prelude> Circle 10 20

<interactive>:9:1: error:
    • No instance for (Show (Float -> Shape))
        arising from a use of ‘print’
        (maybe you haven't applied a function to enough arguments?)
    • In a stmt of an interactive GHCi command: print it
```

们若尝试输出 `Circle 10 20` 到控制台，就会得到一个错误。这是因为 Haskell 还不知道该型别的字符串表示方法。想想，当我们往控制台输出值的时候，Haskell 会先调用 `show` 函数得到这个值的字符串表示才会输出。因此要让我们的 `Shape` 型别成为 Show 型别类的成员。可以这样修改：

```haskell
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
```

```
Prelude> data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
Prelude> map (Circle 10 20) [4,5,6,6]
[Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]
```

##### 型别和值构造子使用相同的名字

```
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
```

注意下 `Point` 的定义，它的型别与值构造子用了相同的名字。没啥特殊含义，实际上，在一个型别含有唯一值构造子时这种重名是很常见的。

```
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

##### 辅助函数 (auxilliary function)

如果不想直接处理 `Point`，我们可以搞个辅助函数 (auxilliary function)，初始从原点创建图形，再移动它们。

```
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)
```

##### export type

```haskell
module Shapes
( Point(..)
, Shape(..)
, surface
, nudge
, baseCircle
, baseRect
) where
```

一个 `Shape` (..)，我们就导出了 `Shape` 的所有值构造子。这一来无论谁导入我们的模块，都可以用 `Rectangle` 和 `Circle` 值构造子来构造 `Shape` 了。这与写 `Shape(Rectangle,Circle)` 等价。

我们可以选择不导出任何 `Shape` 的值构造子，这一来使用我们模块的人就只能用辅助函数 `baseCircle` 和 `baseRect` 来得到 `Shape` 了。`Data.Map` 就是这一套，没有 `Map.Map [(1,2),(3,4)]`，因为它没有导出任何一个值构造子。但你可以用，像 `Map.fromList` 这样的辅助函数得到 `map`。应该记住，值构造子只是函数而已，如果不导出它们，就拒绝了使用我们模块的人调用它们。但可以使用其他返回该型别的函数，来取得这一型别的值。

不导出数据型别的值构造子隐藏了他们的内部实现，令型别的抽象度更高。同时，我们模块的用户也就无法使用该值构造子进行模式匹配了。

##### Record Syntax

**困境**

我们需要一个数据型别来描述一个人，得包含他的姓、名、年龄、身高、电话号码以及最爱的冰淇淋。我不知你的想法，不过我觉得要了解一个人，这些数据就够了。就这样，实现出来！

```
data Person = Person String String Int Float String String deriving (Show)
```

弄个函数得人的某项数据又该如何？如姓的函数，名的函数，等等。好吧，我们只能这样：

```
firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname

lastName :: Person -> String
lastName (Person _ lastname _ _ _ _) = lastname

age :: Person -> Int
age (Person _ _ age _ _ _) = age

height :: Person -> Float
height (Person _ _ _ height _ _) = height

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ number _) = number

flavor :: Person -> String
flavor (Person _ _ _ _ _ flavor) = flavor
```

**解决办法**

```
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)
```

```
ghci> :t flavor
flavor :: Person -> String
ghci> :t firstName
firstName :: Person -> String
```

##### Type parameters

`c++ 的模板`

值构造子可以取几个参数产生一个新值

同样，型别构造子可以取型别作参数，产生新的型别

```
data Maybe a = Nothing | Just a
```

这里的 a 就是个 type parameters

`Maybe` 就成为了一个型别构造子。在它的值不是 `Nothing` 时，它的型别构造子可以搞出 `Maybe Int`，`Maybe String` 等等诸多态别。<u>但只一个 `Maybe` 是不行的，因为它不是型别，而是型别构造子</u>。

```
ghci> Just "Haha"
Just "Haha"
ghci> Just 84
Just 84
ghci> :t Just "Haha"
Just "Haha" :: Maybe [Char]
ghci> :t Just 84
Just 84 :: (Num t) => Maybe t
ghci> :t Nothing
Nothing :: Maybe a
ghci> Just 10 :: Maybe Double
Just 10.0
```

###### List

其实 LIst 也用到了 type parameter

List 型别实际上就是取一个参数来生成一个特定型别，这型别可以是 `[Int]`，`[Char]` 也可以是 `[String]`，但不会跟在 `[]` 的后面。

###### 永远不要在 `data` 声明中添加型别约束

我们之前还遇见过一个型别参数的应用，就是 `Data.Map` 中的 `Map k v`。 `k` 表示 Map 中键的型别，`v` 表示值的型别。这是个好例子，Map 中型别参数的使用允许我们能够用一个型别索引另一个型别，只要键的型别在 `Ord` 型别类就行。如果叫我们自己定义一个 Map 型别，可以在 `data` 声明中加上一个型别类的约束。

```
data (Ord k) => Map k v = ...
```

然而 Haskell 中有一个严格的约定，那就是永远不要在 `data` 声明中添加型别约束。为啥？嗯，因为这样没好处，反而得写更多不必要的型别约束。`Map k v` 要是有 `Ord k` 的约束，那就相当于假定每个 Map 的相关函数都认为 `k` 是可排序的。若不给数据型别加约束，我们就不必给那些不关心键是否可排序的函数另加约束了。这类函数的一个例子就是 `toList`，它只是把一个 Map 转换为关联 List 罢了，型别声明为 `toList :: Map k v -> [(k, v)]`。要是加上型别约束，就只能是 `toList :: (Ord k) =>Map k a -> [(k,v)]`，明显没必要嘛。

###### 型别构造子 && 值构造子

我们实现个表示三维矢量的型别，再给它加几个处理函数。我么那就给它个型别参数，虽然大多数情况都是数值型，不过这一来它就支持了多种数值型别。

```
data Vector a = Vector a a a deriving (Show)
vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)
vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)
scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n
```

`vplus` 用来相加两个矢量，即将其所有对应的项相加。`scalarMult` 用来求两个矢量的标量积，`vectMult` 求一个矢量和一个标量的积。这些函数可以处理 `Vector Int`，`Vector Integer`，`Vector Float` 等等型别，只要 `Vector a` 里的这个 `a` 在 `Num` 型别类中就行。同样，如果你看下这些函数的型别声明就会发现，它们只能处理相同型别的矢量，其中包含的数字型别必须与另一个矢量一致。注意，我们并没有在 `data` 声明中添加 `Num` 的类约束。反正无论怎么着都是给函数加约束。

再度重申，型别构造子和值构造子的区分是相当重要的。<u>**在声明数据型别时，等号=左端的那个是型别构造子，右端的(中间可能有|分隔)都是值构造子。**</u>拿 `Vector t t t -> Vector t t t -> t` 作函数的型别就会产生一个错误，因为在型别声明中只能写型别，而 `Vector` 的型别构造子只有个参数，它的值构造子才是有三个。我们就慢慢耍：

```
ghci> Vector 3 5 8 `vplus` Vector 9 2 8
Vector 12 7 16
ghci> Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3
Vector 12 9 19
ghci> Vector 3 9 7 `vectMult` 10
Vector 30 90 70
ghci> Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0
74.0
ghci> Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)
Vector 148 666 222
```

##### Derived instances

`Eq`, `Ord`, `Enum`, `Bounded`, `Show`, `Read`。只要我们在构造型别时在后面加个 `deriving`(派生)关键字，Haskell 就可以自动地给我们的型别加上这些行为。

```haskell
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)
```

```
ghci> let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
ghci> mikeD
Person {firstName = "Michael", lastName = "Diamond", age = 43}
ghci> "mikeD is: " ++ show mikeD
"mikeD is: Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}"
ghci> read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person
Person {firstName = "Michael", lastName = "Diamond", age = 43}
```

###### Ord

```
data Bool = False | True deriving (Ord)
```

由于值构造子 `False` 安排在 `True` 的前面，我们可以认为 `True` 比 `False` 大。

##### Type synonyms 类型别名

在前面我们提到在写型别名的时候，`[Char]` 和 `String` 等价，可以互换。这就是由型别别名实现的。型别别名实际上什么也没做，只是给型别提供了不同的名字，让我们的代码更容易理解。这就是 `[Char]` 的别名 `String` 的由来。

```
type String = [Char]
```

我们已经介绍过了 `type` 关键字，这个关键字有一定误导性，它并不是用来创造新类（这是 `data` 关键字做的事情），而是给一个既有型别提供一个别名。

###### Type synonyms  可以有参数

```
type AssocList k v = [(k,v)]
```

###### 不全调用

```
type IntMap v = Map Int v
```

也可以这样：

```
type IntMap = Map Int
```

###### Either a b

```
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
```

它有两个值构造子。如果用了 `Left`，那它内容的型别就是 `a`；用了 `Right`，那它内容的型别就是 `b`。我们可以用它来将可能是两种型别的值封装起来，从里面取值时就同时提供 `Left` 和 `Right` 的模式匹配。

```
ghci> Right 20
Right 20
ghci> Left "w00t"
Left "w00t"
ghci> :t Right 'a'
Right 'a' :: Either a Char
ghci> :t Left True
Left True :: Either Bool b
```

##### Recursive data structures (递归地定义数据结构)

考虑一下 List: `[5]`。他其实是 `5:[]` 的语法糖。在 `:` 的左边是一个普通值，而在右边是一串 List。只是在这个案例中是空的 List。再考虑 `[4,5]`。他可以看作 `4:(5:[])`。看看第一个 `:`，我们看到他也有一个元素在左边，一串 List `5:[]` 在右边。同样的道理 `3:(4:(5:6:[]))` 也是这样。

我们可以说一个 List 的定义是要么是空的 List 或是一个元素，后面用 `:` 接了另一串 List。

我们用 algebraic data type 来实作我们自己的 List！

```
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
```

读起来好像我们前一段提及的定义。他要么是空的 List，或是一个元素跟一串 List 的结合。如果你被搞混了，看看用 record syntax 定义的可能比较清楚。

```
data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)
```

###### cons

你可能也对这边的 `Cons` 构造子不太清楚。`cons` 其实就是指 `:`。对 List 而言，`:` 其实是一个构造子，他接受一个值跟另一串 List 来构造一个 List。现在我们可以使用我们新定义的 List 型态。换句话说，他有两个 field，其中一个 field 具有型态 `a`，另一个有型态 `[a]`。

```
ghci> Empty
Empty
ghci> 5 `Cons` Empty
Cons 5 Empty
ghci> 4 `Cons` (5 `Cons` Empty)
Cons 4 (Cons 5 Empty)
ghci> 3 `Cons` (4 `Cons` (5 `Cons` Empty))
Cons 3 (Cons 4 (Cons 5 Empty))
```

我们用中缀的方式调用 `Cons` 构造子，这样你可以很清楚地看到他就是 `:`。`Empty` 代表 `[]`，而 `4 `Cons` (5 `Cons` Empty)` 就是 `4:(5:[])`。

###### infixr 符号优先级

```
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
```

首先我们留意新的语法结构：fixity 宣告。当我们定义函数成 operator，我们能同时指定 fixity (但并不是必须的)。fixity 指定了他应该是 left-associative 或是 right-associative，还有他的优先级。例如说，`*` 的 fixity 是 `infixl 7 *`，而 `+` 的 fixity 是 `infixl 6`。代表他们都是 left-associative。`(4 * 3 * 2)` 等于 `((4 * 3) * 2)`。但 `*` 拥有比 `+` 更高的优先级。所以 `5 * 4 + 3` 会是 `(5 * 4) + 3`。

###### 二元搜索树  的实现

```
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
```

```
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
      | x == a = Node x left right
      | x < a  = Node a (treeInsert x left) right
      | x > a  = Node a left (treeInsert x right)
```

```
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right
```

##### Typeclasses 范型

来快速复习一下什么是 typeclass: typeclass 就像是 interface。一个 typeclass 定义了一些行为(像是比较相不相等，比较大小顺序，能否穷举)而我们会把希望满足这些性质的型别定义成这些 typeclass 的 instance。typeclass 的行为是由定义的函数来描述。并写出对应的实作。当我们把一个型别定义成某个 typeclass 的 instance，就表示我们可以对那个型别使用 typeclass 中定义的函数。

######  `Prelude` 之中 `Eq`  的定义

`Eq` 这个 typeclass 是描述可以比较相等的事物。他定义了 `==` 跟 `/=` 两个函数。

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
```

`class Eq a where`，那代表我们定义了一个新的 typeclass 叫做 `Eq`。`a` 是一个型别变量，他代表 `a` 是任何我们在定义 instance 时的型别。

然后我们又定义了几个函数。我们并不一定要实作函数的本体，不过必须要写出函数的型别宣告。

总之我们实作了 `Eq` 中需要定义的函数本体，只是我们定义他的方式是用交互递归的形式。我们描述两个 `Eq` 的 instance 要相等，那他们就不能不一样，而他们如果不一样，那他们就是不相等。我们其实不必这样写，但很快你会看到这其实是有用的。

###### instance

来看看下面这个型别：

```haskell
data TrafficLight = Red | Yellow | Green
```

这里定义了红绿灯的状态。请注意这个型别并不是任何 class 的 instance，虽然可以透过 derive 让它成为 `Eq` 或 `Show` 的 instance，但我们打算手工打造。下面展示了如何让一个型别成为 `Eq` 的 instance：

```haskell
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False
```

我们使用了 `instance` 这个关键字。class 是用来定义新的 typeclass，而 instance 是用来说明我们要定义某个 typeclass 的 instance。当我们要定义 `Eq`，我们会写 `class Eq a where`，其中 `a` 代表任何型态。我们可以从 instance 的写法：`instance Eq TrafficLight where` 看出来。我们会把 `a` 换成实际的型别。

由于 `==` 是用 `/=` 来定义的，同样的 `/=` 也是用 `==` 来定义。所以我们只需要在 instance 定义中复写其中一个就好了。我们这样叫做定义了一个 minimal complete definition。这是说能让型别符合 class 行为所最小需要实作的函数数量。而 `Eq` 的 minimal complete definition 需要 `==` 或 `/=` 其中一个。而如果 `Eq` 是这样定义的：

```
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
```

当我们定义 instance 的时候必须要两个函数都实作，因为 Haskell 并不知道这两个函数是怎么关联在一起的。所以 minimal complete definition 在这边是 `==` 跟 `/=`。

你可以看到我们是用模式匹配来实作 `==`。由于不相等的情况比较多，所以我们只写出相等的，最后再用一个 `case` 接住说你不在前面相等的 `case` 的话，那就是不相等。

###### show de instance

我们再来写 `Show` 的 instance。要满足 `Show` 的 minimal complete definition，我们必须实作 `show` 函数，他接受一个值并把他转成字串。

```
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"
```

再一次地，我们用模式匹配来完成我们的任务。我们来看看他是如何运作的。

```
ghci> Red == Red
True
ghci> Red == Yellow
False
ghci> Red `elem` [Red, Yellow, Green]
True
ghci> [Red, Yellow, Green]
[Red light,Yellow light,Green light]
```

如果我们用 `derive` 来自动产生 `Eq` 的话，效果是一样的。不过用 `derive` 来产生 `show` 的话，他会把值构造子转换成字串。但我们这边要的不太一样，我们希望印出像 `"Red light"` 这样的字串，所以我们就必须手动来写出 instance。

###### typeclass 的 subclass

> typeclass —> sub typeclass 可以理解为继承关系

你也可以把 typeclass 定义成其他 typeclass 的 subclass。像是 `Num` 的 class 宣告就有点冗长，但我们先看个雏型。

```
class (Eq a) => Num a where
   ...
```

正如我们先前提到的，我们可以在很多地方加上 class constraints。这不过就是在 `class Num a where` 中的 `a` 上，加上他必须要是 `Eq` 的 instance 的限制。这基本上就是在说我们在定义一个型别为 `Num` 之前，必须先为他定义 `Eq` 的 instance。在某个型别可以被视作 `Number` 之前，必须先能被比较相不相等其实是蛮合理的。这就是 subclass 在做的事：帮 class declaration 加上限制。也就是说当我们定义 typeclass 中的函数本体时，我们可以缺省 `a` 是属于 `Eq`，因此能使用 `==`。

###### instance 与 maybe

但像是 `Maybe` 或是 List 是如何被定义成 typeclass 的 instance 呢？`Maybe` 的特别之处在于他跟 `TrafficLight` 不一样，他不是一个具体的型别。他是一个型别构造子，接受一个型别参数（像是 `Char` 之类的）而构造出一个具体的型别（像是 `Maybe Char` ）。让我们再回顾一下 `Eq` 这个 typeclass：

```
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
```

从型别宣告来看，可以看到 `a` 必须是一个具体型别，因为所有在函数中的型别都必须是具体型别。(你没办法写一个函数，他的型别是 `a -> Maybe`，但你可以写一个函数，他的型别是 `a -> Maybe a`，或是 `Maybe Int -> Maybe String`) 这就是为什么我们不能写成像这样：

```
instance Eq Maybe where
    ...
```

```
instance Eq (Maybe m) where
    Just x == Just y = x == y // 这里的 x 和 y 不一样是 Eq 的
    Nothing == Nothing = True
    _ == _ = False
```

这就好像在说我们要把 `Maybe something` 这种东西全部都做成 `Eq` 的 instance。我们的确可以写成 `(Maybe something)`，但我们通常是只用一个字母，这样比较像是 Haskell 的风格。`(Maybe m)` 这边则取代了 `a` 在 `class Eq a where` 的位置。尽管 `Maybe` 不是一个具体的型别。`Maybe m` 却是。指定一个型别参数（在这边是小写的 `m`），我们说我们想要所有像是 `Maybe m` 的都成为 `Eq` 的 instance。

**一个问题**

我们用 `==` 来比较 `Maybe` 包含的东西，但我们并没有任何保证说 `Maybe` 装的东西可以是 `Eq`。这就是为什么我们需要修改我们的 instance 定义：

```
instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
```

这边我们必须要加上限制。在这个 instance 的宣告中，<u>我们希望所有 `Maybe m` 形式的型别都属于 `Eq`</u>，但只有当 `m` 也属于 `Eq` 的时候。这也是 Haskell 在 derive 的时候做的事。

###### ::info

还有一件事要确认。如果你想看看一个 typeclass 有定义哪些 instance。可以在 ghci 中输入 `:info YourTypeClass`。所以输入 `:info Num` 会告诉你这个 typeclass 定义了哪些函数，还有哪些型别属于这个 typeclass。`:info` 也可以查找型别跟型别构造子的信息。如果你输入 `:info Maybe`。他会显示 `Maybe` 所属的所有 typeclass。`:info` 也能告诉你函数的型别宣告。

```
Prelude> :info Num
type Num :: * -> Constraint
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
  	-- Defined in ‘GHC.Num’
instance Num Word -- Defined in ‘GHC.Num’
instance Num Integer -- Defined in ‘GHC.Num’
instance Num Int -- Defined in ‘GHC.Num’
instance Num Float -- Defined in ‘GHC.Float’
instance Num Double -- Defined in ‘GHC.Float’
```

###### yes-no typeclass

这是我们自己定义的一个 typeclass

```haskell
class YesNo a where
    yesno :: a -> Bool
```

接下来我们来定义一些 instance。对于数字，我们会假设任何非零的数字都会被当作 `true`，而 0 则当作 `false`。

```
instance YesNo Int where
    yesno 0 = False
    yesno _ = True
```

空的 List (包含字串)代表 `false`，而非空的 List 则代表 `true`。

```
instance YesNo [a] where
    yesno [] = False
    yesno _ = True
```

留意到我们加了一个型别参数 `a` 来让整个 List 是一个具体型别，不过我们并没有对包涵在 List 中的元素的型别做任何额外假设。我们还剩下 `Bool` 可以被作为真假值，要定义他们也很容易：

id

```
instance YesNo Bool where
    yesno = id
```

你说 `id` 是什么？他不过是标准函式库中的一个函数，他接受一个参数并回传相同的东西。

我们也让 `Maybe a` 成为 `YesNo` 的 instance。

```
instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False
```

由于我们不必对 `Maybe` 的内容做任何假设，因此并不需要 class constraint。我们只要定义遇到 `Just` 包装过的值就代表 true，而 `Nothing` 则代表 false。这里还是得写出 `(Maybe a)` 而不是只有 `Maybe`，毕竟 `Maybe -> Bool` 的函式并不存在（因为 `Maybe` 并不是具体型别），而 `Maybe a -> Bool` 看起来就合理多了。现在有了这个定义，`Maybe something` 型式的型别都属于 `YesNo` 了，不论 `something` 是什么。

现在我们定义了许多 instance，来试着跑跑看！

```
ghci> yesno $ length []
False
ghci> yesno "haha"
True
ghci> yesno ""
False
ghci> yesno $ Just 0
True
ghci> yesno True
True
ghci> yesno EmptyTree
False
ghci> yesno []
False
ghci> yesno [0,0,0]
True
ghci> :t yesno
yesno :: (YesNo a) => a -> Bool
```

很好，统统是我们预期的结果。我们来写一个函数来模仿 if statement 的行为，但他是运作在 `YesNo` 的型别上。

```
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
    if yesno yesnoVal then yesResult else noResult
```

很直觉吧！他接受一个 yes or no 的值还有两个部份，如果值是代表 "yes"，那第一个部份就会被执行，而如果值是 "no"，那第二个部份就会执行。

```
ghci> yesnoIf [] "YEAH!" "NO!"
"NO!"
ghci> yesnoIf [2,3,4] "YEAH!" "NO!"
"YEAH!"
ghci> yesnoIf True "YEAH!" "NO!"
"YEAH!"
ghci> yesnoIf (Just 500) "YEAH!" "NO!"
"YEAH!"
ghci> yesnoIf Nothing "YEAH!" "NO!"
"NO!"
```

###### Functor

`Functor` 是 Haskell 里面的一个 typeclass，它代表可以被 map over 的事物。听到这个词你可能会联想到 List，因为 map over list 在 Haskell 中是很常见的操作。你没想错，List 的确是属于 `Functor` 这个 typeclass。

来看看他的实作会是了解 `Functor` 的最佳方式：

```
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

我们看到他定义了一个函数 `fmap`，而且并没有提供一个缺省的实作。

到目前为止的我们看过的 typeclass 中的型别变量都是具体型别。就像是 `(==) :: (Eq a) => a -> a -> Bool` 中的 `a` 一样。

但现在碰到的 `f` 并不是一个具体型别（一个像是 `Int`, `Bool` 或 `Maybe String`的型别），<u>而是接受一个型别参数的型别构造子</u>。

如果要快速回顾的话可以看一下 `Maybe Int` 是一个具体型别，而 `Maybe` 是一个型别构造子，可接受一个型别作为参数。

总之，我们知道 `fmap` 接受一个函数，这个函数从一个型别映射到另一个型别，还接受一个 functor 装有原始的型别，然后会回传一个 functor 装有映射后的型别。

不过这边 `fmap` 的型别宣告让我们想起类似的东西，就是 `map :: (a -> b) -> [a] -> [b]`。

他接受一个函数，这函数把一个型别的东西映射成另一个。还有一串装有某个型别的 List 变成装有另一个型别的 List。到这边听起来实在太像 functor 了。实际上，`map` 就是针对 List 的 `fmap`。来看看 List 是如何被定义成 `Functor` 的 instance 的。

```
instance Functor [] where
    fmap = map
```

注意到我们不是写成 `instance Functor [a] where`，因为从 `fmap :: (a -> b) -> f a -> f b` 可以知道 `f` 是一个型别构造子，他接受一个型别。而 `[a]` 则已经是一个具体型别（一个拥有某个型别的 List），其中 `[]` 是一个型别构造子，能接受某个型别而构造出像 `[Int]`、`[String]` 甚至是 `[[String]]` 的具体型别。

对于 List，`fmap` 只不过是 `map`，对 List 操作的时候他们都是一样的。

```
map :: (a -> b) -> [a] -> [b]
ghci> fmap (*2) [1..3]
[2,4,6]
ghci> map (*2) [1..3]
[2,4,6]
```

至于当我们对空的 List 操作 `map` 或 `fmap` 呢？我们会得到一个空的 List。他把一个型别为 `[a]` 的空的 List 转成型别为 `[b]` 的空的 List。

看看 `Maybe` 作为一个 functor 的定义：

```
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
```

注意到我们是写 `instance Functor Maybe where` 而不是 `instance Functor (Maybe m) where`，就像我们在写 `YesNo` 时的 `Maybe` 一样。`Functor` 要的是一个接受一个型别参数的型别构造子而不是一个具体型别。如果你把 `f` 代换成 `Maybe`。`fmap` 就会像 `(a -> b) -> Maybe a -> Maybe b`。但如果你把 `f` 代换成 `(Maybe m)`，那他就会像 `(a -> b) -> Maybe m a -> Maybe m b`，这看起来并不合理，因为 `Maybe` 只接受一个型别参数。

总之，`fmap` 的实作是很简单的。如果一个空值是 `Nothing`，那他就会回传 `Nothing`。如果我们 map over 一个空的盒子，我们就会得到一个空的盒子。就像我们 map over 一个空的 List，那我们就会得到一个空的 List。如果他不是一个空值，而是包在 `Just` 中的某个值，那我们就会套用在包在 `Just` 中的值。

```
ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")
Just "Something serious. HEY GUYS IM INSIDE THE JUST"
ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing
Nothing
ghci> fmap (*2) (Just 200)
Just 400
ghci> fmap (*2) Nothing
Nothing
```

另外 `Tree a` 的型别也可以被 map over 且被定义成 `Functor` 的一个 instance。 `Tree` 的型别构造子也刚好接受单一一个型别参数。如果你把 `fmap` 看作是一个特别为 `Tree` 写的函数，他的型别宣告会长得像这样 `(a -> b) -> Tree a -> Tree b`。不过我们在这边会用到递归。map over 一棵空的树会得到一棵空的树。map over 一棵非空的树会得到一棵被函数映射过的树，他的 root 会先被映射，然后左右子树都分别递归地被函数映射。

```
instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) =
        Node (f x) (fmap f leftsub) (fmap f rightsub)
```

```
ghci> fmap (*2) EmptyTree
EmptyTree
ghci> fmap (*4) (foldr treeInsert EmptyTree [5,7,3,2,1,7])
Node 28 (Node 4 EmptyTree (Node 8 EmptyTree (Node 12 EmptyTree (Node 20 EmptyTree EmptyTree)))) EmptyTree
```

那 `Either a b` 又如何？他可以是一个 functor 吗？`Functor` 限制型别构造子只能接受一个型别参数，但 `Either` 却接受两个。聪明的你会想到我可以 partial apply `Either`，先喂给他一个参数，并把另一个参数当作 free parameter。来看看 `Either a` 在标准函式库中是如何被定义的：

```
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x
```

我们在这边做了些什么？你可以看到我们把 `Either a` 定义成一个 instance，而不是 `Either`。那是因为 `Either a` 是一个接受单一型别参数的型别构造子，而 `Either` 则接受两个。如果 `fmap` 是针对 `Either a`，那他的型别宣告就会像是 `(b -> c) -> Either a b -> Either a c`，他又等价于 `(b -> c) -> (Either a) b -> (Either a) c`。在实作中，我们碰到一个 `Right` 的时候会做 `map`，但在碰到 `Left` 的时候却不这样做，为什么呢？如果我们回头看看 `Either a b` 是怎么定义的：

```
data Either a b = Left a | Right b
```

如果我们希望对他们两个都做 `map` 的动作，那 `a` 跟 `b` 必须要是相同的型别。也就是说，如果我们的函数是接受一个字串然后回传另一个字串，而且 `b` 是字串，`a` 是数字，这样的情形是不可行的。而且从观察 `fmap` 的型别也可以知道，当他运作在 `Either` 上的时候，第一个型别参数必须固定，而第二个则可以改变，而其中第一个参数正好就是 `Left` 用的。

我们持续用盒子的比喻也仍然贴切，我们可以把 `Left` 想做是空的盒子在他旁边写上错误消息，说明为什么他是空的。

在 `Data.Map` 中的 Map 也可以被定义成 functor，像是 `Map k v` 的情况下，`fmap` 可以用 `v -> v'` 这样一个函数来 map over `Map k v`，并回传 `Map k v'`。

注意到 `'` 在这边并没有特别的意思，他只是用来表示他跟另一个东西有点像，只有一点点差别而已。

###### Kind :t

型别构造子接受其他型别作为他的参数，来构造出一个具体型别。这样的行为会让我们想到函数，也是接受一个值当作参数，并回传另一个值。我们也看过型别构造子可以 partially apply （`Either String` 是一个型别构造子，他接受一个型别来构造出一个具体型别，就像 `Either String Int`）。

像是 `3`,`"YEAH"` 或是 `takeWhile` 的值他们都有自己的型别（函数也是值的一种，我们可以把他们传来传去）型别是一个标签，值会把他带着，这样我们就可以推测出他的性质。但型别也有他们自己的标签，叫做 kind，kind 是<u>型别的型别</u>。虽然听起来有点玄妙，不过他的确是个有趣的概念。

那kind可以拿来做什么呢？我们可以在 ghci 中用 `:k` 来得知一个型别的 kind。

```
ghci> :k Int
Int :: *
```

一个 `*` 代表这个型别是<u>具体型别</u>。一个具体型别是没有任何型别参数，而值只能属于具体型别。而 `*` 的读法叫做 star 或是 type。

我们再看看 `Maybe` 的 kind：

```
ghci> :k Maybe
Maybe :: * -> *
```

`Maybe` 的型别构造子接受一个具体型别（像是 `Int`）然后回传一个具体型别，像是 `Maybe Int`。这就是 kind 告诉我们的信息。就像 `Int -> Int` 代表这个函数接受 `Int` 并回传一个 `Int`。`* -> *` 代表这个型别构造子接受一个具体型别并回传一个具体型别。我们再来对 `Maybe` 套用型别参数后再看看他的 kind 是什么：

```
ghci> :k Maybe Int
Maybe Int :: *
```

正如我们预期的。我们对 `Maybe` 套用了型别参数后会得到一个具体型别（这就是 `* -> *` 的意思）这跟 `:t isUpper` 还有 `:t isUpper 'A'` 的差别有点类似。`isUpper` 的型别是 `Char -> Bool` 而 `isUpper 'A'` 的型别是 `Bool`。而这两种型别，都是 `*` 的 kind。

我们对一个型别使用 `:k` 来得到他的 kind。就像我们对值使用 `:t` 来得到的他的型别一样。就像我们先前说的，型别是值的标签，而 kind 是型别的标签。

我们再来看看其他的 kind

```
ghci> :k Either
Either :: * -> * -> *
```

这告诉我们 `Either` 接受两个具体型别作为参数，并构造出一个具体型别。他看起来也像是一个接受两个参数并回传值的函数型别。型别构造子是可以做 curry 的，所以我们也能 partially apply。

```
ghci> :k Either String
Either String :: * -> *
ghci> :k Either String Int
Either String Int :: *
```

当我们希望定义 `Either` 成为 `Functor` 的 instance 的时候，我们必须先 partial apply，因为 `Functor` 预期有一个型别参数，但 `Either` 却有两个。也就是说，`Functor` 希望型别的 kind 是 `* -> *`，而我们必须先 partial apply `Either` 来得到 kind `* -> *`，而不是最开始的 `* -> * -> *`。我们再来看看 `Functor` 的定义

```
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

我们看到 `f` 型别变量是接受一个具体型别且构造出一个具体型别的型别。 知道他构造出具体型别是因为是作为函数参数的型别。 从那里我们可以推测出一个型别要是属于 `Functor` 必须是 `* -> *` kind。

现在我们来练习一下。来看看下面这个新定义的 typeclass。

```
class Tofu t where
    tofu :: j a -> t a j
```

```
tofu :: j a
```

由于 `j a` 被当作 `tofu` 这个函数的参数的型别，所以 `j a` 一定是 `*` kind。我们假设 `a` 是 `*` kind，那 `j` 就会是 `* -> *` 的 kind 。

```
tofu :: j a -> t a j
```

而知道 `a` 是 `*`，`j` 是 `* -> *`，我们可以推测出 `t`是`* -> (* -> *) -> *`。也就是说他接受一个具体型别 `a`，一个接受单一参数的型别构造子 `j`，然后产生出一个具体型别。

我们再来定义出一个型别具有 `* -> (* -> *) -> *` 的 kind，下面是一种定义的方法：

```
data Frank a b  = Frank {frankField :: b a} deriving (Show)
```

```
frankField :: b a // b a 的结果就是 frankField 这个字段的类型
```

ADT 中的字段是要来塞值的，所以他们必须是 `*` kind。我们假设 `a` 是 `*`，那 `b` 就是接受一个型别参数的 kind `* -> *`。

现在我们知道 `a` 跟 `b` 的 kind 了，而他们又是 `Frank` 的型别参数，所以我们知道 `Frank` 会有 `* -> (* -> *) -> *` 的 kind。第一个 `*` 代表 `a`，而 `(* -> *)` 代表 `b`。我们构造些 `Frank` 的值并检查他们的型别吧：

```
ghci> :t Frank {frankField = Just "HAHA"}
Frank {frankField = Just "HAHA"} :: Frank [Char] Maybe
ghci> :t Frank {frankField = Node 'a' EmptyTree EmptyTree}
Frank {frankField = Node 'a' EmptyTree EmptyTree} :: Frank Char Tree
ghci> :t Frank {frankField = "YES"}
Frank {frankField = "YES"} :: Frank Char []
```

由于 `frankField` 具有 `a b` 的型别。他的值必定有一个类似的型别。他们可能是 `Just "HAHA"`，也就有 `Maybe [Char]` 的型别，或是他们可能是 `['Y','E','S']`，他的型别是 `[Char]`。（如果我们是用自己定义的 List 型别的话，那就会是 `List Char`）。我们看到 `Frank` 值的型别对应到 `Frank` 的 kind。`[Char]` 具有 `*` 的 kind，而 `Maybe` 则是 `* -> *`。由于结果必须是个值，也就是他必须要是具体型别，因使他必须 fully applied，因此每个 `Frank blah blaah` 的值都会是 `*` 的 kind。

要把 `Frank` 定义成 `Tofu` 的 instance 也是很简单。我们看到 `tofu` 接受 `j a`（例如 `Maybe Int`）并回传 `t a j`。所以我们将 `Frank` 代入 `t`，就得到 `Frank Int Maybe`。

```
instance Tofu Frank where
    tofu x = Frank x
```

```
ghci> tofu (Just 'a') :: Frank Char Maybe
Frank {frankField = Just 'a'}
ghci> tofu ["HELLO"] :: Frank [Char] []
Frank {frankField = ["HELLO"]}
```

这并不是很有用，但让我们做了不少型别的练习。再来看看下面的型别：

###### 一个练习

```
data Barry t k p = Barry { yabba :: p, dabba :: t k }
```

```
ghci> :k Barry
Barry :: (* -> *) -> * -> * -> *
```

现在要把这个型别定义成 `Functor`，我们必须先 partially apply 头两个型别参数，这样我们就会是 `* -> *` 的 kind。这代表 instance 定义会是 `instance Functor (Barry a b) where`。如果我们看 `fmap` 针对 `Barry` 的型别，也就是把 `f` 代换成 `Barry c d`，那就会是 `fmap :: (a -> b) -> Barry c d a -> Barry c d b`。第三个 `Barry` 的型别参数是对于任何型别，所以我们并不牵扯进他。

```
instance Functor (Barry a b) where
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}
```

# 输入和输出

## hello world

##### 编译和执行 ghc --make

```
// hello_world.hs
main = putStrLn "hello, world"
```

```
➜  ghc --make hello_world.hs
[1 of 1] Compiling Main             ( hello_world.hs, hello_world.o )
Linking hello_world ...
➜  ./hello_world
hello, world
```

##### putStrLn

```
Prelude> :t putStrLn
putStrLn :: String -> IO ()
```

我们可以这么解读 `putStrLn` 的型态：`putStrLn` 接受一个字串并回传一个 I/O action，这 I/O action 包含了 `()` 的型态。（即空的 tuple，或者是 unit 型态）。一个 I/O action 是一个会造成副作用的动作，常是指读取输入或输出到屏幕，同时也代表会回传某些值。在屏幕打印出几个字串并没有什么有意义的回传值可言，所以这边用一个 `()` 来代表。

##### main

那究竟 I/O action 会在什么时候被触发呢？这就是 `main` 的功用所在。一个 I/O action 会在我们把它绑定到 `main` 这个名字并且执行程序的时候触发。

##### do

把整个程序限制在只能有一个 I/O action 看似是个极大的限制。这就是为什么我们需要 do 表示法来将所有 I/O action 绑成一个。来看看下面这个例子。

```
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
```

我们写了一个 do 并且接着一连串指令，就像写个命令式程序一般，每一步都是一个 I/O action。将所有 I/O action 用 do 绑在一起变成了一个大的 I/O action。

这个大的 I/O action 的型态是 `IO ()`，这完全是由最后一个 I/O action 所决定的。

##### getLine

```
Prelude> :t getLine
getLine :: IO String
```

我们可以看到 `getLine` 是一个回传 `String` 的 I/O action。因为它会等用户输入某些字串，这很合理。

##### <-

执行一个 I/O action `getLine` 并将它的结果绑定到 `name` 这个名字。`getLine` 的型态是 `IO String`，所以 `name` 的型态会是 `String`。

如果我们要从 I/O action 拿出某些数据，就一定同时要在另一个 I/O action 中。这就是 Haskell 如何漂亮地分开纯粹跟不纯粹的程序的方法。`getLine` 在这样的意义下是不纯粹的，因为执行两次的时候它没办法保证会回传一样的值。这也是为什么它需要在一个 `IO` 的型态建构子中，那样我们才能在 I/O action 中取出数据。而且任何一段程序一旦依赖着 I/O 数据的话，那段程序也会被视为 I/O code。

##### 在纯粹的代码中使用 I/O action 回传的数据

只要我们把 I/O action 回传的值，绑定它到一个名字，我们便可以暂时地使用它。像在 `name <- getLine` 中 `name` 不过是一个普通字串。

像在 `name <- getLine` 中 `name` 不过是一个普通字串，代表在盒子中的内容。我们能将这个普通的字串传给一个极度复杂的函数，并回传你一生会有多少财富。像是这样：

```haskell
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Read this carefully, because this is your future: " ++ tellFortune name
```

##### IO action 不等于 字符串

```haskell
nameTag = "Hello, my name is " ++ getLine
```

这是一段错误的代码，理由是 `++` 要求两个参数都必须是串列。他左边的参数是 `String`，也就是 `[Char]`。然而 `getLine` 的型态是 `IO String`。你不能串接一个字串跟 I/O action。我们必须先把 `String` 的值从 I/O action 中取出，而<u>**唯一可行**</u>的方法就是在 I/O action 中使用 `name <- getLine`。

##### IO action

每个 I/O action 都有一个值封装在里面。这也是为什么我们之前的程序可以这么写：

```
main = do
    foo <- putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
```

`foo` 只会有一个 `()` 的值，所以绑定到 `foo` 这个名字似乎是多余的。另外注意到我们并没有绑定最后一行的 `putStrLn` 给任何名字。那是因为在一个 do block 中，<u>**最后一个 action 不能绑定任何名字**</u>。

 `putStrLn "BLAH"` 可以写成 `_ <- putStrLn "BLAH"`。但这没什么实际的意义，所以我们宁愿写成 `putStrLn something`。

##### 用 do 串接 IO action

I/O actions 只会在绑定给 `main` 的时候或是在另一个用 do 串起来的 I/O action 才会执行。你可以用 do 来串接 I/O actions，再用 do 来串接这些串接起来的 I/O actions。不过只有最外面的 I/O action 被指定给 main 才会触发执行。

##### 在 do block 中使用 let binding

```haskell
import Data.Char

main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
```

##### 代码片段：一行一行不断地读取输入

```haskell
main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
```

当 if 在一个 I/O do block 中的时候，长的样子是 `if condition then I/O action else I/O action`。

###### return

在 Haskell 中，他的意义则是利用某个 pure value **<u>造出</u>** I/O action。并不代表结束 sub_routine

在 I/O do block 中放一个 `return` 并不会结束执行。像下面这个程序会执行到底。

```haskell
main = do
    return ()
    return "HAHAHA"
    line <- getLine
    return "BLAH BLAH BLAH"
    return 4
    putStrLn line
```

```
main = do
    a <- return "hell"
    b <- return "yeah!"
    putStrLn $ a ++ " " ++ b
```

可以看到 `return` 与 `<-` 作用相反。`return` 把 value 装进盒子中，而 `<-` 将 value 从盒子拿出来，并绑定一个名称。

#### IO 函数

##### putStr

```
Prelude> :t putStr
putStr :: String -> IO ()
```

`putStr` 跟 `putStrLn` 几乎一模一样，都是接受一个字串当作参数，并回传一个 I/O action 打印出字串到终端上，只差在 `putStrLn` 会换行而 `putStr` 不会罢了。

```haskell
main = do putStr "Hey, "
          putStr "I'm "
          putStrLn "Andy!"
```

```
$ runhaskell putstr_test.hs
Hey, I'm Andy!
```

##### putChar

```
Prelude> :t putChar
putChar :: Char -> IO ()
```

接受一个字符，并回传一个 I/O action 将他打印到终端上。

`putStr` 实际上就是 `putChar` 递归定义出来的。`putStr` 的边界条件是空字串，所以假设我们打印一个空字串，那他只是回传一个什么都不做的 I/O action，像 `return ()`。如果打印的不是空字串，那就先用 `putChar` 打印出字串的第一个字符，然后再用 `putStr` 打印出字串剩下部份。

```
putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do
    putChar x
    putStr xs
```

##### print

`print` 接受任何是 `Show` typeclass 的 instance 的型态的值，这代表我们知道如何用字串表示他，调用 `show` 来将值变成字串然后将其输出到终端上。基本上，他就是 `putStrLn . show`。首先调用 `show` 然后把结果喂给 `putStrLn`，回传一个 I/O action 打印出我们的值。

```
main = do print True
          print 2
          print "haha"
          print 3.2
          print [3,4,3]
```

```
$ runhaskell print_test.hs
True
2
"haha"
3.2
[3,4,3]
```

GHCI 实际上就是用了 `print` 来将值输出到终端。

##### getChar

` getChar`是一个从输入读进一个字符的 I/O action，因此他的 type signature 是 `getChar :: IO Char`，代表一个 I/O action 的结果是 `Char`。注意由于缓冲区的关系，只有当 Enter 被按下的时候才会触发读取字符的行为。

```
main = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main
        else return ()
```

```
$ runhaskell getchar_test.hs
hello sir
hello
```

##### when

`when` 这函数可以在 `Control.Monad` 中找到他 (你必须 `import Contorl.Monad` 才能使用他)。他在一个 do block 中看起来就像一个控制流程的 statement，但实际上他的确是一个普通的函数。他接受一个 boolean 值跟一个 I/O action。如果 boolean 值是 `True`，便回传我们传给他的 I/O action。如果 boolean 值是 `False`，便回传 `return ()`，即什么都不做的 I/O action。我们接下来用 `when` 来改写我们之前的程序。

> when 实际上就是少写了一个 `else return ()`

```
import Control.Monad

main = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main
```

##### sequence

`sequence` 接受一串 I/O action，并回传一个会依序执行他们的 I/O action。运算的结果是包在一个 I/O action 的一连串 I/O action 的运算结果。他的 type signature 是 `sequence :: [IO a] -> IO [a]`

```
Prelude> :t sequence
sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
```

```
main = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]
```

其实可以写成

```
main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs
```

所以 `sequence [getLine, getLine, getLine]` 作成了一个执行 `getLine` 三次的 I/O action。

一个常见的使用方式是我们将 `print` 或 `putStrLn` 之类的函数 map 到串列上。`map print [1,2,3,4]` 这个动作并不会产生一个 I/O action，而是一串 I/O action，就像是 `[print 1, print 2, print 3, print 4]`。如果我们将一串 I/O action 变成一个 I/O action，我们必须用 `sequence`



```
ghci> sequence (map print [1,2,3,4,5])
1
2
3
4
5
[(),(),(),(),()]
```

那 `[(),(),(),(),()]` 是怎么回事？当我们在 GHCI 中运算 I/O action，他会被执行并把结果打印出来，唯一例外是结果是 `()` 的时候不会被打印出。这也是为什么 `putStrLn "hehe"` 在 GHCI 中只会打印出 `hehe`（因为 `putStrLn "hehe"` 的结果是 `()`）。

##### mapM && mapM_

由于对一个串列 map 一个回传 I/O action 的函数，然后再 sequence 他这个动作太常用了。所以有一些函数在函式库中 `mapM` 跟 `mapM_`。`mapM` 接受一个函数跟一个串列，将对串列用函数 map 然后 sequence 结果。`mapM_` 也作同样的事，只是他把运算的结果丢掉而已。在我们不关心 I/O action 结果的情况下，`mapM_` 是最常被使用的。

```haskell
ghci> mapM print [1,2,3]
1
2
3
[(),(),()]
ghci> mapM_ print [1,2,3]
1
2
3
```

##### forever

`forever` 接受一个 I/O action 并回传一个永远作同一件事的 I/O action。你可以在 `Control.Monad` 中找到他。下面的程序会不断地要用户输入些东西，并把输入的东西转成大写输出到屏幕上。

```
import Control.Monad
import Data.Char


main = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l
```

##### forM

在 `Control.Monad` 中的 `forM` 跟 `mapM` 的作用一样，只是参数的顺序相反而已。第一个参数是串列，而第二个则是函数。

```
Prelude> :t Control.Monad.forM
Control.Monad.forM
  :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
Prelude> :t Control.Monad.mapM
Control.Monad.mapM
  :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
```

```
import Control.Monad

main = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors
```

```
$ runhaskell from_test.hs
Which color do you associate with the number 1?
white
Which color do you associate with the number 2?
blue
Which color do you associate with the number 3?
red
Which color do you associate with the number 4?
orange
The colors that you associate with 1, 2, 3 and 4 are:
white
blue
red
orange
```

你可以把 `forM` 的意思想成将串列中的每个元素作成一个 I/O action。至于每个 I/O action 实际作什么就要看原本的元素是什么。然后，执行这些 I/O action 并将结果绑定到某个名称上。或是直接将结果忽略掉。

## 文件与字符流

##### getContents

`getContents` 是一个从标准输入读取直到 end-of-file 字符的 I/O action。他的型态是 `getContents :: IO String`。最酷的是 `getContents` 是惰性 I/O (Lazy I/O)。当我们写了 `foo <- getContents`，他并不会马上读取所有输入，将他们存在 memory 里面。他只有当你真的需要输入数据的时候才会读取。

```haskell
import Data.Char

main = do
    contents <- getContents
    putStr (map toUpper contents)
```

我们将 `getContents` 取回的字串绑定到 `contents`。然后用 `toUpper` map 到整个字串后打印到终端上。记住字串基本上就是一串惰性的串列 (list)，同时 `getContents` 也是惰性 I/O，他不会一口气读入内容然后将内容存在内存中。实际上，他会一行一行读入并输出大写的版本，这是因为输出才是真的需要输入的数据的时候。

>     contents <- getContents
>     putStr (map toUpper contents)
>
> 这两个阶段，被没有返回，一直在流式处理

##### interact

从输入那一些字串，经由一些转换然后输出这样的模式实在太常用了。常用到甚至建立了一个函数叫 **interact**。`interact` 接受一个 `String -> String` 的函数，并回传一个 I/O action。那个 I/O action 会读取一些输入，调用提供的函数，然后把函数的结果打印出来。所以我们的程序可以改写成这样。

```haskell
main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in result
```

```haskell
main = interact $ unlines . filter ((<10) . length) . lines
```

能应用 `interact` 的情况有几种，像是从输入 pipe 读进一些内容，然后丢出一些结果的程序；或是从用户获取一行一行的输入，然后丢回根据那一行运算的结果，再拿取另一行。这两者的差别主要是取决于用户使用他们的方式。

#### 文件

```haskell
import System.IO

main = do
    handler <- openFile "echo.hs" ReadMode
    content <- hGetContents handler
    putStr content
    hClose handler
```

##### openFIle

他的 type signature 是 `openFile :: FilePath -> IOMode -> IO Handle`。他说了 `openFile` 接受一个文件路径跟一个 `IOMode`，并回传一个 I/O action，他会打开一个文件并把文件关联到一个 handle。

`FilePath` 不过是 `String` 的 type synonym。

```
type FilePath = String
```

`IOMode` 则是一个定义如下的型态

```
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
```

##### withFile

> ps 突然想到了 python

`withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a`。他接受一个文件路径，一个 `IOMode` 以及一个函数，这函数则接受一个 handle 跟一个 I/O action。`withFile` 最后回传一个会打开文件，对文件作某件事然后关掉文件的 I/O action。处理的结果是包在最后的 I/O action 中，这结果跟我们给的函数的回传是相同的。

```haskell
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result
```

##### hxxx

就像 `hGetContents` 对应 `getContents` 一样，只不过是针对某个文件。我们也有 **hGetLine**、**hPutStr**、**hPutStrLn**、**hGetChar** 等等。他们分别是少了 h 的那些函数的对应。只不过他们要多拿一个 handle 当参数，并且是针对特定文件而不是标准输出或标准输入。

##### readFile

**readFile** 的 type signature 是 `readFile :: FilePath -> IO String`。记住，`FilePath` 不过是 `String` 的一个别名。`readFile` 接受一个文件路径，回传一个惰性读取我们文件的 I/O action。然后将文件的内容绑定到某个字串。他比起先 `openFile`，绑定 handle，然后 `hGetContents` 要好用多了。这边是一个用 `readFile` 改写之前例子的范例：

```
import System.IO

main = do
    contents <- readFile "echo.hs"
    putStr contents
```

##### writeFile

**writeFile** 的型态是 `writefile :: FilePath -> String -> IO ()`。他接受一个文件路径，以及一个要写到文件中的字串，并回传一个写入动作的 I/O action。如果这个文件已经存在了，他会先把文件内容都砍了再写入。下面示范了如何把 girlfriend.txt 的内容转成大写然后写入到 girlfriendcaps.txt 中

```
import System.IO
import Data.Char

main = do
    contents <- readFile "girlfriend.txt"
    writeFile "girlfriendcaps.txt" (map toUpper contents)
```



```
$ runhaskell girlfriendtocaps.hs
$ cat girlfriendcaps.txt
HEY! HEY! YOU! YOU!
I DON'T LIKE YOUR GIRLFRIEND!
NO WAY! NO WAY!
I THINK YOU NEED A NEW ONE!
```

##### appendFile

**appendFile** 的型态很像 `writeFile`，只是 `appendFile` 并不会在文件存在时把文件内容砍掉而是接在后面。

假设我们有一个文件叫 todo.txt``，里面每一行是一件要做的事情。现在我们写一个程序，从标准输入接受一行将他加到我们的 to-do list 中。

```
import System.IO

main = do
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")
```

```
$ runhaskell appendtodo.hs
Iron the dishes
$ runhaskell appendtodo.hs
Dust the dog
$ runhaskell appendtodo.hs
Take salad out of the oven
$ cat todo.txt
Iron the dishes
Dust the dog
Take salad out of the oven
```

由于 `getLine` 回传的值不会有换行字符，我们需要在每一行最后加上 `"\n"`。

##### chunk && block && hSetBuffering && hFlush

还有一件事，我们提到 `contents <- hGetContents handle` 是惰性 I/O，不会将文件一次都读到内存中。 所以像这样写的话：

```
main = do
    withFile "something.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
```

实际上像是用一个 pipe 把文件弄到标准输出。正如你可以把 list 想成 stream 一样，你也可以把文件想成 stream。他会每次读一行然后打印到终端上。你也许会问这个 pipe 究竟一次可以塞多少东西，读去硬盘的频率究竟是多少？对于文本档而言，缺省的 buffer 通常是 line-buffering。这代表一次被读进来的大小是一行。这也是为什么在这个 case 我们是一行一行处理。对于 binary file 而言，缺省的 buffer 是 block-buffering。这代表我们是一个 chunk 一个 chunk 去读得。而一个 chunk 的大小是根据操作系统不同而不同。

你能用 `hSetBuffering` 来控制 buffer 的行为。他接受一个 handle 跟一个 `BufferMode`，回传一个会设置 buffer 行为的 I/O action。`BufferMode` 是一个 enumeration 型态，他可能的值有：`NoBuffering`, `LineBuffering` 或 `BlockBuffering (Maybe Int)`。其中 `Maybe Int` 是表示一个 chunck 有几个 byte。如果他的值是 `Nothing`，则操作系统会帮你决定 chunk 的大小。`NoBuffering` 代表我们一次读一个 character。一般来说 `NoBuffering` 的表现很差，因为他访问硬盘的频率很高。

接下来是我们把之前的范例改写成用 2048 bytes 的 chunk 读取，而不是一行一行读。

```
main = do
    withFile "something.txt" ReadMode (\handle -> do
        hSetBuffering handle $ BlockBuffering (Just 2048)
        contents <- hGetContents handle
        putStr contents)
```

用更大的 chunk 来读取对于减少访问硬盘的次数是有帮助的，特别是我们的文件其实是透过网络来访问。

##### removeFile renameFIle

略

#### 命令行

##### getArgs && getProgName

在 `System.Environment` 模块当中有两个很酷的 I/O actions，一个是 **getArgs**，他的 type 是 `getArgs :: IO [String]`，他是一个拿取命令行引数的 I/O action，并把结果放在包含的一个串列中。**getProgName** 的型态是 `getProgName :: IO String`，他则是一个 I/O action 包含了程序的名称。

```haskell
import System.Environment
import Data.List

main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are:"
    mapM putStrLn args
    putStrLn "The program name is:"
    putStrLn progName
```

```
$ ./arg-test first second w00t "multi word arg"
The arguments are:
first
second
w00t
multi word arg
The program name is:
arg-test
```

##### 一个小程序 `todo`

```haskell
import System.Environment
import System.Directory
import System.IO
import Data.List

-- 我们先作一个分发的 association list。他会把命令行引数当作 key，而对应的处理函数当作 value。这些函数的型态都是 [String] -> IO ()。
dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", add)
            , ("view", view)
            , ("remove", remove)
            ]
            
main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args
    
add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
    numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName
```

#### 随机数

##### random

在 `System.Random` 模块中。他包含所有满足我们需求的函数。让我们先来看其中一个，就是 **random**。他的型态是 `random :: (RandomGen g, Random a) => g -> (a, g)`。

```
cabal install --lib random # 需要先安装 random 模块
```

**RandomGen** typeclass 是指那些可以当作乱源的型态。而**Random** typeclass 则是可以装乱数的型态。一个布林值可以是随机值，不是 `True` 就是 `False`。

他接受一个 random generator，然后回传一个随机值以及一个新的 random generator。为什么他要回传一个新的 random generator 呢？就是下面我们要讲的。接着不断对返回的 generator 调用 random 产生新的随机数，一直递归下去。

##### StdGen && mkStdGen

**StdGen** 是 `RandomGen` 的一个 instance。 我们可以自己手动作一个 `StdGen` 也可以告诉系统给我们一个现成的。

要自己做一个 random generator，要使用 **mkStdGen** 这个函数。他的型态是 `mkStdGen :: Int -> StdGen`。他接受一个整数，然后根据这个整数会给一个 random generator。

##### 指定返回类型

```
ghci> random (mkStdGen 100)
```

```
<interactive>:1:0:
    Ambiguous type variable `a' in the constraint:
        `Random a' arising from a use of `random' at <interactive>:1:0-20
    Probable fix: add a type signature that fixes these type variable(s)  `
```

这是什么？由于 `random` 函数会回传 `Random` typeclass 中任何一种型态，所以我们必须告诉 Haskell 我们是要哪一种型态。不要忘了我们是回传 random value 跟 random generator 的一个 pair

```
ghci> random (mkStdGen 100) :: (Int, StdGen)
(-1352021624,651872571 1655838864)
```

##### 例子

仿真丢三次铜板的函数

```
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
    (secondCoin, newGen') = random newGen
    (thirdCoin, newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)
```

留意我们不需要写 `random gen :: (Bool, StdGen)`。那是因为我们已经在函数的型态宣告那边就表明我们要的是布林。而 Haskell 可以推敲出我们要的是布林值。

##### randoms

有一个函数叫 **randoms**，他接受一个 generator 并回传一个无穷串行。

```
ghci> take 5 $ randoms (mkStdGen 11) :: [Int]
[-1807975507,545074951,-1015194702,-1622477312,-502893664]
ghci> take 5 $ randoms (mkStdGen 11) :: [Bool]
[True,True,True,True,False]
ghci> take 5 $ randoms (mkStdGen 11) :: [Float]
[7.904789e-2,0.62691015,0.26363158,0.12223756,0.38291094]
```

##### randomR && randomRs 范围 random

`randomR :: (RandomGen g, Random a) :: (a, a) -> g -> (a, g)`，代表他有点类似 `random`。只不过他的第一个参数是一对数目，定义了最后产生乱数的上界以及下界。

```
ghci> randomR (1,6) (mkStdGen 359353)
(6,1494289578 40692)
ghci> randomR (1,6) (mkStdGen 35935335)
(3,1250031057 40692)
```

另外也有一个 **randomRs** 的函数，他会产生一连串在给定范围内的乱数：

```
ghci> take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]
"ndkxbvmomg"
```

##### getStdGen

他的型态是 `IO StdGen`。当你的程序执行时，他会跟系统要一个 random generator，并存成一个 global generator。`getStdGen` 会替你拿那个 global random generator 并把他绑定到某个名称上。

这里有一个简单的产生随机字串的程序。

```
import System.Random

main = do
    gen <- getStdGen
    putStr $ take 20 (randomRs ('a','z') gen)
```

#### exceptions

尽管有表达力够强的型态( 例如 Either )来辅助失败的情形，Haskell 仍然支持 exception，因为 exception 在 I/O 的 contexts 下是比较合理的。在处理 I/O 的时候会有一堆奇奇怪怪的事情发生，环境是很不能被信赖的。像是打开文件。文件有可能被 lock 起来，也有可能文件被移除了，或是整个硬盘都被拔掉。所以直接跳到处理错误的代码是很合理的。

我们了解到 I/O code 会丢出 exception 是件合理的事。至于 pure code 呢？其实他也能丢出 Exception。想想看 `div` 跟 `head` 两个案例。他们的型态是 `(Integral a) => a -> a -> a` 以及 `[a] -> a`。`Maybe` 跟 `Either` 都没有在他们的回传型态中，但他们都有可能失败。`div` 有可能除以零，而 `head` 有可能你传给他一个空的 list。

```
ghci> 4 `div` 0
*** Exception: divide by zero
ghci> head []
*** Exception: Prelude.head: empty list
```

pure code 能丢出 Exception，但 Exception 只能在 I/O section 中被接到（也就是在 `main` 的 do block 中）这是因为在 pure code 中你不知道什么东西什么时候会被 evaluate。因为 lazy 特性的缘故，程序没有一个特定的执行顺序，但 I/O code 有。

不要在 pure code 里面使用 Exception。利用 Haskell 的型态系统，尽量使用 `Either` 或 `Maybe` 之类的型态来表示可能失败的计算。

##### catch

我们必须使用 `System.IO.Error` 中的 **catch** 函数。他的型态是 `catch :: IO a -> (IOError -> IO a) -> IO a`。他接受两个参数，第一个是一个 I/O action。像是他可以接受一个打开文件的 I/O action。第二个是 handler。如果第一个参数的 I/O action 丢出了 Exception，则他会被传给 handler，他会决定要作些什么。所以整个 I/O action 的结果不是如预期中做完第一个参数的 I/O action，就是 handler 处理的结果。

handler 接受一个 `IOError` 型态的值，他代表的是一个 I/O exception 已经发生了。他也带有一些 exception 本身的信息。至于这型态在语言中使如何被实作则是要看编译器。这代表我们没办法用 pattern matching 的方式来查看 `IOError`。就像我们不能用 pattern matching 来查看 `IO something` 的内容。但我们能用一些 predicate 来查看他们。

我们来看看一个展示 `catch` 的程序



```
import System.Environment
import System.IO
import System.IO.Error


main = toTry `catch` handler


toTry :: IO ()
toTry = do (fileName:_) <- getArgs
            contents <- readFile fileName
            putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"


handler :: IOError -> IO ()
handler e = putStrLn "Whoops, had some trouble!"
```

首先你看到我们可以在关键字周围加上 backticks 来把 `catch` 当作 infix function 用，因为他刚好接受两个参数。这样使用让可读性变好。`toTry `catch` handler` 跟 `catch toTry handler` 是一模一样的。`toTry` 是一个 I/O action，而 `handler` 接受一个 `IOError`，并回传一个当 exception 发生时被执行的 I/O action。

来看看执行的结果。



```
$ runhaskell count_lines.hs i_exist.txt
The file has 3 lines!


$ runhaskell count_lines.hs i_dont_exist.txt
Whoops, had some trouble!
```

在 handler 里面我们并没有检查我们拿到的是什么样的 `IOError`，我们只是打印出 `"Whoops, had some trouble!"`。接住任何种类的 Exception 就跟其他语言一样，在 Haskell 中也不是一个好的习惯。假如其他种类的 Exception 发生了，好比说我们送一个中断指令，而我们没有接到的话会发生什么事？这就是为什么我们要做跟其他语言一样的事：就是检查我们拿到的是什么样的 Exception。如果说是我们要的 Exception，那就做对应的处理。如果不是，我们再重新丢出 Exception。我们把我们的程序这样修改，只接住文件不存在的 Exception。



```
import System.Environment
import System.IO
import System.IO.Error


main = toTry `catch` handler


toTry :: IO ()
toTry = do (fileName:_) <- getArgs
            contents <- readFile fileName
            putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"


handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | otherwise = ioError e
```

除了 handler 以外其他东西都没变，我们只接住我们想要的 I/O exception。这边使用了 `System.IO.Error` 中的函数 **isDoesNotExistError** 跟 **ioError**。`isDoesNotExistError` 是一个运作在 `IOError` 上的 predicate ，他代表他接受一个 `IOError` 然后回传 `True` 或 `False`，他的型态是 `isDoesNotExistError :: IOError -> Bool`。我们用他来判断是否这个错误是文件不存在所造成的。我们这边使用 guard，但其实也可以用 if else。如果 exception 不是由于文件不存在所造成的，我们就用 `ioEroror` 重新丢出接到的 exception。他的型态是 `ioError :: IOException -> IO a`，所以他接受一个 `IOError` 然后产生一个会丢出 exception 的 I/O action。那个 I/O action 的型态是 `IO a`，但他其实不会产生任何结果，所以他可以被当作是 `IO anything`。

所以有可能在 `toTry` 里面丢出的 exception 并不是文件不存在造成的，而 `toTry `catch` handler` 会接住再丢出来，很酷吧。

##### predicate

程序里面有好几个运作在 `IOError` 上的 I/O action，当其中一个没有被 evaluate 成 `True` 时，就会掉到下一个 guard。这些 predicate 分别为：

- **isAlreadyExistsError**
- **isDoesNotExistError**
- **isFullError**
- **isEOFError**
- **isIllegalOperation**
- **isPermissionError**
- **isUserError**

大部分的意思都是显而易见的。当我们用了 **userError** 来丢出 exception 的时候，`isUserError` 被 evaluate 成 `True`。例如说，你可以写 `ioError $ userError "remote computer unplugged!"`，尽管用 `Either` 或 `Maybe` 来表示可能的错误会比自己丢出 exception 更好。

所以你可能写一个像这样的 handler

```
handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | isFullError e = freeSomeSpace
    | isIllegalOperation e = notifyCops
    | otherwise = ioError e
```

其中 `notifyCops` 跟 `freeSomeSpace` 是一些你定义的 I/O action。如果 exception 不是你要的，记得要把他们重新丢出，不然你的程序可能只会安静地当掉。

# haskell 标准库函数

###### succ / pred

`succ` 函数返回一个数的后继 (successor)

`pred` 函数返回一个数的前驱

```
ghci> succ 8
9
ghci> pred 8
7
```

```
ghci> :t succ
succ :: Enum a => a -> a
```

###### min/max

返回两个数的最小值/最大值

```
ghci> min 7 8
7
ghci> min 7 8 9

<interactive>:49:1: error:
    ? Non type-variable argument in the constraint: Ord (t1 -> t2)
      (Use FlexibleContexts to permit this)
    ? When checking the inferred type
        it :: forall {t1} {t2}.
              (Ord (t1 -> t2), Num t1, Num (t1 -> t2)) =>
              t2
```

###### ++

合并 list

```
ghci> [1,2]++[3,4]
[1,2,3,4]
```

###### :

往 list 前端插入元素

```
ghci> 1:[2,3]
[1,2,3]
```

###### : 与 ++ 的区别

: 和 ++ 都是中缀函数

++ 的左右两边的参数都是 list

: 的左边的参数是一个元素，右边的参数是一个 list

###### !!

从 List 中取元素

```
ghci> "hello" !! 0  
'h'
```

###### cycle

接受一个 List 做参数并返回一个无限 List 

```
cycle [1,2,3]
1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,Interrupted.
```

###### repeat

接受一个值作参数，并返回一个仅包含该值的无限 List

```
ghci> repeat 3
3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,Interrupted.
```

###### odd

判断函数是否为奇数，如果为奇数，返回 true

```
ghci> odd 1
True
ghci> odd 2
False
```

###### compare

`compare` 函数取两个 `Ord` 类中的相同型别的值作参数，回传比较的结果。这个结果是如下三种型别之一：`GT, LT, EQ`。

```
ghci> :t compare
compare :: Ord a => a -> a -> Ordering
ghci> compare 1 1
EQ
ghci> compare 1 2
LT
ghci> compare 2 1
GT
```

###### show

它可以取任一Show的成员型别并将其转为字串

```
ghci> :t show
show :: Show a => a -> String
ghci> show 1
"1"
```

###### read 配置型别注释使用

```
ghci> :t read
read :: Read a => String -> a
ghci> read "4" :: Float
4.0
ghci> read "4.0" :: Int
*** Exception: Prelude.read: no parse
```

###### minBound / maxBound

```
ghci> :t minBound
minBound :: Bounded a => a
ghci> minBound :: Int
-9223372036854775808
ghci> maxBound :: Int
9223372036854775807
```

这个 minBound 和 maxBound 居然没有返回值？

###### fromIntegral 

接收一个 Intergral 值，转换为指定的 Num 类型

```
ghci> :t fromIntegral
fromIntegral :: (Integral a, Num b) => a -> b
```

```
ghci> fromIntegral 100 ::Float
100.0
```

###### error

用來生成一个运行时错误

```
ghci> :t error
error :: [Char] -> a
```

```
ghci> error "hello"
*** Exception: hello
CallStack (from HasCallStack):
  error, called at <interactive>:136:1 in interactive:Ghci3
```



hello