# MyFP

一个简单的强类型函数式语言解释器。

## 计划清单

- [x] 词法和语法分析
- [x] 类型检查与惰性求值
- [ ] HM类型系统及类型推导
- [ ] 库函数

## 预期效果

### 基本功能

```
> let add = \x:Int \y:Int x+y;
add : Int → Int → Int
add = λx:Int. λy:Int. x + y

> add 3 4;
7 : Int

> let fact:Int->Int = fix \f:Int->Int \x:Int if x == 0 then 1 else f(x-1)*x;
fact : Int → Int
fact = fix (λf:Int → Int. λx:Int. if x == 0 then 1 else (f (x - 1)) * x)

> fact 5;
120 : Int
```