# MyFP

一个简单的强类型函数式语言解释器。

## 计划清单

- [x] 词法和语法分析
- [x] 类型检查与惰性求值
- [x] HM类型系统及类型推导
- [ ] 库函数

## 效果

```
> let fact = fix \f \x if x==0 then 1 else f(x-1)*x;
fact : Int → Int
fact = fix (λf:Int → Int. λx:Int. if x == 0 then 1 else (f (x - 1)) * x)

> fact 5;
120 :  Int
```

```
> let map = fix \map \f \l if nil l then [] else [f (head l)] ++ map f (tail l);
map : ∀ α ∀ β (β → α) → [β] → [α]
map = fix (λmap:(β → α) → [β] → [α]. λf:β → α. λl:[β]. if nil (l) then [] else [f head (l)] ++ ((map f) tail (l)))

> map fact [1,2,3,4,5];
[1, 2, 6, 24, 120] :  [Int]
```

```
> let curry = \f \x \y f (x,y);
curry : ∀ α ∀ β ∀ γ ((α, β) → γ) → α → β → γ
curry = λf:(α, β) → γ. λx:α. λy:β. f (x, y)

> let uncurry = \f \x:(_,_) f x.0 x.1;
uncurry : ∀ α ∀ β ∀ γ (α → β → γ) → (α, β) → γ
uncurry = λf:α → β → γ. λx:(α, β). f x.0 x.1
```