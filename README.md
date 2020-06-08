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

> let map = fix \map \f \l match l of ([] => [] | x::xs => (f x)::(map f xs));
map : ∀ α ∀ β (α → β) → [α] → [β]
map = fix (λmap:(α → β) → [α] → [β]. λf:α → β. λl:[α]. match l of ([] ⇒ [] | x::xs ⇒ (f x) :: (map f xs)))

> map fact [0,1,2,3,4,5];
[1, 1, 2, 6, 24, 120] : [Int]
```

```
> let curry = \f \x \y f (x,y);
curry : ∀ α ∀ β ∀ γ ((α, β) → γ) → α → β → γ
curry = λf:(α, β) → γ. λx:α. λy:β. f (x, y)

> let uncurry = \f \x:(_,_) f x.0 x.1;
uncurry : ∀ α ∀ β ∀ γ (α → β → γ) → (α, β) → γ
uncurry = λf:α → β → γ. λx:(α, β). f x.0 x.1

> let combine = \f1 \f2 \x case x of (_ a => f1 a | _ b => f2 b);
combine : ∀ α ∀ β ∀ γ (β → α) → (γ → α) → α → α
combine = λf1:β → α. λf2:γ → α. λx:α. case x of (β a ⇒ f1 a | γ b ⇒ f2 b)

> let diverse = \f (\x f union(_ x|_), \y f union(_|_ y));
diverse : ∀ α ∀ β ∀ γ ((β | α) → γ) → (β → γ, α → γ)
diverse = λf:(β | α) → γ. (λx:β. f union (β x | α), λy:α. f union (β | α y))
```