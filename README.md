# MyFP

一个简单的强类型函数式语言解释器。

## 计划清单

- [x] 词法和语法分析
- [x] 类型检查与惰性求值
- [x] HM类型系统及类型推导
- [ ] HKT、类型约束（类型类） *咕咕咕*
- [ ] Char、Monad、Parse Combinator *咕咕咕*

## 效果

```
> let fact = fix \f \x if x==0 then 1 else f(x-1)*x;
fact : Int → Int
fact = fix (λf:Int → Int. λx:Int. if x == 0 then 1 else (f (x - 1)) * x)

> let map = fix \map \f \l match l of ([] => [] | x::xs => (f x)::(map f xs));
map : ∀ α ∀ β (α → β) → [α] → [β]
map = fix (λmap:(α → β) → [α] → [β]. λf:α → β. λl:[α]. match l of ([] ⇒ [] | x::xs ⇒ (f x) :: (map f xs)))

> let gen = fix \gen \x if x == 0 then [0] else (gen (x-1)) ++ [x];
gen : Int → [Int]
gen = fix (λgen:Int → [Int]. λx:Int. if x == 0 then [0] else (gen (x - 1)) ++ [x])

> map fact (gen 10);
[1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800] : [Int]
```

```
> let curry = \f \x \y f (x,y);
curry : ∀ α ∀ β ∀ γ ((α, β) → γ) → α → β → γ
curry = λf:(α, β) → γ. λx:α. λy:β. f (x, y)

> let uncurry = \f \x:(_,_) f x.0 x.1;
uncurry : ∀ α ∀ β ∀ γ (α → β → γ) → (α, β) → γ
uncurry = λf:α → β → γ. λx:(α, β). f x.0 x.1

> let combine = \f1 \f2 \x case x of (_ a => f1 a | _ b => f2 b);
combine : ∀ α ∀ β ∀ γ (α → γ) → (β → γ) → (α | β) → γ
combine = λf1:α → γ. λf2:β → γ. λx:(α | β). case x of (α a ⇒ f1 a | β b ⇒ f2 b)

> let diverse = \f (\x f union(_ x|_), \y f union(_|_ y));
diverse : ∀ α ∀ β ∀ γ ((β | α) → γ) → (β → γ, α → γ)
diverse = λf:(β | α) → γ. (λx:β. f union (β x | α), λy:α. f union (β | α y))
```

试一下Maybe Monad：

```
> let just = \x union ( _ x | () );
just : ∀ α α → (α | ())
just = λx:α. union (α x | ())

> let nothing = union ( _ | _ ());
nothing : ∀ α (α | ())
nothing = union (α | () ())

> let return = just;
return : ∀ α α → (α | ())
return = λx:α. union (α x | ())

> let bind = \x \f case x of ( _ x => f x | () x => union ( _ | () () ) );
bind : ∀ α ∀ β (α | ()) → (α → (β | ())) → (β | ())
bind = λx:(α | ()). λf:α → (β | ()). case x of (α x ⇒ f x | () x ⇒ union (β | () ()))
```

```
> let safe_head = \l match l of ( [] => union ( _ | () () ) | x::xs => union ( _ x | _ ));
safe_head : ∀ α [α] → (α | ())
safe_head = λl:[α]. match l of ([] ⇒ union (α | () ()) | x::xs ⇒ union (α x | ()))

> let safe_tail = \l match l of ( [] => union ( _ | () () ) | x::xs => union ( _ xs | _ ));
safe_tail : ∀ α [α] → ([α] | ())
safe_tail = λl:[α]. match l of ([] ⇒ union ([α] | () ()) | x::xs ⇒ union ([α] xs | ()))

> bind (just [1,2,3]) \x bind (safe_tail x) \y bind (safe_head y) \z return (z*10);
union (Int 20 | ()) : (Int | ())
```

（注：最后一句实质上是`just [1,2,3] >>= \x safe_tail x >>= \y safe_head y >>= \z return (z*10)`，
或者用do：`do { x <- just [1,2,3]; y <- safe_tail x; z <- safe_head y; return (z*10) }`）

## 文档

[语法设计文档](docs/syntax.md)