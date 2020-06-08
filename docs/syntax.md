# 语法规范

类型：

```
T :=
	Int | Bool | T -> T | (T) |
	_ | id						// _为匿名的类型变量，id则为显式的类型变量
	() | (T,) | (T, ..., T) |	// 积类型或元组类型，可以为空，如果只有一个T则必须在末尾加一个逗号
	(T | ... | T) |				// 和类型或union类型，至少要有一个
	[T]							// 列表类型

TS :=
	forall id ... forall id T		// Type Scheme，带类型变量参数的类型
```

表达式：

```
E :=
	int | bool | (E)
	op E | E op E |								// 一元运算 + - !；二元运算 + - * / && || :: ++，::是cons而++是concat
	if E then E else E |

	() | (E,) | (E, ..., E) |					// 元组构造
	E.int |										// 元组解开，下标必须是整数字面值
	union (T | ... | T E | ... | T) |			// union构造，要恰好其中一项有值
	case E of (T id => E | ... | T id => E) |	// union模式匹配
	[E, ..., E] |								// 列表构造
	match E of ([] => E | id::id => E)			// 列表模式匹配

	lambda id:T E | lambda id E					// lambda表达式
	E E |										// 函数调用
	fix E
```


优先级：`||` < `&&` < 比较运算 < `:: ++` < 二元`+ -` < `* /` < 一元`+ -` = `! nil head tail fix` < 函数调用 < '.' < 关键字、括号、元组、列表

二元运算除了`::`为右结合，其他为左结合。比较运算只用于整数，除法是整除。

函数调用的优先级比一元运算高是为了方便`-f(x)`的解析，但这样`f fix g`就无法解析，我选择前者。

语句：

```
S :=
	let id = E | let id: TS = E | E
```

表达式需要有类型变量时，在TS中写forall a _这类形式。